# The nowcasting backends that ship with tbl.now.
#
# Every backend is two S3 methods: `nowcast_fit.<method>()` runs the modelling
# package (reusing the `tbl_now_to_*()` converters wherever one exists) and
# `nowcast_tidy.<method>()` reshapes its output into the tidy quantile/draw
# format documented in `?nowcast_tidy`.
#
# Each modelling package is an optional dependency, checked with `.need_pkg()`
# at fit time.

# Shared helpers -----

#' Run an expression, silencing it unless the user asked for chatter
#'
#' @param expr The expression to evaluate.
#' @param verbose Logical; when `FALSE` messages and warnings are suppressed.
#'
#' @return The value of `expr`.
#'
#' @keywords internal
#' @noRd
.quietly_if <- function(expr, verbose) {
  if (isTRUE(verbose)) {
    return(force(expr))
  }
  suppressMessages(suppressWarnings(force(expr)))
}

#' Build the tidy draws frame from a `[draws x time]` matrix
#'
#' @param matrix_draws A numeric matrix with draws in rows and event times in
#'   columns.
#' @param event_dates The event dates, aligned with the columns.
#' @param event_col Name to give the event-date column.
#'
#' @return A `tibble` with `event_col`, `.draw` and `.value`.
#'
#' @keywords internal
#' @noRd
.matrix_to_draws <- function(matrix_draws, event_dates, event_col) {
  n_draws <- nrow(matrix_draws)
  dplyr::tibble(
    !!event_col := rep(event_dates, each = n_draws),
    .draw = rep(seq_len(n_draws), times = ncol(matrix_draws)),
    .value = as.vector(matrix_draws)
  )
}

#' Warn that a backend cannot honour the declared strata
#'
#' @param x A `tbl_now` object.
#' @param method Name of the method.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
.warn_pooled_strata <- function(x, method) {
  strata <- get_strata(x)
  if (length(strata) > 0) {
    cli::cli_warn(c(
      "{.pkg {method}} cannot nowcast the declared strata {.val {strata}} here; \\
       pooling over them.",
      "i" = "Nowcast each stratum separately if you need stratified results."
    ))
  }
  invisible(NULL)
}

# diseasenowcasting -----

#' @rdname nowcast_fit
#' @export
nowcast_fit.diseasenowcasting <- function(method, x, ...,
                                          quantile_levels = nowcast_quantile_levels(),
                                          verbose = TRUE) {
  .need_pkg("diseasenowcasting")
  # diseasenowcasting was built around `tbl_now`, so it reads the strata,
  # covariates and temporal effects straight off the object: no conversion.
  .quietly_if(diseasenowcasting::nowcast(x, ...), verbose)
}

#' @rdname nowcast_tidy
#' @export
nowcast_tidy.diseasenowcasting <- function(method, fit, x, ..., quantile_levels) {
  prediction <- stats::predict(fit)

  event_col <- get_event_date(x)
  event_dates <- S7::prop(prediction, "event_dates")
  strata_cols <- get_strata(x) %||% character(0)
  strata_levels <- S7::prop(prediction, "strata_levels")
  strata_draws <- S7::prop(prediction, "strata_draws")

  # The stratified draws come back as a [draws x time x stratum] array indexed by
  # `strata_levels`. That collapses several strata columns into one label, so it
  # can only be mapped back when a single column was declared.
  stratified <- length(strata_cols) == 1 &&
    !is.null(strata_draws) &&
    length(dim(strata_draws)) == 3 &&
    length(strata_levels) == dim(strata_draws)[3] &&
    !identical(strata_levels, "all")

  if (stratified) {
    n_draws <- dim(strata_draws)[1]
    n_times <- dim(strata_draws)[2]
    draws <- dplyr::tibble(
      !!event_col := rep(rep(event_dates, each = n_draws), times = length(strata_levels)),
      !!strata_cols := rep(strata_levels, each = n_draws * n_times),
      .draw = rep(seq_len(n_draws), times = n_times * length(strata_levels)),
      .value = as.vector(strata_draws)
    )
    return(list(predictions = NULL, draws = draws))
  }

  if (length(strata_cols) > 0) {
    .warn_pooled_strata(x, "diseasenowcasting")
  }

  list(
    predictions = NULL,
    draws = .matrix_to_draws(S7::prop(prediction, "draws"), event_dates, event_col)
  )
}

# baselinenowcast -----

#' @rdname nowcast_fit
#' @export
nowcast_fit.baselinenowcast <- function(method, x, ..., draws = 1000,
                                        delays_unit = NULL,
                                        quantile_levels = nowcast_quantile_levels(),
                                        verbose = TRUE) {
  .need_pkg("baselinenowcast")
  strata_cols <- get_strata(x) %||% character(0)

  if (length(strata_cols) == 0) {
    triangle <- .quietly_if(
      tbl_now_to_baselinenowcast(x, delays_unit = delays_unit, verbose = verbose),
      verbose
    )
    return(.quietly_if(
      baselinenowcast::baselinenowcast(
        triangle, output_type = "samples", draws = draws, ...
      ),
      verbose
    ))
  }

  # A reporting-triangle *matrix* has no strata dimension, so a stratified
  # nowcast is one triangle (and one fit) per stratum.
  delays_unit <- .baselinenowcast_delays_unit(x, delays_unit)
  long <- .quietly_if(
    tbl_now_to_baselinenowcast(x, format = "long", verbose = verbose),
    verbose
  )

  key <- do.call(paste, c(long[strata_cols], list(sep = "\r")))
  lookup <- dplyr::distinct(dplyr::tibble(.key = key, !!!long[strata_cols]))

  fits <- .quietly_if(
    lapply(split(long, key), function(stratum) {
      triangle <- baselinenowcast::as_reporting_triangle(
        as.data.frame(stratum[, c("reference_date", "report_date", "count")]),
        delays_unit = delays_unit
      )
      baselinenowcast::baselinenowcast(
        triangle, output_type = "samples", draws = draws, ...
      )
    }),
    verbose
  )

  structure(fits, strata_lookup = lookup, class = "baselinenowcast_strata")
}

#' @rdname nowcast_tidy
#' @export
nowcast_tidy.baselinenowcast <- function(method, fit, x, ..., quantile_levels) {
  event_col <- get_event_date(x)

  tidy_one <- function(samples) {
    dplyr::as_tibble(samples) |>
      dplyr::filter(.data$output_type == "samples") |>
      dplyr::transmute(
        !!event_col := .data$reference_date,
        .draw = as.integer(.data$draw),
        .value = as.numeric(.data$pred_count)
      )
  }

  if (!inherits(fit, "baselinenowcast_strata")) {
    return(list(predictions = NULL, draws = tidy_one(fit)))
  }

  lookup <- attr(fit, "strata_lookup")
  draws <- dplyr::bind_rows(lapply(names(fit), function(key) {
    tidy_one(fit[[key]]) |> dplyr::mutate(.key = key)
  }))

  draws <- draws |>
    dplyr::inner_join(lookup, by = ".key") |>
    dplyr::select(-".key")

  list(predictions = NULL, draws = draws)
}

# epinowcast -----

#' @rdname nowcast_fit
#' @export
nowcast_fit.epinowcast <- function(method, x, ..., preprocess_args = list(),
                                   quantile_levels = nowcast_quantile_levels(),
                                   verbose = TRUE) {
  .need_pkg("epinowcast")

  preprocessed <- .quietly_if(
    do.call(
      tbl_now_to_epinowcast,
      c(list(x = x, verbose = verbose, quiet = !verbose), preprocess_args)
    ),
    verbose
  )

  .quietly_if(epinowcast::epinowcast(preprocessed, ...), verbose)
}

#' @rdname nowcast_tidy
#' @export
nowcast_tidy.epinowcast <- function(method, fit, x, ..., quantile_levels) {
  event_col <- get_event_date(x)
  strata_cols <- get_strata(x) %||% character(0)

  samples <- tryCatch(
    summary(fit, type = "nowcast_samples"),
    error = function(e) NULL
  )

  if (!is.null(samples)) {
    samples <- dplyr::as_tibble(samples)
    strata_kept <- intersect(strata_cols, colnames(samples))
    draws <- samples |>
      dplyr::transmute(
        # epinowcast works in data.table's `IDate`; bring it back to `Date` so
        # the predictions join cleanly against the rest of the package.
        !!event_col := as.Date(.data$reference_date),
        dplyr::across(dplyr::all_of(strata_kept)),
        .draw = as.integer(.data$.draw),
        .value = as.numeric(.data$sample)
      )
    if (length(strata_kept) < length(strata_cols)) {
      .warn_pooled_strata(x, "epinowcast")
    }
    return(list(predictions = NULL, draws = draws))
  }

  # No sample storage (e.g. `output_loglik = FALSE` fits): fall back to the
  # quantile summary, which epinowcast can always produce.
  summarised <- dplyr::as_tibble(summary(fit, probs = quantile_levels))
  strata_kept <- intersect(strata_cols, colnames(summarised))
  quantile_map <- stats::setNames(
    quantile_levels,
    paste0("q", formatC(100 * quantile_levels, format = "g"))
  )

  predictions <- .wide_to_quantiles(
    summarised |> dplyr::mutate(!!event_col := as.Date(.data$reference_date)),
    group_cols = c(event_col, strata_kept),
    quantile_map = quantile_map
  )

  list(predictions = predictions, draws = NULL)
}

# nowcaster -----

#' @rdname nowcast_fit
#' @export
nowcast_fit.nowcaster <- function(method, x, ..., trajectories = TRUE,
                                  quantile_levels = nowcast_quantile_levels(),
                                  verbose = TRUE) {
  .need_pkg("nowcaster")
  if (!requireNamespace("INLA", quietly = TRUE)) {
    # nowcaster fits through INLA, which is not on CRAN and is therefore not
    # pulled in when nowcaster is installed.
    cli::cli_abort(c(
      "{.pkg nowcaster} needs {.pkg INLA}, which is not installed.",
      "i" = "Install it with: \\
             {.code install.packages(\"INLA\", repos = c(getOption(\"repos\"), \\
             INLA = \"https://inla.r-inla-download.org/R/stable\"), dep = TRUE)}"
    ))
  }

  if (get_data_type(x) != "linelist") {
    cli::cli_abort(c(
      "{.pkg nowcaster} expects line-list data; {.arg x} has data_type \\
       {.val {get_data_type(x)}}.",
      "i" = "Supply a line-list {.cls tbl_now}."
    ))
  }
  .warn_pooled_strata(x, "nowcaster")

  .quietly_if(
    nowcaster::nowcasting_inla(
      dataset = as.data.frame(x),
      date_onset = get_event_date(x),
      date_report = get_report_date(x),
      trajectories = trajectories,
      ...
    ),
    verbose
  )
}

#' @rdname nowcast_tidy
#' @export
nowcast_tidy.nowcaster <- function(method, fit, x, ..., quantile_levels) {
  event_col <- get_event_date(x)

  # `trajectories = TRUE` gives the posterior samples themselves, which is what
  # the ensemble's linear pool needs; the summary is only a fallback.
  if (!is.null(fit$trajectories)) {
    draws <- dplyr::as_tibble(fit$trajectories) |>
      dplyr::group_by(.data$dt_event, .data$sample) |>
      dplyr::summarise(.value = sum(.data$Y, na.rm = TRUE), .groups = "drop") |>
      dplyr::transmute(
        !!event_col := as.Date(.data$dt_event),
        .draw = as.integer(.data$sample),
        .value = .data$.value
      )
    return(list(predictions = NULL, draws = draws))
  }

  if (is.null(fit$total)) {
    cli::cli_abort(
      "The {.pkg nowcaster} fit has neither {.field trajectories} nor {.field total}."
    )
  }

  cli::cli_warn(
    "The {.pkg nowcaster} fit carries no trajectories; only its five reported \\
     quantiles are available. Refit with {.code trajectories = TRUE} for draws."
  )

  predictions <- .wide_to_quantiles(
    dplyr::as_tibble(fit$total) |>
      dplyr::mutate(!!event_col := as.Date(.data$dt_event)),
    group_cols = event_col,
    quantile_map = c(LI = 0.025, LIb = 0.25, Median = 0.5, LSb = 0.75, LS = 0.975)
  )

  list(predictions = predictions, draws = NULL)
}

# NobBS -----

#' Translate the object's time units into NobBS's `units` argument
#'
#' @param x A `tbl_now` object.
#'
#' @return `"1 day"` or `"1 week"`.
#'
#' @keywords internal
#' @noRd
.nobbs_units <- function(x) {
  units <- get_event_units(x)
  switch(units,
    days = "1 day",
    weeks = "1 week",
    cli::cli_abort(c(
      "{.pkg NobBS} only supports daily or weekly data; {.arg x} has \\
       event_units {.val {units}}."
    ))
  )
}

#' @rdname nowcast_fit
#' @export
nowcast_fit.NobBS <- function(method, x, ..., specs = list(),
                              quantile_levels = nowcast_quantile_levels(),
                              verbose = TRUE) {
  .need_pkg("NobBS")

  if (get_data_type(x) != "linelist") {
    cli::cli_abort(c(
      "{.pkg NobBS} expects line-list data; {.arg x} has data_type \\
       {.val {get_data_type(x)}}.",
      "i" = "Supply a line-list {.cls tbl_now}."
    ))
  }

  # NobBS reports whichever quantiles it is told about at fit time, so the
  # requested levels have to be pushed into `specs`.
  specs$quantiles <- specs$quantiles %||% quantile_levels

  arguments <- list(
    data = as.data.frame(x),
    now = get_now(x),
    units = .nobbs_units(x),
    onset_date = get_event_date(x),
    report_date = get_report_date(x),
    specs = specs,
    ...
  )

  strata_cols <- get_strata(x) %||% character(0)
  if (length(strata_cols) == 1) {
    arguments$strata <- strata_cols
    return(.quietly_if(do.call(NobBS::NobBS.strat, arguments), verbose))
  }
  if (length(strata_cols) > 1) {
    .warn_pooled_strata(x, "NobBS")
  }

  .quietly_if(do.call(NobBS::NobBS, arguments), verbose)
}

#' @rdname nowcast_tidy
#' @export
nowcast_tidy.NobBS <- function(method, fit, x, ..., quantile_levels) {
  event_col <- get_event_date(x)
  strata_cols <- get_strata(x) %||% character(0)

  estimates <- dplyr::as_tibble(fit$estimates)

  # `NobBS.strat()` calls the stratum column `stratum` (and repeats it as
  # `stratum.1`), so map it back onto the name the `tbl_now` declared.
  if (length(strata_cols) == 1 && "stratum" %in% colnames(estimates)) {
    estimates <- estimates |>
      dplyr::select(-dplyr::any_of("stratum.1")) |>
      dplyr::rename(!!strata_cols := "stratum")
  }

  strata_kept <- intersect(strata_cols, colnames(estimates))

  # NobBS names its quantile columns `q_<level>`.
  quantile_columns <- grep("^q_", colnames(estimates), value = TRUE)
  quantile_map <- stats::setNames(
    as.numeric(sub("^q_", "", quantile_columns)),
    quantile_columns
  )

  predictions <- .wide_to_quantiles(
    estimates |> dplyr::rename(!!event_col := "onset_date"),
    group_cols = c(event_col, strata_kept),
    quantile_map = quantile_map
  )

  # `nowcast.post.samps` are draws for the `now` date only, so they cannot fill
  # the `draws` slot (which must cover every event date).
  list(predictions = predictions, draws = NULL)
}
