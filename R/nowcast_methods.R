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

# NobBS -----

#' @rdname nowcast_fit
#' @export
nowcast_fit.NobBS <- function(method, x, ..., specs = list(),
                              quantile_levels = nowcast_quantile_levels(),
                              verbose = TRUE) {
  .need_pkg("NobBS")

  # `NobBS()` counts ROWS. Handing it count data directly nowcasts 1,174 rows
  # as 1,174 cases when they carry 50,160, with no error -- so the line list
  # comes from the converter, which expands counts to one row per case and
  # refuses any grid NobBS cannot model.
  linelist <- .quietly_if(tbl_now_to_nobbs(x, verbose = verbose), verbose)

  # NobBS reports whichever quantiles it is told about at fit time, so the
  # requested levels have to be pushed into `specs`.
  specs$quantiles <- specs$quantiles %||% quantile_levels

  arguments <- list(
    data = linelist,
    now = get_now(x),
    units = .nobbs_units(get_event_units(x)),
    onset_date = "onset_date",
    report_date = "report_date",
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

# Reusing the `tidy()` methods -----
#
# Some backends report a point estimate and ONE interval, and nothing else --
# `surveillance` reads its bounds off the `pi` slot, EpiNow2 off its
# `lower_<pct>` / `upper_<pct>` columns. `R/tidy.R` already knows how to get
# those out, including the width, and re-deriving that here is how the audited
# bugs got in (a pooled `stratum = "all"`, a zero-width band quoted as 95%).
# So these backends go through `tidy()` and `.tidy_to_predictions()` maps the
# standard frame into the tidy quantile shape.

#' Recover the strata columns from `" | "`-pasted `tidy()` stratum labels
#'
#' `tidy()` reports one `stratum` string per row, pasting several stratifying
#' columns `" | "`-separated (`.epinowcast_stratum()`, `.epinow2_region()` and
#' the `triangle_list` names all use that convention). A `tbl_nowcast` keeps the
#' strata as their own columns, so the label has to be split back apart.
#'
#' @param labels Character vector of stratum labels.
#' @param strata_cols Character vector of the declared strata column names.
#'
#' @return A `tibble` with one column per stratum, or `NULL` when there are no
#'   strata.
#'
#' @keywords internal
#' @noRd
.strata_from_labels <- function(labels, strata_cols) {
  if (length(strata_cols) == 0) {
    return(NULL)
  }
  parts <- strsplit(as.character(labels), " | ", fixed = TRUE)
  if (any(lengths(parts) != length(strata_cols))) {
    cli::cli_abort(c(
      "Cannot map the stratum labels back onto {.val {strata_cols}}.",
      "i" = "Labels are {.val { ' | '}}-separated, so a stratum value \\
             containing that separator cannot be split back apart."
    ))
  }
  values <- lapply(seq_along(strata_cols), function(i) {
    vapply(parts, function(p) p[[i]], character(1))
  })
  dplyr::as_tibble(stats::setNames(values, strata_cols))
}

#' Turn a standard `tidy()` nowcast frame into tidy quantile predictions
#'
#' An engine that reports a median and one interval can express exactly three
#' quantile levels: `0.5`, and the two tails implied by the interval's `level`.
#' Anything else the caller asked for is not available, and inventing it would
#' be an approximation dressed up as a quantile -- so it is dropped, with a
#' warning naming what was lost.
#'
#' @param tidied A tibble from a `tidy()` method: `event_date`, `stratum`,
#'   `estimate`, `conf.low`, `conf.high`, `level`.
#' @param x The source `tbl_now`.
#' @param quantile_levels The levels the caller asked for.
#' @param engine Engine name, for the message.
#'
#' @return A list with `predictions` and `draws` (always `NULL`), as
#'   [nowcast_tidy()] must return.
#'
#' @keywords internal
#' @noRd
.tidy_to_predictions <- function(tidied, x, quantile_levels, engine) {
  event_col <- get_event_date(x)
  strata_cols <- get_strata(x) %||% character(0)

  level <- unique(stats::na.omit(tidied$level))
  if (length(level) > 1) {
    cli::cli_abort(
      "{.pkg {engine}} reported intervals of different widths ({.val {level}}); \\
       they cannot be expressed at one set of quantile levels."
    )
  }

  base <- dplyr::tibble(
    !!event_col := as.Date(tidied$event_date),
    .estimate = as.numeric(tidied$estimate),
    .lower = as.numeric(tidied$conf.low),
    .upper = as.numeric(tidied$conf.high)
  )
  strata_values <- .strata_from_labels(tidied$stratum, strata_cols)
  if (!is.null(strata_values)) base <- dplyr::bind_cols(base, strata_values)

  # The median is always available; the tails only when the engine said how wide
  # its interval is.
  pieces <- list(
    dplyr::mutate(base, .quantile_level = 0.5, .value = .data$.estimate)
  )
  if (length(level) == 1) {
    # Rounded: `(1 - 0.8) / 2` is 0.09999999999999998, which then fails to match
    # another member's exact 0.1 when the two are ensembled, and fails autoplot's
    # symmetric-pair detection -- silently, as a missing interval rather than an
    # error.
    tail_level <- round((1 - level) / 2, 10)
    pieces <- c(pieces, list(
      dplyr::mutate(base, .quantile_level = tail_level, .value = .data$.lower),
      dplyr::mutate(
        base, .quantile_level = round(1 - tail_level, 10), .value = .data$.upper
      )
    ))
  }

  produced <- sort(unique(unlist(lapply(pieces, function(p) p$.quantile_level[1]))))
  unavailable <- setdiff(quantile_levels, produced)
  if (length(unavailable) > 0) {
    # Built with paste0() rather than one cli string: a `\\` continuation inside
    # a cli message breaks the plural marker that follows it.
    cli::cli_warn(c(
      paste0(
        "{.pkg {engine}} reports a point estimate and one interval, so it ",
        "cannot report level{?s} {.val {unavailable}}."
      ),
      "i" = "The nowcast carries the levels it has: {.val {produced}}."
    ))
  }

  predictions <- dplyr::bind_rows(pieces) |>
    dplyr::select(dplyr::all_of(c(event_col, strata_cols, ".quantile_level", ".value"))) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(
      c(event_col, strata_cols, ".quantile_level")
    )))

  list(predictions = predictions, draws = NULL)
}

# surveillance -----

#' Split a `tbl_now` into one object per stratum
#'
#' \pkg{surveillance} and the other one-series engines have no strata argument,
#' so a stratified nowcast is one fit per stratum. The names are the
#' `" | "`-pasted labels the `tidy()` methods use, so the two ends agree.
#'
#' @param x A `tbl_now` object.
#'
#' @return A named list of `tbl_now` objects; a single `"all"` element when the
#'   object declares no strata.
#'
#' @keywords internal
#' @noRd
.split_by_strata <- function(x) {
  strata_cols <- get_strata(x) %||% character(0)
  if (length(strata_cols) == 0) {
    return(list(all = x))
  }
  # `.epinow2_region()` subsets columns, and a column subset of a `tbl_now`
  # drops the protected ones and warns. It only needs the strata values, so hand
  # it a bare data frame.
  labels <- .epinow2_region(.strip_tbl_now(x), strata_cols)
  # `split()` on a `tbl_now` emits a spurious dropped-protected-column warning;
  # `filter()` has a `tbl_now` method and leaves the object intact.
  stats::setNames(
    lapply(unique(labels), function(label) {
      dplyr::filter(x, labels == !!label)
    }),
    unique(labels)
  )
}

#' @rdname nowcast_fit
#' @export
nowcast_fit.surveillance <- function(method, x, ..., when = NULL, D = NULL,
                                     fit_method = "bayes.notrunc.bnb",
                                     control = list(),
                                     quantile_levels = nowcast_quantile_levels(),
                                     verbose = TRUE) {
  .need_pkg("surveillance")

  aggregate_by <- .surveillance_aggregate_by(get_event_units(x))
  now <- get_now(x)
  D <- D %||% .surveillance_max_delay(x)

  fit_one <- function(stratum) {
    # The converter expands counts to one row per case; `now` and the grid stay
    # the caller's job, because a line list cannot express a zero day and
    # `surveillance` would otherwise stop at the last date with a report.
    linelist <- .quietly_if(
      tbl_now_to_surveillance(stratum, verbose = verbose), verbose
    )
    grid <- seq(min(linelist$dHospital), now, by = aggregate_by)
    arguments <- utils::modifyList(
      list(
        now = now,
        when = when %||% utils::tail(grid, D + 1L),
        data = linelist,
        dEventCol = "dHospital", dReportCol = "dReport",
        aggregate.by = aggregate_by, D = D, method = fit_method,
        control = utils::modifyList(list(dRange = grid), control)
      ),
      list(...)
    )
    .quietly_if(do.call(surveillance::nowcast, arguments), verbose)
  }

  fits <- lapply(.split_by_strata(x), fit_one)
  if (length(get_strata(x) %||% character(0)) == 0) {
    return(fits[[1]])
  }
  structure(fits, class = "surveillance_strata")
}

#' The maximum delay to give an engine that must be told one
#'
#' @param x A `tbl_now` object.
#'
#' @return A single integer.
#'
#' @keywords internal
#' @noRd
.surveillance_max_delay <- function(x) {
  delays <- x[[".delay"]]
  observed <- suppressWarnings(max(delays[is.finite(delays) & delays >= 0], na.rm = TRUE))
  if (!is.finite(observed)) {
    cli::cli_abort("Cannot derive {.arg D}: {.arg x} has no usable delays.")
  }
  as.integer(ceiling(observed))
}

#' @rdname nowcast_tidy
#' @export
nowcast_tidy.surveillance <- function(method, fit, x, ..., quantile_levels) {
  fits <- if (inherits(fit, "surveillance_strata")) fit else list(all = fit)

  tidied <- dplyr::bind_rows(lapply(names(fits), function(label) {
    one <- tidy(fits[[label]])
    one$stratum <- label
    one
  }))

  .tidy_to_predictions(tidied, x, quantile_levels, "surveillance")
}

# EpiNow2 -----

#' @rdname nowcast_fit
#' @export
nowcast_fit.EpiNow2 <- function(method, x, ..., convert_args = list(), # nolint: object_name_linter.
                                quantile_levels = nowcast_quantile_levels(),
                                verbose = TRUE) {
  .need_pkg("EpiNow2")
  strata_cols <- get_strata(x) %||% character(0)

  # `estimate_infections()` is the entry point that produces a case nowcast;
  # `regional_epinow()` is the same model run once per region, which is how
  # EpiNow2 expresses strata. The other two targets fit a truncation and a delay
  # distribution, neither of which is a nowcast (see `?tbl_now_EpiNow2`).
  target <- if (length(strata_cols) > 0) "regional_epinow" else "estimate_infections"

  series <- .quietly_if(
    do.call(
      tbl_now_to_EpiNow2,
      c(list(x = x, target = target, verbose = verbose, quiet = !verbose), convert_args)
    ),
    verbose
  )

  if (target == "regional_epinow") {
    return(.quietly_if(EpiNow2::regional_epinow(series, ...), verbose))
  }
  .quietly_if(EpiNow2::estimate_infections(series, ...), verbose)
}

#' @rdname nowcast_tidy
#' @export
nowcast_tidy.EpiNow2 <- function(method, fit, x, ..., quantile_levels) { # nolint: object_name_linter.
  # `tidy.estimate_infections()` and the `tidy.list()` EpiNow2 branch both read
  # the interval width off the fit's own `lower_<pct>`/`upper_<pct>` columns,
  # because `CrIs` is a user argument and assuming 0.95 would be wrong for a
  # default fit (which reports 0.9).
  tidied <- tidy(fit)
  .tidy_to_predictions(tidied, x, quantile_levels, "EpiNow2")
}
