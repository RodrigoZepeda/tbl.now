# The `tbl_nowcast` class: the common currency of `run_nowcast()`.
#
# Every backend (diseasenowcasting, epinowcast, baselinenowcast, NobBS,
# surveillance, EpiNow2) returns something different -- a Stan fit, a matrix, a list of
# data frames. `run_nowcast()` wraps whatever the backend produced into a
# `tbl_nowcast`, which always carries the predictions in the *same* tidy shape
# so that they can be plotted, scored and ensembled without knowing which
# package produced them.
#
# The raw backend object is never thrown away: it lives in the `fit` property.

#' Default quantile levels for a nowcast
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' The quantile levels [run_nowcast()] summarises a nowcast at by default: nine
#' probabilities, symmetric about the median, spanning the 50%, 80%, 90% and 95%
#' central intervals.
#'
#' They are a **subset** of the 23 levels the US and European COVID-19 forecast
#' hubs and FluSight ask for (0.01, 0.025, 0.05, then 0.10 to 0.90 in steps of
#' 0.05, then 0.975 and 0.99). Nine cover the intervals people actually read at a
#' fraction of the storage, and every one of them is a hub level, so the output
#' still scores against hub submissions in \pkg{scoringutils} without
#' interpolation. Pass `quantile_levels` explicitly when you need the full hub
#' set:
#'
#' ```r
#' hub_levels <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
#' run_nowcast(x, engine("baselinenowcast", quantile_levels = hub_levels))
#' ```
#'
#' The levels live on the [engine()], not on [run_nowcast()], because for some
#' backends they are a fit-time model argument rather than a way of summarising
#' afterwards.
#'
#' Backends that expose draws can honour any levels you ask for. Ones that report
#' a point estimate and a single interval (`"surveillance"`, `"EpiNow2"`) cannot,
#' and say so rather than interpolating.
#'
#' @return A numeric vector of nine probabilities in `(0, 1)`, sorted
#' increasingly.
#'
#' @seealso
#' [engine()], whose `quantile_levels` argument this is the default for;
#' [run_nowcast()] and [nowcast_backtest()], which report at these levels;
#' [score_nowcast()] and [as_scoringutils()], which score them.
#'
#' @examples
#' nowcast_quantile_levels()
#'
#' # The 50%, 80%, 90% and 95% central intervals, as lower/upper pairs.
#' matrix(nowcast_quantile_levels()[-5], ncol = 2)
#'
#' # Ask an engine for something else -- here the full forecast-hub set.
#' hub_levels <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
#' engine("baselinenowcast", quantile_levels = hub_levels)
#'
#' @export
nowcast_quantile_levels <- function() {
  c(0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975)
}

#' A nowcast produced by [run_nowcast()]
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' An S7 object holding a nowcast in a package-agnostic shape. It is what
#' [run_nowcast()] returns and what [nowcast_ensemble()] and [score_nowcast()]
#' consume.
#'
#' @param predictions A `tibble` of quantile predictions. One row per
#'   (event date, stratum, quantile level) with the columns
#'   `<event_date>`, the strata columns, `.quantile_level` and `.value`.
#' @param draws Either `NULL` (backends that only return quantiles) or a
#'   `tibble` with one row per (event date, stratum, draw) and the columns
#'   `<event_date>`, the strata columns, `.draw` and `.value`.
#' @param method The name of the method that produced the nowcast.
#' @param fit The untouched object returned by the backend.
#' @param now The `now` of the nowcast.
#' @param event_date Name of the event-date column in `predictions`/`draws`.
#' @param strata Character vector with the names of the strata columns
#'   (`character(0)` when the nowcast is not stratified).
#' @param data The `tbl_now` the nowcast was produced from.
#' @param call The matched call of [run_nowcast()].
#' @param metadata A named list with anything else the backend wants to keep.
#'
#' @return A `tbl_nowcast` object.
#'
#' @seealso [run_nowcast()], [nowcast_ensemble()], [score_nowcast()]
#'
#' @examples
#' # Normally built for you by the one-call front door, but the constructor is
#' # exported because a new backend -- or a test -- needs to build one directly.
#' predictions <- data.frame(
#'   onset_week = as.Date("2020-01-05") + c(0, 0, 7, 7),
#'   .quantile_level = c(0.5, 0.9, 0.5, 0.9),
#'   .value = c(10, 14, 12, 17)
#' )
#' tbl_nowcast(predictions = predictions, method = "toy", event_date = "onset_week")
#'
#' @export
tbl_nowcast <- S7::new_class(
  "tbl_nowcast",
  package = "tbl.now",
  properties = list(
    predictions = S7::new_property(S7::class_data.frame,
      default = quote(dplyr::tibble())
    ),
    draws      = S7::new_property(S7::class_any, default = NULL),
    method     = S7::new_property(S7::class_character, default = NA_character_),
    fit        = S7::new_property(S7::class_any, default = NULL),
    now        = S7::new_property(S7::class_any, default = NULL),
    event_date = S7::new_property(S7::class_character, default = "event_date"),
    strata     = S7::new_property(S7::class_character, default = quote(character(0))),
    data       = S7::new_property(S7::class_any, default = NULL),
    call       = S7::new_property(S7::class_any, default = NULL),
    metadata   = S7::new_property(S7::class_list, default = quote(list()))
  ),
  validator = function(self) {
    if (length(self@method) != 1) {
      return("`method` must be a single string.")
    }

    if (nrow(self@predictions) > 0) {
      required <- c(self@event_date, self@strata, ".quantile_level", ".value")
      missing_cols <- setdiff(required, colnames(self@predictions))
      if (length(missing_cols) > 0) {
        return(paste0(
          "`predictions` is missing the column(s): ",
          paste0("`", missing_cols, "`", collapse = ", "), "."
        ))
      }
      lvl <- self@predictions[[".quantile_level"]]
      if (any(!is.na(lvl) & (lvl <= 0 | lvl >= 1))) {
        return("`.quantile_level` must be strictly between 0 and 1.")
      }
    }

    if (!is.null(self@draws)) {
      if (!is.data.frame(self@draws)) {
        return("`draws` must be `NULL` or a data frame.")
      }
      required <- c(self@event_date, self@strata, ".draw", ".value")
      missing_cols <- setdiff(required, colnames(self@draws))
      if (length(missing_cols) > 0) {
        return(paste0(
          "`draws` is missing the column(s): ",
          paste0("`", missing_cols, "`", collapse = ", "), "."
        ))
      }
    }

    NULL
  }
)

#' Is this object a `tbl_nowcast`?
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' Tests whether an object is a fitted nowcast -- the thing [run_nowcast()]
#' returns -- rather than the data a nowcast is fitted to (for which see
#' [is_tbl_now()][validate_tbl_now]).
#'
#' @param x An object.
#'
#' @return A single `TRUE` when `x` is a [tbl_nowcast], `FALSE` otherwise.
#'
#' @seealso
#' [tbl_nowcast] for the class itself; [run_nowcast()] which produces one;
#' [is_tbl_now()][validate_tbl_now] for the input side.
#'
#' @examples
#' # A number is not a nowcast.
#' is_tbl_nowcast(1)
#'
#' # The object run_nowcast() returns is.
#' predictions <- data.frame(
#'   onset_week = as.Date("2020-01-05"),
#'   .quantile_level = c(0.5, 0.9), .value = c(10, 14)
#' )
#' nc <- tbl_nowcast(
#'   predictions = predictions, method = "toy", event_date = "onset_week"
#' )
#' is_tbl_nowcast(nc)
#'
#' @export
is_tbl_nowcast <- function(x) {
  S7::S7_inherits(x, tbl_nowcast)
}

#' Abort when an object is not a `tbl_nowcast`
#'
#' @param x The object to check.
#' @param arg Name of the argument being checked.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
.assert_tbl_nowcast <- function(x, arg = "x") {
  if (!is_tbl_nowcast(x)) {
    cli::cli_abort("{.arg {arg}} must be a {.cls tbl_nowcast} (see {.fn run_nowcast}).")
  }
  invisible(NULL)
}

# Printing -----

#' @export
S7::method(print, tbl_nowcast) <- function(x, ..., n = 6) {
  # NOTE: a print method must write to STDOUT. The `cli_*()` family emits
  # *messages*, so its output disappears under `message = FALSE`, `sink()` or
  # `capture.output()` -- which is exactly where a print method is expected to
  # work. The `cat_*()` family is the stdout counterpart.
  n_dates <- length(unique(x@predictions[[x@event_date]]))

  cli::cat_rule(
    left = cli::format_inline(
      "A {.cls tbl_nowcast} from method {.val {x@method}}"
    )
  )
  cli::cat_bullet(c(
    cli::format_inline("now: {.val {as.character(x@now)}}"),
    cli::format_inline("event dates: {.val {n_dates}}"),
    if (length(x@strata) > 0) {
      cli::format_inline("strata: {.val {x@strata}}")
    },
    cli::format_inline(
      "quantile levels: {.val {sort(unique(x@predictions$.quantile_level))}}"
    ),
    if (is.null(x@draws)) {
      cli::format_inline("draws: {.emph none} (quantiles only)")
    } else {
      cli::format_inline("draws: {.val {length(unique(x@draws$.draw))}}")
    }
  ))

  if (nrow(x@predictions) > 0) {
    # `format()` on a tibble returns its printed lines, so the table reaches
    # stdout without a bare `print()` call inside package code.
    cli::cat_line(format(utils::head(dplyr::as_tibble(x@predictions), n)))
    if (nrow(x@predictions) > n) {
      cli::cat_line(cli::format_inline(paste0(
        "{cli::symbol$info} {nrow(x@predictions) - n} more row{?s}. ",
        "Use {.code as_tibble()} for all of them."
      )))
    }
  }

  invisible(x)
}

#' Coerce a `tbl_nowcast` into a `tibble`
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' @details
#' Registered in `.onLoad()` rather than assigned with
#' `S7::method(as_tibble, tbl_nowcast) <- `, which is what the neighbouring
#' `print()` and `as.data.frame()` methods use. Assigning an S7 method onto an
#' *imported* generic copies that generic into this namespace, and
#' `tibble::as_tibble()` carries `rownames = pkgconfig::get_config(...)` as a
#' default argument -- so the copy makes `R CMD check` report \pkg{pkgconfig} as
#' an undeclared `::` import of a package this one never uses. Plain S3
#' registration dispatches on `class(x)`, which for an S7 object is
#' `"tbl.now::tbl_nowcast"`, and copies nothing.
#'
#' @description
#' Turns a fitted nowcast into an ordinary `tibble` you can plot, join or write
#' out: one row per event date and quantile level by default, or one row per
#' posterior draw with `type = "draws"`.
#'
#' Not every engine keeps draws. When the backend returned only summarised
#' quantiles, `type = "draws"` has nothing to give you.
#'
#' @param x A [tbl_nowcast].
#' @param ... Unused.
#' @param type Either `"quantiles"` (default) for the tidy quantile predictions,
#'   or `"draws"` for the posterior draws.
#'
#' @return A `tibble`. For `type = "quantiles"`, one row per event date,
#' stratum and quantile level, with the quantile level in `.quantile_level` and
#' the predicted count in `.value`. For `type = "draws"`, one row per draw.
#'
#' @seealso
#' [tidy()][tidy.tbl_nowcast] for the same content with the engine's metadata
#' attached; [autoplot()][autoplot.tbl_nowcast] to plot it;
#' [score_nowcast()] to score it against observed counts.
#'
#' @examples
#' predictions <- data.frame(
#'   onset_week = as.Date("2020-01-05"),
#'   .quantile_level = c(0.5, 0.9), .value = c(10, 14)
#' )
#' nc <- tbl_nowcast(predictions = predictions, method = "toy", event_date = "onset_week")
#' tibble::as_tibble(nc)
#'
#' @name as_tibble.tbl_nowcast
#' @usage NULL
as_tibble_tbl_nowcast <- function(x, ..., type = c("quantiles", "draws")) {
  type <- match.arg(type)
  if (type == "draws") {
    if (is.null(x@draws)) {
      cli::cli_abort(
        "Method {.val {x@method}} did not return posterior draws; only quantiles are available."
      )
    }
    return(dplyr::as_tibble(x@draws))
  }
  dplyr::as_tibble(x@predictions)
}

#' @export
S7::method(as.data.frame, tbl_nowcast) <- function(x, ...) {
  as.data.frame(x@predictions)
}

# Helpers shared by the backends -----

#' Summarise draws into the tidy quantile format
#'
#' @param draws A data frame with the grouping columns plus `.draw` and
#'   `.value`.
#' @param group_cols Character vector of columns identifying a prediction
#'   target (the event date and the strata).
#' @param quantile_levels Numeric vector of probabilities.
#'
#' @return A `tibble` with `group_cols`, `.quantile_level` and `.value`.
#'
#' @keywords internal
#' @noRd
.draws_to_quantiles <- function(draws, group_cols, quantile_levels) {
  quantile_levels <- sort(unique(as.numeric(quantile_levels)))

  draws |>
    dplyr::as_tibble() |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::reframe(
      .quantile_level = quantile_levels,
      .value = unname(stats::quantile(.data$.value, probs = quantile_levels, na.rm = TRUE))
    ) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c(group_cols, ".quantile_level"))))
}

#' Reshape a wide quantile table into the tidy quantile format
#'
#' Backends that only return quantiles (NobBS, say) hand them over as
#' one column per level. This pivots those columns into `.quantile_level` /
#' `.value`.
#'
#' @param data A data frame with `group_cols` plus one column per quantile.
#' @param group_cols Character vector of columns identifying a prediction target.
#' @param quantile_map A named numeric vector: names are the column names in
#'   `data`, values are the corresponding probabilities.
#'
#' @return A `tibble` with `group_cols`, `.quantile_level` and `.value`.
#'
#' @keywords internal
#' @noRd
.wide_to_quantiles <- function(data, group_cols, quantile_map) {
  quantile_map <- quantile_map[names(quantile_map) %in% colnames(data)]
  if (length(quantile_map) == 0) {
    cli::cli_abort("None of the expected quantile columns were found in the backend output.")
  }

  data |>
    dplyr::as_tibble() |>
    dplyr::select(dplyr::all_of(c(group_cols, names(quantile_map)))) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(names(quantile_map)),
      names_to = ".quantile_name", values_to = ".value"
    ) |>
    dplyr::mutate(.quantile_level = unname(quantile_map[.data$.quantile_name])) |>
    dplyr::select(dplyr::all_of(c(group_cols, ".quantile_level", ".value"))) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c(group_cols, ".quantile_level"))))
}
