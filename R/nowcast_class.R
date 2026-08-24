# The `tbl_nowcast` class: the common currency of `run_nowcast()`.
#
# Every backend (diseasenowcasting, epinowcast, baselinenowcast, nowcaster,
# NobBS, ...) returns something different -- a Stan fit, a matrix, a list of
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
#' The quantile levels [run_nowcast()] summarises a nowcast at by default.
#' These are the levels used by the US and European forecast hubs, which makes
#' the output directly comparable with hub submissions and with
#' \pkg{scoringutils}.
#'
#' @return A numeric vector of probabilities in `(0, 1)`, sorted increasingly.
#'
#' @examples
#' nowcast_quantile_levels()
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
#' # `tbl_nowcast` objects are normally built by `run_nowcast()`, but the
#' # constructor can be called directly when writing a new backend.
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
#' @param x An object.
#'
#' @return `TRUE` when `x` is a [tbl_nowcast], `FALSE` otherwise.
#'
#' @examples
#' is_tbl_nowcast(1)
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
  n_dates <- length(unique(x@predictions[[x@event_date]]))
  cli::cli_h3("A {.cls tbl_nowcast} from method {.val {x@method}}")
  cli::cli_ul()
  cli::cli_li("now: {.val {x@now}}")
  cli::cli_li("event dates: {.val {n_dates}}")
  if (length(x@strata) > 0) {
    cli::cli_li("strata: {.val {x@strata}}")
  }
  cli::cli_li("quantile levels: {.val {sort(unique(x@predictions$.quantile_level))}}")
  if (is.null(x@draws)) {
    cli::cli_li("draws: {.emph none} (quantiles only)")
  } else {
    cli::cli_li("draws: {.val {length(unique(x@draws$.draw))}}")
  }
  cli::cli_end()

  if (nrow(x@predictions) > 0) {
    print(utils::head(dplyr::as_tibble(x@predictions), n))
    if (nrow(x@predictions) > n) {
      cli::cli_alert_info(
        "{nrow(x@predictions) - n} more row{?s}. Use {.code as_tibble()} for all of them."
      )
    }
  }

  invisible(x)
}

#' Coerce a `tbl_nowcast` into a `tibble`
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' Returns the tidy quantile predictions (the `predictions` property), or the
#' posterior draws when `type = "draws"`.
#'
#' @param x A [tbl_nowcast].
#' @param ... Unused.
#' @param type Either `"quantiles"` (default) or `"draws"`.
#'
#' @return A `tibble`.
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
#' @importFrom tibble as_tibble
#' @export
S7::method(as_tibble, tbl_nowcast) <- function(x, ...,
                                               type = c("quantiles", "draws")) {
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
#' Backends that only return quantiles (NobBS, nowcaster) hand them over as
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
