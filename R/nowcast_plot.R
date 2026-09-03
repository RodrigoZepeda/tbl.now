# A fan chart for a `tbl_nowcast`: nested prediction intervals around the
# median, with the counts reported so far drawn on top.

#' The counts a nowcast's source data had reported by `now`
#'
#' [get_latest_reported_cases()] returns the snapshot as of the object's `now`;
#' this reduces it to one total per event date (and stratum), which is what the
#' plot draws under the fan.
#'
#' @param x The nowcast's source `tbl_now`.
#' @param event_col Name of the event-date column.
#' @param strata Character vector of stratifying columns.
#'
#' @return A tibble with the event date, the strata, and `.observed`.
#'
#' @keywords internal
#' @noRd
.nowcast_reported_counts <- function(x, event_col, strata) {
  strata <- intersect(strata %||% character(0), colnames(x))
  reported <- suppressWarnings(suppressMessages(
    get_latest_reported_cases(ungroup(x))
  ))
  # `.cases_at()` names the count after the object's `case_count`, or
  # `n` when the source was a line list.
  count_col <- get_case_count(reported) %||% "n"

  .declass_tbl_now(reported) |>
    dplyr::as_tibble() |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(event_col, strata)))) |>
    dplyr::summarise(
      .observed = sum(.data[[count_col]], na.rm = TRUE), .groups = "drop"
    )
}

#' Bar width, in days, for one period of a `tbl_now`'s event axis
#'
#' The reported counts are drawn as columns, and a column's width is measured in
#' the units of the x axis -- which is days, whatever the series' own step is. A
#' fixed width is therefore wrong twice over: 0.8 on a WEEKLY series draws
#' hairlines with six-sevenths of the axis empty, and a width wider than the step
#' makes neighbouring bars overlap, which \pkg{ggplot2} resolves by STACKING
#' them, so each bar would show the sum of several periods rather than its own
#' count.
#'
#' @param x The nowcast's source `tbl_now`, or `NULL`.
#'
#' @return A single number: 90% of one period, in days. `0.9` (a daily step) when
#'   there is no object to ask.
#'
#' @keywords internal
#' @noRd
.nowcast_bar_width <- function(x) {
  if (is.null(x)) {
    return(0.9)
  }
  step <- switch(get_event_units(x),
    days = 1, weeks = 7, months = 30, years = 365,
    # A "numeric" axis is already in its own units, so one step is one unit.
    1
  )
  0.9 * step
}

#' Plot a nowcast
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' Draws a fan chart of a [tbl_nowcast]: the counts reported so far as grey
#' columns, one shaded band per central prediction interval over them, and the
#' median as a line, so that the size of the correction the model is applying is
#' visible as the gap between the bars and the fan.
#'
#' @param object A [tbl_nowcast] object.
#' @param ... Unused; present for compatibility with [ggplot2::autoplot()].
#' @param levels Numeric vector of central interval widths to shade. Defaults to
#'   the widest intervals available in the object.
#' @param show_reported Logical. Whether to draw the cases **reported so far**
#'   as columns under the fan -- [get_latest_reported_cases()] on the object's own
#'   source data, so the bars are what the model was actually shown as of `now`,
#'   not what those dates eventually reached. The vertical gap between the top of
#'   a bar and the fan is the correction the nowcast is making, which is the
#'   whole reason to draw it. Requires the nowcast to carry its source data.
#'
#'   The bars are one period wide, taken from [get_event_units()]. A fixed width
#'   would draw hairlines on a weekly series, and one wider than the step would
#'   make \pkg{ggplot2} stack overlapping bars, so each would show several
#'   periods' counts rather than its own.
#' @param colour Colour of the fan. Defaults to the `tbl.now` palette's green:
#'   a nowcast is an estimate of the **epidemic** process (cases by event date),
#'   which the package always draws in green, with red reserved for the
#'   reporting process.
#'
#' @return A `ggplot` object.
#'
#' @seealso [run_nowcast()], [nowcast_ensemble()]
#'
#' @examples
#' predictions <- tidyr::expand_grid(
#'   onset_week = as.Date("2020-01-05") + seq(0, 28, by = 7),
#'   .quantile_level = c(0.05, 0.25, 0.5, 0.75, 0.95)
#' )
#' predictions$.value <- 10 + 30 * predictions$.quantile_level
#' nc <- tbl_nowcast(predictions = predictions, method = "toy", event_date = "onset_week")
#'
#' autoplot(nc)
#'
#' @name autoplot.tbl_nowcast
#' @usage NULL
#' @importFrom ggplot2 autoplot
#' @export
S7::method(autoplot, tbl_nowcast) <- function(object, ..., levels = NULL,
                                              show_reported = TRUE,
                                              colour = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg ggplot2} is required for {.fn autoplot}.")
  }

  palette <- .tbl_now_palette()
  colour <- colour %||% palette[["primary_green"]]

  event_col <- object@event_date
  strata <- object@strata
  predictions <- object@predictions

  available <- sort(unique(predictions$.quantile_level))
  # A central interval only exists when both of its tails were reported. The two
  # tails are carried around explicitly rather than re-derived from the width:
  # `1 - 2 * lower` and `(1 - level) / 2` do NOT round-trip in floating point --
  # `(1 - (1 - 2 * 0.05)) / 2` is 0.049999999999999996 -- so an exact `%in%`
  # matched no rows and every band but the 50% one was silently drawn as NA.
  candidates <- available[available < 0.5]
  upper_tail <- vapply(candidates, function(lower) {
    hit <- available[.near(available, 1 - lower)]
    if (length(hit) == 1) hit else NA_real_
  }, numeric(1))
  candidates <- candidates[!is.na(upper_tail)]
  upper_tail <- upper_tail[!is.na(upper_tail)]

  interval_levels <- 1 - 2 * candidates
  ordering <- order(interval_levels)
  candidates <- candidates[ordering]
  upper_tail <- upper_tail[ordering]
  interval_levels <- interval_levels[ordering]

  if (!is.null(levels)) {
    keep <- vapply(levels, function(level) {
      hit <- which(.near(interval_levels, level))
      if (length(hit) == 0) NA_integer_ else hit[1]
    }, integer(1))
    missing_levels <- levels[is.na(keep)]
    if (length(missing_levels) > 0) {
      cli::cli_abort("Interval{?s} {.val {missing_levels}} {?is/are} not available.")
    }
    keep <- sort(keep)
    candidates <- candidates[keep]
    upper_tail <- upper_tail[keep]
    interval_levels <- interval_levels[keep]
  }
  if (length(interval_levels) == 0) {
    cli::cli_abort("The nowcast has no symmetric quantile pairs to draw a fan from.")
  }

  bands <- dplyr::bind_rows(lapply(seq_along(interval_levels), function(i) {
    lower <- candidates[i]
    upper <- upper_tail[i]
    predictions |>
      dplyr::filter(
        .near(.data$.quantile_level, lower) | .near(.data$.quantile_level, upper)
      ) |>
      dplyr::mutate(.bound = ifelse(.data$.quantile_level < 0.5, "lower", "upper")) |>
      dplyr::select(dplyr::all_of(c(event_col, strata, ".bound", ".value"))) |>
      tidyr::pivot_wider(names_from = ".bound", values_from = ".value") |>
      dplyr::mutate(.level = interval_levels[i])
  }))
  bands$.level <- factor(bands$.level, levels = sort(interval_levels, decreasing = TRUE))

  median <- predictions |>
    dplyr::filter(abs(.data$.quantile_level - 0.5) < 1e-8)

  plot <- ggplot2::ggplot()

  if (isTRUE(show_reported) && !is.null(object@data)) {
    # `get_latest_reported_cases()` -- the counts as they stood at `now`, which
    # is what the model saw. NOT the eventual totals: drawing those would show
    # the answer next to the estimate of it, and the gap the fan is correcting
    # would silently become a gap the fan already knows about.
    reported <- .nowcast_reported_counts(object@data, event_col, strata)
    # Columns rather than points, and added FIRST so the fan draws over them.
    # A bar reads as a count measured from zero, which is what these are, and it
    # turns the correction the nowcast applies into a visible gap between the top
    # of the bar and the fan. Points floated in the middle of the band instead,
    # which reads as a second estimate rather than as the data underneath one.
    plot <- plot +
      ggplot2::geom_col(
        data = reported,
        ggplot2::aes(
          x = .data[[event_col]], y = .data$.observed,
          fill = "Reported by now"
        ),
        width = .nowcast_bar_width(object@data)
      ) +
      ggplot2::scale_fill_manual(
        # Neutral grey, not a palette hue: these are the OBSERVED counts, and
        # colouring them green would put them in the same visual family as the
        # estimate drawn on top of them.
        values = c("Reported by now" = "#dfe1df"), name = NULL
      )
  }

  plot <- plot +
    ggplot2::geom_ribbon(
      data = bands,
      ggplot2::aes(
        x = .data[[event_col]], ymin = .data$lower, ymax = .data$upper,
        alpha = .data$.level
      ),
      fill = colour
    ) +
    ggplot2::scale_alpha_manual(
      # Widest interval palest, so the nested bands read as a fan.
      values = stats::setNames(
        seq(0.15, 0.5, length.out = length(interval_levels)),
        sort(interval_levels, decreasing = TRUE)
      ),
      name = "Interval"
    )

  if (nrow(median) > 0) {
    plot <- plot +
      ggplot2::geom_line(
        data = median,
        ggplot2::aes(x = .data[[event_col]], y = .data$.value),
        colour = colour, linewidth = 0.7
      )
  }

  if (length(strata) > 0) {
    plot <- plot + ggplot2::facet_wrap(strata, scales = "free_y")
  }

  plot +
    ggplot2::labs(
      title = paste0("Nowcast (", object@method, ")"),
      subtitle = if (!is.null(object@now)) paste("as of", object@now) else NULL,
      x = event_col, y = "Cases"
    ) +
    ggplot2::theme_bw()
}
