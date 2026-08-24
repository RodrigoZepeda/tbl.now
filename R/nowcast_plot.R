# A fan chart for a `tbl_nowcast`: nested prediction intervals around the
# median, with the counts reported so far drawn on top.

#' Plot a nowcast
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' Draws a fan chart of a [tbl_nowcast]: one shaded band per central prediction
#' interval, the median as a line, and the counts reported so far as points, so
#' that the size of the correction the model is applying is visible.
#'
#' @param object A [tbl_nowcast] object.
#' @param ... Unused; present for compatibility with [ggplot2::autoplot()].
#' @param levels Numeric vector of central interval widths to shade. Defaults to
#'   the widest intervals available in the object.
#' @param show_reported Logical. Whether to overlay the cases reported up to
#'   `now`. Requires the nowcast to carry its source data.
#' @param colour Colour of the fan.
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
                                              colour = "#0072B2") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg ggplot2} is required for {.fn autoplot}.")
  }

  event_col <- object@event_date
  strata <- object@strata
  predictions <- object@predictions

  available <- sort(unique(predictions$.quantile_level))
  # A central interval only exists when both of its tails were reported.
  candidates <- available[available < 0.5]
  candidates <- candidates[(1 - candidates) %in% available]
  interval_levels <- sort(1 - 2 * candidates)

  if (!is.null(levels)) {
    missing_levels <- setdiff(levels, interval_levels)
    if (length(missing_levels) > 0) {
      cli::cli_abort("Interval{?s} {.val {missing_levels}} {?is/are} not available.")
    }
    interval_levels <- sort(levels)
  }
  if (length(interval_levels) == 0) {
    cli::cli_abort("The nowcast has no symmetric quantile pairs to draw a fan from.")
  }

  bands <- dplyr::bind_rows(lapply(interval_levels, function(level) {
    lower <- (1 - level) / 2
    predictions |>
      dplyr::filter(.data$.quantile_level %in% c(lower, 1 - lower)) |>
      dplyr::mutate(.bound = ifelse(.data$.quantile_level < 0.5, "lower", "upper")) |>
      dplyr::select(dplyr::all_of(c(event_col, strata, ".bound", ".value"))) |>
      tidyr::pivot_wider(names_from = ".bound", values_from = ".value") |>
      dplyr::mutate(.level = level)
  }))
  bands$.level <- factor(bands$.level, levels = sort(interval_levels, decreasing = TRUE))

  median <- predictions |>
    dplyr::filter(abs(.data$.quantile_level - 0.5) < 1e-8)

  plot <- ggplot2::ggplot() +
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

  if (isTRUE(show_reported) && !is.null(object@data)) {
    reported <- nowcast_truth(object@data, strata = strata)
    plot <- plot +
      ggplot2::geom_point(
        data = reported,
        ggplot2::aes(x = .data[[event_col]], y = .data$.observed),
        colour = "black", size = 0.8
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
