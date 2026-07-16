# =============================================================================
# Optional interactivity: every exported plot takes `plotly = TRUE` to return an
# interactive \pkg{plotly} widget (hover, zoom) instead of a static ggplot.
#
# ggplotly() renders the data faithfully (points, tiles, lines all get hover
# tooltips) but is not perfect on heavily annotated static layers -- a few
# `annotate()` texts, rotated axis furniture or `coord_fixed()` details may not
# survive the conversion. The static ggplot remains the reference; plotly is a
# convenience for exploring.
# =============================================================================

#' Abort with a helpful message when plotly is requested but not installed.
#' @keywords internal
#' @noRd
.require_plotly <- function() {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    cli::cli_abort(c(
      "{.arg plotly = TRUE} needs the {.pkg plotly} package.",
      "i" = "Install it with {.code install.packages(\"plotly\")}."
    ))
  }
  invisible(TRUE)
}

#' Return `p` as-is, or as an interactive plotly widget when `plotly = TRUE`.
#' @keywords internal
#' @noRd
.as_plotly <- function(p, plotly = FALSE) {
  if (!isTRUE(plotly)) {
    return(p)
  }
  .require_plotly()
  suppressWarnings(plotly::ggplotly(p))
}

#' Combine panels into a patchwork, or -- when `plotly = TRUE` -- a stacked
#' interactive plotly subplot. `panels` is a list of plain ggplots.
#' @keywords internal
#' @noRd
.combine_panels <- function(panels, plotly = FALSE, ncol = 2L, byrow = FALSE,
                            title = NULL, palette = .tbl_now_palette()) {
  if (isTRUE(plotly)) {
    .require_plotly()
    widgets <- lapply(panels, function(p) suppressWarnings(plotly::ggplotly(p)))
    return(plotly::subplot(widgets, nrows = length(widgets),
                           titleX = TRUE, titleY = TRUE, margin = 0.04))
  }
  out <- patchwork::wrap_plots(panels, ncol = ncol, byrow = byrow)
  if (!is.null(title)) {
    out <- out + patchwork::plot_annotation(
      title = title,
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", colour = palette[["near_black"]])
      )
    )
  }
  out
}
