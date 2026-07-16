# =============================================================================
# EXPERIMENT (devel only -- NOT part of the package).
#
# A "rotated" reporting triangle. The usual reporting triangle plots
# (event date, delay); a batch is then a DIAGONAL there (all the released reports
# share one report date, report = event + delay). Here we rotate that view by
# plotting (event date, report date) instead -- equivalent to a 45-degree shear --
# so that:
#   * a batch (one report date) becomes a HORIZONTAL stripe,
#   * the delay is the vertical distance above the diagonal report = event,
#   * all three quantities (event date, report date, delay) are read off at once,
#     which is the "ternary-like" view.
#
# Reported zeros are left UNCOLOURED (blank); only the not-yet-reportable region
# (report date beyond `now`) is shaded grey. The most obvious batch stripe is
# annotated automatically with a horizontal rule at its report date.
#
# Run with: devtools::load_all("."); source("devel/rotated_triangle.R")
#           plot_rotated_triangle(tn)
# =============================================================================

`%||%` <- function(a, b) if (is.null(a)) b else a

plot_rotated_triangle <- function(x, max_delay = NULL, mark_batches = 1L,
                                  palette = tbl.now:::.tbl_now_palette()) {
  stopifnot(tbl.now::is_tbl_now(x))
  suppressWarnings({
    inc <- tbl.now:::.batch_report_increments(x)
  })

  report_unit <- tbl.now::get_report_units(x) %||% "days"
  event_unit  <- tbl.now::get_event_units(x)  %||% "days"
  unit_days_r <- tbl.now:::.tbl_now_units_to_days(report_unit)
  unit_days_e <- tbl.now:::.tbl_now_units_to_days(event_unit)
  now <- tbl.now::get_now(x) %||% max(inc$.report_date, na.rm = TRUE)
  cap <- max_delay %||% tbl.now:::.diag_delay_cap(inc)

  counts <- inc |>
    dplyr::group_by(.data$.event_date, .data$.report_date) |>
    dplyr::summarise(n = sum(pmax(.data$.count, 0)), .groups = "drop")

  # Complete grid over (event date, delay); derive the report date. A row is
  # "observable" when its report date is on or before `now`; otherwise it is
  # not-yet-reportable (the grey wedge).
  event_grid <- seq(min(inc$.event_date), max(inc$.event_date),
                    by = as.character(event_unit))
  grid <- tidyr::expand_grid(.event_date = event_grid, .delay = seq.int(0L, cap)) |>
    dplyr::mutate(.report_date = .data$.event_date + .data$.delay * unit_days_r) |>
    dplyr::left_join(counts, by = c(".event_date", ".report_date")) |>
    dplyr::mutate(
      n          = dplyr::coalesce(.data$n, 0),
      observable = .data$.report_date <= now
    )

  positives <- dplyr::filter(grid, .data$n > 0, .data$observable)
  future    <- dplyr::filter(grid, !.data$observable)   # yet to be reported -> grey

  # The most obvious batch stripe(s): now a horizontal rule at the report date.
  stripes <- tbl.now:::.diag_batch_stripes(inc, mark_batches)

  p <- ggplot2::ggplot() +
    ggplot2::geom_tile(data = future,
                       ggplot2::aes(.data$.event_date, .data$.report_date),
                       fill = "grey85", width = unit_days_e, height = unit_days_r) +
    ggplot2::geom_tile(data = positives,
                       ggplot2::aes(.data$.event_date, .data$.report_date, fill = .data$n),
                       width = unit_days_e, height = unit_days_r) +
    ggplot2::scale_fill_gradient(name = "reports", low = "grey85",
                                 high = palette[["accent_red"]], trans = "sqrt",
                                 labels = scales::label_comma())

  if (length(stripes) > 0L) {
    p <- p +
      ggplot2::geom_hline(yintercept = stripes, colour = "grey20",
                          linetype = "dashed", linewidth = 0.4) +
      ggplot2::annotate("label", x = min(event_grid), y = stripes,
                        label = format(stripes), hjust = 0, size = 2.6,
                        colour = "grey20", fill = "white", label.size = 0, alpha = 0.75)
  }

  p +
    ggplot2::labs(
      x = "Event date", y = "Report date",
      title = "Rotated reporting triangle",
      subtitle = "Event date, report date and delay in one view (a batch is a horizontal stripe)",
      caption = paste(
        "x = event date, y = report date; the diagonal y = x is delay 0 and delay grows",
        "upward.\nBlank = an observable reported zero; grey = not yet reportable (report",
        "date beyond now).\nA batch releases one report date -> a horizontal stripe; the",
        "dashed rule marks the clearest."
      )
    ) +
    tbl.now:::.tbl_now_theme(palette)
}
