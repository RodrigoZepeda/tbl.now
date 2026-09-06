# =============================================================================
# Wavelet scalograms of the reporting / epidemic series (window-inner).
# =============================================================================

#' The series of reports or of cases, one value per time step, on a common grid.
#' @keywords internal
#' @noRd
.scalo_series <- function(x, type, axis = "report") {
  inc         <- .batch_report_increments(x, axis = axis)
  report_unit <- if (identical(axis, "validation")) {
    get_validation_units(x) %||% get_report_units(x) %||% "days"
  } else {
    get_report_units(x) %||% "days"
  }
  key         <- if (type == "reporting") ".report_date" else ".event_date"
  lo   <- min(c(inc$.event_date, inc$.report_date), na.rm = TRUE)
  hi   <- max(c(inc$.event_date, inc$.report_date), na.rm = TRUE)
  grid <- seq(lo, hi, by = as.character(report_unit))
  agg  <- tapply(pmax(inc$.count, 0), inc[[key]], sum)
  n    <- as.numeric(agg[as.character(grid)])
  n[is.na(n)] <- 0
  data.frame(date = grid, n = n)
}

#' Plot the reporting or epidemic scalogram
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A **wavelet scalogram** splits a count series, at every moment, into fast
#' wiggles (short periods, at the bottom) and slow swings (long periods, at the
#' top), and shows the energy at each as a heat map. A **batch** -- a sudden
#' one-step burst of reports -- lights up as a bright **short-period ridge** in the
#' *reporting* scalogram that the *epidemic* (event) scalogram lacks, since real
#' cases arrive smoothly. Periods are measured in the object's own time step (days,
#' weeks, ...), so the series is analysed on its integer grid, not forced to days.
#'
#' @details
#' This uses a **window-inner** scalogram (\pkg{wavScalogram}, `border_effects =
#' "INNER"`): it is computed from the observed data only, with **no border
#' padding**. That matters for surveillance / nowcasting, where the usual periodic
#' or zero padding would fabricate structure exactly at the most recent ("now")
#' edge we care about. The price is that the estimate near the time edges uses a
#' smaller window (the blank region), so there is no need to hedge what *is* shown.
#'
#' @param x A [tbl_now()] object.
#' @param type `"reporting"` (default; reports by report date) or `"epidemic"`
#'   (cases by event date).
#' @param windowrad Radius of the time window (in report-grid steps). `NULL`
#'   lets \pkg{wavScalogram} choose.
#' @param wname Mother wavelet passed to \pkg{wavScalogram} (e.g. `"PAUL"`,
#'   `"MORLET"`). `"PAUL"` localises a batch more sharply in time.
#' @param format Date format for the x-axis tick labels (see [strftime()]).
#'   Default `"%d/%b/%y"`.
#' @param plotly If `TRUE`, return an interactive \pkg{plotly} widget instead of a
#'   static plot. Default `FALSE`.
#' @param palette A named colour palette (see [tbl_now_palette()]). The
#'   scalogram is drawn entirely with tiles, so it takes no `size` or
#'   `linewidth`.
#'
#' @param axis Which time axis to draw: `"report"` (default) or
#'   `"validation"`. On the validation axis the picture answers the
#'   laboratory's version of the question -- when results arrived, rather than
#'   when reports did. Needs a validation process (see [add_validation_date()]);
#'   cases still `"pending"` have no validation date and are left out.
#' @returns A \pkg{ggplot2} object (or a \pkg{plotly} widget when `plotly = TRUE`).
#'
#' @seealso
#' [plot_reporting_process()][plot_epidemic_process] and
#' [plot_epidemic_process()] for the two series this decomposes;
#' [plot_cycles()] for the same idea pooled over time (a periodogram) rather than
#' resolved moment by moment; [diagnose_batches()] to test a short-period ridge;
#' [diagnostic_plot()] for the whole gallery.
#'
#' @examplesIf requireNamespace("wavScalogram", quietly = TRUE)
#' data(denguedat)
#' dn <- tbl_now(denguedat, onset_week, report_week, verbose = FALSE)
#' plot_scalogram(dn, type = "reporting")
#'
#' @export
#' @md
plot_scalogram <- function(x, type = c("reporting", "epidemic"), windowrad = 1,
                           wname = "PAUL", format = "%d/%b/%y",
                           plotly = FALSE, axis = c("report", "validation"),
                           palette = .tbl_now_palette()) {
  type <- match.arg(type)
  axis <- match.arg(axis)
  .diag_check(x)
  .tbl_now_check_palette(palette, "plot_scalogram")
  if (!requireNamespace("wavScalogram", quietly = TRUE)) {
    cli::cli_abort(c(
      "{.fn plot_scalogram} needs the {.pkg wavScalogram} package.",
      "i" = "Install it with {.code install.packages(\"wavScalogram\")}."
    ))
  }

  s          <- .scalo_series(x, type, axis = axis)
  report_unit <- get_report_units(x) %||% "days"
  fill_col   <- if (type == "reporting") palette[["reporting"]] else palette[["epidemic"]]
  title      <- if (type == "reporting") "Reporting scalogram" else "Epidemic scalogram"

  # Analyse on the *integer* time grid (one step = one report unit), i.e. dt = 1.
  # Passing dt = unit_days makes wavScalogram return `tcentral` in day-units instead
  # of grid indices, which then over-runs the (weekly, monthly, ...) date grid.
  ws <- wavScalogram::windowed_scalogram(
    sqrt(s$n), dt = 1, windowrad = windowrad, border_effects = "INNER",
    wname = wname,
    energy_density = TRUE, makefigure = FALSE, figureperiod = TRUE)

  periods <- ws$scales * ws$fourierfactor          # in report units (dt = 1 unit)
  # `tcentral` is returned as time-grid indices (and, for long series, wavScalogram
  # SUBSAMPLES them, e.g. every 5th step). Map each back to its date, but plot on a
  # *uniform integer* x-axis (the window number) so tiles have a constant width of 1
  # and always touch -- drawing on the Date axis leaves gaps when the centres are
  # spaced several steps apart. The x-axis is then relabelled with the dates.
  dates   <- s$date[round(ws$tcentral)]

  # Normalise per period (divide by that period's median over time) so a localised
  # burst shows over the slow epidemic trend that dominates the long periods.
  P   <- ws$wsc
  med <- apply(P, 2L, function(col) stats::median(col[col > 0], na.rm = TRUE))
  Pn  <- sweep(P, 2L, pmax(med, .Machine$double.eps), "/")

  df <- expand.grid(ti = seq_along(dates), pj = seq_along(periods))
  df$xi     <- df$ti                                # uniform window index (width 1)
  # Plot on log2(period): the wavelet scales are geometric, so this is a regular
  # grid that tiles correctly (linear period is not, and collapses).
  df$logp   <- log2(periods[df$pj])
  df$energy <- log1p(Pn[cbind(df$ti, df$pj)])
  df <- df[is.finite(df$energy), , drop = FALSE]

  # Explicit tile height: the dense wavelet scales make the auto-detected height
  # tiny (tiles collapse to a line), so pass the actual log2-period step.
  hstep <- stats::median(diff(sort(unique(log2(periods)))))
  brks  <- 2^(0:12); brks <- brks[brks >= min(periods) & brks <= max(periods)]
  # Date axis: place a handful of pretty dates at their (interpolated) window index.
  date_breaks <- pretty(dates, n = 6)
  date_breaks <- date_breaks[as.numeric(date_breaks) >= min(as.numeric(dates)) &
                             as.numeric(date_breaks) <= max(as.numeric(dates))]
  xpos <- stats::approx(as.numeric(dates), seq_along(dates),
                        xout = as.numeric(date_breaks), rule = 2)$y

  p <- ggplot2::ggplot(df, ggplot2::aes(.data$xi, .data$logp, fill = .data$energy)) +
    ggplot2::geom_tile(width = 1, height = hstep) +
    ggplot2::scale_x_continuous(breaks = xpos, labels = base::format(date_breaks, format),
                                expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0), breaks = log2(brks), labels = round(brks)) +
    ggplot2::scale_fill_gradient(low = palette[["surface_muted"]], high = fill_col,
                                 name = "rel.\nenergy") +
    ggplot2::labs(x = "Date", y = sprintf("Period (%s)", report_unit), title = title) +
    .tbl_now_theme(palette) +
    # Outside the cone of influence there is no reliable estimate: paint it gray10.
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = palette[["surface_dark"]],
                                                            colour = NA),
                   panel.grid = ggplot2::element_blank())
  .as_plotly(p, plotly)
}
