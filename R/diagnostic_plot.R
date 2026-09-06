# =============================================================================
# diagnostic_plot(): a gallery of reporting-process diagnostics
#
# Each view is also an exported stand-alone function (plot_reporting_process(),
# plot_reporting_triangle(), plot_delay_profiles(), plot_delay_drift(),
# plot_transport_discriminant()), and diagnostic_plot()
# lays the ones you ask for into a two-column patchwork (three rows for "all").
# Every view is facetted by stratum when the tbl_now declares strata.
# =============================================================================

# --- shared count scales, so every count legend looks the same ---------------

#' The count colour scale shared by every diagnostic panel
#'
#' Defined once so that a count means the same colour in every plot the gallery
#' draws.
#'
#' @param palette A named colour palette (see [tbl_now_palette()]).
#' @param aesthetic Which aesthetic to build the scale for.
#'
#' @return A \pkg{ggplot2} scale.
#'
#' @keywords internal
#' @noRd
.diag_count_scale <- function(palette, aesthetic = c("fill", "colour")) {
  aesthetic <- match.arg(aesthetic)
  fun <- if (aesthetic == "fill") ggplot2::scale_fill_gradient else ggplot2::scale_colour_gradient
  fun(name = "reports", low = palette[["guide"]], high = palette[["reporting"]],
      trans = "sqrt", labels = scales::label_comma())
}

#' A y axis with thousands separators
#'
#' Case counts run to five and six figures, which are unreadable unquoted.
#'
#' @return A \pkg{ggplot2} scale.
#'
#' @keywords internal
#' @noRd
.diag_comma_axis <- function() ggplot2::scale_y_continuous(labels = scales::label_comma())

#' Shared context (increments, units, now, delay cap) for the diagnostic panels.
#' @keywords internal
#' @noRd
.diag_context <- function(x, increments, max_delay = NULL, axis = "report") {
  # On the validation axis every "report" in these pictures is a validation,
  # so the unit and the wording follow the axis rather than being hard-coded.
  report_unit <- if (identical(axis, "validation")) {
    get_validation_units(x) %||% get_report_units(x) %||% "days"
  } else {
    get_report_units(x) %||% "days"
  }
  event_unit  <- get_event_units(x) %||% "days"
  list(
    has_strata  = length(get_strata(x)) > 0L,
    axis        = axis,
    arrival     = if (identical(axis, "validation")) "validation" else "report",
    report_unit = report_unit,
    event_unit  = event_unit,
    unit_days   = .tbl_now_units_to_days(report_unit),
    now         = get_now(x) %||% max(increments$.report_date, na.rm = TRUE),
    delay_cap   = max_delay %||% .diag_delay_cap(increments)
  )
}

#' Facet a panel by stratum (string form, so multi-layer panels are safe).
#' @keywords internal
#' @noRd
.diag_facet <- function(panel, has_strata) {
  if (!has_strata) panel else panel + ggplot2::facet_wrap(".stratum")
}

# =============================================================================
# Epidemic and reporting processes
# =============================================================================

#' The epidemic process and the reporting process
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' The same cases, counted on two different clocks. Comparing the two is the
#' single most useful thing you can do to tell a real outbreak from a reporting
#' artifact.
#'
#' * `plot_epidemic_process()` counts by **event date** -- when the cases
#'   actually happened. Epidemics grow and shrink smoothly, so this curve should
#'   be smooth.
#' * `plot_reporting_process()` counts by **report date** -- when news of them
#'   arrived. Reporting is administrative, so this curve is spiky: weekends,
#'   holidays and backlog releases all show up here.
#'
#' A lone spike in the reporting process with nothing under it in the epidemic
#' process is a **batch** -- a day the system cleared its inbox, not a day people
#' got sick. A spike in both is a genuine surge.
#'
#' @details
#' Both are facetted by stratum when the object has strata.
#'
#' @param x A [tbl_now()] object.
#' @param plotly If `TRUE`, return an interactive \pkg{plotly} widget (hover,
#'   zoom) instead of a static \pkg{ggplot2} plot. Default `FALSE`.
#' @param palette A named colour palette (see [tbl_now_palette()]). These two
#'   panels draw bars and nothing else, so they take no `size` or `linewidth`.
#' @param axis Which time axis to draw: `"report"` (default) or
#'   `"validation"`. On the validation axis the picture answers the
#'   laboratory's version of the question -- when results arrived, rather than
#'   when reports did. Needs a validation process (see
#'   [add_validation_date()][add]); cases still `"pending"` have no
#'   validation date and are left out.
#'
#' @returns A \pkg{ggplot2} object (or a \pkg{plotly} widget when `plotly = TRUE`).
#'
#' @seealso
#' [diagnostic_plot()], which draws these alongside the rest of the
#' reporting-process gallery; [plot_observed_cases()] for the epidemic process
#' with the incompleteness cutoff marked; [plot_scalogram()] to separate the two
#' processes by timescale; [diagnose_batches()] to test a suspicious spike rather
#' than eyeball it.
#'
#' @examples
#' data(denguedat)
#' dn <- tbl_now(denguedat, onset_week, report_week, verbose = FALSE)
#'
#' # When cases happened: smooth, because epidemics are.
#' plot_epidemic_process(dn)
#'
#' # When news of them arrived: spikier, because reporting is administrative.
#' plot_reporting_process(dn)
#'
#' @name plot_epidemic_process
#' @export
plot_reporting_process <- function(x, plotly = FALSE, axis = c("report", "validation"),
                                   palette = .tbl_now_palette()) {
  axis <- match.arg(axis)
  .diag_check(x)
  .tbl_now_check_palette(palette, "plot_reporting_process")
  inc <- .batch_report_increments(x, axis = axis)
  ctx <- .diag_context(x, inc)
  .as_plotly(.diag_build_process(inc, ctx, palette, axis = "report"), plotly)
}

#' @rdname plot_epidemic_process
#' @export
plot_epidemic_process <- function(x, plotly = FALSE, axis = c("report", "validation"),
                                  palette = .tbl_now_palette()) {
  axis <- match.arg(axis)
  .diag_check(x)
  .tbl_now_check_palette(palette, "plot_epidemic_process")
  inc <- .batch_report_increments(x, axis = axis)
  ctx <- .diag_context(x, inc)
  .as_plotly(.diag_build_process(inc, ctx, palette, axis = "event"), plotly)
}

#' @keywords internal
#' @noRd
.diag_build_process <- function(increments, ctx, palette, axis) {
  key   <- if (axis == "event") ".event_date" else ".report_date"
  title <- if (axis == "event") "Epidemic process" else "Reporting process"
  sub   <- if (axis == "event") "Cases by the date they occurred" else "Reports by the date they arrived"
  xlab  <- if (axis == "event") "Event date" else "Report date"
  ylab  <- if (axis == "event") "Cases" else "Reports"
  fill  <- if (axis == "event") palette[["epidemic"]] else palette[["reporting"]]
  cap   <- NULL

  totals <- increments |>
    dplyr::group_by(.data$.stratum, .data[[key]]) |>
    dplyr::summarise(n = sum(.data$.count), .groups = "drop")

  # Only when a *pathological* dump dwarfs the whole series (e.g. covid's 1.8M-report
  # day, ~50x the median) does a linear y-axis go mostly empty. There we cap it at
  # the 99th percentile so the ordinary curve fills the panel and the few dumps run
  # off the top. An ordinary batch spike -- the very thing this plot exists to show,
  # like the made-up example's ~20x release -- is NOT capped, so it towers as it
  # should.
  y_med  <- stats::median(totals$n, na.rm = TRUE)
  y_cap  <- stats::quantile(totals$n, 0.99, names = FALSE, na.rm = TRUE)
  capped <- is.finite(y_med) && y_med > 0 && max(totals$n, na.rm = TRUE) > 30 * y_med
  if (capped) {
    cap <- paste(cap, "\nThe y-axis is capped at the 99th percentile; a few extreme",
                 "backlog dumps run off the top.")
  }

  panel <- ggplot2::ggplot(totals, ggplot2::aes(.data[[key]], .data$n)) +
    ggplot2::geom_col(fill = fill, width = ctx$unit_days) +
    .diag_comma_axis() +
    ggplot2::labs(x = xlab, y = ylab, title = title, subtitle = sub, caption = cap) +
    .tbl_now_theme(palette)
  if (capped) {
    panel <- panel + ggplot2::coord_cartesian(ylim = c(0, y_cap))
  }
  .diag_facet(panel, ctx$has_strata)
}

# =============================================================================
# Reporting triangle
# =============================================================================

#' Plot the reporting triangle
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Tiles over (event date, delay), filled by the reported count. Cells that are
#' **observable but empty** (a genuine reported zero) are drawn in a muted blue;
#' cells that are **not yet reportable** (report date beyond `now`, the upper-right
#' wedge) are left blank. A **third axis for report date** is drawn as evenly
#' spaced dashed diagonals (`report = event + delay`) running up-right at 45
#' degrees, so all three quantities -- event date, delay and report date -- can be
#' read off one plot. A batch is a single report date, i.e. one such diagonal.
#'
#' @param x A [tbl_now()] object.
#' @param max_delay Largest delay to draw. `NULL` (default) caps at the delay
#'   covering 99% of reported mass.
#' @param report_ticks Integer: how many evenly spaced report-date diagonals to
#'   draw as the third (report-date) axis. `0` disables it. Default `6`.
#' @param mark_batches Integer: additionally highlight this many of the biggest
#'   batch stripes with a stronger dashed diagonal labelled by report date. `0`
#'   (default) disables it. Found cheaply from volume spikes, not [diagnose_batches()].
#' @param plotly If `TRUE`, return an interactive \pkg{plotly} widget instead of a
#'   static plot. Default `FALSE`.
#' @param size Multiplier on the size of the report-date and batch-stripe
#'   labels. Default `1`.
#' @param grid_linewidth Line width of the iso-report diagonals this function
#'   draws as the third axis -- the package's own grid, not \pkg{ggplot2}'s.
#'   Default `0.3`; the `mark_batches` stripes are drawn a third heavier.
#' @param palette A named colour palette (see [tbl_now_palette()]).
#' @param axis Which time axis to draw: `"report"` (default) or
#'   `"validation"`. On the validation axis the picture answers the
#'   laboratory's version of the question -- when results arrived, rather than
#'   when reports did. Needs a validation process (see [add_validation_date()]);
#'   cases still `"pending"` have no validation date and are left out.
#' @returns A \pkg{ggplot2} object (or a \pkg{plotly} widget when `plotly = TRUE`).
#' @seealso
#' [plot_reporting_hexamap()] for the same grid drawn so that event date, report
#' date and delay are all read the same way; [plot_delay_profiles()] for one
#' curve per date instead of a grid; [complete_zeroes()] to fill the cells that
#' are genuinely zero; [diagnostic_plot()] for the whole gallery.
#'
#' @examples
#' data(denguedat)
#' dn <- tbl_now(denguedat, onset_week, report_week, verbose = FALSE)
#'
#' # Rows are event dates, columns are delays. The blank upper-right wedge is
#' # the future: those reports cannot have arrived yet. That wedge is what a
#' # nowcast fills in.
#' plot_reporting_triangle(dn)
#' @export
#' @md
plot_reporting_triangle <- function(x, max_delay = NULL, report_ticks = 6L,
                                    mark_batches = 0L, plotly = FALSE,
                                    axis = c("report", "validation"),
                                    size = 1, grid_linewidth = 0.3,
                                    palette = .tbl_now_palette()) {
  axis <- match.arg(axis)
  .diag_check(x)
  .tbl_now_check_palette(palette, "plot_reporting_triangle")
  .tbl_now_check_size(size, "size")
  .tbl_now_check_size(grid_linewidth, "grid_linewidth")
  inc <- .batch_report_increments(x, axis = axis)
  ctx <- .diag_context(x, inc, max_delay, axis = axis)
  .as_plotly(.diag_build_triangle(inc, ctx, palette, report_ticks = report_ticks,
                                  mark_batches = mark_batches, size = size,
                                  grid_linewidth = grid_linewidth), plotly)
}

#' Report dates whose volume spikes above a local median: the obvious batch
#' stripes, found cheaply (no `diagnose_batches()`), so the triangle can annotate them.
#' @keywords internal
#' @noRd
.diag_batch_stripes <- function(increments, k) {
  k <- as.integer(k)
  if (is.na(k) || k < 1L) return(as.Date(character(0)))
  totals <- increments |>
    dplyr::group_by(.data$.report_date) |>
    dplyr::summarise(n = sum(pmax(.data$.count, 0)), .groups = "drop") |>
    dplyr::arrange(.data$.report_date)
  if (nrow(totals) < 5L) return(as.Date(character(0)))

  width <- min(15L, nrow(totals))
  if (width %% 2L == 0L) width <- width - 1L
  width <- max(width, 3L)
  local  <- stats::runmed(totals$n, width, endrule = "median")
  ratio  <- totals$n / pmax(local, 1)
  excess <- totals$n - local

  # A clear jump over the local level; rank the qualifying ones by absolute
  # excess so the largest backlog dumps are the ones annotated.
  spikes <- ratio > 2 & excess > 0
  if (!any(spikes)) return(as.Date(character(0)))
  ranked <- order(excess, decreasing = TRUE)
  head(totals$.report_date[ranked[spikes[ranked]]], k)
}

#' The report-date axis: evenly spaced iso-report diagonals (`report = event +
#' delay`) with labels running up-right at 45 degrees -- a third axis for report
#' date on the (event, delay) triangle.
#' @keywords internal
#' @noRd
.diag_report_axis <- function(increments, ctx, event_grid, n_ticks, palette,
                              size = 1, grid_linewidth = 0.3) {
  n_ticks <- as.integer(n_ticks)
  if (is.na(n_ticks) || n_ticks < 1L) return(list())
  unit   <- ctx$unit_days
  cap    <- ctx$delay_cap
  ev_min <- min(event_grid); ev_max <- max(event_grid)
  r_lo   <- min(increments$.report_date, na.rm = TRUE)
  r_hi   <- min(ctx$now, max(increments$.report_date, na.rm = TRUE))
  ticks  <- seq(r_lo, r_hi, length.out = n_ticks)

  iso <- data.frame(slope = -1 / unit, intercept = as.numeric(ticks) / unit)
  # Label at the centre of each diagonal's visible segment inside the panel.
  seg_lo <- pmax(ev_min, ticks - cap * unit)
  seg_hi <- pmin(ev_max, ticks)
  mid_e  <- as.Date((as.numeric(seg_lo) + as.numeric(seg_hi)) / 2, origin = "1970-01-01")
  lab_y  <- as.numeric(ticks - mid_e) / unit
  keep   <- seg_hi >= seg_lo & lab_y >= 0 & lab_y <= cap
  lab    <- data.frame(x = mid_e[keep], y = lab_y[keep], label = format(ticks[keep]))

  list(
    ggplot2::geom_abline(data = iso,
                         ggplot2::aes(slope = .data$slope, intercept = .data$intercept),
                         colour = palette[["guide_strong"]], linetype = "dashed",
                         linewidth = grid_linewidth,
                         inherit.aes = FALSE),
    if (nrow(lab) > 0L) ggplot2::geom_label(
      data = lab, ggplot2::aes(.data$x, .data$y, label = .data$label),
      inherit.aes = FALSE, size = 2.4 * size, colour = palette[["annotation"]],
      fill = palette[["surface"]], label.size = 0, alpha = 0.8, angle = -45)
  )
}

#' @keywords internal
#' @noRd
.diag_build_triangle <- function(increments, ctx, palette, report_ticks = 6L,
                                 mark_batches = 0L, size = 1,
                                 grid_linewidth = 0.3) {
  counts <- increments |>
    dplyr::group_by(.data$.stratum, .data$.event_date, .data$.delay) |>
    dplyr::summarise(n = sum(pmax(.data$.count, 0)), .groups = "drop")

  # Complete the observable grid so reported zeros are distinct from cells that
  # cannot have been reported yet (report date > now). The event-date axis must
  # be the FULL calendar, not just dates that happen to appear in `increments` --
  # an origin with genuinely zero cases never generates a row there, and would
  # otherwise leave a silent gap instead of a zero column.
  event_grid <- seq(min(increments$.event_date), max(increments$.event_date),
                    by = as.character(ctx$event_unit))
  grid <- tidyr::expand_grid(
    .stratum    = sort(unique(increments$.stratum)),
    .event_date = event_grid,
    .delay      = seq.int(0L, ctx$delay_cap)
  ) |>
    dplyr::filter(.data$.event_date + .data$.delay * ctx$unit_days <= ctx$now) |>
    dplyr::left_join(counts, by = c(".stratum", ".event_date", ".delay")) |>
    dplyr::mutate(n = dplyr::coalesce(.data$n, 0))

  zeros <- dplyr::filter(grid, .data$n == 0)
  positive <- dplyr::filter(grid, .data$n > 0)

  unit <- ctx$unit_days

  # The report-date axis: a family of evenly spaced iso-report diagonals labelled
  # by report date, forming a third axis that runs up-right at 45 degrees.
  axis_layers <- .diag_report_axis(increments, ctx, event_grid, report_ticks,
                                   palette, size = size,
                                   grid_linewidth = grid_linewidth)

  # Optionally highlight the biggest batch stripes in a stronger style (off by
  # default now that the full report-date axis is drawn).
  stripes <- .diag_batch_stripes(increments, mark_batches)
  stripe_layers <- list()
  if (length(stripes) > 0L) {
    sdf <- data.frame(slope = -1 / unit, intercept = as.numeric(stripes) / unit)
    # Stagger the label along each stripe's diagonal so nearby report dates do not
    # stack their labels on top of one another.
    mids      <- pmax(round(ctx$delay_cap * seq(0.35, 0.75, length.out = length(stripes))), 1)
    lab_event <- pmin(pmax(stripes - mids * unit, min(event_grid)), max(event_grid))
    lab_y     <- as.numeric(stripes - lab_event) / unit
    keep      <- lab_y >= 0 & lab_y <= ctx$delay_cap
    ldf <- data.frame(x = lab_event[keep], y = lab_y[keep],
                      label = format(stripes[keep]))
    stripe_layers <- list(
      ggplot2::geom_abline(data = sdf,
                           ggplot2::aes(slope = .data$slope, intercept = .data$intercept),
                           colour = palette[["annotation"]], linetype = "dashed",
                           linewidth = 1.33 * grid_linewidth,
                           inherit.aes = FALSE),
      if (nrow(ldf) > 0L) ggplot2::geom_label(
        data = ldf, ggplot2::aes(.data$x, .data$y, label = .data$label),
        inherit.aes = FALSE, size = 2.6 * size, colour = palette[["annotation"]],
        fill = palette[["surface"]], label.size = 0, alpha = 0.75)
    )
  }

  panel <- ggplot2::ggplot() +
    ggplot2::geom_tile(data = zeros,
                       ggplot2::aes(.data$.event_date, .data$.delay),
                       fill = palette[["zero"]]) +
    ggplot2::geom_tile(data = positive,
                       ggplot2::aes(.data$.event_date, .data$.delay, fill = .data$n)) +
    axis_layers +
    stripe_layers +
    .diag_count_scale(palette, "fill") +
    ggplot2::labs(
      x = "Event date", y = sprintf("Delay (%s)", ctx$report_unit),
      title = if (identical(ctx$arrival, "validation")) {
        "Validation triangle"
      } else {
        "Reporting triangle"
      },
      subtitle = sprintf("Event date (x), delay (y) and %s date (diagonal)", ctx$arrival)
    ) +
    .tbl_now_theme(palette)
  .diag_facet(panel, ctx$has_strata)
}

# =============================================================================
# Delay profiles
# =============================================================================

#' Plot the per-date delay profiles
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' One translucent curve per date (see `by`) giving that date's share of reports
#' at each delay, coloured by its mean delay. A batch is a lone right-shifted
#' (long-delay) curve.
#'
#' @param x A [tbl_now()] object.
#' @param by One line per `"report"` date (default) or per `"event"` date.
#' @param max_delay Largest delay to draw. `NULL` (default) caps at the delay
#'   covering 99% of reported mass.
#' @param plotly If `TRUE`, return an interactive \pkg{plotly} widget instead of a
#'   static plot. Default `FALSE`.
#' @param linewidth Multiplier on the width of the per-date curves. Default `1`
#'   (drawn at `0.4`). The curves are deliberately faint and overplotted -- it is
#'   their envelope that carries the message -- so raising this on a long series
#'   fills the panel in.
#' @param palette A named colour palette (see [tbl_now_palette()]).
#' @param axis Which time axis the delay is measured to: `"report"` (default)
#'   or `"validation"`. Both are measured *from the event*, so the two are
#'   directly comparable -- run each in turn and the gap between them is the
#'   time the laboratory adds. (This is not the same quantity as the
#'   `.validation_delay` column, which is the laboratory's own turnaround,
#'   measured from the report.) Needs a validation process (see
#'   [add_validation_date()]); cases still `"pending"` are left out.
#' @returns A \pkg{ggplot2} object (or a \pkg{plotly} widget when `plotly = TRUE`).
#' @seealso
#' [plot_delay_distribution()] for the pooled delay distribution rather than one
#' curve per date; [plot_delay_drift()] for whether those curves move over time;
#' [diagnose_batches2()] for the test behind the eyeball;
#' [diagnostic_plot()] for the whole gallery.
#'
#' @examples
#' data(denguedat)
#' dn <- tbl_now(denguedat, onset_week, report_week, verbose = FALSE)
#' plot_delay_profiles(dn)
#' @export
#' @md
plot_delay_profiles <- function(x, by = c("report", "event"), max_delay = NULL,
                                plotly = FALSE, axis = c("report", "validation"),
                                linewidth = 1,
                                palette = .tbl_now_palette()) {
  by <- match.arg(by)
  axis <- match.arg(axis)
  .diag_check(x)
  .tbl_now_check_palette(palette, "plot_delay_profiles")
  .tbl_now_check_size(linewidth, "linewidth")
  inc <- .batch_report_increments(x, axis = axis)
  ctx <- .diag_context(x, inc, max_delay, axis = axis)
  .as_plotly(.diag_build_profiles(inc, ctx, by, palette, linewidth = linewidth), plotly)
}

#' @keywords internal
#' @noRd
.diag_build_profiles <- function(increments, ctx, by, palette, linewidth = 1) {
  key <- if (identical(by, "report")) ".report_date" else ".event_date"

  profile <- increments |>
    dplyr::group_by(.data$.stratum, .data[[key]], .data$.delay) |>
    dplyr::summarise(n = sum(pmax(.data$.count, 0)), .groups = "drop") |>
    dplyr::group_by(.data$.stratum, .data[[key]]) |>
    dplyr::mutate(
      prop       = .data$n / sum(.data$n),
      mean_delay = sum(.data$.delay * .data$n) / sum(.data$n)
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(is.finite(.data$prop), .data$.delay <= ctx$delay_cap)

  panel <- ggplot2::ggplot(profile, ggplot2::aes(
    .data$.delay, .data$prop,
    group = interaction(.data$.stratum, .data[[key]])
  )) +
    ggplot2::geom_line(colour = palette[["epidemic_dark"]], alpha = 0.15,
                       linewidth = 0.4 * linewidth) +
    ggplot2::labs(
      x = sprintf("Delay (%s)", ctx$report_unit),
      y = sprintf("Share of the date's %ss", ctx$arrival),
      title = if (identical(ctx$arrival, "validation")) {
        "Validation delay profiles"
      } else {
        "Delay profiles"
      },
      caption = paste(
        "Each faint line is one date's reporting-delay distribution.",
        "\nMost lines peak at short delays (fast reporting); a lone line pushed to the",
        "right\nheld its reports back -- a candidate batch."
      )
    ) +
    .tbl_now_theme(palette) +
    ggplot2::theme(panel.grid = ggplot2::element_blank())
  .diag_facet(panel, ctx$has_strata)
}

# =============================================================================
# Transport-discriminant plane
# =============================================================================

#' Plot the transport-discriminant plane
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Places each report date by its creation score (x) and transport / deficit
#' score (y) from [transport_discriminant()], shading the region that decides the
#' batch call. Surges are not distinguished here (they fold into the quiet
#' background) since only the batch call is of interest.
#'
#' @details
#' Only the [diagnose_batches()]-confirmed batches (Benjamini-Hochberg-corrected) are
#' coloured red; the dashed lines and shaded region are a reference for where a
#' batch sits (deficit cleared, and significant), not the flagging rule. The most
#' extreme-looking points (far left, far up) are *holds* -- windows still depleted
#' because the release has not happened yet -- not batches. A genuine batch sits in
#' the band just to the right of the vertical line, once the window total recovers.
#'
#' @param x A [tbl_now()] object.
#' @param ... Passed to [transport_discriminant()] (e.g. `lookback`, `period`,
#'   `alpha`).
#' @param plotly If `TRUE`, return an interactive \pkg{plotly} widget instead of a
#'   static plot. Default `FALSE`.
#' @param size Multiplier on the size of the points and their date labels.
#'   Default `1`: unflagged points are drawn at `1.1`, confirmed batches at
#'   `2.6`. It is a multiplier rather than an absolute size precisely so that
#'   enlarging the marks keeps the flagged ones bigger than the rest.
#' @param grid_linewidth Line width of the zero lines and the dashed
#'   significance thresholds this function draws -- the package's own reference
#'   grid, not \pkg{ggplot2}'s. Default `0.3`.
#' @param palette A named colour palette (see [tbl_now_palette()]).
#' @returns A \pkg{ggplot2} object (or a \pkg{plotly} widget when `plotly = TRUE`).
#' @seealso
#' [transport_discriminant()] for the numbers behind the plane;
#' [diagnose_batches()] for the hypothesis test that flags the red points;
#' [plot_reporting_process()][plot_epidemic_process] for the series they come
#' from; [diagnostic_plot()] for the whole gallery.
#' @examples
#' data(denguedat)
#' dn <- tbl_now(denguedat, onset_week, report_week, verbose = FALSE)
#' plot_transport_discriminant(dn)
#' @export
#' @md
plot_transport_discriminant <- function(x, ..., plotly = FALSE, size = 1,
                                        grid_linewidth = 0.3,
                                        palette = .tbl_now_palette()) {
  .diag_check(x)
  .tbl_now_check_palette(palette, "plot_transport_discriminant")
  .tbl_now_check_size(size, "size")
  .tbl_now_check_size(grid_linewidth, "grid_linewidth")
  td   <- transport_discriminant(x, ...)
  dots <- list(...)
  # The hover costs a second pass over the data and only a widget can show it,
  # so a static plot does not pay for it. `tooltip = "text"` then puts the dates
  # in the tooltip instead of the two z-scores the point is positioned by.
  if (isTRUE(plotly)) {
    axis <- match.arg(dots$axis %||% "report", c("report", "validation"))
    td   <- .diag_transport_hover(td, x, axis, dots$drop_censored %||% TRUE)
  }
  .as_plotly(.diag_build_transport(td, palette, hover = isTRUE(plotly),
                                   size = size, grid_linewidth = grid_linewidth),
             plotly, tooltip = "text")
}

#' Attach the hover label for the transport plane.
#'
#' A point is one *arrival* date, so the report (or validation) date names it.
#' The event dates are the other half of the question the plane asks -- a
#' backlog release reports old events -- so the mean event date behind that
#' date's arrivals, and the delay it implies, go in the label too.
#'
#' @param td A `transport_discriminant` tibble.
#' @param x The `tbl_now` it came from.
#' @param axis The axis it was computed on.
#' @param drop_censored Whether censored rows were dropped, so the label
#'   describes the same rows the statistics do.
#'
#' @returns `td` with a `.hover` column.
#' @keywords internal
#' @noRd
.diag_transport_hover <- function(td, x, axis, drop_censored) {
  increments <- .batch_report_increments(x, axis = axis,
                                         drop_censored = drop_censored)
  # Only arrivals carry an event date to average; a down-revision removes cases
  # rather than announcing any.
  arrivals <- dplyr::filter(increments, .data$.count > 0)

  per_date <- arrivals |>
    dplyr::group_by(.data$.stratum, .data$.report_date) |>
    dplyr::summarise(
      # Date arithmetic is not defined for a weighted mean, so average the
      # numeric representation and put the class back afterwards.
      .mean_event_num = stats::weighted.mean(as.numeric(.data$.event_date),
                                             w = .data$.count),
      .mean_delay = stats::weighted.mean(.data$.delay, w = .data$.count),
      .groups = "drop"
    )
  per_date$.mean_event <- if (inherits(arrivals$.event_date, "Date")) {
    as.Date(round(per_date$.mean_event_num), origin = "1970-01-01")
  } else {
    per_date$.mean_event_num
  }
  per_date$.mean_event_num <- NULL

  td <- dplyr::left_join(
    td, per_date,
    by = c("stratum" = ".stratum", "report_date" = ".report_date")
  )

  arrival_label <- if (identical(axis, "validation")) {
    "Validation date"
  } else {
    "Report date"
  }
  td$.hover <- paste0(
    arrival_label, ": ", format(td$report_date),
    "\nMean event date: ",
    ifelse(is.na(td$.mean_event), "no arrivals", format(td$.mean_event)),
    "\nMean delay: ",
    ifelse(is.na(td$.mean_delay), "-", sprintf("%.1f", td$.mean_delay)),
    "\nArrived: ", format(round(td$reported, 1), trim = TRUE),
    " (baseline ", format(round(td$baseline, 1), trim = TRUE), ")",
    if ("stratum" %in% names(td)) paste0("\nStratum: ", td$stratum) else ""
  )
  td
}

#' `.diag_transport_hover()`, but only when a widget will read the label.
#' @keywords internal
#' @noRd
.diag_transport_hover_if <- function(td, x, axis, drop_censored, wanted) {
  if (!isTRUE(wanted)) return(td)
  .diag_transport_hover(td, x, axis, drop_censored)
}

#' @keywords internal
#' @noRd
.diag_build_transport <- function(td, palette, hover = FALSE, size = 1,
                                  grid_linewidth = 0.3) {
  td$.stratum <- td$stratum
  z_star <- stats::qnorm(1 - attr(td, "alpha"))
  red    <- palette[["reporting"]]

  # Colour ONLY the diagnose_batches()-confirmed batches (BH-corrected `batch`), not
  # the raw per-point classification: at level alpha the raw quadrants paint
  # ~10-20% of points batch/surge/hold by construction, ignoring multiplicity and
  # the heavy autocorrelation of the window statistics. The dashed lines and the
  # shaded batch region are drawn only as a reference for where a batch would sit
  # (deficit cleared -- right of -z*; and significant -- above z*); they are not
  # the flagging rule.
  batches <- td[td$batch %in% TRUE, , drop = FALSE]

  # Limit the y-axis below the lowest confirmed batch: the deep-negative "hold"
  # tail is not of interest and otherwise squashes the batches into a sliver at
  # the top. Use the default clipping (NOT clip = "off") so cropped points stop at
  # the panel edge instead of bleeding into the plot below.
  finite_ty <- td$transport_z[is.finite(td$transport_z)]
  top   <- max(c(finite_ty, z_star), na.rm = TRUE)
  y_lo  <- if (nrow(batches) > 0L) {
    min(c(batches$transport_z, 0), na.rm = TRUE) - 2 * z_star
  } else {
    as.numeric(stats::quantile(finite_ty, 0.10, na.rm = TRUE))
  }
  # Headroom above the highest point so its date label is not clipped at the top.
  y_hi <- top + 0.18 * (top - y_lo)

  # `text` is not a ggplot2 aesthetic -- it is what `ggplotly(tooltip = "text")`
  # reads -- so it is added only when a widget is being built, or ggplot2 warns
  # about it on every static plot.
  point_aes <- if (isTRUE(hover) && ".hover" %in% names(td)) {
    ggplot2::aes(text = .data$.hover)
  } else {
    ggplot2::aes()
  }

  panel <- ggplot2::ggplot(td, ggplot2::aes(.data$creation_z, .data$transport_z)) +
    ggplot2::geom_hline(yintercept = 0, colour = palette[["guide"]],
                        linewidth = grid_linewidth) +
    ggplot2::geom_vline(xintercept = 0, colour = palette[["guide"]],
                        linewidth = grid_linewidth) +
    ggplot2::geom_hline(yintercept = z_star, linetype = "dashed",
                        colour = palette[["grid_major"]],
                        linewidth = 1.17 * grid_linewidth) +
    ggplot2::geom_vline(xintercept = -z_star, linetype = "dashed",
                        colour = palette[["grid_major"]],
                        linewidth = 1.17 * grid_linewidth) +
    ggplot2::geom_point(point_aes,
                        colour = palette[["neutral"]], alpha = 0.4, size = 1.1 * size, na.rm = TRUE) +
    ggplot2::geom_point(data = batches, point_aes,
                        colour = red, alpha = 0.9, size = 2.6 * size) +
    ggplot2::geom_label(data = batches, ggplot2::aes(label = format(.data$report_date)),
                        colour = palette[["ink_inverse"]], fill = red, label.size = 0, alpha = 0.95,
                        size = 3.2 * size, fontface = "bold", vjust = -0.6, na.rm = TRUE) +
    ggplot2::labs(
      # Plain strings, not `expression()`: plotly cannot render a plotmath
      # label and silently drops it, leaving the interactive plot with no axis
      # names at all.
      x = "Creation z",
      y = "Transport z",
      title = "Transport discriminant"
    ) +
    ggplot2::coord_cartesian(ylim = c(y_lo, y_hi)) +
    .tbl_now_theme(palette)
  .diag_facet(panel, "stratum" %in% names(td) && length(unique(td$.stratum)) > 1L)
}

# =============================================================================
# The gallery
# =============================================================================

#' Diagnostic plots of the reporting process
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Lays out a gallery of complementary views of a `tbl_now`'s reporting process,
#' all aimed at spotting reporting artefacts -- especially *batch reporting*. Each
#' view is also available on its own (see **See also**); `diagnostic_plot()` picks
#' the ones named in `panels` and combines them with \pkg{patchwork}. Selecting a
#' single panel returns it as a plain plot. Every view is facetted by stratum when
#' the `tbl_now` declares strata.
#'
#' @param x A [tbl_now()] object.
#' @param panels Which panels, `"all"` (default) or any subset of `"reporting"`,
#'   `"triangle"`, `"profiles"`, `"delay_drift"` and `"transport"`.
#' @param by For the `"profiles"` panel, one mark per `"report"` date (default) or
#'   per `"event"` date.
#' @param max_delay Largest delay on the delay-based panels. `NULL` (default) caps
#'   at the delay covering 99% of reported mass.
#' @param ... Batch controls (`lookback`, `period`, `alpha`) routed to the
#'   `"transport"` panel.
#' @param plotly If `TRUE`, return an interactive \pkg{plotly} widget (the panels
#'   stacked) instead of a static \pkg{patchwork}. Default `FALSE`.
#' @param size Multiplier on point and label sizes, forwarded to every panel
#'   that draws them (`"triangle"`, `"transport"`). Default `1`.
#' @param linewidth Multiplier on data line widths, forwarded to every panel that
#'   draws them (`"profiles"`, `"delay_drift"`). Default `1`.
#' @param grid_linewidth Line width of the reference grids the package draws
#'   itself -- not \pkg{ggplot2}'s panel grid. Forwarded to `"triangle"`,
#'   `"transport"` and `"delay_drift"`. Default `0.3`.
#' @param palette A named colour palette (see [tbl_now_palette()]).
#'
#' @param axis Which time axis the delay is measured to: `"report"` (default)
#'   or `"validation"`. Both are measured *from the event*, so the two are
#'   directly comparable -- run each in turn and the gap between them is the
#'   time the laboratory adds. (This is not the same quantity as the
#'   `.validation_delay` column, which is the laboratory's own turnaround,
#'   measured from the report.) Needs a validation process (see
#'   [add_validation_date()]); cases still `"pending"` are left out.
#' @returns A \pkg{patchwork} object, or a single plot when one panel is selected
#'   (or a \pkg{plotly} widget when `plotly = TRUE`).
#'
#' @seealso
#' Every panel is also a function of its own:
#' [plot_reporting_process()][plot_epidemic_process] and
#' [plot_epidemic_process()] (when reports arrived, versus when cases happened),
#' [plot_reporting_triangle()] (the full event-by-delay grid),
#' [plot_delay_profiles()] (each date's delay curve),
#' [plot_delay_drift()] (whether delays are getting longer),
#'   [plot_transport_discriminant()], [plot_scalogram()].
#'
#' @examplesIf requireNamespace("patchwork", quietly = TRUE)
#' data(denguedat)
#' dn <- tbl_now(denguedat, onset_week, report_week, verbose = FALSE)
#' diagnostic_plot(dn, panels = c("triangle", "transport"))
#'
#' @export
#' @md
diagnostic_plot <- function(x,
                            panels    = "all",
                            by        = c("report", "event"),
                            max_delay = NULL,
                            ...,
                            plotly    = FALSE,
                            axis      = c("report", "validation"),
                            size      = 1,
                            linewidth = 1,
                            grid_linewidth = 0.3,
                            palette   = .tbl_now_palette()) {
  by <- match.arg(by)
  axis <- match.arg(axis)
  .diag_check(x)
  .tbl_now_check_palette(palette, "diagnostic_plot")
  .tbl_now_check_size(size, "size")
  .tbl_now_check_size(linewidth, "linewidth")
  .tbl_now_check_size(grid_linewidth, "grid_linewidth")
  keys <- .diag_resolve_panels(panels)
  if (length(keys) > 1L && !isTRUE(plotly) && !requireNamespace("patchwork", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg patchwork} is required to combine panels.")
  }

  dots <- list(...)
  # No `drop_censored` here: censoring is a statement about the arrival axis, so
  # the panels that draw cases and delays keep every row. Only the `transport`
  # panel drops them, through `transport_discriminant()` below.
  inc <- .batch_report_increments(x, axis = axis)
  ctx <- .diag_context(x, inc, max_delay, axis = axis)

  # `...` may carry batch controls (lookback, period, alpha, ...); route to each
  # panel only the arguments it actually accepts.
  pass <- function(fn) dots[names(dots) %in% names(formals(fn))]

  build_one <- function(key) {
    switch(key,
      reporting   = .diag_build_process(inc, ctx, palette, "report"),
      triangle    = .diag_build_triangle(inc, ctx, palette, size = size,
                                         grid_linewidth = grid_linewidth),
      profiles    = .diag_build_profiles(inc, ctx, by, palette,
                                         linewidth = linewidth),
      transport   = .diag_build_transport(
        .diag_transport_hover_if(
          do.call(transport_discriminant,
                  c(list(x, axis = axis), pass(transport_discriminant))),
          x, axis, dots$drop_censored %||% TRUE, isTRUE(plotly)
        ),  # `drop_censored` defaults TRUE inside transport_discriminant()
        palette, hover = isTRUE(plotly), size = size,
        grid_linewidth = grid_linewidth
      ),
      delay_drift = plot_delay_drift(x, by_strata = ctx$has_strata, axis = axis,
                                     linewidth = linewidth,
                                     grid_linewidth = grid_linewidth,
                                     palette = palette)
    )
  }

  built <- lapply(keys, build_one)
  if (length(built) == 1L) {
    return(.as_plotly(built[[1L]], plotly))
  }

  # Two columns, filled column-major so the panels stack down each column in the
  # order given (for "all": reporting / triangle / profiles on the left, delay
  # drift / transport on the right). `plotly = TRUE` stacks them interactively.
  .combine_panels(built, plotly = plotly, ncol = 2L, byrow = FALSE,
                  title = "Diagnostic plots", palette = palette)
}

# =============================================================================
# Shared helpers
# =============================================================================

#' Abort unless `x` is a tbl_now.
#' @keywords internal
#' @noRd
.diag_check <- function(x) {
  if (!is_tbl_now(x)) {
    cli::cli_abort("{.arg x} must be a {.cls tbl_now}.")
  }
  invisible(x)
}

#' Resolve the `panels` argument into ordered concrete panel keys.
#' @keywords internal
#' @noRd
.diag_resolve_panels <- function(panels) {
  all_keys <- c("reporting", "triangle", "profiles", "delay_drift", "transport")
  if (is.null(panels)) panels <- "all"
  if (!is.character(panels)) {
    cli::cli_abort("{.arg panels} must be a character vector of panel names.")
  }
  if ("all" %in% panels) {
    return(all_keys)
  }
  unknown <- setdiff(panels, all_keys)
  if (length(unknown) > 0L) {
    cli::cli_abort(c(
      "Unknown panel{?s}: {.val {unknown}}.",
      "i" = "Choose from {.val {all_keys}} or {.val all}."
    ))
  }
  all_keys[all_keys %in% panels]
}

#' Delay covering 99% of the reported mass, so the sparse tail is trimmed.
#' @keywords internal
#' @noRd
.diag_delay_cap <- function(increments) {
  positive <- increments[increments$.count > 0, , drop = FALSE]
  if (nrow(positive) == 0L) {
    return(max(increments$.delay, na.rm = TRUE))
  }
  by_delay <- tapply(positive$.count, positive$.delay, sum)
  by_delay <- by_delay[order(as.integer(names(by_delay)))]
  cap      <- as.integer(names(by_delay))[which(cumsum(by_delay) / sum(by_delay) >= 0.99)[1]]
  max(cap, 7L)
}

