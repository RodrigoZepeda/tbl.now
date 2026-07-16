# =============================================================================
# The reporting "V": the reporting triangle rotated 45 degrees.
# =============================================================================

#' Plot the reporting "V"
#'
#' `r lifecycle::badge("experimental")`
#'
#' The **same information** as [plot_reporting_triangle()] -- every (event date,
#' delay) cell of the reporting triangle -- drawn in **rotated coordinates**, so
#' that report date runs straight up the page and the data opens into a *V*. Where
#' [plot_reporting_triangle()] uses square axes (event date across, delay up), this
#' view rotates them 45 degrees:
#' \itemize{
#'   \item the vertical axis is `event + delay` = the **report date** (the V opens
#'     upward over report time);
#'   \item the **left arm** (delay 0, where event = report) is the **event-date**
#'     axis;
#'   \item the **right arm** (the earliest event) is the **delay** axis, reading
#'     `0` outward;
#'   \item every **horizontal** line is one report date.
#' }
#' A **batch** -- a single report date releasing a pile of old cases -- is then a
#' single **horizontal streak** across the V, where the reporting triangle would
#' show it as a diagonal. The whole observable triangle is filled: every (event
#' date, delay) that *could* have been reported by `now` is drawn, pale blue where
#' nothing was reported (a genuine zero) and coloured where reports landed.
#'
#' @param x A [tbl_now()] object.
#' @param max_delay Largest delay to draw. `NULL` (default) fills *every* delay
#'   that could have been observed (0 up to `now` minus the earliest event).
#' @param point_size Size of the cell markers. `NULL` (default) scales it to the
#'   number of cells.
#' @param plotly If `TRUE`, return an interactive \pkg{plotly} widget instead of a
#'   static plot. Default `FALSE`.
#' @param palette A named colour palette. Defaults to the package palette.
#'
#' @returns A \pkg{ggplot2} object (or a \pkg{plotly} widget when `plotly = TRUE`).
#'
#' @seealso [plot_reporting_triangle()] for the same data on square axes,
#'   [diagnostic_plot()].
#'
#' @examples
#' data(denguedat)
#' dn <- tbl_now(denguedat, onset_week, report_week, verbose = FALSE)
#' plot_reporting_v(dn)
#'
#' @export
#' @md
plot_reporting_v <- function(x, max_delay = NULL, point_size = NULL,
                            plotly = FALSE, palette = .tbl_now_palette()) {
  .diag_check(x)
  inc        <- .batch_report_increments(x)
  unit       <- .tbl_now_units_to_days(get_report_units(x) %||% "days")
  event_unit <- get_event_units(x) %||% "days"
  now    <- as.numeric(get_now(x) %||% max(inc$.report_date))
  ev_min <- min(as.numeric(inc$.event_date))
  ev_max <- max(as.numeric(inc$.event_date))
  # by default fill EVERY observable delay (0 up to now - earliest event)
  cap    <- max_delay %||% ceiling((now - ev_min) / unit)

  counts <- inc |>
    dplyr::group_by(.data$.event_date, .data$.delay) |>
    dplyr::summarise(n = sum(pmax(.data$.count, 0)), .groups = "drop") |>
    dplyr::filter(.data$n > 0)

  # complete observable grid: (event, delay) with report = event + delay <= now
  event_grid <- seq(min(inc$.event_date), max(inc$.event_date), by = as.character(event_unit))
  grid <- tidyr::expand_grid(.event_date = event_grid, .delay = seq.int(0L, cap)) |>
    dplyr::mutate(report = as.numeric(.data$.event_date) + .data$.delay * unit) |>
    dplyr::filter(.data$report <= now) |>
    dplyr::left_join(counts, by = c(".event_date", ".delay")) |>
    dplyr::mutate(n = dplyr::coalesce(.data$n, 0))

  # Coordinates RELATIVE to the earliest event (working near the origin, not at
  # ~19700 epoch-days, which wrecks coord_fixed). event_rel = event - ev_min.
  erel <- function(d) as.numeric(d) - ev_min
  grid$x <- grid$.delay * unit - erel(grid$.event_date)   # reflected rotated x
  grid$y <- erel(grid$.event_date) + grid$.delay * unit   # report date, relative
  zeros    <- dplyr::filter(grid, .data$n == 0)
  positive <- dplyr::filter(grid, .data$n > 0)

  capd  <- now - ev_min
  e_top <- ev_max - ev_min
  event_arm <- data.frame(x = c(0, -e_top), y = c(0, e_top))   # left arm = event date
  delay_arm <- data.frame(x = c(0, capd),  y = c(0, capd))     # right arm = delay

  # horizontal report-date reference lines + labels (y is relative; label as dates)
  r_breaks  <- pretty(as.Date(c(ev_min, now), origin = "1970-01-01"), n = 6)
  r_breaks  <- r_breaks[as.numeric(r_breaks) >= ev_min & as.numeric(r_breaks) <= now]
  r_rel     <- as.numeric(r_breaks) - ev_min

  # Ticks on each arm, perpendicular and pointing OUTWARD; labels just beyond,
  # rotated orthogonal to the arm; and the arm titles further out still.
  ext       <- max(capd, e_top)
  tick_len  <- 0.02  * ext
  lab_off   <- 0.055 * ext
  title_off <- 0.21  * ext
  s2        <- sqrt(2)

  e_ticks   <- r_breaks[as.numeric(r_breaks) >= ev_min & as.numeric(r_breaks) <= ev_max]
  ev_ticks  <- data.frame(x = -(as.numeric(e_ticks) - ev_min),
                          y = as.numeric(e_ticks) - ev_min, lab = format(e_ticks))
  ev_ticks$xend <- ev_ticks$x - tick_len / s2; ev_ticks$yend <- ev_ticks$y - tick_len / s2
  ev_ticks$lx   <- ev_ticks$x - lab_off  / s2; ev_ticks$ly   <- ev_ticks$y - lab_off  / s2

  d_vals    <- pretty(c(0, cap), n = 4); d_vals <- d_vals[d_vals > 0 & d_vals <= cap]
  dl_ticks  <- data.frame(x = d_vals * unit, y = d_vals * unit, lab = d_vals)
  dl_ticks$xend <- dl_ticks$x + tick_len / s2; dl_ticks$yend <- dl_ticks$y - tick_len / s2
  dl_ticks$lx   <- dl_ticks$x + lab_off  / s2; dl_ticks$ly   <- dl_ticks$y - lab_off  / s2

  # arm titles: at each arm's midpoint, pushed OUTSIDE along the outward normal
  ev_title <- c(x = -e_top / 2 - title_off / s2, y = e_top / 2 - title_off / s2)
  dl_title <- c(x =  capd  / 2 + title_off / s2, y = capd  / 2 - title_off / s2)

  psize <- point_size %||% max(0.25, 3.2 / sqrt(nrow(grid) / 1000))

  p <- ggplot2::ggplot(grid, ggplot2::aes(.data$x, .data$y)) +
    ggplot2::geom_hline(yintercept = r_rel, colour = "grey92", linewidth = 0.3) +
    ggplot2::geom_point(data = zeros, colour = .DIAG_ZERO_COLOUR, shape = 12, size = psize) +
    ggplot2::geom_point(data = positive, ggplot2::aes(colour = .data$n), shape = 12, size = psize) +
    ggplot2::scale_colour_gradient(name = "reports", low = "grey80",
                                   high = palette[["accent_red"]], transform = "sqrt",
                                   labels = scales::label_comma()) +
    ggplot2::geom_line(data = event_arm, ggplot2::aes(.data$x, .data$y), colour = "grey25", linewidth = 0.6) +
    ggplot2::geom_line(data = delay_arm, ggplot2::aes(.data$x, .data$y), colour = "grey25", linewidth = 0.6) +
    ggplot2::geom_segment(data = ev_ticks,
                          ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend),
                          colour = "grey30", linewidth = 0.4) +
    ggplot2::geom_segment(data = dl_ticks,
                          ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend),
                          colour = "grey30", linewidth = 0.4) +
    ggplot2::geom_text(data = ev_ticks, ggplot2::aes(.data$lx, .data$ly, label = .data$lab),
                       angle = 45, hjust = 1, size = 2.6, colour = "grey30") +
    ggplot2::geom_text(data = dl_ticks, ggplot2::aes(.data$lx, .data$ly, label = .data$lab),
                       angle = -45, hjust = 0, size = 2.6, colour = "grey30") +
    ggplot2::annotate("text", x = ev_title[["x"]], y = e_top, angle = -45,
                      label = "event date", fontface = "bold",
                      colour = "grey25", size = 3.4) +
    ggplot2::annotate("text", x = dl_title[["x"]], y = e_top, angle = 45,
                      label = "delay", fontface = "bold",
                      colour = "grey25", size = 3.4) +
    ggplot2::scale_y_continuous(breaks = r_rel, labels = format(r_breaks)) +
    ggplot2::coord_fixed(ratio = 1, clip = "off") +
    ggplot2::labs(
      x = NULL, y = "report date",
      title = "Reporting V",
      subtitle = "# cases indexed by event, delay and report date") +
    .tbl_now_theme(palette) +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_text(face = "bold"),
                   panel.grid = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(hjust = 0.5, vjust = 1, angle = 90),
                   legend.box.margin = ggplot2::margin(0, 0, 0, 20),
                   legend.position = "top",
                   plot.margin = ggplot2::margin(6, 75, 52, 75))
  .as_plotly(p, plotly)
}
