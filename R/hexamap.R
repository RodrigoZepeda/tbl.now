# =============================================================================
# Age-period-cohort HEXAMAP of the reporting triangle (Jalal-Burke projection)
# =============================================================================
# event / report / delay are an age-period-cohort triple (report = event + delay
# is exactly period = cohort + age), with the correspondence
#     age = delay,  period = report date,  cohort = event date.
# We use the projection of Jalal & Burke (2020) / APCtools::plot_APChexamap,
#     x = period * sqrt(3)/2 ,   y = age - period/2 ,
# whose vertical extent comes from the cohort (event) shear rather than the short
# delay axis, so the very different axis ranges do not squash the picture. A batch
# is a single report date -- a constant-period line -- hence a VERTICAL stripe.
# =============================================================================

#' Plot the reporting triangle as an age-period-cohort hexamap
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Draws the reporting triangle as a hexagonal age-period-cohort map, using the
#' projection of Jalal and Burke (2020). Event date, report date and reporting
#' delay are the cohort, period and age of the map (`report = event + delay`), and
#' each `(event, delay)` cell is one hexagon coloured by its report count. Because
#' a batch is a single **report date**, it appears as a clean **vertical stripe**;
#' the fast-reporting bulk sits along the short-delay bottom edge.
#'
#' @details
#' The three axes are read off three families of iso-lines: **report date**
#' (period) runs vertically, **delay** (age) up the right-hand spine, and **event
#' date** (cohort) up the left. A major/minor triangular grid is drawn so any
#' hexagon can be traced back to its event date, report date and delay.
#'
#' The number of hexagons is `#\{observed (event, delay) cells\}`, which grows with
#' the delay range. To stay responsive the delay axis is capped so at most
#' `max_cells` hexagons are drawn (raise `max_cells`, or set `max_delay`, to change
#' this). `complete = TRUE` first fills the whole observable triangle with explicit
#' zeros (via [complete_zeroes()]) so the empty cells are shown in grey.
#'
#' @param x A [tbl_now()] object.
#' @param max_delay Largest delay (in report units) to draw. `NULL` (default) shows
#'   the observed range, auto-capped to respect `max_cells`.
#' @param complete If `TRUE`, fill the whole observable triangle with zeros so the
#'   grey background shows every observable cell. Default `FALSE` (observed cells
#'   only). Coerces linelist input to counts via [to_count()].
#' @param iso,iso_minor Major and minor grid spacings (in report units). `NULL`
#'   picks sensible defaults from the data.
#' @param format Date format for the event/report tick labels (see [strftime()]).
#'   Default `"%d/%b/%y"`.
#' @param max_cells Safety cap on the number of hexagons. Default `12000`.
#' @param trans Fill transform for the count scale. Default `"sqrt"`.
#' @param palette A named colour palette. Defaults to the package palette.
#'
#' @param axis Which time axis to draw: `"report"` (default) or
#'   `"validation"`. On the validation axis the picture answers the
#'   laboratory's version of the question -- when results arrived, rather than
#'   when reports did. Needs a validation process (see [add_validation_date()]);
#'   cases still `"pending"` have no validation date and are left out.
#' @returns A \pkg{ggplot2} object.
#'
#' @references Jalal, H. and Burke, D. S. (2020). Hexamaps for Age-Period-Cohort
#'   Data Visualization. *Epidemiology* **31**, e47-e49.
#'
#' @seealso
#' [plot_reporting_triangle()] for the same data on ordinary axes, where the
#' third quantity has to be read off the diagonals; [diagnostic_plot()] for the
#' whole gallery.
#'
#' @examples
#' data(denguedat)
#' dn <- tbl_now(denguedat, onset_week, report_week, verbose = FALSE)
#' plot_reporting_hexamap(dn)
#'
#' @export
#' @md
plot_reporting_hexamap <- function(x, max_delay = NULL, complete = FALSE,
                                   iso = NULL, iso_minor = NULL,
                                   format = "%d/%b/%y", max_cells = 12000L,
                                   trans = "sqrt", axis = c("report", "validation"),
                                   palette = .tbl_now_palette()) {
  axis <- match.arg(axis)
  .diag_check(x)
  report_unit <- get_report_units(x) %||% "days"
  unit_days   <- .tbl_now_units_to_days(report_unit)

  xin <- x
  if (isTRUE(complete)) {
    if (identical(get_data_type(x), "linelist")) {
      xin <- to_count(ungroup(x), to = "count-incidence")
    }
    inc0   <- .batch_report_increments(xin, axis = axis)
    now0   <- get_now(x) %||% max(inc0$.report_date, na.rm = TRUE)
    min_ev <- min(inc0$.event_date, na.rm = TRUE)
    full_d <- max_delay %||% as.integer(round((as.numeric(now0) - as.numeric(min_ev)) / unit_days))
    xin    <- suppressWarnings(complete_zeroes(xin, max_delay = full_d))
  }

  inc  <- .batch_report_increments(xin, axis = axis)
  lo   <- min(inc$.event_date, na.rm = TRUE)
  now  <- get_now(x) %||% max(inc$.report_date, na.rm = TRUE)
  grid <- seq(lo, max(inc$.report_date, na.rm = TRUE), by = as.character(report_unit))

  cells <- inc |>
    dplyr::mutate(.count = pmax(.data$.count, 0)) |>
    dplyr::group_by(.data$.event_date, .data$.delay) |>
    dplyr::summarise(n = sum(.data$.count), .groups = "drop")
  cells$t <- match(cells$.event_date, grid) - 1L
  cells$d <- as.integer(cells$.delay)
  cells$r <- cells$t + cells$d
  now_idx <- sum(grid <= now) - 1L
  cells <- cells[cells$r <= now_idx, , drop = FALSE]
  if (!is.null(max_delay))   cells <- cells[cells$d <= max_delay, , drop = FALSE]
  if (!isTRUE(complete))     cells <- cells[cells$n > 0, , drop = FALSE]
  if (nrow(cells) == 0L) cli::cli_abort("No observable cells to draw.")

  # Safety cap: never expand more than `max_cells` hexagons into polygons. If the
  # requested delay range would exceed it, lower the delay cap (keeping the low
  # delays where the reports are) and tell the user.
  if (nrow(cells) > max_cells) {
    # Keep the largest delay whose whole band still fits under the cap. Taking
    # the delay at position `max_cells` and keeping `d <= that` does NOT bound
    # the result: every cell sharing that delay comes along too, so a wide band
    # at the cut overshoots.
    per_delay <- table(cells$d)
    fits      <- which(cumsum(per_delay) <= max_cells)
    d_cut     <- if (length(fits)) {
      as.integer(names(per_delay)[max(fits)])
    } else {
      min(cells$d)
    }
    cells <- cells[cells$d <= d_cut, , drop = FALSE]

    # If even the shortest delay alone overflows the cap there is no delay cut
    # that helps, so trim the remaining cells outright rather than silently
    # exceeding a documented bound.
    if (nrow(cells) > max_cells) {
      cells <- cells[order(cells$d, cells$.event_date), , drop = FALSE][seq_len(max_cells), ]
    }
    cli::cli_inform(c("i" = "Capped the delay axis at {d_cut} {report_unit} to keep \\
                             {.arg max_cells} = {max_cells} hexagons; raise {.arg max_cells} \\
                             or set {.arg max_delay} to change this."))
  }

  # --- Jalal-Burke projection --------------------------------------------------
  a  <- 1 / sqrt(3)
  px <- function(r) r * sqrt(3) / 2
  py <- function(r, d) d - r / 2
  cells$cx <- px(cells$r); cells$cy <- py(cells$r, cells$d)

  # Vectorised flat-top hexagons: one geom_polygon, no per-cell loop.
  b  <- sqrt(3) / 2 * a
  xv <- c(-a, -a / 2, a / 2, a, a / 2, -a / 2)
  yv <- c(0,  b,      b,     0, -b,    -b)
  nC <- nrow(cells)
  polys <- data.frame(
    x  = as.vector(outer(cells$cx, xv, "+")),
    y  = as.vector(outer(cells$cy, yv, "+")),
    id = rep.int(seq_len(nC), 6L),
    n  = rep.int(cells$n, 6L)
  )

  tmin <- min(cells$t); tmax <- max(cells$t)
  dcap <- max(cells$d); rmin <- min(cells$r); rmax <- max(cells$r)
  fmt  <- function(idx) format(lo + round(idx * unit_days), format)

  # --- triangular grid following the projection --------------------------------
  grid_family <- function(step) {
    ri <- seq(ceiling(rmin / step) * step, rmax, by = step)      # report -> vertical
    dlo <- pmax(0, ri - tmax); dhi <- pmin(dcap, ri - tmin); ok <- dlo <= dhi
    rep_l <- data.frame(x = px(ri[ok]), y = py(ri[ok], dlo[ok]),
                        xend = px(ri[ok]), yend = py(ri[ok], dhi[ok]), v = ri[ok])
    di <- seq(0, dcap, by = step)                                # delay -> down-right
    rlo <- tmin + di; rhi <- pmin(tmax + di, now_idx); ok <- rlo <= rhi
    del_l <- data.frame(x = px(rlo[ok]), y = py(rlo[ok], di[ok]),
                        xend = px(rhi[ok]), yend = py(rhi[ok], di[ok]), v = di[ok])
    ti <- seq(tmin, tmax, by = step)                             # event -> up-right
    dh <- pmin(dcap, now_idx - ti); ok <- dh >= 0
    ev_l <- data.frame(x = px(ti[ok]), y = py(ti[ok], 0),
                       xend = px(ti[ok] + dh[ok]), yend = py(ti[ok] + dh[ok], dh[ok]), v = ti[ok])
    list(rep = rep_l, del = del_l, ev = ev_l)
  }
  iso       <- iso       %||% max(1L, round(diff(range(cells$r)) / 8))
  iso_minor <- iso_minor %||% max(1L, round(iso / 5))
  gmaj <- grid_family(iso); gmin <- grid_family(iso_minor)

  # --- delay axis: a clean diagonal spine so labels sit OUTSIDE the hexes ------
  # The delay axis is the event iso-line at the largest event; offset it outward
  # (down-right normal) so the "0" is not eaten by the last hexagon.
  nrm  <- c(0.5, -sqrt(3) / 2)               # outward normal of the delay axis
  offs <- 1.0
  d_ax  <- seq(0, dcap, by = iso)
  ax_x  <- px(tmax + d_ax) + offs * nrm[1]
  ax_y  <- py(tmax + d_ax, d_ax) + offs * nrm[2]
  delay_spine <- data.frame(x = ax_x[1], y = ax_y[1],
                            xend = ax_x[length(ax_x)], yend = ax_y[length(ax_y)])
  delay_ticks <- data.frame(x = ax_x, y = ax_y,
                            xend = ax_x + 0.35 * nrm[1], yend = ax_y + 0.35 * nrm[2])
  delay_lab   <- data.frame(x = ax_x + 1.1 * nrm[1], y = ax_y + 1.1 * nrm[2],
                            lab = as.character(d_ax))

  rep_lab <- data.frame(x = px(gmaj$rep$v), y = gmaj$rep$yend, lab = fmt(gmaj$rep$v))
  ev_lab  <- data.frame(x = gmaj$ev$x,      y = gmaj$ev$y,     lab = fmt(gmaj$ev$v))

  seg_aes <- ggplot2::aes(.data$x, .data$y, xend = .data$xend, yend = .data$yend)
  x_span  <- diff(range(polys$x)); y_span <- diff(range(polys$y))
  nb      <- palette[["near_black"]]

  ggplot2::ggplot() +
    ggplot2::geom_segment(data = gmin$rep, seg_aes, colour = "grey88", linewidth = 0.15) +
    ggplot2::geom_segment(data = gmin$del, seg_aes, colour = "grey88", linewidth = 0.15) +
    ggplot2::geom_segment(data = gmin$ev,  seg_aes, colour = "grey88", linewidth = 0.15) +
    ggplot2::geom_segment(data = gmaj$rep, seg_aes, colour = "grey60", linewidth = 0.3) +
    ggplot2::geom_segment(data = gmaj$del, seg_aes, colour = "grey60", linewidth = 0.3) +
    ggplot2::geom_segment(data = gmaj$ev,  seg_aes, colour = "grey60", linewidth = 0.3) +
    ggplot2::geom_polygon(data = polys, ggplot2::aes(.data$x, .data$y, group = .data$id, fill = .data$n), colour = NA) +
    ggplot2::scale_fill_gradient(low = "grey93", high = palette[["accent_red"]],
                                 trans = trans, name = "reports", labels = scales::label_comma(),
                                 guide = ggplot2::guide_colourbar(
                                   title.position = "top", title.hjust = 0.5,
                                   barwidth = grid::unit(7, "cm"), barheight = grid::unit(0.4, "cm"))) +
    # delay axis spine (the diagonal "\\" edge), its ticks and outside labels
    ggplot2::geom_segment(data = delay_spine, seg_aes, colour = nb, linewidth = 0.4) +
    ggplot2::geom_segment(data = delay_ticks, seg_aes, colour = nb, linewidth = 0.3) +
    ggplot2::geom_text(data = delay_lab, ggplot2::aes(.data$x, .data$y, label = .data$lab), size = 2.3, angle = -30, hjust = 0, colour = nb) +
    # event / report tick labels
    ggplot2::geom_text(data = rep_lab, ggplot2::aes(.data$x, .data$y, label = .data$lab), size = 2.3, angle = 90, hjust = 0, colour = nb) +
    ggplot2::geom_text(data = ev_lab,  ggplot2::aes(.data$x, .data$y, label = .data$lab), size = 2.3, angle = 30, hjust = 1, nudge_x = -0.5, colour = nb) +
    # axis titles
    ggplot2::annotate("text", x = px(mean(c(rmin, rmax))), y = max(rep_lab$y) + 0.10 * y_span, label = "Report date", fontface = "bold", size = 3.6, colour = nb) +
    ggplot2::annotate("text", x = max(delay_lab$x) + 0.06 * x_span, y = mean(ax_y), label = "Delay", fontface = "bold", size = 3.6, hjust = 0, colour = nb) +
    ggplot2::annotate("text", x = min(polys$x) - 0.11 * x_span, y = py(mean(c(tmin, tmax)), 0), label = "Event date", angle = 30, fontface = "bold", size = 3.6, colour = nb) +
    ggplot2::coord_fixed(clip = "off") +
    ggplot2::labs(title = "Reporting hexamap") +
    .tbl_now_theme(palette) +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(), axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(), panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(margin = ggplot2::margin(b = 26)),
      legend.position = "bottom",
      legend.text = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5),
      plot.margin = ggplot2::margin(14, 64, 6, 60))
}
