# =============================================================================
# Plots removed from tbl.now (kept here for reference / experimentation).
#
# `plot_epidemic_process()`, `plot_transport_timeline()`,
# `plot_delay_band_ternary()` and `plot_reporting_v()` were dropped from the
# package. They still rely on package internals; to run them, load the package
# first and source this file:
#
#   devtools::load_all(".")
#   source("devel/removed_plots.R")
#   plot_delay_band_ternary(dn)
#
# Internals that remain in the package are referenced with `tbl.now:::`; the
# ternary helpers below are self-contained in this file.
# =============================================================================

# Height of a unit equilateral triangle (short=(0,0), medium=(1,0), long=(.5,H)).
.DIAG_TERN_H <- sqrt(3) / 2

# ---------------------------------------------------------------------------
# Epidemic process (companion plot_reporting_process() stayed in the package)
# ---------------------------------------------------------------------------

plot_epidemic_process <- function(x, palette = tbl.now:::.tbl_now_palette()) {
  tbl.now:::.diag_check(x)
  inc <- tbl.now:::.batch_report_increments(x)
  ctx <- tbl.now:::.diag_context(x, inc)
  tbl.now:::.diag_build_process(inc, ctx, palette, axis = "event")
}

# ---------------------------------------------------------------------------
# Transport discriminant over time
# ---------------------------------------------------------------------------

plot_transport_timeline <- function(x, ..., palette = tbl.now:::.tbl_now_palette()) {
  tbl.now:::.diag_check(x)
  td <- tbl.now::transport_discriminant(x, ...)
  .diag_build_transport_timeline(td, palette)
}

.diag_build_transport_timeline <- function(td, palette) {
  td$.stratum <- td$stratum
  z_star <- stats::qnorm(1 - attr(td, "alpha"))

  category <- dplyr::case_when(
    td$classification %in% c("batch", "batch_and_surge") ~ "batch",
    td$classification == "hold_or_deletion"               ~ "hold_or_deletion",
    TRUE                                                  ~ "none"
  )
  category[is.na(td$classification)] <- NA_character_
  td$category <- factor(category, levels = c("none", "hold_or_deletion", "batch"))
  colours <- c(none = "grey70", hold_or_deletion = "#C9A227", batch = palette[["accent_red"]])

  batches <- td[!is.na(td$category) & td$category == "batch", ]

  panel <- ggplot2::ggplot(td, ggplot2::aes(.data$report_date, .data$transport_z)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey85", linewidth = 0.3) +
    ggplot2::geom_hline(yintercept = z_star, linetype = "dashed", colour = "grey55", linewidth = 0.35) +
    ggplot2::geom_line(colour = "grey85", linewidth = 0.25, na.rm = TRUE) +
    ggplot2::geom_point(ggplot2::aes(colour = .data$category), size = 1.1, alpha = 0.8, na.rm = TRUE) +
    ggplot2::geom_text(data = batches, ggplot2::aes(label = format(.data$report_date)),
                       colour = colours[["batch"]], size = 2.2, vjust = -0.7, check_overlap = TRUE) +
    ggplot2::scale_colour_manual(values = colours, breaks = c("batch", "hold_or_deletion"),
                                 labels = c("batch", "hold / release pending"), name = NULL) +
    tbl.now:::.diag_comma_axis() +
    ggplot2::labs(
      x = "Report date", y = expression(paste("transport  ", italic(z), "   (", W, ": deficit before)")),
      title = "Transport discriminant over time",
      subtitle = "A batch is an isolated spike above the dashed threshold"
    ) +
    tbl.now:::.tbl_now_theme(palette)
  tbl.now:::.diag_facet(panel, "stratum" %in% names(td) && length(unique(td$.stratum)) > 1L)
}

# ---------------------------------------------------------------------------
# Delay-band ternary
# ---------------------------------------------------------------------------

plot_delay_band_ternary <- function(x, by = c("report", "event"), band_breaks = NULL,
                                    band_probs = c(1 / 3, 2 / 3), density = TRUE,
                                    label_top = 5L, palette = tbl.now:::.tbl_now_palette()) {
  by <- match.arg(by)
  tbl.now:::.diag_check(x)
  .diag_validate_bands(band_breaks, band_probs)
  inc <- tbl.now:::.batch_report_increments(x)
  ctx <- tbl.now:::.diag_context(x, inc)
  .diag_build_ternary(inc, ctx, by, band_breaks, band_probs, density, label_top, palette)
}

.diag_build_ternary <- function(increments, ctx, by, band_breaks, band_probs, density,
                                label_top, palette) {
  key    <- if (identical(by, "report")) ".report_date" else ".event_date"
  breaks <- .diag_band_breaks(increments, band_breaks, band_probs)
  source <- if (is.null(band_breaks)) "delay terciles" else "fixed delay cuts"
  unit   <- ctx$report_unit
  band_range <- function(lo, hi) {
    if (lo == hi) sprintf("%g %s", lo, unit) else sprintf("%g-%g %s", lo, hi, unit)
  }
  labels <- c(
    sprintf("short\n(%s)", band_range(0, breaks[1])),
    sprintf("medium\n(%s)", band_range(breaks[1] + 1, breaks[2])),
    sprintf("long\n(>%g %s)", breaks[2], unit)
  )

  shares <- increments |>
    dplyr::mutate(band = cut(.data$.delay, c(-Inf, breaks, Inf),
                             labels = c("short", "medium", "long"))) |>
    dplyr::group_by(.data$.stratum, .data[[key]], .data$band) |>
    dplyr::summarise(n = sum(pmax(.data$.count, 0)), .groups = "drop") |>
    tidyr::pivot_wider(names_from = "band", values_from = "n", values_fill = 0)
  for (band in c("short", "medium", "long")) {
    if (!band %in% names(shares)) shares[[band]] <- 0
  }
  shares <- dplyr::mutate(shares, reports = .data$short + .data$medium + .data$long)
  shares <- dplyr::filter(shares, .data$reports > 0)
  xy <- .diag_bary_to_xy(shares$short, shares$medium, shares$long)
  shares$x <- xy$x
  shares$y <- xy$y
  shares$.label_date <- shares[[key]]

  closest_to_top <- if (isTRUE(label_top > 0)) {
    shares |>
      dplyr::group_by(.data$.stratum) |>
      dplyr::slice_max(order_by = .data$long, n = label_top, with_ties = FALSE) |>
      dplyr::ungroup()
  } else {
    shares[0, , drop = FALSE]
  }

  border   <- data.frame(x = c(0, 1, 0.5, 0), y = c(0, 0, .DIAG_TERN_H, 0))
  gridline <- .diag_tern_gridlines()
  corners  <- data.frame(
    x = c(0, 1, 0.5), y = c(0, 0, .DIAG_TERN_H), label = labels,
    hjust = c(0.5, 0.5, 0.5), vjust = c(1.3, 1.3, -0.5)
  )
  grid <- if (isTRUE(density)) .diag_tern_density_all(shares, ctx$has_strata) else NULL

  if (ctx$has_strata) {
    strata_levels <- sort(unique(shares$.stratum))
    border   <- .diag_rep_strata(border, strata_levels)
    gridline <- .diag_rep_strata(gridline, strata_levels)
    corners  <- .diag_rep_strata(corners, strata_levels)
  }

  panel <- ggplot2::ggplot()
  if (!is.null(grid)) {
    panel <- panel +
      ggplot2::geom_raster(data = grid, ggplot2::aes(.data$x, .data$y, fill = .data$z),
                           na.rm = TRUE) +
      ggplot2::geom_contour(data = grid, ggplot2::aes(.data$x, .data$y, z = .data$z),
                            colour = palette[["dark_green"]], linewidth = 0.2, bins = 6,
                            na.rm = TRUE) +
      ggplot2::scale_fill_gradient(low = "grey97", high = palette[["light_green"]],
                                   na.value = NA, guide = "none")
  }
  panel <- panel +
    ggplot2::geom_segment(data = gridline,
                          ggplot2::aes(.data$x, .data$y, xend = .data$xend, yend = .data$yend),
                          colour = "grey85", linewidth = 0.2) +
    ggplot2::geom_path(data = border, ggplot2::aes(.data$x, .data$y),
                       colour = palette[["near_black"]], linewidth = 0.4) +
    ggplot2::geom_point(data = shares,
                        ggplot2::aes(.data$x, .data$y, colour = .data$reports),
                        size = 1.5, alpha = 0.7) +
    ggplot2::geom_text(data = closest_to_top,
                       ggplot2::aes(.data$x, .data$y, label = format(.data$.label_date)),
                       colour = palette[["near_black"]], size = 2.2, vjust = -0.7,
                       check_overlap = TRUE) +
    ggplot2::geom_text(data = corners,
                       ggplot2::aes(.data$x, .data$y, label = .data$label,
                                    hjust = .data$hjust, vjust = .data$vjust),
                       size = 2.7, colour = palette[["near_black"]], lineheight = 0.9) +
    tbl.now:::.diag_count_scale(palette, "colour") +
    ggplot2::coord_equal(clip = "off",
                         xlim = c(-0.05, 1.05), ylim = c(-0.16, .DIAG_TERN_H + 0.16)) +
    ggplot2::labs(
      title    = "Delay-band ternary",
      subtitle = sprintf("Each point = one %s date's delay-band shares (%s); a batch pulls toward LONG",
                         by, source)
    ) +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold", colour = palette[["near_black"]]),
      plot.subtitle = ggplot2::element_text(colour = palette[["muted_green"]], size = 9),
      legend.position = "right",
      panel.spacing  = ggplot2::unit(2, "lines"),
      plot.margin    = ggplot2::margin(6, 6, 12, 6)
    )

  if (ctx$has_strata) {
    panel <- panel + ggplot2::facet_wrap(".stratum")
  }
  panel
}

# ---- self-contained ternary helpers ----------------------------------------

.diag_validate_bands <- function(band_breaks, band_probs) {
  if (!is.null(band_breaks) && (length(band_breaks) != 2L || is.unsorted(band_breaks))) {
    cli::cli_abort("`band_breaks` must be two increasing delay cut points, or NULL.")
  }
  if (length(band_probs) != 2L || is.unsorted(band_probs) ||
      any(band_probs <= 0) || any(band_probs >= 1)) {
    cli::cli_abort("`band_probs` must be two increasing probabilities in (0, 1).")
  }
  invisible(NULL)
}

.diag_band_breaks <- function(increments, band_breaks, band_probs) {
  if (!is.null(band_breaks)) {
    return(as.numeric(band_breaks))
  }
  weights <- pmax(increments$.count, 0)
  q1 <- round(tbl.now:::.tbl_now_weighted_quantile(increments$.delay, weights, band_probs[1]))
  q2 <- round(tbl.now:::.tbl_now_weighted_quantile(increments$.delay, weights, band_probs[2]))
  if (q2 <= q1) q2 <- q1 + 1
  c(q1, q2)
}

.diag_bary_to_xy <- function(short, medium, long) {
  total <- short + medium + long
  b <- medium / total
  c <- long / total
  list(x = b + 0.5 * c, y = .DIAG_TERN_H * c)
}

.diag_tern_gridlines <- function(levels = c(0.25, 0.5, 0.75)) {
  seg <- function(a1, b1, c1, a2, b2, c2) {
    p1 <- .diag_bary_to_xy(a1, b1, c1)
    p2 <- .diag_bary_to_xy(a2, b2, c2)
    data.frame(x = p1$x, y = p1$y, xend = p2$x, yend = p2$y)
  }
  rows <- lapply(levels, function(l) {
    rbind(
      seg(l, 1 - l, 0, l, 0, 1 - l),
      seg(1 - l, l, 0, 0, l, 1 - l),
      seg(1 - l, 0, l, 0, 1 - l, l)
    )
  })
  do.call(rbind, rows)
}

.diag_rep_strata <- function(df, levels) {
  do.call(rbind, lapply(levels, function(level) {
    df$.stratum <- level
    df
  }))
}

.diag_tern_density_grid <- function(points, n = 90L) {
  if (nrow(points) < 8L) {
    return(NULL)
  }
  bandwidth <- function(v) {
    b <- stats::bw.nrd0(v)
    if (!is.finite(b) || b <= 0) 0.05 else b
  }
  gx <- seq(0, 1, length.out = n)
  gy <- seq(0, .DIAG_TERN_H, length.out = n)
  hx <- bandwidth(points$x)
  hy <- bandwidth(points$y)
  ax <- outer(gx, points$x, "-") / hx
  ay <- outer(gy, points$y, "-") / hy
  z  <- tcrossprod(matrix(stats::dnorm(ax), nrow = n), matrix(stats::dnorm(ay), nrow = n)) /
    (nrow(points) * hx * hy)

  grid <- expand.grid(x = gx, y = gy)
  grid$z <- as.vector(z)
  long   <- grid$y / .DIAG_TERN_H
  medium <- grid$x - 0.5 * long
  short  <- 1 - medium - long
  grid$z[short < -1e-9 | medium < -1e-9 | long < -1e-9] <- NA
  grid
}

.diag_tern_density_all <- function(points, has_strata, n = 90L) {
  groups <- if (has_strata) split(points, points$.stratum) else list(points)
  grids  <- lapply(names(groups) %||% seq_along(groups), function(nm) {
    grp  <- groups[[nm]]
    grid <- .diag_tern_density_grid(grp, n)
    if (is.null(grid)) return(NULL)
    if (has_strata) grid$.stratum <- grp$.stratum[1]
    grid
  })
  grids <- grids[!vapply(grids, is.null, logical(1))]
  if (length(grids) == 0L) NULL else do.call(rbind, grids)
}

# ---------------------------------------------------------------------------
# The reporting "V": the reporting triangle rotated 45 degrees. Superseded by
# plot_reporting_hexamap() in the batch-reporting article.
# ---------------------------------------------------------------------------

plot_reporting_v <- function(x, max_delay = NULL, point_size = NULL,
                             plotly = FALSE, palette = tbl.now:::.tbl_now_palette()) {
  tbl.now:::.diag_check(x)
  inc        <- tbl.now:::.batch_report_increments(x)
  unit       <- tbl.now:::.tbl_now_units_to_days(tbl.now::get_report_units(x) %||% "days")
  event_unit <- tbl.now::get_event_units(x) %||% "days"
  now    <- as.numeric(tbl.now::get_now(x) %||% max(inc$.report_date))
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
    ggplot2::geom_point(data = zeros, colour = tbl.now:::.DIAG_ZERO_COLOUR, shape = 12, size = psize) +
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
    tbl.now:::.tbl_now_theme(palette) +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_text(face = "bold"),
                   panel.grid = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(hjust = 0.5, vjust = 1, angle = 90),
                   legend.box.margin = ggplot2::margin(0, 0, 0, 20),
                   legend.position = "top",
                   plot.margin = ggplot2::margin(6, 75, 52, 75))
  tbl.now:::.as_plotly(p, plotly)
}
