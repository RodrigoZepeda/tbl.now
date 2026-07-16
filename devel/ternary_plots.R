# =============================================================================
# EXPERIMENT (devel only -- NOT part of the package).
#
# Three barycentric / ternary explorations of the reporting triangle. ggtern is
# not used (it does not track ggplot2 4.x); the triangle is hand-drawn from the
# barycentric->cartesian map, so it composes with plain ggplot2.
#
#   plot_ternary_reporting(x)   ternary of (event number, delay, report date),
#                               each scaled to its own max then renormalised to
#                               barycentric weights.
#   plot_ternary_transport(x)   ternary of (event number, delay, transport),
#                               transport = transport_discriminant()'s transport_z
#                               (clamped at 0 for the weight), batches in red.
#
# (The third-axis reporting triangle graduated into the package itself as
#  plot_reporting_triangle(report_ticks = ).)
#
# Run with: devtools::load_all("."); source("devel/ternary_plots.R")
#           plot_ternary_reporting(tn); plot_ternary_transport(tn)
# =============================================================================

`%||%` <- function(a, b) if (is.null(a)) b else a

# --- barycentric helpers -----------------------------------------------------

# Corners: A = (0,0) bottom-left, B = (1,0) bottom-right, C = (0.5, h) top.
.TERN_H <- sqrt(3) / 2

# (a, b, c) weights (vectors) -> cartesian (x, y). Renormalises to sum 1.
.bary2xy <- function(a, b, c) {
  s <- a + b + c
  s[s == 0] <- 1
  a <- a / s; b <- b / s; c <- c / s
  data.frame(x = b + c * 0.5, y = c * .TERN_H)
}

# Static triangle frame + corner titles, as a list of ggplot layers.
.ternary_frame <- function(corner_a, corner_b, corner_c, palette) {
  border <- data.frame(x = c(0, 1, 0.5, 0), y = c(0, 0, .TERN_H, 0))
  grid_layers <- list()
  for (f in c(0.25, 0.5, 0.75)) {
    # lines of constant a / b / c, to read proportions off the triangle
    grid_layers <- c(grid_layers, list(
      ggplot2::annotate("segment",
        x = .bary2xy(f, 1 - f, 0)$x,       y = .bary2xy(f, 1 - f, 0)$y,
        xend = .bary2xy(f, 0, 1 - f)$x,    yend = .bary2xy(f, 0, 1 - f)$y,
        colour = "grey88", linewidth = 0.25),
      ggplot2::annotate("segment",
        x = .bary2xy(1 - f, f, 0)$x,       y = .bary2xy(1 - f, f, 0)$y,
        xend = .bary2xy(0, f, 1 - f)$x,    yend = .bary2xy(0, f, 1 - f)$y,
        colour = "grey88", linewidth = 0.25),
      ggplot2::annotate("segment",
        x = .bary2xy(1 - f, 0, f)$x,       y = .bary2xy(1 - f, 0, f)$y,
        xend = .bary2xy(0, 1 - f, f)$x,    yend = .bary2xy(0, 1 - f, f)$y,
        colour = "grey88", linewidth = 0.25)
    ))
  }
  c(
    grid_layers,
    list(
      ggplot2::geom_path(data = border, ggplot2::aes(.data$x, .data$y),
                         inherit.aes = FALSE, colour = "grey50", linewidth = 0.4),
      ggplot2::annotate("text", x = 0,   y = -0.04, label = corner_a, hjust = 0.9, vjust = 1, size = 3.2, fontface = "bold"),
      ggplot2::annotate("text", x = 1,   y = -0.04, label = corner_b, hjust = 0.1, vjust = 1, size = 3.2, fontface = "bold"),
      ggplot2::annotate("text", x = 0.5, y = .TERN_H + 0.04, label = corner_c, vjust = 0, size = 3.2, fontface = "bold"),
      ggplot2::coord_equal(clip = "off"),
      ggplot2::theme_void(base_size = 11)
    )
  )
}

.tern_increments <- function(x) {
  suppressWarnings(inc <- tbl.now:::.batch_report_increments(x))
  origin <- min(inc$.event_date, na.rm = TRUE)
  inc$ev <- as.numeric(inc$.event_date  - origin)   # event number (days from start)
  inc$rp <- as.numeric(inc$.report_date - origin)   # report number (days from start)
  inc$dl <- as.numeric(inc$.delay)
  inc$n  <- pmax(inc$.count, 0)
  attr(inc, "origin") <- origin
  inc[inc$n > 0, , drop = FALSE]
}

.tern_subsample <- function(d, max_points) {
  if (nrow(d) <= max_points) return(d)
  d[sample.int(nrow(d), max_points, prob = d$n), , drop = FALSE]
}

# --- 1. (event, delay, report date) ------------------------------------------

plot_ternary_reporting <- function(x, max_points = 20000L,
                                   palette = tbl.now:::.tbl_now_palette()) {
  d <- .tern_increments(x)
  a <- d$ev / max(d$ev, 1); b <- d$dl / max(d$dl, 1); c <- d$rp / max(d$rp, 1)
  d$a <- a; d$b <- b; d$c <- c
  d <- .tern_subsample(d, max_points)
  xy <- .bary2xy(d$a, d$b, d$c)
  xy$report <- as.numeric(d$rp)

  ggplot2::ggplot(xy, ggplot2::aes(.data$x, .data$y)) +
    ggplot2::geom_point(ggplot2::aes(colour = .data$report), size = 0.5, alpha = 0.35) +
    ggplot2::scale_colour_gradient(name = "report\n(rel. day)",
                                   low = palette[["medium_green"]], high = palette[["accent_red"]]) +
    .ternary_frame("event #", "delay", "report date", palette) +
    ggplot2::labs(
      title = "Ternary: event / delay / report date",
      subtitle = "Each cell as barycentric weights of (event #, delay, report date), each scaled to its max",
      caption = paste(
        "Corner = that quantity dominates. Because report = event + delay the cloud lies on a",
        "\nconstraint surface, not the whole triangle; colour = report date (time).")
    ) +
    ggplot2::theme(plot.caption = ggplot2::element_text(colour = palette[["muted_green"]], hjust = 0, size = 8),
                   plot.title = ggplot2::element_text(face = "bold"),
                   plot.subtitle = ggplot2::element_text(colour = palette[["muted_green"]]))
}

# --- 2. (event, delay, transport) --------------------------------------------

plot_ternary_transport <- function(x, max_points = 20000L, period = NULL,
                                   palette = tbl.now:::.tbl_now_palette()) {
  d <- .tern_increments(x)
  td <- suppressWarnings(tbl.now::transport_discriminant(x, period = period))
  # Attach each cell's report-date transport score and batch flag.
  d <- dplyr::left_join(
    d, td[, c("report_date", "transport_z", "batch")],
    by = c(".report_date" = "report_date")
  )
  d <- d[is.finite(d$transport_z), , drop = FALSE]
  d$tr <- pmax(d$transport_z, 0)                       # weight must be >= 0

  a <- d$ev / max(d$ev, 1); b <- d$dl / max(d$dl, 1); c <- d$tr / max(d$tr, 1)
  d$a <- a; d$b <- b; d$c <- c
  d <- .tern_subsample(d, max_points)
  xy <- .bary2xy(d$a, d$b, d$c)
  xy$batch <- ifelse(d$batch %in% TRUE, "batch", "other")

  ggplot2::ggplot(xy, ggplot2::aes(.data$x, .data$y)) +
    ggplot2::geom_point(data = subset(xy, batch == "other"),
                        colour = "grey75", size = 0.5, alpha = 0.3) +
    ggplot2::geom_point(data = subset(xy, batch == "batch"),
                        colour = palette[["accent_red"]], size = 0.9, alpha = 0.7) +
    .ternary_frame("event #", "delay", "transport", palette) +
    ggplot2::labs(
      title = "Ternary: event / delay / transport",
      subtitle = "Third corner is the transport (deficit) score of the cell's report date; batches in red",
      caption = paste(
        "transport_z clamped at 0 for the weight. If batches separate toward the transport corner,",
        "\nthe deficit signal is visible in this projection.")
    ) +
    ggplot2::theme(plot.caption = ggplot2::element_text(colour = palette[["muted_green"]], hjust = 0, size = 8),
                   plot.title = ggplot2::element_text(face = "bold"),
                   plot.subtitle = ggplot2::element_text(colour = palette[["muted_green"]]))
}

