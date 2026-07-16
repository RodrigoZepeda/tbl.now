# =============================================================================
# EXPLORATION (devel): the WINDOWED SCALOGRAM DIFFERENCE between the reporting
# series (reports by report date) and the epidemic series (cases by event date),
# rendered in the same style as the package's plot_scalogram(): window-inner,
# PAUL wavelet, integer-index x relabelled with dates, gray10 outside the cone.
#
# Each series gets its own window-inner scalogram (per-period normalised, as in
# plot_scalogram), and we plot log2(reporting) - log2(epidemic). A batch is
# report-only structure -- energy in the reporting series the epidemic lacks --
# so it lights up RED; epidemic-only structure is GREEN.
#
# Run:
#   devtools::load_all(".")
#   source("devel/wavscalogram_explore.R")
#   plot_scalogram_difference(ideal)                 # a ggplot
#   ggplot2::ggsave("wsd.png", plot_scalogram_difference(tn), width = 9, height = 4.5)
# =============================================================================

`%||%` <- function(a, b) if (is.null(a)) b else a

# One window-inner, per-period-normalised scalogram (matches plot_scalogram()).
.wsd_one <- function(n, wname, windowrad) {
  ws  <- wavScalogram::windowed_scalogram(
    sqrt(n), dt = 1, wname = wname, windowrad = windowrad, border_effects = "INNER",
    energy_density = TRUE, makefigure = FALSE, figureperiod = TRUE)
  P   <- ws$wsc
  med <- apply(P, 2L, function(col) stats::median(col[col > 0], na.rm = TRUE))
  list(Pn = sweep(P, 2L, pmax(med, .Machine$double.eps), "/"),
       periods = ws$scales * ws$fourierfactor, tcentral = ws$tcentral)
}

# Windowed scalogram difference: reporting minus epidemic, as a ggplot heat map.
# Positive (red) = the reporting series carries MORE relative energy there than
# the epidemic series (a reporting artefact such as a batch); negative (green) =
# the epidemic series carries more.
plot_scalogram_difference <- function(x, wname = "PAUL", windowrad = 1,
                                      format = "%d/%b/%y",
                                      palette = tbl.now:::.tbl_now_palette()) {
  stopifnot(requireNamespace("wavScalogram", quietly = TRUE))
  rep_s <- tbl.now:::.scalo_series(x, "reporting")
  epi_s <- tbl.now:::.scalo_series(x, "epidemic")         # same common grid
  report_unit <- get_report_units(x) %||% "days"

  r <- .wsd_one(rep_s$n, wname, windowrad)
  e <- .wsd_one(epi_s$n, wname, windowrad)
  val     <- pmax(log2(r$Pn) - log2(e$Pn), 0)                      # reporting minus epidemic
  periods <- r$periods
  dates   <- rep_s$date[round(r$tcentral)]

  df <- expand.grid(ti = seq_along(dates), pj = seq_along(periods))
  df$xi    <- df$ti
  df$logp  <- log2(periods[df$pj])
  df$value <- val[cbind(df$ti, df$pj)]
  df <- df[is.finite(df$value), , drop = FALSE]

  hstep <- stats::median(diff(sort(unique(log2(periods)))))
  brks  <- 2^(0:12); brks <- brks[brks >= min(periods) & brks <= max(periods)]
  lim   <- stats::quantile(abs(df$value), 0.99, names = FALSE, na.rm = TRUE)
  date_breaks <- pretty(dates, n = 6)
  date_breaks <- date_breaks[as.numeric(date_breaks) >= min(as.numeric(dates)) &
                             as.numeric(date_breaks) <= max(as.numeric(dates))]
  xpos <- stats::approx(as.numeric(dates), seq_along(dates),
                        xout = as.numeric(date_breaks), rule = 2)$y

  ggplot2::ggplot(df, ggplot2::aes(.data$xi, .data$logp, fill = .data$value)) +
    ggplot2::geom_tile(width = 1, height = hstep) +
    ggplot2::scale_x_continuous(breaks = xpos, labels = base::format(date_breaks, format),
                                expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0), breaks = log2(brks), labels = round(brks)) +
    ggplot2::scale_fill_gradient2(
      low = palette[["dark_green"]], mid = "grey96", high = palette[["accent_red"]],
      midpoint = 0, limits = c(0, lim), oob = scales::squish,
      name = "reporting -\nepidemic") +
    ggplot2::labs(x = "Date", y = sprintf("Period (%s)", report_unit),
                  title = "Windowed scalogram difference (reporting minus epidemic)") +
    tbl.now:::.tbl_now_theme(palette) +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "gray10", colour = NA),
                   panel.grid = ggplot2::element_blank())
}
