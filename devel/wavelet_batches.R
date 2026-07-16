# =============================================================================
# EXPLORATION (devel): a wavelet look at the paired reporting / epidemic series.
#
# Two daily series on a common calendar:
#   * report series  R[t] = how many reports ARRIVED on day t (reporting process)
#   * event  series  E[t] = how many cases OCCURRED on day t (epidemic curve)
# A batch is a spike in R that has no counterpart in E (no new cases happened,
# reports just landed). A spike is localised in time and broadband in frequency,
# so in a wavelet scalogram a batch is a bright vertical ridge -- present in R,
# absent in E. Plotting the two scalograms together should make batches pop.
#
# Self-contained Morlet CWT (Torrence & Compo 1998), no extra packages.
# Run: devtools::load_all("."); source("devel/wavelet_batches.R")
#            scalogram_plot(tn)
#
# FINDING (2026-07-10):
#   * MADE-UP data (clean epidemic + 2 planted batches): the batches are TEXTBOOK
#     -- two bright short-period (~4-16 day) ridges in the report scalogram at the
#     exact release dates, and NOTHING at those times/scales in the event
#     scalogram. The wavelet contrast is a clean batch detector here.
#   * COVID_US: suggestive but noisier. Real reporting is spiky everywhere (weekly
#     cadence + daily noise) so the report scalogram has short-period ridges
#     throughout; the batches are the STRONGEST of them, a dark short-period band
#     clustered around late-2021/early-2022 (the 1.8M and ~0.9M dumps) that the
#     event scalogram lacks. So batches = the report series' short-period energy
#     that the event series does not share -- visible, but you must read amplitude,
#     not mere presence. Power is normalised per period so localised bursts show
#     over the slow epidemic trend.
#
# WAVELET COHERENCE (WaveletComp::analyze.coherency, loess-detrended):
#   * MADE-UP: the two series are near-identical except at the batches, so
#     coherence is ~1 almost everywhere, with LOCALISED low-coherence /
#     disordered-phase patches exactly at the two release dates (short periods).
#     Real but subtler than the scalogram.
#   * COVID_US (Jun-2021..Jun-2022): the story is dramatic. While epidemic and
#     reporting run together (2021) coherence is high with a steady phase. Once
#     reporting DECOUPLES from events (Jan-2022 on -- pure backlog dumping, and
#     covid_us only carries 2020-2021 event dates so the event series falls to ~0)
#     coherence COLLAPSES to a big blue (~0) block at periods 2-32 days. The
#     batch-dominated era is a coherence hole: reports doing things no recent event
#     explains. (Note the event->0 is partly a dataset artefact, but it faithfully
#     says late reports are pure backlog.)
#   * biwavelet::wtc was tried first but its AR(1) surrogate fit fails on the
#     trended epidemic ("non-stationary AR part"); WaveletComp's loess detrend +
#     white-noise surrogates avoid that.
# =============================================================================

suppressPackageStartupMessages({library(ggplot2); library(dplyr); library(patchwork)})

# --- Morlet continuous wavelet transform, FFT-based --------------------------
morlet_cwt <- function(x, dt = 1, dj = 0.125, s0 = 2 * dt, w0 = 6) {
  n <- length(x)
  x <- x - mean(x)
  xhat <- stats::fft(x)

  # FFT angular frequencies (standard ordering)
  freq <- (0:(n - 1))
  freq[freq > n / 2] <- freq[freq > n / 2] - n
  wk <- 2 * pi * freq / (n * dt)

  J      <- floor(log2(n * dt / s0) / dj)
  scales <- s0 * 2^((0:J) * dj)

  power <- matrix(0, nrow = length(scales), ncol = n)
  for (i in seq_along(scales)) {
    s        <- scales[i]
    norm     <- sqrt(2 * pi * s / dt) * pi^(-0.25)
    daughter <- norm * exp(-(s * wk - w0)^2 / 2) * (wk > 0)
    W        <- stats::fft(xhat * daughter, inverse = TRUE) / n
    power[i, ] <- Mod(W)^2
  }
  fourier_factor <- 4 * pi / (w0 + sqrt(2 + w0^2))
  period <- fourier_factor * scales

  # cone of influence (e-folding time = sqrt(2) s -> period)
  coi_t <- fourier_factor * sqrt(2) *
    dt * c(1e-5, 1:((n + 1) %/% 2 - 1), rev(1:(n %/% 2)), 1e-5)[seq_len(n)]

  list(power = power, period = period, coi = coi_t, n = n)
}

# --- build the two daily series from a tbl_now -------------------------------
paired_series <- function(x) {
  inc <- suppressWarnings(tbl.now:::.batch_report_increments(x))
  lo  <- min(c(inc$.event_date, inc$.report_date))
  hi  <- max(c(inc$.event_date, inc$.report_date))
  grid <- seq(lo, hi, by = "day")
  rep_n <- tapply(pmax(inc$.count, 0), inc$.report_date, sum)
  ev_n  <- tapply(pmax(inc$.count, 0), inc$.event_date,  sum)
  R <- as.numeric(rep_n[as.character(grid)]); R[is.na(R)] <- 0
  E <- as.numeric(ev_n[as.character(grid)]);  E[is.na(E)] <- 0
  data.frame(date = grid, report = R, event = E)
}

# --- scalogram as a ggplot ---------------------------------------------------
# Power is NORMALISED per period (divided by that period's median over time), so a
# localised burst at any period stands out even though the slow epidemic trend
# carries far more raw power at long periods. A batch is then a bright vertical
# ridge at short periods.
.scalo_df <- function(cwt, dates) {
  P   <- cwt$power
  med <- apply(P, 1, function(r) stats::median(r[r > 0]))
  Pn  <- P / pmax(med, .Machine$double.eps)
  coi <- pmax(cwt$coi, min(cwt$period))            # clamp COI into the period range
  expand.grid(ti = seq_along(dates), pj = seq_along(cwt$period)) |>
    dplyr::mutate(date = dates[.data$ti], period = cwt$period[.data$pj],
                  power = log1p(Pn[cbind(.data$pj, .data$ti)]),
                  coi   = coi[.data$ti])
}
.scalo_plot <- function(cwt, dates, title, fillcol) {
  df     <- .scalo_df(cwt, dates)
  coi_df <- data.frame(date = dates, coi = pmax(cwt$coi, min(cwt$period)))
  brks   <- 2^(1:7)
  brks   <- brks[brks >= min(cwt$period) & brks <= max(cwt$period)]
  maxp   <- max(cwt$period)
  ggplot2::ggplot(df, ggplot2::aes(.data$date, .data$period)) +
    ggplot2::geom_raster(ggplot2::aes(fill = .data$power)) +
    # grey out the region OUTSIDE the cone of influence (edge effects: unreliable)
    ggplot2::geom_ribbon(data = coi_df,
                         ggplot2::aes(x = .data$date, ymin = .data$coi, ymax = maxp),
                         inherit.aes = FALSE, fill = "grey55", alpha = 0.5) +
    # the cone-of-influence boundary: dotted grey, bounding the area to analyse
    ggplot2::geom_line(data = coi_df, ggplot2::aes(x = .data$date, y = .data$coi),
                       inherit.aes = FALSE, colour = "grey35", linewidth = 0.4,
                       linetype = "dotted") +
    ggplot2::scale_y_continuous(trans = "log2", expand = c(0, 0), breaks = brks,
                                limits = range(cwt$period)) +
    ggplot2::scale_fill_gradient(low = "grey96", high = fillcol, name = "rel. power") +
    ggplot2::labs(x = NULL, y = "period (days)", title = title) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "none")
}

# --- wavelet COHERENCE between the two series (WaveletComp) -------------------
# Coherence (0..1) is the local correlation of the two series in time-frequency.
# Where report and event march together (the shared epidemic) coherence is high
# and the phase arrows point one consistent way (event leads report by the
# reporting delay). A BATCH is report-only short-period energy the event series
# has no match for, so at the batch time and short periods coherence collapses
# and the phase arrows lose their order -- a local rip in the phase field.
#
# WaveletComp is used (not biwavelet) because it detrends with loess first, so the
# strongly-trended epidemic curve does not break the AR surrogate fit.
wavelet_coherence_plot <- function(x, file = NULL, label = "", n_sim = 20L,
                                   loess_span = 0.2, max_period = 64L, date_range = NULL) {
  if (!requireNamespace("WaveletComp", quietly = TRUE)) {
    stop("install.packages('WaveletComp')")
  }
  s <- paired_series(x)
  if (!is.null(date_range)) {
    s <- s[s$date >= date_range[1] & s$date <= date_range[2], , drop = FALSE]
  }
  df <- data.frame(R = sqrt(s$report), E = sqrt(s$event))

  # Cap the longest period: the long-period band is uniformly high-coherence (both
  # series ride the same epidemic) and swamps the short-period band where batches
  # live, so we focus on sub-`max_period`-day structure.
  wc <- WaveletComp::analyze.coherency(
    df, my.pair = c("R", "E"), dt = 1, dj = 1 / 20,
    lowerPeriod = 2, upperPeriod = min(max_period, floor(nrow(df) / 3)),
    loess.span = loess_span, make.pval = TRUE, n.sim = n_sim, verbose = FALSE)

  draw <- function() {
    WaveletComp::wc.image(
      wc, which.image = "wc", color.key = "interval", n.levels = 100,
      siglvl.contour = 0.1, siglvl.arrow = 0.1, plot.arrow = TRUE,
      periodlab = "period (days)", timelab = "day index",
      legend.params = list(lab = "coherence"),
      main = paste0(label, "Wavelet coherence: report vs event",
                    " (arrows = phase; contour = signif)"))
  }
  if (is.null(file)) {
    draw()                                   # to the current device (e.g. knitr)
  } else {
    grDevices::png(file, width = 1150, height = 800, res = 110)
    on.exit(grDevices::dev.off(), add = TRUE)
    draw()
  }
  invisible(wc)
}

# A clearer, practitioner-friendly coherence view: a ggplot heatmap where LOW
# coherence POPS (batches) and high coherence fades to pale, with the confirmed
# batch dates drawn as dashed verticals so the reader can see the coherence break
# line up with them.
wavelet_coherence_gg <- function(x, mark = NULL, label = "", n_sim = 20L,
                                 loess_span = 0.2, max_period = 64L, date_range = NULL,
                                 palette = tbl.now:::.tbl_now_palette()) {
  if (!requireNamespace("WaveletComp", quietly = TRUE)) stop("install WaveletComp")
  s <- paired_series(x)
  if (!is.null(date_range)) s <- s[s$date >= date_range[1] & s$date <= date_range[2], , drop = FALSE]
  df <- data.frame(R = sqrt(s$report), E = sqrt(s$event))
  wc <- WaveletComp::analyze.coherency(
    df, my.pair = c("R", "E"), dt = 1, dj = 1 / 20,
    lowerPeriod = 2, upperPeriod = min(max_period, floor(nrow(df) / 3)),
    loess.span = loess_span, make.pval = TRUE, n.sim = n_sim, verbose = FALSE)

  g <- expand.grid(ti = seq_along(wc$axis.1), pj = seq_along(wc$Period))
  g$coherence <- wc$Coherence[cbind(g$pj, g$ti)]
  g$date   <- s$date[g$ti]
  g$period <- wc$Period[g$pj]

  p <- ggplot2::ggplot(g, ggplot2::aes(.data$date, .data$period, fill = .data$coherence)) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_gradientn(
      colours = c(palette[["accent_red"]], "#E8A33D", "grey92", "grey92"),
      values  = scales::rescale(c(0, 0.5, 0.8, 1)), limits = c(0, 1),
      name = "move\ntogether?") +
    ggplot2::scale_y_continuous(trans = "log2", expand = c(0, 0)) +
    ggplot2::labs(
      x = NULL, y = "period (days)",
      title = paste0(label, "Do reports and cases move together?"),
      subtitle = "Pale = yes, in step. A coloured blotch = no: reporting broke away from cases (a batch)") +
    ggplot2::theme_minimal(base_size = 11)
  if (!is.null(mark) && length(mark) > 0) {
    p <- p + ggplot2::geom_vline(xintercept = as.Date(mark),
                                 colour = "grey15", linetype = "dashed", linewidth = 0.4)
  }
  p
}

scalogram_plot <- function(x, label = "") {
  inc <- tbl.now:::.batch_report_increments(x)
  ctx <- tbl.now:::.diag_context(x, inc)
  pal <- tbl.now:::.tbl_now_palette()
  s   <- paired_series(x)
  cwR <- morlet_cwt(sqrt(s$report))
  cwE <- morlet_cwt(sqrt(s$event))

  # Top row: the SAME count plots as diagnostic_plot() (reuse its builders, so the
  # "reporting process" and "epidemic process" here match exactly). Bottom row:
  # each series' scalogram, aligned underneath.
  p_rep_cases <- tbl.now:::.diag_build_process(inc, ctx, pal, "report") +
    ggplot2::labs(caption = NULL)
  p_evt_cases <- tbl.now:::.diag_build_process(inc, ctx, pal, "event") +
    ggplot2::labs(caption = NULL)
  # match each column to its scalogram colour: reporting process in the scalogram
  # red, epidemic process in the scalogram green.
  p_rep_cases$layers[[1]]$aes_params$fill <- "#C0392B"
  p_evt_cases$layers[[1]]$aes_params$fill <- "#4C9A6A"
  p_R <- .scalo_plot(cwR, s$date, "Report scalogram (a batch = bright short-period ridge)", "#C0392B")
  p_E <- .scalo_plot(cwE, s$date, "Event scalogram (smooth epidemic, no such ridge)", "#4C9A6A")

  # 2 x 2: reports column (left), events-by-event-date column (right).
  patchwork::wrap_plots(list(p_rep_cases, p_evt_cases, p_R, p_E),
                        ncol = 2, byrow = TRUE, heights = c(0.9, 1)) +
    patchwork::plot_annotation(
      title = paste0(label, "Reports (left) vs cases by event date (right), and their scalograms"))
}

# quick self-test: a lone spike should localise as a vertical ridge
if (identical(environment(), globalenv()) && !exists(".wavelet_selftest_done")) {
  set.seed(1); z <- rnorm(256, 0, 0.2); z[128] <- 8
  ct <- morlet_cwt(z)
  cat("self-test: peak-power time index =",
      which.max(colSums(ct$power)), "(planted spike at 128)\n")
  .wavelet_selftest_done <- TRUE
}
