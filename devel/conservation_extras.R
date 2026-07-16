# =============================================================================
# Conservation monitors moved OUT of the package (kept for reference / reuse).
# The shared helpers (.conservation_prep / .conservation_score_line / etc.) stay
# in the package (R/conservation_plots.R); these heavier monitors did not earn
# their place in the default gallery.
#
#   plot_creation_transport(x)      the two window scores as two stacked panels;
#                                   transport spikes at a batch, creation at a surge.
#   plot_cumulative_backlog(x)      detrended cumulative residual (CUSUM); a batch
#                                   is a V, a run of batches a staircase.
#   plot_reporting_lag(x)           mean reporting delay vs a local null band.
#   plot_conservation_dashboard(x)  creation-vs-transport overlay + the batch
#                                   score (transport - creation).
#
# Run with: devtools::load_all("."); source("devel/conservation_extras.R")
# =============================================================================

# --- creation vs transport score (two stacked panels) ------------------------

plot_creation_transport <- function(x,
                                    lookback        = 7L,
                                    baseline_window = NULL,
                                    period          = NULL,
                                    alpha           = 0.05,
                                    plotly          = FALSE,
                                    palette         = tbl.now:::.tbl_now_palette()) {
  tbl.now:::.batch_experimental_warning("plot_creation_transport")
  tbl.now:::.batch_check_tbl_now(x)
  if (!isTRUE(plotly) && !requireNamespace("patchwork", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg patchwork} is required for {.fn plot_creation_transport}.")
  }
  if (alpha <= 0 || alpha >= 1) {
    cli::cli_abort("`alpha` must lie strictly between 0 and 1. Got {alpha}.")
  }
  prep <- tbl.now:::.conservation_prep(x, as.integer(lookback), baseline_window, period, alpha)

  p_transport <- tbl.now:::.conservation_score_line(
    prep, "transport_z", "Transport score (a batch)",
    "How many reports the days just before were missing; breaks out at a batch",
    palette[["accent_red"]], palette, xlab = NULL)
  p_creation <- tbl.now:::.conservation_score_line(
    prep, "creation_z", "Creation score (a surge)",
    "How much the window gained cases overall; breaks out at a real surge",
    palette[["medium_green"]], palette, xlab = "Report date",
    caption = paste(
      "A batch: the transport panel spikes above the dashed band while creation stays flat.",
      "\nA surge: the other way round. Red points = batch_test()-confirmed batches. The axis",
      "\nis signed-log because a big batch sits hundreds of standard deviations out."))

  tbl.now:::.combine_panels(list(p_transport, p_creation), plotly = plotly, ncol = 1,
                            title = "Creation vs transport score", palette = palette)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# --- the transport-minus-creation "batch score" panel ------------------------

.conservation_score_panel <- function(prep, palette, xlab = "Report date") {
  reg <- prep$reg
  reg$batch_score <- reg$transport_z - reg$creation_z
  flagged <- prep$flagged
  flagged$batch_score <- flagged$transport_z - flagged$creation_z

  p <- ggplot2::ggplot(reg, ggplot2::aes(.data$report_date, .data$batch_score)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey80", linewidth = 0.3) +
    ggplot2::geom_line(colour = palette[["dark_green"]], linewidth = 0.35, na.rm = TRUE) +
    ggplot2::geom_point(data = flagged, colour = palette[["accent_red"]],
                        size = 1.8, na.rm = TRUE) +
    tbl.now:::.conservation_y_scale() +
    ggplot2::labs(
      x = xlab, y = "transport - creation (SDs)", title = "Batch score",
      subtitle = "transport minus creation: a clean batch is a positive spike",
      caption = paste(
        "The single number from subtracting the two scores. It works for a clean batch",
        "\n(transport up, creation flat -> positive) but NOT in general: a HOLD (creation dips)",
        "\nalso goes positive, and a BIG dump inflates creation too, flipping it negative."
      )
    ) +
    tbl.now:::.tbl_now_theme(palette)
  tbl.now:::.diag_facet(p, prep$has_strata)
}

# --- 1. cumulative reporting backlog -----------------------------------------

plot_cumulative_backlog <- function(x,
                                    lookback        = 3L,
                                    baseline_window = NULL,
                                    period          = NULL,
                                    alpha           = 0.05,
                                    palette         = tbl.now:::.tbl_now_palette()) {
  tbl.now:::.batch_experimental_warning("plot_cumulative_backlog")
  tbl.now:::.batch_check_tbl_now(x)

  reg <- tbl.now:::.batch_detrended(x, as.integer(lookback), baseline_window, period)
  reg <- reg |>
    dplyr::filter(is.finite(.data$z)) |>
    dplyr::group_by(.data$.stratum) |>
    dplyr::arrange(.data$report_date, .by_group = TRUE) |>
    dplyr::mutate(backlog = cumsum(.data$z - stats::median(.data$z, na.rm = TRUE))) |>
    dplyr::ungroup()

  confirmed <- tbl.now:::.batch_confirmed(x, as.integer(lookback), baseline_window, period, alpha)
  flagged   <- dplyr::semi_join(reg, confirmed, by = c("report_date", ".stratum"))

  panel <- ggplot2::ggplot(reg, ggplot2::aes(.data$report_date, .data$backlog)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
    ggplot2::geom_line(colour = palette[["dark_green"]], linewidth = 0.6) +
    ggplot2::geom_point(data = flagged, colour = palette[["accent_red"]], size = 1.8) +
    ggplot2::labs(
      x = "Report date", y = "Cumulative standardised residual",
      title = "Cumulative reporting backlog",
      subtitle = "Reports that are running ahead of, or behind, schedule"
    ) +
    tbl.now:::.tbl_now_theme(palette)
  tbl.now:::.diag_facet(panel, tbl.now:::.batch_has_strata(x))
}

# --- 2. the conservation dashboard -------------------------------------------

plot_conservation_dashboard <- function(x,
                                        lookback        = 3L,
                                        baseline_window = NULL,
                                        period          = NULL,
                                        alpha           = 0.05,
                                        palette         = tbl.now:::.tbl_now_palette()) {
  tbl.now:::.batch_experimental_warning("plot_conservation_dashboard")
  tbl.now:::.batch_check_tbl_now(x)
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg patchwork} is required for {.fn plot_conservation_dashboard}.")
  }

  prep <- tbl.now:::.conservation_prep(x, as.integer(lookback), baseline_window, period, alpha)
  p_ct    <- tbl.now:::.conservation_ct_panel(prep, palette, xlab = NULL)
  p_score <- .conservation_score_panel(prep, palette, xlab = "Report date")

  patchwork::wrap_plots(list(p_ct, p_score), ncol = 1) +
    patchwork::plot_annotation(
      title = "Conservation dashboard",
      subtitle = "The two window scores, then their difference; red = batch_test()-confirmed batches",
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", colour = palette[["near_black"]])
      )
    )
}

# --- 3. reporting lag --------------------------------------------------------

plot_reporting_lag <- function(x,
                               max_delay = NULL,
                               window    = 15L,
                               period    = NULL,
                               lookback  = 3L,
                               alpha     = 0.05,
                               palette   = tbl.now:::.tbl_now_palette()) {
  tbl.now:::.batch_experimental_warning("plot_reporting_lag")
  tbl.now:::.batch_check_tbl_now(x)

  increments <- tbl.now:::.batch_report_increments(x)
  cap        <- max_delay %||% tbl.now:::.diag_delay_cap(increments)
  has_strata <- tbl.now:::.batch_has_strata(x)

  track <- increments |>
    dplyr::filter(.data$.delay <= cap) |>
    dplyr::group_by(.data$.stratum) |>
    dplyr::group_modify(~ .reporting_lag_one_stratum(.x, cap, window, period)) |>
    dplyr::ungroup()

  confirmed <- tbl.now:::.batch_confirmed(x, as.integer(lookback), NULL, period, alpha)
  flagged   <- dplyr::semi_join(track, confirmed,
                                by = c(".report_date" = "report_date", ".stratum" = ".stratum"))

  panel <- ggplot2::ggplot(track, ggplot2::aes(.data$.report_date)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lo, ymax = .data$hi),
                         fill = palette[["muted_green"]], alpha = 0.25) +
    ggplot2::geom_line(ggplot2::aes(y = .data$expected), colour = "grey55", linewidth = 0.3) +
    ggplot2::geom_line(ggplot2::aes(y = .data$mean_delay), colour = palette[["dark_green"]], linewidth = 0.4) +
    ggplot2::geom_point(data = flagged, ggplot2::aes(y = .data$mean_delay),
                        colour = palette[["accent_red"]], size = 1.8) +
    ggplot2::labs(
      x = "Report date", y = "Mean delay",
      title = "Reporting lag",
      subtitle = "How old the cases reported each day are"
    ) +
    tbl.now:::.tbl_now_theme(palette)
  tbl.now:::.diag_facet(panel, has_strata)
}

.reporting_lag_one_stratum <- function(cells, cap, window, period) {
  by_report <- cells |>
    dplyr::group_by(.data$.report_date) |>
    dplyr::summarise(
      n          = sum(pmax(.data$.count, 0)),
      mean_delay = sum(.data$.delay * pmax(.data$.count, 0)) / sum(pmax(.data$.count, 0)),
      .groups    = "drop"
    ) |>
    dplyr::filter(.data$n > 0, is.finite(.data$mean_delay)) |>
    dplyr::arrange(.data$.report_date)

  pmf <- cells |>
    dplyr::group_by(.data$.delay) |>
    dplyr::summarise(g = sum(pmax(.data$.count, 0)), .groups = "drop") |>
    dplyr::mutate(g = .data$g / sum(.data$g))
  mean_g <- sum(pmf$.delay * pmf$g)
  var_g  <- sum(pmf$g * (pmf$.delay - mean_g)^2)

  k <- min(as.integer(window), nrow(by_report))
  if (k %% 2L == 0L) k <- k - 1L
  k <- max(k, 3L)
  local <- if (nrow(by_report) >= 3L) {
    stats::runmed(by_report$mean_delay, k, endrule = "median")
  } else {
    rep(stats::median(by_report$mean_delay), nrow(by_report))
  }

  if (!is.null(period) && period > 1L) {
    phase <- as.integer(by_report$.report_date) %% as.integer(period)
    resid <- by_report$mean_delay - local
    offset <- stats::ave(resid, phase, FUN = function(v) stats::median(v, na.rm = TRUE))
    expected <- local + offset
  } else {
    expected <- local
  }

  se0    <- sqrt(var_g / by_report$n)
  z0     <- (by_report$mean_delay - expected) / se0
  robust <- max(stats::mad(z0, na.rm = TRUE), 1e-6)
  se     <- se0 * robust

  tibble::tibble(
    .report_date = by_report$.report_date,
    n            = by_report$n,
    mean_delay   = by_report$mean_delay,
    expected     = expected,
    lo           = pmax(expected - 3 * se, 0),
    hi           = expected + 3 * se
  )
}
