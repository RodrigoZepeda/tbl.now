# =============================================================================
# The creation-vs-transport score panel (and the detrended machinery it shares
# with diagnose_batches()).
#
# The two window scores are put on the detrended, deseasonalised, standardised
# residual -- observed minus the package's own robust baseline (a trend smooth
# times weekday factors when `period` is set), divided by its dispersion-corrected
# standard error -- so a batch reads the same whether the counts are in the tens
# or the tens of thousands.
#
# The heavier conservation monitors (cumulative backlog, reporting lag, the full
# dashboard, the transport-minus-creation "batch score") were moved out to
# devel/conservation_extras.R; they reuse the helpers here.
# =============================================================================

#' Detrended, deseasonalised, standardised residual series.
#'
#' Reuses `diagnose_batches()`'s registration: `baseline_global` is a robust trend
#' smooth optionally multiplied by weekday factors (`period`), so the residual
#' `reported - baseline_global` has the trend and the reporting calendar removed.
#' Standardising by the quasi-Poisson SE makes the series scale-free. Also carries
#' the window statistics standardised as `creation_z` and `transport_z`.
#' @keywords internal
#' @noRd
.batch_detrended <- function(data, lookback, baseline_window, period,
                             axis = "report") {
  reg  <- .batch_registration(data, lookback, baseline_window, period, axis = axis)
  disp <- .batch_dispersion(reg)
  dplyr::mutate(
    reg,
    .stratum    = .data$stratum,
    dispersion  = disp,
    residual    = .data$reported - .data$baseline_global,
    z           = .data$residual / sqrt(disp * pmax(abs(.data$baseline_global), 1)),
    creation_z  = .data$delta   / sqrt(disp * .data$window_scale),
    transport_z = .data$deficit / sqrt(disp * .data$deficit_scale)
  )
}

.batch_has_strata <- function(x) length(get_strata(x)) > 0L

#' Report dates (and strata) that `diagnose_batches()` confirms as batches.
#'
#' Marks the dates the *test itself* flags -- with its null model and
#' Benjamini-Hochberg multiplicity control -- not per-point threshold crossings,
#' which on a long series paint a red carpet whether or not any batch is real.
#' Returns a two-column frame (`report_date`, `.stratum`).
#' @keywords internal
#' @noRd
.batch_confirmed <- function(x, lookback, baseline_window, period, alpha,
                             axis = "report") {
  screened <- suppressWarnings(diagnose_batches(
    x, lookback = lookback, baseline_window = baseline_window,
    period = period, alpha = alpha, axis = axis
  ))
  confirmed <- screened[screened$batch %in% TRUE, c("report_date", "stratum"), drop = FALSE]
  names(confirmed)[match("stratum", names(confirmed))] <- ".stratum"
  dplyr::as_tibble(confirmed)
}

#' Shared preparation: the detrended registration, the diagnose_batches()-confirmed
#' rows, and the reference level.
#' @keywords internal
#' @noRd
.conservation_prep <- function(x, lookback = 7L, baseline_window = NULL,
                               period = NULL, alpha = 0.05, axis = "report") {
  reg       <- .batch_detrended(x, as.integer(lookback), baseline_window, period, axis)
  confirmed <- .batch_confirmed(x, as.integer(lookback), baseline_window, period, alpha, axis)
  list(
    reg        = reg,
    flagged    = dplyr::semi_join(reg, confirmed, by = c("report_date", ".stratum")),
    z_star     = stats::qnorm(1 - alpha),
    has_strata = .batch_has_strata(x)
  )
}

#' Signed pseudo-log y-scale for the standardised conservation series.
#'
#' The scores are z-scores (standard deviations from the expected level). A large
#' batch is genuinely hundreds of SDs out, which flattens the +/- reference band on
#' a linear axis; a signed (pseudo-)log axis keeps both the band near zero and the
#' tall batch/surge excursions readable at once.
#' @keywords internal
#' @noRd
.conservation_y_scale <- function() {
  ggplot2::scale_y_continuous(
    transform = "pseudo_log",
    breaks = c(-300, -100, -30, -10, -3, 0, 3, 10, 30, 100, 300)
  )
}

#' Creation and transport scores on one panel, in two colours.
#' @keywords internal
#' @noRd
.conservation_ct_panel <- function(prep, palette, xlab = "Report date") {
  z_star <- prep$z_star
  reg    <- prep$reg
  long <- dplyr::bind_rows(
    data.frame(report_date = reg$report_date, .stratum = reg$.stratum,
               value = reg$creation_z,  score = "creation (a surge?)"),
    data.frame(report_date = reg$report_date, .stratum = reg$.stratum,
               value = reg$transport_z, score = "transport (a batch?)")
  )
  cols <- c("creation (a surge?)"  = palette[["medium_green"]],
            "transport (a batch?)" = palette[["accent_red"]])

  p <- ggplot2::ggplot(long, ggplot2::aes(.data$report_date, .data$value,
                                          colour = .data$score))
  if (nrow(prep$flagged) > 0L) {
    p <- p + ggplot2::geom_vline(
      data = data.frame(rd = unique(prep$flagged$report_date)),
      ggplot2::aes(xintercept = .data$rd), colour = "grey85", linewidth = 0.3,
      inherit.aes = FALSE)
  }
  p <- p +
    ggplot2::geom_hline(yintercept = 0, colour = "grey80", linewidth = 0.3) +
    ggplot2::geom_hline(yintercept = c(-z_star, z_star), linetype = "dashed",
                        colour = "grey70", linewidth = 0.3) +
    ggplot2::geom_line(linewidth = 0.35, na.rm = TRUE) +
    ggplot2::scale_colour_manual(values = cols, name = NULL) +
    .conservation_y_scale() +
    ggplot2::labs(
      x = xlab, y = "z (SDs)", title = "Creation vs transport score",
      subtitle = "A batch: transport (red) breaks out while creation (green) stays flat",
      caption = paste(
        "Both are z-scores: (observed minus expected) in standard deviations. A big batch",
        "\ngenuinely sits hundreds of SDs out, so the axis is signed-log -- the dashed +/- band",
        "\nis the ordinary range. Grey verticals = diagnose_batches()-confirmed batch dates."
      )
    ) +
    .tbl_now_theme(palette) +
    ggplot2::theme(legend.position = "top")
  .diag_facet(p, prep$has_strata)
}

#' One score (creation or transport) as its own line panel.
#' @keywords internal
#' @noRd
.conservation_score_line <- function(prep, value, title, subtitle, colour, palette,
                                     xlab = "Report date", caption = NULL) {
  z_star <- prep$z_star
  p <- ggplot2::ggplot(prep$reg, ggplot2::aes(.data$report_date, .data[[value]])) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey80", linewidth = 0.3) +
    ggplot2::geom_hline(yintercept = c(-z_star, z_star), linetype = "dashed",
                        colour = "grey70", linewidth = 0.3) +
    ggplot2::geom_line(colour = colour, linewidth = 0.35, na.rm = TRUE) +
    ggplot2::geom_point(data = prep$flagged, ggplot2::aes(y = .data[[value]]),
                        colour = palette[["accent_red"]], size = 1.8, na.rm = TRUE) +
    .conservation_y_scale() +
    ggplot2::labs(x = xlab, y = "z (SDs)", title = title, subtitle = subtitle,
                  caption = caption) +
    .tbl_now_theme(palette)
  .diag_facet(p, prep$has_strata)
}
