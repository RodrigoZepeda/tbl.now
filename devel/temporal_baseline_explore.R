# =============================================================================
# EXPLORATION (devel): can day-of-week temporal effects be folded into the
# baseline so the creation / transport scores show trend, not weekly wiggle?
#
# We compare four definitions of the "expected reports" for each report date,
# all built on the same robust trend smooth, and measure how much of the leftover
# wiggle is still explained by the day of the week (lower = the weekly effect has
# been absorbed). We also keep an eye on how much of the trend survives.
#
# Run: devtools::load_all("."); source("devel/temporal_baseline_explore.R")
#
# FINDING (covid_us, 2026-07-10): the squiggle in the creation / transport scores
# is NOT a day-of-week effect.
#   * The raw daily reports DO have a real weekly pattern (~Sun 0.73x, Wed 1.22x).
#   * But the SCORES already have it removed: weekly R^2 of transport_z/creation_z
#     is ~0.000-0.001 whether or not period=7 is passed. The window differencing +
#     leave-window-out baseline difference the weekly pattern out on their own.
#   * transport_z lag-1 autocorrelation is ~0.92 (smooth), not the negative
#     zig-zag a weekly artefact would give. The wiggle is genuine day-to-day
#     over-dispersion of the reporting process seen through overlapping windows.
#   * Among baseline definitions the multiplicative "trend x weekday" (= the
#     period=7 option) is best for the DAILY residual (weekly R^2 0.011 -> 0.004);
#     additive and average variants are close, but none reaches the scores because
#     they are already de-weeked. So no temporal-effect definition smooths the
#     scores; passing period=7 is still worth it for the daily reporting views.
# =============================================================================

suppressPackageStartupMessages({library(dplyr); library(ggplot2); library(tidyr)})

explore_temporal <- function(x, baseline_window = 29L) {
  inc <- suppressWarnings(tbl.now:::.batch_report_increments(x))
  tot <- inc |>
    dplyr::group_by(.report_date) |>
    dplyr::summarise(y = sum(pmax(.count, 0)), .groups = "drop") |>
    dplyr::arrange(.report_date)

  y     <- tot$y
  trend <- tbl.now:::.batch_repeated_median(y, baseline_window)   # robust trend
  wd    <- factor(weekdays(tot$.report_date),
                  levels = weekdays(as.Date("2024-01-01") + 0:6)) # Mon..Sun

  # (1) trend only; (2) trend x weekday factor (multiplicative, = period=7 idea);
  # (3) trend + weekday offset (additive); (4) the average of (2) and (3).
  ratio     <- y / pmax(trend, 1)
  mult_f    <- tapply(ratio, wd, stats::median)[wd]
  off       <- tapply(y - trend, wd, stats::median)[wd]
  base_mult <- pmax(trend * as.numeric(mult_f), 0)
  base_add  <- pmax(trend + as.numeric(off), 0)
  base_avg  <- (base_mult + base_add) / 2

  bases <- list(`trend only` = trend, `x weekday (mult)` = base_mult,
                `+ weekday (add)` = base_add, `average of the two` = base_avg)

  # weekly wiggle left in the residual = R^2 of residual ~ day-of-week.
  weekly_left <- function(b) {
    r <- (y - b) / sqrt(pmax(b, 1))                 # variance-stabilised residual
    summary(stats::lm(r ~ wd))$r.squared
  }
  # lag-1 autocorrelation (a fast up-down-up wiggle shows as a strong negative ac1)
  ac1 <- function(b) {
    r <- (y - b) / sqrt(pmax(b, 1))
    stats::acf(r, lag.max = 1, plot = FALSE, na.action = stats::na.pass)$acf[2]
  }

  summ <- data.frame(
    baseline    = names(bases),
    weekly_R2   = round(vapply(bases, weekly_left, numeric(1)), 3),
    lag1_autocor= round(vapply(bases, ac1, numeric(1)), 3),
    row.names   = NULL
  )
  cat("== weekly wiggle left in the residual (lower = weekly effect removed) ==\n")
  print(summ)

  # a picture: the variance-stabilised residual under each baseline
  long <- purrr_map_bases(tot$.report_date, y, bases)
  p <- ggplot(long, aes(report_date, resid)) +
    geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.3) +
    geom_line(colour = tbl.now:::.tbl_now_palette()[["dark_green"]], linewidth = 0.3) +
    facet_wrap(~ baseline, ncol = 1, scales = "free_y") +
    labs(x = "Report date", y = "residual (SD units)",
         title = "Leftover wiggle under four baseline definitions",
         subtitle = "The flatter the weekly zig-zag, the better the day-of-week effect is absorbed") +
    tbl.now:::.tbl_now_theme(tbl.now:::.tbl_now_palette())
  list(summary = summ, plot = p)
}

purrr_map_bases <- function(dates, y, bases) {
  do.call(rbind, lapply(names(bases), function(nm) {
    b <- bases[[nm]]
    data.frame(report_date = dates, baseline = nm,
               resid = (y - b) / sqrt(pmax(b, 1)))
  }))
}
