# =============================================================================
# plot_scalogram(): window-inner wavelet scalogram of the reporting / epidemic
# series (needs the suggested wavScalogram package).
# =============================================================================

make_scalo_tbl <- function(seed = 11L) {
  set.seed(seed)
  onset    <- rep(seq(as.Date("2024-01-01"), by = "day", length.out = 80L),
                  times = round(60 * dnorm(seq(-2.5, 2.5, length.out = 80L)) /
                                  max(dnorm(seq(-2.5, 2.5, length.out = 80L)))) + 5)
  reported <- onset + stats::rpois(length(onset), 1.5)
  tbl_now(data.frame(onset = onset, reported = reported),
          event_date = onset, report_date = reported,
          data_type = "linelist", verbose = FALSE)
}

test_that("plot_scalogram builds for both series and validates input", {
  skip_if_not_installed("wavScalogram")
  tn <- make_scalo_tbl()
  expect_s3_class(suppressWarnings(plot_scalogram(tn, type = "reporting")), "ggplot")
  expect_s3_class(suppressWarnings(plot_scalogram(tn, type = "epidemic")), "ggplot")
  expect_error(plot_scalogram(mtcars), class = "rlang_error")
  expect_error(plot_scalogram(tn, type = "nonsense"))
})

test_that("the scalogram tiles a gapless uniform index grid", {
  skip_if_not_installed("wavScalogram")
  tn <- make_scalo_tbl()
  p  <- suppressWarnings(plot_scalogram(tn, type = "reporting"))
  dat <- ggplot2::ggplot_build(p)$data[[1]]
  # x is a uniform window index (width 1), so tiles touch: consecutive integers,
  # no gaps. (Drawing on the Date axis left holes when centres were spaced apart.)
  xs <- sort(unique(dat$x))
  expect_equal(max(diff(xs)), 1)
  expect_equal(unique(dat$width), 1)
})

test_that("weekly data is analysed on its own grid (not forced to days)", {
  skip_if_not_installed("wavScalogram")
  data(denguedat)
  dn  <- tbl_now(denguedat, onset_week, report_week, verbose = FALSE)
  p   <- suppressWarnings(plot_scalogram(dn, type = "reporting"))
  dat <- ggplot2::ggplot_build(p)$data[[1]]
  # Gapless even on the full 20-year series, where wavScalogram subsamples the
  # window centres (the old Date-axis + fixed width left 4-in-5 columns blank).
  xs <- sort(unique(dat$x))
  expect_equal(max(diff(xs)), 1)
  # Periods are in the object's unit (weeks): the smallest is ~2 weeks, not ~2 days.
  expect_gt(min(2^dat$y), 1.5)
})
