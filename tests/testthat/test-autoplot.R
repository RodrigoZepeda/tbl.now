library(dplyr, quietly = TRUE, warn.conflicts = FALSE)

# A small daily count-incidence tbl_now used across tests
make_daily_now <- function() {
  set.seed(42)
  dates <- seq(as.Date("2021-01-01"), as.Date("2021-03-31"), by = "day")
  rows  <- lapply(dates, function(d) {
    max_delay <- rpois(1, 3)
    data.frame(
      event_date  = d,
      report_date = d + 0:max_delay,
      n           = rpois(max_delay + 1, 4)
    )
  })
  df <- do.call(rbind, rows)
  tbl_now(df, event_date = event_date, report_date = report_date, case_count = n,
          data_type = "count-incidence", event_units = "days", report_units = "days",
          verbose = FALSE)
}

test_that("autoplot.tbl_now returns a patchwork object (weekly linelist)", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")

  data(denguedat)
  dengue <- tbl_now(denguedat, event_date = "onset_week", report_date = "report_week",
                    verbose = FALSE)
  p <- ggplot2::autoplot(dengue)
  expect_s3_class(p, "patchwork")
})

test_that("autoplot.tbl_now works on daily count data", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")

  p <- ggplot2::autoplot(make_daily_now())
  expect_s3_class(p, "patchwork")
  # daily data => 5 panels (delay, epidemic, day-of-week, week-of-year, periodogram)
  expect_length(p$patches$plots, 4)  # patchwork stores n-1 in $plots + the last
})

test_that("daily autoplot has 5 panels, weekly has 4", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")

  p_daily <- ggplot2::autoplot(make_daily_now())
  expect_length(p_daily$patches$plots, 4)   # 5 panels

  data(denguedat)
  dengue <- tbl_now(denguedat, event_date = "onset_week", report_date = "report_week",
                    verbose = FALSE)
  p_weekly <- ggplot2::autoplot(dengue)
  expect_length(p_weekly$patches$plots, 3)  # 4 panels
})

test_that("calendar groupings depend on event units", {
  expect_equal(tbl.now:::.tbl_now_calendar_groupings("days"),   c("weekday", "week"))
  expect_equal(tbl.now:::.tbl_now_calendar_groupings("weeks"),  "week")
  expect_equal(tbl.now:::.tbl_now_calendar_groupings("months"), "month")
  expect_length(tbl.now:::.tbl_now_calendar_groupings("numeric"), 0)
})

test_that("autoplot.tbl_now errors on a non-tbl_now", {
  skip_if_not_installed("ggplot2")
  expect_error(autoplot.tbl_now(data.frame(a = 1)), "tbl_now")
})

test_that("autoplot.tbl_now validates level", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")

  nowobj <- make_daily_now()
  expect_error(ggplot2::autoplot(nowobj, level = -1),   "level")
  expect_error(ggplot2::autoplot(nowobj, level = 1.1),   "level")
  expect_error(ggplot2::autoplot(nowobj, level = 1.5), "level")
  expect_error(ggplot2::autoplot(nowobj, level = "a"), "level")
})

test_that("autoplot.tbl_now accepts a custom level without error", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")

  expect_s3_class(ggplot2::autoplot(make_daily_now(), level = 0.8), "patchwork")
})

# --- internal helpers -------------------------------------------------------

test_that("weighted quantile matches an unweighted quantile when weights equal", {
  values  <- c(0, 1, 2, 3, 4, 5)
  weights <- rep(1, length(values))
  q <- tbl.now:::.tbl_now_weighted_quantile(values, weights, 0.5)
  expect_true(q %in% values)
  # 0.95 quantile should be near the top
  expect_equal(tbl.now:::.tbl_now_weighted_quantile(values, weights, 1), 5)
})

test_that("weighted quantile respects weights", {
  values  <- c(0, 10)
  weights <- c(99, 1)             # nearly all mass at 0
  expect_equal(tbl.now:::.tbl_now_weighted_quantile(values, weights, 0.95), 0)
  expect_equal(tbl.now:::.tbl_now_weighted_quantile(values, weights, 0.999), 10)
})

test_that("delay distribution helper returns positive weights", {
  dd <- tbl.now:::.tbl_now_delay_distribution(make_daily_now())
  expect_true(all(dd$weight > 0))
  expect_true(all(dd$delay >= 0))
})

test_that("epidemic process helper aggregates to one row per event date", {
  nowobj <- make_daily_now()
  ep <- tbl.now:::.tbl_now_epidemic_process(nowobj)
  expect_named(ep, c("event_date", "case_count"))
  expect_equal(nrow(ep), dplyr::n_distinct(nowobj[[get_event_date(nowobj)]]))
})

test_that("units_to_days multiplier is correct", {
  expect_equal(tbl.now:::.tbl_now_units_to_days("days"), 1)
  expect_equal(tbl.now:::.tbl_now_units_to_days("weeks"), 7)
})

# --- normalized calendar effect --------------------------------------------

test_that("calendar panel draws boxplots of a normalized effect centred on ~1", {
  skip_if_not_installed("ggplot2")

  nowobj <- make_daily_now()
  panel  <- tbl.now:::.tbl_now_panel_calendar(
    tbl.now:::.tbl_now_epidemic_process(nowobj), "weekday", tbl.now:::.tbl_now_palette()
  )
  # geom_boxplot layer
  expect_s3_class(panel$layers[[1]]$geom, "GeomBoxplot")

  built       <- ggplot2::ggplot_build(panel)
  box_medians <- built$data[[1]]$middle  # boxplot medians per weekday
  expect_true(all(box_medians > 0))
  expect_lt(abs(mean(box_medians) - 1), 0.5)
  expect_equal(panel$labels$y, "Normalized effect")
})

test_that("calendar panel supports weekday, week and month groupings", {
  skip_if_not_installed("ggplot2")

  ep <- tbl.now:::.tbl_now_epidemic_process(make_daily_now())
  for (grouping in c("weekday", "week", "month")) {
    panel <- tbl.now:::.tbl_now_panel_calendar(ep, grouping, tbl.now:::.tbl_now_palette())
    expect_s3_class(panel, "ggplot")
  }
})

# --- x-axis limits ----------------------------------------------------------

test_that("autoplot accepts per-panel x-axis limits", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")

  nowobj <- make_daily_now()
  p <- ggplot2::autoplot(
    nowobj,
    delay_distribution_xlim = c(0, 8),
    event_date_xlim         = as.Date(c("2021-02-01", "2021-03-31")),
    calendar_effect_xlim    = c(0.5, 7.5),
    seasonality_xlim        = c(0, 30)
  )
  expect_s3_class(p, "patchwork")
})

test_that(".tbl_now_apply_xlim adds a coord and rejects bad input", {
  skip_if_not_installed("ggplot2")
  base <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) +
    ggplot2::geom_point()
  expect_identical(tbl.now:::.tbl_now_apply_xlim(base, NULL), base)
  zoomed <- tbl.now:::.tbl_now_apply_xlim(base, c(1, 2))
  expect_s3_class(zoomed$coordinates, "CoordCartesian")
  expect_error(tbl.now:::.tbl_now_apply_xlim(base, c(1, 2, 3)), "length-2")
})

# --- holiday dots -----------------------------------------------------------

test_that("holiday points are empty when there is no holiday spec", {
  nowobj <- make_daily_now()
  ep     <- tbl.now:::.tbl_now_epidemic_process(nowobj)
  hp     <- tbl.now:::.tbl_now_holiday_points(nowobj, ep)
  expect_equal(nrow(hp), 0)
})

test_that("holiday points are detected from a temporal_effects spec", {
  skip_if_not_installed("almanac")

  nowobj <- make_daily_now() %>%
    add_temporal_effects(temporal_effects(holidays = almanac::cal_us_federal()))
  ep <- tbl.now:::.tbl_now_epidemic_process(nowobj)
  hp <- tbl.now:::.tbl_now_holiday_points(nowobj, ep)
  expect_gt(nrow(hp), 0)
  expect_true(all(hp$event_date %in% ep$event_date))
})

test_that("autoplot renders with holidays marked", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  skip_if_not_installed("almanac")

  nowobj <- make_daily_now() %>%
    add_temporal_effects(temporal_effects(holidays = almanac::cal_us_federal()))
  expect_s3_class(ggplot2::autoplot(nowobj), "patchwork")
})
