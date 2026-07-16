library(dplyr, quietly = TRUE, warn.conflicts = FALSE)

# A small daily count-incidence tbl_now used across tests
make_daily_now <- function() {
  set.seed(42)
  dates <- seq(as.Date("2021-01-01"), as.Date("2021-03-31"), by = "day")
  rows <- lapply(dates, function(d) {
    max_delay <- rpois(1, 3)
    data.frame(
      event_date  = d,
      report_date = d + 0:max_delay,
      n           = rpois(max_delay + 1, 4)
    )
  })
  df <- do.call(rbind, rows)
  tbl_now(df,
    event_date = event_date, report_date = report_date, case_count = n,
    data_type = "count-incidence", event_units = "days", report_units = "days",
    verbose = FALSE
  )
}

test_that("autoplot.tbl_now returns a patchwork object (weekly linelist)", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")

  data(denguedat)
  dengue <- tbl_now(denguedat,
    event_date = "onset_week", report_date = "report_week",
    verbose = FALSE
  )
  p <- ggplot2::autoplot(dengue)
  expect_s3_class(p, "patchwork")
})

test_that("autoplot.tbl_now works on daily count data", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")

  p <- ggplot2::autoplot(make_daily_now())
  expect_s3_class(p, "patchwork")
  # daily data => 8 panels: delay distribution, epidemic, day-of-week &
  # week-of-year (case), seasonality, day-of-week & week-of-year (delay), delay
  # periodogram. patchwork stores n-1 in $plots + the last.
  expect_length(p$patches$plots, 7)
})

test_that("daily autoplot has 8 panels, weekly has 6", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")

  p_daily <- ggplot2::autoplot(make_daily_now())
  expect_length(p_daily$patches$plots, 7) # 8 panels

  data(denguedat)
  dengue <- tbl_now(denguedat,
    event_date = "onset_week", report_date = "report_week",
    verbose = FALSE
  )
  p_weekly <- ggplot2::autoplot(dengue)
  expect_length(p_weekly$patches$plots, 5) # 6 panels
})

test_that("autoplot panel selection returns the requested subset", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")

  nowobj <- make_daily_now()

  # A single panel comes back as a plain ggplot (not a patchwork)
  single <- ggplot2::autoplot(nowobj, panels = "delay_week")
  expect_s3_class(single, "ggplot")
  expect_false(inherits(single, "patchwork"))

  # Two panels come back as a patchwork
  two <- ggplot2::autoplot(nowobj, panels = c("epidemic", "delay_seasonality"))
  expect_s3_class(two, "patchwork")
  expect_length(two$patches$plots, 1) # 2 panels

  # The "delay_calendar" alias expands to both delay calendar panels (daily)
  delay_cal <- ggplot2::autoplot(nowobj, panels = "delay_calendar")
  expect_length(delay_cal$patches$plots, 1) # 2 panels (weekday + week)
})

test_that("autoplot rejects unknown panels and warns on inapplicable ones", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")

  expect_error(ggplot2::autoplot(make_daily_now(), panels = "not_a_panel"), "Unknown panel")

  data(denguedat)
  dengue <- tbl_now(denguedat,
    event_date = "onset_week", report_date = "report_week", verbose = FALSE
  )
  # weekly data has no day-of-week panel
  expect_warning(
    ggplot2::autoplot(dengue, panels = c("delay_week", "delay_weekday")),
    "does not apply"
  )
})

test_that("panel key vocabulary depends on the event unit", {
  expect_equal(
    tbl.now:::.tbl_now_all_panel_keys("days"),
    c("delay_distribution", "epidemic", "calendar_weekday", "calendar_week",
      "seasonality", "delay_weekday", "delay_week", "delay_seasonality")
  )
  expect_equal(
    tbl.now:::.tbl_now_all_panel_keys("weeks"),
    c("delay_distribution", "epidemic", "calendar_week", "seasonality",
      "delay_week", "delay_seasonality")
  )
  # "all" resolves to every applicable key, in canonical order
  expect_equal(
    tbl.now:::.tbl_now_resolve_panels("all", "weeks"),
    tbl.now:::.tbl_now_all_panel_keys("weeks")
  )
})

test_that("calendar groupings depend on event units", {
  expect_equal(tbl.now:::.tbl_now_calendar_groupings("days"), c("weekday", "week"))
  expect_equal(tbl.now:::.tbl_now_calendar_groupings("weeks"), "week")
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
  expect_error(ggplot2::autoplot(nowobj, level = -1), "level")
  expect_error(ggplot2::autoplot(nowobj, level = 1.1), "level")
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
  values <- c(0, 1, 2, 3, 4, 5)
  weights <- rep(1, length(values))
  q <- tbl.now:::.tbl_now_weighted_quantile(values, weights, 0.5)
  expect_true(q %in% values)
  # 0.95 quantile should be near the top
  expect_equal(tbl.now:::.tbl_now_weighted_quantile(values, weights, 1), 5)
})

test_that("weighted quantile respects weights", {
  values <- c(0, 10)
  weights <- c(99, 1) # nearly all mass at 0
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

test_that("delay-per-date helper returns one weighted mean delay per event date", {
  nowobj <- make_daily_now()
  dpd <- tbl.now:::.tbl_now_delay_per_date(nowobj)
  expect_named(dpd, c("event_date", "mean_delay", "cases"))
  expect_equal(nrow(dpd), dplyr::n_distinct(nowobj[[get_event_date(nowobj)]]))
  expect_true(all(dpd$mean_delay >= 0))
  expect_true(all(dpd$cases > 0))
})

# --- reporting-delay effect panels -----------------------------------------

test_that("delay calendar panel plots a normalized delay effect centred on ~1", {
  skip_if_not_installed("ggplot2")

  dpd <- tbl.now:::.tbl_now_delay_per_date(make_daily_now())
  panel <- tbl.now:::.tbl_now_panel_delay_calendar(
    dpd, "weekday", tbl.now:::.tbl_now_palette()
  )
  expect_s3_class(panel$layers[[1]]$geom, "GeomBoxplot")
  expect_equal(panel$labels$y, "Normalized delay")

  built <- ggplot2::ggplot_build(panel)
  box_medians <- built$data[[1]]$middle # boxplot medians per weekday
  expect_true(all(box_medians > 0))
  # normalized by the overall mean delay, so the boxes straddle 1
  expect_lt(abs(mean(box_medians) - 1), 0.5)
})

test_that("delay calendar panel supports weekday, week and month groupings", {
  skip_if_not_installed("ggplot2")

  dpd <- tbl.now:::.tbl_now_delay_per_date(make_daily_now())
  for (grouping in c("weekday", "week", "month")) {
    panel <- tbl.now:::.tbl_now_panel_delay_calendar(
      dpd, grouping, tbl.now:::.tbl_now_palette()
    )
    expect_s3_class(panel, "ggplot")
  }
})

test_that("delay periodogram panel is a periodogram of the mean-delay series", {
  skip_if_not_installed("ggplot2")

  dpd <- tbl.now:::.tbl_now_delay_per_date(make_daily_now())
  panel <- tbl.now:::.tbl_now_panel_delay_periodogram(
    dpd, "days", tbl.now:::.tbl_now_palette()
  )
  expect_s3_class(panel, "ggplot")
  expect_equal(panel$labels$y, "Spectral power")
})

# --- by-strata mode ---------------------------------------------------------

make_daily_strata_now <- function() {
  set.seed(11)
  dates <- seq(as.Date("2021-01-01"), as.Date("2021-04-30"), by = "day")
  rows <- do.call(rbind, lapply(dates, function(d) {
    do.call(rbind, lapply(c("a", "b"), function(g) {
      md <- rpois(1, 3)
      data.frame(event_date = d, report_date = d + 0:md, n = rpois(md + 1, 4), grp = g)
    }))
  }))
  tbl_now(rows,
    event_date = event_date, report_date = report_date, case_count = n,
    strata = "grp", data_type = "count-incidence",
    event_units = "days", report_units = "days", verbose = FALSE
  )
}

test_that("by_strata splits every panel and keeps the panel count", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")

  nowobj <- make_daily_strata_now()
  p <- ggplot2::autoplot(nowobj, by_strata = TRUE)
  expect_s3_class(p, "patchwork")
  expect_length(p$patches$plots, 7) # 8 panels, same as non-strata daily

  # A single by-strata panel still comes back as a plain ggplot
  single <- ggplot2::autoplot(nowobj, panels = "delay_week", by_strata = TRUE)
  expect_s3_class(single, "ggplot")
  expect_false(inherits(single, "patchwork"))
})

test_that("by_strata errors without strata and honours the strata override", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")

  # No strata attached -> informative error
  expect_error(ggplot2::autoplot(make_daily_now(), by_strata = TRUE), "strata")

  # Unknown override column -> error
  expect_error(
    ggplot2::autoplot(make_daily_strata_now(), by_strata = TRUE, strata = "nope"),
    "not in the data"
  )

  # by_strata must be a single logical
  expect_error(ggplot2::autoplot(make_daily_strata_now(), by_strata = "yes"), "by_strata")
})

test_that("by-strata data helpers carry a combined strata label", {
  nowobj <- make_daily_strata_now()

  ep <- tbl.now:::.tbl_now_epidemic_process_by(nowobj, "grp")
  expect_true(all(c("event_date", "case_count", "strata") %in% names(ep)))
  expect_setequal(unique(ep$strata), c("a", "b"))

  dd <- tbl.now:::.tbl_now_delay_distribution_by(nowobj, "grp")
  expect_true(all(c("strata", "delay", "density") %in% names(dd)))
  # densities sum to 1 within each stratum
  sums <- tapply(dd$density, dd$strata, sum)
  expect_true(all(abs(sums - 1) < 1e-8))

  dpd <- tbl.now:::.tbl_now_delay_per_date_by(nowobj, "grp")
  expect_true(all(c("strata", "event_date", "mean_delay", "cases") %in% names(dpd)))
})

test_that("multiple strata columns are combined into one label", {
  set.seed(3)
  dates <- seq(as.Date("2021-01-01"), as.Date("2021-02-28"), by = "day")
  rows <- do.call(rbind, lapply(dates, function(d) {
    data.frame(event_date = d, report_date = d + 0:1, n = 2, sex = "F", age = "adult")
  }))
  nowobj <- tbl_now(rows,
    event_date = event_date, report_date = report_date, case_count = n,
    strata = c("sex", "age"), data_type = "count-incidence",
    event_units = "days", report_units = "days", verbose = FALSE
  )
  ep <- tbl.now:::.tbl_now_epidemic_process_by(nowobj, c("sex", "age"))
  expect_equal(unique(ep$strata), "F / adult")
})

test_that("delay panels degrade gracefully on an empty / tiny series", {
  skip_if_not_installed("ggplot2")

  empty <- dplyr::tibble(
    event_date = as.Date(character(0)), mean_delay = numeric(0), cases = numeric(0)
  )
  cal <- tbl.now:::.tbl_now_panel_delay_calendar(
    empty, "weekday", tbl.now:::.tbl_now_palette()
  )
  expect_s3_class(cal, "ggplot")
  peri <- tbl.now:::.tbl_now_panel_delay_periodogram(
    empty, "days", tbl.now:::.tbl_now_palette()
  )
  expect_s3_class(peri, "ggplot")
})

# --- normalized calendar effect --------------------------------------------

test_that("calendar panel draws boxplots of a normalized effect centred on ~1", {
  skip_if_not_installed("ggplot2")

  nowobj <- make_daily_now()
  panel <- tbl.now:::.tbl_now_panel_calendar(
    tbl.now:::.tbl_now_epidemic_process(nowobj), "weekday", tbl.now:::.tbl_now_palette()
  )
  # geom_boxplot layer
  expect_s3_class(panel$layers[[1]]$geom, "GeomBoxplot")

  built <- ggplot2::ggplot_build(panel)
  box_medians <- built$data[[1]]$middle # boxplot medians per weekday
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
  ep <- tbl.now:::.tbl_now_epidemic_process(nowobj)
  hp <- tbl.now:::.tbl_now_holiday_points(nowobj, ep)
  expect_equal(nrow(hp), 0)
})

test_that("holiday points are detected from a temporal_effects spec", {
  skip_if_not_installed("almanac")

  nowobj <- make_daily_now() |>
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

  nowobj <- make_daily_now() |>
    add_temporal_effects(temporal_effects(holidays = almanac::cal_us_federal()))
  expect_s3_class(ggplot2::autoplot(nowobj), "patchwork")
})

# --- cumulative growth (count-cumulative delay panel) -----------------------

make_cumulative_now <- function() {
  # two event dates, cumulative counts revised UP then DOWN over report delay
  df <- data.frame(
    event_date  = as.Date(c(rep("2021-01-04", 4), rep("2021-01-11", 4))),
    report_date = as.Date(c(
      "2021-01-04", "2021-01-11", "2021-01-18", "2021-01-25",
      "2021-01-11", "2021-01-18", "2021-01-25", "2021-02-01"
    )),
    n = c(10, 30, 30, 21, 5, 8, 8, 8) # event 1: x3 then stable then down to 70%
  )
  tbl_now(df,
    event_date = event_date, report_date = report_date, case_count = n,
    data_type = "count-cumulative", event_units = "weeks", report_units = "weeks",
    verbose = FALSE
  )
}

test_that("cumulative growth helper computes ratios to the previous delay", {
  g <- tbl.now:::.tbl_now_cumulative_growth(make_cumulative_now())
  expect_true(all(c("event_date", "delay", "ratio") %in% names(g)))
  expect_true(all(is.finite(g$ratio) & g$ratio > 0))
  # event 1 delay 1: 30/10 = 3 ; delay 3: 21/30 = 0.7
  ev1 <- g[g$event_date == as.Date("2021-01-04"), ]
  expect_equal(ev1$ratio[ev1$delay == 1], 3)
  expect_equal(ev1$ratio[ev1$delay == 3], 0.7)
})

test_that("count-cumulative delay panel is the growth panel (log scale)", {
  skip_if_not_installed("ggplot2")
  p <- ggplot2::autoplot(make_cumulative_now(), panels = "delay_distribution")
  expect_equal(p$labels$title, "Cumulative growth by delay")
  expect_equal(p$labels$y, "Cumulative count ratio")
  # a log-10 y scale is applied
  is_log <- any(vapply(p$scales$scales, function(s) {
    !is.null(s$trans) && identical(s$trans$name, "log-10")
  }, logical(1)))
  expect_true(is_log)
  expect_s3_class(ggplot2::ggplot_build(p), "ggplot_built")
})

test_that("non-cumulative data keeps the histogram delay panel", {
  skip_if_not_installed("ggplot2")
  p <- ggplot2::autoplot(make_daily_now(), panels = "delay_distribution")
  expect_equal(p$labels$title, "Empirical delay distribution")
})

test_that("cumulative growth panel works by stratum and full autoplot builds", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")

  df <- data.frame(
    event_date  = as.Date(rep(c("2021-01-04", "2021-01-11"), each = 6)),
    report_date = as.Date(rep(c("2021-01-04", "2021-01-11", "2021-01-18"), 4)),
    grp = rep(c("a", "b"), each = 3, times = 2),
    n = c(10, 20, 22, 5, 9, 9, 8, 12, 12, 4, 6, 6)
  )
  nowobj <- tbl_now(df,
    event_date = event_date, report_date = report_date, case_count = n,
    strata = "grp", data_type = "count-cumulative",
    event_units = "weeks", report_units = "weeks", verbose = FALSE
  )
  ps <- ggplot2::autoplot(nowobj, panels = "delay_distribution", by_strata = TRUE)
  expect_equal(ps$labels$title, "Cumulative growth by delay")
  expect_s3_class(ggplot2::autoplot(nowobj, by_strata = TRUE), "patchwork")
})
