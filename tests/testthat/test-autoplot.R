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
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")

  nowobj <- make_daily_now()
  expect_error(ggplot2::autoplot(nowobj, level = -1), "level")
  expect_error(ggplot2::autoplot(nowobj, level = 1.1), "level")
  expect_error(ggplot2::autoplot(nowobj, level = 1.5), "level")
  expect_error(ggplot2::autoplot(nowobj, level = "a"), "level")
})

test_that("autoplot.tbl_now accepts a custom level without error", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")

  expect_s3_class(ggplot2::autoplot(make_daily_now(), level = 0.8), "patchwork")
})

# --- internal helpers -------------------------------------------------------

test_that("weighted quantile matches an unweighted quantile when weights equal", {
  skip_on_cran()
  values <- c(0, 1, 2, 3, 4, 5)
  weights <- rep(1, length(values))
  q <- tbl.now:::.tbl_now_weighted_quantile(values, weights, 0.5)
  expect_true(q %in% values)
  # 0.95 quantile should be near the top
  expect_equal(tbl.now:::.tbl_now_weighted_quantile(values, weights, 1), 5)
})

test_that("weighted quantile respects weights", {
  skip_on_cran()
  values <- c(0, 10)
  weights <- c(99, 1) # nearly all mass at 0
  expect_equal(tbl.now:::.tbl_now_weighted_quantile(values, weights, 0.95), 0)
  expect_equal(tbl.now:::.tbl_now_weighted_quantile(values, weights, 0.999), 10)
})

test_that("delay distribution helper returns positive weights", {
  skip_on_cran()
  dd <- tbl.now:::.tbl_now_delay_distribution(make_daily_now())
  expect_true(all(dd$weight > 0))
  expect_true(all(dd$delay >= 0))
})

test_that("epidemic process helper aggregates to one row per event date", {
  skip_on_cran()
  nowobj <- make_daily_now()
  ep <- tbl.now:::.tbl_now_epidemic_process(nowobj)
  expect_named(ep, c("event_date", "case_count"))
  expect_equal(nrow(ep), dplyr::n_distinct(nowobj[[get_event_date(nowobj)]]))
})

test_that("units_to_days multiplier is correct", {
  skip_on_cran()
  expect_equal(tbl.now:::.tbl_now_units_to_days("days"), 1)
  expect_equal(tbl.now:::.tbl_now_units_to_days("weeks"), 7)
})

test_that("delay-per-date helper returns one weighted mean delay per event date", {
  skip_on_cran()
  nowobj <- make_daily_now()
  dpd <- tbl.now:::.tbl_now_delay_per_date(nowobj)
  expect_named(dpd, c("event_date", "mean_delay", "cases"))
  expect_equal(nrow(dpd), dplyr::n_distinct(nowobj[[get_event_date(nowobj)]]))
  expect_true(all(dpd$mean_delay >= 0))
  expect_true(all(dpd$cases > 0))
})

# --- reporting-delay effect panels -----------------------------------------

test_that("delay calendar panel plots a normalized delay effect centred on ~1", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")

  dpd <- tbl.now:::.tbl_now_delay_per_date(make_daily_now())
  panel <- tbl.now:::.tbl_now_panel_delay_calendar(
    dpd, "weekday", tbl.now:::.tbl_now_palette()
  )
  expect_s3_class(panel$layers[[1]]$geom, "GeomBoxplot")
  expect_equal(panel$labels$y, "Normalized mean delay (1 = average)")

  built <- ggplot2::ggplot_build(panel)
  box_medians <- built$data[[1]]$middle # boxplot medians per weekday
  expect_true(all(box_medians > 0))
  # normalized by the overall mean delay, so the boxes straddle 1
  expect_lt(abs(mean(box_medians) - 1), 0.5)
})

test_that("delay calendar panel supports weekday, week and month groupings", {
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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

test_that("strata= works on a column that is not a declared stratum", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")

  # `grp` is a column of the data but was never declared as a stratum, so
  # `to_count()` would sum it away before the panels could group on it.
  set.seed(11)
  dates <- seq(as.Date("2021-01-01"), as.Date("2021-04-30"), by = "day")
  rows <- do.call(rbind, lapply(dates, function(day) {
    do.call(rbind, lapply(c("a", "b"), function(group) {
      max_delay <- rpois(1, 3)
      data.frame(
        event_date  = day,
        report_date = day + 0:max_delay,
        n           = rpois(max_delay + 1, 4),
        grp         = group
      )
    }))
  }))
  # Leaving `grp` undeclared makes (event, report) pairs repeat, which `tbl_now()`
  # rightly flags — that non-uniqueness is the scenario under test.
  undeclared <- suppressWarnings(tbl_now(rows,
    event_date = event_date, report_date = report_date, case_count = n,
    data_type = "count-incidence",
    event_units = "days", report_units = "days", verbose = FALSE
  ))
  expect_length(get_strata(undeclared), 0)

  expect_no_error(
    ggplot2::autoplot(undeclared, strata = "grp", by_strata = TRUE)
  )

  # Passing strata= must match declaring the stratum up front.
  from_argument <- ggplot2::autoplot(undeclared, panels = "epidemic",
                                     strata = "grp", by_strata = TRUE)
  from_declared <- ggplot2::autoplot(add_strata(undeclared, grp),
                                     panels = "epidemic", by_strata = TRUE)
  expect_equal(
    ggplot2::ggplot_build(from_argument)$data,
    ggplot2::ggplot_build(from_declared)$data
  )
})

test_that("by-strata data helpers carry a combined strata label", {
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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
  expect_equal(panel$labels$y, "Normalized effect (1 = average)")
})

test_that("calendar panel supports weekday, week and month groupings", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")

  ep <- tbl.now:::.tbl_now_epidemic_process(make_daily_now())
  for (grouping in c("weekday", "week", "month")) {
    panel <- tbl.now:::.tbl_now_panel_calendar(ep, grouping, tbl.now:::.tbl_now_palette())
    expect_s3_class(panel, "ggplot")
  }
})

# --- x-axis limits ----------------------------------------------------------

test_that("autoplot accepts per-panel x-axis limits", {
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
  nowobj <- make_daily_now()
  ep <- tbl.now:::.tbl_now_epidemic_process(nowobj)
  hp <- tbl.now:::.tbl_now_holiday_points(nowobj, ep)
  expect_equal(nrow(hp), 0)
})

test_that("holiday points are detected from a temporal_effects spec", {
  skip_on_cran()
  skip_if_not_installed("almanac")

  nowobj <- make_daily_now() |>
    add_temporal_effects(temporal_effects(holidays = almanac::cal_us_federal()))
  ep <- tbl.now:::.tbl_now_epidemic_process(nowobj)
  hp <- tbl.now:::.tbl_now_holiday_points(nowobj, ep)
  expect_gt(nrow(hp), 0)
  expect_true(all(hp$event_date %in% ep$event_date))
})

test_that("autoplot renders with holidays marked", {
  skip_on_cran()
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
  skip_on_cran()
  g <- tbl.now:::.tbl_now_cumulative_growth(make_cumulative_now())
  expect_true(all(c("event_date", "delay", "ratio") %in% names(g)))
  expect_true(all(is.finite(g$ratio) & g$ratio > 0))
  # event 1 delay 1: 30/10 = 3 ; delay 3: 21/30 = 0.7
  ev1 <- g[g$event_date == as.Date("2021-01-04"), ]
  expect_equal(ev1$ratio[ev1$delay == 1], 3)
  expect_equal(ev1$ratio[ev1$delay == 3], 0.7)
})

test_that("count-cumulative delay panel is the growth panel (log scale)", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  p <- ggplot2::autoplot(make_cumulative_now(), panels = "delay_distribution")
  expect_equal(p$labels$title, "Cumulative growth by delay")
  expect_equal(p$labels$y, "Ratio to the previous delay (1 = stable, log scale)")
  # a log-10 y scale is applied
  is_log <- any(vapply(p$scales$scales, function(s) {
    !is.null(s$trans) && identical(s$trans$name, "log-10")
  }, logical(1)))
  expect_true(is_log)
  expect_s3_class(ggplot2::ggplot_build(p), "ggplot_built")
})

test_that("non-cumulative data keeps the histogram delay panel", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  p <- ggplot2::autoplot(make_daily_now(), panels = "delay_distribution")
  expect_equal(p$labels$title, "Empirical delay distribution")
})

test_that("cumulative growth panel works by stratum and full autoplot builds", {
  skip_on_cran()
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

# ---------------------------------------------------------------------------
# Holiday panels (calendar_holiday / calendar_holiday_lag + delay twins)
# ---------------------------------------------------------------------------

# A daily tbl_now spanning Christmas and New Year, so the US federal calendar
# has holidays to find.
make_holiday_now <- function() {
  set.seed(7)
  dates <- seq(as.Date("2020-12-01"), as.Date("2021-01-31"), by = "day")
  rows <- lapply(dates, function(day) {
    max_delay <- rpois(1, 3)
    data.frame(
      event_date  = day,
      report_date = day + 0:max_delay,
      n           = rpois(max_delay + 1, 4)
    )
  })
  tbl_now(do.call(rbind, rows),
    event_date = event_date, report_date = report_date, case_count = n,
    data_type = "count-incidence", event_units = "days", report_units = "days",
    verbose = FALSE
  )
}

holiday_panel_keys <- c(
  "calendar_holiday", "calendar_holiday_lag", "delay_holiday", "delay_holiday_lag"
)

test_that("holiday panels appear only when a holiday or weekend effect is attached", {
  skip_on_cran()
  skip_if_not_installed("almanac")

  # No temporal effect at all -> no holiday panels, and asking for one warns
  bare <- make_holiday_now()
  expect_null(.tbl_now_holiday_config(bare))
  expect_false(any(holiday_panel_keys %in% .tbl_now_all_panel_keys("days", NULL)))
  expect_warning(
    ggplot2::autoplot(bare, panels = c("epidemic", "calendar_holiday")),
    "holiday or weekend temporal effect"
  )

  # A weekend effect alone is enough for the day-type panels, but the lag panels
  # need a calendar and a non-zero depth.
  weekend_only <- add_temporal_effects(bare, temporal_effects(weekend = TRUE))
  keys <- .tbl_now_all_panel_keys("days", .tbl_now_holiday_config(weekend_only))
  expect_true(all(c("calendar_holiday", "delay_holiday") %in% keys))
  expect_false(any(c("calendar_holiday_lag", "delay_holiday_lag") %in% keys))

  # Holidays with a lag depth unlock all four
  full <- add_temporal_effects(
    bare,
    temporal_effects(weekend = TRUE, holidays = almanac::cal_us_federal(),
                     holiday_lags = 2)
  )
  expect_true(all(holiday_panel_keys %in%
                    .tbl_now_all_panel_keys("days", .tbl_now_holiday_config(full))))
})

test_that("the holiday config folds every attached spec together", {
  skip_on_cran()
  skip_if_not_installed("almanac")

  # The documented both-sides idiom: one spec per direction.
  calendar <- almanac::cal_us_federal()
  object <- make_holiday_now() |>
    add_temporal_effects(temporal_effects(weekend = TRUE, holidays = calendar,
                                          holiday_lags = -2)) |>
    add_temporal_effects(temporal_effects(holidays = calendar, holiday_lags = 3))

  config <- .tbl_now_holiday_config(object)
  expect_true(config$weekend)
  expect_length(config$calendars, 2)
  expect_identical(config$lag_depths, c(-2L, 3L))

  # Both directions reach the axis, ordered before -> holiday -> after -> other
  expect_identical(
    .tbl_now_holiday_lag_levels(config$lag_depths),
    c("2 before", "1 before", "Holiday", "1 after", "2 after", "3 after", "Other")
  )
})

test_that("day-type categories follow what the spec asked for", {
  skip_on_cran()
  skip_if_not_installed("almanac")

  calendar <- almanac::rcalendar(almanac::hol_christmas())
  # Christmas 2020 is a Friday; Dec 26 a Saturday; Dec 28 a Monday.
  dates <- as.Date(c("2020-12-25", "2020-12-26", "2020-12-28"))

  both <- list(calendars = list(calendar), weekend = TRUE,
               weekend_days = c("Sat", "Sun"), lag_depths = integer(0))
  expect_identical(
    as.character(.tbl_now_holiday_group(dates, both)),
    c("Holiday", "Weekend", "Weekday")
  )
  expect_identical(.tbl_now_holiday_levels(both), c("Weekday", "Weekend", "Holiday"))

  # A calendar with no weekend effect contrasts holidays with everything else
  holiday_only <- list(calendars = list(calendar), weekend = FALSE,
                       weekend_days = c("Sat", "Sun"), lag_depths = integer(0))
  expect_identical(
    as.character(.tbl_now_holiday_group(dates, holiday_only)),
    c("Holiday", "Non-holiday", "Non-holiday")
  )

  # A weekend effect with no calendar knows nothing about Christmas
  weekend_only <- list(calendars = list(), weekend = TRUE,
                       weekend_days = c("Sat", "Sun"), lag_depths = integer(0))
  expect_identical(
    as.character(.tbl_now_holiday_group(dates, weekend_only)),
    c("Weekday", "Weekend", "Weekday")
  )
  expect_identical(.tbl_now_holiday_levels(weekend_only), c("Weekday", "Weekend"))
})

test_that("holiday lag positions skip weekends and match the lag columns", {
  skip_on_cran()
  skip_if_not_installed("almanac")

  config <- list(
    calendars = list(almanac::rcalendar(almanac::hol_christmas())),
    weekend = TRUE, weekend_days = c("Sat", "Sun"), lag_depths = c(-2L, 2L)
  )
  dates <- seq(as.Date("2020-12-18"), as.Date("2021-01-05"), by = "day")
  group <- .tbl_now_holiday_lag_group(dates, config)
  named <- stats::setNames(as.character(group), format(dates))

  # Christmas is Fri Dec 25 2020. The weekend Dec 26/27 is skipped, so the
  # working days around it are Wed 23 / Thu 24 before and Mon 28 / Tue 29 after.
  expect_identical(named[["2020-12-25"]], "Holiday")
  expect_identical(named[["2020-12-24"]], "1 before")
  expect_identical(named[["2020-12-23"]], "2 before")
  expect_identical(named[["2020-12-28"]], "1 after")
  expect_identical(named[["2020-12-29"]], "2 after")

  # Weekend days are not working days, so they carry no lag position
  expect_identical(named[["2020-12-26"]], "Other")
  expect_identical(named[["2020-12-27"]], "Other")
  # Days beyond the requested depth fall back to the reference category
  expect_identical(named[["2020-12-30"]], "Other")

  # The panel must flag exactly the dates the effect columns flag
  columns <- make_holiday_now() |>
    add_temporal_effects(
      temporal_effects(holidays = config$calendars[[1]], holiday_lags = 2)
    ) |>
    compute_temporal_effects() |>
    as.data.frame()
  flagged_by_column <- sort(unique(columns$event_date[columns$.event_holiday_lag_1 == 1]))
  panel_config <- utils::modifyList(config, list(lag_depths = 2L))
  panel_dates <- sort(unique(columns$event_date))
  flagged_by_panel <- panel_dates[
    .tbl_now_holiday_lag_group(panel_dates, panel_config) == "1 after"
  ]
  expect_identical(flagged_by_panel, flagged_by_column)
})

test_that("all four holiday panels build, alone and by strata", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  skip_if_not_installed("almanac")

  object <- make_holiday_now() |>
    add_temporal_effects(
      temporal_effects(weekend = TRUE, holidays = almanac::cal_us_federal(),
                       holiday_lags = 2)
    )

  for (key in holiday_panel_keys) {
    panel <- ggplot2::autoplot(object, panels = key)
    expect_s3_class(panel, "ggplot")
    # The boxes are the day-type / lag categories, not raw dates
    expect_true(is.factor(ggplot2::ggplot_build(panel)$plot$data$calendar_group))
  }

  # They join the "all" gallery and the calendar aliases
  expect_true(all(holiday_panel_keys %in%
                    .tbl_now_resolve_panels("all", "days", .tbl_now_holiday_config(object))))
  expect_true("calendar_holiday" %in%
                .tbl_now_resolve_panels("calendar", "days", .tbl_now_holiday_config(object)))
  expect_true("delay_holiday" %in%
                .tbl_now_resolve_panels("delay_calendar", "days", .tbl_now_holiday_config(object)))

  # And they split by stratum like every other panel
  rows <- as.data.frame(make_holiday_now())[, c("event_date", "report_date", "n")]
  stratified <- rbind(
    transform(rows, grp = "a"),
    transform(rows, grp = "b")
  ) |>
    tbl_now(event_date = event_date, report_date = report_date, case_count = n,
            strata = "grp", data_type = "count-incidence",
            event_units = "days", report_units = "days", verbose = FALSE) |>
    add_temporal_effects(
      temporal_effects(weekend = TRUE, holidays = almanac::cal_us_federal(),
                       holiday_lags = 2)
    )
  expect_s3_class(
    ggplot2::autoplot(stratified, panels = "delay_holiday_lag", by_strata = TRUE),
    "ggplot"
  )
})


# --- process colours, subtitles and the percent measure ---------------------

test_that("every panel names the process it describes in its subtitle", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  object <- make_daily_now()

  epidemic_keys  <- c("epidemic", "calendar_weekday", "calendar_week", "seasonality")
  reporting_keys <- c("delay_distribution", "delay_weekday", "delay_week",
                      "delay_seasonality")

  for (key in epidemic_keys) {
    expect_equal(
      ggplot2::autoplot(object, panels = key)$labels$subtitle,
      "Epidemic (event-date) process",
      info = key
    )
  }
  for (key in reporting_keys) {
    expect_equal(
      ggplot2::autoplot(object, panels = key)$labels$subtitle,
      "Reporting delay process",
      info = key
    )
  }
})

test_that("both periodogram panels are titled 'Cycles (periodogram)'", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  object <- make_daily_now()

  expect_equal(
    ggplot2::autoplot(object, panels = "seasonality")$labels$title,
    "Cycles (periodogram)"
  )
  expect_equal(
    ggplot2::autoplot(object, panels = "delay_seasonality")$labels$title,
    "Cycles (periodogram)"
  )
})

test_that("percent shares of a calendar block add up to 100", {
  skip_on_cran()
  object <- make_daily_now()
  epidemic_process <- tbl.now:::.tbl_now_epidemic_process(object)

  shares <- tbl.now:::.tbl_now_percent_shares(
    epidemic_process, "weekday", "event_date", "case_count"
  )
  # One block is a week, so the seven weekdays share out 100% of its cases.
  totals <- tapply(shares$percent, shares$block, sum)
  expect_true(all(abs(totals - 100) < 1e-8))
  expect_setequal(
    as.character(unique(shares$calendar_group)),
    c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  )
})

test_that("measure = 'percent' switches the panels to percentage shares", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  object <- make_daily_now()

  cases <- ggplot2::autoplot(object, panels = "calendar_weekday", measure = "percent")
  expect_equal(cases$labels$y, "Percent of cases (%)")

  # The reporting twin moves to the report date and shares out the reports.
  reports <- ggplot2::autoplot(object, panels = "delay_weekday", measure = "percent")
  expect_equal(reports$labels$y, "Percent of reported cases (%)")
  expect_equal(reports$labels$title, "Day-of-week reporting effect")
})

test_that("measure = 'normalized' gives the mean-relative view", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  object <- make_daily_now()

  expect_equal(
    ggplot2::autoplot(
      object, panels = "calendar_weekday", measure = "normalized"
    )$labels$y,
    "Normalized effect (1 = average)"
  )
  # The delay twin normalizes the mean delay rather than the case count.
  expect_equal(
    ggplot2::autoplot(
      object, panels = "delay_weekday", measure = "normalized"
    )$labels$y,
    "Normalized mean delay (1 = average)"
  )
})

test_that("the default calendar measure is 'percent'", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  object <- make_daily_now()

  # `autoplot()` and the `plot_*` twins all default to `measure = "percent"`.
  # Guard the default explicitly: it used to be "normalized", and this test
  # file kept asserting the old value long after the switch.
  expect_equal(
    ggplot2::autoplot(object, panels = "calendar_weekday")$labels$y,
    "Percent of cases (%)"
  )
  expect_equal(
    plot_day_of_week_effects(object)$labels$y,
    "Percent of cases (%)"
  )
  expect_equal(
    eval(formals(autoplot.tbl_now)$measure)[1],
    "percent"
  )
})

test_that("the plot_* twins draw the same panel as autoplot()", {
  skip_if_not_installed("ggplot2")
  object <- make_daily_now()

  expect_equal(
    plot_day_of_week_effects(object)$labels,
    ggplot2::autoplot(object, panels = "calendar_weekday")$labels
  )
  expect_equal(
    plot_day_of_week_effects(object, type = "report", measure = "percent")$labels,
    ggplot2::autoplot(object, panels = "delay_weekday", measure = "percent")$labels
  )
  expect_equal(
    plot_cycles(object, type = "report")$labels,
    ggplot2::autoplot(object, panels = "delay_seasonality")$labels
  )
  expect_s3_class(plot_delay_distribution(object), "ggplot")
  expect_s3_class(plot_observed_cases(object), "ggplot")
  expect_s3_class(plot_week_of_year_effects(object), "ggplot")
})

test_that("percent needs date columns", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  numeric_data <- data.frame(
    event_date = rep(1:20, each = 3),
    report_date = rep(1:20, each = 3) + 0:2,
    n = 5
  )
  object <- tbl_now(numeric_data,
    event_date = event_date, report_date = report_date, case_count = n,
    data_type = "count-incidence", verbose = FALSE
  )
  # Numeric time has no weeks to share out, so the panel says so rather than
  # inventing a denominator.
  expect_null(
    tbl.now:::.tbl_now_percent_shares(
      data.frame(event_date = 1:10, case_count = 1),
      "weekday", "event_date", "case_count"
    )
  )
})
