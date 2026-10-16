# Calendar effects are read off the calendar, not off the data's first date
# (#99). Before 1.2.0 `week_of_year` and `month_of_year` were offsets from the
# first ROW of the data and `seasons` counted time from the earliest event date,
# so the same calendar week got a different code in every series, stratum and
# backtest window.

weekly_cumulative <- function(start, end = "2024-02-03") {
  d <- dplyr::tibble(
    event = seq(as.Date(start), as.Date(end), by = "week"),
    report = as.Date(end),
    n = 100L
  )
  tbl_now(d,
    event_date = event, report_date = report, case_count = n,
    units = "weeks", data_type = "count-cumulative", verbose = FALSE
  )
}

calendar_codes <- function(x, dates) {
  x |>
    add_temporal_effects(
      temporal_effects(week_of_year = TRUE, month_of_year = TRUE, seasons = 52)
    ) |>
    compute_temporal_effects() |>
    tibble::as_tibble() |>
    dplyr::distinct(
      event, .event_week_of_year, .event_month_of_year,
      .event_season_52_cos, .event_season_52_sin
    ) |>
    dplyr::filter(event %in% as.Date(dates)) |>
    dplyr::arrange(event)
}

test_that("the same date gets the same codes whatever the series starts on (#99)", {
  skip_on_cran()
  dates <- c("2024-01-06", "2024-01-13")
  from_january <- calendar_codes(weekly_cumulative("2023-01-07"), dates)
  from_november <- calendar_codes(weekly_cumulative("2023-11-25"), dates)
  from_2010 <- calendar_codes(weekly_cumulative("2010-10-09"), dates)

  expect_equal(from_november, from_january)
  expect_equal(from_2010, from_january)

  # And the codes are the calendar's: epiweeks 1 and 2 of 2024, in January.
  expect_equal(as.integer(as.character(from_january$.event_week_of_year)), c(1L, 2L))
  expect_equal(as.integer(as.character(from_january$.event_month_of_year)), c(1L, 1L))
  expect_equal(
    from_january$.event_season_52_cos,
    cos(2 * pi * (as.numeric(as.Date(dates)) / 7) / 52)
  )
})

test_that("a rolling window gives the shared dates the same codes", {
  skip_on_cran()
  # A backtest that moves `min_date` forward refits on a filtered object; the
  # dates both windows contain must be coded alike.
  x <- weekly_cumulative("2022-06-04")
  window <- dplyr::filter(x, event >= as.Date("2023-03-04"))
  dates <- seq(as.Date("2023-03-04"), as.Date("2024-02-03"), by = "week")

  expect_equal(calendar_codes(window, dates), calendar_codes(x, dates))
})

test_that("calendar codes do not depend on row order", {
  skip_on_cran()
  # The old reference date was the FIRST ROW, not even the earliest date.
  df <- data.frame(date = seq(as.Date("2021-10-02"), by = "week", length.out = 30))
  shuffled <- df[c(17, 3, 30, 1, 22, 9, seq_len(30)[-c(17, 3, 30, 1, 22, 9)]), , drop = FALSE]
  t_eff <- temporal_effects(week_of_year = TRUE, month_of_year = TRUE, seasons = 52)

  in_order <- add_temporal_effects.data.frame(df,
    t_effects = t_eff, date_col = "date", units = "weeks"
  )
  out_of_order <- add_temporal_effects.data.frame(shuffled,
    t_effects = t_eff, date_col = "date", units = "weeks"
  )
  out_of_order <- out_of_order[order(out_of_order$date), , drop = FALSE]
  rownames(out_of_order) <- NULL

  expect_equal(out_of_order, in_order)
})

test_that("seasons are counted from 1970-01-01 in the data's units", {
  skip_on_cran()
  days <- as.Date("2023-05-01") + 0:20
  daily <- add_temporal_effects.data.frame(data.frame(date = days),
    t_effects = temporal_effects(seasons = 7), date_col = "date"
  )
  expect_equal(daily$.date_season_7_cos, cos(2 * pi * as.numeric(days) / 7))
  expect_equal(daily$.date_season_7_sin, sin(2 * pi * as.numeric(days) / 7))
  # A 7-day wave counted from a fixed origin lines up with the weekday.
  same_weekday <- lubridate::wday(days) == lubridate::wday(days[1])
  expect_equal(
    daily$.date_season_7_cos[same_weekday],
    rep(daily$.date_season_7_cos[1], sum(same_weekday))
  )

  months <- seq(as.Date("2019-11-01"), by = "month", length.out = 15)
  monthly <- add_temporal_effects.data.frame(data.frame(date = months),
    t_effects = temporal_effects(seasons = 12), date_col = "date", units = "months"
  )
  # Whole months since January 1970, so every January has phase zero.
  elapsed <- 12 * (lubridate::year(months) - 1970) + lubridate::month(months) - 1
  expect_equal(monthly$.date_season_12_cos, cos(2 * pi * elapsed / 12))
  expect_equal(
    monthly$.date_season_12_cos[lubridate::month(months) == 1],
    c(1, 1)
  )

  # `season_units` counts the periods in another unit than `units`.
  in_weeks <- add_temporal_effects.data.frame(data.frame(date = days),
    t_effects = temporal_effects(seasons = 52), date_col = "date",
    units = "days", season_units = "weeks"
  )
  expect_equal(
    in_weeks$.date_season_52_cos,
    cos(2 * pi * (as.numeric(days) / 7) / 52)
  )
})

test_that("an explicit numeric_col still overrides the calendar", {
  skip_on_cran()
  df <- data.frame(date = as.Date(c("2020-01-01", "2020-07-01")), t = c(1, 183))
  out <- add_temporal_effects.data.frame(df,
    t_effects = temporal_effects(seasons = 365),
    date_col = "date", numeric_col = "t"
  )
  expect_equal(out$.date_season_365_cos, cos(2 * pi * c(1, 183) / 365))
})

test_that("seasons on a non-date column still need a numeric_col", {
  skip_on_cran()
  df <- data.frame(day = 1:5)
  expect_error(
    add_temporal_effects.data.frame(df,
      t_effects = temporal_effects(seasons = 7), date_col = "day"
    ),
    "numeric_col"
  )
})

test_that("a data column called `period` does not mask the Fourier period", {
  skip_on_cran()
  days <- as.Date("2023-05-01") + 0:6
  out <- add_temporal_effects.data.frame(
    data.frame(date = days, period = 1000),
    t_effects = temporal_effects(seasons = 7), date_col = "date"
  )
  expect_equal(out$.date_season_7_cos, cos(2 * pi * as.numeric(days) / 7))
})

test_that("a numeric axis keeps counting from its first event", {
  skip_on_cran()
  # No calendar to anchor to, so the relative `.event_num` is still the axis.
  x <- tbl_now(data.frame(event = 5:14, report = 7:16, n = 1L),
    event_date = event, report_date = report, case_count = n,
    data_type = "count-incidence", units = "numeric", verbose = FALSE
  ) |>
    add_temporal_effects(temporal_effects(seasons = 7)) |>
    compute_temporal_effects()

  expect_equal(x$.event_season_7_cos, cos(2 * pi * as.numeric(x$.event_num) / 7))
})

test_that("report- and revision-date effects use those dates' calendar", {
  skip_on_cran()
  x <- tbl_now(
    data.frame(
      event = as.Date(c("2020-12-20", "2020-12-28", "2021-01-05")),
      report = as.Date(c("2020-12-29", "2021-01-04", "2021-01-12")),
      revised = as.Date(c("2021-01-02", "2021-02-01", NA)),
      outcome = c("confirmed", "retracted", "pending")
    ),
    event_date = event, report_date = report,
    revision_date = revised, revision_type = outcome,
    units = "days", verbose = FALSE
  ) |>
    add_temporal_effects(
      temporal_effects(week_of_year = TRUE, month_of_year = TRUE, seasons = 7),
      date_type = "report_date"
    ) |>
    add_temporal_effects(
      temporal_effects(week_of_year = TRUE, month_of_year = TRUE),
      date_type = "revision_date"
    ) |>
    compute_temporal_effects()

  expect_equal(
    as.integer(as.character(x$.report_week_of_year)),
    as.integer(lubridate::epiweek(x$report))
  )
  expect_equal(as.integer(as.character(x$.report_month_of_year)), c(12L, 1L, 1L))
  expect_equal(x$.report_season_7_cos, cos(2 * pi * as.numeric(x$report) / 7))
  # 2021-01-02 is the last day of MMWR 2020's week 53.
  expect_equal(as.integer(as.character(x$.revision_week_of_year)), c(53L, 5L, NA))
  expect_equal(as.integer(as.character(x$.revision_month_of_year)), c(1L, 2L, NA))
})

test_that("grouped and stratified objects get the ungrouped codes", {
  skip_on_cran()
  # Strata that start on different dates: each stratum's own first row used to
  # be the reference on a grouped object (and recycled with a warning).
  d <- dplyr::bind_rows(
    dplyr::tibble(
      event = seq(as.Date("2023-01-07"), as.Date("2024-02-03"), by = "week"),
      region = "a"
    ),
    dplyr::tibble(
      event = seq(as.Date("2023-11-25"), as.Date("2024-02-03"), by = "week"),
      region = "b"
    )
  ) |>
    dplyr::mutate(report = as.Date("2024-02-03"), n = 10L)
  x <- tbl_now(d,
    event_date = event, report_date = report, case_count = n, strata = region,
    units = "weeks", data_type = "count-cumulative", verbose = FALSE
  ) |>
    add_temporal_effects(
      temporal_effects(week_of_year = TRUE, month_of_year = TRUE, seasons = 52)
    )

  ungrouped <- compute_temporal_effects(x)
  grouped <- expect_no_warning(compute_temporal_effects(dplyr::group_by(x, region)))

  expect_true(dplyr::is_grouped_df(grouped))
  expect_equal(dplyr::group_vars(grouped), "region")
  effect_cols <- get_temporal_effect_cols(ungrouped)
  expect_equal(
    tibble::as_tibble(dplyr::ungroup(grouped))[effect_cols],
    tibble::as_tibble(ungrouped)[effect_cols]
  )

  # Both strata code the dates they share identically.
  shared <- tibble::as_tibble(ungrouped) |>
    dplyr::filter(event >= as.Date("2023-11-25")) |>
    dplyr::select(region, event, dplyr::all_of(effect_cols))
  expect_equal(
    dplyr::select(dplyr::filter(shared, region == "a"), -region),
    dplyr::select(dplyr::filter(shared, region == "b"), -region)
  )
})

test_that("aggregating days to weeks keeps the seasonal phase of each date", {
  skip_on_cran()
  d <- data.frame(
    event = as.Date("2023-01-01") + 0:69,
    report = as.Date("2023-01-01") + 0:69 + 2,
    n = 1L
  )
  daily <- tbl_now(d,
    event_date = event, report_date = report, case_count = n,
    data_type = "count-incidence", units = "days", verbose = FALSE
  ) |>
    add_temporal_effects(temporal_effects(seasons = 364))

  weekly <- suppressMessages(aggregate_time_units(daily, to = "weeks")) |>
    compute_temporal_effects()

  # 364 days became 52 weeks; on the calendar origin both describe the same wave.
  expect_equal(
    weekly$.event_season_52_cos,
    cos(2 * pi * as.numeric(weekly$event) / 364)
  )
})
