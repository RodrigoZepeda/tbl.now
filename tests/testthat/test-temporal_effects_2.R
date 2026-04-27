# Test file for temporal_effects.R

# ============================================================================
# Tests for temporal_effects() constructor
# ============================================================================

test_that("temporal_effects creates object with default values", {
  t_eff <- temporal_effects()

  expect_s3_class(t_eff, "tbl.now::temporal_effects")
  expect_true(S7::S7_inherits(t_eff, temporal_effects))

  # Check default values are all FALSE
  expect_false(t_eff@day_of_week)
  expect_false(t_eff@weekend)
  expect_false(t_eff@day_of_month)
  expect_false(t_eff@month_of_year)
  expect_false(t_eff@week_of_year)

  # Check default seasons is empty
  expect_equal(length(t_eff@seasons), 0)
  expect_true(is.numeric(t_eff@seasons))

  # Check default holidays is NULL
  expect_null(t_eff@holidays)
})

test_that("temporal_effects creates object with single effect", {
  t_eff <- temporal_effects(day_of_week = TRUE)

  expect_true(t_eff@day_of_week)
  expect_false(t_eff@weekend)
  expect_false(t_eff@day_of_month)
  expect_false(t_eff@month_of_year)
  expect_false(t_eff@week_of_year)
})

test_that("temporal_effects creates object with multiple effects", {
  t_eff <- temporal_effects(
    day_of_week = TRUE,
    weekend = TRUE,
    week_of_year = TRUE
  )

  expect_true(t_eff@day_of_week)
  expect_true(t_eff@weekend)
  expect_true(t_eff@week_of_year)
  expect_false(t_eff@day_of_month)
  expect_false(t_eff@month_of_year)
})

test_that("temporal_effects creates object with all effects", {
  t_eff <- temporal_effects(
    day_of_week = TRUE,
    weekend = TRUE,
    day_of_month = TRUE,
    month_of_year = TRUE,
    week_of_year = TRUE
  )

  expect_true(t_eff@day_of_week)
  expect_true(t_eff@weekend)
  expect_true(t_eff@day_of_month)
  expect_true(t_eff@month_of_year)
  expect_true(t_eff@week_of_year)
})

test_that("temporal_effects handles seasons parameter", {
  t_eff <- temporal_effects(seasons = 52)

  expect_equal(t_eff@seasons, 52)
  expect_true(is.numeric(t_eff@seasons))
})

test_that("temporal_effects handles multiple seasons", {
  t_eff <- temporal_effects(seasons = c(7, 52, 365))

  expect_equal(length(t_eff@seasons), 3)
  expect_true(7 %in% t_eff@seasons)
  expect_true(52 %in% t_eff@seasons)
  expect_true(365 %in% t_eff@seasons)
})

test_that("temporal_effects removes duplicate seasons", {
  t_eff <- temporal_effects(seasons = c(52, 52, 365, 365, 7))

  expect_equal(length(t_eff@seasons), 3)
  expect_equal(sort(t_eff@seasons), c(7, 52, 365))
})

test_that("temporal_effects handles empty seasons vector", {
  t_eff <- temporal_effects(seasons = integer(0))

  expect_equal(length(t_eff@seasons), 0)
  expect_true(is.numeric(t_eff@seasons))
})

test_that("temporal_effects handles numeric seasons (coerces to correct type)", {
  t_eff <- temporal_effects(seasons = c(7.0, 52.0, 365.0))

  expect_true(is.numeric(t_eff@seasons))
  expect_equal(length(t_eff@seasons), 3)
})

test_that("temporal_effects handles holidays parameter", {
  skip_if_not_installed("almanac")

  cal <- almanac::rcalendar(almanac::hol_christmas())
  t_eff <- temporal_effects(holidays = cal)

  expect_false(is.null(t_eff@holidays))
  expect_s3_class(t_eff@holidays, "almanac_rcalendar")
})

test_that("temporal_effects handles US federal holidays", {
  skip_if_not_installed("almanac")

  t_eff <- temporal_effects(holidays = almanac::cal_us_federal())

  expect_false(is.null(t_eff@holidays))
  expect_s3_class(t_eff@holidays, "almanac_rcalendar")
})

test_that("temporal_effects handles multiple holidays", {
  skip_if_not_installed("almanac")

  cal <- almanac::rcalendar(
    almanac::hol_christmas(),
    almanac::hol_new_years_day()
  )
  t_eff <- temporal_effects(holidays = cal)

  expect_false(is.null(t_eff@holidays))
  expect_s3_class(t_eff@holidays, "almanac_rcalendar")
})

test_that("temporal_effects accepts NULL for holidays", {
  t_eff <- temporal_effects(holidays = NULL)

  expect_null(t_eff@holidays)
})

# ============================================================================
# Tests for temporal_effects() validation
# ============================================================================

test_that("temporal_effects fails with non-logical day_of_week", {
  expect_error(
    temporal_effects(day_of_week = "TRUE"),
    "must be either.*TRUE.*FALSE"
  )

  expect_error(
    temporal_effects(day_of_week = 1),
    "must be either.*TRUE.*FALSE"
  )
})

test_that("temporal_effects fails with non-logical weekend", {
  expect_error(
    temporal_effects(weekend = "yes"),
    "must be either.*TRUE.*FALSE"
  )
})

test_that("temporal_effects fails with non-logical day_of_month", {
  expect_error(
    temporal_effects(day_of_month = 1),
    "must be either.*TRUE.*FALSE"
  )
})

test_that("temporal_effects fails with non-logical month_of_year", {
  expect_error(
    temporal_effects(month_of_year = "TRUE"),
    "must be either.*TRUE.*FALSE"
  )
})

test_that("temporal_effects fails with non-logical week_of_year", {
  expect_error(
    temporal_effects(week_of_year = 1),
    "must be either.*TRUE.*FALSE"
  )
})

test_that("temporal_effects fails with invalid holidays object", {
  expect_error(
    temporal_effects(holidays = "not_a_calendar"),
    "Invalid.*holidays.*Must be.*almanac::rcalendar"
  )

  expect_error(
    temporal_effects(holidays = list(hol_christmas = "2024-12-25")),
    "Invalid.*holidays.*Must be.*almanac::rcalendar"
  )
})

test_that("temporal_effects fails with multiple logical values", {
  expect_error(
    temporal_effects(day_of_week = c(TRUE, FALSE)),
    "must be either.*TRUE.*FALSE"
  )
})

# ============================================================================
# Tests for temporal_effects() edge cases
# ============================================================================

test_that("temporal_effects handles all FALSE values", {
  t_eff <- temporal_effects(
    day_of_week = FALSE,
    weekend = FALSE,
    day_of_month = FALSE,
    month_of_year = FALSE,
    week_of_year = FALSE,
    seasons = integer(0),
    holidays = NULL
  )

  expect_s3_class(t_eff, "tbl.now::temporal_effects")
  expect_false(t_eff@day_of_week)
  expect_false(t_eff@weekend)
})

test_that("temporal_effects handles all TRUE values", {
  t_eff <- temporal_effects(
    day_of_week = TRUE,
    weekend = TRUE,
    day_of_month = TRUE,
    month_of_year = TRUE,
    week_of_year = TRUE
  )

  expect_s3_class(t_eff, "tbl.now::temporal_effects")
  expect_true(t_eff@day_of_week)
  expect_true(t_eff@weekend)
  expect_true(t_eff@day_of_month)
  expect_true(t_eff@month_of_year)
  expect_true(t_eff@week_of_year)
})

test_that("temporal_effects handles large season values", {
  t_eff <- temporal_effects(seasons = c(365, 730, 1095))

  expect_equal(length(t_eff@seasons), 3)
  expect_true(all(t_eff@seasons %in% c(365, 730, 1095)))
})

# ============================================================================
# Tests for print.temporal_effects()
# ============================================================================

test_that("print.temporal_effects displays header", {
  t_eff <- temporal_effects(day_of_week = TRUE)

  output <- cli::cli_fmt(print(t_eff))

  expect_true(any(grepl("Temporal Effects", output)))
})

test_that("print.temporal_effects shows single effect", {
  t_eff <- temporal_effects(day_of_week = TRUE)

  output <- cli::cli_fmt(print(t_eff))

  expect_true(any(grepl("day_of_week", output)))
  expect_false(any(grepl("weekend", output)))
  expect_false(any(grepl("month_of_year", output)))
})

test_that("print.temporal_effects shows multiple effects", {
  t_eff <- temporal_effects(
    day_of_week = TRUE,
    weekend = TRUE,
    week_of_year = TRUE
  )

  output <- cli::cli_fmt(print(t_eff))

  expect_true(any(grepl("day_of_week", output)))
  expect_true(any(grepl("weekend", output)))
  expect_true(any(grepl("week_of_year", output)))
  expect_false(any(grepl("day_of_month", output)))
})

test_that("print.temporal_effects shows seasons", {
  t_eff <- temporal_effects(seasons = c(7, 52, 365))

  output <- cli::cli_fmt(print(t_eff))

  expect_true(any(grepl("season", output)))
  expect_true(any(grepl("7", output)))
  expect_true(any(grepl("52", output)))
  expect_true(any(grepl("365", output)))
})

test_that("print.temporal_effects shows holidays", {
  skip_if_not_installed("almanac")

  cal <- almanac::rcalendar(almanac::hol_christmas())
  t_eff <- temporal_effects(holidays = cal)

  output <- cli::cli_fmt(print(t_eff))

  expect_true(any(grepl("holiday", output, ignore.case = TRUE)))
})

test_that("print.temporal_effects shows US federal holidays", {
  skip_if_not_installed("almanac")

  t_eff <- temporal_effects(holidays = almanac::cal_us_federal())

  output <- cli::cli_fmt(print(t_eff))

  expect_true(any(grepl("holiday", output, ignore.case = TRUE)))
})

test_that("print.temporal_effects shows no effects message", {
  t_eff <- temporal_effects()

  output <- cli::cli_fmt(print(t_eff))

  expect_true(any(grepl("No temporal effects", output)))
})

test_that("print.temporal_effects shows all effects", {
  skip_if_not_installed("almanac")

  t_eff <- temporal_effects(
    day_of_week = TRUE,
    weekend = TRUE,
    day_of_month = TRUE,
    month_of_year = TRUE,
    week_of_year = TRUE,
    seasons = c(52, 365),
    holidays = almanac::cal_us_federal()
  )

  output <- cli::cli_fmt(print(t_eff))

  expect_true(any(grepl("day_of_week", output)))
  expect_true(any(grepl("weekend", output)))
  expect_true(any(grepl("day_of_month", output)))
  expect_true(any(grepl("month_of_year", output)))
  expect_true(any(grepl("week_of_year", output)))
  expect_true(any(grepl("season", output)))
  expect_true(any(grepl("holiday", output, ignore.case = TRUE)))
})

# ============================================================================
# S7 properties
# ============================================================================

test_that("temporal_effects has correct S7 properties", {
  t_eff <- temporal_effects()

  props <- S7::props(t_eff)

  expect_true("day_of_week" %in% names(props))
  expect_true("weekend" %in% names(props))
  expect_true("day_of_month" %in% names(props))
  expect_true("month_of_year" %in% names(props))
  expect_true("week_of_year" %in% names(props))
  expect_true("seasons" %in% names(props))
  expect_true("holidays" %in% names(props))
})

test_that("temporal_effects properties have correct types", {
  t_eff <- temporal_effects(day_of_week = TRUE, seasons = 52)

  expect_true(is.logical(t_eff@day_of_week))
  expect_true(is.logical(t_eff@weekend))
  expect_true(is.numeric(t_eff@seasons))
})

test_that("temporal_effects properties are accessible via @ operator", {
  t_eff <- temporal_effects(day_of_week = TRUE, weekend = FALSE, seasons = c(52, 365))

  expect_equal(t_eff@day_of_week, TRUE)
  expect_equal(t_eff@weekend, FALSE)
  expect_equal(t_eff@seasons, c(52, 365))
})

# ============================================================================
# Documentation examples
# ============================================================================

test_that("documentation example 1 works", {
  t_eff <- temporal_effects(day_of_week = TRUE, week_of_year = TRUE)

  expect_s3_class(t_eff, "tbl.now::temporal_effects")
  expect_true(t_eff@day_of_week)
  expect_true(t_eff@week_of_year)
})

test_that("documentation example 2 works", {
  skip_if_not_installed("almanac")

  cal <- almanac::rcalendar(almanac::hol_christmas())
  t_eff <- temporal_effects(holidays = cal, day_of_month = TRUE, seasons = c(7, 365))

  expect_s3_class(t_eff, "tbl.now::temporal_effects")
  expect_true(t_eff@day_of_month)
  expect_equal(length(t_eff@seasons), 2)
  expect_false(is.null(t_eff@holidays))
})

# ============================================================================
# Stress tests
# ============================================================================

test_that("temporal_effects handles very large number of seasons", {
  many_seasons <- 1:100
  t_eff <- temporal_effects(seasons = many_seasons)

  expect_equal(length(t_eff@seasons), 100)
  expect_true(all(1:100 %in% t_eff@seasons))
})

test_that("temporal_effects handles fractional seasons (coerces to numeric)", {
  t_eff <- temporal_effects(seasons = c(7.5, 52.3, 365.25))

  expect_true(is.numeric(t_eff@seasons))
  expect_equal(length(t_eff@seasons), 3)
})

test_that("temporal_effects handles zero season", {
  t_eff <- temporal_effects(seasons = 0)
  expect_equal(t_eff@seasons, 0)
})

test_that("temporal_effects handles negative seasons", {
  t_eff <- temporal_effects(seasons = c(-1, -7, -52))

  expect_equal(length(t_eff@seasons), 3)
  expect_true(all(c(-1, -7, -52) %in% t_eff@seasons))
})

# ============================================================================
# get_temporal_effects / get_temporal_effect_cols
# ============================================================================

test_that("get_temporal_effects returns list of specs before computation", {
  data(denguedat)

  df_now <- denguedat %>%
    tbl_now(event_date = "onset_week", report_date = "report_week", verbose = FALSE) %>%
    add_temporal_effects(temporal_effects(day_of_week = TRUE, week_of_year = TRUE))

  specs <- get_temporal_effects(df_now)
  expect_type(specs, "list")
  expect_equal(length(specs), 1L)
  expect_true(specs[[1]]$t_effects@day_of_week)
  expect_true(specs[[1]]$t_effects@week_of_year)
})

test_that("get_temporal_effect_cols returns character(0) before computation", {
  data(denguedat)

  df_now <- denguedat %>%
    tbl_now(event_date = "onset_week", report_date = "report_week", verbose = FALSE) %>%
    add_temporal_effects(temporal_effects(day_of_week = TRUE))

  expect_equal(get_temporal_effect_cols(df_now), character(0))
})

test_that("get_temporal_effect_cols returns column names after computation", {
  data(denguedat)

  df_computed <- denguedat %>%
    tbl_now(event_date = "onset_week", report_date = "report_week", verbose = FALSE) %>%
    add_temporal_effects(temporal_effects(day_of_week = TRUE, week_of_year = TRUE)) %>%
    compute_temporal_effects()

  cols <- get_temporal_effect_cols(df_computed)
  expect_true(".event_day_of_week"  %in% cols)
  expect_true(".event_week_of_year" %in% cols)
})

# ============================================================================
# replace / remove temporal effects
# ============================================================================

test_that("remove_temporal_effects clears spec and computed cols", {
  data(denguedat)

  df_computed <- denguedat %>%
    tbl_now(event_date = "onset_week", report_date = "report_week", verbose = FALSE) %>%
    add_temporal_effects(temporal_effects(day_of_week = TRUE)) %>%
    compute_temporal_effects()

  cleared <- remove_temporal_effects(df_computed)

  expect_equal(length(get_temporal_effects(cleared)), 0L)
  expect_equal(get_temporal_effect_cols(cleared), character(0))
  expect_false(".event_day_of_week" %in% names(cleared))
})

test_that("replace_temporal_effects replaces spec with new one", {
  data(denguedat)

  df_now <- denguedat %>%
    tbl_now(event_date = "onset_week", report_date = "report_week", verbose = FALSE) %>%
    add_temporal_effects(temporal_effects(day_of_week = TRUE)) %>%
    compute_temporal_effects()

  replaced <- replace_temporal_effects(df_now, temporal_effects(week_of_year = TRUE))

  # Old spec gone
  spec <- get_temporal_effects(replaced)
  expect_equal(length(spec), 1L)
  expect_false(spec[[1]]$t_effects@day_of_week)
  expect_true(spec[[1]]$t_effects@week_of_year)
  # Old computed cols removed
  expect_false(".event_day_of_week" %in% names(replaced))
})
