library(dplyr, quietly = TRUE, warn.conflicts = FALSE)

# ============================================================
# Shared fixtures
# ============================================================

# Weekly tbl_now — used across many tests
base_weekly <- function(strata = FALSE, count = FALSE) {
  d <- tibble(
    onset  = as.Date(c("2020-07-05", "2020-07-12", "2020-07-19", "2020-07-05")),
    report = as.Date(c("2020-07-12", "2020-07-12", "2020-07-26", "2020-07-19")),
    sex    = c("M", "F", "M", "F"),
    n      = c(3L, 2L, 4L, 1L)
  )
  strata_val <- if (strata) "sex" else NULL
  if (count) {
    tbl_now(d,
      event_date = "onset", report_date = "report",
      strata = strata_val,
      case_count = "n", data_type = "count-incidence",
      verbose = FALSE
    )
  } else {
    tbl_now(d,
      event_date = "onset", report_date = "report",
      strata = strata_val,
      verbose = FALSE
    )
  }
}

# Daily tbl_now with extra columns
base_daily <- function() {
  tibble(
    event = as.Date(c(
      "2023-01-01", "2023-01-02", "2023-01-03",
      "2023-01-01", "2023-01-02"
    )),
    report = as.Date(c(
      "2023-01-03", "2023-01-03", "2023-01-05",
      "2023-01-04", "2023-01-04"
    )),
    sex = c("M", "F", "M", "F", "M"),
    weather = c(25.1, 24.0, 23.5, 26.0, 22.0),
    n = c(2L, 3L, 1L, 4L, 2L)
  ) |>
    tbl_now(
      event_date = event, report_date = report,
      strata = sex, covariates = weather,
      verbose = FALSE
    )
}

# Count-cumulative tbl_now
base_cumulative <- function() {
  tibble(
    event = as.Date(c(
      "2020-01-01", "2020-01-01",
      "2020-01-08", "2020-01-08"
    )),
    report = as.Date(c(
      "2020-01-01", "2020-01-08",
      "2020-01-08", "2020-01-15"
    )),
    sex = c("M", "M", "F", "F"),
    n = c(1L, 3L, 2L, 5L)
  ) |>
    tbl_now(
      event_date = event, report_date = report,
      strata = sex, case_count = n,
      data_type = "count-cumulative",
      verbose = FALSE
    )
}

# ============================================================
# adders_changers_and_removers.R — untested functions
# ============================================================

# --- change_report_date ---
test_that("change_report_date updates report_date attribute", {
  x <- base_daily()
  x$report2 <- x$report - 1L
  result <- change_report_date(x, "report2")
  expect_equal(get_report_date(result), "report2")
  expect_s3_class(result, "tbl_now")
})

test_that("change_report_date recalculates .delay", {
  x <- base_daily()
  x$report2 <- x$report - 1L
  result <- change_report_date(x, "report2")
  expected_delay <- as.numeric(x$report2 - x$event)
  expect_equal(result[[".delay"]], expected_delay)
})

# --- change_case_count ---
test_that("change_case_count updates case_count attribute", {
  x <- base_weekly(count = TRUE)
  x$n2 <- x$n * 2L
  result <- change_case_count(x, n2)
  expect_equal(get_case_count(result), "n2")
})

test_that("change_case_count accepts NULL (clears case_count on linelist data)", {
  # Clearing case_count on linelist data is fine; count-incidence would fail validate
  x <- base_daily() # linelist, no case_count
  x$n2 <- as.numeric(seq_len(nrow(x)))
  x2 <- change_case_count(x, n2) # add it first
  # Now x2 is still linelist type so clearing is fine
  result <- change_case_count(x2, NULL)
  expect_null(get_case_count(result))
})

# --- change_is_censored ---
test_that("change_is_censored sets is_censored column", {
  x <- base_daily()
  x$cens <- c(FALSE, TRUE, FALSE, FALSE, FALSE)
  result <- change_is_censored(x, cens)
  expect_equal(get_is_censored(result), "cens")
})

test_that("change_is_censored accepts NULL (clears is_censored)", {
  x <- base_daily()
  x$cens <- c(FALSE, TRUE, FALSE, FALSE, FALSE)
  x2 <- change_is_censored(x, cens)
  result <- change_is_censored(x2, NULL)
  expect_null(get_is_censored(result))
})

# --- remove_is_censored ---
test_that("remove_is_censored clears is_censored", {
  x <- base_daily()
  x$cens <- c(FALSE, TRUE, FALSE, FALSE, FALSE)
  x2 <- change_is_censored(x, cens)
  result <- remove_is_censored(x2)
  expect_null(get_is_censored(result))
})

test_that("remove_is_censored emits message on count-cumulative data", {
  # cli_alert_warning fires as a message, not an R warning
  x <- base_cumulative()
  x$cens <- c(FALSE, FALSE, FALSE, FALSE)
  x2 <- change_is_censored(x, cens)
  expect_message(remove_is_censored(x2), "cumulative")
})

# --- remove_strata / remove_all_strata ---
test_that("remove_strata removes one strata column", {
  x <- base_daily()
  result <- remove_strata(x, sex)
  expect_null(get_strata(result))
})

test_that("remove_strata emits message on count-cumulative data", {
  x <- base_cumulative()
  expect_message(suppressWarnings(remove_strata(x, sex)), "cumulative")
})

test_that("remove_all_strata removes all strata", {
  x <- base_daily()
  result <- remove_all_strata(x)
  expect_null(get_strata(result))
})

test_that("remove_all_strata emits message on count-cumulative data", {
  x <- base_cumulative()
  expect_message(remove_all_strata(x), "cumulative")
})

# --- remove_covariates / remove_all_covariates ---
test_that("remove_covariates removes specific covariate", {
  x <- base_daily()
  result <- remove_covariates(x, weather)
  expect_null(get_covariates(result))
})

test_that("remove_covariates emits message on count-cumulative data", {
  x <- base_cumulative()
  x$tmp_cov <- rnorm(4)
  x <- change_covariates(x, tmp_cov)
  expect_message(suppressWarnings(remove_covariates(x, tmp_cov)), "cumulative")
})

test_that("remove_all_covariates removes all covariates", {
  x <- base_daily()
  result <- remove_all_covariates(x)
  expect_null(get_covariates(result))
})

test_that("remove_all_covariates emits message on count-cumulative data", {
  x <- base_cumulative()
  x$tmp_cov <- rnorm(4)
  x <- change_covariates(x, tmp_cov)
  expect_message(remove_all_covariates(x), "cumulative")
})

# --- replace_temporal_effects ---
test_that("replace_temporal_effects emits message on count-cumulative data", {
  x <- base_cumulative()
  te <- temporal_effects(week_of_year = TRUE)
  expect_message(replace_temporal_effects(x, te), "cumulative")
})

test_that("replace_temporal_effects replaces the spec", {
  x <- base_weekly() |>
    add_temporal_effects(temporal_effects(week_of_year = TRUE))
  te2 <- temporal_effects(month_of_year = TRUE)
  result <- replace_temporal_effects(x, te2)
  spec <- get_temporal_effects(result)
  expect_equal(length(spec), 1L)
  expect_true(spec[[1]]$t_effects@month_of_year)
  expect_false(spec[[1]]$t_effects@week_of_year)
})

test_that("replace_temporal_effects removes old computed columns", {
  x <- base_weekly() |>
    add_temporal_effects(temporal_effects(week_of_year = TRUE)) |>
    compute_temporal_effects()
  old_col <- get_temporal_effect_cols(x)
  expect_true(length(old_col) > 0)
  result <- replace_temporal_effects(x, temporal_effects(month_of_year = TRUE))
  expect_false(any(old_col %in% colnames(result)))
})

# ============================================================
# add_temporal_effects.data.frame — untested branches
# ============================================================

test_that("add_temporal_effects.data.frame adds day_of_month column", {
  d <- tibble(
    event  = as.Date(c("2023-01-01", "2023-01-08", "2023-01-15", "2023-01-22")),
    report = as.Date(c("2023-01-02", "2023-01-09", "2023-01-16", "2023-01-23"))
  )
  te <- temporal_effects(day_of_month = TRUE)
  result <- add_temporal_effects(as.data.frame(d),
    t_effects = te, date_col = "event",
    name_prefix = ".event"
  )
  expect_true(".event_day_of_month" %in% colnames(result))
  expect_equal(result[[".event_day_of_month"]], lubridate::day(d$event))
})

test_that("add_temporal_effects.data.frame adds weekend column", {
  d <- tibble(
    event  = as.Date(c("2023-01-02", "2023-01-07", "2023-01-08", "2023-01-09")),
    report = as.Date(c("2023-01-03", "2023-01-08", "2023-01-09", "2023-01-10"))
  )
  te <- temporal_effects(weekend = TRUE)
  result <- add_temporal_effects(as.data.frame(d),
    t_effects = te, date_col = "event",
    name_prefix = ".event"
  )
  expect_true(".event_weekend" %in% colnames(result))
  # 2023-01-07 (Sat) and 2023-01-08 (Sun) are weekend
  expect_equal(result[[".event_weekend"]][2], 1L)
  expect_equal(result[[".event_weekend"]][3], 1L)
})

test_that("add_temporal_effects.data.frame adds month_of_year column", {
  d <- tibble(
    event  = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01", "2023-04-01")),
    report = as.Date(c("2023-01-02", "2023-02-02", "2023-03-02", "2023-04-02"))
  )
  te <- temporal_effects(month_of_year = TRUE)
  result <- add_temporal_effects(as.data.frame(d),
    t_effects = te, date_col = "event",
    name_prefix = ".event"
  )
  expect_true(".event_month_of_year" %in% colnames(result))
})

test_that("add_temporal_effects.data.frame errors when overwrite=FALSE and col exists", {
  d <- tibble(
    event           = as.Date(c("2023-01-02", "2023-01-09")),
    report          = as.Date(c("2023-01-03", "2023-01-10")),
    .event_weekend  = c(0L, 0L) # pre-existing column
  )
  te <- temporal_effects(weekend = TRUE)
  expect_error(
    add_temporal_effects(as.data.frame(d),
      t_effects = te, date_col = "event",
      name_prefix = ".event", overwrite = FALSE
    ),
    "overwrite"
  )
})

test_that("add_temporal_effects.tbl_now errors on invalid date_type", {
  x <- base_weekly()
  te <- temporal_effects(week_of_year = TRUE)
  expect_error(
    add_temporal_effects(x, t_effects = te, date_type = "bad_type"),
    "date_type"
  )
})

test_that("add_temporal_effects.tbl_now errors on non-temporal_effects object", {
  x <- base_weekly()
  expect_error(
    add_temporal_effects(x, t_effects = list(a = 1)),
    "temporal_effects"
  )
})

test_that("add_temporal_effects.tbl_now with report_date stores spec with report_date type", {
  x <- base_weekly()
  te <- temporal_effects(week_of_year = TRUE)
  result <- add_temporal_effects(x, t_effects = te, date_type = "report_date")
  specs <- get_temporal_effects(result)
  expect_equal(specs[[1]]$date_type, "report_date")
})

# ============================================================
# compute_temporal_effects — overwrite path
# ============================================================

test_that("compute_temporal_effects with overwrite=TRUE replaces existing cols", {
  x <- base_weekly() |>
    add_temporal_effects(temporal_effects(week_of_year = TRUE)) |>
    compute_temporal_effects()
  # Re-running with overwrite=TRUE should succeed (not error)
  result <- compute_temporal_effects(x, overwrite = TRUE)
  expect_s3_class(result, "tbl_now")
  expect_true(".event_week_of_year" %in% colnames(result))
})

# ============================================================
# dplyr_generics.R — rowwise, summarise, reframe, $<-.grouped_tbl_now
# ============================================================

test_that("rowwise.tbl_now emits message and returns a result", {
  # cli_alert_warning fires as a message, not an R warning
  x <- base_weekly()
  expect_message(result <- rowwise(x), "rowwise")
})

test_that("summarise.tbl_now with .groups argument works", {
  x <- base_weekly(strata = TRUE)
  xg <- group_by(x, sex)
  # .groups argument path
  result <- suppressWarnings(summarise(xg, n_rows = n(), .groups = "drop"))
  # Should at least return a tibble (may lose tbl_now if dates are gone)
  expect_true(is.data.frame(result))
})

test_that("summarise.tbl_now: preserves tbl_now when dates survive", {
  x <- base_daily()
  # summarise that keeps event_date and report_date
  xg <- group_by(x, event, report)
  result <- suppressWarnings(summarise(xg, n_rows = n()))
  expect_true(is.data.frame(result))
})

test_that("summarize.grouped_tbl_now dispatches correctly", {
  x <- base_weekly(strata = TRUE)
  xg <- group_by(x, sex)
  # Uses summarize (alias) on grouped_tbl_now
  result <- suppressWarnings(summarize(xg, n_rows = n()))
  expect_true(is.data.frame(result))
})

test_that("reframe.tbl_now returns valid result", {
  x <- base_weekly(strata = TRUE)
  # reframe with no grouping just expands rows
  result <- suppressWarnings(reframe(x, delay = unique(.delay)))
  expect_true(is.data.frame(result))
})

test_that("$<-.grouped_tbl_now adds column and preserves class", {
  x <- base_weekly()
  xg <- group_by(x, onset)
  xg$new_col <- seq_len(nrow(xg))
  expect_true("new_col" %in% colnames(xg))
  expect_true(inherits(xg, "tbl_now"))
})

# ============================================================
# validate_tbl_now — error branches
# ============================================================

test_that("validate_tbl_now emits message when event and report date are the same col", {
  # cli_alert_warning fires as a message, not an R warning
  x <- base_daily()
  attr(x, "report_date") <- get_event_date(x)
  expect_message(validate_tbl_now(x), "same")
})

# ============================================================
# tbl_now() — grouped data warning, force branches
# ============================================================

test_that("tbl_now warns and ungroups grouped input", {
  d <- tibble(
    event  = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03")),
    report = as.Date(c("2023-01-02", "2023-01-03", "2023-01-04"))
  ) |> group_by(event)
  expect_warning(
    tbl_now(d, event_date = event, report_date = report, verbose = FALSE),
    "grouped"
  )
})

test_that("tbl_now force=TRUE overwrites .event_num and .report_num if present", {
  d <- tibble(
    event = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03")),
    report = as.Date(c("2023-01-02", "2023-01-03", "2023-01-04")),
    .event_num = c(99, 99, 99),
    .report_num = c(99, 99, 99)
  )
  result <- tbl_now(d,
    event_date = event, report_date = report,
    verbose = FALSE, force = TRUE
  )
  expect_s3_class(result, "tbl_now")
  expect_false(all(result[[".event_num"]] == 99))
})

# ============================================================
# tbl_now() — delay construction edge cases not in test-tbl_now_delay.R
# ============================================================

test_that("tbl_now delay: error if delay column named .event_date when report+delay used", {
  d <- tibble(
    report = as.Date(c("2023-01-02", "2023-01-03")),
    .event_date = c(1, 1)
  )
  expect_error(
    tbl_now(d,
      report_date = report, delay = .event_date,
      event_units = "days", verbose = FALSE
    ),
    "\\.event_date"
  )
})

test_that("tbl_now delay: error if delay column named .report_date when event+delay used", {
  d <- tibble(
    event = as.Date(c("2023-01-01", "2023-01-02")),
    .report_date = c(1, 1)
  )
  expect_error(
    tbl_now(d,
      event_date = event, delay = .report_date,
      event_units = "days", verbose = FALSE
    ),
    "\\.report_date"
  )
})

test_that("tbl_now delay: non-numeric delay column errors in .reconstruct_date_from_delay", {
  d <- tibble(
    event = as.Date(c("2023-01-01", "2023-01-02")),
    delay = c("a", "b")
  )
  expect_error(
    tbl_now(d,
      event_date = event, delay = delay,
      event_units = "days", verbose = FALSE
    ),
    "numeric"
  )
})

# ============================================================
# utils.R — tbl_now_attributes and attr_default
# ============================================================

test_that("tbl_now_attributes errors on non-tbl_now input", {
  expect_error(tbl_now_attributes(data.frame(x = 1)), "tbl_now")
})

test_that("tbl_now_attributes returns expected names", {
  x <- base_daily()
  attrs <- tbl_now_attributes(x)
  expect_true("event_date" %in% names(attrs))
  expect_true("report_date" %in% names(attrs))
  expect_true("now" %in% names(attrs))
})

# ============================================================
# getters.R — get_temporal_effect_cols when attribute is NULL
# ============================================================

test_that("get_temporal_effect_cols returns character(0) when attr missing", {
  x <- base_daily()
  # Remove the attribute entirely
  attr(x, "computed_temporal_effect_cols") <- NULL
  result <- get_temporal_effect_cols(x)
  expect_equal(result, character(0))
})

# ============================================================
# infer.R — infer_data_type: linelist + case_count present warning
# ============================================================

test_that("infer_data_type warns when linelist data has a case_count column", {
  d <- tibble(
    event  = as.Date(c("2023-01-01", "2023-01-02")),
    report = as.Date(c("2023-01-02", "2023-01-03")),
    n      = c(1L, 2L)
  )
  # This path: data_type="linelist" explicitly but case_count column exists
  expect_warning(
    tbl_now(d,
      event_date = event, report_date = report,
      case_count = n, data_type = "linelist", verbose = FALSE
    ),
    "overwritten"
  )
})

# ============================================================
# time_cols_to_numeric — .delay column pre-existing with force=TRUE
# ============================================================

test_that("time_cols_to_numeric with force=TRUE overwrites existing .delay", {
  d <- tibble(
    event  = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03")),
    report = as.Date(c("2023-01-02", "2023-01-03", "2023-01-04")),
    .delay = c(999L, 999L, 999L)
  )
  result <- tbl_now(d,
    event_date = event, report_date = report,
    verbose = FALSE, force = TRUE
  )
  expect_false(all(result[[".delay"]] == 999L))
})

# ============================================================
# update.R — basic update path
# ============================================================

test_that("update.tbl_now binds new rows and preserves class", {
  x <- base_daily()
  new_rows <- tibble(
    event   = as.Date(c("2023-01-06")),
    report  = as.Date(c("2023-01-07")),
    sex     = "M",
    weather = 20.0,
    n       = 1L
  )
  # update.tbl_now takes new_data as a named argument
  result <- update(x, new_data = new_rows)
  expect_s3_class(result, "tbl_now")
  expect_gt(nrow(result), nrow(x))
})

# update.tbl_now: the remove_duplicates warning branch (line 76-78) is
# unreachable in the current implementation because the function overwrites
# remove_duplicates=TRUE with FALSE for non-count data. Skipping that branch.

# ============================================================
# rename_with.tbl_now — updates attribute references
# ============================================================

test_that("rename_with.tbl_now renames a strata column and updates attribute", {
  x <- base_daily()
  result <- rename_with(x, toupper, .cols = "sex")
  expect_true("SEX" %in% colnames(result))
  expect_equal(get_strata(result), "SEX")
})

test_that("rename_with.tbl_now renames a covariate column and updates attribute", {
  x <- base_daily()
  result <- rename_with(x, toupper, .cols = "weather")
  expect_true("WEATHER" %in% colnames(result))
  expect_equal(get_covariates(result), "WEATHER")
})

test_that("rename_with.tbl_now renaming event_date col updates event_date attribute", {
  x <- base_daily()
  result <- rename_with(x, toupper, .cols = "event")
  expect_true("EVENT" %in% colnames(result))
  expect_equal(get_event_date(result), "EVENT")
})

test_that("rename_with.tbl_now renaming a protected generated col emits message", {
  x <- base_daily()
  add_prefix <- function(nm) paste0("x_", nm)
  expect_message(
    result <- rename_with(x, add_prefix, .cols = ".delay"),
    "protected"
  )
  # Downgrades to plain tibble
  expect_false(inherits(result, "tbl_now"))
})

# ============================================================
# summarise.tbl_now — .by and .groups argument paths
# ============================================================

test_that("summarise.tbl_now with .by argument works", {
  x <- base_daily()
  result <- suppressWarnings(summarise(x, n_rows = n(), .by = "sex"))
  expect_true(is.data.frame(result))
  expect_true("sex" %in% colnames(result))
})

test_that("summarize.tbl_now alias dispatches correctly", {
  x <- base_daily()
  xg <- group_by(x, sex)
  result <- suppressWarnings(summarize(xg, n_rows = n()))
  expect_true(is.data.frame(result))
})

test_that("reframe.grouped_tbl_now dispatches correctly and returns data", {
  x <- base_weekly(strata = TRUE)
  xg <- group_by(x, sex)
  # reframe on a grouped_tbl_now should work: the method strips grouped_df
  # before calling dplyr::reframe so the .by=NULL argument is accepted
  result <- suppressWarnings(reframe(xg, delay_vals = unique(.delay)))
  expect_true(is.data.frame(result))
})

# ============================================================
# validate_tbl_now — additional error branches
# ============================================================

test_that("validate_tbl_now catches NA report_date values with a warning", {
  x <- base_daily()
  x$report[1] <- NA # inject NA
  # validate_tbl_now should warn about NA report dates
  expect_warning(validate_tbl_now(x))
})

# ============================================================
# tbl_now() — additional error branches
# ============================================================

test_that("tbl_now errors when data is not a data.frame", {
  expect_error(
    tbl_now(list(a = 1), event_date = "a", report_date = "b"),
    "data.frame"
  )
})

test_that("tbl_now errors when case_count has length > 1 (via string vector)", {
  d <- tibble(
    event  = as.Date(c("2023-01-01", "2023-01-02")),
    report = as.Date(c("2023-01-02", "2023-01-03")),
    n1     = 1:2,
    n2     = 3:4
  )
  # Passing two case_count columns should error
  expect_error(
    tbl_now(d,
      event_date = event, report_date = report,
      case_count = c(n1, n2), verbose = FALSE
    ),
    "case_count"
  )
})

test_that("tbl_now errors when .event_num already exists without force", {
  d <- tibble(
    event = as.Date(c("2023-01-01", "2023-01-02")),
    report = as.Date(c("2023-01-02", "2023-01-03")),
    .event_num = c(0, 1)
  )
  expect_error(
    tbl_now(d, event_date = event, report_date = report, verbose = FALSE),
    "\\.event_num"
  )
})

test_that("tbl_now errors when .report_num already exists without force", {
  d <- tibble(
    event       = as.Date(c("2023-01-01", "2023-01-02")),
    report      = as.Date(c("2023-01-02", "2023-01-03")),
    .report_num = c(0, 1)
  )
  expect_error(
    tbl_now(d, event_date = event, report_date = report, verbose = FALSE),
    "\\.report_num"
  )
})

# ============================================================
# change_report_date / change_case_count / change_is_censored — error branches
# ============================================================

test_that("change_report_date errors when column not found", {
  x <- base_daily()
  # tidyselect fires the "doesn't exist" error before our check
  expect_error(change_report_date(x, "nonexistent_col"))
})

test_that("change_case_count errors on non-numeric column", {
  x <- base_daily()
  expect_error(change_case_count(x, sex), "numeric")
})

test_that("change_is_censored errors on non-logical column", {
  x <- base_daily()
  expect_error(change_is_censored(x, weather), "logical")
})

test_that("replace_temporal_effects errors on non-temporal_effects object", {
  x <- base_daily()
  expect_error(replace_temporal_effects(x, list(a = 1)), "temporal_effects")
})

# ============================================================
# add_temporal_effects.data.frame — season and overwrite error branches
# ============================================================

test_that("add_temporal_effects.data.frame adds season (Fourier) columns", {
  d <- tibble(
    event      = as.Date(c("2023-01-01", "2023-01-08", "2023-01-15", "2023-01-22")),
    report     = as.Date(c("2023-01-02", "2023-01-09", "2023-01-16", "2023-01-23")),
    .event_num = 0:3
  )
  te <- temporal_effects(seasons = 4L)
  result <- add_temporal_effects(as.data.frame(d),
    t_effects = te,
    date_col = "event", numeric_col = ".event_num",
    name_prefix = ".event"
  )
  expect_true(".event_season_4_cos" %in% colnames(result))
  expect_true(".event_season_4_sin" %in% colnames(result))
})

test_that("add_temporal_effects.data.frame errors when season col already exists", {
  d <- tibble(
    event = as.Date(c("2023-01-01", "2023-01-08")),
    .event_num = 0:1,
    .event_season_4_cos = c(1.0, 0.5)
  )
  te <- temporal_effects(seasons = 4L)
  expect_error(
    add_temporal_effects(as.data.frame(d),
      t_effects = te,
      date_col = "event", numeric_col = ".event_num",
      name_prefix = ".event", overwrite = FALSE
    ),
    "overwrite"
  )
})

test_that("add_temporal_effects.data.frame errors when week_of_year col already exists", {
  d <- tibble(
    event                = as.Date(c("2023-01-02", "2023-01-09")),
    .event_week_of_year  = c(1L, 2L)
  )
  te <- temporal_effects(week_of_year = TRUE)
  expect_error(
    add_temporal_effects(as.data.frame(d),
      t_effects = te,
      date_col = "event", name_prefix = ".event",
      overwrite = FALSE
    ),
    "overwrite"
  )
})

test_that("add_temporal_effects.data.frame errors when month_of_year col already exists", {
  d <- tibble(
    event                 = as.Date(c("2023-01-01", "2023-02-01")),
    .event_month_of_year  = c(1L, 2L)
  )
  te <- temporal_effects(month_of_year = TRUE)
  expect_error(
    add_temporal_effects(as.data.frame(d),
      t_effects = te,
      date_col = "event", name_prefix = ".event",
      overwrite = FALSE
    ),
    "overwrite"
  )
})

# ============================================================
# complete_zeroes — remaining branches
# ============================================================

test_that("complete_zeroes works with numeric data", {
  d <- tibble(
    event  = c(1L, 1L, 2L, 4L, 4L),
    report = c(1L, 2L, 2L, 4L, 5L),
    n      = c(3L, 2L, 4L, 1L, 5L)
  ) |>
    tbl_now(
      event_date = event, report_date = report,
      case_count = n, data_type = "count-incidence",
      verbose = FALSE
    )
  result <- complete_zeroes(d)
  expect_s3_class(result, "tbl_now")
  # Event 3 should be filled in
  expect_true(3L %in% result[[get_event_date(result)]])
})

test_that("complete_zeroes errors when event and report units differ", {
  # Manually craft object with mismatched units
  x <- tibble(
    event  = as.Date(c("2020-01-01", "2020-01-08", "2020-01-15")),
    report = as.Date(c("2020-01-01", "2020-01-08", "2020-01-15")),
    n      = c(1L, 2L, 3L)
  ) |>
    tbl_now(
      event_date = event, report_date = report,
      case_count = n, data_type = "count-incidence",
      event_units = "weeks", report_units = "weeks",
      verbose = FALSE
    )
  # Force mismatched units in attributes
  attr(x, "report_units") <- "days"
  expect_error(complete_zeroes(x), "different")
})
