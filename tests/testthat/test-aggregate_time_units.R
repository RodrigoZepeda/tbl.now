# `aggregate_time_units()` coarsens the time grid a `tbl_now` lives on. The
# assertions worth making are about the three things a coarser grid must not
# break: the total number of cases, the `event <= report <= now` timeline, and
# the units the object reports about itself.

library(dplyr, quietly = TRUE, warn.conflicts = FALSE)

# 2024-01-01 is a Monday, so the epi week (Sunday-start) containing it begins
# on 2023-12-31. Every expected date below is worked out by hand from that.
make_daily_linelist <- function() {
  df <- data.frame(
    onset = as.Date("2024-01-01") + c(0, 1, 3, 8, 9, 15),
    reported = as.Date("2024-01-01") + c(2, 2, 5, 9, 12, 16),
    sex = c("F", "M", "F", "M", "F", "M")
  )
  tbl_now(df,
    event_date = onset, report_date = reported, strata = sex,
    data_type = "linelist", units = "days", verbose = FALSE
  )
}

make_daily_counts <- function(type = "count-incidence") {
  df <- data.frame(
    event = as.Date("2024-01-01") + rep(c(0, 1, 8), each = 3),
    report = as.Date("2024-01-01") + c(0, 1, 2, 1, 2, 3, 8, 9, 10),
    n = if (type == "count-cumulative") {
      c(1, 3, 5, 2, 2, 4, 1, 1, 7)
    } else {
      c(1, 2, 2, 2, 0, 2, 1, 0, 6)
    }
  )
  tbl_now(df,
    event_date = event, report_date = report, case_count = n,
    data_type = type, units = "days", verbose = FALSE
  )
}

make_validated <- function() {
  cases <- data.frame(
    onset = as.Date("2021-01-04") + 0:9,
    visit = as.Date("2021-01-05") + 0:9,
    result = as.Date("2021-01-06") + 0:9,
    outcome = c(rep("confirmed", 8), "retracted", "pending")
  )
  cases$result[10] <- NA
  tbl_now(cases,
    event_date = onset, report_date = visit,
    validation_date = result, validation_type = outcome,
    data_type = "linelist", units = "days", verbose = FALSE
  )
}

# ---- Wrong inputs -----------------------------------------------------------

test_that("aggregate_time_units refuses anything that is not a tbl_now", {
  expect_error(
    aggregate_time_units(data.frame(a = 1)),
    "must be a <tbl_now>"
  )
  expect_error(aggregate_time_units(NULL), "must be a <tbl_now>")
})

test_that("aggregate_time_units refuses an unknown `to`", {
  x <- make_daily_linelist()
  expect_error(aggregate_time_units(x, to = "fortnights"), "Invalid")
  expect_error(aggregate_time_units(x, to = "numeric"), "Invalid")
  expect_error(aggregate_time_units(x, to = c("weeks", "months")), "single string")
  expect_error(aggregate_time_units(x, to = NA_character_), "single string")
  expect_error(aggregate_time_units(x, to = 7), "single string")
})

test_that("aggregate_time_units refuses an unknown axis, and says `to` is the unit", {
  x <- make_daily_linelist()
  expect_error(aggregate_time_units(x, axes = "onset"), "Unknown")
  # The confusable case: `axes` takes an axis name, not a time unit.
  expect_error(aggregate_time_units(x, axes = "weeks"), "axes")
  expect_error(aggregate_time_units(x, axes = character(0)), "character vector")
  expect_error(aggregate_time_units(x, axes = NA), "character vector")
})

test_that("aggregate_time_units refuses an unknown `label` and a non-logical verbose", {
  x <- make_daily_linelist()
  expect_error(aggregate_time_units(x, label = "middle"), "must be one of")
  expect_error(aggregate_time_units(x, verbose = "yes"), "verbose")
})

test_that("aggregation never refines: weeks cannot go back to days", {
  weekly <- aggregate_time_units(make_daily_linelist(), to = "weeks", verbose = FALSE)
  expect_error(
    aggregate_time_units(weekly, to = "days"),
    "finer"
  )
  monthly <- aggregate_time_units(weekly, to = "months", verbose = FALSE)
  expect_error(aggregate_time_units(monthly, to = "weeks"), "finer")
})

test_that("a numeric axis has no calendar to aggregate on", {
  df <- data.frame(event = 1:10, report = 1:10 + 2L, n = 1:10)
  x <- tbl_now(df,
    event_date = event, report_date = report, case_count = n,
    data_type = "count-incidence", units = "numeric", verbose = FALSE
  )
  expect_error(aggregate_time_units(x, to = "weeks"), "numeric")
})

test_that("aggregating only the event axis is refused with a usable hint", {
  expect_error(
    aggregate_time_units(make_daily_linelist(), to = "weeks", axes = "event"),
    "coarser than the report axis"
  )
})

# ---- Columns that are not in the data ---------------------------------------

test_that("the validation axis needs a validation process", {
  x <- make_daily_linelist()
  expect_false(has_validation(x))
  expect_error(
    aggregate_time_units(x, axes = "validation"),
    "needs a validation process"
  )
  expect_error(
    aggregate_time_units(x, axes = c("event", "report", "validation")),
    "needs a validation process"
  )
})

test_that("`axes = \"all\"` simply skips the validation axis when there is none", {
  out <- aggregate_time_units(make_daily_linelist(), to = "weeks", verbose = FALSE)
  expect_true(is_tbl_now(out))
  expect_null(get_validation_units(out))
})

# ---- Results worked out by hand ---------------------------------------------

test_that("a daily line list lands on the right epi weeks", {
  out <- aggregate_time_units(make_daily_linelist(), to = "weeks", verbose = FALSE)

  # Onsets Jan 1, 2, 4 -> week of 2023-12-31; Jan 9, 10 -> 2024-01-07;
  # Jan 16 -> 2024-01-14.
  expect_equal(
    out[[get_event_date(out)]],
    as.Date(c(rep("2023-12-31", 3), rep("2024-01-07", 2), "2024-01-14"))
  )
  # Reports Jan 3, 3, 6 -> 2023-12-31; Jan 10, 13 -> 2024-01-07;
  # Jan 17 -> 2024-01-14.
  expect_equal(
    out[[get_report_date(out)]],
    as.Date(c(rep("2023-12-31", 3), rep("2024-01-07", 2), "2024-01-14"))
  )
  expect_equal(out$.delay, rep(0, 6))
  expect_equal(get_event_units(out), "weeks")
  expect_equal(get_report_units(out), "weeks")
  expect_equal(get_now(out), as.Date("2024-01-14"))
  # A line list keeps one row per case.
  expect_equal(nrow(out), 6)
  expect_equal(get_data_type(out), "linelist")
  expect_equal(get_strata(out), "sex")
})

test_that("`label = \"end\"` names the period by its last day", {
  out <- aggregate_time_units(
    make_daily_linelist(),
    to = "weeks", label = "end", verbose = FALSE
  )
  # The epi week starting 2023-12-31 ends on 2024-01-06.
  expect_equal(
    out[[get_event_date(out)]],
    as.Date(c(rep("2024-01-06", 3), rep("2024-01-13", 2), "2024-01-20"))
  )
  # Labelling both axes the same way leaves the delays untouched.
  expect_equal(out$.delay, rep(0, 6))

  monthly <- aggregate_time_units(
    make_daily_linelist(),
    to = "months", label = "end", verbose = FALSE
  )
  expect_equal(unique(monthly[[get_event_date(monthly)]]), as.Date("2024-01-31"))
})

test_that("`label = \"end\"` is what makes a report-only aggregation usable", {
  x <- make_daily_linelist()

  # Named by the week START, a report lands before its own event.
  starts <- suppressWarnings(
    aggregate_time_units(x, to = "weeks", axes = "report", label = "start", verbose = FALSE)
  )
  expect_true(all(starts$.delay < 0))

  ends <- aggregate_time_units(
    x,
    to = "weeks", axes = "report", label = "end", verbose = FALSE
  )
  expect_true(all(ends$.delay >= 0))
  # Only the report axis moved.
  expect_equal(get_event_units(ends), "days")
  expect_equal(get_report_units(ends), "weeks")
  expect_equal(ends[[get_event_date(ends)]], x[[get_event_date(x)]])
})

test_that("count-incidence cells are summed, and the total is preserved", {
  x <- make_daily_counts("count-incidence")
  total <- sum(x$n)

  out <- aggregate_time_units(x, to = "weeks", verbose = FALSE)

  expect_equal(sum(out$n), total)
  expect_equal(get_data_type(out), "count-incidence")
  # Events Jan 1 and Jan 2 are one week; Jan 9 is the next. Reports Jan 1-4 are
  # in the first week, Jan 9-11 in the second, so exactly two cells survive.
  expect_equal(nrow(out), 2)
  expect_equal(
    out[[get_event_date(out)]],
    as.Date(c("2023-12-31", "2024-01-07"))
  )
  expect_equal(out$n, c(1 + 2 + 2 + 2 + 0 + 2, 1 + 0 + 6))
})

test_that("count-cumulative is de-accumulated before it is summed", {
  x <- make_daily_counts("count-cumulative")
  out <- aggregate_time_units(x, to = "weeks", verbose = FALSE)

  expect_equal(get_data_type(out), "count-cumulative")
  # Week 1 holds two event dates whose final cumulative totals are 5 and 4;
  # week 2 holds one whose total is 7. Naively summing the cumulative column
  # would have given 9 + 7 = 16 for week 1.
  expect_equal(out$n, c(9, 7))
})

test_that("aggregating to the unit an object already has is a no-op", {
  weekly <- aggregate_time_units(make_daily_linelist(), to = "weeks", verbose = FALSE)
  expect_message(
    again <- aggregate_time_units(weekly, to = "weeks"),
    "nothing to do"
  )
  expect_equal(again[[get_event_date(again)]], weekly[[get_event_date(weekly)]])
  expect_equal(get_now(again), get_now(weekly))

  # `to = "days"` on daily data is the same no-op.
  daily <- make_daily_linelist()
  expect_equal(
    aggregate_time_units(daily, to = "days", verbose = FALSE)[[get_event_date(daily)]],
    daily[[get_event_date(daily)]]
  )
})

test_that("the validation axis moves with the others, NAs included", {
  x <- make_validated()
  out <- aggregate_time_units(x, to = "weeks", verbose = FALSE)

  expect_equal(get_validation_units(out), "weeks")
  expect_true(has_validation(out))
  expect_equal(get_validation_type(out), "outcome")
  # The pending case has no validation date, and still has none.
  expect_true(is.na(out[[get_validation_date(out)]][10]))
  expect_equal(out[["outcome"]], x[["outcome"]])
  # 2021-01-04 is a Monday, so its epi week starts 2021-01-03.
  expect_equal(out[[get_validation_date(out)]][1], as.Date("2021-01-03"))
})

test_that("aggregating the validation axis alone leaves the other two alone", {
  x <- make_validated()
  out <- suppressWarnings(
    aggregate_time_units(x, to = "weeks", axes = "validation", verbose = FALSE)
  )
  expect_equal(get_event_units(out), "days")
  expect_equal(get_report_units(out), "days")
  expect_equal(get_validation_units(out), "weeks")
  expect_equal(out[[get_event_date(out)]], x[[get_event_date(x)]])
})

# ---- Grouped objects --------------------------------------------------------

test_that("a grouped tbl_now comes back grouped, with the same numbers", {
  x <- make_daily_linelist()
  ungrouped <- aggregate_time_units(x, to = "weeks", verbose = FALSE)

  out <- aggregate_time_units(x |> group_by(sex), to = "weeks", verbose = FALSE)

  expect_true(is_tbl_now(out))
  expect_equal(dplyr::group_vars(out), "sex")
  # `as_tibble()` keeps the class attributes, and a grouped object carries an
  # extra `groups` among them -- ungroup both sides to compare the data.
  expect_equal(as_tibble(ungroup(out)), as_tibble(ungroup(ungrouped)))

  # Grouping by a column the function does not care about changes nothing.
  by_other <- aggregate_time_units(
    x |> group_by(!!as.symbol(get_report_date(x))),
    to = "weeks", verbose = FALSE
  )
  expect_equal(as_tibble(ungroup(by_other)), as_tibble(ungroup(ungrouped)))
})

test_that("grouping does not change what a count aggregation computes", {
  x <- make_daily_counts("count-incidence")
  ungrouped <- aggregate_time_units(x, to = "weeks", verbose = FALSE)
  grouped <- aggregate_time_units(
    x |> group_by(!!as.symbol(get_event_date(x))),
    to = "weeks", verbose = FALSE
  )
  expect_equal(sum(grouped$n), sum(ungrouped$n))
  expect_equal(nrow(grouped), nrow(ungrouped))
})

test_that("a stratified count aggregation keeps every stratum's total", {
  df <- data.frame(
    event = rep(as.Date("2024-01-01") + c(0, 1, 8), each = 2),
    report = rep(as.Date("2024-01-03") + c(0, 1, 8), each = 2),
    sex = rep(c("F", "M"), 3),
    n = c(3, 4, 5, 6, 7, 8)
  )
  x <- tbl_now(df,
    event_date = event, report_date = report, case_count = n, strata = sex,
    data_type = "count-incidence", units = "days", verbose = FALSE
  )
  out <- aggregate_time_units(x, to = "months", verbose = FALSE)

  by_sex <- out |>
    as_tibble() |>
    dplyr::group_by(!!as.symbol("sex")) |>
    dplyr::summarise(total = sum(!!as.symbol("n")), .groups = "drop")
  expect_equal(by_sex$total, c(3 + 5 + 7, 4 + 6 + 8))
  expect_equal(get_strata(out), "sex")
})

# ---- Attributes that must survive ------------------------------------------

test_that("covariates, censoring and a user-set `now` survive the aggregation", {
  df <- data.frame(
    onset = as.Date("2024-01-01") + c(0, 1, 3, 8),
    reported = as.Date("2024-01-01") + c(2, 2, 5, 9),
    rain = c(1.1, 2.2, 3.3, 4.4),
    flag = c(TRUE, FALSE, FALSE, TRUE)
  )
  x <- tbl_now(df,
    event_date = onset, report_date = reported,
    covariates = rain, is_censored_report = flag,
    data_type = "linelist", units = "days",
    now = as.Date("2024-02-01"), verbose = FALSE
  )
  out <- aggregate_time_units(x, to = "weeks", verbose = FALSE)

  expect_equal(get_covariates(out), "rain")
  expect_equal(get_is_censored_report(out), "flag")
  expect_equal(out[["flag"]], df$flag)
  # `now` moves onto the new grid: 2024-02-01 falls in the week of 2024-01-28.
  expect_equal(get_now(out), as.Date("2024-01-28"))
})

test_that("materialised temporal-effect columns are dropped, the spec is kept", {
  x <- make_daily_linelist() |>
    add_temporal_effects(t_effects = temporal_effects(day_of_week = TRUE)) |>
    compute_temporal_effects()

  effect_cols <- get_temporal_effect_cols(x)
  expect_gt(length(effect_cols), 0)

  out <- aggregate_time_units(x, to = "weeks", verbose = FALSE)

  expect_false(any(effect_cols %in% colnames(out)))
  expect_equal(get_temporal_effect_cols(out), character(0))
  # The lazy spec survives, so the columns can be rebuilt on the new grid.
  expect_equal(get_temporal_effects(out), get_temporal_effects(x))
  expect_true(all(effect_cols %in% colnames(compute_temporal_effects(out))))
})

test_that("`type` and `align_on_day` mean what they mean in align_weeks()", {
  x <- make_daily_linelist()

  iso <- aggregate_time_units(
    x,
    to = "weeks", type = "iso", align_on_day = 1, verbose = FALSE
  )
  # ISO weeks start on Monday, and 2024-01-01 IS a Monday.
  expect_equal(iso[[get_event_date(iso)]][1], as.Date("2024-01-01"))

  epi <- aggregate_time_units(x, to = "weeks", verbose = FALSE)
  expect_equal(epi[[get_event_date(epi)]][1], as.Date("2023-12-31"))
})

test_that("years aggregate as well as weeks and months", {
  df <- data.frame(
    onset = as.Date(c("2022-03-01", "2022-11-30", "2023-02-01")),
    reported = as.Date(c("2022-03-05", "2022-12-04", "2023-02-06")),
    n = c(2, 3, 4)
  )
  x <- tbl_now(df,
    event_date = onset, report_date = reported, case_count = n,
    data_type = "count-incidence", units = "days", verbose = FALSE
  )
  out <- aggregate_time_units(x, to = "years", verbose = FALSE)

  expect_equal(get_event_units(out), "years")
  expect_equal(
    out[[get_event_date(out)]],
    as.Date(c("2022-01-01", "2023-01-01"))
  )
  expect_equal(out$n, c(5, 4))
  expect_equal(sum(out$n), sum(df$n))
})

test_that("the result is still a valid tbl_now a model could be handed", {
  out <- aggregate_time_units(make_daily_counts(), to = "weeks", verbose = FALSE)
  expect_silent(validate_tbl_now(out))
  expect_true(all(out$.delay >= 0))
  expect_true(all(out[[get_report_date(out)]] <= get_now(out)))
  expect_true(all(out[[get_event_date(out)]] <= out[[get_report_date(out)]]))
})

test_that("aggregate_time_units reports what it did unless silenced", {
  x <- make_daily_linelist()
  expect_message(aggregate_time_units(x, to = "weeks"), "Aggregated")
  expect_silent(aggregate_time_units(x, to = "weeks", verbose = FALSE))
})

# ---- Second pass: invariants, real data, and the neighbouring verbs ---------

test_that("aggregating is idempotent", {
  once <- aggregate_time_units(make_daily_linelist(), to = "weeks", verbose = FALSE)
  twice <- suppressMessages(aggregate_time_units(once, to = "weeks"))

  expect_equal(once[[get_event_date(once)]], twice[[get_event_date(twice)]])
  expect_equal(get_now(once), get_now(twice))
  expect_equal(nrow(once), nrow(twice))
})

test_that("aggregating then counting equals counting then aggregating", {
  x <- make_daily_linelist()

  count_first <- aggregate_time_units(
    to_count(x, to = "count-incidence"),
    to = "weeks", verbose = FALSE
  )
  aggregate_first <- to_count(
    aggregate_time_units(x, to = "weeks", verbose = FALSE),
    to = "count-incidence"
  )

  expect_equal(sum(count_first$n), sum(aggregate_first$n))
  expect_equal(
    count_first[[get_event_date(count_first)]],
    aggregate_first[[get_event_date(aggregate_first)]]
  )
  expect_equal(count_first$n, aggregate_first$n)
})

test_that("aggregating in steps is NOT the same as aggregating in one go", {
  x <- make_daily_counts("count-incidence")
  direct <- aggregate_time_units(x, to = "months", verbose = FALSE)
  stepped <- aggregate_time_units(
    aggregate_time_units(x, to = "weeks", verbose = FALSE),
    to = "months", verbose = FALSE
  )

  # Cases are never created or lost either way ...
  expect_equal(sum(direct$n), sum(stepped$n))
  expect_equal(sum(direct$n), sum(x$n))

  # ... but the buckets differ, because the epi week containing 2024-01-01
  # STARTS on 2023-12-31, and a second aggregation sees only that start date.
  # Weeks do not nest inside months, so the order matters.
  expect_equal(direct[[get_event_date(direct)]], as.Date("2024-01-01"))
  expect_equal(
    stepped[[get_event_date(stepped)]],
    as.Date(c("2023-12-01", "2024-01-01"))
  )
})

test_that("an NA count is not silently turned into a zero", {
  df <- data.frame(
    event = as.Date("2024-01-01") + c(0, 1, 2, 8),
    report = as.Date("2024-01-03") + c(0, 1, 2, 8),
    n = c(2, NA, 3, 4)
  )
  x <- suppressWarnings(tbl_now(df,
    event_date = event, report_date = report, case_count = n,
    data_type = "count-incidence", units = "days", verbose = FALSE
  ))
  out <- suppressWarnings(aggregate_time_units(x, to = "weeks", verbose = FALSE))

  # An `NA` cell means "not yet observed", so a week containing one has an
  # unknown total rather than a total that quietly omits it.
  expect_true(is.na(out$n[1]))
  expect_equal(out$n[2], 4)
})

test_that("an undeclared column is pooled away, as to_count() documents", {
  df <- data.frame(
    event = as.Date("2024-01-01") + c(0, 1),
    report = as.Date("2024-01-03") + c(0, 1),
    n = c(2, 3),
    ward = c("A", "B")
  )
  x <- tbl_now(df,
    event_date = event, report_date = report, case_count = n,
    data_type = "count-incidence", units = "days", verbose = FALSE
  )
  out <- aggregate_time_units(x, to = "weeks", verbose = FALSE)

  expect_false("ward" %in% colnames(out))
  expect_equal(sum(out$n), 5)

  # Declaring it keeps it, and keeps the cells apart.
  declared <- add_strata(x, "ward")
  kept <- aggregate_time_units(declared, to = "weeks", verbose = FALSE)
  expect_true("ward" %in% colnames(kept))
  expect_equal(nrow(kept), 2)
})

test_that("a `now` set beyond the data still moves onto the new grid", {
  x <- make_daily_linelist() |>
    change_now(as.Date("2024-03-15"))
  out <- aggregate_time_units(x, to = "months", verbose = FALSE)

  expect_equal(get_now(out), as.Date("2024-03-01"))
  expect_true(all(out[[get_report_date(out)]] <= get_now(out)))
})

test_that("`now` is never dragged below a date the aggregation left alone", {
  x <- make_validated()
  original_now <- get_now(x)
  out <- suppressWarnings(
    aggregate_time_units(x, to = "weeks", axes = "validation", verbose = FALSE)
  )
  # Only the validation axis moved, and it moved backwards, so `now` must stay
  # where the untouched report axis put it.
  expect_gte(get_now(out), max(out[[get_report_date(out)]], na.rm = TRUE))
  expect_equal(get_now(out), original_now)
})

test_that("an object built from a delay column aggregates like any other", {
  df <- data.frame(
    reported = as.Date("2024-01-05") + 0:9,
    d = c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1)
  )
  x <- tbl_now(df,
    report_date = reported, delay = d, units = "days", verbose = FALSE
  )
  out <- aggregate_time_units(x, to = "weeks", verbose = FALSE)

  expect_equal(get_event_units(out), "weeks")
  expect_true(all(out$.delay >= 0))
})

test_that("summary() and diagnose() still run on an aggregated object", {
  out <- aggregate_time_units(make_daily_counts(), to = "weeks", verbose = FALSE)
  expect_s3_class(summary(out), "tbl_df")
  expect_s3_class(diagnose(out), "tbl_df")
})

test_that("complete_zeroes() fills the coarser grid", {
  out <- aggregate_time_units(make_daily_counts(), to = "weeks", verbose = FALSE)
  filled <- complete_zeroes(out)

  expect_true(is_tbl_now(filled))
  expect_gte(nrow(filled), nrow(out))
  expect_equal(sum(filled$n, na.rm = TRUE), sum(out$n, na.rm = TRUE))
})

test_that("a real, messy daily line list aggregates to weeks without losing cases", {
  skip_if_not(exists("hai_bucaramanga"), "dataset not available")
  data(hai_bucaramanga, envir = environment())
  x <- suppressWarnings(tbl_now(hai_bucaramanga,
    event_date = specimen_date, report_date = report_date, strata = sex,
    data_type = "linelist", units = "days", verbose = FALSE
  ))

  weekly <- suppressWarnings(aggregate_time_units(x, to = "weeks", verbose = FALSE))

  # A line list keeps one row per case whatever the grid.
  expect_equal(nrow(weekly), nrow(x))
  expect_equal(get_event_units(weekly), "weeks")
  expect_equal(get_report_units(weekly), "weeks")
  expect_equal(sum(suppressWarnings(to_count(weekly, "count-incidence"))$n), nrow(x))
})

test_that("a weekly shipped dataset aggregates up to months and years", {
  data(denguedat, envir = environment())
  x <- tbl_now(denguedat,
    event_date = onset_week, report_date = report_week, strata = gender,
    verbose = FALSE
  )
  counts <- to_count(x, to = "count-incidence")

  monthly <- aggregate_time_units(counts, to = "months", verbose = FALSE)
  expect_equal(sum(monthly$n), sum(counts$n))
  expect_equal(get_event_units(monthly), "months")

  yearly <- aggregate_time_units(monthly, to = "years", verbose = FALSE)
  expect_equal(sum(yearly$n), sum(counts$n))
  expect_equal(get_event_units(yearly), "years")
  # Every date is a 1 January.
  expect_true(all(format(yearly[[get_event_date(yearly)]], "%m-%d") == "01-01"))
})

test_that("aggregation makes a sparse daily grid denser, which is the point", {
  set.seed(1)
  df <- data.frame(
    event = as.Date("2024-01-01") + sample(0:120, 200, replace = TRUE)
  )
  df$report <- df$event + sample(0:6, 200, replace = TRUE)
  x <- tbl_now(df,
    event_date = event, report_date = report,
    data_type = "linelist", units = "days", verbose = FALSE
  )

  daily_cells <- nrow(to_count(x, to = "count-incidence"))
  weekly_cells <- nrow(to_count(
    aggregate_time_units(x, to = "weeks", verbose = FALSE),
    to = "count-incidence"
  ))

  expect_lt(weekly_cells, daily_cells)
})

test_that("aggregation composes with censoring in either order", {
  x <- make_daily_counts("count-incidence")

  censored_first <- aggregate_time_units(
    censor_reporting_delays(x, .delay > 1, verbose = FALSE),
    to = "weeks", verbose = FALSE
  )
  aggregated_first <- censor_reporting_delays(
    aggregate_time_units(x, to = "weeks", verbose = FALSE),
    .delay > 0,
    verbose = FALSE
  )

  expect_true(is_tbl_now(censored_first))
  expect_true(is_tbl_now(aggregated_first))
  expect_equal(sum(censored_first$n), sum(x$n))
  expect_equal(sum(aggregated_first$n), sum(x$n))
  # Censoring first keeps the flag as a declared column, so its cells stay apart.
  expect_equal(get_is_censored_report(censored_first), ".is_censored_report")
})

# ---- Third pass: the cells that must stay apart ----------------------------

test_that("a censoring flag keeps cells apart instead of being pooled away", {
  df <- data.frame(
    event = as.Date("2024-01-01") + c(0, 1, 2),
    report = as.Date("2024-01-03") + c(0, 1, 2),
    n = c(2, 3, 4),
    flag = c(TRUE, FALSE, FALSE)
  )
  x <- tbl_now(df,
    event_date = event, report_date = report, case_count = n,
    is_censored_report = flag, data_type = "count-incidence", units = "days",
    verbose = FALSE
  )
  out <- aggregate_time_units(x, to = "weeks", verbose = FALSE)

  # One week, but two cells: the flag distinguishes them.
  expect_equal(nrow(out), 2)
  expect_equal(get_is_censored_report(out), "flag")
  expect_setequal(out$n, c(2, 7))
  expect_equal(sum(out$n), sum(df$n))
})

test_that("a validation outcome keeps cells apart, so a retraction is not netted", {
  df <- data.frame(
    event = as.Date("2024-01-01") + c(0, 0, 1),
    report = as.Date("2024-01-02") + c(0, 0, 1),
    resolved = as.Date("2024-01-04") + c(0, 0, 1),
    outcome = c("confirmed", "retracted", "confirmed"),
    n = c(5, 2, 3)
  )
  x <- tbl_now(df,
    event_date = event, report_date = report, case_count = n,
    validation_date = resolved, validation_type = outcome,
    data_type = "count-incidence", units = "days", verbose = FALSE
  )
  out <- aggregate_time_units(x, to = "weeks", verbose = FALSE)

  # The confirmed cases pool; the retraction stays its own row.
  expect_equal(nrow(out), 2)
  expect_setequal(out$n, c(8, 2))
  expect_equal(sum(out$n[out[["outcome"]] == "confirmed"]), 8)
})

test_that("a stratified cumulative series accumulates within each stratum", {
  df <- data.frame(
    event = rep(as.Date("2024-01-01") + c(0, 8), each = 4),
    report = as.Date("2024-01-01") + c(0, 1, 0, 1, 8, 9, 8, 9),
    sex = rep(rep(c("F", "M"), each = 2), 2),
    n = c(1, 4, 2, 5, 3, 6, 1, 8)
  )
  x <- tbl_now(df,
    event_date = event, report_date = report, case_count = n, strata = sex,
    data_type = "count-cumulative", units = "days", verbose = FALSE
  )
  out <- aggregate_time_units(x, to = "weeks", verbose = FALSE)

  expect_equal(get_data_type(out), "count-cumulative")
  expect_equal(nrow(out), 4)
  # Each (week, stratum) keeps the last cumulative total it reached.
  totals <- stats::setNames(
    out$n,
    paste(format(out[[get_event_date(out)]]), out[["sex"]])
  )
  expect_equal(unname(totals[["2023-12-31 F"]]), 4)
  expect_equal(unname(totals[["2023-12-31 M"]]), 5)
  expect_equal(unname(totals[["2024-01-07 F"]]), 6)
  expect_equal(unname(totals[["2024-01-07 M"]]), 8)
})

# ---- The attributes 0.29.0 added --------------------------------------------

test_that("validation_levels and is_censored_validation survive aggregation", {
  cases <- data.frame(
    onset = as.Date("2021-01-04") + 0:9,
    visit = as.Date("2021-01-05") + 0:9,
    result = as.Date("2021-01-06") + 0:9,
    outcome = c(rep("confirmado", 8), "retractado", "pendiente")
  )
  levels_map <- c(
    confirmado = "confirmed", retractado = "retracted", pendiente = "pending"
  )
  flu <- tbl_now(cases,
    event_date = onset, report_date = visit,
    validation_date = result, validation_type = outcome,
    validation_levels = levels_map,
    data_type = "linelist", units = "days", verbose = FALSE
  )
  flagged <- censor_validation_delays_above(flu, 0, verbose = FALSE)
  expect_equal(get_is_censored_validation(flagged), ".is_censored_validation")

  out <- aggregate_time_units(flagged, to = "weeks", verbose = FALSE)

  # Every rebuild is a place an attribute can be dropped in silence.
  expect_equal(get_validation_levels(out), levels_map)
  expect_equal(get_is_censored_validation(out), ".is_censored_validation")
  expect_equal(
    out[[".is_censored_validation"]],
    flagged[[".is_censored_validation"]]
  )
  expect_equal(get_validation_units(out), "weeks")
})
