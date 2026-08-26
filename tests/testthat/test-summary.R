# Tests for summary.tbl_now() and its component functions.
#
# Every expectation in this file is computed BY HAND from a four-row fixture,
# and the arithmetic is written out in the comments. The point is that a reader
# can check the numbers without running the code -- so please keep the fixture
# small enough that they still can.

# The fixture ------------------------------------------------------------------
#
#   event        report       gender   n
#   2024-01-01   2024-01-01   F        2     delay 0
#   2024-01-01   2024-01-03   M        1     delay 2
#   2024-01-02   2024-01-04   M        3     delay 2
#   2024-01-05   2024-01-05   F        4     delay 0
#
#   now = 2024-01-05, daily units, 10 cases in total.
#
#   Event grid  (01-01 .. 01-05):  3, 3, 0, 0, 4
#   Report grid (01-01 .. 01-05):  2, 0, 1, 3, 4
#     F on the event grid:         2, 0, 0, 0, 4
#     M on the event grid:         1, 3, 0, 0, 0
#     F on the report grid:        2, 0, 0, 0, 4
#     M on the report grid:        0, 0, 1, 3, 0

fixture_frame <- function() {
  data.frame(
    onset  = as.Date(c("2024-01-01", "2024-01-01", "2024-01-02", "2024-01-05")),
    report = as.Date(c("2024-01-01", "2024-01-03", "2024-01-04", "2024-01-05")),
    gender = c("F", "M", "M", "F"),
    n      = c(2L, 1L, 3L, 4L)
  )
}

fixture_plain <- function() {
  tbl_now(fixture_frame(),
    event_date = "onset", report_date = "report", case_count = "n",
    data_type = "count-incidence", now = as.Date("2024-01-05"), verbose = FALSE
  )
}

fixture_strata <- function() {
  tbl_now(fixture_frame(),
    event_date = "onset", report_date = "report", case_count = "n",
    strata = "gender", data_type = "count-incidence",
    now = as.Date("2024-01-05"), verbose = FALSE
  )
}

# The same ten cases, one row each, so the count and line-list paths must agree.
fixture_linelist <- function() {
  frame <- fixture_frame()
  expanded <- frame[rep(seq_len(nrow(frame)), frame$n), c("onset", "report", "gender")]
  rownames(expanded) <- NULL
  tbl_now(expanded,
    event_date = "onset", report_date = "report", strata = "gender",
    data_type = "linelist", now = as.Date("2024-01-05"), verbose = FALSE
  )
}

# One row of a summary, as a plain list, so expectations read one field at a time.
pick <- function(result, component, quantity, stratum = "all") {
  row <- result[result$component == component &
                  result$quantity == quantity &
                  result$stratum == stratum, ]
  expect_equal(nrow(row), 1)
  as.list(row)
}

# Schema -----------------------------------------------------------------------

test_that("summary() returns the documented schema, in order", {
  result <- summary(fixture_plain())

  expect_s3_class(result, "tbl_df")
  expect_identical(
    names(result),
    c(
      "component", "quantity", "stratum", "n", "total",
      "mean", "sd", "min", "q25", "q50", "q75", "q90", "max",
      "prop_zero", "prop", "value", "date_min", "date_max"
    )
  )
  expect_true(all(result$stratum == "all"))
})

test_that("summary() is a generic dispatching on tbl_now", {
  expect_true("summary.tbl_now" %in% as.character(utils::methods("summary")))
  expect_false(identical(
    summary(fixture_plain()),
    summary(as.data.frame(fixture_plain()))
  ))
})

test_that("component functions return the same schema as summary()", {
  schema <- names(summary(fixture_plain()))
  for (result in list(
    cases_per_date(fixture_plain()),
    delay_summary(fixture_plain()),
    zero_run_summary(fixture_plain()),
    case_autocorrelation(fixture_plain()),
    triangle_occupancy(fixture_plain()),
    reporting_completeness(fixture_plain())
  )) {
    expect_true(all(names(result) %in% schema))
    expect_identical(names(result), intersect(schema, names(result)))
  }
})

test_that("summary() is the bind_rows of its components", {
  x <- fixture_plain()
  whole <- summary(x)
  parts <- dplyr::bind_rows(
    cases_per_date(x, axis = "event"),
    cases_per_date(x, axis = "report"),
    delay_summary(x, delay = "event_to_report")
  )
  for (index in seq_len(nrow(parts))) {
    row <- parts[index, ]
    matched <- whole[whole$component == row$component &
                       whole$quantity == row$quantity &
                       whole$stratum == row$stratum, ]
    expect_equal(nrow(matched), 1)
    expect_equal(matched$mean, row$mean)
    expect_equal(matched$sd, row$sd)
    expect_equal(matched$total, row$total)
  }
})

# Cases per date ---------------------------------------------------------------

test_that("cases per event date match the hand-computed grid", {
  # Grid 3, 3, 0, 0, 4. Sum 10, mean 10/5 = 2.
  # sd = sqrt((1 + 1 + 4 + 4 + 4) / 4) = sqrt(3.5)
  # Sorted 0, 0, 3, 3, 4 with equal weights; cumulative shares .2 .4 .6 .8 1
  #   min  -> 0    q25 (first >= .25 is .4) -> 0    q50 (.6) -> 3
  #   q75 (.8) -> 3    q90 (1) -> 4    max -> 4
  row <- pick(summary(fixture_plain()), "cases", "per_event_date")

  expect_equal(row$n, 5L)
  expect_equal(row$total, 10)
  expect_equal(row$mean, 2)
  expect_equal(row$sd, sqrt(3.5))
  expect_equal(row$min, 0)
  expect_equal(row$q25, 0)
  expect_equal(row$q50, 3)
  expect_equal(row$q75, 3)
  expect_equal(row$q90, 4)
  expect_equal(row$max, 4)
  expect_equal(row$prop_zero, 0.4)
})

test_that("cases per report date match the hand-computed grid", {
  # Grid 2, 0, 1, 3, 4. Sum 10, mean 2.
  # sd = sqrt((0 + 4 + 1 + 1 + 4) / 4) = sqrt(2.5)
  # Sorted 0, 1, 2, 3, 4 -> min 0, q25 1, q50 2, q75 3, q90 4, max 4
  row <- pick(summary(fixture_plain()), "cases", "per_report_date")

  expect_equal(row$n, 5L)
  expect_equal(row$total, 10)
  expect_equal(row$mean, 2)
  expect_equal(row$sd, sqrt(2.5))
  expect_equal(unlist(row[c("min", "q25", "q50", "q75", "q90", "max")]),
               c(min = 0, q25 = 1, q50 = 2, q75 = 3, q90 = 4, max = 4))
  expect_equal(row$prop_zero, 0.2)
})

test_that("the date grid runs to now, not to the last row", {
  # Nothing is reported after 2024-01-03, but now is 2024-01-08, so the grid
  # is eight days long and the last five of them are zeros.
  late <- tbl_now(
    data.frame(
      onset  = as.Date(c("2024-01-01", "2024-01-03")),
      report = as.Date(c("2024-01-01", "2024-01-03")),
      n      = c(1L, 1L)
    ),
    event_date = "onset", report_date = "report", case_count = "n",
    data_type = "count-incidence", now = as.Date("2024-01-08"), verbose = FALSE
  )
  row <- pick(summary(late), "cases", "per_event_date")

  expect_equal(row$n, 8L)          # 01-01 .. 01-08
  expect_equal(row$total, 2)
  expect_equal(row$prop_zero, 6 / 8)
})

test_that("cases per event date are stratified on the shared grid", {
  result <- summary(fixture_strata())

  # F: 2, 0, 0, 0, 4 -> total 6, mean 1.2,
  #    sd = sqrt((0.64 + 3 * 1.44 + 7.84) / 4) = sqrt(3.2)
  #    sorted 0, 0, 0, 2, 4 -> min 0, q25 0, q50 0, q75 2, q90 4, max 4
  female <- pick(result, "cases", "per_event_date", "F")
  expect_equal(female$n, 5L)
  expect_equal(female$total, 6)
  expect_equal(female$mean, 1.2)
  expect_equal(female$sd, sqrt(3.2))
  expect_equal(unlist(female[c("min", "q25", "q50", "q75", "q90", "max")]),
               c(min = 0, q25 = 0, q50 = 0, q75 = 2, q90 = 4, max = 4))
  expect_equal(female$prop_zero, 0.6)

  # M: 1, 3, 0, 0, 0 -> total 4, mean 0.8,
  #    sd = sqrt((0.04 + 4.84 + 3 * 0.64) / 4) = sqrt(1.7)
  male <- pick(result, "cases", "per_event_date", "M")
  expect_equal(male$total, 4)
  expect_equal(male$mean, 0.8)
  expect_equal(male$sd, sqrt(1.7))
  expect_equal(male$prop_zero, 0.6)

  # Both strata are measured on the same five-day grid, and they add up.
  expect_equal(female$n, male$n)
  expect_equal(female$total + male$total,
               pick(result, "cases", "per_event_date")$total)
})

test_that("by_strata = FALSE drops the per-stratum rows", {
  result <- summary(fixture_strata(), by_strata = FALSE)
  expect_true(all(result$stratum == "all"))
  expect_equal(
    pick(result, "cases", "per_event_date")$mean,
    pick(summary(fixture_plain()), "cases", "per_event_date")$mean
  )
})

# Delays -----------------------------------------------------------------------

test_that("the delay distribution is weighted by the case counts", {
  # Delays 0 (2 cases), 2 (1), 2 (3), 0 (4): six 0s and four 2s.
  #   mean = 8 / 10 = 0.8
  #   sd   = sqrt((6 * 0.64 + 4 * 1.44) / 9) = sqrt(9.6 / 9)
  # Sorted 0 (weight 6) then 2 (weight 4); cumulative shares .6 then 1
  #   min 0, q25 0, q50 0 (60% of cases arrived same day), q75 2, q90 2, max 2
  row <- pick(summary(fixture_plain()), "delay", "event_to_report")

  expect_equal(row$n, 4L)          # four data rows
  expect_equal(row$total, 10)      # ten cases
  expect_equal(row$mean, 0.8)
  expect_equal(row$sd, sqrt(9.6 / 9))
  expect_equal(unlist(row[c("min", "q25", "q50", "q75", "q90", "max")]),
               c(min = 0, q25 = 0, q50 = 0, q75 = 2, q90 = 2, max = 2))
})

test_that("the weighted delay statistics equal the expanded line list's", {
  # The same ten cases, one row each: the weighting must be exactly equivalent
  # to expanding the counts, which is what the documentation promises.
  counts <- pick(delay_summary(fixture_strata()), "delay", "event_to_report")
  cases <- pick(delay_summary(fixture_linelist()), "delay", "event_to_report")

  expect_equal(counts$mean, cases$mean)
  expect_equal(counts$sd, cases$sd)
  expect_equal(counts$total, cases$total)
  expect_equal(
    unlist(counts[c("min", "q25", "q50", "q75", "q90", "max")]),
    unlist(cases[c("min", "q25", "q50", "q75", "q90", "max")])
  )

  # ... and it is the ordinary sd of the ten expanded delays.
  expect_equal(counts$sd, stats::sd(c(rep(0, 6), rep(2, 4))))
})

test_that("per-stratum delays are hand-computable", {
  result <- delay_summary(fixture_strata())

  # F: both rows have delay 0 -> mean 0, sd 0, six cases.
  female <- pick(result, "delay", "event_to_report", "F")
  expect_equal(female$mean, 0)
  expect_equal(female$sd, 0)
  expect_equal(female$total, 6)

  # M: both rows have delay 2 -> mean 2, sd 0, four cases.
  male <- pick(result, "delay", "event_to_report", "M")
  expect_equal(male$mean, 2)
  expect_equal(male$sd, 0)
  expect_equal(male$total, 4)
})

test_that("delay_summary() refuses count-cumulative data", {
  cumulative <- to_count(fixture_plain(), to = "count-cumulative")
  expect_error(delay_summary(cumulative), "not additive across delays")
})

test_that("summary() of count-cumulative data trades delays for growth", {
  result <- summary(to_count(fixture_plain(), to = "count-cumulative"))
  expect_false("delay" %in% result$component)
  expect_true("growth" %in% result$component)
})

# Zero runs --------------------------------------------------------------------

test_that("zero-run lengths count consecutive zero dates", {
  # Event grid 3, 3, 0, 0, 4 -> exactly one run, of length 2.
  event <- pick(summary(fixture_plain()), "zero_run", "event_date")
  expect_equal(event$n, 1L)        # one run
  expect_equal(event$total, 2)     # two zero dates
  expect_equal(event$mean, 2)
  expect_equal(event$sd, NA_real_) # a single run has no spread

  # Report grid 2, 0, 1, 3, 4 -> one run, of length 1.
  report <- pick(summary(fixture_plain()), "zero_run", "report_date")
  expect_equal(report$n, 1L)
  expect_equal(report$total, 1)
  expect_equal(report$mean, 1)
})

test_that("zero runs split correctly when there are two of them", {
  # M on the report grid is 0, 0, 1, 3, 0: runs of length 2 and 1.
  #   mean = 1.5, sd = sqrt(((2 - 1.5)^2 + (1 - 1.5)^2) / 1) = sqrt(0.5)
  #   sorted 1, 2 -> min 1, q25 1, q50 1, q75 2, q90 2, max 2
  row <- pick(zero_run_summary(fixture_strata(), axis = "report"),
              "zero_run", "report_date", "M")

  expect_equal(row$n, 2L)
  expect_equal(row$total, 3)
  expect_equal(row$mean, 1.5)
  expect_equal(row$sd, sqrt(0.5))
  expect_equal(unlist(row[c("min", "q25", "q50", "q75", "q90", "max")]),
               c(min = 1, q25 = 1, q50 = 1, q75 = 2, q90 = 2, max = 2))
})

test_that("a series with no zeros reports no runs", {
  dense <- tbl_now(
    data.frame(
      onset  = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
      report = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
      n      = c(1L, 2L, 3L)
    ),
    event_date = "onset", report_date = "report", case_count = "n",
    data_type = "count-incidence", now = as.Date("2024-01-03"), verbose = FALSE
  )
  row <- pick(zero_run_summary(dense), "zero_run", "event_date")

  expect_equal(row$n, 0L)
  expect_equal(row$total, 0)
  expect_equal(row$mean, NA_real_)
})

# Autocorrelation --------------------------------------------------------------

test_that("lag-1 autocorrelation is the lagged-pair correlation", {
  # Event grid 3, 3, 0, 0, 4.
  #   head = 3, 3, 0, 0 (mean 1.5)   tail = 3, 0, 0, 4 (mean 1.75)
  #   sum of products = 1.875 - 2.625 + 2.625 - 3.375 = -1.5
  #   cor = (-1.5 / 3) / (sqrt(9 / 3) * sqrt(12.75 / 3)) = -0.5 / sqrt(12.75)
  row <- pick(summary(fixture_plain()), "autocorrelation", "per_event_date lag 1")

  expect_equal(row$n, 4L)
  expect_equal(row$value, -0.5 / sqrt(12.75))
  expect_equal(row$value, stats::cor(c(3, 3, 0, 0), c(3, 0, 0, 4)))
})

test_that("autocorrelation accepts several lags and axes", {
  result <- case_autocorrelation(fixture_plain(), lags = c(1, 2), axis = "report")

  expect_equal(nrow(result), 2)
  # Report grid 2, 0, 1, 3, 4: lag 2 pairs (2, 0, 1) with (1, 3, 4).
  lag_two <- pick(result, "autocorrelation", "per_report_date lag 2")
  expect_equal(lag_two$n, 3L)
  expect_equal(lag_two$value, stats::cor(c(2, 0, 1), c(1, 3, 4)))
})

test_that("a constant series has no autocorrelation to report", {
  flat <- tbl_now(
    data.frame(
      onset  = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
      report = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
      n      = c(2L, 2L, 2L)
    ),
    event_date = "onset", report_date = "report", case_count = "n",
    data_type = "count-incidence", now = as.Date("2024-01-03"), verbose = FALSE
  )
  expect_equal(
    pick(case_autocorrelation(flat), "autocorrelation", "per_event_date lag 1")$value,
    NA_real_
  )
})

# Composition ------------------------------------------------------------------

test_that("prop_strata() splits the cases between the strata", {
  result <- prop_strata(fixture_strata())

  expect_equal(nrow(result), 2)
  expect_equal(pick(result, "composition", "strata = F")$prop, 0.6)  # 6 / 10
  expect_equal(pick(result, "composition", "strata = M")$prop, 0.4)  # 4 / 10
  expect_equal(sum(result$prop), 1)
  expect_true(all(result$stratum == "all"))
})

test_that("prop_censored() reports the case-weighted censored share", {
  # Censor the two rows carrying 1 and 3 cases: 4 of 10 cases, 2 of 4 rows.
  censored <- tbl_now(
    cbind(fixture_frame(), flagged = c(FALSE, TRUE, TRUE, FALSE)),
    event_date = "onset", report_date = "report", case_count = "n",
    is_censored = "flagged", data_type = "count-incidence",
    now = as.Date("2024-01-05"), verbose = FALSE
  )
  row <- pick(prop_censored(censored), "composition", "censored")

  expect_equal(row$n, 2L)
  expect_equal(row$total, 4)
  expect_equal(row$prop, 0.4)

  # And the censored cases get their own per-event-date row: 0, 1, 3, 0, 0.
  cases <- pick(cases_per_date(censored), "cases", "censored_per_event_date")
  expect_equal(cases$total, 4)
  expect_equal(cases$prop_zero, 0.6)
})

test_that("prop_censored() is absent when the object has no flag", {
  expect_equal(nrow(prop_censored(fixture_plain())), 0)
  expect_false("censored" %in% summary(fixture_plain())$quantity)
})

test_that("prop_confirmation_type() splits the cases between the outcomes", {
  # 2 confirmed, 1 retracted, 3 pending, 4 confirmed -> 6 confirmed, 1
  # retracted, 3 pending out of ten.
  confirmed <- tbl_now(
    cbind(
      fixture_frame(),
      checked = as.Date(c("2024-01-02", "2024-01-04", NA, "2024-01-05")),
      outcome = c("confirmed", "retracted", "pending", "confirmed")
    ),
    event_date = "onset", report_date = "report", case_count = "n",
    confirmation_date = "checked", confirmation_type = "outcome",
    data_type = "count-incidence", now = as.Date("2024-01-05"), verbose = FALSE
  )
  result <- prop_confirmation_type(confirmed)

  expect_equal(pick(result, "composition", "confirmation_type = confirmed")$prop, 0.6)
  expect_equal(pick(result, "composition", "confirmation_type = retracted")$prop, 0.1)
  expect_equal(pick(result, "composition", "confirmation_type = pending")$prop, 0.3)
  expect_equal(sum(result$prop), 1)

  # A pending case has no confirmation date, so it must not be counted as an
  # arrival on the confirmation axis. A RETRACTED one does have a date -- the
  # laboratory answered, it just answered no -- so the axis carries
  # 2 + 1 + 4 = 7 cases, not all ten and not only the six confirmed.
  axis <- pick(cases_per_date(confirmed, axis = "confirmation"),
               "cases", "per_confirmation_date")
  expect_equal(axis$total, 7)

  # With more than one outcome present the axis is also split by outcome.
  by_type <- cases_per_date(confirmed, axis = "confirmation")
  expect_equal(
    pick(by_type, "cases", "per_confirmation_date [confirmed]")$total, 6
  )
  expect_equal(
    pick(by_type, "cases", "per_confirmation_date [retracted]")$total, 1
  )

  # The laboratory turnaround is measured FROM THE REPORT: confirmed cases wait
  # 01-01 -> 01-02 (1 day, 2 cases) and 01-05 -> 01-05 (0 days, 4 cases).
  turnaround <- pick(
    delay_summary(confirmed, delay = "report_to_confirmation"),
    "delay", "report_to_confirmation [confirmed]"
  )
  expect_equal(turnaround$total, 6)
  expect_equal(turnaround$mean, (2 * 1 + 4 * 0) / 6)
})

test_that("prop_covariate_levels() reports categorical covariates only", {
  covariates <- tbl_now(
    cbind(fixture_frame(), setting = c("urban", "rural", "rural", "urban"),
          temperature = c(20, 21, 22, 23)),
    event_date = "onset", report_date = "report", case_count = "n",
    covariates = c("setting", "temperature"), data_type = "count-incidence",
    now = as.Date("2024-01-05"), verbose = FALSE
  )
  result <- prop_covariate_levels(covariates)

  # urban carries 2 + 4 = 6 cases, rural 1 + 3 = 4.
  expect_equal(
    pick(result, "composition", "covariate: setting = urban")$prop, 0.6
  )
  expect_equal(
    pick(result, "composition", "covariate: setting = rural")$prop, 0.4
  )
  # The numeric covariate is skipped: one row per value describes nothing.
  expect_false(any(grepl("temperature", result$quantity)))
})

# Coverage ---------------------------------------------------------------------

test_that("date_ranges() reports the totals, the ranges and now", {
  result <- date_ranges(fixture_plain())

  totals <- pick(result, "coverage", "total_cases")
  expect_equal(totals$n, 4L)       # four data rows
  expect_equal(totals$total, 10)   # ten cases

  event <- pick(result, "coverage", "event_date")
  expect_equal(event$n, 3L)        # 01-01, 01-02, 01-05
  expect_equal(event$date_min, as.Date("2024-01-01"))
  expect_equal(event$date_max, as.Date("2024-01-05"))

  report <- pick(result, "coverage", "report_date")
  expect_equal(report$n, 4L)       # 01-01, 01-03, 01-04, 01-05
  expect_equal(report$date_min, as.Date("2024-01-01"))
  expect_equal(report$date_max, as.Date("2024-01-05"))

  now_row <- pick(result, "coverage", "now")
  expect_equal(now_row$date_min, as.Date("2024-01-05"))
  expect_equal(now_row$date_max, as.Date("2024-01-05"))
})

test_that("triangle occupancy counts the cells that could have arrived", {
  # Widest delay is 2. The five event dates have 4, 3, 2, 1 and 0 days of room
  # before now, so the reachable cells are 3 + 3 + 3 + 2 + 1 = 12.
  # Four cells carry cases: (01-01, 0), (01-01, 2), (01-02, 2), (01-05, 0).
  result <- triangle_occupancy(fixture_plain())

  expect_equal(pick(result, "coverage", "max_delay")$value, 2)
  expect_equal(pick(result, "coverage", "triangle_cells_observed")$n, 4L)
  expect_equal(pick(result, "coverage", "triangle_cells_possible")$n, 12L)
  expect_equal(pick(result, "coverage", "triangle_occupancy")$value, 4 / 12)
  expect_equal(pick(result, "coverage", "now_gap_event")$value, 0)
  expect_equal(pick(result, "coverage", "now_gap_report")$value, 0)
})

test_that("the occupancy denominator is shared between strata", {
  # F's own delays are all 0, but it is still measured against the same
  # twelve-cell triangle as M, or the two would not be comparable.
  result <- triangle_occupancy(fixture_strata())

  expect_equal(pick(result, "coverage", "triangle_cells_possible", "F")$n, 12L)
  expect_equal(pick(result, "coverage", "triangle_cells_possible", "M")$n, 12L)
  expect_equal(pick(result, "coverage", "triangle_occupancy", "F")$value, 2 / 12)
  expect_equal(pick(result, "coverage", "triangle_occupancy", "M")$value, 2 / 12)

  # M's last event is 01-02 and its last report 01-04, three and one days
  # before now: the per-stratum staleness.
  expect_equal(pick(result, "coverage", "now_gap_event", "M")$value, 3)
  expect_equal(pick(result, "coverage", "now_gap_report", "M")$value, 1)
})

test_that("the now-gap notices a stale object", {
  stale <- tbl_now(
    data.frame(
      onset  = as.Date(c("2024-01-01", "2024-01-02")),
      report = as.Date(c("2024-01-01", "2024-01-02")),
      n      = c(1L, 1L)
    ),
    event_date = "onset", report_date = "report", case_count = "n",
    data_type = "count-incidence", now = as.Date("2024-01-09"), verbose = FALSE
  )
  expect_equal(
    pick(triangle_occupancy(stale), "coverage", "now_gap_report")$value, 7
  )
})

# Completeness -----------------------------------------------------------------

test_that("reporting completeness is the share arrived by each delay", {
  # mature_only trims to now minus the 95th delay percentile (2 days), so only
  # the event dates 01-01 and 01-02 are used. Their eventual totals are 3 and 3.
  #   delay <= 0: 2/3 and 0/3   -> mean 1/3, sd sqrt(2/9), pooled 2/6
  #   delay <= 2: 3/3 and 3/3   -> mean 1, pooled 1
  result <- reporting_completeness(fixture_plain())

  same_day <- pick(result, "completeness", "delay <= 0")
  expect_equal(same_day$n, 2L)
  expect_equal(same_day$mean, 1 / 3)
  expect_equal(same_day$sd, sqrt(2 / 9))
  expect_equal(same_day$prop, 2 / 6)

  complete <- pick(result, "completeness", "delay <= 2")
  expect_equal(complete$mean, 1)
  expect_equal(complete$sd, 0)
  expect_equal(complete$prop, 1)
})

test_that("mature_only = FALSE keeps the immature event dates", {
  # 2024-01-05 is one day old and fully reported, so adding it lifts the
  # same-day share to (2/3 + 0/3 + 4/4) / 3 = 5/9, pooled 6/10.
  result <- reporting_completeness(fixture_plain(), mature_only = FALSE)
  same_day <- pick(result, "completeness", "delay <= 0")

  expect_equal(same_day$n, 3L)
  expect_equal(same_day$mean, 5 / 9)
  expect_equal(same_day$prop, 0.6)
})

test_that("reporting_completeness() honours an explicit delay set", {
  result <- reporting_completeness(fixture_plain(), delays = c(0, 2))
  expect_equal(result$quantity, c("delay <= 0", "delay <= 2"))
})

# Growth -----------------------------------------------------------------------

test_that("cumulative growth is the ratio of consecutive running totals", {
  # Event 01-01 runs 2, 2, 3 over delays 0, 1, 2 -> ratios 1 and 1.5.
  # Event 01-02 has nothing until delay 2, so it has no ratio at delay 1 or 2
  # (dividing out of zero is infinite, not large) and only joins at delay 3.
  # Event 01-05 likewise runs 4, 4, 4.
  result <- cumulative_growth(fixture_plain(), k = 2)

  first <- pick(result, "growth", "delay 1")
  expect_equal(first$n, 2L)        # 01-01 and 01-05
  expect_equal(first$mean, 1)
  expect_equal(first$total, 0)     # nothing new arrived

  second <- pick(result, "growth", "delay 2")
  expect_equal(second$n, 2L)
  expect_equal(second$mean, (1.5 + 1) / 2)
  expect_equal(second$sd, stats::sd(c(1.5, 1)))
  expect_equal(second$total, 1)    # the one case that arrived at delay 2
})

test_that("cumulative_growth() rejects a nonsensical k", {
  expect_error(cumulative_growth(fixture_plain(), k = 0), "positive whole number")
})

# Not-yet-observed cells -------------------------------------------------------

test_that("NA counts are dropped as not-yet-observed, and the drop is reported", {
  # An NA count is a cell that has not been observed yet. It carries no cases,
  # so the totals must match the fixture exactly -- and before this was handled
  # a single NA turned every sum in the table into NA.
  with_na <- tbl_now(
    rbind(fixture_frame(),
          data.frame(onset = as.Date("2024-01-04"),
                     report = as.Date("2024-01-05"),
                     gender = "F", n = NA_integer_)),
    event_date = "onset", report_date = "report", case_count = "n",
    data_type = "count-incidence", now = as.Date("2024-01-05"), verbose = FALSE
  )
  result <- summary(with_na, by_strata = FALSE)

  expect_equal(pick(result, "coverage", "unobserved_cells")$n, 1L)
  expect_equal(pick(result, "coverage", "total_cases")$total, 10)
  expect_equal(pick(result, "cases", "per_event_date")$total, 10)
  expect_equal(pick(result, "cases", "per_event_date")$mean, 2)
  expect_false(any(is.na(result$total[result$component == "cases"])))

  # 2024-01-04 is unobserved, so it stays a zero on the grid rather than
  # becoming a date with cases.
  expect_equal(pick(result, "cases", "per_event_date")$prop_zero, 0.4)
})

test_that("unobserved_cells is zero when everything is observed", {
  expect_equal(
    pick(date_ranges(fixture_plain()), "coverage", "unobserved_cells")$n, 0L
  )
})

# Factor covariates ------------------------------------------------------------

test_that("factor covariates are summarised by level", {
  # The stated case is a factor, not a character: a level with no cases must
  # still not appear, and the shares must come out case-weighted.
  covariates <- tbl_now(
    cbind(fixture_frame(),
          setting = factor(c("urban", "rural", "rural", "urban"),
                           levels = c("urban", "rural", "peri-urban"))),
    event_date = "onset", report_date = "report", case_count = "n",
    covariates = "setting", data_type = "count-incidence",
    now = as.Date("2024-01-05"), verbose = FALSE
  )
  result <- prop_covariate_levels(covariates)

  expect_equal(nrow(result), 2)
  expect_equal(
    pick(result, "composition", "covariate: setting = urban")$prop, 0.6
  )
  expect_equal(
    pick(result, "composition", "covariate: setting = rural")$prop, 0.4
  )
  # An unused factor level describes no cases and gets no row.
  expect_false(any(grepl("peri-urban", result$quantity)))
})

test_that("covariate shares are also computed within each stratum", {
  covariates <- tbl_now(
    cbind(fixture_frame(),
          setting = factor(c("urban", "rural", "rural", "urban"))),
    event_date = "onset", report_date = "report", case_count = "n",
    strata = "gender", covariates = "setting", data_type = "count-incidence",
    now = as.Date("2024-01-05"), verbose = FALSE
  )
  result <- prop_covariate_levels(covariates)

  # F is urban in both its rows, M rural in both.
  expect_equal(
    pick(result, "composition", "covariate: setting = urban", "F")$prop, 1
  )
  expect_equal(
    pick(result, "composition", "covariate: setting = rural", "M")$prop, 1
  )
})

# The confirmation axis in a full summary --------------------------------------

test_that("summary() carries the confirmation blocks when there is a third date", {
  confirmed <- tbl_now(
    cbind(
      fixture_frame(),
      checked = as.Date(c("2024-01-02", "2024-01-04", NA, "2024-01-05")),
      outcome = c("confirmed", "retracted", "pending", "confirmed")
    ),
    event_date = "onset", report_date = "report", case_count = "n",
    confirmation_date = "checked", confirmation_type = "outcome",
    data_type = "count-incidence", now = as.Date("2024-01-05"), verbose = FALSE
  )
  result <- summary(confirmed)

  expect_true("per_confirmation_date" %in% result$quantity)
  expect_true("confirmation_date" %in%
                result$quantity[result$component == "zero_run"])
  expect_true("confirmation_date" %in%
                result$quantity[result$component == "coverage"])

  # Both confirmation delays are present, and they are different quantities:
  # from the event (2 cases wait 1 day, 1 waits 3, 4 wait 0 -> 10/7)
  # versus from the report (2 wait 1, 1 waits 1, 4 wait 0 -> 3/7).
  from_event <- pick(result, "delay", "event_to_confirmation")
  from_report <- pick(result, "delay", "report_to_confirmation")
  expect_equal(from_event$total, 7)
  expect_equal(from_report$total, 7)
  expect_equal(from_event$mean, (2 * 1 + 1 * 3 + 4 * 0) / 7)
  expect_equal(from_report$mean, (2 * 1 + 1 * 1 + 4 * 0) / 7)

  # The confirmation-date range excludes the pending case, which has no date.
  range <- pick(result, "coverage", "confirmation_date")
  expect_equal(range$total, 7)
  expect_equal(range$date_min, as.Date("2024-01-02"))
  expect_equal(range$date_max, as.Date("2024-01-05"))
})

# Line-list equivalence --------------------------------------------------------

test_that("a line list and its counts summarise identically", {
  # The line list cannot represent a zero, so this also proves the grid is
  # built from now rather than from the rows that happen to be present.
  from_counts <- summary(fixture_strata())
  from_cases <- summary(fixture_linelist())

  comparable <- c("cases", "zero_run", "delay", "autocorrelation", "composition")
  left <- from_counts[from_counts$component %in% comparable, ]
  right <- from_cases[from_cases$component %in% comparable, ]

  expect_equal(left$quantity, right$quantity)
  expect_equal(left$stratum, right$stratum)
  expect_equal(left$total, right$total)
  expect_equal(left$mean, right$mean)
  expect_equal(left$sd, right$sd)
  expect_equal(left$prop, right$prop)
  expect_equal(left$prop_zero, right$prop_zero)
})

# Argument checking ------------------------------------------------------------

test_that("the summary functions reject non-tbl_now input", {
  expect_error(cases_per_date(fixture_frame()), "must be a")
  expect_error(delay_summary(fixture_frame()), "must be a")
  expect_error(prop_censored(fixture_frame()), "must be a")
})

test_that("by_strata = TRUE without strata is an error, not a silent pooling", {
  expect_error(prop_strata(fixture_plain()), "no strata")
  expect_error(cases_per_date(fixture_plain(), by_strata = TRUE), "no strata")
})

test_that("bad lags are rejected", {
  expect_error(case_autocorrelation(fixture_plain(), lags = 0), "positive whole")
  expect_error(case_autocorrelation(fixture_plain(), lags = -1), "positive whole")
})
