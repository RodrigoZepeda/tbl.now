# Withdrawn summary components: `case_autocorrelation()` and
# `reporting_completeness()`
# ==============================================================================
#
# This file is NOT part of the package. `devel/` is listed in `.Rbuildignore`,
# so nothing here is installed, documented, or checked.
#
# Both functions were written by an AI and never reviewed by a human. They were
# taken out of `summary()` for that reason (a report a user reads by default
# must not contain a statistic nobody has verified) and kept exported with a
# warning on every call. Before the 1.0.0 CRAN release they were removed from
# the package entirely and parked here, so the work is available if someone
# wants to review it and put it back.
#
# To restore either one:
#
#   1. move the exported function, its `@rdname nowcast_summary_components`
#      roxygen tags, and the internal helpers it calls back into `R/summary.R`;
#   2. restore the `lags` / `delays` / `mature_only` `@param` entries and the
#      bullet list items in the `nowcast_summary_components` block;
#   3. restore the `completeness` / `autocorrelation` entries in
#      `.summary_block_gloss()`;
#   4. add the topic back to `_pkgdown.yml` and move the tests at the bottom of
#      this file back into `tests/testthat/test-summary.R`;
#   5. review the statistic first.
#
# The helpers below depend on the summary context machinery that is still in
# `R/summary.R`: `.summary_context()`, `.summary_finalise()`, `.summary_rows()`,
# `.summary_series()`, `.summary_axis()`, `.summary_stat_row()`,
# `.tbl_now_maturity_threshold()` and `check_bool()`.

# Exported functions -----------------------------------------------------------

#' @rdname nowcast_summary_components
#' @export
case_autocorrelation <- function(x, lags = 1,
                                 axis = c("event", "report", "revision"),
                                 by_strata = NULL, strata = NULL) {
  axis <- match.arg(axis)
  .summary_unreviewed_warning("case_autocorrelation")
  context <- .summary_context(x, by_strata, strata, "case_autocorrelation")
  .summary_finalise(list(.summary_autocorrelation(context, axis, lags)))
}

#' @rdname nowcast_summary_components
#' @export
reporting_completeness <- function(x, delays = NULL, mature_only = TRUE,
                                   by_strata = NULL, strata = NULL) {
  .summary_unreviewed_warning("reporting_completeness")
  context <- .summary_context(x, by_strata, strata, "reporting_completeness")
  .summary_finalise(list(.summary_completeness(context, delays, mature_only)))
}

# Internal helpers -------------------------------------------------------------

#' Warn that a block is unreviewed AI-written code.
#'
#' `case_autocorrelation()` and `reporting_completeness()` were written by an
#' LLM and have not yet been checked by a human, which is why they were taken
#' out of `summary()`: a report a user reads by default must not contain a
#' statistic nobody has verified. They stay exported so the work is not lost,
#' but every call says what they are.
#' @param function_name The calling function, for the message.
#' @keywords internal
#' @noRd
.summary_unreviewed_warning <- function(function_name) {
  cli::cli_warn(
    c(
      "!" = "{.fn {function_name}} is {.emph experimental} and was written by
             an AI; it has not yet been reviewed by a human.",
      "i" = "It is no longer part of {.fn summary}. Check the numbers before
             you rely on them."
    )
    # Deliberately NOT throttled with `.frequency`, unlike the other
    # experimental diagnostics: an unreviewed number must carry its warning
    # every time it is produced, including inside a loop or a report.
  )
  invisible(NULL)
}

#' Lagged autocorrelation of the case series
#'
#' @param context A summary context.
#' @param axis The axis.
#' @param lags Integer vector of lags.
#'
#' @return A tibble of `"autocorrelation"` rows.
#'
#' @keywords internal
#' @noRd
.summary_autocorrelation <- function(context, axis, lags) {
  if (is.null(.summary_axis(context, axis))) return(NULL)
  lags <- as.integer(lags)
  if (length(lags) == 0 || any(is.na(lags)) || any(lags < 1)) {
    cli::cli_abort("{.arg lags} must be positive whole numbers.")
  }

  rows <- lapply(context$labels, function(label) {
    series <- .summary_series(context, axis, .summary_rows(context, label))
    dplyr::bind_rows(lapply(lags, function(lag) {
      pairs <- .summary_lagged_correlation(series, lag)
      dplyr::tibble(
        component = "autocorrelation",
        quantity = paste0("per_", axis, "_date lag ", lag),
        stratum = label,
        n = as.integer(pairs$n),
        value = pairs$value
      )
    }))
  })

  dplyr::bind_rows(rows)
}

#' Pearson correlation between a series and its own lag
#'
#' Computed as `cor(y[1:(n - lag)], y[(1 + lag):n])`: the correlation of the
#' lagged pairs. This is **not** the same as [stats::acf()], whose estimator
#' divides by the full series length and centres both halves on the full-series
#' mean. The lagged-pair form is the one a reader can reproduce by hand, and
#' the difference is negligible except on very short series.
#'
#' @param series Numeric vector, or `NULL`.
#' @param lag Positive integer.
#'
#' @return A list with `n` (number of pairs) and `value`.
#'
#' @keywords internal
#' @noRd
.summary_lagged_correlation <- function(series, lag) {
  if (is.null(series) || length(series) <= lag + 1) {
    return(list(n = 0L, value = NA_real_))
  }
  head_values <- series[seq_len(length(series) - lag)]
  tail_values <- series[seq(lag + 1, length(series))]
  if (stats::sd(head_values) == 0 || stats::sd(tail_values) == 0) {
    return(list(n = length(head_values), value = NA_real_))
  }
  list(
    n = length(head_values),
    value = stats::cor(head_values, tail_values)
  )
}

#' Share of each event date's eventual total that had arrived by delay `d`
#'
#' @param context A summary context.
#' @param delays Integer vector of delays, or `NULL` for all observed ones.
#' @param mature_only Logical.
#'
#' @return A tibble of `"completeness"` rows.
#'
#' @keywords internal
#' @noRd
.summary_completeness <- function(context, delays, mature_only) {
  cases <- context$cases
  observed <- cases$event_to_report[!is.na(cases$event_to_report) &
                                      cases$count > 0]
  if (length(observed) == 0) return(NULL)

  if (is.null(delays)) {
    delays <- seq(0, max(observed))
  }
  delays <- sort(unique(as.integer(delays)))
  delays <- delays[delays >= 0 & delays <= max(observed)]
  if (length(delays) == 0) return(NULL)

  check_bool(mature_only, "mature_only")
  cutoff <- if (isTRUE(mature_only)) {
    .tbl_now_maturity_threshold(
      context$x,
      dplyr::tibble(delay = cases$event_to_report, weight = cases$count),
      0.95
    )
  } else {
    NA
  }

  rows <- lapply(context$labels, function(label) {
    selected <- .summary_rows(context, label) & !is.na(cases$event_to_report)
    if (!is.na(cutoff)) selected <- selected & cases$event_date <= cutoff
    if (!any(selected)) return(NULL)

    event <- cases$event_date[selected]
    delay <- cases$event_to_report[selected]
    count <- cases$count[selected]
    eventual <- tapply(count, factor(event), sum)
    # An event date with no cases at all has no "share of its total".
    eventual <- eventual[!is.na(eventual) & eventual != 0]
    if (length(eventual) == 0) return(NULL)

    dplyr::bind_rows(lapply(delays, function(d) {
      arrived <- tapply(
        count * (delay <= d), factor(event), sum
      )[names(eventual)]
      arrived[is.na(arrived)] <- 0
      shares <- as.numeric(arrived / eventual)
      row <- .summary_stat_row(
        shares, NULL, "completeness", paste0("delay <= ", d), label,
        n = length(shares), total = sum(arrived)
      )
      row$prop <- sum(arrived) / sum(eventual)
      row
    }))
  })

  dplyr::bind_rows(rows)
}


# ==============================================================================
# The tests these functions had, lifted verbatim out of
# `tests/testthat/test-summary.R`. They rely on that file's `fixture_plain()`
# and `pick()` helpers, so they only run if they are moved back into it.
# ==============================================================================

# Autocorrelation --------------------------------------------------------------

test_that("lag-1 autocorrelation is the lagged-pair correlation", {
  skip_on_cran()
  # Event grid 3, 3, 0, 0, 4.
  #   head = 3, 3, 0, 0 (mean 1.5)   tail = 3, 0, 0, 4 (mean 1.75)
  #   sum of products = 1.875 - 2.625 + 2.625 - 3.375 = -1.5
  #   cor = (-1.5 / 3) / (sqrt(9 / 3) * sqrt(12.75 / 3)) = -0.5 / sqrt(12.75)
  row <- pick(
    suppressWarnings(case_autocorrelation(fixture_plain())),
    "autocorrelation", "per_event_date lag 1"
  )

  expect_equal(row$n, 4L)
  expect_equal(row$value, -0.5 / sqrt(12.75))
  expect_equal(row$value, stats::cor(c(3, 3, 0, 0), c(3, 0, 0, 4)))
})

test_that("autocorrelation accepts several lags and axes", {
  skip_on_cran()
  result <- suppressWarnings(
    case_autocorrelation(fixture_plain(), lags = c(1, 2), axis = "report")
  )

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
    pick(suppressWarnings(case_autocorrelation(flat)),
         "autocorrelation", "per_event_date lag 1")$value,
    NA_real_
  )
})

# Completeness -----------------------------------------------------------------

test_that("reporting completeness is the share arrived by each delay", {
  skip_on_cran()
  # mature_only trims to now minus the 95th delay percentile (2 days), so only
  # the event dates 01-01 and 01-02 are used. Their eventual totals are 3 and 3.
  #   delay <= 0: 2/3 and 0/3   -> mean 1/3, sd sqrt(2/9), pooled 2/6
  #   delay <= 2: 3/3 and 3/3   -> mean 1, pooled 1
  result <- suppressWarnings(reporting_completeness(fixture_plain()))

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
  skip_on_cran()
  # 2024-01-05 is one day old and fully reported, so adding it lifts the
  # same-day share to (2/3 + 0/3 + 4/4) / 3 = 5/9, pooled 6/10.
  result <- suppressWarnings(
    reporting_completeness(fixture_plain(), mature_only = FALSE)
  )
  same_day <- pick(result, "completeness", "delay <= 0")

  expect_equal(same_day$n, 3L)
  expect_equal(same_day$mean, 5 / 9)
  expect_equal(same_day$prop, 0.6)
})

test_that("completeness is a distribution, so `value` stays empty", {
  skip_on_cran()
  # The share arrived by delay d varies from one event date to the next, so it
  # is reported like every other distribution in the schema: mean/sd/quantiles
  # across the event dates, plus the pooled share in `prop`. `value` is the
  # column for the rows that really are a single scalar -- an autocorrelation,
  # a gap, an occupancy -- and completeness must not fill it, because that
  # would be a second estimator of a number `prop` already carries. The
  # documented examples select `mean`/`q50`/`prop` for exactly this reason.
  result <- suppressWarnings(reporting_completeness(fixture_plain()))
  expect_true("value" %in% names(result))
  expect_true(all(is.na(result$value)))
  expect_false(anyNA(result$mean))
  expect_false(anyNA(result$q50))
  expect_false(anyNA(result$prop))

})

test_that("reporting_completeness() honours an explicit delay set", {
  result <- suppressWarnings(
    reporting_completeness(fixture_plain(), delays = c(0, 2))
  )
  expect_equal(result$quantity, c("delay <= 0", "delay <= 2"))
})

test_that("bad lags are rejected", {
  skip_on_cran()
  suppressWarnings({
    expect_error(case_autocorrelation(fixture_plain(), lags = 0), "positive whole")
    expect_error(case_autocorrelation(fixture_plain(), lags = -1), "positive whole")
  })
})

# Unreviewed components --------------------------------------------------------

test_that("the AI-written components warn, and are not in summary()", {
  skip_on_cran()
  # They were written by an LLM and have not been checked by a human, so they
  # cannot sit inside the report a user reads by default.
  components <- unique(summary(fixture_plain())$component)
  expect_false("autocorrelation" %in% components)
  expect_false("completeness" %in% components)

  expect_warning(case_autocorrelation(fixture_plain()), "written by\n?\\s*an AI")
  expect_warning(reporting_completeness(fixture_plain()), "reviewed by a human")

  # Deliberately not throttled: every call says it.
  suppressWarnings(case_autocorrelation(fixture_plain()))
  expect_warning(case_autocorrelation(fixture_plain()), "reviewed by a human")
})
