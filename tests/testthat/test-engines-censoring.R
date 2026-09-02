# A censoring flag must be handled visibly, never quietly.
#
# `is_censored_report` says a report date is only an UPPER BOUND: the case is known to
# have been reported at or before it. Three things can happen to that
# information, and again only one is unacceptable:
#
#   used     the target models the censoring (only a delay-distribution fit can)
#   dropped  the target has one slot per (event, report) cell, so the flag is
#            collapsed -- AND IT SAYS SO
#   silent   collapsed with no warning   <- the user believes censoring is
#                                           modelled when it is not
#
# The fixture uses PER-CASE censoring, which varies within a cell. The other
# kind -- derived from the delay by `censor_delays_above()` -- is constant within
# a cell and collapses harmlessly, so a test built on it would pass whether the
# handling were right or wrong.

skip_on_cran()

CENSORING_SPEC <- list(
  list(
    id = "baselinenowcast", package = "baselinenowcast", collapses = TRUE,
    convert = function(x) tbl_now_to_baselinenowcast(x, verbose = FALSE)
  ),
  list(
    id = "epinowcast", package = "epinowcast", collapses = TRUE,
    convert = function(x) tbl_now_to_epinowcast(x, verbose = FALSE, quiet = TRUE)
  ),
  list(
    id = "nobbs", package = "NobBS", collapses = TRUE,
    convert = function(x) tbl_now_to_nobbs(x, verbose = FALSE)
  ),
  list(
    id = "surveillance", package = "surveillance", collapses = TRUE,
    convert = function(x) tbl_now_to_surveillance(x, verbose = FALSE)
  ),
  list(
    id = "EpiNow2/estimate_infections", package = "EpiNow2", collapses = TRUE,
    convert = function(x) tbl_now_to_EpiNow2(x, verbose = FALSE, quiet = TRUE)
  ),
  list(
    id = "tsibble", package = "tsibble", collapses = TRUE,
    convert = function(x) tbl_now_to_tsibble(x, verbose = FALSE)
  ),
  # The two exceptions, and they are exceptions for the same reason: estimating
  # a DELAY DISTRIBUTION is the one job that can use censoring, because the
  # censored report becomes an interval rather than a point.
  list(
    id = "epidist", package = "epidist", collapses = FALSE,
    convert = function(x) tbl_now_to_epidist(x, verbose = FALSE)
  ),
  list(
    id = "EpiNow2/estimate_dist", package = "EpiNow2", collapses = FALSE,
    convert = function(x) {
      tbl_now_to_EpiNow2(x, target = "estimate_dist", verbose = FALSE, quiet = TRUE)
    }
  )
)

censoring_fixture <- function(data_type = "count-incidence") {
  engine_fixture(
    units = "days", data_type = data_type, n_strata = 0L,
    censored = TRUE, n_periods = 20L
  )
}

for (entry in CENSORING_SPEC) {
  local({
    this <- entry

    test_that(paste0("censoring is used or announced: ", this$id), {
      skip_if_not_installed(this$package)

      x <- censoring_fixture()
      expect_equal(get_is_censored_report(x), "is_censored_report")
      # The flag must genuinely split cells, or this test proves nothing.
      cells <- dplyr::n_distinct(x[[get_event_date(x)]], x[[get_report_date(x)]])
      expect_gt(nrow(x), cells)

      warnings <- character()
      converted <- withCallingHandlers(
        suppressMessages(this$convert(x)),
        warning = function(cnd) {
          warnings <<- c(warnings, conditionMessage(cnd))
          invokeRestart("muffleWarning")
        }
      )
      mentions_censoring <- any(grepl("censor", warnings, ignore.case = TRUE))

      if (isTRUE(this$collapses)) {
        # The failure this test exists for: collapsed in silence.
        expect_true(
          mentions_censoring,
          label = paste0(this$id, " announces that censoring was collapsed")
        )
      } else {
        # A delay fit USES it, so there is nothing to announce.
        expect_false(
          mentions_censoring,
          label = paste0(this$id, " is silent (it models the censoring)")
        )
      }
      expect_false(is.null(converted))
    })
  })
}

test_that("collapsing censoring preserves the case total", {
  skip_if_not_installed("baselinenowcast")

  # Collapsing sums the counts over the flag, so the totals must be untouched.
  # A collapse that dropped the censored rows instead would still "work".
  x <- censoring_fixture()
  total <- sum(x$n)

  triangle <- suppressWarnings(suppressMessages(
    tbl_now_to_baselinenowcast(x, verbose = FALSE)
  ))
  expect_equal(sum(triangle, na.rm = TRUE), total)
})

test_that("collapsing a line list drops the column without losing rows", {
  skip_if_not_installed("surveillance")

  # For a line list one row IS one case, so there is nothing to sum: the column
  # goes and the row count stays.
  x <- censoring_fixture(data_type = "linelist")
  rows <- nrow(x)

  linelist <- suppressWarnings(suppressMessages(
    tbl_now_to_surveillance(x, verbose = FALSE)
  ))
  expect_equal(nrow(linelist), rows)
  expect_false("is_censored_report" %in% colnames(linelist))
})

test_that("epidist keeps the flag, because it is the one model that can use it", {
  skip_if_not_installed("epidist")

  x <- censoring_fixture()
  converted <- suppressWarnings(suppressMessages(
    tbl_now_to_epidist(x, verbose = FALSE)
  ))

  # The censored rows become a WIDER interval, not a dropped row: a censored
  # report is known only to lie in [origin, report_date].
  expect_true(all(
    c("pdate_lwr", "pdate_upr", "sdate_lwr", "sdate_upr") %in% colnames(converted)
  ))
  widths <- converted$sdate_upr - converted$sdate_lwr
  expect_gt(length(unique(widths)), 1L)
})

test_that("no censoring declared means no censoring warning anywhere", {
  x <- engine_fixture(units = "days", censored = FALSE, n_periods = 20L)
  expect_null(get_is_censored_report(x))

  for (entry in CENSORING_SPEC) {
    if (!requireNamespace(entry$package, quietly = TRUE)) next
    warnings <- character()
    withCallingHandlers(
      suppressMessages(entry$convert(x)),
      warning = function(cnd) {
        warnings <<- c(warnings, conditionMessage(cnd))
        invokeRestart("muffleWarning")
      }
    )
    expect_false(
      any(grepl("censor", warnings, ignore.case = TRUE)),
      label = paste0(entry$id, " is silent about censoring when there is none")
    )
  }
})

test_that("run_nowcast() surfaces the collapse rather than swallowing it", {
  skip_if_not_installed("baselinenowcast")

  # `run_nowcast(verbose = FALSE)` quiets the CHATTER, not the warnings: a
  # silently collapsed flag is exactly what the user must not miss.
  x <- censoring_fixture()

  warnings <- character()
  set.seed(20260825)
  withCallingHandlers(
    suppressMessages(
      run_nowcast(x, engine_baselinenowcast(draws = 25), verbose = FALSE)
    ),
    warning = function(cnd) {
      warnings <<- c(warnings, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("censor", warnings, ignore.case = TRUE)))
})
