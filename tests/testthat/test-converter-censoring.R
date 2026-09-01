# A censoring indicator that is a per-case property rather than a function of
# the delay splits one (event, report) cell into a censored and an uncensored
# row. Reporting triangles have one slot per cell, so before
# `.tbl_now_collapse_censoring()` this made the cell non-unique and both
# baselinenowcast and epinowcast aborted. See #25 follow-up.

censored_fixture <- function(type = c("linelist", "count-incidence")) {
  type <- match.arg(type)
  data(denguedat, envir = environment())
  d <- denguedat |>
    dplyr::filter(
      onset_week >= as.Date("2002-01-01"),
      onset_week < as.Date("2002-07-22")
    )
  # A flag that varies WITHIN a cell, unlike one derived from the delay.
  set.seed(1)
  d$upper_bound_only <- sample(
    c(TRUE, FALSE), nrow(d), replace = TRUE, prob = c(0.3, 0.7)
  )

  ll <- tbl_now(
    d,
    event_date = "onset_week", report_date = "report_week",
    data_type = "linelist", verbose = FALSE
  ) |>
    add_is_censored_report("upper_bound_only")

  if (type == "linelist") ll else to_count(ll, to = "count-incidence")
}

converters <- c(
  "tbl_now_to_baselinenowcast", "tbl_now_to_epinowcast",
  "tbl_now_to_tsibble", "tbl_now_to_surveillance"
)

# Every converter also warns about unrelated things (lossy conversion, counts
# being re-aggregated). `expect_warning()` re-emits whatever it did not match,
# so collect the censoring warning explicitly and muffle the rest.
censoring_warnings <- function(expr) {
  seen <- character(0)
  withCallingHandlers(
    suppressMessages(expr),
    warning = function(cnd) {
      msg <- conditionMessage(cnd)
      if (grepl("censored delays", msg)) seen <<- c(seen, msg)
      invokeRestart("muffleWarning")
    }
  )
  seen
}

test_that("a per-case censoring flag makes cells non-unique", {
  counts <- as.data.frame(censored_fixture("count-incidence"))
  expect_gt(
    sum(duplicated(counts[, c("onset_week", "report_week")])),
    0
  )
})

for (fn in converters) {
  local({
    converter <- fn

    test_that(paste0(converter, "() collapses censoring on count data"), {
      skip_if_not_installed(sub("^tbl_now_to_", "", converter))
      counts <- censored_fixture("count-incidence")

      seen <- censoring_warnings(
        do.call(converter, list(counts, verbose = FALSE))
      )
      expect_length(seen, 1L)
      expect_match(seen, "summing counts over")
    })

    test_that(paste0(converter, "() drops censoring on a line list"), {
      skip_if_not_installed(sub("^tbl_now_to_", "", converter))
      ll <- censored_fixture("linelist")

      seen <- censoring_warnings(
        do.call(converter, list(ll, verbose = FALSE))
      )
      expect_length(seen, 1L)
      expect_match(seen, "dropping the")
    })

    test_that(paste0(converter, "() does not warn without censoring"), {
      skip_if_not_installed(sub("^tbl_now_to_", "", converter))
      data(denguedat, envir = environment())
      plain <- tbl_now(
        denguedat |>
          dplyr::filter(
            onset_week >= as.Date("2002-01-01"),
            onset_week < as.Date("2002-07-22")
          ),
        event_date = "onset_week", report_date = "report_week",
        data_type = "linelist", verbose = FALSE
      )

      expect_length(
        censoring_warnings(do.call(converter, list(plain, verbose = FALSE))),
        0L
      )
    })
  })
}

test_that("collapsing censored counts preserves the case total", {
  skip_if_not_installed("baselinenowcast")
  counts <- censored_fixture("count-incidence")

  triangle <- suppressWarnings(suppressMessages(
    tbl_now_to_baselinenowcast(counts, verbose = FALSE)
  ))

  expect_equal(sum(triangle, na.rm = TRUE), sum(counts$n))
})

test_that("collapsing drops the flag but keeps every other column", {
  counts <- censored_fixture("count-incidence")
  collapsed <- suppressWarnings(
    .tbl_now_collapse_censoring(counts, "tbl_now_to_baselinenowcast")
  )

  expect_true(is_tbl_now(collapsed))
  expect_null(get_is_censored_report(collapsed))
  expect_false("upper_bound_only" %in% names(collapsed))
  expect_true(all(
    setdiff(names(counts), "upper_bound_only") %in% names(collapsed)
  ))
  expect_lt(nrow(collapsed), nrow(counts))
  expect_equal(sum(collapsed$n), sum(counts$n))
})

test_that("collapsing a line list keeps one row per case", {
  ll <- censored_fixture("linelist")
  collapsed <- suppressWarnings(
    .tbl_now_collapse_censoring(ll, "tbl_now_to_surveillance")
  )

  expect_true(is_tbl_now(collapsed))
  expect_null(get_is_censored_report(collapsed))
  expect_false("upper_bound_only" %in% names(collapsed))
  expect_equal(nrow(collapsed), nrow(ll))
})

test_that("delay-derived censoring still reaches a triangle", {
  skip_if_not_installed("baselinenowcast")
  data(denguedat, envir = environment())
  d <- denguedat |>
    dplyr::filter(
      onset_week >= as.Date("2002-01-01"),
      onset_week < as.Date("2002-07-22")
    )
  ll <- tbl_now(
    d,
    event_date = "onset_week", report_date = "report_week",
    data_type = "linelist", verbose = FALSE
  )
  censored <- suppressMessages(censor_delays_above(ll, max_delay = 6))

  triangle <- suppressWarnings(suppressMessages(
    tbl_now_to_baselinenowcast(censored, verbose = FALSE)
  ))
  expect_equal(sum(triangle, na.rm = TRUE), nrow(d))
})

test_that("tbl_now_to_epidist() keeps the censoring indicator", {
  skip_if_not_installed("epidist")
  ll <- censored_fixture("linelist")

  # epidist estimates a delay distribution, so it is the one converter that can
  # use the flag: it must NOT be collapsed away.
  expect_length(
    censoring_warnings(tbl_now_to_epidist(ll, verbose = FALSE)),
    0L
  )
})

test_that("tbl_now_to_nobbs() collapses censoring without losing a case", {
  skip_if_not_installed("NobBS")
  # NobBS is absent from the `converters` loop above because its package name
  # ("NobBS") is not the converter's suffix ("nobbs"), so it needs its own test.
  # It is also the converter where a lossy collapse is easiest to SEE: NobBS
  # counts rows, so the expansion has to come back with exactly one row per case.
  counts <- censored_fixture("count-incidence")

  seen <- censoring_warnings(tbl_now_to_nobbs(counts, verbose = FALSE))
  expect_length(seen, 1L)
  expect_match(seen, "summing counts over")

  linelist <- suppressWarnings(suppressMessages(
    tbl_now_to_nobbs(counts, verbose = FALSE)
  ))
  expect_equal(nrow(linelist), sum(counts$n))
  expect_false("upper_bound_only" %in% names(linelist))

  ll <- censored_fixture("linelist")
  seen_ll <- censoring_warnings(tbl_now_to_nobbs(ll, verbose = FALSE))
  expect_length(seen_ll, 1L)
  expect_match(seen_ll, "dropping the")
  expect_equal(
    nrow(suppressWarnings(suppressMessages(
      tbl_now_to_nobbs(ll, verbose = FALSE)
    ))),
    nrow(ll)
  )
})
