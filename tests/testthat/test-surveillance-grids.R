# `get_surveillance_when()` / `get_surveillance_range()`.
#
# `surveillance::nowcast()` takes three dates and two date grids and has no
# usable defaults for any of them. These build the grids from the object, so the
# thing that matters is that they agree with `get_now()` and the object's own
# time step -- restating either by hand is how the article got them wrong.

daily_tbl_now <- function() {
  d <- data.frame(
    ev = as.Date("2024-01-01") + c(0, 1, 2, 3, 4),
    rp = as.Date("2024-01-01") + c(1, 2, 4, 5, 7)
  )
  tbl_now(d, event_date = "ev", report_date = "rp", verbose = FALSE)
}

weekly_tbl_now <- function() {
  data(denguedat, envir = environment())
  tbl_now(denguedat, event_date = "onset_week", report_date = "report_week",
          verbose = FALSE)
}

test_that("the `when` grid ends exactly at `now` and has `length` entries", {
  x <- daily_tbl_now()
  when <- get_surveillance_when(x, length = 3)

  expect_length(when, 3L)
  expect_equal(max(when), get_now(x))
  expect_true(!is.unsorted(when))
  expect_equal(when, get_now(x) - c(2, 1, 0))
})

test_that("the `range` grid runs from the first event to `now`", {
  x <- daily_tbl_now()
  range <- get_surveillance_range(x)

  expect_equal(min(range), min(x$ev))
  expect_equal(max(range), get_now(x))
  # Daily object, daily grid: no gaps.
  expect_equal(as.numeric(diff(range)), rep(1, length(range) - 1L))
})

test_that("both grids follow the object's own time step", {
  x <- weekly_tbl_now()

  when <- get_surveillance_when(x, length = 4)
  expect_equal(as.numeric(diff(when)), rep(7, 3))
  expect_equal(max(when), get_now(x))

  range <- get_surveillance_range(x)
  expect_equal(as.numeric(unique(diff(range))), 7)
  expect_equal(max(range), get_now(x))
})

test_that("`by` counts back from `now` rather than forward from a start", {
  # A month is not a fixed number of days, so seq()-ing forward from a computed
  # start lands somewhere near `now` instead of on it. It is the `now` end that
  # has to be exact -- that is the day being nowcast.
  x <- daily_tbl_now()
  when <- get_surveillance_when(x, length = 3, by = "1 month")

  expect_equal(max(when), get_now(x))
  expect_length(when, 3L)
})

test_that("`from`, `to` and `by` override the defaults", {
  x <- daily_tbl_now()

  expect_equal(
    get_surveillance_range(x, from = as.Date("2024-01-03"),
                           to = as.Date("2024-01-05")),
    seq(as.Date("2024-01-03"), as.Date("2024-01-05"), by = "1 day")
  )
  expect_equal(
    max(get_surveillance_when(x, length = 2, to = as.Date("2024-01-02"))),
    as.Date("2024-01-02")
  )
})

test_that("a numeric grid is refused rather than turned into 1970 dates", {
  d <- data.frame(ev = c(1L, 2L, 3L), rp = c(2L, 3L, 5L))
  x <- tbl_now(d, event_date = "ev", report_date = "rp", event_units = "numeric",
               report_units = "numeric", verbose = FALSE)

  # `as.Date()` on an integer index anchors at the epoch and hands back a
  # plausible-looking grid of invented dates. Both must abort instead.
  expect_error(get_surveillance_when(x), "numeric")
  expect_error(get_surveillance_range(x), "numeric")
})

test_that("bad arguments abort with a message that names them", {
  x <- daily_tbl_now()

  expect_error(get_surveillance_when(x, length = 0), "length")
  expect_error(get_surveillance_when(x, length = c(2, 3)), "length")
  expect_error(get_surveillance_when(x, to = as.Date(NA)), "to")
  expect_error(
    get_surveillance_range(x, from = as.Date("2030-01-01")),
    "after"
  )
  expect_error(get_surveillance_when(data.frame(a = 1)), "tbl_now")
  expect_error(get_surveillance_range(data.frame(a = 1)), "tbl_now")
})

test_that("the grids drive a real surveillance::nowcast() call", {
  skip_on_cran()
  skip_if_not_installed("surveillance")
  x <- weekly_tbl_now()
  x <- x[x$onset_week >= get_now(x) - 200, ]

  linelist <- suppressWarnings(suppressMessages(
    tbl_now_to_surveillance(x, verbose = FALSE)
  ))
  fit <- suppressWarnings(suppressMessages(surveillance::nowcast(
    now  = get_now(x),
    when = get_surveillance_when(x, length = 3),
    data = linelist,
    dEventCol = "dHospital", dReportCol = "dReport",
    aggregate.by = "1 week", D = 5, method = "bayes.notrunc.bnb",
    control = list(dRange = get_surveillance_range(x),
                   N.tInf.max = 10000, nSamples = 200)
  )))

  tidied <- suppressWarnings(generics::tidy(fit))
  # `when` asked for three dates ending at `now`, so that is what comes back --
  # the property `dRange` exists to guarantee, since the last weeks of a line
  # list can be empty and would otherwise fall off the inferred axis.
  expect_equal(nrow(tidied), 3L)
  expect_equal(max(tidied$event_date), get_now(x))
})

# `N.tInf.max`: the support of surveillance's nowcast distribution -------------
#
# `surveillance::nowcast()` defaults it to 300 -- smaller than a single period's
# count on most real series. Exceeding it does not warn: the fit dies with
# `subscript out of bounds`, which names neither the argument nor the number, and
# sends you looking at the date grids instead. `run_nowcast()` passed only
# `dRange`, so it inherited that default and failed on anything of consequence.

test_that("the surveillance support is scaled off the data, not left at 300", {
  x <- daily_tbl_now()
  # Five events, one per day: the largest period total is 1, so the floor holds.
  expect_equal(tbl.now:::.surveillance_support(x), 300L)

  big <- data.frame(
    ev = as.Date("2024-01-01") + c(0, 1, 2),
    rp = as.Date("2024-01-02") + c(0, 1, 2),
    n  = c(10, 5000, 20)
  )
  big_now <- tbl_now(big, event_date = "ev", report_date = "rp", case_count = "n",
                     data_type = "count-incidence", verbose = FALSE)
  # Ten times the largest period total, so the nowcast has room to correct up.
  expect_equal(tbl.now:::.surveillance_support(big_now), 50000L)
})

test_that("run_nowcast(surveillance) survives counts above surveillance's default", {
  skip_on_cran()
  skip_if_not_installed("surveillance")
  x <- weekly_tbl_now()
  x <- x[x$onset_week >= get_now(x) - 700, ]

  # The largest weekly total here is well above 300, which is exactly the case
  # that used to abort with "subscript out of bounds".
  counted <- suppressWarnings(suppressMessages(to_count(x, to = "count-incidence")))
  largest <- max(tapply(counted[[get_case_count(counted)]],
                        counted[[get_event_date(counted)]], sum))
  expect_gt(largest, 300)

  fit <- suppressWarnings(suppressMessages(
    run_nowcast(x, "surveillance", D = 5, verbose = FALSE)
  ))
  expect_s3_class(tidy(fit), "data.frame")
  expect_gt(nrow(as_tibble(fit)), 0L)
})

test_that("an explicit N.tInf.max in `control` still wins", {
  skip_on_cran()
  skip_if_not_installed("surveillance")
  x <- weekly_tbl_now()
  x <- x[x$onset_week >= get_now(x) - 200, ]

  # Whatever the default would have been, the caller's value is what is used --
  # so this is small enough to be visibly the caller's choice, not ours.
  expect_no_error(suppressWarnings(suppressMessages(
    run_nowcast(x, "surveillance", D = 4,
                control = list(N.tInf.max = 100000), verbose = FALSE)
  )))
})
