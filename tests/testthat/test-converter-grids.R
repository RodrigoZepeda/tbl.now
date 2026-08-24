# Temporal grid x delay shape x completeness.
#
# `test-converter-datasets.R` runs every converter against every shipped
# dataset, and every one of those is on a **daily or weekly calendar grid with
# non-negative delays**. The dimensions this file adds are the ones no shipped
# dataset has:
#
#   * a `numeric` (index) grid, where the "date" columns are integers;
#   * a delay of exactly zero, and a delay that is NEGATIVE (report before
#     event);
#   * a gap in the event grid, and a trailing event period with no reports;
#   * a single very long straggler, which sets the width of the delay axis.
#
# The rule these tests enforce is the one in DEVELOPMENT_SKILL.md section 7:
# "Resolve units from the attribute, never assume days". A converter may accept
# a grid or refuse it, but it may not quietly reinterpret it.

q <- function(expr) suppressWarnings(suppressMessages(expr))

# --- fixtures -----------------------------------------------------------------

# A `numeric` grid: `event_units` and `report_units` are both "numeric" and the
# two columns are integer indices, not Dates.
numeric_grid_tbl_now <- function() {
  tbl_now(
    data.frame(
      ev = c(1L, 1L, 2L, 2L, 3L, 4L),
      rp = c(1L, 2L, 2L, 3L, 3L, 4L),
      n  = c(2, 1, 3, 1, 5, 2)
    ),
    event_date = "ev", report_date = "rp", case_count = "n",
    data_type = "count-incidence", verbose = FALSE
  )
}

# --- numeric grid -------------------------------------------------------------

test_that("a numeric grid stays an integer index where it is carried through", {
  x <- numeric_grid_tbl_now()
  expect_equal(get_event_units(x), "numeric")

  # data.table keeps every column as it found it.
  dt <- q(tbl_now_to_data_table(x, verbose = FALSE))
  expect_type(dt$ev, "integer")
  expect_type(dt$rp, "integer")

  # A tsibble index may be any ordered type, so the integer survives.
  skip_if_not_installed("tsibble")
  ts <- q(tbl_now_to_tsibble(x, verbose = FALSE))
  expect_type(ts[[tsibble::index_var(ts)]], "integer")

  # baselinenowcast's LONG format is a tidy data frame with no delay axis, so it
  # has nothing to resolve and carries the index through too.
  skip_if_not_installed("baselinenowcast")
  lng <- q(tbl_now_to_baselinenowcast(x, format = "long", verbose = FALSE))
  expect_type(lng$reference_date, "integer")
  expect_equal(sum(lng$count), 14)
})

test_that("the date-based back-ends refuse a numeric grid by name", {
  x <- numeric_grid_tbl_now()

  # Each of these needs a calendar grid. Refusing is the point: coercing the
  # integer indices with `as.Date()` anchors them at the 1970 epoch and hands
  # back a plausible-looking 1970 outbreak instead of an error.
  skip_if_not_installed("baselinenowcast")
  expect_error(
    q(tbl_now_to_baselinenowcast(x, verbose = FALSE)), "delays_unit"
  )
  skip_if_not_installed("epinowcast")
  expect_error(
    q(tbl_now_to_epinowcast(x, verbose = FALSE, quiet = TRUE)), "timestep"
  )
  skip_if_not_installed("NobBS")
  expect_error(q(tbl_now_to_nobbs(x, verbose = FALSE)), "numeric")
  skip_if_not_installed("surveillance")
  expect_error(q(tbl_now_to_surveillance(x, verbose = FALSE)), "numeric")
})

test_that("no converter turns a numeric index into a 1970 date", {
  x <- numeric_grid_tbl_now()
  fns <- c("tbl_now_to_nobbs", "tbl_now_to_surveillance")
  for (fn in fns) {
    skip_if_not_installed(if (fn == "tbl_now_to_nobbs") "NobBS" else "surveillance")
    out <- tryCatch(q(get(fn)(x, verbose = FALSE)), error = function(e) NULL)
    # Either it refused (NULL) or, if a future version accepts it, the dates it
    # produced must not sit at the Unix epoch.
    if (!is.null(out)) {
      dates <- unlist(lapply(out, function(col) {
        if (inherits(col, "Date")) as.character(col) else character(0)
      }))
      expect_false(any(substr(dates, 1, 4) == "1970"))
    } else {
      expect_null(out)
    }
  }
})

# --- unit resolution on the calendar grids -----------------------------------

test_that("the line-list back-ends translate units into their own vocabulary", {
  # `NobBS::NobBS()` takes `units` as "1 day"/"1 week" and
  # `surveillance::linelist2sts()` takes `aggregate.by` from a fixed set of
  # strings. Neither accepts the object's own "days"/"weeks", so the converters
  # have to translate -- including in the verbose summary, which is what a
  # reader copies into the call.
  daily <- tbl_now(
    data.frame(
      ev = as.Date("2024-01-01") + c(0, 0, 1),
      rp = as.Date("2024-01-01") + c(0, 1, 1)
    ),
    event_date = "ev", report_date = "rp",
    data_type = "linelist", verbose = FALSE
  )
  weekly <- tbl_now(
    data.frame(
      ev = as.Date("2024-01-01") + 7 * c(0, 0, 1),
      rp = as.Date("2024-01-01") + 7 * c(0, 1, 1)
    ),
    event_date = "ev", report_date = "rp",
    event_units = "weeks", report_units = "weeks",
    data_type = "linelist", verbose = FALSE
  )

  # `verbose = TRUE` writes through cli, i.e. to the MESSAGE stream, so the
  # messages must not be suppressed here -- only the (unrelated) warnings.
  units_line <- function(x, fn) {
    msg <- capture.output(
      invisible(suppressWarnings(get(fn)(x, verbose = TRUE))), type = "message"
    )
    grep("units|aggregate", msg, value = TRUE)
  }

  skip_if_not_installed("NobBS")
  expect_match(units_line(daily, "tbl_now_to_nobbs"), "1 day", all = FALSE)
  expect_match(units_line(weekly, "tbl_now_to_nobbs"), "1 week", all = FALSE)

  skip_if_not_installed("surveillance")
  expect_match(
    units_line(daily, "tbl_now_to_surveillance"), "1 day", all = FALSE
  )
  expect_match(
    units_line(weekly, "tbl_now_to_surveillance"), "1 week", all = FALSE
  )
})

# --- delays: zero, negative, very long ---------------------------------------

test_that("an all-zero-delay series gives a one-column triangle", {
  skip_if_not_installed("baselinenowcast")
  x <- tbl_now(
    data.frame(
      ev = as.Date("2024-01-01") + 0:3,
      rp = as.Date("2024-01-01") + 0:3,
      n  = 1:4
    ),
    event_date = "ev", report_date = "rp", case_count = "n",
    data_type = "count-incidence", verbose = FALSE
  )
  tri <- q(tbl_now_to_baselinenowcast(x, verbose = FALSE))

  expect_equal(ncol(tri), 1L)
  expect_equal(colnames(tri), "0")
  expect_equal(sum(tri, na.rm = TRUE), 10)
  # No NA: with every delay zero, the triangle is fully observed.
  expect_false(anyNA(tri))
})

test_that("a zero delay still gets a strictly positive epidist window", {
  skip_if_not_installed("epidist")
  x <- tbl_now(
    data.frame(
      ev = as.Date("2024-01-01") + 0:3,
      rp = as.Date("2024-01-01") + 0:3,
      n  = 1:4
    ),
    event_date = "ev", report_date = "rp", case_count = "n",
    data_type = "count-incidence", verbose = FALSE
  )
  ed <- q(tbl_now_to_epidist(x, verbose = FALSE))

  # epidist requires every censoring window to have positive width, and a
  # same-period report is where that is easiest to get wrong.
  expect_true(all(as.numeric(ed$pdate_upr - ed$pdate_lwr) > 0))
  expect_true(all(as.numeric(ed$sdate_upr - ed$sdate_lwr) > 0))
})

negative_delay_tbl_now <- function() {
  q(tbl_now(
    data.frame(
      ev = as.Date("2024-01-05") + c(0, 1, 2, 3),
      rp = as.Date("2024-01-05") + c(-2, 1, 4, 3),
      n  = c(1, 2, 3, 4)
    ),
    event_date = "ev", report_date = "rp", case_count = "n",
    data_type = "count-incidence", verbose = FALSE
  ))
}

test_that("a NEGATIVE delay is dropped by the triangle back-ends, not by all", {
  # A report BEFORE its event has no slot in a reporting triangle: the delay
  # axis starts at 0. This records what each target does with it, because the
  # answers differ.
  x <- negative_delay_tbl_now()
  expect_equal(min(x$.delay), -2)

  skip_if_not_installed("baselinenowcast")
  tri <- q(tbl_now_to_baselinenowcast(x, verbose = FALSE))
  # 10 cases in, 9 in the triangle. The pre-event report is gone and its cell
  # reads 0 -- an OBSERVED zero -- rather than NA, which is why the converter has
  # to say so out loud (next test).
  expect_equal(sum(tri, na.rm = TRUE), 9)
  expect_equal(tri[1, 1], 0)

  # The line-list back-ends count rows, so they keep every case, negative delay
  # and all.
  skip_if_not_installed("NobBS")
  nb <- q(tbl_now_to_nobbs(x, verbose = FALSE))
  expect_equal(nrow(nb), 10L)
  expect_equal(min(as.numeric(nb$report_date - nb$onset_date)), -2)

  # epidist measures a delay from a censoring window and rejects a negative one
  # outright (its own assertion, not ours).
  skip_if_not_installed("epidist")
  expect_error(q(tbl_now_to_epidist(x, verbose = FALSE)))
})

test_that("the delay-indexed converters warn about the cases they drop", {
  x <- negative_delay_tbl_now()

  # Collect only the negative-delay warning: both converters warn about other
  # things too (lossy conversion, count coercion).
  negative_warnings <- function(expr) {
    seen <- character(0)
    withCallingHandlers(
      suppressMessages(expr),
      warning = function(cnd) {
        msg <- conditionMessage(cnd)
        if (grepl("negative delay", msg)) seen <<- c(seen, msg)
        invokeRestart("muffleWarning")
      }
    )
    seen
  }

  skip_if_not_installed("baselinenowcast")
  seen <- negative_warnings(tbl_now_to_baselinenowcast(x, verbose = FALSE))
  expect_length(seen, 1L)
  # It must name what is lost, in cases, not just say something happened.
  expect_match(seen, "1 case")
  expect_match(seen, "dropped")

  # The LONG format is a tidy data frame with no delay axis, so it keeps the row
  # and must stay quiet -- a warning there would be crying wolf.
  expect_length(
    negative_warnings(
      tbl_now_to_baselinenowcast(x, format = "long", verbose = FALSE)
    ),
    0L
  )

  skip_if_not_installed("epinowcast")
  skip_on_cran()
  expect_length(
    negative_warnings(
      tbl_now_to_epinowcast(x, verbose = FALSE, quiet = TRUE)
    ),
    1L
  )

  # Non-negative delays must not warn at all.
  clean <- tbl_now(
    data.frame(
      ev = as.Date("2024-01-01") + c(0, 0, 1),
      rp = as.Date("2024-01-01") + c(0, 1, 1),
      n  = c(1, 2, 3)
    ),
    event_date = "ev", report_date = "rp", case_count = "n",
    data_type = "count-incidence", verbose = FALSE
  )
  expect_length(
    negative_warnings(tbl_now_to_baselinenowcast(clean, verbose = FALSE)),
    0L
  )
})

# --- epidist needs n >= 1 ----------------------------------------------------

test_that("tbl_now_to_epidist() drops the counts epidist cannot hold", {
  skip_if_not_installed("epidist")
  # De-accumulating cumulative totals gives a 0 wherever a report added nothing.
  # epidist asserts `n >= 1`, so before this every realistic cumulative series
  # died on `Assertion on 'data$n' failed`.
  cumulative <- tbl_now(
    data.frame(
      ev = rep(as.Date("2024-01-01") + 0:3, each = 3),
      rp = as.Date("2024-01-01") + c(0:2, 1:3, 2:4, 3:5),
      n  = c(1, 3, 5, 2, 4, 4, 1, 1, 6, 2, 2, 2)
    ),
    event_date = "ev", report_date = "rp", case_count = "n",
    data_type = "count-cumulative", verbose = FALSE
  )
  increments <- q(to_count(cumulative, to = "count-incidence"))
  positive <- increments[[get_case_count(increments)]]
  positive <- sum(positive[positive > 0])

  ed <- q(tbl_now_to_epidist(cumulative, verbose = FALSE))
  expect_s3_class(ed, "epidist_aggregate_data")
  expect_true(all(ed$n >= 1))
  # Every case that CAN be represented survives; only the empty cells go.
  expect_equal(sum(ed$n), positive)
})

test_that("a downward revision warns before its negative count is dropped", {
  skip_if_not_installed("epidist")
  revised <- tbl_now(
    data.frame(
      ev = rep(as.Date("2024-01-01") + 0:1, each = 3),
      rp = as.Date("2024-01-01") + c(0:2, 1:3),
      n  = c(5, 9, 7, 4, 6, 6)   # 9 -> 7 is a downward revision
    ),
    event_date = "ev", report_date = "rp", case_count = "n",
    data_type = "count-cumulative", verbose = FALSE
  )
  expect_warning(
    ed <- suppressMessages(tbl_now_to_epidist(revised, verbose = FALSE)),
    "negative count"
  )
  expect_true(all(ed$n >= 1))
})

test_that("tbl_now_to_epidist() aborts when no count is usable", {
  skip_if_not_installed("epidist")
  empty <- tbl_now(
    data.frame(
      ev = as.Date("2024-01-01") + c(0, 0, 1, 1),
      rp = as.Date("2024-01-01") + c(0, 1, 1, 2),
      n  = c(0, 0, 0, 0)
    ),
    event_date = "ev", report_date = "rp", case_count = "n",
    data_type = "count-incidence",
    event_units = "days", report_units = "days", verbose = FALSE
  )
  # Aborting in the converter, saying why, beats epidist's own
  # "Assertion on 'data$n' failed: Element 1 is not >= 1".
  expect_error(
    q(tbl_now_to_epidist(empty, verbose = FALSE)), "No usable counts"
  )
})

test_that("flusight converts to epidist now that empty cells are dropped", {
  skip_on_cran()
  skip_if_not_installed("epidist")
  # `flusight` is count-cumulative and was the one `error` cell in the article's
  # converter matrix.
  data(flusight, envir = environment())
  one_state <- flusight |>
    dplyr::filter(location_name == "Alabama") |>
    tbl_now(
      event_date = target_end_date, report_date = as_of,
      case_count = observation, data_type = "count-cumulative", verbose = FALSE
    )

  ed <- q(tbl_now_to_epidist(one_state, verbose = FALSE))
  expect_s3_class(ed, "epidist_aggregate_data")
  expect_true(all(ed$n >= 1))
  expect_gt(nrow(ed), 0L)
})

test_that("one long straggler sets the width of the delay axis", {
  skip_if_not_installed("baselinenowcast")
  x <- tbl_now(
    data.frame(
      ev = as.Date("2024-01-01") + c(0, 0, 1),
      rp = as.Date("2024-01-01") + c(0, 30, 1),
      n  = c(4, 1, 6)
    ),
    event_date = "ev", report_date = "rp", case_count = "n",
    data_type = "count-incidence", verbose = FALSE
  )
  tri <- q(tbl_now_to_baselinenowcast(x, verbose = FALSE))

  # One column per delay 0..30 -- the cost the help page warns about.
  expect_equal(ncol(tri), 31L)
  expect_equal(sum(tri, na.rm = TRUE), 11)
  # Cells inside the triangle are OBSERVED zeros, not missing.
  expect_equal(unname(tri[1, 2:4]), c(0, 0, 0))
  # Only the not-yet-observable corner is NA: the second event date cannot yet
  # have reached delay 30.
  expect_equal(sum(is.na(tri)), 1L)
  expect_true(is.na(tri[2, 31]))
})

# --- completeness: gaps and a trailing empty period --------------------------

test_that("`complete` decides whether a gap in the event grid is filled", {
  skip_if_not_installed("baselinenowcast")
  # 2024-01-02 and -03 have no rows at all, and the `now` is 2024-01-05.
  x <- tbl_now(
    data.frame(
      ev = as.Date("2024-01-01") + c(0, 0, 3, 3),
      rp = as.Date("2024-01-01") + c(0, 1, 3, 4),
      n  = c(5, 5, 7, 7)
    ),
    event_date = "ev", report_date = "rp", case_count = "n",
    data_type = "count-incidence", verbose = FALSE
  )
  expect_equal(get_now(x), as.Date("2024-01-05"))

  rows_of <- function(complete) {
    rownames(q(tbl_now_to_baselinenowcast(x, complete = complete,
                                          verbose = FALSE)))
  }

  # COUNT data under "auto" is left exactly as supplied: the triangle has only
  # the two observed event dates and so stops three periods short of the `now`.
  # That is deliberate (filling would turn a not-yet-observable cell into an
  # observed zero) but it means the newest event periods are absent from the fit.
  expect_equal(rows_of("auto"), c("2024-01-01", "2024-01-04"))
  expect_equal(rows_of(FALSE), c("2024-01-01", "2024-01-04"))
  # `TRUE` forces the rectangular grid out to the `now`.
  expect_equal(
    rows_of(TRUE),
    as.character(as.Date("2024-01-01") + 0:4)
  )
  # Filling adds zeros; it must not add or lose cases.
  for (complete in list("auto", TRUE, FALSE)) {
    tri <- q(tbl_now_to_baselinenowcast(x, complete = complete,
                                         verbose = FALSE))
    expect_equal(sum(tri, na.rm = TRUE), 24)
  }
})

test_that("a line list is completed to the `now` under `complete = 'auto'`", {
  skip_if_not_installed("baselinenowcast")
  # A line list cannot express a zero -- an event period with no cases has no
  # rows -- so "auto" completes it, which is the asymmetry with count data.
  x <- tbl_now(
    data.frame(
      ev = as.Date("2024-01-01") + c(0, 0, 1),
      rp = as.Date("2024-01-01") + c(0, 1, 1)
    ),
    event_date = "ev", report_date = "rp", data_type = "linelist",
    now = as.Date("2024-01-04"), verbose = FALSE
  )
  tri <- q(tbl_now_to_baselinenowcast(x, verbose = FALSE))

  expect_equal(rownames(tri), as.character(as.Date("2024-01-01") + 0:3))
  expect_equal(sum(tri, na.rm = TRUE), 3)
  # The completed periods are observed zeros; only the newest cell that cannot
  # have been observed yet is NA.
  expect_equal(unname(tri["2024-01-03", ]), c(0, 0))
  expect_true(is.na(tri["2024-01-04", "1"]))
  expect_equal(unname(tri["2024-01-04", "0"]), 0)
})
