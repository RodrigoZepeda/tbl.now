# Tests for diagnose() and its component functions.
#
# Every fixture here plants ONE defect, deliberately, and the count is written
# out next to it. A check that silently stops working must therefore fail
# exactly one test, and the assertion must be about the defect that was
# planted -- a duplicate-row test built from rows that differ in an *undeclared*
# column, for instance, passes whether or not the key is right, so both
# variants are asserted separately below.

# The clean fixture ------------------------------------------------------------
#
#   event        report       gender   n
#   2024-01-01   2024-01-01   F        2     delay 0
#   2024-01-01   2024-01-03   M        1     delay 2
#   2024-01-02   2024-01-04   M        3     delay 2
#   2024-01-05   2024-01-05   F        4     delay 0
#
#   now = 2024-01-05, daily units, 10 cases.
#
#   Nothing here is out of order, missing, repeated, negative, off-grid or
#   dated after `now`, so a clean run must produce no `error` and no `warning`.

clean_frame <- function() {
  data.frame(
    onset  = as.Date(c("2024-01-01", "2024-01-01", "2024-01-02", "2024-01-05")),
    report = as.Date(c("2024-01-01", "2024-01-03", "2024-01-04", "2024-01-05")),
    gender = c("F", "M", "M", "F"),
    n      = c(2L, 1L, 3L, 4L)
  )
}

clean_tbl <- function(frame = clean_frame(), ...) {
  tbl_now(frame,
    event_date = "onset", report_date = "report", case_count = "n",
    strata = "gender", data_type = "count-incidence",
    now = as.Date("2024-01-05"), verbose = FALSE, ...
  )
}

# The same fixture with ONE row out of order: row 3 is reported on 2024-01-01,
# the day before its event.
dirty_frame <- function() {
  frame <- clean_frame()
  frame$report[3] <- as.Date("2024-01-01") # event is 2024-01-02
  frame
}

# One finding, as a plain list, so expectations read one field at a time.
finding <- function(result, check, scope, stratum = "all") {
  row <- result[result$check == check & result$scope == scope &
                  result$stratum == stratum, ]
  expect_equal(nrow(row), 1)
  as.list(row)
}

# Schema -----------------------------------------------------------------------

test_that("diagnose() returns the documented schema, in order", {
  result <- diagnose(clean_tbl())

  expect_s3_class(result, "tbl_df")
  expect_identical(
    names(result),
    c(
      "check", "scope", "stratum", "status", "n_affected", "n_total",
      "prop", "message", "hint", "rows"
    )
  )
  expect_true(is.ordered(result$status))
  expect_identical(
    levels(result$status),
    c("error", "warning", "note", "ok", "not_run", "skipped")
  )
  expect_type(result$rows, "list")
  expect_type(result$message, "character")
})

test_that("the findings are sorted worst first", {
  result <- diagnose(suppressWarnings(clean_tbl(dirty_frame())))

  expect_false(is.unsorted(as.integer(result$status)))
  # And the worst one really is at the top.
  expect_equal(as.character(result$status[1]), "warning")
})

test_that("diagnose() is the bind_rows() of its components", {
  ndata <- clean_tbl()

  components <- dplyr::bind_rows(
    diagnose_declarations(ndata),
    diagnose_ordering(ndata),
    diagnose_missing(ndata),
    diagnose_duplicates(ndata),
    diagnose_units(ndata),
    diagnose_negatives(ndata),
    diagnose_now(ndata),
    diagnose_truncation(ndata),
    diagnose_strata(ndata),
    diagnose_signposts(ndata)
  )
  components <- dplyr::arrange(
    components, .data$status, .data$check, .data$scope, .data$stratum
  )

  expect_equal(diagnose(ndata), components)
})

test_that("`checks` runs exactly the blocks it names", {
  ndata <- clean_tbl()

  expect_equal(diagnose(ndata, checks = "units"), diagnose_units(ndata))
  expect_setequal(diagnose(ndata, checks = "units")$check, "units")
  expect_error(diagnose(ndata, checks = "nonsense"), "should be one of")
})

test_that("a clean object produces no error and no warning findings", {
  result <- diagnose(clean_tbl())

  expect_equal(sum(result$status %in% c("error", "warning")), 0)
  expect_equal(as.character(finding(result, "ordering", "event_to_report")$status), "ok")
  expect_equal(as.character(finding(result, "duplicates", "key")$status), "ok")
  expect_equal(as.character(finding(result, "units", "delay")$status), "ok")
})

test_that("diagnose() refuses anything that is not a tbl_now", {
  expect_error(diagnose(mtcars), "tbl_now")
  expect_error(diagnose_missing(mtcars), "tbl_now")
})

# Pre-flight -------------------------------------------------------------------

test_that("a missing required attribute short-circuits to error rows", {
  broken <- clean_tbl()
  attr(broken, "event_date") <- NULL

  result <- diagnose(broken)

  expect_setequal(result$check, "attributes")
  expect_equal(as.character(unique(result$status)), "error")
  expect_true(any(grepl("Missing required attribute", result$message)))
})

# Ordering ---------------------------------------------------------------------
#
# One row, and one only, has its report BEFORE its event.

test_that("a report before its event is one warning naming that row", {
  ndata <- suppressWarnings(clean_tbl(dirty_frame()))

  row <- finding(diagnose(ndata), "ordering", "event_to_report")

  expect_equal(as.character(row$status), "warning")
  expect_equal(row$n_affected, 1)
  expect_equal(row$n_total, 4)
  expect_equal(row$rows[[1]], 3L)
  # The indices point back into the object the user is holding.
  expect_true(ndata$report[row$rows[[1]]] < ndata$onset[row$rows[[1]]])
})

test_that("the confirmation timeline is checked on both of its legs", {
  frame <- clean_frame()
  # Row 2 is confirmed the day BEFORE it was reported; row 4 is confirmed
  # before the event happened, and has no report date at all, so only the
  # transitive check can see it.
  frame$result <- as.Date(c("2024-01-02", "2024-01-02", "2024-01-05", NA))
  frame$outcome <- c("confirmed", "confirmed", "confirmed", "confirmed")
  frame$result[4] <- as.Date("2024-01-04")
  frame$report[4] <- NA
  frame$onset[4] <- as.Date("2024-01-05")

  ndata <- suppressWarnings(tbl_now(frame,
    event_date = "onset", report_date = "report", case_count = "n",
    confirmation_date = "result", confirmation_type = "outcome",
    data_type = "count-incidence", now = as.Date("2024-01-05"),
    verbose = FALSE
  ))
  result <- diagnose(ndata)

  before_report <- finding(result, "ordering", "report_to_confirmation")
  expect_equal(as.character(before_report$status), "warning")
  expect_equal(before_report$n_affected, 1)
  expect_equal(before_report$rows[[1]], 2L)

  before_event <- finding(result, "ordering", "event_to_confirmation")
  expect_equal(as.character(before_event$status), "note")
  expect_equal(before_event$n_affected, 1)
  expect_equal(before_event$rows[[1]], 4L)
})

test_that("the confirmation legs are skipped without a confirmation process", {
  result <- diagnose(clean_tbl())

  expect_equal(
    as.character(finding(result, "ordering", "report_to_confirmation")$status),
    "skipped"
  )
})

# Missing ----------------------------------------------------------------------

test_that("NA counts are reported neutrally, NA dates are not", {
  frame <- clean_frame()
  frame$n[2] <- NA_integer_ # one unobserved cell
  frame$gender[4] <- NA # one row with no stratum

  ndata <- suppressWarnings(clean_tbl(frame))
  result <- diagnose(ndata, by_strata = FALSE)

  counts <- finding(result, "missing", "n")
  expect_equal(as.character(counts$status), "note")
  expect_equal(counts$n_affected, 1)
  expect_equal(counts$rows[[1]], 2L)
  # An NA count is CORRECT data: the cell has not been observed yet.
  expect_match(counts$hint, "not yet observed")

  strata <- finding(result, "missing", "gender")
  expect_equal(strata$n_affected, 1)
  expect_equal(strata$rows[[1]], 4L)

  dates <- finding(result, "missing", "onset")
  expect_equal(as.character(dates$status), "ok")
})

test_that("a missing event date is a warning, as it always has been", {
  frame <- clean_frame()
  frame$onset[1] <- NA

  ndata <- suppressWarnings(clean_tbl(frame))
  row <- finding(diagnose(ndata), "missing", "onset")

  expect_equal(as.character(row$status), "warning")
  expect_equal(row$n_affected, 1)
  expect_equal(row$rows[[1]], 1L)
})

test_that("missingness is reported per stratum when there are strata", {
  frame <- clean_frame()
  frame$n[2] <- NA_integer_ # the NA is in an M row

  result <- diagnose(suppressWarnings(clean_tbl(frame)), by_strata = TRUE)

  expect_equal(finding(result, "missing", "n", stratum = "M")$n_affected, 1)
  expect_equal(finding(result, "missing", "n", stratum = "F")$n_affected, 0)
  expect_equal(finding(result, "missing", "n", stratum = "all")$n_affected, 1)
})

# Duplicates -------------------------------------------------------------------

test_that("genuine repeats on the full key are found and named", {
  frame <- rbind(clean_frame(), clean_frame()[1, ]) # one exact repeat

  ndata <- suppressWarnings(clean_tbl(frame))
  row <- finding(diagnose(ndata), "duplicates", "key")

  expect_equal(as.character(row$status), "warning")
  expect_equal(row$n_affected, 1)
  expect_equal(row$rows[[1]], 5L)
  # These rows really are duplicates, so `distinct()` is the right advice.
  expect_match(row$hint, "distinct")
})

test_that("rows split by an undeclared column are diagnosed as such", {
  # NOT the same defect: these two rows are genuinely distinct, they differ in
  # a column the object was never told about. Advising `distinct()` here would
  # delete real cases.
  frame <- clean_frame()
  frame$sex <- c("a", "a", "a", "a")
  extra <- frame[1, ]
  extra$sex <- "b"
  frame <- rbind(frame, extra)

  ndata <- suppressWarnings(clean_tbl(frame))
  row <- finding(diagnose(ndata), "duplicates", "key")

  expect_equal(as.character(row$status), "warning")
  expect_equal(row$n_affected, 1)
  expect_match(row$hint, "sex")
  expect_false(grepl("distinct", row$hint))
})

test_that("the duplicate check can be switched off, and skips line lists", {
  ndata <- clean_tbl()

  off <- finding(diagnose(ndata, warn_non_uniqueness = FALSE), "duplicates", "key")
  expect_equal(as.character(off$status), "skipped")

  linelist <- tbl_now(
    clean_frame()[rep(1:4, clean_frame()$n), c("onset", "report", "gender")],
    event_date = "onset", report_date = "report", strata = "gender",
    data_type = "linelist", now = as.Date("2024-01-05"), verbose = FALSE
  )
  expect_equal(
    as.character(finding(diagnose(linelist), "duplicates", "key")$status),
    "skipped"
  )
})

# Units ------------------------------------------------------------------------

test_that("weekly dates on two weekday grids are found, with the fix named", {
  # Mondays for the event, Wednesdays for the report: the delay is then 2/7 of
  # a week, which is what breaks a converter.
  frame <- data.frame(
    onset  = as.Date(c("2024-01-01", "2024-01-08", "2024-01-15")),
    report = as.Date(c("2024-01-10", "2024-01-17", "2024-01-24")),
    n      = c(1L, 2L, 3L)
  )
  ndata <- tbl_now(frame,
    event_date = "onset", report_date = "report", case_count = "n",
    data_type = "count-incidence", event_units = "weeks",
    report_units = "weeks", now = as.Date("2024-01-24"), verbose = FALSE,
    align_weeks = FALSE
  )
  result <- diagnose(ndata)

  grid <- finding(result, "units", "report_grid")
  expect_equal(as.character(grid$status), "note")
  expect_equal(grid$n_affected, 3)
  expect_match(grid$hint, "align_weeks")

  # The event column is on its own regular grid; only the pair disagrees.
  expect_equal(as.character(finding(result, "units", "event_grid")$status), "ok")

  fractional <- finding(result, "units", "delay")
  expect_equal(as.character(fractional$status), "note")
  expect_equal(fractional$n_affected, 3)
})

test_that("confirmation units that differ from the event units are flagged", {
  frame <- clean_frame()
  frame$result <- frame$report + 1
  frame$outcome <- "confirmed"

  ndata <- tbl_now(frame,
    event_date = "onset", report_date = "report", case_count = "n",
    confirmation_date = "result", confirmation_type = "outcome",
    data_type = "count-incidence",
    now = as.Date("2024-01-06"), verbose = FALSE
  )
  attr(ndata, "confirmation_units") <- "weeks" # days everywhere else

  row <- finding(diagnose(ndata), "units", "declared")
  expect_equal(as.character(row$status), "note")
  expect_match(row$message, "confirmation_units")
})

test_that("units that agree are reported as ok", {
  row <- finding(diagnose(clean_tbl()), "units", "declared")

  expect_equal(as.character(row$status), "ok")
  expect_equal(row$n_affected, 0)
})

# Negatives --------------------------------------------------------------------

test_that("a downward revision shows up as a negative increment", {
  # One event date, reported twice: the cumulative total goes 5 then 3, so the
  # de-accumulated increment is -2. Exactly one negative.
  frame <- data.frame(
    onset  = as.Date(c("2024-01-01", "2024-01-01", "2024-01-02")),
    report = as.Date(c("2024-01-01", "2024-01-02", "2024-01-02")),
    n      = c(5L, 3L, 4L)
  )
  ndata <- tbl_now(frame,
    event_date = "onset", report_date = "report", case_count = "n",
    data_type = "count-cumulative", now = as.Date("2024-01-02"),
    verbose = FALSE
  )

  row <- finding(diagnose(ndata), "negatives", "increment")
  expect_equal(as.character(row$status), "note")
  expect_equal(row$n_affected, 1)
  # The indices would be the de-accumulated view's rows, not this object's.
  expect_equal(row$rows[[1]], integer(0))
})

test_that("a negative incidence count names the row it is on", {
  frame <- clean_frame()
  frame$n[3] <- -2L

  ndata <- clean_tbl(frame)
  row <- finding(diagnose(ndata), "negatives", "count", stratum = "all")

  expect_equal(as.character(row$status), "note")
  expect_equal(row$n_affected, 1)
  expect_equal(row$rows[[1]], 3L)
})

test_that("a line list has no counts to go negative", {
  linelist <- tbl_now(
    clean_frame()[rep(1:4, clean_frame()$n), c("onset", "report")],
    event_date = "onset", report_date = "report", data_type = "linelist",
    now = as.Date("2024-01-05"), verbose = FALSE
  )

  expect_equal(
    as.character(finding(diagnose(linelist), "negatives", "count")$status),
    "skipped"
  )
})

# now --------------------------------------------------------------------------

test_that("an event dated after now is a note naming the row", {
  frame <- clean_frame()
  frame$onset[2] <- as.Date("2024-02-01") # after now, and after its report

  ndata <- suppressWarnings(clean_tbl(frame))
  row <- finding(diagnose(ndata), "now", "event_date")

  expect_equal(as.character(row$status), "note")
  expect_equal(row$n_affected, 1)
  expect_equal(row$rows[[1]], 2L)
})

test_that("a confirmation after now is an error", {
  frame <- clean_frame()
  frame$result <- frame$report
  frame$outcome <- "confirmed"
  ndata <- tbl_now(frame,
    event_date = "onset", report_date = "report", case_count = "n",
    confirmation_date = "result", confirmation_type = "outcome",
    data_type = "count-incidence", now = as.Date("2024-01-05"), verbose = FALSE
  )
  attr(ndata, "now") <- as.Date("2024-01-04")

  row <- finding(diagnose(ndata), "now", "confirmation_date")
  expect_equal(as.character(row$status), "error")
  expect_equal(row$n_affected, 1)
})

test_that("the gap to now is read off the triangle, per stratum", {
  result <- diagnose(clean_tbl(), by_strata = TRUE)

  # F reports up to 2024-01-05 = now; M's last event is 2024-01-02, three days
  # earlier.
  expect_equal(finding(result, "now", "now_gap_event", stratum = "F")$n_affected, 0)
  expect_equal(finding(result, "now", "now_gap_event", stratum = "M")$n_affected, 3)
})

# Truncation -------------------------------------------------------------------

test_that("recent event dates are reported as still filling in", {
  # Delays are 0 (6 cases) and 2 (4 cases), so the 95th percentile is 2 and the
  # maturity cutoff is 2024-01-03. Exactly one event date (2024-01-05) is
  # younger than that.
  row <- finding(diagnose(clean_tbl(), by_strata = FALSE), "truncation", "event_date")

  expect_equal(as.character(row$status), "note")
  expect_equal(row$n_affected, 1)
  expect_match(row$hint, "right-truncation")
})

# Strata -----------------------------------------------------------------------

test_that("the smallest stratum is named rather than thresholded", {
  # F has 6 of the 10 cases, M has 4.
  row <- finding(diagnose(clean_tbl()), "strata", "size", stratum = "M")

  expect_equal(as.character(row$status), "note")
  expect_equal(row$n_affected, 4)
  expect_equal(row$n_total, 10)
  expect_equal(row$prop, 0.4)
  expect_match(row$message, "smallest stratum")
})

test_that("an unstratified object skips the stratum comparison", {
  ndata <- tbl_now(clean_frame(),
    event_date = "onset", report_date = "report", case_count = "n",
    data_type = "count-incidence", now = as.Date("2024-01-05"), verbose = FALSE
  )

  expect_equal(
    as.character(finding(diagnose(ndata), "strata", "size")$status), "skipped"
  )
})

test_that("pending confirmations are counted, not thresholded away", {
  frame <- clean_frame()
  frame$result <- as.Date(c("2024-01-02", "2024-01-04", NA, NA))
  frame$outcome <- c("confirmed", "confirmed", "pending", "pending")

  ndata <- tbl_now(frame,
    event_date = "onset", report_date = "report", case_count = "n",
    confirmation_date = "result", confirmation_type = "outcome",
    data_type = "count-incidence", now = as.Date("2024-01-05"), verbose = FALSE
  )

  # Rows 3 and 4 are pending: 3 + 4 = 7 of the 10 cases.
  row <- finding(diagnose(ndata), "strata", "pending", stratum = "all")
  expect_equal(as.character(row$status), "note")
  expect_equal(row$n_affected, 7)
  expect_equal(row$n_total, 10)
})

# Signposts --------------------------------------------------------------------

test_that("the statistical questions are signposted, never answered", {
  result <- diagnose(clean_tbl())
  signposts <- result[result$check == "signposts", ]

  expect_equal(nrow(signposts), 4)
  expect_setequal(
    signposts$scope,
    c("report", "confirmation", "report_batches", "confirmation_batches")
  )
  # Without a confirmation process the two confirmation axes cannot be asked.
  expect_equal(
    as.character(signposts$status[signposts$scope == "confirmation"]), "skipped"
  )
  expect_equal(
    as.character(signposts$status[signposts$scope == "report_batches"]), "not_run"
  )
  expect_match(
    signposts$message[signposts$scope == "report_batches"],
    "diagnose_batches", fixed = TRUE
  )
})

test_that("the drift signpost names the package it needs", {
  skip_if(requireNamespace("modifiedmk", quietly = TRUE))

  row <- finding(diagnose(clean_tbl()), "signposts", "report")
  expect_equal(as.character(row$status), "skipped")
  expect_match(row$hint, "modifiedmk")
})

# Declarations -----------------------------------------------------------------

test_that("an undeclared column is a note, with both ways out", {
  frame <- clean_frame()
  frame$sex <- c("a", "b", "a", "b")

  ndata <- suppressWarnings(clean_tbl(frame))
  row <- finding(diagnose(ndata), "declarations", "undeclared")

  expect_equal(as.character(row$status), "note")
  expect_equal(row$n_affected, 1)
  expect_match(row$hint, "strata")
  expect_match(row$hint, "to_count")
})

test_that("temporal effects that were never materialised are a note", {
  ndata <- add_temporal_effects(
    clean_tbl(),
    t_effects = temporal_effects(day_of_week = TRUE)
  )

  pending <- finding(diagnose(ndata), "declarations", "temporal_effects")
  expect_equal(as.character(pending$status), "note")
  expect_match(pending$hint, "compute_temporal_effects")

  computed <- finding(
    diagnose(compute_temporal_effects(ndata)),
    "declarations", "temporal_effects"
  )
  expect_equal(as.character(computed$status), "ok")
})

test_that("attribute failures are errors, one row each", {
  broken <- clean_tbl()
  attr(broken, "event_units") <- "fortnights"

  row <- finding(diagnose(broken), "declarations", "event_units")
  expect_equal(as.character(row$status), "error")
  expect_match(row$message, "must be one of")
})

# validate_tbl_now() -----------------------------------------------------------
#
# The refactor moved validate_tbl_now()'s body onto the findings engine. These
# tests exist to prove that what it EMITS did not change with it.

test_that("validate_tbl_now() is silent and TRUE on a clean object", {
  ndata <- clean_tbl()

  expect_silent(result <- validate_tbl_now(ndata))
  expect_true(result)
  expect_invisible(validate_tbl_now(ndata))
})

test_that("validate_tbl_now() still warns about the same things", {
  ndata <- suppressWarnings(clean_tbl(dirty_frame()))

  expect_warning(validate_tbl_now(ndata), "report_date.*before.*event_date")
})

test_that("validate_tbl_now() still aborts on a structural error", {
  broken <- clean_tbl()
  attr(broken, "now") <- NULL
  expect_error(validate_tbl_now(broken), "Missing required attribute")

  wrong_type <- clean_tbl()
  attr(wrong_type, "data_type") <- "nonsense"
  expect_error(validate_tbl_now(wrong_type), "data_type.*must be")
})

test_that("validate_tbl_now() keeps warn_non_uniqueness off by default", {
  frame <- rbind(clean_frame(), clean_frame()[1, ])
  ndata <- suppressWarnings(clean_tbl(frame))

  expect_silent(validate_tbl_now(ndata))
  expect_warning(
    validate_tbl_now(ndata, warn_non_uniqueness = TRUE), "Non-unique"
  )
  # diagnose() defaults it the other way round.
  expect_equal(
    as.character(finding(diagnose(ndata), "duplicates", "key")$status), "warning"
  )
})

test_that("validate_tbl_now() reports the same-column case as a message", {
  frame <- clean_frame()
  ndata <- suppressWarnings(tbl_now(frame,
    event_date = "onset", report_date = "onset", case_count = "n",
    data_type = "count-incidence", now = as.Date("2024-01-05"), verbose = FALSE
  ))

  # A message, not a warning: it has always been one, and promoting it would
  # change what tbl_now() does on data it has always accepted.
  expect_message(validate_tbl_now(ndata), "same")
  expect_equal(
    as.character(finding(diagnose(ndata), "declarations", "same_columns")$status),
    "note"
  )
})

test_that("validate_tbl_now() does not emit the notes diagnose() adds", {
  frame <- clean_frame()
  frame$sex <- c("a", "b", "a", "b") # an undeclared column: a note, never a warning

  ndata <- suppressWarnings(clean_tbl(frame))

  expect_silent(validate_tbl_now(ndata))
  expect_equal(
    as.character(finding(diagnose(ndata), "declarations", "undeclared")$status),
    "note"
  )
})

test_that("validate_tbl_now() warns when a confirmation precedes its report", {
  frame <- clean_frame()
  frame$result <- frame$report
  frame$result[2] <- frame$report[2] - 1 # confirmed before it was reported
  frame$outcome <- "confirmed"

  ndata <- suppressWarnings(tbl_now(frame,
    event_date = "onset", report_date = "report", case_count = "n",
    confirmation_date = "result", confirmation_type = "outcome",
    data_type = "count-incidence", now = as.Date("2024-01-05"), verbose = FALSE
  ))

  expect_warning(validate_tbl_now(ndata), "confirmed BEFORE")
})
