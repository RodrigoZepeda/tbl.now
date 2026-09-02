# The two validation-process columns -- issues #53 and #54.
#
#   #53  `is_censored_validation`, the validation-axis twin of
#        `is_censored_report`: the delay from report to resolution is a bound,
#        not a measurement.
#   #54  `validation_type` may hold only "confirmed", "retracted", "pending"
#        or `NA`, with `validation_levels` as the way to get data recorded in
#        another language into those four.
#
# The point of most of what follows is that the columns survive an ordinary
# `dplyr` pipeline. Every verb that rebuilds a `tbl_now` by hand is a place an
# attribute can be dropped in silence, and silence is exactly the failure mode.

validated_linelist <- function() {
  cases <- data.frame(
    onset   = as.Date("2021-01-04") + rep(0:4, each = 2),
    visit   = as.Date("2021-01-05") + rep(0:4, each = 2),
    result  = as.Date("2021-01-05") + rep(0:4, each = 2) +
      c(1, 40, 2, 1, 60, 3, 1, 2, 90, 1),
    outcome = rep(c("confirmed", "retracted"), 5),
    sex     = rep(c("F", "M"), each = 5),
    stringsAsFactors = FALSE
  )
  tbl_now(cases,
    event_date = "onset", report_date = "visit",
    validation_date = "result", validation_type = "outcome",
    strata = "sex", data_type = "linelist", verbose = FALSE
  )
}

validated_counts <- function() {
  cases <- data.frame(
    onset   = as.Date("2021-01-04") + rep(0:3, each = 2),
    visit   = as.Date("2021-01-05") + rep(0:3, each = 2),
    result  = as.Date("2021-01-06") + rep(0:3, each = 2),
    outcome = rep(c("confirmed", "retracted"), 4),
    n       = c(5L, 1L, 7L, 2L, 9L, 3L, 4L, 1L),
    stringsAsFactors = FALSE
  )
  tbl_now(cases,
    event_date = "onset", report_date = "visit",
    validation_date = "result", validation_type = "outcome",
    case_count = "n", data_type = "count-incidence", verbose = FALSE
  )
}

# ---------------------------------------------------------------------------
# #53: the column exists, and holds what it says
# ---------------------------------------------------------------------------

test_that("is_censored_validation holds TRUE exactly where the delay is long", {
  flu <- validated_linelist()
  censored <- suppressMessages(censor_validation_delays_above(flu, 30))

  flag <- get_is_censored_validation(censored)
  expect_identical(flag, ".is_censored_validation")
  expect_type(censored[[flag]], "logical")
  expect_false(anyNA(censored[[flag]]))

  # The definition, restated independently of the implementation.
  expect_identical(censored[[flag]], censored$.validation_delay > 30)
  expect_equal(sum(censored[[flag]]), 3L)

  # It is a bound on the DELAY, so nothing about the case changes.
  expect_identical(censored$result, flu$result)
  expect_identical(censored$outcome, flu$outcome)
  expect_identical(censored$.validation_delay, flu$.validation_delay)
})

test_that("the two censoring axes are independent", {
  flu <- validated_linelist()
  both <- suppressMessages(
    censor_reporting_delays_above(censor_validation_delays_above(flu, 30), 0)
  )

  expect_identical(get_is_censored_report(both), ".is_censored_report")
  expect_identical(get_is_censored_validation(both), ".is_censored_validation")
  # Every report here takes one day, so the report axis flags everything and
  # the validation axis only the three stragglers.
  expect_true(all(both[[get_is_censored_report(both)]]))
  expect_equal(sum(both[[get_is_censored_validation(both)]]), 3L)
})

test_that("add/change/remove_is_censored_validation round-trip", {
  flu <- validated_linelist()
  flu$slow <- flu$.validation_delay > 30

  added <- add_is_censored_validation(flu, slow)
  expect_identical(get_is_censored_validation(added), "slow")

  # add_*() refuses to overwrite; change_*() replaces.
  expect_error(add_is_censored_validation(added, slow), "Already has value")

  added$slower <- flu$.validation_delay > 50
  changed <- change_is_censored_validation(added, slower)
  expect_identical(get_is_censored_validation(changed), "slower")

  expect_null(get_is_censored_validation(remove_is_censored_validation(changed)))
})

test_that("the flag is refused where there is no validation delay to bound", {
  plain <- tbl_now(
    data.frame(
      onset = as.Date("2021-01-04") + 0:4,
      visit = as.Date("2021-01-05") + 0:4,
      slow  = rep(c(TRUE, FALSE), length.out = 5)
    ),
    event_date = "onset", report_date = "visit",
    data_type = "linelist", verbose = FALSE
  )

  expect_error(
    add_is_censored_validation(plain, slow), "no validation process"
  )
  expect_error(
    tbl_now(
      data.frame(
        onset = as.Date("2021-01-04") + 0:4,
        visit = as.Date("2021-01-05") + 0:4,
        slow  = rep(c(TRUE, FALSE), length.out = 5)
      ),
      event_date = "onset", report_date = "visit",
      is_censored_validation = "slow",
      data_type = "linelist", verbose = FALSE
    ),
    "without a .*validation_date"
  )

  # And it has to be a logical column, like its report-axis twin.
  flu <- validated_linelist()
  flu$note <- "slow"
  expect_error(add_is_censored_validation(flu, note), "must be logical")
})

test_that("the flag is protected: dropping the column demotes the object", {
  flu <- suppressMessages(censor_validation_delays_above(validated_linelist(), 30))
  flag <- get_is_censored_validation(flu)

  expect_true(flag %in% get_protected_cols(flu))
  demoted <- suppressWarnings(dplyr::select(flu, -dplyr::all_of(flag)))
  expect_false(is_tbl_now(demoted))
})

test_that("remove_validation_date() takes the generated flag with it", {
  flu <- suppressMessages(censor_validation_delays_above(validated_linelist(), 30))
  stripped <- remove_validation_date(flu)

  expect_false(has_validation(stripped))
  expect_null(get_is_censored_validation(stripped))
  expect_false(".is_censored_validation" %in% colnames(stripped))
})

# ---------------------------------------------------------------------------
# #53: it survives dplyr
# ---------------------------------------------------------------------------

test_that("the flag survives row-wise dplyr verbs, values and all", {
  flu <- suppressMessages(censor_validation_delays_above(validated_linelist(), 30))
  flag <- get_is_censored_validation(flu)

  verbs <- list(
    filter = dplyr::filter(flu, .data$outcome == "confirmed"),
    arrange = dplyr::arrange(flu, dplyr::desc(.data$onset)),
    slice = dplyr::slice(flu, 1:6),
    mutate = dplyr::mutate(flu, extra = 1L),
    relocate = dplyr::relocate(flu, dplyr::all_of(flag)),
    ungrouped = dplyr::ungroup(dplyr::group_by(flu, .data$sex))
  )

  for (name in names(verbs)) {
    out <- verbs[[name]]
    expect_true(is_tbl_now(out), info = name)
    expect_identical(get_is_censored_validation(out), flag, info = name)
    # The values travel with their rows rather than being recomputed.
    expect_identical(out[[flag]], out$.validation_delay > 30, info = name)
  }

  # And the flag still points at the same column after a rename that leaves it
  # alone.
  renamed <- dplyr::rename(flu, day_of_onset = "onset")
  expect_identical(get_is_censored_validation(renamed), flag)
})

test_that("the flag survives summarise(), and keeps censored rows apart", {
  counts <- suppressMessages(censor_validation_delays_above(validated_counts(), 1))
  flag <- get_is_censored_validation(counts)

  summarised <- counts |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(
      "onset", "visit", "result", "outcome", flag,
      ".event_num", ".report_num", ".delay", ".validation_num",
      ".validation_delay"
    )))) |>
    dplyr::summarise(n = sum(.data$n), .groups = "drop")
  expect_true(is_tbl_now(summarised))
  expect_identical(get_is_censored_validation(summarised), flag)
})

test_that("to_count() does not pool a censored resolution with an exact one", {
  # Two rows sharing an (event, report, outcome) triple, differing ONLY in the
  # flag. Pooling them would report a bound as a fact.
  cases <- data.frame(
    onset   = as.Date("2021-01-04") + c(0, 0, 1, 2),
    visit   = as.Date("2021-01-05") + c(0, 0, 1, 2),
    result  = as.Date("2021-01-06") + c(0, 40, 1, 2),
    outcome = rep("confirmed", 4),
    stringsAsFactors = FALSE
  )
  flu <- tbl_now(cases,
    event_date = "onset", report_date = "visit",
    validation_date = "result", validation_type = "outcome",
    data_type = "linelist", verbose = FALSE
  )
  flagged <- suppressMessages(censor_validation_delays_above(flu, 30))

  counts <- suppressMessages(to_count(flagged, to = "count-incidence"))
  expect_identical(get_is_censored_validation(counts), ".is_censored_validation")
  # The first two cases share (event, report, outcome) and differ ONLY in the
  # flag, so they stay two rows rather than becoming one count of 2.
  first_day <- counts[counts$onset == as.Date("2021-01-04"), ]
  expect_equal(nrow(first_day), 2L)
  expect_equal(sort(first_day[[".is_censored_validation"]]), c(FALSE, TRUE))
  expect_equal(sum(counts$n), 4)
})

test_that("the flag survives update()", {
  counts <- suppressMessages(censor_validation_delays_above(validated_counts(), 1))
  flag <- get_is_censored_validation(counts)

  new_rows <- data.frame(
    onset = as.Date("2021-01-09"), visit = as.Date("2021-01-10"),
    result = as.Date("2021-01-11"), outcome = "confirmed", n = 2L,
    stringsAsFactors = FALSE
  )
  new_rows[[flag]] <- FALSE

  merged <- suppressMessages(suppressWarnings(
    stats::update(counts, new_data = new_rows, remove_duplicates = FALSE)
  ))
  expect_identical(get_is_censored_validation(merged), flag)
  expect_equal(nrow(merged), nrow(counts) + 1L)
  expect_equal(sum(merged$n), sum(counts$n) + 2L)
})

# ---------------------------------------------------------------------------
# #54: only four values, and how to get there from another language
# ---------------------------------------------------------------------------

test_that("validation_type only ever holds the four allowed values", {
  flu <- validated_linelist()
  expect_true(all(flu$outcome %in% c("confirmed", "retracted", "pending")))

  bad <- data.frame(
    onset = as.Date("2021-01-04") + 0:2,
    visit = as.Date("2021-01-05") + 0:2,
    result = as.Date("2021-01-06") + 0:2,
    outcome = c("confirmed", "maybe", "resolved"),
    stringsAsFactors = FALSE
  )
  expect_error(
    tbl_now(bad,
      event_date = "onset", report_date = "visit",
      validation_date = "result", validation_type = "outcome",
      data_type = "linelist", verbose = FALSE
    ),
    "unrecognised value"
  )
  # The error names the way out.
  expect_error(
    tbl_now(bad,
      event_date = "onset", report_date = "visit",
      validation_date = "result", validation_type = "outcome",
      data_type = "linelist", verbose = FALSE
    ),
    "validation_levels"
  )
})

test_that("a value poked in after construction is caught", {
  flu <- validated_linelist()
  flu$outcome[2] <- "resuelto"

  # The findings engine sees it ...
  findings <- suppressWarnings(diagnose(flu))
  offending <- findings[
    findings$check == "now" & findings$scope == "validation_type",
  ]
  expect_equal(as.character(offending$status), "error")

  # ... and so does every rebuild that goes back through the constructor.
  bare <- suppressWarnings(
    as.data.frame(flu[, c("onset", "visit", "result", "outcome")])
  )
  expect_error(
    tbl_now(bare,
      event_date = "onset", report_date = "visit",
      validation_date = "result", validation_type = "outcome",
      data_type = "linelist", verbose = FALSE
    ),
    "unrecognised value"
  )
})

test_that("validation_levels translates the labels and keeps the dictionary", {
  spanish <- data.frame(
    inicio = as.Date("2021-01-04") + 0:4,
    visita = as.Date("2021-01-05") + 0:4,
    resultado = as.Date("2021-01-07") + 0:4,
    desenlace = c(
      "confirmado", "retractado", "confirmado", "pendiente", "confirmado"
    ),
    stringsAsFactors = FALSE
  )
  spanish$resultado[spanish$desenlace == "pendiente"] <- as.Date(NA)

  dictionary <- c(
    confirmado = "confirmed", retractado = "retracted", pendiente = "pending"
  )
  tn <- tbl_now(spanish,
    event_date = "inicio", report_date = "visita",
    validation_date = "resultado", validation_type = "desenlace",
    validation_levels = dictionary,
    data_type = "linelist", verbose = FALSE
  )

  # The column is rewritten; the dictionary records what it was.
  expect_identical(
    tn$desenlace,
    c("confirmed", "retracted", "confirmed", "pending", "confirmed")
  )
  expect_identical(get_validation_levels(tn), dictionary)

  # And the rest of the machinery now works on it.
  expect_equal(sum(get_latest_confirmed(tn)[["n"]]), 3)
  expect_equal(sum(get_net_confirmed(tn)[["n"]]), 2)
})

test_that("the dictionary survives dplyr, and recoding does not run twice", {
  spanish <- data.frame(
    inicio = as.Date("2021-01-04") + rep(0:4, each = 2),
    visita = as.Date("2021-01-05") + rep(0:4, each = 2),
    resultado = as.Date("2021-01-07") + rep(0:4, each = 2),
    desenlace = rep(c("confirmado", "retractado"), 5),
    stringsAsFactors = FALSE
  )
  dictionary <- c(confirmado = "confirmed", retractado = "retracted")
  tn <- tbl_now(spanish,
    event_date = "inicio", report_date = "visita",
    validation_date = "resultado", validation_type = "desenlace",
    validation_levels = dictionary,
    data_type = "linelist", verbose = FALSE
  )

  verbs <- list(
    filter = dplyr::filter(tn, .data$desenlace == "confirmed"),
    arrange = dplyr::arrange(tn, dplyr::desc(.data$inicio)),
    mutate = dplyr::mutate(tn, extra = 1L),
    ungrouped = dplyr::ungroup(dplyr::group_by(tn, .data$desenlace))
  )
  for (name in names(verbs)) {
    out <- verbs[[name]]
    expect_identical(get_validation_levels(out), dictionary, info = name)
    # Idempotence: a second pass through the dictionary must not move a
    # canonical value anywhere.
    expect_true(
      all(out$desenlace %in% c("confirmed", "retracted", "pending")),
      info = name
    )
  }

  counted <- suppressMessages(to_count(tn, to = "count-incidence"))
  expect_identical(get_validation_levels(counted), dictionary)
  expect_true(all(counted$desenlace %in% c("confirmed", "retracted")))
})

test_that("a malformed dictionary is refused", {
  spanish <- data.frame(
    inicio = as.Date("2021-01-04") + 0:2,
    visita = as.Date("2021-01-05") + 0:2,
    resultado = as.Date("2021-01-07") + 0:2,
    desenlace = c("confirmado", "retractado", "confirmado"),
    stringsAsFactors = FALSE
  )
  build <- function(levels) {
    tbl_now(spanish,
      event_date = "inicio", report_date = "visita",
      validation_date = "resultado", validation_type = "desenlace",
      validation_levels = levels,
      data_type = "linelist", verbose = FALSE
    )
  }

  expect_error(build(c("confirmed", "retracted")), "NAMED character vector")
  expect_error(
    build(c(confirmado = "confirmed", confirmado = "retracted")),
    "more than once"
  )
  expect_error(
    build(c(confirmado = "positivo", retractado = "retracted")),
    "not an outcome"
  )
  # A dictionary that renames a canonical value into a different one would
  # flip the column on every rebuild.
  expect_error(
    build(c(confirmado = "confirmed", retractado = "retracted",
            confirmed = "retracted")),
    "not repeatable"
  )
  # Mapping a canonical value to ITSELF is harmless and allowed.
  expect_s3_class(
    build(c(confirmado = "confirmed", retractado = "retracted",
            pending = "pending")),
    "tbl_now"
  )
})

test_that("a dictionary needs a validation date to translate", {
  expect_error(
    tbl_now(
      data.frame(
        onset = as.Date("2021-01-04") + 0:2,
        visit = as.Date("2021-01-05") + 0:2
      ),
      event_date = "onset", report_date = "visit",
      validation_levels = c(confirmado = "confirmed"),
      data_type = "linelist", verbose = FALSE
    ),
    "without a .*validation_date"
  )
})

test_that("covid_us carries a real validation process (#52)", {
  data(covid_us, envir = environment())

  expect_true(all(
    c("onset_dt", "pos_spec_dt", "cdc_report_dt", "current_status", "sex", "n")
      %in% colnames(covid_us)
  ))
  # The timeline the dataset promises.
  expect_true(all(covid_us$onset_dt <= covid_us$pos_spec_dt))
  expect_true(all(covid_us$pos_spec_dt <= covid_us$cdc_report_dt))
  expect_setequal(
    unique(covid_us$current_status),
    c("Laboratory-confirmed case", "Probable Case")
  )

  # CDC's labels are not this package's, which is what `validation_levels` is
  # for. This is the example the help page and the vignette use.
  tn <- tbl_now(covid_us,
    event_date = "onset_dt", report_date = "pos_spec_dt",
    validation_date = "cdc_report_dt", validation_type = "current_status",
    validation_levels = c(
      "Laboratory-confirmed case" = "confirmed", "Probable Case" = "pending"
    ),
    case_count = "n", strata = "sex",
    data_type = "count-incidence", verbose = FALSE
  )

  expect_true(has_validation(tn))
  expect_setequal(unique(tn$current_status), c("confirmed", "pending"))
  expect_true(all(tn$.validation_delay >= 0))
  # Every confirmed case is counted, and there are fewer of them than reports.
  expect_lt(
    sum(get_latest_confirmed(tn)[["n"]]),
    sum(get_latest_reported_cases(tn)[["n"]])
  )
})

# ---- Grouped objects --------------------------------------------------------
#
# `add_is_censored_validation()` refuses a `grouped_tbl_now`, so a verb that
# writes the flag has to ungroup and regroup around the write. Nothing about a
# grouping should change which rows are censored.

test_that("censor_validation_delays_above works on a grouped tbl_now", {
  flu <- validated_linelist()
  grouped <- flu |> dplyr::group_by(!!as.symbol("sex"))

  ungrouped_result <- suppressMessages(censor_validation_delays_above(flu, 30))
  grouped_result <- suppressMessages(censor_validation_delays_above(grouped, 30))

  expect_true(is_tbl_now(grouped_result))
  expect_equal(dplyr::group_vars(grouped_result), "sex")
  expect_equal(
    grouped_result[[get_is_censored_validation(grouped_result)]],
    ungrouped_result[[get_is_censored_validation(ungrouped_result)]]
  )
  # Grouping by something else must not change the answer either.
  by_outcome <- suppressMessages(
    censor_validation_delays_above(
      flu |> dplyr::group_by(!!as.symbol("outcome")), 30
    )
  )
  expect_equal(
    by_outcome[[get_is_censored_validation(by_outcome)]],
    ungrouped_result[[get_is_censored_validation(ungrouped_result)]]
  )
})

test_that("both censoring axes can be set on one grouped object", {
  grouped <- validated_linelist() |> dplyr::group_by(!!as.symbol("sex"))

  both <- suppressMessages(
    censor_reporting_delays_above(censor_validation_delays_above(grouped, 30), 0)
  )

  expect_equal(dplyr::group_vars(both), "sex")
  expect_equal(get_is_censored_report(both), ".is_censored_report")
  expect_equal(get_is_censored_validation(both), ".is_censored_validation")
  # The two axes are independent: neither write clears the other.
  expect_true(any(both[[".is_censored_report"]]))
  expect_true(any(both[[".is_censored_validation"]]))
})

test_that("an existing validation flag is merged, never cleared, when grouped", {
  flu <- suppressMessages(censor_validation_delays_above(validated_linelist(), 30))
  strict <- flu[[".is_censored_validation"]]
  expect_true(any(strict))

  # A later, looser threshold flags nothing new and must un-censor nothing.
  loose <- suppressMessages(
    censor_validation_delays_above(
      flu |> dplyr::group_by(!!as.symbol("sex")), 1000
    )
  )
  expect_equal(loose[[".is_censored_validation"]], strict)
  expect_equal(dplyr::group_vars(loose), "sex")
})
