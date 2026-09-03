# `units` is the shared default for `event_units`, `report_units` and
# `validation_units`. The point of the tests below is that it is *only* a
# default: anything given explicitly still wins, including `"auto"`.

library(dplyr, quietly = TRUE, warn.conflicts = FALSE)

make_daily <- function() {
  data.frame(
    onset = as.Date("2024-01-01") + 0:9,
    reported = as.Date("2024-01-02") + 0:9,
    resolved = as.Date("2024-01-04") + 0:9,
    outcome = rep("confirmed", 10),
    sex = rep(c("F", "M"), 5)
  )
}

make_weekly <- function() {
  data.frame(
    onset = as.Date("2024-01-07") + 7 * (0:9),
    reported = as.Date("2024-01-14") + 7 * (0:9)
  )
}

# ---- Wrong inputs -----------------------------------------------------------

test_that("an invalid `units` is rejected, naming `units` and not another argument", {
  df <- make_daily()
  expect_error(
    tbl_now(df, event_date = onset, report_date = reported, units = "fortnights"),
    "`units = \"fortnights\"`"
  )
  expect_error(
    tbl_now(df, event_date = onset, report_date = reported, units = c("days", "weeks")),
    "single string"
  )
  expect_error(
    tbl_now(df, event_date = onset, report_date = reported, units = NA_character_),
    "single string"
  )
  expect_error(
    tbl_now(df, event_date = onset, report_date = reported, units = 7),
    "single string"
  )
  expect_error(
    tbl_now(df, event_date = onset, report_date = reported, units = NULL),
    "single string"
  )
})

test_that("`units` is a real argument, so it is not swallowed as metadata", {
  x <- tbl_now(make_daily(),
    event_date = onset, report_date = reported,
    units = "days", verbose = FALSE
  )
  expect_null(attr(x, "units", exact = TRUE))
})

test_that("a near miss on `units` is flagged as a typo rather than stored quietly", {
  expect_warning(
    tbl_now(make_daily(),
      event_date = onset, report_date = reported, verbose = FALSE,
      unts = "days"
    ),
    "units"
  )
})

test_that("`units` is a formal, so R's partial matching reaches it", {
  # `unit =` never lands in `...`: it prefixes exactly one formal.
  x <- tbl_now(make_daily(),
    event_date = onset, report_date = reported, verbose = FALSE,
    unit = "days"
  )
  expect_equal(get_event_units(x), "days")
})

# ---- What `units` sets ------------------------------------------------------

test_that("`units` sets both date axes at once", {
  x <- tbl_now(make_daily(),
    event_date = onset, report_date = reported,
    units = "days", verbose = FALSE
  )
  expect_equal(get_event_units(x), "days")
  expect_equal(get_report_units(x), "days")
})

test_that("`units` sets the validation axis too", {
  x <- tbl_now(make_daily(),
    event_date = onset, report_date = reported,
    validation_date = resolved, validation_type = outcome,
    units = "days", verbose = FALSE
  )
  expect_equal(get_validation_units(x), "days")
  expect_true(has_validation(x))
})

test_that("an explicit axis argument beats `units`", {
  x <- tbl_now(make_daily(),
    event_date = onset, report_date = reported,
    units = "days", report_units = "weeks", verbose = FALSE
  )
  expect_equal(get_event_units(x), "days")
  expect_equal(get_report_units(x), "weeks")
})

test_that("an explicit validation_units beats `units`", {
  x <- tbl_now(make_daily(),
    event_date = onset, report_date = reported,
    validation_date = resolved, validation_type = outcome,
    units = "days", validation_units = "weeks", verbose = FALSE
  )
  expect_equal(get_event_units(x), "days")
  expect_equal(get_validation_units(x), "weeks")
})

test_that("an explicit \"auto\" still means infer, even when `units` says otherwise", {
  # Weekly dates declared `units = "weeks"` but with the event axis left to be
  # inferred: inference also says weeks, and the point is that it RAN.
  x <- tbl_now(make_weekly(),
    event_date = onset, report_date = reported,
    units = "weeks", event_units = "auto", verbose = FALSE
  )
  expect_equal(get_event_units(x), "weeks")

  # And the inference disagrees with `units` when the data disagrees.
  y <- tbl_now(make_daily(),
    event_date = onset, report_date = reported,
    units = "weeks", event_units = "auto", report_units = "auto", verbose = FALSE
  )
  expect_equal(get_event_units(y), "days")
  expect_equal(get_report_units(y), "days")
})

test_that("the default is unchanged: no `units` means inference on both axes", {
  x <- tbl_now(make_weekly(), event_date = onset, report_date = reported, verbose = FALSE)
  expect_equal(get_event_units(x), "weeks")
  expect_equal(get_report_units(x), "weeks")

  y <- tbl_now(make_daily(), event_date = onset, report_date = reported, verbose = FALSE)
  expect_equal(get_event_units(y), "days")
  expect_equal(get_report_units(y), "days")
})

test_that("`units` declares the grid inference cannot see", {
  # Two observations a month apart, which `infer_units()` would read as months.
  # Saying `units = "days"` is how a user overrides that in one place.
  df <- data.frame(
    onset = as.Date(c("2024-01-01", "2024-02-01")),
    reported = as.Date(c("2024-01-03", "2024-02-03"))
  )
  inferred <- tbl_now(df, event_date = onset, report_date = reported, verbose = FALSE)
  expect_equal(get_event_units(inferred), "months")

  declared <- tbl_now(df,
    event_date = onset, report_date = reported,
    units = "days", verbose = FALSE
  )
  expect_equal(get_event_units(declared), "days")
  expect_equal(declared$.delay, c(2, 2))
})

test_that("`units = \"numeric\"` works for a non-date axis", {
  df <- data.frame(event = 1:10, report = 1:10 + 2L)
  x <- tbl_now(df,
    event_date = event, report_date = report,
    units = "numeric", verbose = FALSE
  )
  expect_equal(get_event_units(x), "numeric")
  expect_equal(get_report_units(x), "numeric")
  expect_equal(x$.delay, rep(2, 10))
})

test_that("`units` still has to leave the report axis coarser than the event axis", {
  expect_error(
    tbl_now(make_daily(),
      event_date = onset, report_date = reported,
      units = "weeks", report_units = "days", verbose = FALSE
    ),
    "coarser"
  )
})

# ---- Through the other entry points ----------------------------------------

test_that("`units` reaches tbl_now() through as_tbl_now()", {
  x <- as_tbl_now(make_daily(),
    event_date = onset, report_date = reported,
    units = "days", verbose = FALSE
  )
  expect_equal(get_event_units(x), "days")
  expect_equal(get_report_units(x), "days")
})

test_that("`units` survives a grouped construction and a dplyr verb", {
  x <- tbl_now(make_daily(),
    event_date = onset, report_date = reported, strata = sex,
    units = "days", verbose = FALSE
  )
  grouped <- x |> group_by(sex) |> filter(!!as.symbol("sex") == "F")
  expect_equal(get_event_units(grouped), "days")
  expect_equal(get_report_units(grouped), "days")
})

test_that("mixed units survive `group_by()` and `ungroup()`", {
  x <- tbl_now(make_daily(),
    event_date = onset, report_date = reported, strata = sex,
    units = "days", report_units = "weeks", verbose = FALSE
  )
  expect_equal(get_report_units(x), "weeks")
  # The rebuild inside these verbs used to copy the EVENT units onto the report
  # axis, which quietly re-typed a mixed-unit object as a uniform one.
  expect_equal(get_report_units(x |> group_by(sex) |> ungroup()), "weeks")
  expect_equal(get_event_units(x |> group_by(sex) |> ungroup()), "days")

  # `group_by()` with nothing to group on takes a different branch, and so do
  # `summarise()` and `reframe()`. All three rebuilt the object by hand.
  expect_equal(get_report_units(x |> group_by()), "weeks")
  summarised <- x |>
    group_by(!!as.symbol("onset"), !!as.symbol("reported"), sex) |>
    summarise(n = dplyr::n(), .groups = "drop")
  expect_equal(get_report_units(summarised), "weeks")
  expect_equal(get_event_units(summarised), "days")
})

# ---- Second pass: the paths that build a date rather than read one ----------

test_that("`units` governs the axis reconstructed from a delay column", {
  df <- data.frame(
    reported = as.Date("2024-01-05") + 0:9,
    d = c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1)
  )
  x <- tbl_now(df,
    report_date = reported, delay = d, units = "days", verbose = FALSE
  )
  expect_equal(get_event_units(x), "days")
  expect_equal(x$.delay, df$d)
  # The delay is read in days, so the event date is `reported - d` days.
  expect_equal(x[[get_event_date(x)]], df$reported - df$d)
})

test_that("`units = \"weeks\"` reads a delay column as weeks, not days", {
  df <- data.frame(
    onset = as.Date("2024-01-07") + 7 * (0:9),
    d = rep(1, 10)
  )
  x <- tbl_now(df,
    event_date = onset, delay = d, units = "weeks", verbose = FALSE
  )
  expect_equal(get_event_units(x), "weeks")
  expect_equal(x[[get_report_date(x)]], df$onset + 7)
  expect_equal(x$.delay, rep(1, 10))
})

test_that("`units` is enough on its own where inference would refuse", {
  # One distinct event date: `infer_units()` cannot see a spacing at all.
  df <- data.frame(
    onset = rep(as.Date("2024-01-01"), 4),
    reported = as.Date("2024-01-01") + c(1, 2, 3, 4)
  )
  expect_error(
    tbl_now(df, event_date = onset, report_date = reported, verbose = FALSE),
    "single distinct date"
  )
  x <- tbl_now(df,
    event_date = onset, report_date = reported, units = "days", verbose = FALSE
  )
  expect_equal(get_event_units(x), "days")
  expect_equal(x$.delay, c(1, 2, 3, 4))
})

test_that("`units` reaches the update() rebuild too", {
  df <- make_daily()
  x <- tbl_now(df[1:5, ],
    event_date = onset, report_date = reported, strata = sex,
    units = "days", verbose = FALSE
  )
  updated <- update(x, new_data = df[6:10, ])

  expect_equal(get_event_units(updated), "days")
  expect_equal(get_report_units(updated), "days")
  expect_equal(nrow(updated), 10)
})

test_that("`units` and aggregate_time_units() tell the same story", {
  x <- tbl_now(make_daily(),
    event_date = onset, report_date = reported,
    units = "days", verbose = FALSE
  )
  weekly <- aggregate_time_units(x, to = "weeks", verbose = FALSE)

  # What `units` declares up front is what aggregation produces afterwards.
  expect_equal(get_event_units(weekly), "weeks")
  expect_equal(get_report_units(weekly), "weeks")
  expect_equal(
    get_event_units(tbl_now(make_weekly(),
      event_date = onset, report_date = reported,
      units = "weeks", verbose = FALSE
    )),
    get_event_units(weekly)
  )
})

test_that("`units` does not disturb an object that had none", {
  # The whole feature is a default, so an object built without it must be
  # byte-for-byte what it always was.
  df <- make_weekly()
  before <- tbl_now(df,
    event_date = onset, report_date = reported,
    event_units = "weeks", report_units = "weeks", verbose = FALSE
  )
  after <- tbl_now(df,
    event_date = onset, report_date = reported,
    units = "weeks", verbose = FALSE
  )
  expect_equal(tbl_now_attributes(before), tbl_now_attributes(after))
  expect_equal(as_tibble(before), as_tibble(after))
})
