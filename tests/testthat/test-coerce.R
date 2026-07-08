# Tests for the as_tibble() / as.data.frame() methods on tbl_now, in particular
# the opt-in `compute_temporal_effects` argument that materialises the lazy
# temporal-effects spec.

make_spec_now <- function() {
  data(denguedat)
  tbl_now(
    denguedat,
    event_date = "onset_week", report_date = "report_week",
    strata = "gender", verbose = FALSE
  ) |>
    add_temporal_effects(temporal_effects(week_of_year = TRUE))
}

test_that("as_tibble() is lazy by default (does not materialise temporal effects)", {
  x <- make_spec_now()

  tb <- tibble::as_tibble(x)

  expect_s3_class(tb, "tbl_df")
  expect_false(is_tbl_now(tb))
  expect_false(".event_week_of_year" %in% names(tb))
  # input untouched
  expect_equal(get_temporal_effect_cols(x), character(0))
})

test_that("as.data.frame() is lazy by default", {
  x <- make_spec_now()

  df <- as.data.frame(x)

  expect_s3_class(df, "data.frame")
  expect_false(is_tbl_now(df))
  expect_false(".event_week_of_year" %in% names(df))
})

test_that("as_tibble(compute_temporal_effects = TRUE) materialises the spec", {
  x <- make_spec_now()

  tb <- tibble::as_tibble(x, compute_temporal_effects = TRUE)

  expect_true(".event_week_of_year" %in% names(tb))
  expect_false(is_tbl_now(tb))
  # the original object is left lazy (materialised on a copy)
  expect_equal(get_temporal_effect_cols(x), character(0))
})

test_that("as.data.frame(compute_temporal_effects = TRUE) materialises the spec", {
  x <- make_spec_now()

  df <- as.data.frame(x, compute_temporal_effects = TRUE)

  expect_true(".event_week_of_year" %in% names(df))
  expect_s3_class(df, "data.frame")
  expect_equal(get_temporal_effect_cols(x), character(0))
})

test_that("coercion works on a grouped_tbl_now", {
  x <- make_spec_now() |> dplyr::group_by(gender)

  # lazy by default
  expect_false(".event_week_of_year" %in% names(tibble::as_tibble(x)))
  # opt-in materialises, on ungrouped data (single global reference date, so no
  # per-group recycling warning)
  expect_no_warning(tb <- tibble::as_tibble(x, compute_temporal_effects = TRUE))
  expect_true(".event_week_of_year" %in% names(tb))
  expect_no_warning(df <- as.data.frame(x, compute_temporal_effects = TRUE))
  expect_true(".event_week_of_year" %in% names(df))
})

test_that("coercion of a tbl_now without a temporal spec keeps the same columns", {
  data(denguedat)
  x <- tbl_now(denguedat, event_date = "onset_week", report_date = "report_week",
               verbose = FALSE)

  plain  <- tibble::as_tibble(x)
  optin  <- tibble::as_tibble(x, compute_temporal_effects = TRUE)

  expect_identical(names(plain), names(optin))
})

test_that("dplyr verbs on a spec-carrying tbl_now stay lazy and do not recurse", {
  # Regression test: group_by() / to_count() / get_*_reported_cases() call
  # as_tibble()/as.data.frame() on the tbl_now internally (via dplyr's
  # compute_groups() and data mask). These must stay cheap declassers, otherwise
  # they materialise the spec and infinitely recurse.
  x <- make_spec_now()

  expect_equal(get_temporal_effect_cols(dplyr::group_by(x, gender)), character(0))

  expect_no_error(counted <- to_count(x, to = "count-cumulative"))
  expect_equal(get_temporal_effect_cols(counted), character(0))

  expect_no_error(gi <- get_initial_reported_cases(x))
  expect_no_error(gl <- get_latest_reported_cases(x))
  # spec is preserved lazily through these operations
  expect_true(length(get_temporal_effects(gi)) > 0)
  expect_equal(get_temporal_effect_cols(gi), character(0))
})
