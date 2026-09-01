# Regression guard: every converter, against every dataset the package ships.
#
# This is the testthat counterpart of `data-raw/converter_matrix.R`. The matrix
# in the article is documentation; this is the thing that FAILS if a converter
# change breaks a dataset shape. The expectations record what works TODAY, so a
# regression shows up as a test failure rather than as a surprised user.
#
# The datasets themselves are built by `dataset_tbl_now()` in
# `helper-datasets.R`, shared with `test-engines-datasets.R`: a converter
# succeeding says nothing about whether the engine behind it can fit what the
# converter produced.

CONVERTERS <- list(
  baselinenowcast = function(x) tbl_now_to_baselinenowcast(x, verbose = FALSE),
  epinowcast      = function(x) tbl_now_to_epinowcast(x, verbose = FALSE, quiet = TRUE),
  epidist         = function(x) tbl_now_to_epidist(x, verbose = FALSE),
  surveillance    = function(x) tbl_now_to_surveillance(x, verbose = FALSE),
  data.table      = function(x) tbl_now_to_data_table(x, verbose = FALSE),
  tsibble         = function(x) tbl_now_to_tsibble(x, verbose = FALSE)
)

# Datasets whose shape every converter handles.
WORKING_DATASETS <- c(
  "denguedat", "hai_bucaramanga", "covid_colombia",
  "covid_us", "mpoxdat"
)

for (dataset in WORKING_DATASETS) {
  local({
    this_dataset <- dataset
    test_that(paste("every converter handles", this_dataset), {
      skip_on_cran()
      x <- dataset_tbl_now(this_dataset)
      expect_true(is_tbl_now(x))
      for (nm in names(CONVERTERS)) {
        skip_if_not_installed(nm)
        expect_no_error(
          suppressWarnings(suppressMessages(CONVERTERS[[nm]](x))),
          message = paste0(nm, " broke on ", this_dataset)
        )
      }
    })
  })
}

test_that("count-cumulative converts once the delays are whole periods", {
  skip_on_cran()
  skip_if_not_installed("baselinenowcast")

  # FluSight reports `as_of` on Saturdays AND Wednesdays while `target_end_date`
  # is always Saturday, so raw delays are fractional (0.571 weeks) and no
  # reporting triangle can be built. That is an alignment problem, NOT a
  # cumulative-data problem: `align_weeks = TRUE` fixes it.
  aligned <- dataset_tbl_now("flusight_aligned")
  expect_true(all(aligned$.delay == round(aligned$.delay)))

  triangle <- suppressWarnings(
    tbl_now_to_baselinenowcast(aligned, verbose = FALSE)
  )
  expect_s3_class(triangle, "reporting_triangle")

  # De-accumulating a cumulative series produces NEGATIVE increments wherever a
  # total was revised downward. They must be absorbed, not passed through.
  expect_equal(sum(triangle < 0, na.rm = TRUE), 0L)
})

test_that("`negatives = 'error'` refuses cumulative input outright", {
  skip_on_cran()
  skip_if_not_installed("baselinenowcast")
  flusight_now <- dataset_tbl_now("flusight")
  expect_equal(get_data_type(flusight_now), "count-cumulative")
  expect_error(
    suppressWarnings(
      tbl_now_to_baselinenowcast(flusight_now, negatives = "error", verbose = FALSE)
    ),
    "count-cumulative"
  )
})

test_that("count-cumulative converts for the other targets too", {
  skip_on_cran()
  flusight_now <- dataset_tbl_now("flusight")
  for (nm in c("epinowcast", "surveillance", "data.table", "tsibble")) {
    skip_if_not_installed(nm)
    expect_no_error(
      suppressWarnings(suppressMessages(CONVERTERS[[nm]](flusight_now))),
      message = paste0(nm, " broke on cumulative flusight")
    )
  }
})
