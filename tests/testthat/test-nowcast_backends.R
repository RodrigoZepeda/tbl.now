# The real backends. Only `baselinenowcast` is fast enough to run routinely;
# the Bayesian ones (epinowcast, diseasenowcasting, NobBS, nowcaster) need a
# Stan/JAGS/INLA toolchain and minutes per fit, so they are only checked for
# their guard rails here and exercised by hand in
# `vignette("ensemble-nowcasting")`.

backend_tbl_now <- function(strata = FALSE) {
  data(denguedat, package = "tbl.now", envir = environment())
  recent <- denguedat[denguedat$onset_week >= as.Date("2009-01-01"), ]

  tbl_now(recent,
    event_date = "onset_week", report_date = "report_week",
    strata = if (strata) "gender" else NULL,
    verbose = FALSE
  )
}

test_that("baselinenowcast produces draws on the object's own event dates", {
  skip_on_cran()
  skip_if_not_installed("baselinenowcast")

  x <- backend_tbl_now()
  nowcast <- suppressWarnings(
    run_nowcast(x, "baselinenowcast", draws = 100, verbose = FALSE)
  )

  expect_true(is_tbl_nowcast(nowcast))
  expect_equal(nowcast@method, "baselinenowcast")
  expect_false(is.null(nowcast@draws))
  expect_equal(length(unique(nowcast@draws$.draw)), 100)

  # The event dates come back as Dates that exist in the source object
  expect_s3_class(nowcast@predictions$onset_week, "Date")
  expect_true(all(nowcast@predictions$onset_week %in% x$onset_week))

  # Scoring the object against the data it was fitted on must line up
  scores <- score_nowcast(nowcast)
  expect_gt(nrow(scores), 0)
  expect_false(any(is.na(scores$wis)))
})

test_that("baselinenowcast nowcasts each stratum separately", {
  skip_on_cran()
  skip_if_not_installed("baselinenowcast")

  x <- backend_tbl_now(strata = TRUE)
  nowcast <- suppressWarnings(
    run_nowcast(x, "baselinenowcast", draws = 50, verbose = FALSE)
  )

  expect_equal(nowcast@strata, "gender")
  expect_setequal(unique(nowcast@predictions$gender), unique(x$gender))
  expect_true("gender" %in% colnames(nowcast@draws))
})

test_that("two baselinenowcast fits can be ensembled", {
  skip_on_cran()
  skip_if_not_installed("baselinenowcast")

  x <- backend_tbl_now()
  members <- lapply(1:2, function(i) {
    suppressWarnings(run_nowcast(x, "baselinenowcast", draws = 100, verbose = FALSE))
  })

  ensemble <- nowcast_ensemble(a = members[[1]], b = members[[2]], verbose = FALSE)
  pooled <- nowcast_ensemble(
    a = members[[1]], b = members[[2]],
    type = "linear_pool", n_draws = 200, verbose = FALSE
  )

  expect_true(is_tbl_nowcast(ensemble))
  expect_equal(nrow(ensemble@predictions), nrow(members[[1]]@predictions))
  expect_equal(length(unique(pooled@draws$.draw)), 200)
})

test_that("the line-list backends refuse aggregated data", {
  skip_if_not_installed("NobBS")

  counts <- tbl_now(
    data.frame(
      event_date = as.Date("2020-01-06") + c(0, 0, 7),
      report_date = as.Date("2020-01-06") + c(0, 7, 7),
      n = c(3L, 1L, 5L)
    ),
    event_date = "event_date", report_date = "report_date",
    case_count = "n", data_type = "count-incidence", verbose = FALSE
  )

  expect_error(run_nowcast(counts, "NobBS", verbose = FALSE), "line-list")
})

test_that("NobBS only accepts daily or weekly units", {
  skip_if_not_installed("NobBS")

  monthly <- tbl_now(
    data.frame(
      event_date = as.Date(c("2020-01-01", "2020-02-01")),
      report_date = as.Date(c("2020-02-01", "2020-03-01"))
    ),
    event_date = "event_date", report_date = "report_date",
    event_units = "months", report_units = "months",
    data_type = "linelist", verbose = FALSE
  )

  expect_error(run_nowcast(monthly, "NobBS", verbose = FALSE), "daily or weekly")
})

test_that("nowcaster reports the missing INLA dependency clearly", {
  skip_if_not_installed("nowcaster")
  skip_if(requireNamespace("INLA", quietly = TRUE), "INLA is installed")

  x <- backend_tbl_now()
  expect_error(run_nowcast(x, "nowcaster", verbose = FALSE), "INLA")
})

test_that("nowcaster returns a nowcast with draws when INLA is available", {
  skip_on_cran()
  skip_if_not_installed("nowcaster")
  skip_if_not_installed("INLA")

  x <- backend_tbl_now()
  nowcast <- suppressWarnings(run_nowcast(x, "nowcaster", verbose = FALSE))

  expect_true(is_tbl_nowcast(nowcast))
  expect_equal(nowcast@method, "nowcaster")
  # `trajectories = TRUE` (the default) means the posterior draws are kept
  expect_false(is.null(nowcast@draws))
  expect_gt(length(unique(nowcast@draws$.draw)), 1)
})

test_that("every built-in method has both extension methods registered", {
  builtin <- c("diseasenowcasting", "baselinenowcast", "epinowcast", "nowcaster", "NobBS")
  registered_fit <- sub("^nowcast_fit\\.", "", as.character(utils::methods("nowcast_fit")))
  registered_tidy <- sub("^nowcast_tidy\\.", "", as.character(utils::methods("nowcast_tidy")))

  expect_true(all(builtin %in% registered_fit))
  expect_true(all(builtin %in% registered_tidy))
})
