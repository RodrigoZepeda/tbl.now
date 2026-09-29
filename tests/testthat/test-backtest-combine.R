# `backtest_combine()` joins backtests run separately. Combining what a single
# call would have produced must give that call's object -- the same rows in the
# same order -- so nothing downstream can tell the difference.

combine_data <- function(strata = FALSE) {
  df <- data.frame(
    onset = as.Date("2024-01-07") + rep(7 * (0:9), each = 6),
    report = as.Date("2024-01-07") + rep(7 * (0:9), each = 6) + rep(c(0, 7, 14), 2),
    gender = rep(c("F", "M"), each = 3)
  )
  tbl_now(df,
    event_date = onset, report_date = report,
    strata = if (strata) "gender" else NULL,
    data_type = "linelist", verbose = FALSE
  )
}

combine_dates <- as.Date(c("2024-02-18", "2024-03-03", "2024-03-10"))

narrow_engine <- function() example_engine(spread = 0.2, label = "narrow")
wide_engine <- function() example_engine(spread = 0.5, label = "wide")

combine_run <- function(x, ..., dates = combine_dates) {
  nowcast_backtest(x, ..., now_dates = dates, verbose = FALSE)
}

expect_same_but_elapsed <- function(a, b) {
  expect_equal(a$scores, b$scores)
  expect_equal(a$predictions, b$predictions)
  expect_equal(a$draws, b$draws)
  expect_equal(a$truth, b$truth)
  expect_equal(a$methods, b$methods)
  expect_equal(a$now_dates, b$now_dates)
  expect_equal(
    dplyr::select(a$timings, -"elapsed_seconds"),
    dplyr::select(b$timings, -"elapsed_seconds")
  )
  expect_equal(
    a[setdiff(names(a), c("scores", "predictions", "draws", "timings", "truth",
                          "methods", "now_dates"))],
    b[setdiff(names(b), c("scores", "predictions", "draws", "timings", "truth",
                          "methods", "now_dates"))]
  )
}

test_that("combining models on the same dates equals one joint backtest", {
  x <- combine_data()
  joint <- combine_run(x, narrow_engine(), wide_engine())
  combined <- backtest_combine(
    combine_run(x, narrow_engine()), combine_run(x, wide_engine())
  )
  expect_s3_class(combined, "nowcast_backtest")
  expect_same_but_elapsed(joint, combined)
  # Dates outer, engines inner, as in a single call.
  expect_equal(combined$timings$.method, rep(c("narrow", "wide"), 3))
})

test_that("combining the same model on different dates equals one backtest", {
  x <- combine_data()
  joint <- combine_run(x, narrow_engine())
  combined <- backtest_combine(
    combine_run(x, narrow_engine(), dates = combine_dates[1:2]),
    combine_run(x, narrow_engine(), dates = combine_dates[3])
  )
  expect_same_but_elapsed(joint, combined)

  # The order the backtests are given in does not change the rows.
  reversed <- backtest_combine(
    combine_run(x, narrow_engine(), dates = combine_dates[3]),
    combine_run(x, narrow_engine(), dates = combine_dates[1:2])
  )
  expect_same_but_elapsed(joint, reversed)
})

test_that("models and dates can be combined at once, with a list too", {
  x <- combine_data()
  joint <- combine_run(x, narrow_engine(), wide_engine())
  pieces <- list(
    combine_run(x, narrow_engine(), dates = combine_dates[1:2]),
    combine_run(x, wide_engine(), dates = combine_dates[1:2]),
    combine_run(x, narrow_engine(), wide_engine(), dates = combine_dates[3])
  )
  expect_same_but_elapsed(joint, do.call(backtest_combine, pieces))
  expect_same_but_elapsed(joint, backtest_combine(pieces))
})

test_that("strata and a grouped tbl_now combine like a joint backtest", {
  x <- combine_data(strata = TRUE)
  joint <- combine_run(x, narrow_engine(), wide_engine())
  combined <- backtest_combine(
    combine_run(dplyr::group_by(x, gender), narrow_engine()),
    combine_run(x, wide_engine())
  )
  expect_same_but_elapsed(joint, combined)
  expect_true("gender" %in% colnames(combined$scores))
})

test_that("a single backtest is returned as it is", {
  bt <- combine_run(combine_data(), narrow_engine())
  expect_same_but_elapsed(bt, backtest_combine(bt))
})

test_that("the combined backtest feeds the weights and prints", {
  x <- combine_data()
  combined <- backtest_combine(
    combine_run(x, narrow_engine()), combine_run(x, wide_engine())
  )
  joint <- combine_run(x, narrow_engine(), wide_engine())
  expect_equal(nowcast_weights(combined), nowcast_weights(joint))
  expect_equal(utils::capture.output(print(combined)),
               utils::capture.output(print(joint)))
})

test_that("a fit present in two backtests aborts", {
  x <- combine_data()
  first <- combine_run(x, narrow_engine(), dates = combine_dates[1:2])
  second <- combine_run(x, narrow_engine(), dates = combine_dates[2:3])
  expect_error(backtest_combine(first, second), "overlap")
  expect_error(backtest_combine(first, second), "narrow at 2024-03-03")
  expect_error(backtest_combine(first, first), "overlap")

  # Different methods at the same date are not an overlap.
  expect_no_error(backtest_combine(
    first, combine_run(x, wide_engine(), dates = combine_dates[1:2])
  ))
})

test_that("a success replaces a failure of the same fit", {
  x <- combine_data()
  complete <- combine_run(x, narrow_engine())

  # `failed` is `complete` as if the fit at the second date had failed.
  failed <- complete
  gone <- failed$scores$.now == combine_dates[[2]]
  failed$scores <- failed$scores[!gone, ]
  failed$predictions <- failed$predictions[failed$predictions$.now != combine_dates[[2]], ]
  failed$timings$success[failed$timings$.now == combine_dates[[2]]] <- FALSE
  failed$timings$error[failed$timings$.now == combine_dates[[2]]] <- "boom"

  redo <- combine_run(x, narrow_engine(), dates = combine_dates[[2]])
  combined <- backtest_combine(failed, redo)
  expect_same_but_elapsed(complete, combined)
  expect_true(all(combined$timings$success))

  # Two failures of one fit keep one row.
  failure <- failed$timings[failed$timings$.now == combine_dates[[2]], ]
  kept <- .combine_backtest_timings(list(
    list(timings = failure), list(timings = failure)
  ))
  expect_equal(nrow(kept), 1L)
  expect_false(kept$success)
})

test_that("backtests answering different questions are refused", {
  x <- combine_data()
  base <- combine_run(x, narrow_engine(), dates = combine_dates[1])
  other <- combine_run(x, wide_engine(), dates = combine_dates[2])

  for (field in c("event_date", "strata", "truth_axis", "truth_type", "keep_draws")) {
    changed <- other
    changed[[field]] <- if (field == "keep_draws") TRUE else "different"
    expect_error(backtest_combine(base, changed), field, info = field)
  }

  levels <- combine_run(
    x, example_engine(label = "few", quantile_levels = c(0.25, 0.5, 0.75)),
    dates = combine_dates[2]
  )
  expect_error(backtest_combine(base, levels), "quantile levels")
})

test_that("arguments are validated", {
  bt <- combine_run(combine_data(), narrow_engine())
  expect_error(backtest_combine(), "at least one")
  expect_error(backtest_combine(bt, "nope"), "nowcast_backtest")
  expect_error(backtest_combine(bt, data.frame()), "nowcast_backtest")
  expect_error(backtest_combine(bt, only_common_dates = NA), "only_common_dates")
  expect_error(backtest_combine(bt, only_common_dates = "yes"), "only_common_dates")
})

test_that("only_common_dates keeps the dates every method has", {
  x <- combine_data()
  narrow <- combine_run(x, narrow_engine())
  wide <- combine_run(x, wide_engine(), dates = combine_dates[2:3])

  all_dates <- backtest_combine(narrow, wide)
  expect_equal(all_dates$now_dates, combine_dates)

  common <- backtest_combine(narrow, wide, only_common_dates = TRUE)
  expect_equal(common$now_dates, combine_dates[2:3])
  for (table in c("scores", "predictions", "timings")) {
    expect_true(all(common[[table]]$.now %in% combine_dates[2:3]), info = table)
  }
  expect_equal(common$methods, c("narrow", "wide"))
  expect_same_but_elapsed(
    common, combine_run(x, narrow_engine(), wide_engine(), dates = combine_dates[2:3])
  )

  # Comparing methods with different dates uses the targets both scored.
  expect_warning(nowcast_weights(all_dates), "wide")
  expect_no_warning(nowcast_weights(common))

  disjoint <- backtest_combine(
    combine_run(x, narrow_engine(), dates = combine_dates[1]),
    combine_run(x, wide_engine(), dates = combine_dates[3])
  )
  expect_error(
    backtest_combine(disjoint, only_common_dates = TRUE),
    "every method"
  )
})

test_that("the truth of the latest backtest is used and disagreement warned", {
  x <- combine_data()
  early <- combine_run(x, narrow_engine(), dates = combine_dates[1])
  late <- combine_run(x, wide_engine(), dates = combine_dates[3])

  expect_no_warning(same <- backtest_combine(early, late))
  expect_equal(same$truth, late$truth)

  # The earlier backtest was scored against a revised truth.
  revised <- early
  revised$truth$.observed[[1]] <- revised$truth$.observed[[1]] + 10
  expect_warning(combined <- backtest_combine(revised, late), "disagree on 1")
  expect_equal(combined$truth, late$truth)

  # Event dates only the earlier backtest has are added.
  shorter <- late
  shorter$truth <- shorter$truth[-1, ]
  combined <- backtest_combine(early, shorter)
  expect_equal(combined$truth, early$truth)
})
