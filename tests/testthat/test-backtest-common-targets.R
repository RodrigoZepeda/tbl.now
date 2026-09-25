# A failed fit leaves one method with fewer scored targets than the others.
# The print summary and the performance weights must then compare the methods
# on the targets all of them scored -- and say so -- rather than averaging each
# over its own dates.

common_data <- function(strata = FALSE) {
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

common_dates_used <- as.Date(c("2024-02-18", "2024-03-03"))

common_backtest <- function(strata = FALSE) {
  nowcast_backtest(common_data(strata),
    example_engine(spread = 0.2, label = "narrow"),
    example_engine(spread = 0.5, label = "wide"),
    now_dates = common_dates_used, verbose = FALSE
  )
}

# What a failed fit of `method` at `date` leaves behind.
drop_fit <- function(bt, method, date) {
  gone <- function(tbl) tbl$.method == method & tbl$.now == date
  bt$scores <- bt$scores[!gone(bt$scores), ]
  bt$predictions <- bt$predictions[!gone(bt$predictions), ]
  bt
}

mean_wis_weights <- function(scores) {
  mean_wis <- tapply(scores$wis, scores$.method, mean, na.rm = TRUE)
  weights <- 1 / mean_wis
  weights / sum(weights)
}

test_that("a complete backtest is unchanged and does not warn", {
  bt <- common_backtest()
  expect_no_warning(common <- nowcast_weights(bt))
  expect_equal(common, nowcast_weights(bt, common_dates = FALSE))
  expect_no_warning(utils::capture.output(print(bt)))
})

test_that("default weights use only the targets every method scored", {
  bt <- drop_fit(common_backtest(), "wide", common_dates_used[[2]])

  expect_warning(
    weights <- nowcast_weights(bt),
    "wide.*2024-03-03"
  )
  first_date <- bt$scores[bt$scores$.now == common_dates_used[[1]], ]
  expected <- mean_wis_weights(first_date)
  expect_equal(weights[names(expected)], c(expected), ignore_attr = TRUE)

  # Switched off: each method over its own targets, as before, and silent.
  expect_no_warning(own <- nowcast_weights(bt, common_dates = FALSE))
  expected_own <- mean_wis_weights(bt$scores)
  expect_equal(own[names(expected_own)], c(expected_own), ignore_attr = TRUE)
  expect_false(isTRUE(all.equal(weights, own)))
})

test_that("the warning counts the dropped rows and names only the gaps", {
  bt <- drop_fit(common_backtest(), "wide", common_dates_used[[2]])
  dropped <- sum(bt$scores$.now == common_dates_used[[2]])
  warning <- tryCatch(nowcast_weights(bt), warning = function(w) w)
  message <- conditionMessage(warning)
  expect_match(message, paste("dropped", dropped, "score row"))
  expect_match(message, "common_dates = FALSE", fixed = TRUE)
  expect_no_match(message, "narrow")
  expect_no_match(message, "2024-02-18")
})

test_that("optim weights warn too, and use the common targets either way", {
  bt <- drop_fit(common_backtest(), "wide", common_dates_used[[2]])
  expect_warning(optim <- nowcast_weights(bt, type = "optim"), "wide")
  expect_no_warning(
    optim_off <- nowcast_weights(bt, type = "optim", common_dates = FALSE)
  )
  expect_equal(optim, optim_off)
  # Equal weights never look at the scores.
  expect_no_warning(nowcast_weights(bt, type = "equal"))
})

test_that("the print summary uses the common targets", {
  bt <- drop_fit(common_backtest(), "wide", common_dates_used[[2]])
  first_date <- bt$scores[bt$scores$.now == common_dates_used[[1]], ]
  narrow_first <- mean(first_date$wis[first_date$.method == "narrow"], na.rm = TRUE)
  narrow_all <- mean(bt$scores$wis[bt$scores$.method == "narrow"], na.rm = TRUE)
  fmt <- function(v) format(signif(v, 3))

  expect_warning(printed <- utils::capture.output(print(bt)), "wide")
  expect_true(any(grepl(fmt(narrow_first), printed, fixed = TRUE)))

  expect_no_warning(
    printed_own <- utils::capture.output(print(bt, common_dates = FALSE))
  )
  expect_true(any(grepl(fmt(narrow_all), printed_own, fixed = TRUE)))
})

test_that("a stratum missing for one method is a gap too", {
  bt <- common_backtest(strata = TRUE)
  gone <- bt$scores$.method == "narrow" & bt$scores$gender == "M" &
    bt$scores$.now == common_dates_used[[1]]
  bt$scores <- bt$scores[!gone, ]
  expect_warning(nowcast_weights(bt), "narrow.*2024-02-18")
})

test_that("no common target aborts the weights and is explained in print", {
  bt <- common_backtest()
  bt <- drop_fit(bt, "wide", common_dates_used[[2]])
  bt <- drop_fit(bt, "narrow", common_dates_used[[1]])
  expect_error(
    suppressWarnings(nowcast_weights(bt)),
    "no target was scored by every method"
  )
  printed <- suppressWarnings(utils::capture.output(print(bt)))
  expect_true(any(grepl("common_dates = FALSE", printed, fixed = TRUE)))
})

test_that("a method that failed everywhere is not counted as a gap", {
  registerS3method("nowcast_fit", "brokentoy",
    function(method, x, ..., quantile_levels, verbose = TRUE) stop("nope"),
    envir = asNamespace("tbl.now")
  )
  bt <- suppressWarnings(nowcast_backtest(common_data(),
    example_engine(spread = 0.2, label = "narrow"),
    example_engine(spread = 0.5, label = "wide"),
    engine("brokentoy"),
    now_dates = common_dates_used, verbose = FALSE
  ))
  expect_setequal(bt$methods, c("narrow", "wide"))
  expect_no_warning(nowcast_weights(bt))
})

test_that("nowcast_ensemble() passes common_dates through", {
  bt <- drop_fit(common_backtest(), "wide", common_dates_used[[2]])
  x <- common_data()
  narrow <- run_nowcast(x, example_engine(spread = 0.2, label = "narrow"),
                        verbose = FALSE)
  wide <- run_nowcast(x, example_engine(spread = 0.5, label = "wide"),
                      verbose = FALSE)

  expect_warning(
    nowcast_ensemble(narrow = narrow, wide = wide, weights = "inverse_score",
                     backtest = bt, verbose = FALSE),
    "wide"
  )
  expect_no_warning(
    nowcast_ensemble(narrow = narrow, wide = wide, weights = "inverse_score",
                     backtest = bt, common_dates = FALSE, verbose = FALSE)
  )
})

test_that("common_dates must be TRUE or FALSE", {
  bt <- common_backtest()
  expect_error(nowcast_weights(bt, common_dates = NA), "common_dates")
  expect_error(nowcast_weights(bt, common_dates = "yes"), "common_dates")
})
