# `nowcast_backtest(checkpoint_file = )` saves finished fits so an interrupted
# backtest resumes. Only the main R session writes the file; the futures workers
# never see it.

checkpoint_data <- function(strata = FALSE, drop_row = FALSE) {
  df <- data.frame(
    onset = as.Date("2024-01-07") + rep(7 * (0:9), each = 6),
    report = as.Date("2024-01-07") + rep(7 * (0:9), each = 6) + rep(c(0, 7, 14), 2),
    gender = rep(c("F", "M"), each = 3)
  )
  if (drop_row) df <- df[-1, ]
  tbl_now(df,
    event_date = onset, report_date = report,
    strata = if (strata) "gender" else NULL,
    data_type = "linelist", verbose = FALSE
  )
}

checkpoint_dates <- as.Date(c("2024-02-18", "2024-03-03", "2024-03-10"))

# An engine that records every fit it is asked to make and can be told to fail
# at one date, standing in for an interruption. State lives in an environment
# because a closure over the test frame would not survive `registerS3method()`.
flaky_state <- new.env()
flaky_state$calls <- character(0)
flaky_state$fail_at <- NULL

local({
  fit <- function(engine, x, ..., quantile_levels = nowcast_quantile_levels(),
                  verbose = TRUE) {
    flaky_state$calls <- c(flaky_state$calls, as.character(get_now(x)))
    if (!is.null(flaky_state$fail_at) && get_now(x) == flaky_state$fail_at) {
      stop("interrupted")
    }
    nowcast_fit.example(engine, x, spread = 0.3, quantile_levels = quantile_levels)
  }
  registerS3method("nowcast_fit", "flakytoy", fit, envir = asNamespace("tbl.now"))
  registerS3method("nowcast_tidy", "flakytoy", nowcast_tidy.example,
                   envir = asNamespace("tbl.now"))
})

reset_flaky <- function(fail_at = NULL) {
  flaky_state$calls <- character(0)
  flaky_state$fail_at <- fail_at
}

checkpoint_run <- function(x, ..., dates = checkpoint_dates, file = NULL,
                           seed = NULL, keep_draws = FALSE, on_error = "warn",
                           truth_axis = "report", truth_type = "total") {
  nowcast_backtest(x, ..., now_dates = dates, verbose = FALSE,
                   checkpoint_file = file, seed = seed, keep_draws = keep_draws,
                   on_error = on_error, truth_axis = truth_axis,
                   truth_type = truth_type)
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
}

checkpoint_fits <- function(file) {
  vapply(readRDS(file)$fits, function(e) {
    paste(e$label, as.character(e$now))
  }, character(1))
}

test_that("a checkpointed backtest equals the plain one and leaves one file", {
  file <- withr::local_tempfile(fileext = ".rds")
  x <- checkpoint_data()
  plain <- checkpoint_run(x, example_engine(spread = 0.2, label = "a"),
                          example_engine(spread = 0.5, label = "b"))
  saved <- checkpoint_run(x, example_engine(spread = 0.2, label = "a"),
                          example_engine(spread = 0.5, label = "b"), file = file)
  expect_same_but_elapsed(plain, saved)

  expect_true(file.exists(file))
  expect_length(checkpoint_fits(file), 6L)
  # No temporary file is left next to it.
  expect_equal(list.files(dirname(file), all.files = TRUE, no.. = TRUE,
                          pattern = "\\.tmp$"), character(0))
})

test_that("a complete checkpoint refits nothing and returns the same object", {
  file <- withr::local_tempfile()
  x <- checkpoint_data()
  reset_flaky()
  first <- checkpoint_run(x, engine("flakytoy", label = "flaky"), file = file)
  expect_length(flaky_state$calls, 3L)

  reset_flaky()
  again <- checkpoint_run(x, engine("flakytoy", label = "flaky"), file = file)
  expect_length(flaky_state$calls, 0L)
  # Not even the timings change: they are the saved ones.
  expect_equal(first, again)
})

test_that("resuming after an interruption fits only what is missing", {
  file <- withr::local_tempfile()
  x <- checkpoint_data()

  # The fit at the second date stops the run, as an interruption would.
  reset_flaky(fail_at = checkpoint_dates[[2]])
  expect_error(
    checkpoint_run(x, engine("flakytoy", label = "flaky"), file = file,
                   on_error = "abort"),
    "interrupted|failed"
  )
  expect_equal(checkpoint_fits(file), paste("flaky", checkpoint_dates[[1]]))

  reset_flaky()
  resumed <- checkpoint_run(x, engine("flakytoy", label = "flaky"), file = file)
  expect_equal(flaky_state$calls, as.character(checkpoint_dates[2:3]))

  reset_flaky()
  plain <- checkpoint_run(x, engine("flakytoy", label = "flaky"))
  expect_same_but_elapsed(plain, resumed)
})

test_that("a failed fit is kept in the file but retried on resume", {
  file <- withr::local_tempfile()
  x <- checkpoint_data()

  reset_flaky(fail_at = checkpoint_dates[[2]])
  expect_warning(
    first <- checkpoint_run(x, engine("flakytoy", label = "flaky"), file = file),
    "interrupted"
  )
  expect_equal(first$timings$success, c(TRUE, FALSE, TRUE))
  expect_length(checkpoint_fits(file), 3L)

  reset_flaky()
  second <- checkpoint_run(x, engine("flakytoy", label = "flaky"), file = file)
  expect_equal(flaky_state$calls, as.character(checkpoint_dates[[2]]))
  expect_equal(second$timings$success, c(TRUE, TRUE, TRUE))
  expect_equal(nrow(second$scores), nrow(first$scores) +
    nrow(dplyr::filter(second$scores, .data$.now == checkpoint_dates[[2]])))
})

test_that("new dates and new engines are added to an existing checkpoint", {
  file <- withr::local_tempfile()
  x <- checkpoint_data()

  reset_flaky()
  checkpoint_run(x, engine("flakytoy", label = "flaky"),
                 dates = checkpoint_dates[1], file = file)
  expect_equal(flaky_state$calls, as.character(checkpoint_dates[1]))

  reset_flaky()
  more <- checkpoint_run(x, engine("flakytoy", label = "flaky"),
                         dates = checkpoint_dates, file = file)
  expect_equal(flaky_state$calls, as.character(checkpoint_dates[2:3]))
  expect_equal(more$now_dates, checkpoint_dates)

  reset_flaky()
  wider <- checkpoint_run(
    x, engine("flakytoy", label = "flaky"), example_engine(label = "extra"),
    file = file
  )
  expect_length(flaky_state$calls, 0L)
  expect_equal(wider$methods, c("flaky", "extra"))
  expect_length(checkpoint_fits(file), 6L)

  # A run over fewer dates than the file has returns just those dates.
  fewer <- checkpoint_run(x, engine("flakytoy", label = "flaky"),
                          dates = checkpoint_dates[2], file = file)
  expect_equal(fewer$now_dates, checkpoint_dates[2])
  expect_equal(unique(fewer$scores$.now), checkpoint_dates[2])
  expect_length(checkpoint_fits(file), 6L)
})

test_that("a checkpoint from a different backtest is refused", {
  file <- withr::local_tempfile()
  x <- checkpoint_data()
  checkpoint_run(x, example_engine(spread = 0.2, label = "a"), file = file,
                 seed = 1)
  before <- readRDS(file)

  expect_error(
    checkpoint_run(x, example_engine(spread = 0.4, label = "a"), file = file,
                   seed = 1),
    "different backtest.*engine"
  )
  expect_error(
    checkpoint_run(x, example_engine(spread = 0.2, label = "a"), file = file,
                   seed = 2),
    "seed"
  )
  expect_error(
    checkpoint_run(x, example_engine(spread = 0.2, label = "a"), file = file,
                   seed = 1, keep_draws = TRUE),
    "keep_draws"
  )
  expect_error(
    checkpoint_run(x, example_engine(spread = 0.2, label = "a"), file = file,
                   seed = 1, truth_type = "total", truth_axis = "revision"),
    "truth_axis|revision"
  )
  expect_error(
    checkpoint_run(checkpoint_data(drop_row = TRUE),
                   example_engine(spread = 0.2, label = "a"), file = file,
                   seed = 1),
    "the data"
  )
  # Refusing does not touch the file.
  expect_identical(readRDS(file), before)

  # The same call is accepted.
  expect_no_error(checkpoint_run(
    x, example_engine(spread = 0.2, label = "a"), file = file, seed = 1
  ))
})

test_that("the engine fingerprint ignores a function's environment", {
  make_engine <- function(offset) {
    f <- function(z) z + 1
    engine("flakytoy", transform = f, label = "f")
  }
  x <- checkpoint_data()
  fingerprint <- function(e) {
    .backtest_fingerprint(x, list(f = e), seed = NULL, keep_draws = FALSE,
                          truth_axis = "report", truth_type = "total")$engines
  }
  expect_equal(fingerprint(make_engine(1)), fingerprint(make_engine(2)))
  other <- engine("flakytoy", transform = function(z) z + 2, label = "f")
  expect_false(identical(fingerprint(make_engine(1)), fingerprint(other)))
  # The label is the key, not part of the model.
  relabelled <- make_engine(1)
  relabelled$label <- "g"
  expect_equal(unname(fingerprint(make_engine(1))), unname(fingerprint(relabelled)))
})

test_that("a file that is not a checkpoint is never overwritten", {
  file <- withr::local_tempfile()
  writeLines("not a checkpoint", file)
  expect_error(
    checkpoint_run(checkpoint_data(), example_engine(), file = file),
    "not a .*checkpoint"
  )
  expect_equal(readLines(file), "not a checkpoint")

  saveRDS(list(version = 99L), file)
  expect_error(
    checkpoint_run(checkpoint_data(), example_engine(), file = file),
    "not a .*checkpoint"
  )
})

test_that("checkpoint_file is validated and its folder is created", {
  x <- checkpoint_data()
  for (bad in list(1, NA_character_, c("a", "b"), "", TRUE)) {
    expect_error(
      checkpoint_run(x, example_engine(), file = bad), "checkpoint_file"
    )
  }
  expect_error(
    checkpoint_run(x, example_engine(), file = tempdir()), "directory"
  )

  folder <- withr::local_tempdir()
  nested <- file.path(folder, "tmp", "deeper", "myfile")
  checkpoint_run(x, example_engine(), file = nested)
  expect_true(file.exists(nested))
})

test_that("an unwritable checkpoint path fails before any fit", {
  # A folder cannot be made below a file, whoever is running the test.
  blocker <- withr::local_tempfile()
  writeLines("a file", blocker)
  reset_flaky()
  expect_error(
    checkpoint_run(checkpoint_data(), engine("flakytoy", label = "flaky"),
                   file = file.path(blocker, "cp")),
    "Could not write"
  )
  expect_length(flaky_state$calls, 0L)
})

test_that("strata and a grouped tbl_now checkpoint like the plain backtest", {
  file <- withr::local_tempfile()
  x <- checkpoint_data(strata = TRUE)
  plain <- checkpoint_run(x, example_engine(label = "toy"))
  saved <- checkpoint_run(dplyr::group_by(x, gender), example_engine(label = "toy"),
                          file = file)
  expect_same_but_elapsed(plain, saved)
  resumed <- checkpoint_run(x, example_engine(label = "toy"), file = file)
  expect_equal(saved, resumed)
})

test_that("a checkpointed backtest combines like any other", {
  file <- withr::local_tempfile()
  x <- checkpoint_data()
  first <- checkpoint_run(x, example_engine(label = "toy"),
                          dates = checkpoint_dates[1:2], file = file)
  later <- checkpoint_run(x, example_engine(label = "toy"),
                          dates = checkpoint_dates[3])
  expect_same_but_elapsed(
    checkpoint_run(x, example_engine(label = "toy")),
    backtest_combine(first, later)
  )
})

test_that("a parallel checkpointed backtest equals the sequential one", {
  skip_if_not_installed("doFuture")
  skip_if_not_installed("foreach")
  skip_if_not_installed("future")
  future::plan(future::sequential)
  file <- withr::local_tempfile()
  x <- checkpoint_data()
  engines <- list(example_engine(spread = 0.2, label = "a"),
                  example_engine(spread = 0.5, label = "b"))

  plain <- checkpoint_run(x, engines)
  saved <- nowcast_backtest(x, engines, now_dates = checkpoint_dates,
                            verbose = FALSE, parallel = TRUE,
                            checkpoint_file = file)
  expect_same_but_elapsed(plain, saved)
  expect_length(checkpoint_fits(file), 6L)

  # Resuming in the other mode finds everything already there.
  sequential <- checkpoint_run(x, engines, file = file)
  expect_equal(saved, sequential)
})

test_that("parallel failures leave earlier waves saved", {
  skip_if_not_installed("doFuture")
  skip_if_not_installed("foreach")
  skip_if_not_installed("future")
  future::plan(future::sequential)
  file <- withr::local_tempfile()
  x <- checkpoint_data()

  reset_flaky(fail_at = checkpoint_dates[[3]])
  expect_error(
    suppressWarnings(nowcast_backtest(
      x, engine("flakytoy", label = "flaky"), now_dates = checkpoint_dates,
      verbose = FALSE, parallel = TRUE, on_error = "abort",
      checkpoint_file = file
    )),
    "failed"
  )
  # A sequential plan runs one fit per wave, so two waves finished.
  expect_equal(checkpoint_fits(file), paste("flaky", checkpoint_dates[1:2]))
})

test_that("real multisession workers never write the checkpoint", {
  skip_on_cran()
  skip_on_covr()
  skip_if_not_installed("doFuture")
  skip_if_not_installed("foreach")
  skip_if_not_installed("future")
  future::plan(future::multisession, workers = 2)
  withr::defer(future::plan(future::sequential))
  # Workers load the INSTALLED tbl.now; skip unless it is the code under test.
  installed <- tryCatch(
    future::value(future::future(
      deparse(body(asNamespace("tbl.now")$nowcast_backtest))
    )),
    error = function(e) NULL
  )
  skip_if_not(identical(installed, deparse(body(nowcast_backtest))),
              "the installed tbl.now is not the version under test")

  folder <- withr::local_tempdir()
  file <- file.path(folder, "cp.rds")
  x <- checkpoint_data()
  engines <- list(example_engine(spread = 0.2, label = "a"),
                  example_engine(spread = 0.5, label = "b"))

  # The main session rewrites the checkpoint after each wave; the workers only
  # return fits. Nothing but the checkpoint itself may be left in the folder.
  plain <- checkpoint_run(x, engines)
  saved <- nowcast_backtest(x, engines, now_dates = checkpoint_dates,
                            verbose = FALSE, parallel = TRUE,
                            checkpoint_file = file)
  expect_same_but_elapsed(plain, saved)
  expect_length(checkpoint_fits(file), 6L)
  expect_equal(list.files(folder, all.files = TRUE, no.. = TRUE), "cp.rds")

  # Resuming under the same plan refits nothing.
  again <- nowcast_backtest(x, engines, now_dates = checkpoint_dates,
                            verbose = FALSE, parallel = TRUE,
                            checkpoint_file = file)
  expect_equal(saved, again)
})
