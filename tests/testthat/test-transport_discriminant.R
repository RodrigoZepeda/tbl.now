# =============================================================================
# transport_discriminant(): the (deficit W, discriminant Delta) plane
# =============================================================================

# A genuine batch *moves* reports (leaving a deficit), so plant it with
# simulate_batch() rather than by adding reports (which would be a surge).
make_td_tbl <- function(with_batch = TRUE, seed = 1L) {
  set.seed(seed)
  origins <- seq(as.Date("2023-01-01"), by = "day", length.out = 70L)
  rows <- lapply(origins, function(origin) {
    k <- stats::rpois(1L, 12L)
    if (k == 0L) return(NULL)
    data.frame(event_date = origin, report_date = origin + stats::rgeom(k, 0.5))
  })
  linelist <- dplyr::bind_rows(rows)
  linelist <- linelist[linelist$report_date <= max(origins), , drop = FALSE]
  clean    <- tbl_now(linelist, event_date, report_date, data_type = "linelist", verbose = FALSE)
  if (!with_batch) {
    return(list(tbl = clean, batch_date = NULL))
  }
  # A week-long closure, to match the default lookback of 7.
  closed  <- seq(as.Date("2023-02-06"), by = "day", length.out = 7L)
  planted <- suppressWarnings(simulate_batch(clean, closed_dates = closed, verbose = FALSE))
  list(tbl = planted, batch_date = as.Date("2023-02-13"))  # first open date after the closure
}

test_that("transport_discriminant() returns the documented columns and class", {
  td <- suppressWarnings(transport_discriminant(make_td_tbl()$tbl))
  expect_s3_class(td, "transport_discriminant")
  expect_true(all(c(
    "report_date", "stratum", "reported", "baseline", "window_total", "spike",
    "deficit", "delta", "transport_z", "creation_z", "classification", "batch"
  ) %in% names(td)))
  expect_type(td$batch, "logical")
})

test_that("a planted transport batch scores high on transport, not creation", {
  fixture <- make_td_tbl()
  td      <- suppressWarnings(transport_discriminant(fixture$tbl))

  expect_gte(sum(td$batch, na.rm = TRUE), 1L)     # the release is recovered
  flagged <- td[td$batch %in% TRUE, ]
  expect_true(all(flagged$transport_z > flagged$creation_z))
  expect_true(all(flagged$classification == "batch"))
  # the release date itself carries a real deficit
  at <- td[td$report_date == fixture$batch_date, ]
  expect_gt(at$transport_z, 2)
})

test_that("planting a batch flags a release date that clean data does not", {
  fixture <- make_td_tbl(with_batch = TRUE)
  batched <- suppressWarnings(transport_discriminant(fixture$tbl))
  clean   <- suppressWarnings(transport_discriminant(make_td_tbl(with_batch = FALSE)$tbl))

  release <- fixture$batch_date
  expect_true(batched$batch[batched$report_date == release])
  expect_false(isTRUE(clean$batch[clean$report_date == release]))
})

test_that("the flags agree with batch_test()'s robust null", {
  fixture <- make_td_tbl()
  td <- suppressWarnings(transport_discriminant(fixture$tbl))
  bs <- suppressWarnings(batch_test(fixture$tbl, null_model = "robust"))
  expect_equal(sum(td$batch, na.rm = TRUE), sum(bs$batch, na.rm = TRUE))
})

test_that("transport_discriminant() validates its inputs", {
  tn <- make_td_tbl(with_batch = FALSE)$tbl
  expect_error(suppressWarnings(transport_discriminant(tn, lookback = 0)), "positive")
  expect_error(suppressWarnings(transport_discriminant(tn, alpha = 1)), "between 0 and 1")
  expect_error(suppressWarnings(transport_discriminant(mtcars)), class = "rlang_error")
})

test_that("print returns the object invisibly", {
  td <- suppressWarnings(transport_discriminant(make_td_tbl()$tbl))
  expect_output(print(td), "A tibble")
  expect_invisible(print(td))
})
