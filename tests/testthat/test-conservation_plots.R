# =============================================================================
# Conservation registration helpers. `plot_creation_transport()` and the heavier
# monitors (backlog / reporting lag / dashboard) now live in
# devel/conservation_extras.R, so here we exercise the shared machinery they and
# the transport diagnostics rely on.
# =============================================================================

make_cons_tbl <- function(seed = 5L) {
  set.seed(seed)
  events  <- rep(seq(as.Date("2023-01-01"), by = "day", length.out = 90L), each = 5L)
  reports <- events + stats::rpois(length(events), 2L)
  df <- data.frame(event_date = events, report_date = reports)
  df <- dplyr::count(df, .data$event_date, .data$report_date, name = "n")
  tbl_now(df, event_date, report_date, case_count = n,
          data_type = "count-incidence", verbose = FALSE)
}

test_that(".conservation_prep returns standardised creation/transport scores", {
  tn   <- make_cons_tbl()
  prep <- tbl.now:::.conservation_prep(tn, lookback = 6L)
  expect_true(all(c("transport_z", "creation_z") %in% names(prep$reg)))
  expect_type(prep$z_star, "double")
  expect_true(is.finite(prep$z_star))
})

test_that("a planted batch is confirmed by the screening machinery", {
  tn       <- make_cons_tbl()
  closed   <- seq(as.Date("2023-02-06"), by = "day", length.out = 6L)
  batched  <- suppressWarnings(simulate_batch(tn, closed_dates = closed))
  screened <- suppressWarnings(batch_test(batched, lookback = 6))
  expect_true(any(screened$batch %in% TRUE))
})
