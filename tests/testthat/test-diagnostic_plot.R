# =============================================================================
# diagnostic_plot(): the reporting-process gallery
# =============================================================================

make_diag_tbl <- function(strata = TRUE, seed = 3L) {
  set.seed(seed)
  events  <- rep(seq(as.Date("2023-01-01"), by = "day", length.out = 60L), each = 4L)
  reports <- events + stats::rpois(length(events), 2L)
  df <- data.frame(event_date = events, report_date = reports,
                   sex = sample(c("f", "m"), length(events), replace = TRUE))
  if (strata) {
    df <- dplyr::count(df, .data$event_date, .data$report_date, .data$sex, name = "n")
    tbl_now(df, event_date, report_date, case_count = n, strata = sex,
            data_type = "count-incidence", verbose = FALSE)
  } else {
    df <- dplyr::count(df, .data$event_date, .data$report_date, name = "n")
    tbl_now(df, event_date, report_date, case_count = n,
            data_type = "count-incidence", verbose = FALSE)
  }
}

n_panels <- function(p) nrow(ggplot2::ggplot_build(p)$layout$layout)
builds_ok <- function(p) {
  isTRUE(tryCatch({ggplot2::ggplot_build(p); TRUE}, error = function(e) FALSE))
}

test_that("all panels returns a patchwork, a single panel returns a plain plot", {
  skip_if_not_installed("patchwork")
  tn <- make_diag_tbl()
  expect_s3_class(suppressWarnings(diagnostic_plot(tn)), "patchwork")
  one <- diagnostic_plot(tn, panels = "triangle")
  expect_s3_class(one, "ggplot")
  expect_false(inherits(one, "patchwork"))
})

test_that("every panel builds, on its own and facetted by stratum", {
  tn <- make_diag_tbl(strata = TRUE)
  panels <- c("reporting", "triangle", "profiles", "delay_drift", "transport")
  for (panel in panels) {
    expect_true(builds_ok(suppressWarnings(diagnostic_plot(tn, panels = panel))),
                info = panel)
  }
})

test_that("each panel has a stand-alone plotting function", {
  tn <- make_diag_tbl(strata = TRUE)
  expect_s3_class(suppressWarnings(plot_reporting_process(tn)), "ggplot")
  expect_s3_class(suppressWarnings(plot_epidemic_process(tn)), "ggplot")
  expect_s3_class(suppressWarnings(plot_reporting_triangle(tn)), "ggplot")
  expect_s3_class(suppressWarnings(plot_delay_profiles(tn, by = "event")), "ggplot")
  expect_s3_class(suppressWarnings(plot_transport_discriminant(tn, period = 7)), "ggplot")
  expect_error(plot_reporting_process(mtcars), class = "rlang_error")
  expect_error(plot_epidemic_process(mtcars), class = "rlang_error")
})

test_that("plot_reporting_v (the rotated reporting triangle) builds and validates", {
  tn <- make_diag_tbl(strata = FALSE)
  expect_true(builds_ok(suppressWarnings(plot_reporting_v(tn))))
  expect_error(plot_reporting_v(mtcars), class = "rlang_error")
})

test_that(".diag_batch_stripes finds an obvious volume spike and honours k = 0", {
  inc <- data.frame(
    .report_date = as.Date("2023-01-01") + 0:29,
    .count       = c(rep(5, 15), 500, rep(5, 14))       # a lone spike on day 16
  )
  expect_length(tbl.now:::.diag_batch_stripes(inc, 0L), 0L)
  stripes <- tbl.now:::.diag_batch_stripes(inc, 1L)
  expect_equal(stripes, as.Date("2023-01-16"))
})

test_that("the reporting triangle draws the report-date axis, toggled by report_ticks", {
  tn <- make_diag_tbl(strata = FALSE)
  has_abline <- function(p) {
    any(vapply(p$layers, function(l) inherits(l$geom, "GeomAbline"), logical(1)))
  }
  expect_true(has_abline(suppressWarnings(plot_reporting_triangle(tn))))   # default axis
  expect_false(has_abline(suppressWarnings(
    plot_reporting_triangle(tn, report_ticks = 0, mark_batches = 0))))
})

test_that("the reporting triangle separates reported zeros from not-yet-reportable", {
  tn    <- make_diag_tbl(strata = FALSE)
  build <- ggplot2::ggplot_build(suppressWarnings(plot_reporting_triangle(tn)))
  fills <- unlist(lapply(build$data, function(d) d$fill))
  expect_true(tbl.now:::.DIAG_ZERO_COLOUR %in% fills)   # a zero-coloured layer is drawn
})

test_that("an event date with zero rows in the raw data is drawn as a zero, not left blank", {
  # A whole origin week with truly no cases never appears in the raw data at all,
  # so the tile grid must be built from the FULL calendar (not just observed event
  # dates), or that date silently vanishes from the triangle instead of reading 0.
  events  <- seq(as.Date("2023-01-01"), by = "day", length.out = 40L)
  gap     <- as.Date("2023-01-20")
  events  <- setdiff(events, gap) |> as.Date(origin = "1970-01-01")
  df <- data.frame(event_date = rep(events, each = 3),
                   report_date = rep(events, each = 3) + 2)
  df <- dplyr::count(df, .data$event_date, .data$report_date, name = "n")
  tn <- tbl_now(df, event_date, report_date, case_count = n,
               data_type = "count-incidence", verbose = FALSE)

  build <- ggplot2::ggplot_build(suppressWarnings(plot_reporting_triangle(tn)))
  zero_layer <- build$data[[1]]     # the zero-tile layer is drawn first
  tile_dates <- as.Date(zero_layer$x, origin = "1970-01-01")
  expect_true(any(tile_dates == gap))
})

test_that("panels facet by stratum only when strata are present", {
  expect_equal(n_panels(diagnostic_plot(make_diag_tbl(TRUE),  panels = "triangle")), 2L)
  expect_equal(n_panels(diagnostic_plot(make_diag_tbl(FALSE), panels = "triangle")), 1L)
  expect_equal(n_panels(suppressWarnings(diagnostic_plot(make_diag_tbl(TRUE), panels = "transport"))), 2L)
})

test_that("the profiles panel switches summarisation axis with `by`", {
  tn <- make_diag_tbl(strata = FALSE)
  expect_true(builds_ok(diagnostic_plot(tn, panels = "profiles", by = "report")))
  expect_true(builds_ok(diagnostic_plot(tn, panels = "profiles", by = "event")))
})

test_that("`...` routes only the accepted args to the transport panel", {
  tn <- make_diag_tbl(strata = FALSE)
  expect_true(builds_ok(suppressWarnings(
    diagnostic_plot(tn, panels = "transport", period = 7, lookback = 3)
  )))
})

test_that("inputs are validated", {
  tn <- make_diag_tbl(strata = FALSE)
  expect_error(diagnostic_plot(tn, panels = "ledger"),  "Unknown panel")   # old name, renamed
  expect_error(diagnostic_plot(tn, panels = "staleness"),  "Unknown panel") # old name, renamed
  expect_error(diagnostic_plot(mtcars), class = "rlang_error")
})
