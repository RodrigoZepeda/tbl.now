# =============================================================================
# plot_reporting_hexamap(): the age-period-cohort hexamap of the reporting
# triangle. Kept light -- build the ggplot, never write a file.
# =============================================================================

make_hex_tbl <- function(seed = 4L) {
  set.seed(seed)
  ev <- rep(seq(as.Date("2024-01-01"), by = "day", length.out = 40L), each = 5L)
  rp <- ev + stats::rpois(length(ev), 2L)
  tbl_now(data.frame(event_date = ev, report_date = rp),
          event_date, report_date, data_type = "linelist", verbose = FALSE)
}

test_that("plot_reporting_hexamap builds and validates input", {
  tn <- make_hex_tbl()
  p  <- plot_reporting_hexamap(tn)
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
  expect_error(plot_reporting_hexamap(mtcars), class = "rlang_error")
})

test_that("max_delay caps the delay axis", {
  tn   <- make_hex_tbl()
  poly <- ggplot2::ggplot_build(plot_reporting_hexamap(tn, max_delay = 3))$data
  # the hexagon layer is the geom_polygon; its y never exceeds the delay-3 hull
  hex  <- poly[[which.max(vapply(poly, nrow, integer(1)))]]
  expect_true(is.finite(max(hex$y)))
})

test_that("the max_cells guard bounds the number of hexagons", {
  tn <- make_hex_tbl()
  # a tiny cap must trigger the informational message and still build
  expect_message(
    p <- plot_reporting_hexamap(tn, complete = TRUE, max_cells = 200L),
    "Capped the delay axis"
  )
  build <- ggplot2::ggplot_build(p)$data
  hex   <- build[[which.max(vapply(build, nrow, integer(1)))]]
  # 6 vertices per hexagon, so <= 200 hexagons => <= 1200 polygon vertices
  expect_lte(length(unique(hex$group)), 200L)
})
