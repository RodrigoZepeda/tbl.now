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

# The marks are a geom_point layer (they were a geom_polygon until the `size`
# argument arrived: a hexagon is drawn in data units and cannot be enlarged
# without overlapping, a point is drawn in millimetres and can). One point per
# observable cell, so the mark layer is the one with the most rows.
mark_layer <- function(p) {
  build <- ggplot2::ggplot_build(p)$data
  build[[which.max(vapply(build, nrow, integer(1)))]]
}

test_that("max_delay caps the delay axis", {
  tn    <- make_hex_tbl()
  marks <- mark_layer(plot_reporting_hexamap(tn, max_delay = 3))
  expect_true(is.finite(max(marks$y)))
})

test_that("the max_cells guard bounds the number of points", {
  tn <- make_hex_tbl()
  # a tiny cap must trigger the informational message and still build
  expect_message(
    p <- plot_reporting_hexamap(tn, complete = TRUE, max_cells = 200L),
    "Capped the delay axis"
  )
  # one row per point now, rather than six polygon vertices per hexagon
  expect_lte(nrow(mark_layer(p)), 200L)
})

test_that("size and shape reach the point layer", {
  tn <- make_hex_tbl()
  expect_equal(unique(mark_layer(plot_reporting_hexamap(tn))$size), 1.5)
  big <- mark_layer(plot_reporting_hexamap(tn, size = 4, shape = 15))
  expect_equal(unique(big$size), 4)
  expect_equal(unique(big$shape), 15)
})

test_that("the grid line widths are settable and independent", {
  tn <- make_hex_tbl()
  # The minor grid is drawn first, in three families, then the major grid: the
  # first six layers are the two triangular grids this function draws itself.
  widths <- function(p) {
    vapply(ggplot2::ggplot_build(p)$data[1:6],
           function(d) unique(d$linewidth), numeric(1))
  }
  expect_equal(widths(plot_reporting_hexamap(tn)),
               c(0.15, 0.15, 0.15, 0.3, 0.3, 0.3))
  expect_equal(
    widths(plot_reporting_hexamap(tn, grid_linewidth_minor = 0.5,
                                  grid_linewidth_major = 1.2)),
    c(0.5, 0.5, 0.5, 1.2, 1.2, 1.2)
  )
})

test_that("a size argument is validated", {
  tn <- make_hex_tbl()
  expect_error(plot_reporting_hexamap(tn, size = -1), "non-negative")
  expect_error(plot_reporting_hexamap(tn, size = c(1, 2)), "single")
})

test_that("an incomplete palette is refused by name", {
  tn <- make_hex_tbl()
  expect_error(
    plot_reporting_hexamap(tn, palette = c(reporting = "red")),
    "missing"
  )
})
