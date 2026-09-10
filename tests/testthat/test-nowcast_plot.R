test_that("autoplot.tbl_nowcast draws every symmetric band, not just the 50%", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")

  levels <- nowcast_quantile_levels()
  predictions <- tidyr::expand_grid(
    onset_week = as.Date("2020-01-06") + seq(0, 21, by = 7),
    .quantile_level = levels
  )
  predictions$.value <- 100 * predictions$.quantile_level

  nowcast <- tbl_nowcast(
    predictions = predictions, method = "toy", event_date = "onset_week"
  )

  bands <- ggplot2::ggplot_build(ggplot2::autoplot(nowcast))$data[[1]]

  # 0.025/0.975, 0.05/0.95, 0.1/0.9 and 0.25/0.75 -> four bands, four dates.
  expect_equal(nrow(bands), 16)
  # The regression: `(1 - (1 - 2 * 0.05)) / 2` is not `0.05`, so matching the
  # tails by equality left every band but the 50% one with an NA lower bound.
  expect_false(any(is.na(bands$ymin)))
  expect_false(any(is.na(bands$ymax)))
})

test_that("autoplot.tbl_nowcast honours `levels` and rejects unavailable ones", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")

  predictions <- tidyr::expand_grid(
    onset_week = as.Date("2020-01-06") + seq(0, 21, by = 7),
    .quantile_level = c(0.05, 0.25, 0.5, 0.75, 0.95)
  )
  predictions$.value <- 100 * predictions$.quantile_level

  nowcast <- tbl_nowcast(
    predictions = predictions, method = "toy", event_date = "onset_week"
  )

  one_band <- ggplot2::ggplot_build(
    ggplot2::autoplot(nowcast, levels = 0.9)
  )$data[[1]]
  expect_equal(nrow(one_band), 4)
  expect_false(any(is.na(one_band$ymin)))

  expect_error(ggplot2::autoplot(nowcast, levels = 0.99), "not available")
})

test_that("autoplot.tbl_nowcast errors when no symmetric pair exists", {
  skip_if_not_installed("ggplot2")

  predictions <- data.frame(
    onset_week = as.Date("2020-01-06"),
    .quantile_level = c(0.1, 0.5),
    .value = c(5, 10)
  )
  nowcast <- tbl_nowcast(
    predictions = predictions, method = "toy", event_date = "onset_week"
  )

  expect_error(ggplot2::autoplot(nowcast), "no symmetric quantile pairs")
})

test_that("autoplot.tbl_nowcast zooms with `date_lim` and `ylim` without dropping rows", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")

  predictions <- tidyr::expand_grid(
    onset_week = as.Date("2020-01-06") + seq(0, 28, by = 7),
    .quantile_level = c(0.05, 0.25, 0.5, 0.75, 0.95)
  )
  predictions$.value <- 100 * predictions$.quantile_level

  nowcast <- tbl_nowcast(
    predictions = predictions, method = "toy", event_date = "onset_week"
  )

  window <- c(as.Date("2020-01-20"), as.Date("2020-02-03"))
  zoomed <- ggplot2::autoplot(nowcast, date_lim = window, ylim = c(0, 80))

  expect_s3_class(zoomed, "ggplot")
  built <- ggplot2::ggplot_build(zoomed)
  # `coord_cartesian()` crops, it does not filter: every date is still in the
  # ribbon's data, which is what keeps the fan running to the panel edge rather
  # than stopping dead at the limit.
  expect_equal(nrow(built$data[[1]]), 5 * 2)
  expect_equal(built$layout$coord$limits$x, window)
  expect_equal(built$layout$coord$limits$y, c(0, 80))

  # An open end is allowed on either side.
  expect_s3_class(
    ggplot2::autoplot(nowcast, date_lim = c(as.Date("2020-01-20"), NA)),
    "ggplot"
  )
  # Two NAs mean "no zoom at all", so no coord is added.
  expect_null(
    ggplot2::ggplot_build(
      ggplot2::autoplot(nowcast, date_lim = c(NA, NA))
    )$layout$coord$limits$x
  )
})

test_that("autoplot.tbl_nowcast rejects malformed zoom limits", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")

  predictions <- tidyr::expand_grid(
    onset_week = as.Date("2020-01-06") + seq(0, 21, by = 7),
    .quantile_level = c(0.05, 0.5, 0.95)
  )
  predictions$.value <- 100 * predictions$.quantile_level
  nowcast <- tbl_nowcast(
    predictions = predictions, method = "toy", event_date = "onset_week"
  )

  expect_error(
    ggplot2::autoplot(nowcast, date_lim = as.Date("2020-01-20")),
    "length-2"
  )
  expect_error(ggplot2::autoplot(nowcast, ylim = c("a", "b")), "length-2")
})
