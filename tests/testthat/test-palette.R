# =============================================================================
# tbl_now_palette(): the role-named colour contract, and the size / linewidth
# arguments every plot takes. Kept light -- build the ggplot, never write a file.
# =============================================================================

make_palette_tbl <- function(seed = 11L) {
  set.seed(seed)
  ev <- rep(seq(as.Date("2024-01-01"), by = "day", length.out = 60L), each = 4L)
  rp <- ev + stats::rpois(length(ev), 2L)
  tbl_now(data.frame(event_date = ev, report_date = rp),
          event_date, report_date, data_type = "linelist", verbose = FALSE)
}

test_that("the palette is complete, named by role, and hue-free", {
  pal <- tbl_now_palette()

  expect_s3_class(pal, "tbl_now_palette")
  expect_type(pal, "character")
  expect_false(anyNA(pal))
  expect_true(all(nzchar(names(pal))))

  # The whole point of the rename: no role is named after the colour it happens
  # to hold, so a palette in other hues can still fill every role.
  expect_false(any(grepl("green|red|black|blue|grey|gray", names(pal))))
})

test_that("overriding one role leaves the rest at their defaults", {
  pal <- tbl_now_palette(reporting = "#5B4B8A")
  expect_equal(unname(pal[["reporting"]]), "#5B4B8A")
  expect_equal(
    pal[setdiff(names(pal), "reporting")],
    tbl_now_palette()[setdiff(names(pal), "reporting")]
  )
})

test_that("the roles are taken from the constructor, so they cannot drift", {
  expect_equal(tbl.now:::.tbl_now_palette_roles(), names(tbl_now_palette()))
  expect_equal(tbl.now:::.tbl_now_palette(), tbl_now_palette())
})

test_that("an incomplete palette is refused, naming what is missing", {
  expect_error(
    tbl.now:::.tbl_now_check_palette(c(reporting = "red")),
    "epidemic"
  )
  expect_error(tbl.now:::.tbl_now_check_palette(c("red", "green")), "named")
  expect_silent(tbl.now:::.tbl_now_check_palette(tbl_now_palette()))
})

test_that("a custom palette reaches the plot", {
  tn <- make_palette_tbl()
  p  <- plot_epidemic_process(tn, palette = tbl_now_palette(epidemic = "#2F6DB4"))
  fills <- ggplot2::ggplot_build(p)$data[[1]]$fill
  expect_true("#2F6DB4" %in% fills)
})

test_that("every plot with a palette argument validates it", {
  tn      <- make_palette_tbl()
  broken  <- c(reporting = "red")
  callers <- list(
    plot_epidemic_process   = function(pal) plot_epidemic_process(tn, palette = pal),
    plot_reporting_process  = function(pal) plot_reporting_process(tn, palette = pal),
    plot_reporting_triangle = function(pal) plot_reporting_triangle(tn, palette = pal),
    plot_delay_profiles     = function(pal) plot_delay_profiles(tn, palette = pal),
    plot_delay_drift        = function(pal) plot_delay_drift(tn, palette = pal),
    plot_reporting_hexamap  = function(pal) plot_reporting_hexamap(tn, palette = pal),
    autoplot                = function(pal) autoplot(tn, panels = "epidemic", palette = pal)
  )
  for (name in names(callers)) {
    expect_error(callers[[name]](broken), "missing", info = name)
  }
})

test_that("size and linewidth multiply rather than replace", {
  tn <- make_palette_tbl()

  # The transport plane draws unflagged points at 1.1 and confirmed batches at
  # 2.6. A multiplier has to keep that ordering; an absolute size would erase it
  # exactly when the user was trying to make the flagged points stand out.
  sizes <- function(mult) {
    build <- suppressWarnings(
      ggplot2::ggplot_build(plot_transport_discriminant(tn, size = mult))$data
    )
    points <- Filter(function(d) "shape" %in% names(d) && nrow(d) > 0, build)
    sort(unique(unlist(lapply(points, function(d) unique(d$size)))))
  }
  expect_equal(sizes(1) * 2, sizes(2))

  # Profiles: one geom_line, drawn at 0.4.
  lw <- function(mult) {
    unique(ggplot2::ggplot_build(
      plot_delay_profiles(tn, linewidth = mult)
    )$data[[1]]$linewidth)
  }
  expect_equal(lw(1), 0.4)
  expect_equal(lw(3), 1.2)
})

test_that("size and linewidth are validated everywhere they are offered", {
  tn <- make_palette_tbl()
  expect_error(plot_delay_profiles(tn, linewidth = -1), "non-negative")
  expect_error(plot_reporting_triangle(tn, size = "big"), "single")
  expect_error(plot_delay_drift(tn, grid_linewidth = NA), "non-negative")
  expect_error(autoplot(tn, panels = "epidemic", size = c(1, 2)), "single")
  expect_error(diagnostic_plot(tn, panels = "triangle", linewidth = -0.5),
               "non-negative")
})

test_that("the gallery forwards the sizes to its panels", {
  tn <- make_palette_tbl()
  # A single panel comes back as a plain ggplot, so it can be compared with the
  # standalone function it wraps.
  gallery <- diagnostic_plot(tn, panels = "profiles", linewidth = 3)
  alone   <- plot_delay_profiles(tn, linewidth = 3)
  expect_equal(
    unique(ggplot2::ggplot_build(gallery)$data[[1]]$linewidth),
    unique(ggplot2::ggplot_build(alone)$data[[1]]$linewidth)
  )
})

test_that("the palette prints one role per line", {
  expect_output(print(tbl_now_palette()), "reporting")
  expect_output(print(tbl_now_palette()), "epidemic")
})
