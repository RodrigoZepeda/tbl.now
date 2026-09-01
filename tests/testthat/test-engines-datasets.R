# Regression guard: every engine, against every dataset the package ships.
#
# `test-converter-datasets.R` already runs every CONVERTER against these shapes,
# and `test-engines-matrix.R` runs every ENGINE against 24 synthetic ones. The
# gap between them is where the `flusight` failure lived, and it is a real gap
# rather than an oversight of degree:
#
# * the converter succeeded. `tbl_now_to_baselinenowcast()` returned a perfectly
#   well-formed 112 x 113 reporting triangle, so the converter matrix was green;
# * the fixtures cannot be that shape. `engine_fixture()` builds a 3-period
#   delay tail against 40 event periods, so every cell of the engine matrix is
#   far taller than it is wide, and the one inequality `baselinenowcast` checks
#   -- more reference dates than delay columns -- can never fail there.
#
# So this file asks the question neither of them asks: does `run_nowcast()`
# actually FIT the data the package ships?
#
# `baselinenowcast` is the engine used throughout: it is the only one that fits
# in seconds without Stan or JAGS, and the failure this file exists for is about
# the SHAPE of the data rather than about any one sampler. The engines that need
# a compiler are covered by the shape grid in `test-engines-matrix.R`.

skip_on_cran()

# The delay cap each dataset needs, or `NULL` for the ones that fit as they come.
#
# For `flusight`, `covid_us` and `mpoxdat` a cap is REQUIRED, not a tuning knob.
# `flusight` is a SNAPSHOT series -- every snapshot restates the whole history --
# so its delay axis is as long as the series itself and the triangle comes out
# square; the other two reach the same place through a long straggler tail.
# `baselinenowcast` spends `max_delay` reference dates estimating the delay
# distribution and keeps two back for the uncertainty model, so a triangle that
# wide leaves it nothing and it refuses outright. Three of the six shipped
# datasets are in that state, which is the other half of why nothing caught it:
# a fixture with a 3-period delay tail is a shape almost none of the real data
# has.
#
# `covid_colombia`'s cap is the one here that is only about COST: uncapped it is
# 44 s of this file's 60, for a 142-column triangle whose cases are 99% arrived
# by delay 12. `denguedat` and `hai_bucaramanga` keep the uncapped path covered.
ENGINE_DATASETS <- list(
  denguedat        = list(max_delay = NULL),
  hai_bucaramanga  = list(max_delay = NULL),
  covid_colombia   = list(max_delay = 30),
  covid_us         = list(max_delay = 94),
  mpoxdat          = list(max_delay = 21),
  flusight_aligned = list(max_delay = 47)
)

for (dataset in names(ENGINE_DATASETS)) {
  local({
    this_dataset <- dataset
    this_max_delay <- ENGINE_DATASETS[[dataset]]$max_delay

    test_that(paste("run_nowcast() fits", this_dataset), {
      skip_if_not_installed("baselinenowcast")

      x <- dataset_tbl_now(this_dataset)
      nowcast <- suppressWarnings(suppressMessages(run_nowcast(
        x,
        engine_baselinenowcast(draws = 10, max_delay = this_max_delay),
        verbose = FALSE
      )))

      expect_true(is_tbl_nowcast(nowcast))
      predictions <- tibble::as_tibble(nowcast)
      expect_gt(nrow(predictions), 0)
      # A fit that returns only NA is not a fit. `run_nowcast()` wraps the
      # backend in `suppressMessages()`, so a backend that quietly gave up would
      # otherwise record as a success.
      expect_false(all(is.na(predictions$.value)))
      # A stratified object must come back stratified, not pooled.
      expect_true(all(get_strata(x) %in% names(predictions)))
    })
  })
}

test_that("a snapshot series is refused with the cap that would fix it", {
  skip_if_not_installed("baselinenowcast")

  x <- dataset_tbl_now("flusight_aligned")

  # The message has to name the delay axis and a number to cap it at.
  # `baselinenowcast`'s own is arithmetic ("112 reference times available and
  # 114 are needed"), which mentions neither.
  expect_error(
    suppressWarnings(suppressMessages(
      run_nowcast(x, engine_baselinenowcast(draws = 10), verbose = FALSE)
    )),
    "too wide to nowcast"
  )
  expect_error(
    suppressWarnings(suppressMessages(
      run_nowcast(x, engine_baselinenowcast(draws = 10), verbose = FALSE)
    )),
    "max_delay"
  )
})

test_that("the delay cap reaches the CONVERTER, not the modelling call", {
  skip_if_not_installed("baselinenowcast")

  # `max_delay` is an argument of `tbl_now_to_baselinenowcast()`, and
  # `baselinenowcast::baselinenowcast()` has none. Before it was named in
  # `nowcast_fit.baselinenowcast()` it went into `...`, straight past the
  # converter and into the modelling call, where it was silently ignored: the
  # user got the same unfittable triangle and no indication why.
  x <- dataset_tbl_now("flusight_aligned")
  capped <- suppressWarnings(suppressMessages(
    tbl_now_to_baselinenowcast(x, max_delay = 47, verbose = FALSE)
  ))
  expect_lte(ncol(capped), 47L)

  nowcast <- suppressWarnings(suppressMessages(run_nowcast(
    x, engine_baselinenowcast(draws = 10, max_delay = 47), verbose = FALSE
  )))
  expect_true(is_tbl_nowcast(nowcast))
})
