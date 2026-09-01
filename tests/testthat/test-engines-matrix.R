# Every engine, against every shape of `tbl_now`.
#
# The contract this file checks is the one `?run_nowcast` states: an engine
# either handles a shape correctly, or it ERRORS. What must never happen is a
# fit that silently answers a different question -- counts read as rows, a
# weekly series modelled as daily, a numeric grid coerced into 1970 dates.
#
# Three tiers, because a Stan/JAGS fit per cell would be hours:
#
#   DRY RUN {0,2 covariates} x {0,2 strata} x {days,weeks} x {the 3 data types}
#           = 24 shapes, run through each engine in the cheapest mode its
#           package offers (`engine_dry_args()`). This tier asks whether the
#           CALL is right -- do the strata, covariates, dates and quantile
#           levels survive the round trip -- not whether the model fits well.
#   REAL    one genuine fit per data type, at full settings, in "each data type
#           is either modelled or refused with a reason" below. A dry run cannot
#           tell you the sampler path works; that test can.
#   MOCKED  the rest of the matrix -- the numeric grid, one stratum, and the slow
#           Bayesian engines -- where the modelling call is replaced and what is
#           asserted is the DATA and ARGUMENTS the engine was handed.
#
# The dry-run tier used to fit every cell for real, and `diseasenowcasting`
# alone took 956 s of it -- 52% of the entire test suite -- for 24 assertions
# about plumbing. See `engine_dry_args()` for what was tried and what worked.
#
# `skip_on_cran()` throughout: these fit models.

skip_on_cran()

# -- the 24 shapes -------------------------------------------------------------

SHAPES <- expand.grid(
  units = c("days", "weeks"),
  data_type = c("linelist", "count-incidence", "count-cumulative"),
  n_strata = c(0L, 2L),
  n_covariates = c(0L, 2L),
  stringsAsFactors = FALSE
)

test_that("the shape grid is the 24 shapes it claims to be", {
  expect_equal(nrow(SHAPES), 24L)
  expect_setequal(unique(SHAPES$units), c("days", "weeks"))
  expect_setequal(
    unique(SHAPES$data_type),
    c("linelist", "count-incidence", "count-cumulative")
  )
})

# EVERY engine, including the three that are `fast = FALSE` in `ENGINE_SPEC`.
# That flag gates the tiers that fit for real, and it is why `NobBS`,
# `epinowcast` and `EpiNow2` were once left out of this grid entirely -- a
# genuine fit per cell was hours. Run as dry runs they cost 9 s, 135 s and
# 198 s, so the PLUMBING tier can afford all six even though the real-fit tiers
# still cannot. `engine_dry_args()` has the per-engine lever and what it costs.
for (engine_name in c(
  "baselinenowcast", "diseasenowcasting", "surveillance", "NobBS",
  "epinowcast", "EpiNow2"
)) {
  local({
    this_engine <- engine_name

    test_that(paste0("all 24 shapes are handled: ", this_engine), {
      skip_if_not_installed(this_engine)
      skip_if_not(
        engine_backend_available(this_engine),
        paste0(this_engine, ": modelling backend not available")
      )

      failures <- character()
      for (row in seq_len(nrow(SHAPES))) {
        shape <- SHAPES[row, ]
        label <- paste(
          this_engine, shape$units, shape$data_type,
          paste0(shape$n_strata, " strata"), paste0(shape$n_covariates, " cov"),
          sep = " / "
        )

        x <- engine_fixture(
          units = shape$units, data_type = shape$data_type,
          n_strata = shape$n_strata, n_covariates = shape$n_covariates,
          n_periods = 30L
        )
        set.seed(20260825L)
        out <- do.call(
          try_run_nowcast,
          c(list(this_engine, x), engine_dry_args(this_engine))
        )

        if (inherits(out, "condition")) {
          failures <- c(failures, paste0(label, ": ", conditionMessage(out)))
          next
        }

        # Content, not just "did not error".
        problems <- character()
        if (!is_tbl_nowcast(out)) problems <- c(problems, "not a tbl_nowcast")
        if (nrow(out@predictions) == 0) problems <- c(problems, "no predictions")
        if (!identical(sort(out@strata), sort(get_strata(x) %||% character(0)))) {
          problems <- c(problems, paste0(
            "strata: got [", paste(out@strata, collapse = ","),
            "] want [", paste(get_strata(x), collapse = ","), "]"
          ))
        }
        # Every declared stratum combination must be represented.
        if (shape$n_strata > 0 && length(out@strata) > 0) {
          combinations <- nrow(dplyr::distinct(
            dplyr::as_tibble(out@predictions)[out@strata]
          ))
          if (combinations != 2^shape$n_strata) {
            problems <- c(problems, paste0(
              "expected ", 2^shape$n_strata, " strata combinations, got ", combinations
            ))
          }
        }
        # Weekly data must come back on a weekly grid. `EpiNow2` models a daily
        # process, so this is the axis a converter silently gets wrong.
        dates <- sort(unique(out@predictions[[out@event_date]]))
        if (identical(shape$units, "weeks") && length(dates) > 1 &&
          !all(as.integer(diff(dates)) %% 7L == 0L)) {
          problems <- c(problems, "event dates are not weekly-spaced")
        }

        if (engine_dry_run_is_valueless(this_engine, shape$data_type)) {
          # Asserted, not tolerated: if this ever stops being all-NA the
          # exception in `engine_dry_run_is_valueless()` has been fixed
          # upstream and should be deleted, and a silent pass would hide that.
          if (!all(is.na(out@predictions$.value))) {
            problems <- c(problems, paste0(
              "the dry run now returns values -- drop this data type from ",
              "engine_dry_run_is_valueless()"
            ))
          }
        } else {
          # A dry run may be a crude model, but it may not be an ABSENT one:
          # all-NA draws would satisfy every shape assertion above.
          if (anyNA(out@predictions$.value)) {
            problems <- c(problems, "NA predictions")
          }
          # Quantiles must be monotone in the level: a crossing means the
          # summarisation is wrong whatever the model said.
          crossing <- dplyr::as_tibble(out@predictions) |>
            dplyr::group_by(dplyr::across(dplyr::all_of(
              c(out@event_date, out@strata)
            ))) |>
            dplyr::summarise(
              bad = is.unsorted(.data$.value[order(.data$.quantile_level)]),
              .groups = "drop"
            )
          if (any(crossing$bad)) problems <- c(problems, "quantiles cross")
        }

        if (length(problems)) {
          failures <- c(failures, paste0(label, ": ", paste(problems, collapse = "; ")))
        }
      }

      expect_equal(failures, character(0))
    })
  })
}

# -- units: handled, or refused ------------------------------------------------

test_that("a numeric event grid is modelled or refused, never faked", {
  # The failure this guards: a numeric grid's date columns are integer indices,
  # and `as.Date()` on those anchors them at 1970. An engine that "works" on a
  # numeric grid by inventing 1970 dates is worse than one that refuses.
  for (engine_name in available_engines()) {
    spec <- ENGINE_SPEC[[engine_name]]
    x <- engine_fixture(units = "numeric", data_type = "count-incidence", n_periods = 30L)
    set.seed(20260825L)
    out <- try_run_nowcast(engine_name, x)

    if ("numeric" %in% spec$units) {
      expect_false(
        inherits(out, "condition"),
        label = paste0(engine_name, " handles a numeric grid")
      )
      if (!inherits(out, "condition")) {
        dates <- out@predictions[[out@event_date]]
        # It must stay on the numeric grid it was given.
        expect_false(
          inherits(dates, "Date"),
          label = paste0(engine_name, " kept the numeric grid numeric")
        )
      }
    } else {
      expect_true(
        inherits(out, "condition"),
        label = paste0(engine_name, " refuses a numeric grid")
      )
      # And the refusal has to say why.
      if (inherits(out, "condition")) {
        expect_match(
          conditionMessage(out), spec$numeric_error,
          label = paste0(engine_name, " refusal message"), ignore.case = TRUE
        )
      }
    }
  }
})

test_that("weekly data is modelled on a weekly grid, not a daily one", {
  # `EpiNow2` models a DAILY process and has no timestep, so a weekly series
  # passed as one row per week would be an epidemic seven times too fast. The
  # converter lays it on a daily grid with `accumulate`; whatever an engine
  # does, the predictions must come back on the object's own weekly grid.
  for (engine_name in available_engines(fast_only = TRUE)) {
    x <- engine_fixture(units = "weeks", data_type = "count-incidence", n_periods = 30L)
    set.seed(20260825L)
    out <- try_run_nowcast(engine_name, x)
    if (inherits(out, "condition")) next

    dates <- sort(unique(out@predictions[[out@event_date]]))
    gaps <- unique(as.integer(diff(dates)))
    expect_true(
      all(gaps %% 7L == 0L),
      label = paste0(engine_name, " returns weekly-spaced event dates")
    )
  }
})

# -- data types: modelled, or refused ------------------------------------------

test_that("each data type is either modelled or refused with a reason", {
  # THE REAL-FIT TIER. No `engine_dry_args()` here on purpose: the 24-shape grid
  # above runs the engines in their cheapest mode, so this is the only place
  # that exercises the actual sampler. `count-cumulative` is the shape that
  # needs `confirmation_process()` -- without it `diseasenowcasting` reports
  # "Joint fit failed to converge for all init attempts" -- and it is worth the
  # ~24 s it costs. Do not make this one cheaper.
  for (engine_name in available_engines(fast_only = TRUE)) {
    spec <- ENGINE_SPEC[[engine_name]]
    for (data_type in c("linelist", "count-incidence", "count-cumulative")) {
      x <- engine_fixture(units = "days", data_type = data_type, n_periods = 30L)
      set.seed(20260825L)
      out <- try_run_nowcast(engine_name, x)
      label <- paste0(engine_name, " / ", data_type)

      if (data_type %in% spec$data_type) {
        expect_false(inherits(out, "condition"), label = paste0(label, " fits"))
      } else {
        expect_true(inherits(out, "condition"), label = paste0(label, " refuses"))
      }
    }
  }
})

test_that("counts are nowcast as CASES, not as rows", {
  # The failure that motivated `tbl_now_to_nobbs()`: a row-counting engine
  # handed count data nowcasts 30 rows as 30 cases when they carry 1,800. The
  # fixture is built so the two differ by an order of magnitude.
  skip_if_not_installed("NobBS")

  x <- engine_fixture(units = "days", data_type = "count-incidence", n_periods = 25L)
  total <- sum(x$n)
  expect_gt(total, 5 * nrow(x))

  seen <- NULL
  local_mocked_bindings(
    NobBS = function(data, ...) {
      seen <<- data
      list(estimates = data.frame(
        onset_date = unique(data$onset_date), estimate = 1, q_0.5 = 1
      ))
    },
    .package = "NobBS"
  )
  suppressWarnings(run_nowcast(
    x, engine_nobbs(max_D = 3, moving_window = 20), verbose = FALSE
  ))

  expect_equal(nrow(seen), total)
})

# -- strata: 0, 1 and many -----------------------------------------------------

test_that("every engine models 0, 1 and 2 strata and labels them correctly", {
  for (engine_name in available_engines(fast_only = TRUE)) {
    for (n_strata in c(0L, 1L, 2L)) {
      x <- engine_fixture(
        units = "days", data_type = "count-incidence",
        n_strata = n_strata, n_periods = 30L
      )
      set.seed(20260825L)
      out <- try_run_nowcast(engine_name, x)
      label <- paste0(engine_name, " / ", n_strata, " strata")

      expect_false(inherits(out, "condition"), label = paste0(label, " fits"))
      if (inherits(out, "condition")) next

      expect_equal(
        sort(out@strata), sort(get_strata(x) %||% character(0)),
        label = paste0(label, " reports its strata")
      )

      if (n_strata == 0L) {
        expect_equal(unique(tidy(out)$stratum), "all", label = label)
      } else {
        # Each declared combination present exactly once per event date, and
        # `(stratum, event_date)` unique -- the pairing failure that pooling
        # bugs produce.
        tidied <- tidy(out)
        expect_equal(
          nrow(dplyr::distinct(tidied, stratum, event_date)), nrow(tidied),
          label = paste0(label, " has a unique (stratum, event_date) key")
        )
        expect_equal(
          length(unique(tidied$stratum)), 2^n_strata,
          label = paste0(label, " reports every combination")
        )
      }
    }
  }
})

test_that("run_nowcast() agrees with the hand-written call", {
  # The two front doors must produce the same numbers. `baselinenowcast` is the
  # one that can be reproduced exactly: same seed, same triangle, same draws.
  skip_if_not_installed("baselinenowcast")

  x <- engine_fixture(units = "days", data_type = "count-incidence", n_periods = 30L)

  set.seed(20260825L)
  through_run_nowcast <- suppressWarnings(suppressMessages(
    run_nowcast(x, engine_baselinenowcast(draws = 100), verbose = FALSE)
  ))

  set.seed(20260825L)
  by_hand <- suppressWarnings(suppressMessages({
    triangle <- tbl_now_to_baselinenowcast(x, verbose = FALSE)
    baselinenowcast::baselinenowcast(triangle, output_type = "samples", draws = 100)
  }))

  hand_tidy <- suppressWarnings(tidy(by_hand))
  run_tidy <- tidy(through_run_nowcast)

  expect_equal(nrow(hand_tidy), nrow(run_tidy))
  expect_equal(hand_tidy$event_date, run_tidy$event_date)
  expect_equal(hand_tidy$estimate, run_tidy$estimate)
})

# -- the shape the grid above cannot make --------------------------------------
#
# Every cell of the 24-shape grid has a SHORT delay tail: `engine_fixture()`
# reports each event period over three delays against forty event periods, so
# the reporting triangle is always far taller than it is wide. That is one axis
# the grid does not vary, and the `flusight` failure lived in it -- a snapshot
# ("as of") series restates the whole history in every snapshot, so the delay
# axis is as long as the series and the triangle comes out square, which no cell
# of the grid can be.

test_that("a square reporting triangle is refused with the cap that fixes it", {
  skip_if_not_installed("baselinenowcast")

  x <- snapshot_fixture()
  triangle <- suppressWarnings(suppressMessages(
    tbl_now_to_baselinenowcast(x, verbose = FALSE)
  ))
  # The converter succeeds. This is the whole reason the converter matrix could
  # not have caught it.
  expect_s3_class(triangle, "reporting_triangle")
  expect_equal(nrow(triangle), ncol(triangle))

  expect_error(
    suppressWarnings(suppressMessages(
      run_nowcast(x, engine_baselinenowcast(draws = 10), verbose = FALSE)
    )),
    "too wide to nowcast"
  )
})

test_that("capping the delay axis makes a snapshot series fittable", {
  skip_if_not_installed("baselinenowcast")

  x <- snapshot_fixture()
  nowcast <- suppressWarnings(suppressMessages(
    run_nowcast(x, engine_baselinenowcast(draws = 10, max_delay = 4), verbose = FALSE)
  ))

  expect_true(is_tbl_nowcast(nowcast))
  predictions <- tibble::as_tibble(nowcast)
  expect_gt(nrow(predictions), 0)
  expect_false(all(is.na(predictions$.value)))
})
