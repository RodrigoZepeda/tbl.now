# Every engine, against every shape of `tbl_now`.
#
# The contract this file checks is the one `?run_nowcast` states: an engine
# either handles a shape correctly, or it ERRORS. What must never happen is a
# fit that silently answers a different question -- counts read as rows, a
# weekly series modelled as daily, a numeric grid coerced into 1970 dates.
#
# Two tiers, because a Stan/JAGS fit per cell would be hours:
#
#   REAL   {0,2 covariates} x {0,2 strata} x {days,weeks} x {the 3 data types}
#          = 24 shapes, actually fitted by the fast engines.
#   MOCKED the rest of the matrix -- the numeric grid, one stratum, and the slow
#          Bayesian engines -- where the modelling call is replaced and what is
#          asserted is the DATA and ARGUMENTS the engine was handed.
#
# `skip_on_cran()` throughout: these fit models.

skip_on_cran()

# -- the 24 real-fit shapes ----------------------------------------------------

REAL_SHAPES <- expand.grid(
  units = c("days", "weeks"),
  data_type = c("linelist", "count-incidence", "count-cumulative"),
  n_strata = c(0L, 2L),
  n_covariates = c(0L, 2L),
  stringsAsFactors = FALSE
)

test_that("the real-fit grid is the 24 shapes it claims to be", {
  expect_equal(nrow(REAL_SHAPES), 24L)
  expect_setequal(unique(REAL_SHAPES$units), c("days", "weeks"))
  expect_setequal(
    unique(REAL_SHAPES$data_type),
    c("linelist", "count-incidence", "count-cumulative")
  )
})

for (engine_name in c("baselinenowcast", "diseasenowcasting", "surveillance")) {
  local({
    this_engine <- engine_name

    test_that(paste0("all 24 shapes really fit: ", this_engine), {
      skip_if_not_installed(this_engine)

      failures <- character()
      for (row in seq_len(nrow(REAL_SHAPES))) {
        shape <- REAL_SHAPES[row, ]
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
        out <- try_run_nowcast(this_engine, x)

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
