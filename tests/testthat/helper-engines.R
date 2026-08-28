# Fixtures and a capability matrix for the engine tests.
#
# Everything here is BUILT, not taken from a shipped dataset: a synthetic series
# can be varied one axis at a time (units, data type, strata, covariates,
# censoring), which is the only way to say which axis broke an engine. The
# shipped datasets each fix several axes at once.
#
# The series is deliberately boring -- a smooth rise with a short geometric
# reporting delay -- because these tests are about plumbing, not about whether a
# model recovers a hard signal.

#' Build a `tbl_now` with one axis varied at a time
#'
#' @param units `"days"`, `"weeks"` or `"numeric"`.
#' @param data_type `"linelist"`, `"count-incidence"` or `"count-cumulative"`.
#' @param n_strata 0, 1 or 2 stratifying columns (`sex`, then `region`).
#' @param n_covariates 0 or 2 covariate columns.
#' @param censored Add an `is_censored` flag that varies WITHIN a cell (the
#'   per-case kind that actually breaks a reporting triangle), not the
#'   delay-derived kind that is constant within one.
#' @param n_periods How many event periods.
#' @param max_delay Longest reporting delay, in periods.
#' @param seed RNG seed.
#'
#' @return A `tbl_now`.
engine_fixture <- function(units = "days",
                           data_type = "count-incidence",
                           n_strata = 0L,
                           n_covariates = 0L,
                           censored = FALSE,
                           n_periods = 40L,
                           max_delay = 3L,
                           seed = 20260825L) {
  set.seed(seed)

  step <- switch(units, days = 1L, weeks = 7L, numeric = 1L)
  origin <- as.Date("2021-01-04") # a Monday, so weekly grids are clean
  periods <- seq_len(n_periods) - 1L

  event <- if (identical(units, "numeric")) {
    periods
  } else {
    origin + step * periods
  }

  strata_levels <- list(sex = c("F", "M"), region = c("north", "south"))
  strata_cols <- names(strata_levels)[seq_len(n_strata)]

  grid <- tidyr::expand_grid(
    .period = periods,
    .delay_p = seq_len(max_delay + 1L) - 1L
  )
  if (n_strata > 0) {
    grid <- tidyr::expand_grid(grid, !!!strata_levels[strata_cols])
  }

  grid <- grid |>
    dplyr::mutate(
      event_time = if (identical(units, "numeric")) {
        .data$.period
      } else {
        origin + step * .data$.period
      },
      report_time = if (identical(units, "numeric")) {
        .data$.period + .data$.delay_p
      } else {
        origin + step * (.data$.period + .data$.delay_p)
      },
      # A smooth rise, times a geometric delay profile: no zero cells, so every
      # engine has something to fit in every cell.
      n = as.integer(round(
        (10 + 2 * .data$.period) * 0.5^.data$.delay_p
      )) + 1L
    )

  if (n_covariates > 0) {
    grid <- grid |>
      dplyr::mutate(
        temperature = round(20 + 5 * sin(2 * pi * .data$.period / 12), 2),
        humidity = round(60 + 10 * cos(2 * pi * .data$.period / 9), 2)
      )
  }
  covariate_cols <- if (n_covariates > 0) c("temperature", "humidity") else character(0)

  if (censored) {
    # Per-CASE censoring: varies within an (event, report) cell, so it genuinely
    # splits the cell. The delay-derived kind is constant within a cell and
    # would prove nothing.
    grid <- dplyr::bind_rows(
      dplyr::mutate(grid, is_censored = FALSE),
      dplyr::mutate(grid, is_censored = TRUE, n = 1L)
    )
  }

  # TRUNCATE at `now`, which is the last EVENT period. Without this the fixture
  # is a complete rectangle -- every delay observed for every event date -- and
  # there is nothing left to nowcast: `baselinenowcast` refuses such a triangle
  # outright ("doesn't contain any missing values"). Real data always has the
  # newest event dates only partly reported, and that incompleteness is the
  # whole problem.
  now_time <- if (identical(units, "numeric")) {
    max(periods)
  } else {
    origin + step * max(periods)
  }
  grid <- dplyr::filter(grid, .data$report_time <= now_time)

  data <- grid |>
    dplyr::select(
      event_time = "event_time", report_time = "report_time",
      dplyr::all_of(c(strata_cols, covariate_cols)),
      dplyr::any_of("is_censored"), "n"
    )

  if (identical(data_type, "linelist")) {
    data <- data |>
      tidyr::uncount(weights = .data$n) |>
      dplyr::as_tibble()
  } else if (identical(data_type, "count-cumulative")) {
    data <- data |>
      dplyr::arrange(.data$report_time) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(
        c("event_time", strata_cols, if (censored) "is_censored")
      ))) |>
      dplyr::mutate(n = cumsum(.data$n)) |>
      dplyr::ungroup()
  }

  suppressWarnings(tbl_now(
    data,
    now = now_time,
    event_date = "event_time",
    report_date = "report_time",
    case_count = if (identical(data_type, "linelist")) NULL else "n",
    strata = if (n_strata > 0) strata_cols else NULL,
    covariates = if (n_covariates > 0) covariate_cols else NULL,
    is_censored = if (censored) "is_censored" else NULL,
    data_type = data_type,
    event_units = units,
    report_units = units,
    verbose = FALSE
  ))
}

# What each engine can be handed, and what it does with it -----------------
#
# This table IS the specification the tests check. `units` and `data_type` list
# what the engine accepts; anything else must ERROR rather than fit something
# meaningless. `strata` says how many stratifying columns it can model.
#
# Read it against `?run_nowcast`: if the two disagree, one of them is wrong and
# the disagreement is the bug.
ENGINE_SPEC <- list(
  baselinenowcast = list(
    units = c("days", "weeks"),
    data_type = c("linelist", "count-incidence", "count-cumulative"),
    strata = Inf,
    draws = TRUE,
    # A triangle needs a delay axis in whole periods; a numeric grid has no
    # unit to resolve `delays_unit` from.
    numeric_error = "delays_unit|units",
    fast = TRUE
  ),
  diseasenowcasting = list(
    units = c("days", "weeks"),
    data_type = c("linelist", "count-incidence", "count-cumulative"),
    strata = Inf,
    draws = TRUE,
    # It does not refuse a numeric grid so much as fall over on one: the message
    # is a cli formatting failure inside the package ("Could not evaluate cli
    # `{}` expression: `descr`") rather than an explanation. Recorded as-is,
    # because pretending it is a clean refusal would hide an upstream bug.
    numeric_error = "descr|numeric|units",
    fast = TRUE
  ),
  surveillance = list(
    units = c("days", "weeks"),
    data_type = c("linelist", "count-incidence", "count-cumulative"),
    strata = Inf,
    draws = FALSE,
    numeric_error = "aggregate|numeric",
    fast = TRUE
  ),
  NobBS = list(
    units = c("days", "weeks"),
    data_type = c("linelist", "count-incidence", "count-cumulative"),
    strata = Inf,
    draws = FALSE,
    numeric_error = "event units|1 day|1 week",
    fast = FALSE
  ),
  epinowcast = list(
    units = c("days", "weeks"),
    data_type = c("linelist", "count-incidence", "count-cumulative"),
    strata = Inf,
    draws = TRUE,
    numeric_error = "timestep|units",
    fast = FALSE
  ),
  EpiNow2 = list(
    units = c("days", "weeks"),
    data_type = c("linelist", "count-incidence", "count-cumulative"),
    strata = Inf,
    draws = FALSE,
    numeric_error = "numeric|accumulate|units",
    fast = FALSE
  )
)

#' Engines whose backing package is installed
available_engines <- function(fast_only = FALSE) {
  names <- names(ENGINE_SPEC)
  if (fast_only) {
    names <- names[vapply(ENGINE_SPEC[names], `[[`, logical(1), "fast")]
  }
  names[vapply(names, requireNamespace, logical(1), quietly = TRUE)]
}

#' Arguments that keep each engine quick on a 40-period fixture
engine_args <- function(engine, x) {
  periods <- length(unique(x[[get_event_date(x)]]))
  switch(engine,
    baselinenowcast = list(draws = 50),
    # A `count-cumulative` stream needs a CONFIRMATION PROCESS: the signed
    # increments it models can go down, and `model()`'s default is
    # `no_confirmation()`, under which the fit reports "Joint fit failed to
    # converge for all init attempts".
    #
    # `run_nowcast()` deliberately does not inject one -- picking a model
    # component on the caller's behalf would change what the fit answers -- so
    # the caller supplies it, and this is what that looks like.
    diseasenowcasting = c(
      list(n_draws = 100),
      if (identical(get_data_type(x), "count-cumulative")) {
        list(model = diseasenowcasting::model(
          confirmation = diseasenowcasting::confirmation_process()
        ))
      }
    ),
    surveillance = list(D = 3),
    NobBS = list(max_D = 3, moving_window = min(20L, periods)),
    epinowcast = list(
      preprocess_args = list(max_delay = 4),
      fit = epinowcast::enw_fit_opts(
        chains = 1, iter_sampling = 100, iter_warmup = 100,
        show_messages = FALSE, refresh = 0
      )
    ),
    EpiNow2 = list(
      generation_time = EpiNow2::gt_opts(EpiNow2::Fixed(1)),
      delays = EpiNow2::delay_opts(EpiNow2::LogNormal(meanlog = 0.5, sdlog = 0.5, max = 4)),
      stan = EpiNow2::stan_opts(samples = 100, warmup = 100, chains = 1)
    ),
    list()
  )
}

#' The cheapest mode each engine's package offers
#'
#' The 24-shape grid in `test-engines-matrix.R` asks whether the CALL is right
#' -- that the strata, covariates, dates and quantile levels survive the round
#' trip -- not whether the model fits well. So it runs each engine as a dry run.
#'
#' * `diseasenowcasting` simulates from the prior instead of sampling
#'   (`prior_only`), which takes its 24 shapes from **956 s to 8 s**. The whole
#'   956 s was the eight `count-cumulative` shapes -- one of them alone was
#'   137 s -- and none of the obvious "make it smaller" levers help: measured on
#'   the fixture, `delay_window = 4` changes nothing, `K = 5` makes one shape SIX
#'   TIMES slower (24 s -> 140 s) and the three together reach 1447 s, because
#'   fewer basis functions break convergence and the fitter retries.
#' * `baselinenowcast` draws 10 samples instead of 50 (32 s -> 7 s).
#' * `NobBS` runs a short JAGS chain instead of its 1000/1000/10000 default,
#'   which is what lets it be in the grid at all: 8 s for the 24 shapes against
#'   about 2 minutes at its own settings.
#' * `surveillance` has no such lever and needs none: its 24 shapes take 10 s.
#'   Do NOT cap its `N.tInf.max` to speed it up. The support cap has to exceed
#'   the counts, and below them the fit dies with `subscript out of bounds`,
#'   which names nothing -- see the comment in `nowcast_fit.surveillance()`.
#'
#' * `epinowcast` and `EpiNow2` drop their likelihood and sample the prior, via
#'   `enw_fit_opts(likelihood = FALSE)` and `obs_opts(likelihood = FALSE)`.
#'   (There is no `passthrough` argument in `epinowcast` 0.7.0; `likelihood` is
#'   the flag, and it sets `likelihood = 0` in the Stan data.) Both give real,
#'   non-NA, monotone quantiles.
#'
#' What actually costs on those last two is NOT the likelihood, and measuring it
#' is the only way to see that:
#'
#' * `epinowcast` spends about **90 s compiling its Stan model, once per R
#'   process** (`enw_model()` compiles into a temp dir). After that a fit is
#'   1.5 s, so all 24 shapes cost 135 s and the 12 unstratified ones cost 104 s
#'   -- almost the same, because the compile dominates either way. Turning the
#'   likelihood off saves 6 s of 101; `enw_pathfinder()` saves nothing. Do not
#'   read the first fit in a session as the cost of a fit.
#' * `EpiNow2` has no compile cost (rstan, precompiled) but about 4 s of setup
#'   per fit, and a 2-strata shape is four fits. That, not the sampler, is its
#'   198 s: cutting `samples`/`warmup` from 100 to 25 moved one shape 21.1 s ->
#'   17.5 s.
#'
#' A dry run cannot tell you the SAMPLER path works. That is what
#' `"each data type is either modelled or refused with a reason"` is for: it
#' fits every data type for real, at full settings.
#'
#' @param engine A method name.
#'
#' @return A list of arguments to override `engine_args()` with.
#'
engine_dry_args <- function(engine) {
  switch(engine,
    diseasenowcasting = list(prior_only = TRUE),
    baselinenowcast = list(draws = 10),
    NobBS = list(specs = list(nAdapt = 100, nBurnin = 100, nSamp = 200)),
    epinowcast = list(fit = epinowcast::enw_fit_opts(
      likelihood = FALSE, chains = 1, iter_sampling = 100, iter_warmup = 100,
      show_messages = FALSE, refresh = 0
    )),
    EpiNow2 = list(obs = EpiNow2::obs_opts(likelihood = FALSE)),
    list()
  )
}

#' Whether a dry run of this engine returns NA instead of numbers
#'
#' `diseasenowcasting`'s prior simulation does not support cumulative input (as
#' of 2.1.0): every draw comes back `NA` while the SHAPE -- rows, strata, dates,
#' quantile levels -- is still right, and it does so silently rather than
#' erroring. The dry run therefore still proves the plumbing for those shapes;
#' it just cannot say anything about the numbers, which the real fits cover.
#'
#' The grid asserts this is still true, so that an upstream fix shows up as a
#' failure telling you to delete the exception rather than passing unnoticed.
#'
#' @param engine A method name.
#' @param data_type A `tbl_now` data type.
#'
#' @return `TRUE` when the dry run cannot produce values.
engine_dry_run_is_valueless <- function(engine, data_type) {
  identical(engine, "diseasenowcasting") &&
    identical(data_type, "count-cumulative")
}

#' Build the `engine()` object for one method on one fixture
#'
#' `engine_args()` above is keyed by METHOD NAME, which is how these tests are
#' written -- they loop over `available_engines()`. `engine()` is the general
#' constructor precisely so a name plus a list of arguments can be turned into an
#' engine without a `switch()` over the six specific ones.
#'
#' An override REPLACES the default argument; it does not merge with it. That is
#' deliberate, and it is not what [utils::modifyList()] does: `modifyList()`
#' recurses into list-valued arguments, so overriding one merges it with the
#' default instead. Several of these arguments ARE lists --
#' `epinowcast`'s `fit`, `EpiNow2`'s `obs` and `stan` -- and merging them is not
#' a subtle difference. Passing `fit = epinowcast::enw_fit_opts(sampler =
#' epinowcast::enw_pathfinder)` left the default `chains` and `iter_sampling`
#' sitting in the merged list, and `enw_pathfinder()` then died on them as
#' unused arguments, which reads as "pathfinder is broken" rather than "your
#' override did not take".
#'
#' Single-bracket assignment, so that an explicit `NULL` override sets the
#' argument to `NULL` rather than deleting it.
engine_for <- function(method, x, ...) {
  overrides <- list(...)
  arguments <- engine_args(method, x)
  arguments[names(overrides)] <- overrides
  do.call(engine, c(list(method), arguments))
}

#' Fit an engine, returning either the nowcast or the error condition
try_run_nowcast <- function(method, x, ...) {
  tryCatch(
    suppressWarnings(suppressMessages(
      run_nowcast(x, engine_for(method, x, ...), verbose = FALSE)
    )),
    error = function(e) e
  )
}
