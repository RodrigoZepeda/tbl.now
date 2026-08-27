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

#' Build the `engine()` object for one method on one fixture
#'
#' `engine_args()` above is keyed by METHOD NAME, which is how these tests are
#' written -- they loop over `available_engines()`. `engine()` is the general
#' constructor precisely so a name plus a list of arguments can be turned into an
#' engine without a `switch()` over the six specific ones.
engine_for <- function(method, x, ...) {
  arguments <- utils::modifyList(engine_args(method, x), list(...))
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
