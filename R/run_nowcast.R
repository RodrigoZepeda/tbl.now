# `run_nowcast()`: one entry point, many nowcasting packages.
#
# The dispatch is deliberately boring S3. `run_nowcast(x, engine_foo(...))`
# takes the engine -- an object of class `c("foo", "nowcast_engine")` carrying
# its own arguments (see R/nowcast_engine.R) -- and calls two generics on it:
#
#   nowcast_fit(method, x, ...)        -> whatever the backend returns
#   nowcast_tidy(method, fit, x, ...)  -> list(predictions =, draws =)
#
# Adding a backend therefore means writing two S3 methods; nothing in tbl.now
# needs to change and the methods can live in any package. See
# `vignette("custom-nowcast-models")`.

#' Built-in method aliases
#'
#' Maps case-insensitive user input onto the canonical S3 class names of the
#' methods that ship with tbl.now. Unknown names are passed through untouched so
#' that third-party methods keep working.
#'
#' @param method A single string.
#'
#' @return A single string: the canonical method name.
#'
#' @keywords internal
#' @noRd
.canonical_nowcast_method <- function(method) {
  builtin <- c(
    diseasenowcasting = "diseasenowcasting",
    baselinenowcast   = "baselinenowcast",
    epinowcast        = "epinowcast",
    surveillance      = "surveillance",
    epinow2           = "EpiNow2",
    nobbs             = "NobBS"
  )
  hit <- builtin[tolower(method)]
  if (is.na(hit)) method else unname(hit)
}

#' Fit a nowcast with one modelling package
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' `nowcast_fit()` and [nowcast_tidy()] are the two extension points of the
#' nowcasting framework. Together they teach [run_nowcast()] about a new
#' modelling package: `nowcast_fit()` runs the model, `nowcast_tidy()` turns
#' whatever it returned into the tidy quantile format every other function in
#' tbl.now understands.
#'
#' Dispatch happens on the object built by [engine()], so a method for
#' `"mypackage"` is a function called `nowcast_fit.mypackage()`. It can live in
#' any package.
#'
#' @param method A [engine()] object. (The argument is called `method` for
#'   historical reasons and because that is what it selects; what arrives is the
#'   engine, so `method$args`, `method$label` and the rest are available to a
#'   backend that wants them.)
#' @param x A `tbl_now` object.
#' @param ... Arguments passed straight to the underlying modelling function.
#'   [run_nowcast()] splices the engine's own arguments in here.
#' @param quantile_levels Numeric vector of probabilities. Most backends ignore
#'   it at fit time (the quantiles are computed from the draws afterwards), but
#'   some need to be told up front which levels to report.
#' @param verbose Logical. Whether the backend (and the converters feeding it)
#'   should be chatty.
#' @param draws (`"baselinenowcast"` only) Number of nowcast samples to draw.
#' @param delays_unit (`"baselinenowcast"` only) Unit of the reporting
#'   triangle's delay axis; inferred from the object's time units when `NULL`.
#' @param preprocess_args (`"epinowcast"` only) A list of arguments for
#'   [tbl_now_to_epinowcast()], e.g. `list(max_delay = 20)`.
#' @param specs (`"NobBS"` only) The `specs` list of `NobBS::NobBS()`. The
#'   `quantiles` element is filled from `quantile_levels` unless you set it.
#' @param when,D,fit_method,control (`"surveillance"` only) The `when`, `D`,
#'   `method` and `control` arguments of [surveillance::nowcast()]. `when`
#'   defaults to `get_surveillance_when(x, length = D + 1)`, `D` to the largest
#'   delay in the data, and `control$dRange` to [get_surveillance_range()] --
#'   the grid running to [get_now()], which a line list cannot express on its
#'   own. Both grids are built from the whole object, so every stratum is fitted
#'   on the same time axis. `fit_method` is \pkg{surveillance}'s `method`
#'   argument, renamed so it cannot collide with [run_nowcast()]'s own `method`.
#' @param convert_args (`"EpiNow2"` only) A list of arguments for
#'   [tbl_now_to_EpiNow2()], e.g. `list(accumulate = FALSE)`.
#'
#' @return `nowcast_fit()` returns the modelling package's own object, verbatim.
#'   It is stored in the `fit` property of the resulting [tbl_nowcast], and it is
#'   the only thing [nowcast_tidy()] is given besides the `tbl_now` itself, so
#'   put whatever the tidying step will need into it.
#'
#' @seealso [nowcast_tidy()], [run_nowcast()], [list_nowcast_methods()] and
#'   `vignette("custom-nowcast-models")` for a worked example of a new backend.
#'
#' @examples
#' # A minimal backend: two S3 methods and you are done.
#' nowcast_fit.constant <- function(method, x, ..., quantile_levels, verbose = TRUE) {
#'   counts <- get_latest_reported_cases(x)
#'   list(dates = counts[[get_event_date(x)]], value = counts[[ncol(counts)]])
#' }
#'
#' nowcast_tidy.constant <- function(method, fit, x, ..., quantile_levels) {
#'   predictions <- tidyr::expand_grid(
#'     event_date = fit$dates, .quantile_level = quantile_levels
#'   )
#'   predictions$.value <- rep(fit$value, each = length(quantile_levels))
#'   names(predictions)[1] <- get_event_date(x)
#'   list(predictions = predictions, draws = NULL)
#' }
#'
#' @export
nowcast_fit <- function(method, x, ..., quantile_levels = nowcast_quantile_levels(),
                        verbose = TRUE) {
  UseMethod("nowcast_fit")
}

#' @rdname nowcast_fit
#' @export
nowcast_fit.default <- function(method, x, ..., quantile_levels = nowcast_quantile_levels(),
                                verbose = TRUE) {
  name <- if (is.list(method) && !is.null(method$name)) method$name else class(method)[1]
  cli::cli_abort(c(
    "No nowcasting method called {.val {name}} is registered.",
    "i" = "Available methods: {.val {list_nowcast_methods()}}.",
    "i" = "To add your own, define {.fn nowcast_fit.{name}} and \\
           {.fn nowcast_tidy.{name}}. See {.code vignette(\"custom-nowcast-models\")}."
  ))
}

#' Standardise a fitted nowcast
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' The second extension point of the nowcasting framework (see [nowcast_fit()]).
#' It receives the object the modelling package returned and must express its
#' predictions in the tidy format tbl.now uses everywhere else.
#'
#' @param method A [engine()] object.
#' @param fit The object returned by [nowcast_fit()].
#' @param x The `tbl_now` the nowcast was produced from.
#' @param ... Not forwarded by [run_nowcast()], which passes the user's `...` to
#'   [nowcast_fit()] only. Anything the tidying step needs -- a number of draws,
#'   a tuning parameter, a lookup table -- must therefore travel inside the
#'   object `nowcast_fit()` returned.
#' @param quantile_levels Numeric vector of probabilities the predictions should
#'   be summarised at.
#'
#' @return A list with two elements:
#'
#'   \describe{
#'     \item{predictions}{A data frame with one row per (event date, stratum,
#'       quantile level) and the columns `<event_date>`, the strata columns,
#'       `.quantile_level` and `.value`.}
#'     \item{draws}{Either `NULL`, or a data frame with one row per (event date,
#'       stratum, draw) and the columns `<event_date>`, the strata columns,
#'       `.draw` and `.value`.}
#'   }
#'
#'   When `draws` is supplied and `predictions` is `NULL`, [run_nowcast()]
#'   derives the quantiles from the draws for you.
#'
#' @seealso [nowcast_fit()], [run_nowcast()]
#'
#' @examples
#' # See `?nowcast_fit` for a complete two-method backend.
#' methods(nowcast_tidy)
#'
#' @export
nowcast_tidy <- function(method, fit, x, ..., quantile_levels) {
  UseMethod("nowcast_tidy")
}

#' @rdname nowcast_tidy
#' @export
nowcast_tidy.default <- function(method, fit, x, ..., quantile_levels) {
  name <- if (is.list(method) && !is.null(method$name)) method$name else class(method)[1]
  cli::cli_abort(c(
    "{.fn nowcast_fit.{name}} exists but {.fn nowcast_tidy.{name}} does not.",
    "i" = "A backend needs both: one to fit the model, one to standardise its output."
  ))
}

#' List the available nowcasting methods
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' Scans the S3 methods registered for [nowcast_fit()] in every loaded
#' namespace, so any backend you (or another package) defined shows up here as
#' soon as it is loaded.
#'
#' @param installed_only Logical. When `TRUE` (default) only the methods whose
#'   backing package is installed are returned. Set to `FALSE` to see every
#'   registered method.
#'
#' @return A character vector of method names.
#'
#' @examples
#' list_nowcast_methods()
#'
#' # Including methods whose modelling package is not installed
#' list_nowcast_methods(installed_only = FALSE)
#'
#' @export
list_nowcast_methods <- function(installed_only = TRUE) {
  registered <- as.character(utils::methods("nowcast_fit"))
  names <- sub("^nowcast_fit\\.", "", registered)
  names <- setdiff(names, "default")

  if (isTRUE(installed_only)) {
    # A method name usually *is* the package name; when it is not (a
    # hand-written backend), there is nothing to check, so keep it.
    keep <- vapply(
      names,
      function(nm) {
        !nm %in% .known_backend_packages() || requireNamespace(nm, quietly = TRUE)
      },
      logical(1)
    )
    names <- names[keep]
  }

  sort(names)
}

#' Method names that correspond to a real (optional) package
#'
#' @return A character vector.
#'
#' @keywords internal
#' @noRd
.known_backend_packages <- function() {
  c(
    "diseasenowcasting", "baselinenowcast", "epinowcast", "surveillance",
    "EpiNow2", "NobBS"
  )
}

#' Nowcast a `tbl_now` with any supported modelling package
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' Fits a nowcasting model to a `tbl_now` and returns the result in a
#' package-agnostic shape, so that models from different packages can be
#' compared, scored and combined ([nowcast_ensemble()]) without any manual
#' reshaping.
#'
#' The function is named `run_nowcast()` rather than `nowcast()` because
#' \pkg{diseasenowcasting} already exports a `nowcast()` function; keeping the
#' names distinct means both packages can be attached at once.
#'
#' @param x A `tbl_now` object.
#' @param engine A [engine()] object: the modelling package **and every argument
#'   it takes**, including `min_date` and `quantile_levels`. Defaults to
#'   `engine_diseasenowcasting()`.
#'
#'   The data and `verbose` are the only things that sit outside it. That is the
#'   point: an argument in an outer `...` had to be routed to the right backend
#'   by name, and one that missed simply vanished, leaving the model at its
#'   default with nothing to say so.
#' @param verbose Logical. Whether to report what is being done.
#'
#' @section Engines:
#'
#' \describe{
#'   \item{[engine_diseasenowcasting()]}{Bayesian structural time series. The
#'     `tbl_now` is passed in directly, so strata and temporal effects are picked
#'     up automatically.}
#'   \item{[engine_baselinenowcast()]}{Fast, assumption-light baseline built from
#'     the reporting triangle. Stratified objects are nowcast one triangle per
#'     stratum.}
#'   \item{[engine_epinowcast()]}{Bayesian model with separate delay and
#'     reference modules; `preprocess_args` controls
#'     [tbl_now_to_epinowcast()].}
#'   \item{[engine_surveillance()]}{Höhle & an der Heiden's nowcast, fed by
#'     [tbl_now_to_surveillance()]. The package models one series, so a
#'     stratified object is fitted one stratum at a time.}
#'   \item{[engine_epinow2()]}{`EpiNow2::estimate_infections()`, fed by
#'     [tbl_now_to_EpiNow2()]; `EpiNow2::regional_epinow()` when the object
#'     declares strata.}
#'   \item{[engine_nobbs()]}{Nowcasting by Bayesian Smoothing, fed by
#'     [tbl_now_to_nobbs()], which expands counts to the one row per case
#'     \pkg{NobBS} counts.}
#' }
#'
#' [engine()] covers any other registered method, including one you wrote
#' yourself. Every modelling package is an optional dependency: it is only needed
#' when you ask for its engine.
#'
#' @section What the object contributes, and what it does not:
#'
#' `run_nowcast()` reads the `tbl_now`'s declarations and hands each package the
#' shape it wants. Three of those declarations behave differently enough to be
#' worth stating plainly.
#'
#' **Strata.** How many strata a backend can honour is a property of the
#' *package*, not of this one. Where a backend cannot, it warns and pools rather
#' than pretending:
#'
#' | engine | how strata are modelled |
#' |---|---|
#' | `"baselinenowcast"` | one reporting triangle, and one fit, per stratum |
#' | `"surveillance"` | one fit per stratum; the package models a single series |
#' | `"EpiNow2"` | `regional_epinow()` instead of `estimate_infections()` |
#' | `"epinowcast"` | passed to the model as `by`, so they are fitted jointly |
#' | `"diseasenowcasting"` | fitted jointly; the package returns a `[draws x time x stratum]` array, one slice per combination |
#' | `"NobBS"` | `NobBS.strat()` instead of `NobBS()` |
#'
#' **Every backend takes any number of strata.** The two that model one series
#' at a time (`"surveillance"`) or accept a single column
#' (`"NobBS.strat()"`) are given the *interaction* of the declared columns,
#' which is what nowcasting each combination separately means; the label is
#' split back into its columns on the way out.
#'
#' The one thing that can go wrong is a stratum **value** that already contains
#' the `" | "` used to join them. That is an error rather than a guess, because
#' silently mis-assigning strata is worse than refusing.
#'
#' Whatever strata columns come back are what the result reports as its
#' `strata`.
#'
#' Columns you did **not** declare are summed away by the converters (see
#' [tbl_now_to_baselinenowcast()]). A `tbl_now` built from `covid_colombia`
#' without `strata = sex` is nowcast as one pooled series, not silently split.
#'
#' **Temporal effects.** [add_temporal_effects()] specs are lazy; the converters
#' materialise them into ordinary columns, so they travel with the data. Whether
#' the *model* then uses them is a separate question, and mostly the answer is
#' "only if you say so":
#'
#' * `"diseasenowcasting"` receives the `tbl_now` itself and reads the effects
#'   off it, so they enter the model with no further work.
#' * `"epinowcast"` carries them as covariates you name in a module formula, e.g.
#'   `reference = epinowcast::enw_reference(~ 1 + day_of_week, data = ...)`.
#'   Without a formula referring to them they are inert.
#' * every other backend ignores them: the columns ride along so you can split
#'   on them, and nothing else happens.
#'
#' **Censored delays.** A per-case censoring flag (see [add_is_censored()]) puts
#' a censored and an uncensored row in the same `(event date, report date)` cell,
#' and a reporting triangle has one slot per cell. Every backend that goes
#' through a converter therefore **collapses the flag with a warning** — counts
#' are summed over it, line lists drop the column. `"diseasenowcasting"` is the
#' exception: it is handed the object untouched, so the flag reaches the package
#' intact. To *estimate* a delay distribution from censored data, use
#' [tbl_now_to_epidist()] instead; nowcasting and delay estimation are different
#' jobs.
#'
#' @section How each model is specified, and how to change it:
#'
#' `run_nowcast()` does not invent priors or model structure: it calls each
#' package with **that package's own defaults** and passes the engine's
#' arguments straight through. The defaults are not always the ones you want, and
#' two are worth knowing before you read the output.
#'
#' **[engine_epinowcast()]** runs three modules, all at their package defaults.
#' The expectation module is `enw_expectation(r = ~ 0 + (1 | day:.group))` -- a
#' **random effect per day** on the growth rate, which is a random walk on the
#' log expected counts in all but name. The reference module is
#' `enw_reference(parametric = ~ 1, distribution = "lognormal")` -- a **single
#' lognormal reporting delay, constant over time**. The report module is
#' `enw_report(non_parametric = ~ 0)` -- **no day-of-week reporting effect**.
#' Each is a named argument of the engine, and `preprocess_args` carries
#' [tbl_now_to_epinowcast()]'s own arguments:
#'
#' ```r
#' run_nowcast(nowobj, engine_epinowcast(
#'   preprocess_args = list(max_delay = 30),
#'   report = epinowcast::enw_report(~ 1 + day_of_week, data = pobs),
#'   fit    = epinowcast::enw_fit_opts(chains = 4, iter_sampling = 1000)
#' ))
#' ```
#'
#' **[engine_epinow2()]** -- read this before trusting the output.
#' `EpiNow2::estimate_infections()` defaults to `delays = delay_opts()`, which is
#' `Fixed(0)`: **no reporting delay at all**. Its `generation_time = gt_opts()`
#' is `Fixed(1)`, a one-day generation time. Those defaults describe a process
#' with nothing to nowcast, so supply the epidemiology yourself. EpiNow2 also
#' models \eqn{R_t} with a **Gaussian process** by default
#' (`rt_opts(rw = 0, gp_on = "R_t-1")`) rather than a random walk:
#'
#' ```r
#' run_nowcast(nowobj, engine_epinow2(
#'   generation_time = EpiNow2::gt_opts(EpiNow2::example_generation_time),
#'   delays          = EpiNow2::delay_opts(EpiNow2::example_reporting_delay),
#'   rt              = EpiNow2::rt_opts(rw = 7)   # weekly random walk instead
#' ))
#' ```
#'
#' **[engine_diseasenowcasting()]** uses the package's own defaults, reading
#' strata, covariates and temporal effects off the object. `model`, `type` and
#' `n_draws` go to [diseasenowcasting::nowcast()].
#'
#' **[engine_baselinenowcast()]** is not Bayesian and has no priors: the delay is
#' estimated from the reporting triangle and applied. `draws` sets the number of
#' nowcast samples, and `max_delay` caps the triangle's width.
#'
#' **[engine_nobbs()]** has a fixed model; what you tune is `max_D` (maximum
#' delay) and `moving_window` (how much history is fitted). `moving_window`
#' counts **event periods and must not exceed the history you hand it** -- ask
#' for more and NobBS pads its grid backwards and returns zero for every date,
#' with no error. `specs` takes its prior list.
#'
#' **[engine_surveillance()]** takes `fit_method`, which is
#' \pkg{surveillance}'s own `method` argument renamed so it cannot collide with
#' the engine's method; it defaults to `"bayes.notrunc.bnb"`. `D`, `when` and
#' `control` are derived from the object when you do not give them.
#'
#' @return A [tbl_nowcast] object.
#'
#' @seealso [engine()] and [nowcast_engines] to specify a model,
#'   [nowcast_ensemble()] to combine several nowcasts, [nowcast_backtest()] to
#'   score them, and [nowcast_fit()] to add a backend.
#'
#' @examples
#' \donttest{
#' data(denguedat)
#'
#' # A short recent window keeps the example quick.
#' recent <- subset(denguedat, onset_week >= as.Date("2010-06-01"))
#' dengue <- tbl_now(recent,
#'   event_date = onset_week, report_date = report_week, verbose = FALSE
#' )
#'
#' if (requireNamespace("baselinenowcast", quietly = TRUE)) {
#'   nc <- run_nowcast(dengue, engine_baselinenowcast(draws = 100), verbose = FALSE)
#'   nc
#' }
#' }
#'
#' @export
run_nowcast <- function(x, engine = engine_diseasenowcasting(), verbose = TRUE) {
  .assert_tbl_now(x, "run_nowcast")
  .assert_engine(engine)

  quantile_levels <- engine$quantile_levels

  if (isTRUE(verbose)) {
    cli::cli_alert_info(
      "Nowcasting with {.val {engine$name}} as of {.val {get_now(x)}}."
    )
  }

  # Trim BEFORE the fit and keep the trimmed object: everything downstream --
  # the tidying, the `data` property, `autoplot()`'s "reported by now" -- must
  # describe the series the model was actually shown, not the one it was not.
  x <- .engine_trim(x, engine$min_date, verbose = verbose)

  fit <- do.call(
    nowcast_fit,
    c(
      list(engine, x), engine$args,
      list(quantile_levels = quantile_levels, verbose = verbose)
    )
  )
  tidied <- nowcast_tidy(engine, fit, x, quantile_levels = quantile_levels)

  .as_tbl_nowcast(
    tidied,
    x = x, method = engine$name, fit = fit,
    quantile_levels = quantile_levels, call = match.call()
  )
}

#' Assemble the `tbl_nowcast` returned by `run_nowcast()`
#'
#' Validates the list a `nowcast_tidy()` method returned and fills in what can
#' be derived (quantiles from draws, the event-date and strata column names).
#'
#' @param tidied The list returned by [nowcast_tidy()].
#' @param x The source `tbl_now`.
#' @param method Method name.
#' @param fit The backend object.
#' @param quantile_levels Quantile levels requested by the user.
#' @param call The call to record.
#'
#' @return A [tbl_nowcast].
#'
#' @keywords internal
#' @noRd
.as_tbl_nowcast <- function(tidied, x, method, fit, quantile_levels, call) {
  if (!is.list(tidied) || !any(c("predictions", "draws") %in% names(tidied))) {
    cli::cli_abort(c(
      "{.fn nowcast_tidy.{method}} must return a list with elements \\
       {.field predictions} and/or {.field draws}.",
      "i" = "See {.code ?nowcast_tidy} for the expected format."
    ))
  }

  event_col <- get_event_date(x)
  draws <- tidied$draws
  predictions <- tidied$predictions

  # The strata of the nowcast are the declared strata the backend actually kept:
  # some (a pooled reporting triangle, for instance) collapse them.
  available <- unique(c(colnames(predictions), colnames(draws)))
  strata <- intersect(get_strata(x) %||% character(0), available)

  if (is.null(predictions)) {
    if (is.null(draws)) {
      cli::cli_abort("{.fn nowcast_tidy.{method}} returned neither predictions nor draws.")
    }
    predictions <- .draws_to_quantiles(draws, c(event_col, strata), quantile_levels)
  }

  tbl_nowcast(
    predictions = dplyr::as_tibble(predictions),
    draws = if (is.null(draws)) NULL else dplyr::as_tibble(draws),
    method = method,
    fit = fit,
    now = get_now(x),
    event_date = event_col,
    strata = strata,
    data = x,
    call = call,
    metadata = tidied$metadata %||% list()
  )
}
