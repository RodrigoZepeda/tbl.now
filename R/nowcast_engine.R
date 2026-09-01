# Engines: one object holding a method and everything that method needs.
#
# Before 0.27.0 a nowcast was specified as a method NAME plus a `...` that had
# to reach the right backend:
#
#   run_nowcast(x, "NobBS", max_D = 10)
#   nowcast_backtest(x, methods = c(a = "NobBS", b = "epinowcast"),
#                    method_args = list(a = list(max_D = 10), b = list(...)))
#
# Two things went wrong with that, both silently. A misspelled argument
# disappeared into `...` and the backend ran at its default; and `method_args`
# was a list of lists keyed by a label, so an entry under the wrong key was
# simply never applied. Neither errors -- you get a fitted model, and no way of
# telling from the object that it is not the model you asked for.
#
# An engine closes both. `engine_nobbs(max_D = 10)` names its arguments, so a
# typo is an error at construction time rather than a default at fit time, and
# each engine carries its own arguments, so there is no keyed side-table to get
# wrong. `run_nowcast()` and `nowcast_backtest()` take engines and nothing else:
#
#   run_nowcast(x, engine_nobbs(max_D = 10))
#   nowcast_backtest(x, engine_nobbs(max_D = 10), engine_epinowcast(...))

#' Specify a nowcasting model and its arguments
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' An **engine** is one modelling package plus every argument that package
#' needs. It is what [run_nowcast()] and [nowcast_backtest()] take, and it is the
#' object [nowcast_fit()] and [nowcast_tidy()] dispatch on.
#'
#' `engine()` is the general constructor and works for any registered method,
#' including one you wrote yourself. The `engine_*()` functions are its
#' package-specific counterparts: each **names** the arguments of the modelling
#' function it drives, so the ones that matter are visible in the signature and
#' at `?engine_nobbs` rather than buried in a `...` that silently swallows a
#' typo. Anything a named argument does not cover still goes through `...`.
#'
#' @param method A single string naming the method, e.g. `"epinowcast"`. Built-in
#'   names are matched case-insensitively. See [list_nowcast_methods()].
#' @param ... Further arguments for the modelling function, passed through
#'   untouched. In `engine()` this is *every* argument; in the `engine_*()`
#'   functions it is whatever their named arguments do not already cover.
#' @param min_date How much history to fit on. One of
#'
#'   * `NULL` (default) -- the whole series;
#'   * a **`Date`** -- keep event dates on or after it;
#'   * a **single number** -- keep the last *n* periods before the object's
#'     `now`, counted in the object's event units.
#'
#'   The number is usually what you want in a [nowcast_backtest()]: `now` moves
#'   between fits, and a fixed calendar date would make the fitted window grow as
#'   the backtest walks forward, so the last fit would be trained on more data
#'   than the first. Trimming is per engine on purpose -- `baselinenowcast` and
#'   `diseasenowcasting` take a long series in their stride, while `epinowcast`
#'   scales with the number of reference dates and is best given a window.
#' @param quantile_levels Numeric vector of probabilities to report the nowcast
#'   at. Defaults to [nowcast_quantile_levels()].
#'
#'   It lives on the engine because for some backends it is a **fit-time model
#'   argument**, not a way of summarising afterwards. \pkg{NobBS} computes
#'   exactly the quantiles it is handed in `specs$quantiles` and keeps no draws,
#'   so a level it was never asked for cannot be recovered, and
#'   \pkg{surveillance} reports a fixed set and warns rather than interpolating.
#'   The draw-keeping backends -- `baselinenowcast`, `diseasenowcasting`,
#'   `epinowcast` and `EpiNow2` -- answer any level after the fact.
#' @param label Name for this engine in a [nowcast_backtest()] and in the
#'   ensemble weights derived from one. Defaults to the method name. Give one
#'   when the same package appears twice with different settings, which is the
#'   whole reason two `diseasenowcasting` models can be weighted separately.
#'
#' @return An object of class `c(method, "nowcast_engine")`.
#'
#' @seealso [run_nowcast()], [nowcast_backtest()], [list_nowcast_methods()], and
#'   the [*Adding your own nowcasting model* article](https://rodrigozepeda.github.io/tbl.now/articles/custom-nowcast-models.html)
#'   for writing a backend of your own.
#'
#' @examples
#' engine("baselinenowcast", draws = 500)
#'
#' # Built-in names are matched case-insensitively
#' engine("nobbs", max_D = 10)
#'
#' # The same package twice, told apart by `label`
#' engine("diseasenowcasting", label = "default")
#'
#' @export
engine <- function(method, ..., min_date = NULL,
                   quantile_levels = nowcast_quantile_levels(),
                   label = NULL) {
  if (!is.character(method) || length(method) != 1 || is.na(method)) {
    cli::cli_abort("{.arg method} must be a single, non-missing string.")
  }
  method <- .canonical_nowcast_method(method)
  .new_engine(method, list(...), min_date, quantile_levels, label)
}

#' Build the engine object
#'
#' One place where the class, the argument list and the validation live, so the
#' general `engine()` and the six `engine_*()` constructors cannot drift apart.
#'
#' @param method Canonical method name.
#' @param args A list of arguments for the backend. `NULL` entries are dropped,
#'   so a named argument left unset means "the backend's own default" rather
#'   than "pass `NULL`".
#' @param min_date,quantile_levels,label As in [engine()].
#'
#' @return A `nowcast_engine`.
#'
#' @keywords internal
#' @noRd
.new_engine <- function(method, args, min_date, quantile_levels, label) {
  # An argument the user did not set must not reach the backend at all: passing
  # `max_D = NULL` is not the same as not passing `max_D`, and several of these
  # packages treat an explicit NULL as an error rather than as a default.
  args <- args[!vapply(args, is.null, logical(1))]

  unnamed <- if (is.null(names(args))) length(args) > 0 else any(!nzchar(names(args)))
  if (unnamed) {
    cli::cli_abort(c(
      "Every argument of an engine must be named.",
      "i" = "They are passed to the modelling function by name, so a positional \\
             one has nowhere to go."
    ))
  }
  if (anyDuplicated(names(args))) {
    duplicated_args <- unique(names(args)[duplicated(names(args))])
    cli::cli_abort("Argument{?s} {.val {duplicated_args}} {?is/are} given twice.")
  }

  quantile_levels <- sort(unique(as.numeric(quantile_levels)))
  if (length(quantile_levels) == 0 || any(quantile_levels <= 0 | quantile_levels >= 1)) {
    cli::cli_abort("{.arg quantile_levels} must be probabilities strictly between 0 and 1.")
  }

  .check_min_date(min_date)

  if (!is.null(label) && (!is.character(label) || length(label) != 1 || is.na(label))) {
    cli::cli_abort("{.arg label} must be a single, non-missing string.")
  }

  structure(
    list(
      name = method,
      args = args,
      min_date = min_date,
      quantile_levels = quantile_levels,
      label = label %||% method
    ),
    class = c(method, "nowcast_engine")
  )
}

#' Validate the `min_date` argument
#'
#' @param min_date The user's `min_date`.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
.check_min_date <- function(min_date) {
  if (is.null(min_date)) {
    return(invisible(NULL))
  }
  if (length(min_date) != 1 || is.na(min_date)) {
    cli::cli_abort("{.arg min_date} must be a single, non-missing value.")
  }
  if (lubridate::is.Date(min_date)) {
    return(invisible(NULL))
  }
  if (!is.numeric(min_date) || min_date <= 0) {
    cli::cli_abort(c(
      "{.arg min_date} must be a {.cls Date} or a positive number.",
      "i" = "A {.cls Date} is a fixed cut; a number is how many periods before \\
             {.fn get_now} to keep."
    ))
  }
  invisible(NULL)
}

#' Trim a `tbl_now` to an engine's `min_date`
#'
#' @param x A `tbl_now`.
#' @param min_date A `Date`, a positive number of periods, or `NULL`.
#' @param verbose Whether to report what was dropped.
#'
#' @return `x`, filtered on the event date. `now` is untouched: trimming says
#'   how much history to fit on, not when the nowcast is made.
#'
#' @keywords internal
#' @noRd
.engine_trim <- function(x, min_date, verbose = FALSE) {
  if (is.null(min_date)) {
    return(x)
  }
  event_col <- get_event_date(x)
  dates <- x[[event_col]]

  keep <- if (lubridate::is.Date(min_date)) {
    dates >= min_date
  } else {
    # Counted in the object's own units rather than in days, so `min_date = 26`
    # on a weekly series is twenty-six weeks and not twenty-six days. Measured
    # from `now`, so every snapshot of a backtest gets a window of the same length
    # rather than one that grows as `now` walks forward.
    .tbl_now_units_between(dates, get_now(x), get_event_units(x)) < min_date
  }
  keep <- keep & !is.na(keep)

  if (!any(keep)) {
    cli::cli_abort(c(
      "{.arg min_date} leaves no data.",
      "i" = if (lubridate::is.Date(min_date)) {
        "The latest event date is {.val {as.character(max(dates, na.rm = TRUE))}}."
      } else {
        "The series is shorter than {.val {min_date}} {get_event_units(x)}."
      }
    ))
  }

  if (isTRUE(verbose) && !all(keep)) {
    cli::cli_alert_info(
      "Trimming to {sum(keep)} of {length(keep)} row{?s} \\
       (event dates from {.val {as.character(min(dates[keep], na.rm = TRUE))}})."
    )
  }
  dplyr::filter(x, keep)
}

#' @exportS3Method base::print
print.nowcast_engine <- function(x, ...) {
  # `cat_*()` rather than `cli_*()`: the latter writes to the MESSAGE stream, so
  # a print method built on it vanishes under `message = FALSE`, `sink()` or
  # `capture.output()`.
  cli::cat_rule(left = cli::format_inline("<nowcast_engine: {.val {x$name}}>"))

  bullets <- character(0)
  if (!identical(x$label, x$name)) {
    bullets <- c(bullets, cli::format_inline("label: {.val {x$label}}"))
  }
  if (!is.null(x$min_date)) {
    # NB: paste0(), not cli's `\\` line continuation -- `format_inline()` does
    # not strip those, so the backslash would be printed literally.
    bullets <- c(bullets, cli::format_inline(paste0(
      "min_date: {.val {as.character(x$min_date)}}",
      "{if (lubridate::is.Date(x$min_date)) '' else ' periods before now'}"
    )))
  }
  bullets <- c(bullets, cli::format_inline(
    "quantile levels: {.val {x$quantile_levels}}"
  ))
  if (length(x$args) > 0) {
    bullets <- c(bullets, cli::format_inline(
      "arguments: {.field {names(x$args)}}"
    ))
  }
  cli::cat_bullet(bullets)
  invisible(x)
}

#' Is this an engine?
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' Tests whether an object is a nowcasting engine -- the specification built by
#' [engine()] that says which modelling package to use and how to configure it.
#' A bare package name is *not* an engine, which is what this is usually used to
#' check.
#'
#' @param x An object.
#'
#' @return A single `TRUE` or `FALSE`.
#'
#' @seealso
#' [engine()] to build one; [nowcast_engines] for the engines that ship with the
#' package; [run_nowcast()], which takes one.
#'
#' @examples
#' ## Built by engine(): yes.
#' is_nowcast_engine(engine("baselinenowcast"))
#'
#' # The name of a package on its own: no.
#' is_nowcast_engine("baselinenowcast")
#'
#' @export
is_nowcast_engine <- function(x) {
  inherits(x, "nowcast_engine")
}

#' Reject anything that is not an engine, and say what to write instead
#'
#' @param x The object given.
#' @param arg Argument name, for the message.
#'
#' @return `x`, invisibly.
#'
#' @keywords internal
#' @noRd
.assert_engine <- function(x, arg = "engine") {
  if (is_nowcast_engine(x)) {
    return(invisible(x))
  }
  # A bare method name is the OLD spelling, and it is worth naming explicitly:
  # `run_nowcast(x, "NobBS", max_D = 10)` used to work, and the arguments it
  # carried have nowhere to go now, so a generic "wrong class" message would
  # leave the reader to guess where `max_D` went.
  if (is.character(x) && length(x) == 1) {
    # Built OUTSIDE the `cli_abort()` call: cli's glue transformer refuses an
    # expression beginning with a dot ("Invalid cli literal: `{.engine...}`
    # starts with a dot"), because that is how its own inline classes are
    # spelled, so a `.`-prefixed helper cannot be called from inside a string.
    constructor <- paste0(.engine_constructor_name(x), "()")
    cli::cli_abort(c(
      "{.arg {arg}} must be a {.cls nowcast_engine}, not a method name.",
      "i" = "Write {.code engine({.str {x}})}, or the engine for that package: \\
             {.code {constructor}}.",
      "i" = "Its arguments go INSIDE the engine, e.g. \\
             {.code engine_nobbs(max_D = 10)}."
    ))
  }
  cli::cli_abort(
    "{.arg {arg}} must be a {.cls nowcast_engine} (see {.fn engine}), not \\
     {.obj_type_friendly {x}}."
  )
}

#' The `engine_*()` constructor for a method name
#'
#' @param method A method name.
#'
#' @return A single string.
#'
#' @keywords internal
#' @noRd
.engine_constructor_name <- function(method) {
  known <- c(
    diseasenowcasting = "engine_diseasenowcasting",
    baselinenowcast   = "engine_baselinenowcast",
    epinowcast        = "engine_epinowcast",
    surveillance      = "engine_surveillance",
    EpiNow2           = "engine_epinow2",
    NobBS             = "engine_nobbs"
  )
  hit <- known[.canonical_nowcast_method(method)]
  if (is.na(hit)) "engine" else unname(hit)
}


# The package-specific engines ------------------------------------------------
#
# Each names the arguments of the function it drives. The list is deliberately
# NOT exhaustive: it is the arguments worth knowing about, and `...` carries the
# rest, so an upstream package adding one does not break anything here.

#' Engines for the built-in nowcasting packages
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' One constructor per supported modelling package. Each is [engine()] with the
#' arguments of that package's own entry point spelled out, so the ones that
#' matter are visible in the signature and a typo is an error rather than a
#' silently ignored extra.
#'
#' The [*One dataset, many nowcasts* article](https://rodrigozepeda.github.io/tbl.now/articles/nowcasting-models.html)
#' documents each package's own API; this page
#' is about driving it through [run_nowcast()].
#'
#' @inheritParams engine
#'
#' @param model,type,n_draws (`engine_diseasenowcasting()`) Arguments of
#'   [diseasenowcasting::nowcast()]. `model` is where the epidemic and
#'   validation processes are chosen, e.g.
#'   `diseasenowcasting::model(epidemic = diseasenowcasting::ar1_epidemic())`.
#'   On `count-cumulative` data that revises downwards you also want a
#'   `validation` process, or the negative increments have nowhere to go.
#'
#' @param draws,delays_unit,max_delay (`engine_baselinenowcast()`) Number of
#'   nowcast samples, the unit of the reporting triangle's delay axis (inferred
#'   from the object's units when `NULL`), and how many delay periods to keep --
#'   `max_delay = 10` keeps delays 0-9, as in [tbl_now_to_baselinenowcast()].
#'   The last one is not only about speed: `baselinenowcast` needs more
#'   reference dates than delay columns, so a **snapshot ("as of") series** --
#'   which re-reports every past period in every snapshot, and therefore has a
#'   delay axis as long as the series itself -- cannot be fitted at all until
#'   the axis is capped. The error says which number to use.
#'
#' @param preprocess_args,expectation,reference,report,fit
#'   (`engine_epinowcast()`) `preprocess_args` is a list for
#'   [tbl_now_to_epinowcast()], e.g. `list(max_delay = 30)`; the other four are
#'   `epinowcast::epinowcast()`'s module arguments. **`epinowcast` is unseeded
#'   unless you say so**: `enw_fit_opts()` passes `...` to the sampler, so
#'   `fit = epinowcast::enw_fit_opts(seed = 1)` is what makes a fit reproducible.
#'
#' @param max_D,moving_window,specs (`engine_nobbs()`) Arguments of
#'   `NobBS::NobBS()` / `NobBS::NobBS.strat()`. `moving_window` counts **event
#'   periods and must not exceed the history you hand it** -- ask for more and
#'   NobBS pads its grid backwards and returns zero for every date, with no
#'   error. `specs$quantiles` is filled from `quantile_levels` unless you set it.
#'
#' @param D,when,fit_method,control (`engine_surveillance()`) Arguments of
#'   [surveillance::nowcast()]. `fit_method` is that function's own `method`
#'   argument, renamed so it cannot collide with the engine's method. `when`
#'   defaults to `get_surveillance_when(x, length = D + 1)` and `control$dRange`
#'   to [get_surveillance_range()] -- both built from the whole object, so every
#'   stratum is fitted on the same time axis.
#'
#' @param generation_time,delays,truncation,rt,stan,convert_args
#'   (`engine_epinow2()`) Arguments of `EpiNow2::estimate_infections()` /
#'   `EpiNow2::regional_epinow()`, plus `convert_args` for
#'   [tbl_now_to_EpiNow2()]. **Read this before trusting the output**:
#'   EpiNow2 defaults to `delays = delay_opts()`, which is `Fixed(0)` -- no
#'   reporting delay at all -- and a one-day generation time. Those defaults
#'   describe a process with nothing to nowcast, so supply the epidemiology
#'   yourself.
#'
#' @return A `nowcast_engine`, as [engine()] returns.
#'
#' @seealso [engine()], [run_nowcast()], [nowcast_backtest()]
#'
#' @examples
#' engine_baselinenowcast(draws = 500)
#' engine_nobbs(max_D = 10, moving_window = 64)
#'
#' # Fit epinowcast on the last 180 periods only; it scales with the number of
#' # reference dates, while the two engines below take the whole series.
#' engine_epinowcast(preprocess_args = list(max_delay = 30), min_date = 180)
#' engine_baselinenowcast()
#' engine_diseasenowcasting()
#'
#' @name nowcast_engines
NULL

#' @rdname nowcast_engines
#' @export
engine_diseasenowcasting <- function(..., model = NULL, type = NULL,
                                     n_draws = NULL, min_date = NULL,
                                     quantile_levels = nowcast_quantile_levels(),
                                     label = NULL) {
  .new_engine(
    "diseasenowcasting",
    c(list(model = model, type = type, n_draws = n_draws), list(...)),
    min_date, quantile_levels, label
  )
}

#' @rdname nowcast_engines
#' @export
engine_baselinenowcast <- function(..., draws = 1000, delays_unit = NULL,
                                   max_delay = NULL, min_date = NULL,
                                   quantile_levels = nowcast_quantile_levels(),
                                   label = NULL) {
  .new_engine(
    "baselinenowcast",
    c(
      list(draws = draws, delays_unit = delays_unit, max_delay = max_delay),
      list(...)
    ),
    min_date, quantile_levels, label
  )
}

#' @rdname nowcast_engines
#' @export
engine_epinowcast <- function(..., preprocess_args = list(), expectation = NULL,
                              reference = NULL, report = NULL, fit = NULL,
                              min_date = NULL,
                              quantile_levels = nowcast_quantile_levels(),
                              label = NULL) {
  .new_engine(
    "epinowcast",
    c(
      list(
        preprocess_args = preprocess_args, expectation = expectation,
        reference = reference, report = report, fit = fit
      ),
      list(...)
    ),
    min_date, quantile_levels, label
  )
}

#' @rdname nowcast_engines
#' @export
engine_nobbs <- function(..., max_D = NULL, moving_window = NULL, # nolint: object_name_linter.
                         specs = NULL, min_date = NULL,
                         quantile_levels = nowcast_quantile_levels(),
                         label = NULL) {
  .new_engine(
    "NobBS",
    c(
      list(max_D = max_D, moving_window = moving_window, specs = specs),
      list(...)
    ),
    min_date, quantile_levels, label
  )
}

#' @rdname nowcast_engines
#' @export
engine_surveillance <- function(..., D = NULL, when = NULL, # nolint: object_name_linter.
                                fit_method = NULL, control = NULL,
                                min_date = NULL,
                                quantile_levels = nowcast_quantile_levels(),
                                label = NULL) {
  .new_engine(
    "surveillance",
    c(
      list(D = D, when = when, fit_method = fit_method, control = control),
      list(...)
    ),
    min_date, quantile_levels, label
  )
}

#' @rdname nowcast_engines
#' @export
engine_epinow2 <- function(..., generation_time = NULL, delays = NULL,
                           truncation = NULL, rt = NULL, stan = NULL,
                           convert_args = list(), min_date = NULL,
                           quantile_levels = nowcast_quantile_levels(),
                           label = NULL) {
  .new_engine(
    "EpiNow2",
    c(
      list(
        generation_time = generation_time, delays = delays,
        truncation = truncation, rt = rt, stan = stan,
        convert_args = convert_args
      ),
      list(...)
    ),
    min_date, quantile_levels, label
  )
}
