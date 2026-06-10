# Converters between `tbl_now` and the data structures of other
# nowcasting / epidemiological packages.
#
# Naming convention:
#   tbl_now_from_<pkg>()  package data  -> tbl_now   (wraps as_tbl_now())
#   tbl_now_to_<pkg>()    tbl_now       -> package object (calls into the pkg)
#
# This file is organised in three blocks:
#   1. internal helpers
#   2. all tbl_now_from_*() converters
#   3. all tbl_now_to_*() converters
#   4. as_tbl_now() methods for the classes produced by tbl_now_to_*()
#
# All `tbl_now_from_*()` accept `...` which is forwarded to `as_tbl_now()`
# (and therefore to `tbl_now()`).  All functions accept `verbose` which prints
# the choices that were made (the inferred `now`, the data type, units, etc.).

# ===========================================================================
# 1. Internal helpers
# ===========================================================================

# Wrap as_tbl_now(), injecting verbose = FALSE unless the caller passed it in
# `...` (we do our own reporting in the from_* functions).
.build_tbl_now <- function(df, dots, ...) {
  # Drop the reserved generated columns if they were carried over (e.g. when a
  # tbl_now was exported via tbl_now_to_*() and is being converted back). They
  # are recomputed by tbl_now().
  generated <- c(".event_num", ".report_num", ".delay")
  if (any(generated %in% names(df))) {
    df <- df[, setdiff(names(df), generated), drop = FALSE]
  }
  fixed <- list(...)
  args  <- c(list(object = df), fixed, dots)
  if (is.null(args$verbose)) args$verbose <- FALSE
  do.call(as_tbl_now, args)
}

# Pretty-print the resulting tbl_now's chosen attributes.
.report_from <- function(result, source, verbose, extra = NULL) {
  if (!isTRUE(verbose)) return(invisible(result))
  cli::cli_h3("Converted {.pkg {source}} {.cls data} into a {.cls tbl_now}")
  cli::cli_ul()
  cli::cli_li("event_date: {.val {get_event_date(result)}}")
  cli::cli_li("report_date: {.val {get_report_date(result)}}")
  cli::cli_li("data_type: {.val {get_data_type(result)}}")
  cli::cli_li("now: {.val {as.character(get_now(result))}}")
  cli::cli_li("units: event = {.val {get_event_units(result)}}, report = {.val {get_report_units(result)}}")
  if (!is.null(get_strata(result)))     cli::cli_li("strata: {.val {get_strata(result)}}")
  if (!is.null(get_covariates(result))) cli::cli_li("covariates: {.val {get_covariates(result)}}")
  if (!is.null(get_case_count(result))) cli::cli_li("case_count: {.val {get_case_count(result)}}")
  for (e in extra) cli::cli_li(e)
  cli::cli_end()
  invisible(result)
}

# Abort if a Suggested package is not installed.
.need_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg {pkg}} is required for this conversion. Install it with {.run install.packages(\"{pkg}\")}."
    )
  }
}

# Guard that `x` is a tbl_now for the to_* direction.
.assert_tbl_now <- function(x, fn) {
  if (!is_tbl_now(x)) {
    cli::cli_abort("{.arg x} must be a {.cls tbl_now} object for {.fn {fn}}.")
  }
}

# Expand a reporting-triangle matrix into a long incremental data.frame
.reporting_triangle_to_long <- function(m, delays_unit = "days") {
  ref_chr <- rownames(m)
  if (is.null(ref_chr)) {
    cli::cli_abort("Reporting-triangle matrix must have reference dates as row names.")
  }
  ref_dates <- as.Date(ref_chr)
  delays    <- suppressWarnings(as.integer(colnames(m)))
  if (any(is.na(delays))) delays <- seq_len(ncol(m)) - 1L

  step <- switch(delays_unit,
    days = 1, weeks = 7, months = 30, years = 365, 1)

  rows <- list()
  for (i in seq_len(nrow(m))) {
    for (j in seq_len(ncol(m))) {
      val <- m[i, j]
      if (is.na(val)) next
      rows[[length(rows) + 1L]] <- data.frame(
        reference_date = ref_dates[i],
        report_date    = ref_dates[i] + delays[j] * step,
        count          = as.numeric(val)
      )
    }
  }
  do.call(rbind, rows)
}

# ===========================================================================
# 2. tbl_now_from_*()  (package data -> tbl_now)
# ===========================================================================

#' Convert between `tbl_now` and \pkg{epinowcast}
#'
#' `r lifecycle::badge("experimental")`
#'
#' @description
#' `tbl_now_from_epinowcast()` takes the long observation `data.frame` used by
#' \pkg{epinowcast} (with `reference_date`, `report_date` and a cumulative
#' `confirm` column, plus optional grouping columns) and converts it into a
#' `tbl_now` of `data_type = "count-cumulative"`.
#'
#' `tbl_now_to_epinowcast()` takes a `tbl_now` and builds an
#' [epinowcast::enw_preprocess_data()] object (or, with `preprocess = FALSE`,
#' the completed observation `data.table`).
#'
#' @param data A `data.frame`/`data.table` in \pkg{epinowcast} long format.
#' @param x A `tbl_now` object.
#' @param reference_date,report_date,confirm Column names in `data`.
#' @param strata Optional character vector of grouping columns. If `NULL`
#'   (default) any column other than `reference_date`, `report_date` and
#'   `confirm` is treated as a stratifying group.
#' @param max_delay Maximum delay (in `timestep`s) to use when preprocessing.
#'   If `NULL` it is inferred from the data as `max(.delay) + 1`.
#' @param preprocess If `TRUE` (default) returns an `enw_preprocess_data`
#'   object; if `FALSE` returns the completed observation `data.table`.
#' @param verbose Logical. Print the choices that were made.
#' @param ... Additional arguments forwarded to [as_tbl_now()] (for `from`)
#'   or to [epinowcast::enw_preprocess_data()] (for `to`).
#'
#' @return `tbl_now_from_epinowcast()` returns a `tbl_now`.
#'   `tbl_now_to_epinowcast()` returns an `enw_preprocess_data` object or a
#'   `data.table`.
#'
#' @examplesIf requireNamespace("epinowcast", quietly = TRUE)
#' obs <- epinowcast::germany_covid19_hosp
#' nowobj <- tbl_now_from_epinowcast(obs, strata = c("location", "age_group"))
#' @name tbl_now_epinowcast
#' @export
tbl_now_from_epinowcast <- function(data, ...,
                                    reference_date = "reference_date",
                                    report_date    = "report_date",
                                    confirm        = "confirm",
                                    strata         = NULL,
                                    verbose        = TRUE) {

  data <- as.data.frame(data)
  needed <- c(reference_date, report_date, confirm)
  if (!all(needed %in% colnames(data))) {
    miss <- needed[!(needed %in% colnames(data))]
    cli::cli_abort("Column{?s} {.val {miss}} not found in {.arg data}.")
  }

  # Auto-detect strata as the remaining columns if not provided
  if (is.null(strata)) {
    strata <- setdiff(colnames(data), needed)
    if (length(strata) == 0) strata <- NULL
  }

  result <- .build_tbl_now(
    data, dots = list(...),
    event_date  = reference_date,
    report_date = report_date,
    case_count  = confirm,
    strata      = strata,
    data_type   = "count-cumulative"
  )

  .report_from(result, "epinowcast", verbose)
  result
}

#' Convert between `tbl_now` and \pkg{baselinenowcast}
#'
#' @description
#' `tbl_now_from_baselinenowcast()` accepts either the long `data.frame`
#' (`reference_date`, `report_date`, `count`) or a `reporting_triangle`
#' matrix (rownames = reference dates, colnames = delays, incremental counts)
#' and converts it into a `tbl_now` of `data_type = "count-incidence"`.
#'
#' `tbl_now_to_baselinenowcast()` returns either the long
#' `baselinenowcast`-style `data.frame` (`format = "long"`, default) or a
#' `reporting_triangle` matrix (`format = "matrix"`) via
#' [baselinenowcast::as_reporting_triangle()].
#'
#' @param data A long `data.frame` or a `reporting_triangle` matrix.
#' @param x A `tbl_now` object.
#' @param reference_date,report_date,count Column names (long format only).
#' @param delays_unit Unit of the delay axis (passed to
#'   [baselinenowcast::as_reporting_triangle()]). Defaults to `"days"`.
#' @param format For `to`: `"long"` (default) or `"matrix"`.
#' @param verbose Logical. Print the choices that were made.
#' @param ... Forwarded to [as_tbl_now()] (`from`) or
#'   [baselinenowcast::as_reporting_triangle()] (`to`, matrix format).
#'
#' @return A `tbl_now` (`from`), or a `data.frame`/`reporting_triangle` (`to`).
#'
#' @examplesIf requireNamespace("baselinenowcast", quietly = TRUE)
#' rt <- baselinenowcast::example_reporting_triangle
#' nowobj <- tbl_now_from_baselinenowcast(rt)
#' @name tbl_now_baselinenowcast
#' @export
tbl_now_from_baselinenowcast <- function(data, ...,
                                         reference_date = "reference_date",
                                         report_date    = "report_date",
                                         count          = "count",
                                         delays_unit    = "days",
                                         verbose        = TRUE) {

  # Matrix / reporting_triangle: expand to long incremental form
  if (is.matrix(data) || inherits(data, "reporting_triangle")) {
    long  <- .reporting_triangle_to_long(data, delays_unit = delays_unit)
    df    <- long
    extra <- "expanded a reporting-triangle matrix to long incremental counts"
  } else {
    df <- as.data.frame(data)
    if (!all(c(reference_date, report_date, count) %in% colnames(df))) {
      miss <- c(reference_date, report_date, count)
      miss <- miss[!(miss %in% colnames(df))]
      cli::cli_abort("Column{?s} {.val {miss}} not found in {.arg data}.")
    }
    df <- df[, c(reference_date, report_date, count), drop = FALSE]
    names(df) <- c("reference_date", "report_date", "count")
    extra <- NULL
  }

  result <- .build_tbl_now(
    df, dots = list(...),
    event_date  = "reference_date",
    report_date = "report_date",
    case_count  = "count",
    data_type   = "count-incidence"
  )

  .report_from(result, "baselinenowcast", verbose, extra = extra)
  result
}

#' Convert between `tbl_now` and \pkg{data.table}
#'
#' @description
#' `tbl_now_from_data_table()` converts a `data.table` into a `tbl_now`
#' (requires explicit `event_date` / `report_date` columns).
#' `tbl_now_to_data_table()` strips the `tbl_now` class and returns a
#' `data.table`.
#'
#' @param data A `data.table`.
#' @param x A `tbl_now` object.
#' @param event_date,report_date Column names (passed to [as_tbl_now()]).
#' @param verbose Logical. Print the choices that were made.
#' @param ... Forwarded to [as_tbl_now()] (`from`) or
#'   [data.table::as.data.table()] (`to`).
#'
#' @return A `tbl_now` (`from`) or a `data.table` (`to`).
#'
#' @examplesIf requireNamespace("data.table", quietly = TRUE)
#' data(denguedat)
#' dt <- data.table::as.data.table(denguedat)
#' nowobj <- tbl_now_from_data_table(dt, event_date = "onset_week",
#'                                   report_date = "report_week", verbose = FALSE)
#' @name tbl_now_data_table
#' @export
tbl_now_from_data_table <- function(data, event_date, report_date, ...,
                                    verbose = TRUE) {

  if (!inherits(data, "data.table")) {
    cli::cli_warn("{.arg data} is not a {.cls data.table}; coercing with {.fn as.data.frame}.")
  }
  df <- as.data.frame(data)

  result <- .build_tbl_now(
    df, dots = list(...),
    event_date  = event_date,
    report_date = report_date
  )

  .report_from(result, "data.table", verbose)
  result
}

#' Convert between `tbl_now` and \pkg{epidist}
#'
#' @description
#' \pkg{epidist} models the delay between a *primary* event (e.g. symptom
#' onset) and a *secondary* event (e.g. report), storing each as an
#' interval-censored pair of date columns: `pdate_lwr`/`pdate_upr` for the
#' primary event and `sdate_lwr`/`sdate_upr` for the secondary event
#' (see [epidist::as_epidist_linelist_data()]).
#'
#' `tbl_now_from_epidist()` converts such data into a `tbl_now`:
#'
#' * `"linelist"` (default): use the lower bounds only — `primary`
#'   (`pdate_lwr`) becomes `event_date` and `secondary` (`sdate_lwr`) becomes
#'   `report_date`. `data_type = "linelist"`.
#' * `"interval"`: additionally attach the upper bounds `primary_upper`
#'   (`pdate_upr`) and `secondary_upper` (`sdate_upr`) as `covariates`
#'   (a warning is emitted).
#'
#' `tbl_now_to_epidist()` performs the inverse and builds an
#' `epidist_linelist_data` object via [epidist::as_epidist_linelist_data()].
#' For `format = "interval"` the upper bounds are taken from covariate columns
#' named in `primary_upper` / `secondary_upper`.
#'
#' @param data A `data.frame` (or `epidist_linelist_data`) of \pkg{epidist}
#'   delay data.
#' @param x A `tbl_now` object.
#' @param format `"linelist"` (default) or `"interval"`.
#' @param primary,secondary Column names of the primary / secondary event
#'   lower-bound dates. Default to epidist's `"pdate_lwr"` / `"sdate_lwr"`.
#' @param primary_upper,secondary_upper Column names of the upper-bound dates
#'   (`format = "interval"`). Default to epidist's `"pdate_upr"` / `"sdate_upr"`.
#' @param verbose Logical. Print the choices that were made.
#' @param ... Forwarded to [as_tbl_now()] (`from`) or
#'   [epidist::as_epidist_linelist_data()] (`to`).
#'
#' @return A `tbl_now` (`from`) or an `epidist_linelist_data` object (`to`).
#'
#' @examplesIf requireNamespace("epidist", quietly = TRUE)
#' df <- data.frame(
#'   pdate_lwr = as.Date(c("2020-03-01", "2020-03-02")),
#'   sdate_lwr = as.Date(c("2020-03-05", "2020-03-04"))
#' )
#' tbl_now_from_epidist(df, event_units = "days", report_units = "days")
#' @name tbl_now_epidist
#' @export
tbl_now_from_epidist <- function(data, ..., format = c("linelist", "interval"),
                                 primary         = "pdate_lwr",
                                 secondary       = "sdate_lwr",
                                 primary_upper   = "pdate_upr",
                                 secondary_upper = "sdate_upr",
                                 verbose         = TRUE) {

  format <- match.arg(format)
  df     <- as.data.frame(data)

  if (format == "linelist") {
    need <- c(primary, secondary)
    if (!all(need %in% colnames(df))) {
      miss <- need[!(need %in% colnames(df))]
      cli::cli_abort("Column{?s} {.val {miss}} not found in {.arg data}.")
    }
    result <- .build_tbl_now(
      df, dots = list(...),
      event_date  = primary,
      report_date = secondary,
      data_type   = "linelist"
    )
    .report_from(result, "epidist", verbose,
                 extra = "format: linelist (primary lower bound -> event_date, secondary lower bound -> report_date)")
    return(result)
  }

  # interval-censored: lower bounds -> dates, upper bounds -> covariates
  need <- c(primary, secondary, primary_upper, secondary_upper)
  if (!all(need %in% colnames(df))) {
    miss <- need[!(need %in% colnames(df))]
    cli::cli_abort("Column{?s} {.val {miss}} not found in {.arg data}.")
  }
  cli::cli_warn(
    paste0(
      "Interval-censored data: using the lower bounds {.val {c(primary, secondary)}} as ",
      "event/report dates and attaching the upper bounds {.val {c(primary_upper, secondary_upper)}} ",
      "as {.field covariates}."
    )
  )
  result <- .build_tbl_now(
    df, dots = list(...),
    event_date  = primary,
    report_date = secondary,
    covariates  = c(primary_upper, secondary_upper),
    data_type   = "linelist"
  )
  .report_from(result, "epidist", verbose,
               extra = "format: interval (lower bounds -> dates, upper bounds -> covariates)")
  result
}

#' Convert between `tbl_now` and \pkg{tsibble}
#'
#' @description
#' A [tsibble::tsibble()] has a single time `index` and a `key` identifying each
#' series. Nowcasting needs two time indices, so the conversion keeps both date
#' columns: the `index` is one of the dates and the other date (plus any strata)
#' becomes part of the `key`.
#'
#' `tbl_now_from_tsibble()` converts a `tbl_ts` into a `tbl_now`. You must say
#' which column is the `event_date`; `report_date` defaults to the tsibble's
#' index ([tsibble::index_var()]).
#'
#' `tbl_now_to_tsibble()` converts a `tbl_now` into a `tbl_ts`, using `index`
#' (`"report_date"`, the default, or `"event_date"`) as the tsibble index and
#' the other date plus the strata as the key. Linelist data is aggregated to
#' `count-incidence` first (a tsibble requires unique index/key combinations).
#'
#' @param data A `tbl_ts` (tsibble).
#' @param x A `tbl_now` object.
#' @param event_date Column name of the event date (required for `from`).
#' @param report_date Column name of the report date (for `from`); defaults to
#'   the tsibble index.
#' @param strata Optional character vector of strata columns (`from`). If `NULL`
#'   (default) the tsibble key columns other than the date columns are used.
#' @param index For `to`: which date becomes the tsibble index, `"report_date"`
#'   (default) or `"event_date"`.
#' @param verbose Logical. Print the choices that were made.
#' @param ... Forwarded to [as_tbl_now()] (`from`) or [tsibble::as_tsibble()]
#'   (`to`).
#'
#' @return A `tbl_now` (`from`) or a `tbl_ts` (`to`).
#'
#' @examplesIf requireNamespace("tsibble", quietly = TRUE)
#' data(denguedat)
#' nowobj <- tbl_now(denguedat, event_date = "onset_week",
#'                   report_date = "report_week", verbose = FALSE)
#' ts <- tbl_now_to_tsibble(nowobj, verbose = FALSE)
#' back <- tbl_now_from_tsibble(ts, event_date = "onset_week", verbose = FALSE)
#' @name tbl_now_tsibble
#' @export
tbl_now_from_tsibble <- function(data, event_date, report_date = NULL,
                                 strata = NULL, ..., verbose = TRUE) {

  .need_pkg("tsibble")
  if (missing(event_date) || is.null(event_date)) {
    cli::cli_abort("Please supply the {.arg event_date} column name.")
  }
  if (is.null(report_date)) {
    if (!inherits(data, "tbl_ts")) {
      cli::cli_abort("{.arg report_date} is required when {.arg data} is not a {.cls tbl_ts}.")
    }
    report_date <- tsibble::index_var(data)
  }

  # Recover strata from the tsibble key (the key vars other than the dates).
  if (is.null(strata) && inherits(data, "tbl_ts")) {
    strata <- setdiff(tsibble::key_vars(data), c(event_date, report_date))
    if (length(strata) == 0) strata <- NULL
  }

  df <- as.data.frame(data)

  result <- .build_tbl_now(
    df, dots = list(...),
    event_date  = event_date,
    report_date = report_date,
    strata      = strata
  )

  .report_from(result, "tsibble", verbose,
               extra = paste0("report_date taken from the tsibble index: ", report_date))
  result
}

# ===========================================================================
# 3. tbl_now_to_*()  (tbl_now -> package object)
# ===========================================================================

#' @rdname tbl_now_epinowcast
#' @export
tbl_now_to_epinowcast <- function(x, ..., max_delay = NULL,
                                  preprocess = TRUE, verbose = TRUE) {

  .assert_tbl_now(x, "tbl_now_to_epinowcast")
  .need_pkg("epinowcast")

  if (requireNamespace("epinowcast", quietly = TRUE)) {

    if (get_data_type(x) != "count-cumulative") {
      cli::cli_warn(
        "epinowcast expects cumulative counts; {.arg x} has data_type {.val {get_data_type(x)}}. Converting with {.fn to_count}."
      )
      x <- to_count(x, to = "count-cumulative")
    }

    ev <- get_event_date(x)
    rp <- get_report_date(x)
    cc <- get_case_count(x)
    st <- get_strata(x)

    # Build the long obs data.frame epinowcast expects
    obs <- as.data.frame(x)
    obs <- obs[, c(ev, rp, cc, st), drop = FALSE]
    names(obs)[1:3] <- c("reference_date", "report_date", "confirm")
    obs <- data.table::as.data.table(obs)

    if (is.null(max_delay)) {
      max_delay <- as.integer(max(x[[".delay"]], na.rm = TRUE)) + 1L
    }
    by <- if (length(st) > 0) st else NULL

    if (verbose) {
      cli::cli_h3("Converting {.cls tbl_now} into an {.pkg epinowcast} object")
      cli::cli_ul()
      cli::cli_li("reference_date <- {.val {ev}}")
      cli::cli_li("report_date <- {.val {rp}}")
      cli::cli_li("confirm <- {.val {cc}}")
      cli::cli_li("by: {.val {if (is.null(by)) 'none' else by}}")
      cli::cli_li("max_delay: {.val {max_delay}}")
      cli::cli_li("preprocess: {.val {preprocess}}")
      cli::cli_end()
    }

    completed <- epinowcast::enw_complete_dates(obs, by = by, max_delay = max_delay)
    if (!preprocess) return(completed)

    epinowcast::enw_preprocess_data(completed, by = by, max_delay = max_delay, ...)
  } else {
    NULL
  }
}

#' @rdname tbl_now_baselinenowcast
#' @export
tbl_now_to_baselinenowcast <- function(x, ..., format = c("long", "matrix"),
                                       delays_unit = "days", verbose = TRUE) {


  .assert_tbl_now(x, "tbl_now_to_baselinenowcast")
  format <- match.arg(format)

  if (requireNamespace("baselinenowcast", quietly = TRUE)) {

    if (get_data_type(x) != "count-incidence") {
      cli::cli_warn(
        "baselinenowcast expects incremental counts; converting {.arg x} to {.val count-incidence} with {.fn to_count}."
      )
      x <- to_count(x, to = "count-incidence")
    }

    ev <- get_event_date(x)
    rp <- get_report_date(x)
    cc <- get_case_count(x)

    long <- as.data.frame(x)[, c(ev, rp, cc), drop = FALSE]
    names(long) <- c("reference_date", "report_date", "count")

    if (verbose) {
      cli::cli_h3("Converting {.cls tbl_now} into a {.pkg baselinenowcast} {format}")
      cli::cli_ul()
      cli::cli_li("reference_date <- {.val {ev}}")
      cli::cli_li("report_date <- {.val {rp}}")
      cli::cli_li("count <- {.val {cc}}")
      cli::cli_li("format: {.val {format}}")
      cli::cli_end()
    }

    if (format == "long") {
      return(long)
    }

    .need_pkg("baselinenowcast")
    baselinenowcast::as_reporting_triangle(long, delays_unit = delays_unit, ...)
  } else {
    NULL
  }
}

#' Convert a `tbl_now` into \pkg{EpiNow2} input
#'
#' @description
#' \pkg{EpiNow2} works with a single incidence time series (`date`, `confirm`)
#' and therefore has no delay/report dimension. This function collapses a
#' `tbl_now` to that single time series, keyed on the `event_date`, using the
#' most recently reported counts (see [get_latest_reported_cases()]).
#'
#' Because \pkg{EpiNow2} has only one time index, there is intentionally **no**
#' `tbl_now_from_EpiNow2()`.
#'
#' @param x A `tbl_now` object.
#' @param verbose Logical. Print the choices that were made.
#' @param ... Forwarded to [data.table::as.data.table()].
#'
#' @return A `data.table` with columns `date` and `confirm`.
#'
#' @examplesIf requireNamespace("data.table", quietly = TRUE)
#' data(mpoxdat)
#' nowobj <- tbl_now(mpoxdat, event_date = "dx_date", report_date = "dx_report_date",
#'                   case_count = "n", data_type = "count-incidence", verbose = FALSE)
#' tbl_now_to_EpiNow2(nowobj, verbose = FALSE)
#' @export
tbl_now_to_EpiNow2 <- function(x, ..., verbose = TRUE) {

  .assert_tbl_now(x, "tbl_now_to_EpiNow2")
  .need_pkg("data.table")

  if (requireNamespace("data.table", quietly = TRUE)) {

    ev <- get_event_date(x)

    if (grepl("count", get_data_type(x))) {
      latest <- get_latest_reported_cases(x)
      cc     <- get_case_count(latest)
      series <- as.data.frame(latest)[, c(ev, cc), drop = FALSE]
      names(series) <- c("date", "confirm")
    } else {
      # linelist: count rows per event_date
      series <- as.data.frame(x) %>%
        dplyr::count(!!as.symbol(ev), name = "confirm")
      names(series)[1] <- "date"
    }

    series <- series %>%
      dplyr::group_by(!!as.symbol("date")) %>%
      dplyr::summarise(!!as.symbol("confirm") := sum(!!as.symbol("confirm"), na.rm = TRUE), .groups = "drop") %>%
      dplyr::arrange(!!as.symbol("date"))

    if (verbose) {
      cli::cli_h3("Converting {.cls tbl_now} into an {.pkg EpiNow2} time series")
      cli::cli_ul()
      cli::cli_li("date <- {.val {ev}} (event_date)")
      cli::cli_li("confirm <- latest reported counts per date")
      cli::cli_li("rows: {.val {nrow(series)}}")
      cli::cli_alert_info("EpiNow2 has a single time index; the delay/report dimension was collapsed.")
      cli::cli_end()
    }

    data.table::as.data.table(series, ...)
  } else {
    NULL
  }
}

#' @rdname tbl_now_data_table
#' @export
tbl_now_to_data_table <- function(x, ..., verbose = TRUE) {

  .assert_tbl_now(x, "tbl_now_to_data_table")
  .need_pkg("data.table")

  if (requireNamespace("baselinenowcast", quietly = TRUE)) {
    if (verbose) {
      cli::cli_h3("Converting {.cls tbl_now} into a {.cls data.table}")
      cli::cli_alert_info("tbl_now attributes are dropped; generated columns (.delay, .event_num, .report_num) are kept.")
    }

    data.table::as.data.table(as.data.frame(x), ...)
  } else {
    NULL
  }
}

#' @rdname tbl_now_epidist
#' @export
tbl_now_to_epidist <- function(x, ..., format = c("linelist", "interval"),
                               primary_upper   = NULL,
                               secondary_upper = NULL,
                               verbose         = TRUE) {



  .assert_tbl_now(x, "tbl_now_to_epidist")
  .need_pkg("epidist")

  if (requireNamespace("epidist", quietly = TRUE)) {
    format <- match.arg(format)

    ev <- get_event_date(x)
    rp <- get_report_date(x)
    df <- as.data.frame(x)

    ctor_args <- list(pdate_lwr = "pdate_lwr", sdate_lwr = "sdate_lwr")

    if (format == "linelist") {
      out <- data.frame(pdate_lwr = df[[ev]], sdate_lwr = df[[rp]])
      extra_cols <- "pdate_lwr <- event_date, sdate_lwr <- report_date"
    } else {
      if (is.null(primary_upper) || is.null(secondary_upper)) {
        cli::cli_abort(
          "For {.val interval} format, supply {.arg primary_upper} and {.arg secondary_upper} (covariate columns holding the upper bounds)."
        )
      }
      if (!all(c(primary_upper, secondary_upper) %in% colnames(df))) {
        miss <- c(primary_upper, secondary_upper)
        miss <- miss[!(miss %in% colnames(df))]
        cli::cli_abort("Upper-bound column{?s} {.val {miss}} not found in {.arg x}.")
      }
      cli::cli_warn(
        "Building interval-censored data: lower bounds from event/report dates, upper bounds from covariates {.val {c(primary_upper, secondary_upper)}}."
      )
      out <- data.frame(
        pdate_lwr = df[[ev]], pdate_upr = df[[primary_upper]],
        sdate_lwr = df[[rp]], sdate_upr = df[[secondary_upper]]
      )
      ctor_args$pdate_upr <- "pdate_upr"
      ctor_args$sdate_upr <- "sdate_upr"
      extra_cols <- "pdate_lwr/sdate_lwr <- dates, pdate_upr/sdate_upr <- covariates"
    }

    if (verbose) {
      cli::cli_h3("Converting {.cls tbl_now} into {.pkg epidist} {format} data")
      cli::cli_ul()
      cli::cli_li(extra_cols)
      cli::cli_end()
    }

    do.call(epidist::as_epidist_linelist_data, c(list(out), ctor_args, list(...)))
  } else {
    NULL
  }
}

#' @rdname tbl_now_tsibble
#' @export
tbl_now_to_tsibble <- function(x, ..., index = c("report_date", "event_date"),
                               verbose = TRUE) {

  .assert_tbl_now(x, "tbl_now_to_tsibble")
  .need_pkg("tsibble")

  if (requireNamespace("tsibble", quietly = TRUE)) {
    index <- match.arg(index)

    # A tsibble needs a unique index/key combination. Linelist rows are not
    # unique per (event, report, strata), so aggregate to count-incidence first.
    if (get_data_type(x) == "linelist") {
      cli::cli_warn(
        "tsibble requires unique index/key rows; aggregating linelist to {.val count-incidence} with {.fn to_count}."
      )
      x <- to_count(x, to = "count-incidence")
    }

    ev <- get_event_date(x)
    rp <- get_report_date(x)
    st <- get_strata(x)

    index_col <- if (index == "report_date") rp else ev
    other_col <- if (index == "report_date") ev else rp
    key_cols  <- c(other_col, st)

    df <- as.data.frame(x)

    if (verbose) {
      cli::cli_h3("Converting {.cls tbl_now} into a {.pkg tsibble}")
      cli::cli_ul()
      cli::cli_li("index <- {.val {index_col}}")
      cli::cli_li("key <- {.val {key_cols}}")
      cli::cli_end()
    }

    tsibble::as_tsibble(
      df,
      index = !!rlang::sym(index_col),
      key   = tidyselect::all_of(key_cols),
      ...
    )
  } else {
    NULL
  }
}

# ===========================================================================
# 4. as_tbl_now() methods for the classes produced by tbl_now_to_*()
#
#    These let a round-trip work: an object created by tbl_now_to_<pkg>() can
#    be converted straight back with as_tbl_now().
# ===========================================================================

#' @rdname as_tbl_now
#' @export
as_tbl_now.enw_preprocess_data <- function(object, event_date, report_date, ...) {
  obs <- as.data.frame(object$obs[[1]])
  obs <- obs[!is.na(obs$reference_date), , drop = FALSE]
  # `by` is stored as a list-column; unwrap it to a plain character vector.
  by  <- unlist(object$by)
  by  <- by[by %in% names(obs)]
  keep <- c("reference_date", "report_date", "confirm", by)
  obs <- obs[, intersect(keep, names(obs)), drop = FALSE]
  # reference/report dates come back as IDate; ensure plain Date
  obs$reference_date <- as.Date(obs$reference_date)
  obs$report_date    <- as.Date(obs$report_date)
  tbl_now_from_epinowcast(obs, strata = if (length(by) > 0) by else NULL, ...)
}

#' @rdname as_tbl_now
#' @export
as_tbl_now.reporting_triangle <- function(object, event_date, report_date, ...) {
  tbl_now_from_baselinenowcast(object, ...)
}

#' @rdname as_tbl_now
#' @export
as_tbl_now.epidist_linelist_data <- function(object, event_date, report_date, ...) {
  tbl_now_from_epidist(object, ...)
}

#' @rdname as_tbl_now
#' @export
as_tbl_now.tbl_ts <- function(object, event_date, report_date, ...) {
  rp <- if (missing(report_date)) NULL else report_date
  if (missing(event_date)) {
    cli::cli_abort("Please supply the {.arg event_date} column name for a {.cls tbl_ts}.")
  }
  tbl_now_from_tsibble(object, event_date = event_date, report_date = rp, ...)
}

#' @rdname as_tbl_now
#' @export
as_tbl_now.data.table <- function(object, event_date, report_date, ...) {
  if (missing(event_date) || missing(report_date)) {
    cli::cli_abort("Please supply {.arg event_date} and {.arg report_date} for a {.cls data.table}.")
  }
  tbl_now_from_data_table(object, event_date = event_date, report_date = report_date, ...)
}
