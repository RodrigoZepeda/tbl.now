# Converters between `tbl_now` and the data structures of other
# nowcasting / epidemiological packages.
#
# Naming convention:
#   tbl_now_from_<pkg>()  package data  -> tbl_now   (wraps as_tbl_now())
#   tbl_now_to_<pkg>()    tbl_now       -> package object (calls into the pkg)
#
# This file is organised in four blocks:
#   1. internal helpers
#   2. all tbl_now_from_*() converters
#   3. all tbl_now_to_*() converters
#   4. S3 methods on the other packages' coercion generics (e.g.
#      `as_tsibble.tbl_now()`), each wrapping the matching tbl_now_to_*()
#
# (The `as_tbl_now()` methods for the classes produced by tbl_now_to_*() live
# next to the `as_tbl_now()` generic in `R/as_tbl_now.R`.)
#
# All `tbl_now_from_*()` accept `...` which is forwarded to `as_tbl_now()`
# (and therefore to `tbl_now()`).  All functions accept `verbose` which prints
# the choices that were made (the inferred `now`, the data type, units, etc.).

# ===========================================================================
# 1. Internal helpers
# ===========================================================================

#' Build a `tbl_now` from a converter's data frame
#'
#' Thin wrapper around [as_tbl_now()] used by every `tbl_now_from_*()`
#' converter. It drops the reserved generated columns (in case the data came
#' from a previously exported `tbl_now`) and forces `verbose = FALSE` unless the
#' caller passes it through `dots`, because the converters print their own
#' summary.
#'
#' @param data A data frame to convert.
#' @param dots A list of extra arguments forwarded by the caller (the
#'   converter's `...`), passed on to [as_tbl_now()].
#' @param ... Fixed arguments set by the converter (e.g. `event_date`,
#'   `data_type`), passed on to [as_tbl_now()].
#'
#' @return A `tbl_now` object.
#'
#' @keywords internal
#' @noRd
.build_tbl_now <- function(data, dots, ...) {
  # Drop the reserved generated columns if they were carried over (e.g. when a
  # tbl_now was exported via tbl_now_to_*() and is converted back); tbl_now()
  # recomputes them.
  generated_columns <- c(".event_num", ".report_num", ".delay")
  data <- data |>
    dplyr::select(-dplyr::any_of(generated_columns))

  # The converters print their own summary, so keep tbl_now() quiet unless the
  # caller explicitly asked for verbosity through `dots`.
  fixed_arguments <- list(...)
  all_arguments <- c(list(object = data), fixed_arguments, dots)
  if (is.null(all_arguments$verbose)) {
    all_arguments$verbose <- FALSE
  }

  do.call(as_tbl_now, all_arguments)
}

#' Print a conversion summary for a `tbl_now_from_*()` converter
#'
#' @param result The resulting `tbl_now`.
#' @param source Name of the source package (for the message).
#' @param verbose Logical; nothing is printed when `FALSE`.
#' @param extra Optional character vector of extra bullet lines to show.
#'
#' @return `result`, invisibly.
#'
#' @keywords internal
#' @noRd
.report_from <- function(result, source, verbose, extra = NULL) {
  if (!isTRUE(verbose)) {
    return(invisible(result))
  }

  cli::cli_h3("Converted {.pkg {source}} {.cls data} into a {.cls tbl_now}")
  cli::cli_ul()
  cli::cli_li("event_date: {.val {get_event_date(result)}}")
  cli::cli_li("report_date: {.val {get_report_date(result)}}")
  cli::cli_li("data_type: {.val {get_data_type(result)}}")
  cli::cli_li("now: {.val {as.character(get_now(result))}}")
  cli::cli_li("event_units: {.val {get_event_units(result)}}")
  cli::cli_li("report_units: {.val {get_report_units(result)}}")

  # Only show the optional attributes that are actually set.
  if (!is.null(get_strata(result))) {
    cli::cli_li("strata: {.val {get_strata(result)}}")
  }
  if (!is.null(get_covariates(result))) {
    cli::cli_li("covariates: {.val {get_covariates(result)}}")
  }
  if (!is.null(get_case_count(result))) {
    cli::cli_li("case_count: {.val {get_case_count(result)}}")
  }
  for (extra_line in extra) {
    cli::cli_li(extra_line)
  }

  cli::cli_end()
  invisible(result)
}

#' Abort if a Suggested package is not installed
#'
#' @param pkg Name of the package required for a conversion.
#'
#' @return `NULL`, invisibly (called for its side effect of aborting when the
#'   package is missing).
#'
#' @keywords internal
#' @noRd
.need_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install_call <- paste0(
      "install.packages(\"", pkg, "\", ",
      "repos = c(options('repos'), ",
      "epinowcast = 'https://epinowcast.r-universe.dev'))"
    )
    cli::cli_abort(c(
      "Package {.pkg {pkg}} is required for this conversion.",
      "i" = paste0("Install it with: ", install_call)
    ))
  }
}

#' Assert that `x` is a `tbl_now` (for the `to_*` converters)
#'
#' @param x Object to check.
#' @param fn Name of the calling function, used in the error message.
#'
#' @return `NULL`, invisibly (aborts when `x` is not a `tbl_now`).
#'
#' @keywords internal
#' @noRd
.assert_tbl_now <- function(x, fn) {
  if (!is_tbl_now(x)) {
    cli::cli_abort("{.arg x} must be a {.cls tbl_now} object for {.fn {fn}}.")
  }
}

#' Abort when required columns are absent from a data frame
#'
#' @param data A data frame.
#' @param required Character vector of required column names.
#' @param arg Name of the argument to reference in the error message.
#'
#' @return `NULL`, invisibly (aborts when a required column is missing).
#'
#' @keywords internal
#' @noRd
.assert_columns_present <- function(data, required, arg = "data") {
  missing_columns <- setdiff(required, colnames(data))
  if (length(missing_columns) > 0L) {
    # qty() pins the pluralisation to `missing_columns` (otherwise cli cannot
    # tell which interpolated vector should drive `{?s}`).
    cli::cli_abort(
      "Column{cli::qty(missing_columns)}{?s} {.val {missing_columns}} \\
       not found in {.arg {arg}}."
    )
  }
  invisible(NULL)
}

#' Drop explicit zero-count rows so the resulting `tbl_now` is minimal
#'
#' External complete-grid formats (epinowcast's completed observations, a
#' baselinenowcast reporting triangle) carry explicit zeros for every
#' reference-date x delay cell. A `tbl_now` does not need those: a missing cell
#' is implicitly zero, and [complete_zeroes()] can re-create them on demand. So
#' converting *from* such a format strips the zeros to stay minimal.
#'
#' @param x A `tbl_now` object.
#'
#' @return `x` with zero-`case_count` rows removed (unchanged for linelist data
#'   or data without a `case_count` column).
#'
#' @keywords internal
#' @noRd
.drop_zero_counts <- function(x) {
  case_count_column <- get_case_count(x)
  if (is.null(case_count_column) || !grepl("count", get_data_type(x))) {
    return(x)
  }
  x |>
    dplyr::filter(
      is.na(.data[[case_count_column]]) | .data[[case_count_column]] != 0
    )
}

#' Extract the long cumulative observations from any epinowcast representation
#'
#' \pkg{epinowcast} carries the same observations in several shapes: the raw
#' long input `data.frame` (`reference_date`, `report_date`, cumulative
#' `confirm`), the preprocessed object returned by
#' [epinowcast::enw_preprocess_data()] (a nested `data.table` whose `obs`
#' element holds those observations), and a fitted `epinowcast` object (which
#' extends the preprocessed object). This pulls the long cumulative
#' observations out of whichever it is given.
#'
#' @param data A raw long `data.frame`/`data.table`, an `enw_preprocess_data`
#'   object, or a fitted `epinowcast` object.
#' @param reference_date,report_date,confirm Column names (raw input only).
#'
#' @return A list with `observations` (a long `data.frame` of
#'   `reference_date`/`report_date`/`confirm` plus any grouping columns),
#'   the resolved column names, `strata` (the grouping columns when known, else
#'   `NULL`), and `preprocessed` (`TRUE` when the input was an epinowcast
#'   object).
#'
#' @keywords internal
#' @noRd
.epinowcast_obs <- function(data, reference_date = "reference_date",
                            report_date = "report_date", confirm = "confirm") {
  # Preprocessed object (or a fitted epinowcast object, which extends it).
  if (inherits(data, "enw_preprocess_data") || inherits(data, "epinowcast")) {
    .need_pkg("epinowcast")

    # The cumulative observations live in the nested `obs` element; drop the
    # padding rows that epinowcast adds for missing reference dates.
    observations <- data$obs[[1]] |>
      as.data.frame() |>
      dplyr::filter(!is.na(.data$reference_date))

    # The grouping columns (`by`) are stored as a list-column; keep the ones
    # that survived into the observations.
    strata_columns <- intersect(unlist(data$by), colnames(observations))

    observations <- observations |>
      dplyr::select(dplyr::all_of(
        c("reference_date", "report_date", "confirm", strata_columns)
      )) |>
      dplyr::mutate(
        reference_date = as.Date(.data$reference_date),
        report_date    = as.Date(.data$report_date)
      )

    return(list(
      observations   = observations,
      reference_date = "reference_date",
      report_date    = "report_date",
      confirm        = "confirm",
      strata         = if (length(strata_columns) > 0) strata_columns else NULL,
      preprocessed   = TRUE
    ))
  }

  # Raw long input data.frame / data.table.
  observations <- as.data.frame(data)
  .assert_columns_present(observations, c(reference_date, report_date, confirm))
  list(
    observations   = observations,
    reference_date = reference_date,
    report_date    = report_date,
    confirm        = confirm,
    strata         = NULL,
    preprocessed   = FALSE
  )
}

#' Expand a reporting-triangle matrix into a long incremental data frame
#'
#' Row names are taken as reference dates and column names as integer delays
#' (falling back to 0-based delays when the column names are not numeric); each
#' non-`NA` cell becomes one row.
#'
#' @param triangle A reporting-triangle matrix (rownames = reference dates,
#'   colnames = delays).
#' @param delays_unit Unit of the delay axis: `"days"`, `"weeks"`, `"months"`
#'   or `"years"`.
#'
#' @return A data frame with columns `reference_date`, `report_date`, `count`.
#'
#' @keywords internal
#' @noRd
.reporting_triangle_to_long <- function(triangle, delays_unit = "days") {
  reference_labels <- rownames(triangle)
  if (is.null(reference_labels)) {
    cli::cli_abort(
      "Reporting-triangle matrix must have reference dates as row names."
    )
  }
  reference_dates <- as.Date(reference_labels)

  # Column names hold the delays; fall back to 0-based delays when they are not
  # numeric.
  delays <- suppressWarnings(as.integer(colnames(triangle)))
  if (anyNA(delays)) {
    delays <- seq_len(ncol(triangle)) - 1L
  }

  days_per_unit <- switch(delays_unit,
    days = 1, weeks = 7, months = 30, years = 365, 1
  )

  # One row per (reference date, delay) cell, dropping the empty (NA) cells.
  tidyr::expand_grid(
    row_index    = seq_len(nrow(triangle)),
    column_index = seq_len(ncol(triangle))
  ) |>
    dplyr::mutate(
      reference_date = reference_dates[.data$row_index],
      report_date    = reference_dates[.data$row_index] +
        delays[.data$column_index] * days_per_unit,
      count          = as.numeric(
        triangle[cbind(.data$row_index, .data$column_index)]
      )
    ) |>
    dplyr::filter(!is.na(.data$count)) |>
    dplyr::select(dplyr::all_of(c("reference_date", "report_date", "count"))) |>
    as.data.frame()
}

# ===========================================================================
# 2. tbl_now_from_*()  (package data -> tbl_now)
# ===========================================================================

#' Convert between `tbl_now` and \pkg{epinowcast}
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' \pkg{epinowcast} represents the same observations in several shapes:
#'
#' * the **raw long input** `data.frame` (`reference_date`, `report_date` and a
#'   **cumulative** `confirm` column, plus optional grouping columns) consumed
#'   by [epinowcast::enw_preprocess_data()];
#' * the **preprocessed object** returned by [epinowcast::enw_preprocess_data()]
#'   (a nested `data.table` used downstream for modelling, summaries and
#'   plotting);
#' * a fitted `epinowcast` object (which extends the preprocessed object).
#'
#' `tbl_now_from_epinowcast()` accepts **any** of these and converts the
#' cumulative observations into a `tbl_now` of `data_type = "count-cumulative"`.
#' When given a preprocessed or fitted object, the grouping (`by`) columns are
#' detected automatically and the observations are those retained by
#' preprocessing (i.e. truncated at `max_delay`).
#'
#' `tbl_now_to_epinowcast()` takes a `tbl_now` and, by default, builds the
#' preprocessed [epinowcast::enw_preprocess_data()] object (the representation
#' used for epinowcast's summaries and plots). With `preprocess = FALSE` it
#' returns the completed long observation `data.table` (the model *input*
#' format, as produced by [epinowcast::enw_complete_dates()]).
#'
#' @param data Source data: a raw long `data.frame`/`data.table`, an
#'   `enw_preprocess_data` object, or a fitted `epinowcast` object.
#' @param x A `tbl_now` object.
#' @param reference_date,report_date,confirm Column names (raw input only;
#'   ignored for preprocessed/fitted objects).
#' @param strata Optional character vector of grouping columns. If `NULL`
#'   (default) the grouping is taken from the preprocessed object's `by`, or,
#'   for raw input, any column other than `reference_date`, `report_date` and
#'   `confirm`.
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
#' @section Round-trip:
#' `tbl_now_from_epinowcast(tbl_now_to_epinowcast(x))` recovers `x` up to the
#' `max_delay` truncation that epinowcast applies during preprocessing: reports
#' with a delay beyond `max_delay` are dropped by
#' [epinowcast::enw_preprocess_data()] and so are absent from the result.
#'
#' @examplesIf requireNamespace("epinowcast", quietly = TRUE)
#' obs <- epinowcast::germany_covid19_hosp
#'
#' # From the raw long input format ...
#' nowobj <- tbl_now_from_epinowcast(obs, strata = c("location", "age_group"))
#'
#' # ... or straight from a preprocessed epinowcast object
#' pre <- tbl_now_to_epinowcast(nowobj, verbose = FALSE)
#' tbl_now_from_epinowcast(pre, verbose = FALSE)
#' @name tbl_now_epinowcast
#' @export
tbl_now_from_epinowcast <- function(data, ...,
                                    reference_date = "reference_date",
                                    report_date = "report_date",
                                    confirm = "confirm",
                                    strata = NULL,
                                    verbose = TRUE) {
  parsed <- .epinowcast_obs(data, reference_date, report_date, confirm)

  # Strata precedence: explicit argument > grouping detected on a preprocessed
  # object > (raw input only) any remaining non-core columns.
  if (is.null(strata)) {
    strata <- parsed$strata
    if (is.null(strata) && !parsed$preprocessed) {
      core_columns <- c(
        parsed$reference_date, parsed$report_date, parsed$confirm
      )
      strata <- setdiff(colnames(parsed$observations), core_columns)
      if (length(strata) == 0) strata <- NULL
    }
  }

  result <- .build_tbl_now(
    parsed$observations,
    dots = list(...),
    event_date = parsed$reference_date,
    report_date = parsed$report_date,
    case_count = parsed$confirm,
    strata = strata,
    data_type = "count-cumulative"
  )

  # epinowcast data is completed (explicit zeros); keep the tbl_now minimal.
  result <- .drop_zero_counts(result)

  .report_from(result, "epinowcast", verbose)
  result
}

#' Convert between `tbl_now` and \pkg{baselinenowcast}
#'
#'
#' @description `r lifecycle::badge("experimental")`
#'
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
                                         report_date = "report_date",
                                         count = "count",
                                         delays_unit = "days",
                                         verbose = TRUE) {
  # A reporting-triangle matrix is expanded to long incremental form; a long
  # data frame is selected and renamed to the canonical column names.
  if (is.matrix(data) || inherits(data, "reporting_triangle")) {
    long_data <- .reporting_triangle_to_long(data, delays_unit = delays_unit)
    extra_message <- "expanded a reporting-triangle matrix to long counts"
  } else {
    long_data <- as.data.frame(data)
    .assert_columns_present(long_data, c(reference_date, report_date, count))
    long_data <- long_data |>
      dplyr::select(
        reference_date = dplyr::all_of(reference_date),
        report_date    = dplyr::all_of(report_date),
        count          = dplyr::all_of(count)
      )
    extra_message <- NULL
  }

  result <- .build_tbl_now(
    long_data,
    dots = list(...),
    event_date = "reference_date",
    report_date = "report_date",
    case_count = "count",
    data_type = "count-incidence"
  )

  # Reporting triangles carry explicit zero cells; keep the tbl_now minimal.
  result <- .drop_zero_counts(result)

  .report_from(result, "baselinenowcast", verbose, extra = extra_message)
  result
}

#' Convert between `tbl_now` and \pkg{data.table}
#'
#' @description `r lifecycle::badge("experimental")`
#'
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
#' nowobj <- tbl_now_from_data_table(dt,
#'   event_date = "onset_week",
#'   report_date = "report_week", verbose = FALSE
#' )
#' @name tbl_now_data_table
#' @export
tbl_now_from_data_table <- function(data, event_date, report_date, ...,
                                    verbose = TRUE) {
  if (!inherits(data, "data.table")) {
    cli::cli_warn(
      "{.arg data} is not a {.cls data.table}; coercing to a data frame."
    )
  }

  observations <- as.data.frame(data)

  result <- .build_tbl_now(
    observations,
    dots = list(...),
    event_date = event_date,
    report_date = report_date
  )

  .report_from(result, "data.table", verbose)
  result
}

#' Convert between `tbl_now` and \pkg{epidist}
#'
#' @description `r lifecycle::badge("experimental")`
#'
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
#'   (`format = "interval"`). Default to epidist's `"pdate_upr"` /
#'   `"sdate_upr"`.
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
                                 primary = "pdate_lwr",
                                 secondary = "sdate_lwr",
                                 primary_upper = "pdate_upr",
                                 secondary_upper = "sdate_upr",
                                 verbose = TRUE) {
  format <- match.arg(format)
  observations <- as.data.frame(data)

  # Linelist: the lower bounds become the event and report dates.
  if (format == "linelist") {
    .assert_columns_present(observations, c(primary, secondary))

    result <- .build_tbl_now(
      observations,
      dots = list(...),
      event_date = primary,
      report_date = secondary,
      data_type = "linelist"
    )
    .report_from(
      result, "epidist", verbose,
      extra = paste0(
        "format: linelist (primary lower bound -> event_date, ",
        "secondary lower bound -> report_date)"
      )
    )
    return(result)
  }

  # Interval-censored: lower bounds become the dates, upper bounds become
  # covariates.
  .assert_columns_present(
    observations, c(primary, secondary, primary_upper, secondary_upper)
  )
  upper_bounds <- c(primary_upper, secondary_upper)
  cli::cli_warn(c(
    "Interval-censored data:",
    "*" = "lower bounds {.val {c(primary, secondary)}} -> event/report dates",
    "*" = "upper bounds {.val {upper_bounds}} -> covariates"
  ))

  result <- .build_tbl_now(
    observations,
    dots = list(...),
    event_date = primary,
    report_date = secondary,
    covariates = upper_bounds,
    data_type = "linelist"
  )
  .report_from(
    result, "epidist", verbose,
    extra = paste0(
      "format: interval (lower bounds -> dates, ",
      "upper bounds -> covariates)"
    )
  )
  result
}

#' Convert between `tbl_now` and \pkg{tsibble}
#'
#' @description `r lifecycle::badge("experimental")`
#'
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
#' nowobj <- tbl_now(denguedat,
#'   event_date = "onset_week",
#'   report_date = "report_week", verbose = FALSE
#' )
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

  # `report_date` defaults to the tsibble index.
  if (is.null(report_date)) {
    if (!inherits(data, "tbl_ts")) {
      cli::cli_abort(
        "{.arg report_date} is required when {.arg data} is not a tsibble."
      )
    }
    report_date <- tsibble::index_var(data)
  }

  # Recover strata from the tsibble key (the key vars other than the dates).
  if (is.null(strata) && inherits(data, "tbl_ts")) {
    strata <- setdiff(tsibble::key_vars(data), c(event_date, report_date))
    if (length(strata) == 0) strata <- NULL
  }

  observations <- as.data.frame(data)

  result <- .build_tbl_now(
    observations,
    dots = list(...),
    event_date = event_date,
    report_date = report_date,
    strata = strata
  )

  .report_from(result, "tsibble", verbose,
    extra = paste0("report_date taken from the tsibble index: ", report_date)
  )
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

  # epinowcast models the cumulative reporting process, so coerce first.
  if (get_data_type(x) != "count-cumulative") {
    cli::cli_warn(
      "epinowcast expects cumulative counts; {.arg x} has data_type \\
       {.val {get_data_type(x)}}. Converting with {.fn to_count}."
    )
    x <- to_count(x, to = "count-cumulative")
  }

  event_col   <- get_event_date(x)
  report_col  <- get_report_date(x)
  count_col   <- get_case_count(x)
  strata_cols <- get_strata(x)

  # epinowcast's schema is reference_date / report_date / confirm (+ grouping);
  # covariates and is_censored have no place in it and are not carried over.
  observations <- x |>
    dplyr::as_tibble() |>
    dplyr::select(
      reference_date = dplyr::all_of(event_col),
      report_date    = dplyr::all_of(report_col),
      confirm        = dplyr::all_of(count_col),
      dplyr::all_of(strata_cols)
    ) |>
    data.table::as.data.table()

  if (is.null(max_delay)) {
    max_delay <- as.integer(max(dplyr::pull(x, ".delay"), na.rm = TRUE)) + 1L
  }
  grouping <- if (length(strata_cols) > 0) strata_cols else NULL

  if (verbose) {
    cli::cli_h3("Converting {.cls tbl_now} into an {.pkg epinowcast} object")
    cli::cli_ul()
    cli::cli_li("reference_date <- {.val {event_col}}")
    cli::cli_li("report_date <- {.val {report_col}}")
    cli::cli_li("confirm <- {.val {count_col}}")
    cli::cli_li("by: {.val {if (is.null(grouping)) 'none' else grouping}}")
    cli::cli_li("max_delay: {.val {max_delay}}")
    cli::cli_li("preprocess: {.val {preprocess}}")
    cli::cli_end()
  }

  completed <- epinowcast::enw_complete_dates(
    observations, by = grouping, max_delay = max_delay
  )
  if (!preprocess) {
    return(completed)
  }

  epinowcast::enw_preprocess_data(
    completed, by = grouping, max_delay = max_delay, ...
  )
}

#' @rdname tbl_now_baselinenowcast
#' @export
tbl_now_to_baselinenowcast <- function(x, ..., format = c("long", "matrix"),
                                       delays_unit = "days", verbose = TRUE) {
  .assert_tbl_now(x, "tbl_now_to_baselinenowcast")
  format <- match.arg(format)
  # Note: the long format is a plain data.frame and needs no package; only the
  # matrix format calls into baselinenowcast (guarded below).

  # baselinenowcast needs incremental (count-incidence) counts.
  #  - count-incidence: use as-is.
  #  - linelist: aggregating to incidence is well defined.
  #  - count-cumulative: NOT convertible — cumulative totals get revised
  #    downward (cases un-confirmed), so de-accumulating would yield negative
  #    "incidence". Refuse rather than produce nonsense.
  data_type <- get_data_type(x)
  if (data_type == "count-cumulative") {
    cli::cli_abort(c(
      "Cannot convert {.val count-cumulative} data to the incremental counts \\
       {.pkg baselinenowcast} requires.",
      "i" = "Cumulative totals can be revised downward, so de-accumulating \\
             them may give negative incidence.",
      "i" = "Supply {.val count-incidence} or {.val linelist} data instead."
    ))
  } else if (data_type != "count-incidence") {
    cli::cli_warn(
      "baselinenowcast expects incremental counts; converting {.arg x} to \\
       {.val count-incidence} with {.fn to_count}."
    )
    x <- to_count(x, to = "count-incidence")
  }

  event_col      <- get_event_date(x)
  report_col     <- get_report_date(x)
  count_col      <- get_case_count(x)
  covariate_cols <- get_covariates(x)
  censored_col   <- get_is_censored(x)

  # The long format is a tidy data frame, so it can also carry the covariates
  # and the censoring indicator. The reporting-triangle matrix cannot, so only
  # the three core columns are kept for it.
  extra_cols <- if (format == "long") c(covariate_cols, censored_col) else NULL

  long_data <- x |>
    dplyr::as_tibble() |>
    dplyr::select(
      reference_date = dplyr::all_of(event_col),
      report_date    = dplyr::all_of(report_col),
      count          = dplyr::all_of(count_col),
      dplyr::all_of(extra_cols)
    )

  if (verbose) {
    cli::cli_h3("Converting {.cls tbl_now} to {.pkg baselinenowcast} {format}")
    cli::cli_ul()
    cli::cli_li("reference_date <- {.val {event_col}}")
    cli::cli_li("report_date <- {.val {report_col}}")
    cli::cli_li("count <- {.val {count_col}}")
    if (length(extra_cols) > 0) {
      cli::cli_li("kept columns: {.val {extra_cols}}")
    }
    cli::cli_li("format: {.val {format}}")
    cli::cli_end()
  }

  if (format == "long") {
    return(as.data.frame(long_data))
  }

  .need_pkg("baselinenowcast")
  baselinenowcast::as_reporting_triangle(
    as.data.frame(long_data), delays_unit = delays_unit, ...
  )
}

#' Convert a `tbl_now` into \pkg{EpiNow2} input
#'
#' `r lifecycle::badge("experimental")`
#'
#' @description
#' \pkg{EpiNow2}'s renewal-equation models ([EpiNow2::estimate_infections()],
#' [EpiNow2::epinow()]) take a **single incidence time series** (`date`,
#' `confirm`) with no report/delay dimension. For these
#' (`model = "estimate_infections"`, the default) the `tbl_now` is collapsed to
#' that single series using the most recently reported counts per `event_date`
#' (equivalent to [get_latest_reported_cases()]).
#'
#' [EpiNow2::estimate_truncation()] is different: it takes **multiple
#' snapshots** of the same series observed at successive report dates and *does*
#' use the report dimension. With `model = "estimate_truncation"` the `tbl_now`
#' is turned into that list of `date`/`confirm` snapshots (one per report date,
#' earliest first) — precisely the information the report dimension encodes.
#'
#' There is intentionally **no** `tbl_now_from_EpiNow2()`: neither a single time
#' series nor a set of snapshots can, in general, reconstruct the full
#' event/report structure of a `tbl_now`.
#'
#' @param x A `tbl_now` object.
#' @param model Which \pkg{EpiNow2} model the output targets:
#'   `"estimate_infections"` (default; a single `date`/`confirm` series for
#'   [EpiNow2::estimate_infections()] / [EpiNow2::epinow()]) or
#'   `"estimate_truncation"` (a list of `date`/`confirm` snapshots for
#'   [EpiNow2::estimate_truncation()]).
#' @param verbose Logical. Print the choices that were made.
#' @param ... Forwarded to [data.table::as.data.table()].
#'
#' @return For `"estimate_infections"`, a `data.table` with columns `date` and
#'   `confirm`. For `"estimate_truncation"`, a list of such `data.table`s
#'   ordered from earliest to latest report snapshot.
#'
#' @examplesIf requireNamespace("data.table", quietly = TRUE)
#' data(mpoxdat)
#' nowobj <- tbl_now(mpoxdat,
#'   event_date = "dx_date", report_date = "dx_report_date",
#'   case_count = "n", strata = "race",
#'   data_type = "count-incidence", verbose = FALSE
#' )
#' # single series for estimate_infections()
#' tbl_now_to_EpiNow2(nowobj, verbose = FALSE)
#'
#' # report-date snapshots for estimate_truncation()
#' tbl_now_to_EpiNow2(nowobj, model = "estimate_truncation", verbose = FALSE)
#' @export
# `tbl_now_to_EpiNow2` is named after the EpiNow2 package (not snake_case).
tbl_now_to_EpiNow2 <- function( # nolint: object_name_linter.
    x, ...,
    model = c("estimate_infections", "estimate_truncation"),
    verbose = TRUE) {
  .assert_tbl_now(x, "tbl_now_to_EpiNow2")
  .need_pkg("data.table")
  model <- match.arg(model)

  event_col   <- get_event_date(x)
  strata_cols <- get_strata(x)

  # Cumulative cases for each event date, by report date and strata. Coercing to
  # cumulative works for any input type (linelist / incidence / cumulative).
  cumulative <- x |>
    dplyr::ungroup() |>
    to_count(to = "count-cumulative")
  count_col <- get_case_count(cumulative)

  known <- cumulative |>
    dplyr::as_tibble() |>
    dplyr::select(
      date        = dplyr::all_of(event_col),
      report_date = dplyr::all_of(get_report_date(cumulative)),
      confirm     = dplyr::all_of(count_col),
      dplyr::all_of(strata_cols)
    )

  # The incidence series as known at report date `as_of`: per (date, strata)
  # take the latest report on or before `as_of`, then sum over strata.
  build_snapshot <- function(as_of) {
    known |>
      dplyr::filter(.data$report_date <= as_of) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c("date", strata_cols)))) |>
      dplyr::slice_max(.data$report_date, n = 1, with_ties = FALSE) |>
      dplyr::group_by(.data$date) |>
      dplyr::summarise(
        confirm = sum(.data$confirm, na.rm = TRUE), .groups = "drop"
      ) |>
      dplyr::arrange(.data$date)
  }

  # estimate_infections: a single series at the current `now`.
  if (model == "estimate_infections") {
    series <- build_snapshot(get_now(x))
    if (verbose) {
      cli::cli_h3("Converting {.cls tbl_now} to an {.pkg EpiNow2} series")
      cli::cli_ul()
      cli::cli_li("date <- {.val {event_col}} (event_date)")
      cli::cli_li("confirm <- latest reported counts per date")
      cli::cli_li("rows: {.val {nrow(series)}}")
      cli::cli_alert_info(
        "For {.fn estimate_infections} the report dimension is collapsed \\
         to a single series."
      )
      cli::cli_end()
    }
    return(data.table::as.data.table(series, ...))
  }

  # estimate_truncation: one snapshot per distinct report date (earliest first).
  report_dates <- sort(unique(known$report_date))
  snapshots <- lapply(report_dates, function(report_date) {
    data.table::as.data.table(build_snapshot(report_date), ...)
  })

  if (verbose) {
    cli::cli_h3(
      "Converting {.cls tbl_now} into {.pkg EpiNow2} truncation snapshots"
    )
    cli::cli_ul()
    cli::cli_li("date <- {.val {event_col}} (event_date)")
    cli::cli_li("snapshots: {.val {length(snapshots)}} (one per report date)")
    cli::cli_alert_info("Pass the list to {.fn EpiNow2::estimate_truncation}.")
    cli::cli_end()
  }

  snapshots
}

#' @rdname tbl_now_data_table
#' @export
tbl_now_to_data_table <- function(x, ..., verbose = TRUE) {
  .assert_tbl_now(x, "tbl_now_to_data_table")
  .need_pkg("data.table")

  if (verbose) {
    cli::cli_h3("Converting {.cls tbl_now} into a {.cls data.table}")
    cli::cli_alert_info(
      "tbl_now attributes are dropped; every column is kept (including the \\
       generated .delay / .event_num / .report_num, the covariates and \\
       is_censored)."
    )
  }

  # A data.table can hold every column, so keep them all (covariates and the
  # censoring indicator included).
  data.table::as.data.table(as.data.frame(x), ...)
}

#' @rdname tbl_now_epidist
#' @export
tbl_now_to_epidist <- function(x, ..., format = c("linelist", "interval"),
                               primary_upper = NULL,
                               secondary_upper = NULL,
                               verbose = TRUE) {
  .assert_tbl_now(x, "tbl_now_to_epidist")
  .need_pkg("epidist")
  format <- match.arg(format)

  event_col      <- get_event_date(x)
  report_col     <- get_report_date(x)
  covariate_cols <- get_covariates(x)
  censored_col   <- get_is_censored(x)
  observations   <- dplyr::as_tibble(x)

  constructor_args <- list(pdate_lwr = "pdate_lwr", sdate_lwr = "sdate_lwr")

  if (format == "linelist") {
    # Lower bounds only: event date and report date.
    epidist_data <- observations |>
      dplyr::transmute(
        pdate_lwr = .data[[event_col]],
        sdate_lwr = .data[[report_col]]
      )
    carried_cols  <- c(covariate_cols, censored_col)
    extra_message <- "pdate_lwr <- event_date, sdate_lwr <- report_date"
  } else {
    if (is.null(primary_upper) || is.null(secondary_upper)) {
      cli::cli_abort(
        "For {.val interval} format, supply {.arg primary_upper} and \\
         {.arg secondary_upper} (covariate columns holding the upper bounds)."
      )
    }
    .assert_columns_present(
      observations, c(primary_upper, secondary_upper), arg = "x"
    )
    upper_bounds <- c(primary_upper, secondary_upper)
    cli::cli_warn(c(
      "Building interval-censored data:",
      "*" = "lower bounds <- event/report dates",
      "*" = "upper bounds <- covariates {.val {upper_bounds}}"
    ))
    epidist_data <- observations |>
      dplyr::transmute(
        pdate_lwr = .data[[event_col]],
        pdate_upr = .data[[primary_upper]],
        sdate_lwr = .data[[report_col]],
        sdate_upr = .data[[secondary_upper]]
      )
    constructor_args$pdate_upr <- "pdate_upr"
    constructor_args$sdate_upr <- "sdate_upr"
    # The upper bounds are already in `epidist_data`; do not carry them twice.
    carried_cols  <- setdiff(c(covariate_cols, censored_col), upper_bounds)
    extra_message <- paste0(
      "pdate_lwr/sdate_lwr <- dates, ",
      "pdate_upr/sdate_upr <- covariates"
    )
  }

  # Carry the remaining covariates and the censoring indicator alongside.
  if (length(carried_cols) > 0) {
    epidist_data <- dplyr::bind_cols(
      epidist_data,
      dplyr::select(observations, dplyr::all_of(carried_cols))
    )
  }

  if (verbose) {
    cli::cli_h3("Converting {.cls tbl_now} into {.pkg epidist} {format} data")
    cli::cli_ul()
    cli::cli_li(extra_message)
    if (length(carried_cols) > 0) {
      cli::cli_li("kept columns: {.val {carried_cols}}")
    }
    cli::cli_end()
  }

  do.call(
    epidist::as_epidist_linelist_data,
    c(list(epidist_data), constructor_args, list(...))
  )
}

#' @rdname tbl_now_tsibble
#' @export
tbl_now_to_tsibble <- function(x, ..., index = c("report_date", "event_date"),
                               verbose = TRUE) {
  .assert_tbl_now(x, "tbl_now_to_tsibble")
  .need_pkg("tsibble")
  index <- match.arg(index)

  # A tsibble needs a unique index/key combination. Linelist rows are not
  # unique per (event, report, strata), so aggregate to count-incidence first.
  if (get_data_type(x) == "linelist") {
    cli::cli_warn(
      "tsibble requires unique index/key rows; aggregating linelist to \\
       {.val count-incidence} with {.fn to_count}."
    )
    x <- to_count(x, to = "count-incidence")
  }

  event_col      <- get_event_date(x)
  report_col     <- get_report_date(x)
  strata_cols    <- get_strata(x)
  covariate_cols <- get_covariates(x)
  censored_col   <- get_is_censored(x)
  count_col      <- get_case_count(x)

  # The chosen date is the tsibble index; the other date plus the strata form
  # the key (so the index/key combination is unique).
  index_col <- if (index == "report_date") report_col else event_col
  other_col <- if (index == "report_date") event_col else report_col
  key_cols  <- c(other_col, strata_cols)

  # Covariates, the censoring indicator and the case count ride along as
  # measurement columns; the tbl_now internals are dropped.
  kept_cols <- c(
    index_col, other_col, strata_cols, covariate_cols, censored_col, count_col
  )
  observations <- x |>
    dplyr::as_tibble() |>
    dplyr::select(dplyr::all_of(kept_cols))

  if (verbose) {
    cli::cli_h3("Converting {.cls tbl_now} into a {.pkg tsibble}")
    cli::cli_ul()
    cli::cli_li("index <- {.val {index_col}}")
    cli::cli_li("key <- {.val {key_cols}}")
    cli::cli_end()
  }

  tsibble::as_tsibble(
    observations,
    index = !!rlang::sym(index_col),
    key   = tidyselect::all_of(key_cols),
    ...
  )
}

# ===========================================================================
# 4. S3 methods on other packages' coercion generics
#
#    These register a `tbl_now` method on each supported package's own coercion
#    generic, so that package's verb accepts a `tbl_now` directly. Each is a
#    thin wrapper around the matching tbl_now_to_*() converter and is quiet by
#    default (verbose = FALSE) because it is a coercion idiom.
# ===========================================================================

#' Coerce a `tbl_now` with another package's generic
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' These S3 methods make each supported package's own coercion verb accept a
#' `tbl_now`. They are thin wrappers around the matching `tbl_now_to_*()`
#' converter and are quiet by default.
#'
#' * `as_epidist_linelist_data()` (\pkg{epidist}) wraps [tbl_now_to_epidist()].
#' * `as_reporting_triangle()` (\pkg{baselinenowcast}) wraps
#'   [tbl_now_to_baselinenowcast()] with `format = "matrix"`.
#' * `as_tsibble()` (\pkg{tsibble}) wraps [tbl_now_to_tsibble()].
#' * `as.data.table()` (\pkg{data.table}) wraps [tbl_now_to_data_table()].
#'
#' @param data,x A `tbl_now` object.
#' @param verbose Logical; forwarded to the underlying converter. Defaults to
#'   `FALSE` so coercion is quiet.
#' @param ... Additional arguments forwarded to the underlying converter.
#'
#' @return The object produced by the corresponding `tbl_now_to_*()` converter.
#'
#' @name tbl_now_coercion_methods
NULL

#' @rdname tbl_now_coercion_methods
#' @exportS3Method epidist::as_epidist_linelist_data
as_epidist_linelist_data.tbl_now <- function(data, ..., verbose = FALSE) {
  tbl_now_to_epidist(data, ..., verbose = verbose)
}

#' @rdname tbl_now_coercion_methods
#' @exportS3Method baselinenowcast::as_reporting_triangle
as_reporting_triangle.tbl_now <- function(data, ..., verbose = FALSE) {
  tbl_now_to_baselinenowcast(data, format = "matrix", ..., verbose = verbose)
}

#' @rdname tbl_now_coercion_methods
#' @exportS3Method tsibble::as_tsibble
as_tsibble.tbl_now <- function(x, ..., verbose = FALSE) {
  tbl_now_to_tsibble(x, ..., verbose = verbose)
}

#' @rdname tbl_now_coercion_methods
#' @exportS3Method data.table::as.data.table
as.data.table.tbl_now <- function(x, ..., verbose = FALSE) {
  tbl_now_to_data_table(x, ..., verbose = verbose)
}
