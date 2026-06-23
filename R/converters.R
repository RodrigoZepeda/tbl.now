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

#' Warn that a `tbl_now -> external format` conversion is lossy
#'
#' Round-trips through `tbl_now` are not the identity: each external format
#' carries metadata, padding, grouping indices or covariate columns that
#' `tbl_now` does not retain. This emits a one-line warning telling the user it
#' is always preferable to work from the original data than to convert back and
#' forth between formats.
#'
#' @param target Name of the destination package/format (for the message).
#' @param quiet If `TRUE`, suppress the warning.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
.warn_lossy_conversion <- function(target, quiet = FALSE) {
  if (isTRUE(quiet)) {
    return(invisible(NULL))
  }
  cli::cli_warn(c(
    "!" = "Converting a {.cls tbl_now} to {.pkg {target}} is {.emph lossy}: \\
           the result is not guaranteed to be identical to a native \\
           {.pkg {target}} object.",
    "i" = "Some information is dropped or re-synthesised in the round-trip \\
           (e.g. covariate columns, grouping indices and padding rows).",
    "i" = "If you have the data in {.pkg {target}}'s own format, prefer using \\
           that directly over converting from another format.",
    "i" = "Silence this warning with {.code quiet = TRUE}."
  ))
  invisible(NULL)
}

#' Strip `tbl_now` metadata, returning a plain `data.frame`
#'
#' The `tbl_now` class stores its metadata (`event_date`, `report_date`, `now`,
#' `event_units`, `data_type`, `temporal_effects`, ... and any extra attributes
#' the constructor was given) as object attributes, and the package's dplyr
#' methods deliberately propagate them. When converting **out** to another
#' format we want none of that to ride along, so this reduces `x` to a base
#' `data.frame` carrying only `names`/`row.names`/`class` (column attributes
#' such as `Date` are untouched). It removes every non-standard attribute
#' generically, so it stays correct even if new `tbl_now` attributes are added.
#'
#' @param x A `tbl_now` (or any data frame).
#'
#' @return A plain `data.frame` with the same columns and no `tbl_now` metadata.
#'
#' @keywords internal
#' @noRd
.strip_tbl_now <- function(x) {
  out <- as.data.frame(x)
  attributes(out) <- list(
    names     = names(out),
    row.names = attr(out, "row.names", exact = TRUE),
    class     = "data.frame"
  )
  out
}

#' Censoring-window width (in days) for a `tbl_now` time unit
#'
#' \pkg{epidist} works in days and represents each event as an interval-censored
#' window `[date, date + width]`. When a `tbl_now` is measured in coarser units
#' the window should match the unit's resolution, so a weekly observation is
#' censored over its whole week. This maps a unit to that width in days.
#'
#' @param units One of `"days"`, `"weeks"`, `"months"`, `"years"`, `"numeric"`.
#'
#' @return A positive integer width in days (the day count epidist should span).
#'
#' @keywords internal
#' @noRd
.epidist_window_days <- function(units) {
  switch(units,
    days = 1L, weeks = 7L, months = 30L, years = 365L, numeric = 1L,
    1L
  )
}

#' Map a day-width back to the coarsest matching `tbl_now` unit
#'
#' Inverse of `.epidist_window_days()` for the units epidist can represent
#' exactly (days and weeks); anything else falls back to `"days"`.
#'
#' @param width_days Numeric primary censoring-window width in days.
#'
#' @return `"weeks"` when the width is a non-zero multiple of 7, else `"days"`.
#'
#' @keywords internal
#' @noRd
.epidist_units_from_window <- function(width_days) {
  if (length(width_days) == 0 || anyNA(width_days)) {
    return("days")
  }
  w <- stats::median(width_days)
  if (w > 0 && w %% 7 == 0) "weeks" else "days"
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

#' Extract the long cumulative observations from any `epinowcast` representation
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
#'
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
#'
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
#' @param missing_reference Passed to [epinowcast::enw_complete_dates()].
#'   Defaults to `FALSE` (unlike epinowcast's own default of `TRUE`): a
#'   `tbl_now` never carries reports with a missing `reference_date`, so leaving
#'   this `TRUE` would synthesise NA-reference padding rows the data never had.
#' @param preprocess If `TRUE` (default) returns an `enw_preprocess_data`
#'   object; if `FALSE` returns the completed observation `data.table`.
#' @param verbose Logical. Print the choices that were made.
#' @param quiet Logical. If `TRUE`, suppress the lossy-conversion warning emitted
#'   by `tbl_now_to_epinowcast()` (see the Round-trip section).
#' @param ... Additional arguments forwarded to [as_tbl_now()] (for `from`)
#'   or to [epinowcast::enw_preprocess_data()] (for `to`).
#'
#' @return `tbl_now_from_epinowcast()` returns a `tbl_now`.
#'   `tbl_now_to_epinowcast()` returns an `enw_preprocess_data` object or a
#'   `data.table`.
#'
#' @section Round-trip:
#' The round-trip is **not** the identity, and `tbl_now_to_epinowcast()` warns
#' to that effect (silence it with `quiet = TRUE`). If you already have the data
#' in epinowcast's format, work from it directly rather than converting through
#' `tbl_now` and back.
#'
#' `tbl_now_from_epinowcast(tbl_now_to_epinowcast(x))` recovers `x` up to the
#' `max_delay` truncation that epinowcast applies during preprocessing: reports
#' with a delay beyond `max_delay` are dropped by
#' [epinowcast::enw_preprocess_data()] and so are absent from the result.
#'
#' Conversely, `tbl_now_to_epinowcast(tbl_now_from_epinowcast(pobs))` is not
#' identical to `pobs`, because a `tbl_now` does not retain everything an
#' `enw_preprocess_data` object carries:
#' * **Covariate columns** that are neither the core
#'   `reference_date`/`report_date`/`confirm` nor a grouping (`by`) column are
#'   dropped (e.g. a constant `location` column).
#' * **Grouping indices** (`.group`) are reassigned from the factor levels, so
#'   the row order of the nested tables can differ even though the underlying
#'   values match.
#' * **NA-reference padding** is not regenerated by default (see
#'   `missing_reference`).
#'
#' @examplesIf requireNamespace("epinowcast", quietly = TRUE)
#' obs  <- epinowcast::germany_covid19_hosp[location == "DE"]
#' pobs <- epinowcast::enw_preprocess_data(obs, max_delay = 40, by = "age_group")
#'
#' # From the raw long input format ...
#' nowobj <- tbl_now_from_epinowcast(obs, strata = c("age_group"))
#'
#' # ... or from a preprocessed epinowcast object
#' tbl_epi <- tbl_now_from_epinowcast(pobs)
#'
#' #You can also convert to epinowcast preprocess data format
#' tbl_now_to_epinowcast(tbl_epi)
#'
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
#' @param format For `to`: `"matrix"` (default) or `"long"`.
#' @param verbose Logical. Print the choices that were made.
#' @param quiet Logical. If `TRUE`, suppress the lossy-conversion warning emitted
#'   by `tbl_now_to_baselinenowcast()` (see the Round-trip section).
#' @param ... Forwarded to [as_tbl_now()] (`from`) or
#'   [baselinenowcast::as_reporting_triangle()] (`to`, matrix format).
#'
#' @return A `tbl_now` (`from`), or a `data.frame`/`reporting_triangle` (`to`).
#'
#' @section Round-trip:
#' The round-trip is **not** the identity, and `tbl_now_to_baselinenowcast()`
#' warns to that effect (silence it with `quiet = TRUE`). If you already have a
#' `reporting_triangle`, work from it directly rather than converting through a
#' `tbl_now` and back.
#'
#' `identical(rt, tbl_now_to_baselinenowcast(tbl_now_from_baselinenowcast(rt)))`
#' is `FALSE` for a fundamental reason: a `reporting_triangle` distinguishes
#' **not-yet-observed** cells (`NA`) from **observed zeros** (`0`), but a
#' `tbl_now` stores only the observed counts as rows — both `NA` and `0` cells
#' become *absent* rows. On the way back,
#' [baselinenowcast::as_reporting_triangle()] re-rectangularises the data by
#' inferring the triangle boundary from the span of report dates present, and
#' fills any gap **inside** that boundary with `0` (only cells **beyond** it stay
#' `NA`). All counts that were actually present are preserved exactly; what is
#' lost is the precise `NA`/`0` missingness mask, which a ragged triangle (such
#' as `baselinenowcast::example_reporting_triangle`) does not encode as a simple
#' function of the report dates.
#'
#' @examplesIf requireNamespace("baselinenowcast", quietly = TRUE)
#' # Get a reporting triangle example
#' rt     <- baselinenowcast::example_reporting_triangle
#'
#' # Convert to a tbl_now
#' nowobj <- tbl_now_from_baselinenowcast(rt)
#'
#' # Note: the round-trip is lossy and so this is *not* identical to `rt`
#' # (not-yet-observed `NA` cells come back as `0`). Prefer using `rt` directly.
#' identical(rt, tbl_now_to_baselinenowcast(nowobj))
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
#' primary event and `sdate_lwr`/`sdate_upr` for the secondary event. It comes
#' in two shapes: a one-row-per-case `epidist_linelist_data`
#' ([epidist::as_epidist_linelist_data()]) and an `epidist_aggregate_data` that
#' adds an `n` count column ([epidist::as_epidist_aggregate_data()]). epidist
#' stores everything in **days** and requires every censoring window to have a
#' strictly positive width.
#'
#' `tbl_now_from_epidist()` converts either shape into a `tbl_now`:
#'
#' * `"auto"` (default): use the lower bounds — `primary` (`pdate_lwr`) becomes
#'   `event_date`, `secondary` (`sdate_lwr`) becomes `report_date`. An
#'   `epidist_aggregate_data` (or any input with an `n` column) becomes
#'   `data_type = "count-incidence"` with `case_count = "n"`; otherwise
#'   `data_type = "linelist"`. The `event_units`/`report_units` are inferred
#'   from the primary censoring-window width (a 7-day window ⇒ `"weeks"`), and a
#'   left-censored secondary window `[origin, report]` is decoded back to
#'   `is_censored = TRUE` with the report taken from `secondary_upper`.
#' * `"interval"`: instead attach the upper bounds `primary_upper`
#'   (`pdate_upr`) and `secondary_upper` (`sdate_upr`) as `covariates`
#'   (a warning is emitted).
#'
#' `tbl_now_to_epidist()` performs the inverse. By default (`format = "auto"`)
#' it builds an `epidist_aggregate_data` when `x` holds counts and an
#' `epidist_linelist_data` otherwise, filling all four interval columns:
#'
#' * the primary event spans `[event_date, event_date + w]`, where the window
#'   `w` matches the `tbl_now` unit (`"days"` ⇒ 1 day, `"weeks"` ⇒ 7 days, ...,
#'   or `censoring_window` if supplied);
#' * the secondary event spans `[report_date, report_date + w]` normally, but
#'   for rows flagged by `is_censored` it is left-censored to
#'   `[origin, report_date]` (with `origin` the earliest `event_date`, i.e.
#'   epidist time 0) — encoding the `tbl_now` convention that a censored report
#'   is only known to lie in `[0, report_date]`.
#'
#' @param data A `data.frame`, `epidist_linelist_data` or
#'   `epidist_aggregate_data` of \pkg{epidist} delay data.
#' @param x A `tbl_now` object.
#' @param format For `from`: `"auto"` (default) or `"interval"`. For `to`:
#'   `"auto"` (default), `"linelist"`, `"aggregate"` or `"interval"`.
#' @param primary,secondary Column names of the primary / secondary event
#'   lower-bound dates. Default to epidist's `"pdate_lwr"` / `"sdate_lwr"`.
#' @param primary_upper,secondary_upper Column names of the upper-bound dates.
#'   Default to epidist's `"pdate_upr"` / `"sdate_upr"`. Used to infer units and
#'   decode censoring (`from`) or, with `format = "interval"`, taken from
#'   covariate columns (`to`).
#' @param censoring_window (`to` only) Optional positive integer width, in days,
#'   of the censoring windows. If `NULL` (default) it is derived from the
#'   `tbl_now` `event_units`.
#' @param verbose Logical. Print the choices that were made.
#' @param quiet Logical. If `TRUE`, suppress the lossy-conversion warning emitted
#'   by `tbl_now_to_epidist()`.
#' @param ... Forwarded to [as_tbl_now()] (`from`) or to the relevant epidist
#'   constructor (`to`).
#'
#' @return A `tbl_now` (`from`) or an `epidist_linelist_data` /
#'   `epidist_aggregate_data` object (`to`).
#'
#' @examplesIf requireNamespace("epidist", quietly = TRUE)
#' df <- data.frame(
#'   pdate_lwr = as.Date(c("2020-03-01", "2020-03-02")),
#'   sdate_lwr = as.Date(c("2020-03-05", "2020-03-04"))
#' )
#' tbl_now_from_epidist(df, event_units = "days", report_units = "days")
#' @name tbl_now_epidist
#' @export
tbl_now_from_epidist <- function(data, ..., format = c("auto", "interval"),
                                 primary = "pdate_lwr",
                                 secondary = "sdate_lwr",
                                 primary_upper = "pdate_upr",
                                 secondary_upper = "sdate_upr",
                                 verbose = TRUE) {
  format <- match.arg(format)
  observations <- as.data.frame(data)
  dots <- list(...)

  # Interval-censored: lower bounds become the dates, upper bounds become
  # covariates (legacy behaviour, opt-in).
  if (format == "interval") {
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
      observations, dots = dots,
      event_date = primary, report_date = secondary,
      covariates = upper_bounds, data_type = "linelist"
    )
    .report_from(
      result, "epidist", verbose,
      extra = "format: interval (lower bounds -> dates, upper bounds -> covariates)"
    )
    return(result)
  }

  .assert_columns_present(observations, c(primary, secondary))

  # Aggregate (counts) -> count-incidence with case_count = n; linelist
  # otherwise.
  is_aggregate <- inherits(data, "epidist_aggregate_data") ||
    "n" %in% colnames(observations)
  count_col <- if (is_aggregate) "n" else NULL
  data_type <- if (is_aggregate) "count-incidence" else "linelist"

  # Infer the units from the primary censoring window unless the caller set them.
  inferred_units <- "days"
  if (primary_upper %in% colnames(observations)) {
    inferred_units <- .epidist_units_from_window(
      as.numeric(observations[[primary_upper]] - observations[[primary]])
    )
  }
  if (is.null(dots$event_units)) dots$event_units <- inferred_units
  if (is.null(dots$report_units)) dots$report_units <- dots$event_units

  # Decode left-censoring: a secondary window of the form [origin, report]
  # (lower bound at epidist time 0, i.e. the earliest event date) means the
  # report was only known up to its upper bound. Recover the report date and set
  # `is_censored` for those rows.
  is_censored_col <- NULL
  if (secondary_upper %in% colnames(observations) &&
      primary_upper %in% colnames(observations)) {
    origin <- min(observations[[primary]], na.rm = TRUE)
    window <- as.numeric(observations[[primary_upper]] - observations[[primary]])
    gap    <- as.numeric(observations[[secondary_upper]] - observations[[secondary]])
    censored <- (observations[[secondary]] == origin) & (gap > window)
    censored[is.na(censored)] <- FALSE
    if (any(censored)) {
      observations[[secondary]][censored] <- observations[[secondary_upper]][censored]
      observations[["is_censored"]] <- censored
      is_censored_col <- "is_censored"
    }
  }

  result <- .build_tbl_now(
    observations, dots = dots,
    event_date = primary, report_date = secondary,
    case_count = count_col, is_censored = is_censored_col,
    data_type = data_type
  )
  .report_from(
    result, "epidist", verbose,
    extra = paste0(
      "format: ", if (is_aggregate) "aggregate" else "linelist",
      " (lower bounds -> event/report dates", if (is_aggregate) ", n -> case_count",
      if (!is.null(is_censored_col)) ", left-censored windows -> is_censored", ")"
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
#' ts   <- tbl_now_to_tsibble(nowobj, verbose = FALSE)
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
                                  missing_reference = FALSE,
                                  preprocess = TRUE, verbose = TRUE,
                                  quiet = FALSE) {
  .assert_tbl_now(x, "tbl_now_to_epinowcast")
  .need_pkg("epinowcast")
  .warn_lossy_conversion("epinowcast", quiet)

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
    cli::cli_li("missing_reference: {.val {missing_reference}}")
    cli::cli_li("preprocess: {.val {preprocess}}")
    cli::cli_end()
  }

  # `missing_reference = FALSE` by default: a tbl_now never carries NA-reference
  # reports (they are dropped on the way in), so the epinowcast default of
  # synthesising padding rows for them would invent observations the data never
  # had. See the Round-trip section.
  completed <- epinowcast::enw_complete_dates(
    observations, by = grouping, max_delay = max_delay,
    missing_reference = missing_reference
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
tbl_now_to_baselinenowcast <- function(x, ..., format = c("matrix", "long"),
                                       delays_unit = "days", verbose = TRUE,
                                       quiet = FALSE) {
  .assert_tbl_now(x, "tbl_now_to_baselinenowcast")
  format <- match.arg(format)
  .warn_lossy_conversion("baselinenowcast", quiet)
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
  # censoring indicator included). Strip the tbl_now metadata first so the
  # resulting data.table carries none of it.
  data.table::as.data.table(.strip_tbl_now(x), ...)
}

#' @rdname tbl_now_epidist
#' @export
tbl_now_to_epidist <- function(x, ...,
                               format = c("auto", "linelist", "aggregate", "interval"),
                               primary_upper = NULL,
                               secondary_upper = NULL,
                               censoring_window = NULL,
                               verbose = TRUE, quiet = FALSE) {
  .assert_tbl_now(x, "tbl_now_to_epidist")
  .need_pkg("epidist")
  .warn_lossy_conversion("epidist", quiet)
  format <- match.arg(format)

  covariate_cols <- get_covariates(x)
  censored_col   <- get_is_censored(x)

  # --- Legacy "interval" branch: upper bounds taken from covariate columns. ---
  if (format == "interval") {
    if (is.null(primary_upper) || is.null(secondary_upper)) {
      cli::cli_abort(
        "For {.val interval} format, supply {.arg primary_upper} and \\
         {.arg secondary_upper} (covariate columns holding the upper bounds)."
      )
    }
    observations <- dplyr::as_tibble(x)
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
        pdate_lwr = .data[[get_event_date(x)]],
        pdate_upr = .data[[primary_upper]],
        sdate_lwr = .data[[get_report_date(x)]],
        sdate_upr = .data[[secondary_upper]]
      )
    carried_cols <- setdiff(c(covariate_cols, censored_col), upper_bounds)
    if (length(carried_cols) > 0) {
      epidist_data <- dplyr::bind_cols(
        epidist_data, dplyr::select(observations, dplyr::all_of(carried_cols))
      )
    }
    if (verbose) {
      cli::cli_h3("Converting {.cls tbl_now} into {.pkg epidist} interval data")
      cli::cli_ul()
      cli::cli_li("pdate_lwr/sdate_lwr <- dates, pdate_upr/sdate_upr <- covariates")
      if (length(carried_cols) > 0) {
        cli::cli_li("kept columns: {.val {carried_cols}}")
      }
      cli::cli_end()
    }
    return(do.call(
      epidist::as_epidist_linelist_data,
      c(list(epidist_data),
        list(pdate_lwr = "pdate_lwr", pdate_upr = "pdate_upr",
             sdate_lwr = "sdate_lwr", sdate_upr = "sdate_upr"),
        list(...))
    ))
  }

  # --- "auto"/"linelist"/"aggregate": build all four censoring windows. ---
  data_type <- get_data_type(x)
  is_count  <- data_type %in% c("count-incidence", "count-cumulative")

  if (format == "auto") {
    format <- if (is_count) "aggregate" else "linelist"
  } else if (format == "linelist" && is_count) {
    # A linelist has one row per case; count data belongs in the aggregate form.
    cli::cli_warn(
      "{.arg x} holds {.val {data_type}} counts; building an \\
       {.cls epidist_aggregate_data} (with {.code n = case_count}) instead of a \\
       linelist."
    )
    format <- "aggregate"
  } else if (format == "aggregate" && !is_count) {
    cli::cli_abort(
      "{.val aggregate} format needs count data, but {.arg x} is \\
       {.val {data_type}}. Use {.val linelist}."
    )
  }

  # epidist's `n` must be an incremental count; de-accumulate cumulative input.
  if (format == "aggregate" && data_type == "count-cumulative") {
    x <- to_count(x, to = "count-incidence")
  }

  event_col  <- get_event_date(x)
  report_col <- get_report_date(x)
  count_col  <- get_case_count(x)
  units      <- get_event_units(x)
  obs        <- dplyr::as_tibble(x)

  win <- if (is.null(censoring_window)) {
    .epidist_window_days(units)
  } else {
    censoring_window
  }
  # epidist time 0 is the earliest primary (event) date.
  origin <- min(obs[[event_col]], na.rm = TRUE)

  censored <- if (!is.null(censored_col)) {
    as.logical(obs[[censored_col]])
  } else {
    rep(FALSE, nrow(obs))
  }
  censored[is.na(censored)] <- FALSE

  # Primary event: [event_date, event_date + win]. Secondary event:
  # [report_date, report_date + win] normally, or the left-censored
  # [origin, report_date] when the row is flagged censored.
  epidist_data <- dplyr::tibble(
    pdate_lwr = obs[[event_col]],
    pdate_upr = obs[[event_col]] + win,
    sdate_lwr = dplyr::if_else(censored, origin, obs[[report_col]]),
    sdate_upr = dplyr::if_else(censored, obs[[report_col]], obs[[report_col]] + win)
  )
  # epidist requires strictly positive widths; a censored report sitting on the
  # origin would collapse to zero width.
  collapsed <- epidist_data$sdate_upr <= epidist_data$sdate_lwr
  epidist_data$sdate_lwr[collapsed] <- epidist_data$sdate_upr[collapsed] - win

  if (length(covariate_cols) > 0) {
    epidist_data <- dplyr::bind_cols(
      epidist_data, dplyr::select(obs, dplyr::all_of(covariate_cols))
    )
  }

  constructor_args <- list(
    pdate_lwr = "pdate_lwr", pdate_upr = "pdate_upr",
    sdate_lwr = "sdate_lwr", sdate_upr = "sdate_upr"
  )
  if (format == "aggregate") {
    epidist_data[["n"]] <- obs[[count_col]]
    constructor_args$n <- "n"
  }

  if (verbose) {
    cli::cli_h3("Converting {.cls tbl_now} into {.pkg epidist} {format} data")
    cli::cli_ul()
    cli::cli_li("pdate_lwr <- {.val {event_col}}, sdate_lwr <- {.val {report_col}}")
    cli::cli_li("censoring window: {.val {win}} day{?s} (from {.val {units}})")
    cli::cli_li("left-censored rows ({.field is_censored}): {.val {sum(censored)}}")
    if (format == "aggregate") cli::cli_li("n <- {.val {count_col}}")
    if (length(covariate_cols) > 0) {
      cli::cli_li("kept covariates: {.val {covariate_cols}}")
    }
    cli::cli_end()
  }

  constructor <- if (format == "aggregate") {
    epidist::as_epidist_aggregate_data
  } else {
    epidist::as_epidist_linelist_data
  }
  do.call(constructor, c(list(epidist_data), constructor_args, list(...)))
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
  # Strip the tbl_now metadata before building the tsibble (the package's dplyr
  # methods would otherwise propagate it onto the result).
  observations <- x |>
    .strip_tbl_now() |>
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
#' * `as_epidist_aggregate_data()` (\pkg{epidist}) wraps [tbl_now_to_epidist()]
#'   with `format = "aggregate"`.
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
#' @exportS3Method epidist::as_epidist_aggregate_data
as_epidist_aggregate_data.tbl_now <- function(data, ..., verbose = FALSE) {
  tbl_now_to_epidist(data, format = "aggregate", ..., verbose = verbose)
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
