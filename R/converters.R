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


# 1. Internal helpers-----

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
.need_pkg <- function(pkg, repo = NULL) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    # Most optional back-ends live on the epinowcast r-universe; `repo` lets a
    # caller point at a different one (e.g. nowcaster on covid19br).
    if (is.null(repo)) {
      repo <- c(epinowcast = "https://epinowcast.r-universe.dev")
    }
    install_call <- paste0(
      "install.packages(\"", pkg, "\", ",
      "repos = c(options('repos'), ",
      names(repo)[1], " = '", unname(repo)[1], "'))"
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
    "i" = "Some information might be dropped (e.g. covariate columns, \\
           maximum delays, grouping indices and padding rows).",
    "i" = "If you have the original data as a `tibble`, `data.frame` or \\
          `data.table`, prefer using that directly over converting \\
           from another format.",
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

#' Restore not-yet-observed (`NA`) cells onto a reporting-triangle matrix
#'
#' [baselinenowcast::as_reporting_triangle()] fills every cell inside the
#' inferred triangle with `0`, including the not-yet-observed cells that a
#' `tbl_now` carries as `NA`-count rows. This puts those `NA`s back, rebuilding
#' the object through [baselinenowcast::new_reporting_triangle()] so the
#' structure is validated once (per-cell `[<-` assignment would reject the
#' intermediate states).
#'
#' @param triangle A `reporting_triangle` matrix from `as_reporting_triangle()`.
#' @param na_long A data frame of the `NA`-count rows (`reference_date`,
#'   `report_date`), i.e. the cells to blank out.
#' @param days_per_unit Number of days per delay unit (1 for days, 7 for weeks).
#'
#' @return The `reporting_triangle` with the requested cells set to `NA`.
#'
#' @keywords internal
#' @noRd
.restore_reporting_triangle_na <- function(triangle, na_long, days_per_unit) {
  if (nrow(na_long) == 0) {
    return(triangle)
  }
  delays_unit <- attr(triangle, "delays_unit")
  reference_dates <- as.Date(rownames(triangle))

  m <- triangle
  attributes(m) <- list(dim = dim(triangle), dimnames = dimnames(triangle))

  row_index <- match(as.character(na_long$reference_date), rownames(m))
  col_index <- match(
    as.character(as.integer(
      (na_long$report_date - na_long$reference_date) / days_per_unit
    )),
    colnames(m)
  )
  ok <- !is.na(row_index) & !is.na(col_index)
  m[cbind(row_index[ok], col_index[ok])] <- NA

  baselinenowcast::new_reporting_triangle(m, reference_dates, delays_unit)
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

#' Materialise the lazy temporal-effect columns for an outgoing conversion
#'
#' Temporal effects are stored as a *lazy specification* (see
#' [compute_temporal_effects()]): the holiday / Fourier / calendar columns only
#' exist once that spec is materialised. When converting **out** to a format that
#' can carry extra columns we want those effects to ride along as covariates, so
#' this computes them on demand. If a spec is set but no columns have been
#' materialised yet it calls [compute_temporal_effects()] (with
#' `overwrite = TRUE`, since this is an export path and any pre-existing
#' columns of the same name are stale). It is a no-op when there is no spec, or
#' when the columns are already present.
#'
#' @param x A `tbl_now` object.
#'
#' @return A list with `x` (the `tbl_now`, possibly with the effect columns
#'   appended) and `cols` (a character vector of the temporal-effect column
#'   names, `character(0)` when there are none).
#'
#' @keywords internal
#' @noRd
.materialize_temporal_effects <- function(x) {
  cols <- get_temporal_effect_cols(x)
  if (length(cols) == 0 && length(get_temporal_effects(x)) > 0) {
    x <- compute_temporal_effects(x, overwrite = TRUE)
    cols <- get_temporal_effect_cols(x)
  }
  list(x = x, cols = cols)
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

  # One row per (reference date, delay) cell.
  long <- tidyr::expand_grid(
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
    )

  # The `NA` cells of a reporting triangle are of two kinds, split by the last
  # observed report date (the latest `report_date` with a non-`NA` count, i.e.
  # the nowcast `now`):
  #   * report_date >  now: not-yet-observable future cells. These are dropped
  #     entirely -- they are not observations, just the empty corner of the grid
  #     (a `tbl_now` carries `now`, so they are re-created on the way back).
  #   * report_date <= now: cells that *could* have been reported by now but
  #     were not. These are genuinely missing and kept as `count = NA` rows, so
  #     the `NA`-vs-`0` distinction survives the round-trip.
  observed_reports <- long$report_date[!is.na(long$count)]
  last_observed <- if (length(observed_reports) > 0) {
    max(observed_reports)
  } else {
    max(long$report_date)
  }

  long |>
    dplyr::filter(!is.na(.data$count) | .data$report_date <= last_observed) |>
    dplyr::select(dplyr::all_of(c("reference_date", "report_date", "count"))) |>
    as.data.frame()
}


# 2. tbl_now_from_*()  (package data -> tbl_now)-----


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
#'   If `NULL` it is inferred from the data as `max(.delay) + 1`. Because `.delay`
#'   is measured in the object's report units, this is only in `timestep`s when
#'   `timestep` matches those units — which is what the default infers.
#' @param timestep The \pkg{epinowcast} timestep: `"day"`, `"week"`, or a whole
#'   number of days. `NULL` (default) infers it from the object's report units
#'   (`"days"` -> `"day"`, `"weeks"` -> `"week"`), which keeps `max_delay` and the
#'   temporal-effect covariates on the same grid as the data. Other units cannot
#'   be inferred (\pkg{epinowcast} does not support calendar months) — pass a
#'   number of days explicitly, e.g. `timestep = 28`.
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
#'
#' * **Covariate columns** that are neither the core
#'   `reference_date`/`report_date`/`confirm`, a grouping (`by`) column, nor a
#'   materialised temporal-effect column are dropped. The temporal-effect
#'   columns (holidays, Fourier terms, calendar effects) *are* carried over: the
#'   lazy [temporal_effects()] spec is materialised with
#'   [compute_temporal_effects()] and the resulting columns are passed through to
#'   the observations and `metareference`/`metareport` tables.
#'
#' * **Grouping indices** (`.group`) are reassigned from the factor levels, so
#'   the row order of the nested tables can differ even though the underlying
#'   values match.
#'
#' * **NA-reference padding** is not regenerated by default (see
#'   `missing_reference`).
#'
#' * **`max_confirm`** (and the derived `cum_prop_reported`) will not match for
#'   reference dates whose reporting completes *after* `max_delay`. The modelled
#'   `confirm` (the reporting triangle) is truncated at `max_delay`, but
#'   epinowcast computes `max_confirm` as the eventual final total from the
#'   *untruncated* history. A `tbl_now` only stores the truncated triangle, so
#'   reports arriving beyond `max_delay` are gone: on the way back
#'   [epinowcast::enw_preprocess_data()] recomputes `max_confirm` from the
#'   within-window data and obtains a smaller value. The `confirm` counts
#'   themselves still round-trip exactly; only these truncation-derived summary
#'   columns differ. (For example, in `germany_covid19_hosp` the
#'   2021-04-06 / 00-04 cell reaches 7 by delay 40 but a final 11 only at
#'   delay 74, so its `max_confirm` is 11 in `pobs` and 7 after the round-trip.)
#'
#' @examplesIf requireNamespace("epinowcast", quietly = TRUE) & requireNamespace("data.table", quietly = TRUE)
#' # Preprocessing epinowcast data is slow, so this is wrapped in \donttest{}.
#' \donttest{
#' library(data.table)
#' library(epinowcast)
#'
#' #Read data from epinowcast
#' obs  <- germany_covid19_hosp[location == "DE"]
#'
#' #Remove unused column
#' obs  <- obs[, location := NULL]
#'
#' #Pre-process data
#' pobs <- epinowcast::enw_preprocess_data(obs, max_delay = 40, by = "age_group")
#'
#' # From the data.table input format ...
#' nowobj <- tbl_now_from_epinowcast(obs, strata = c("age_group"))
#'
#' # ... or from a preprocessed epinowcast object
#' tbl_epi <- tbl_now_from_epinowcast(pobs)
#'
#' #You can also convert to epinowcast preprocess data format
#' preprocessed_tbl <- tbl_now_to_epinowcast(tbl_epi, quiet = TRUE)
#' }
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
#' `tbl_now_to_baselinenowcast()` returns either a `reporting_triangle` matrix
#' (`format = "matrix"`, the default) via
#' [baselinenowcast::as_reporting_triangle()], or the long
#' `baselinenowcast`-style `data.frame` (`format = "long"`). The long format also
#' carries the **strata**, the
#' covariates, the censoring indicator and any materialised temporal-effect
#' columns (see [compute_temporal_effects()]); the matrix holds only the three
#' core columns. A single reporting-triangle matrix has no strata dimension, so
#' `format = "matrix"` **pools** any strata (summing the counts) with a warning;
#' use `format = "triangle_list"` to get one triangle per stratum instead.
#'
#' @param data A long `data.frame` or a `reporting_triangle` matrix.
#' @param x A `tbl_now` object.
#' @param reference_date,report_date,count Column names (long format only).
#' @param delays_unit Unit of the delay axis of the reporting triangle, one of
#'   `"days"` or `"weeks"`. Both directions default to `NULL`, meaning it is
#'   worked out for you. For `tbl_now_from_baselinenowcast()` that means reading
#'   the input matrix's own `delays_unit` attribute (falling back to `"days"`
#'   when it has none); a supplied value always wins. For
#'   `tbl_now_to_baselinenowcast()` (triangle formats only) it is **inferred**
#'   from the object's time units when the event and report units agree and are
#'   `"days"` or `"weeks"`; otherwise you must supply it explicitly.
#' @param format For `to`, one of:
#'   * `"matrix"` (default) -- a single [baselinenowcast::as_reporting_triangle()]
#'     matrix. A triangle has no strata dimension, so any strata are **pooled**
#'     (with a warning).
#'   * `"long"` -- a tidy data frame, which can also carry the strata,
#'     covariates, temporal-effect columns and the censoring indicator.
#'   * `"triangle_list"` -- one reporting triangle **per stratum**, as a
#'     [tbl_now_triangle_list]. Use this instead of pooling when you want a
#'     nowcast per stratum. With no strata attached the result is still a list,
#'     of length one and named `"all"`, so the return type never depends on
#'     whether strata happen to be present. Unlike splitting the long format
#'     yourself, the delay unit and the strata are taken from the object, and
#'     [as_tbl_now()] can rebuild a `tbl_now` from the result.
#' @param verbose Logical. Print the choices that were made.
#' @param ... Forwarded to [as_tbl_now()] (`from`) or
#'   [baselinenowcast::as_reporting_triangle()] (`to`, triangle formats).
#'
#' @return A `tbl_now` (`from`), or a `data.frame`, `reporting_triangle` or
#'   [tbl_now_triangle_list] (`to`), according to `format`.
#'
#' @section Round-trip:
#'
#' A `reporting_triangle` distinguishes **not-yet-observed** cells (`NA`) from
#' **observed zeros** (`0`). The `NA` cells split at the **last observed report
#' date** (the latest report with a non-`NA` count, taken as the nowcast's `now`):
#'
#' * cells with `report_date > now` are * not-yet-observable* future cells. They
#'   are **dropped** from the `tbl_now()`.
#' * cells with `report_date <= now` *could* have been reported but were not.
#'   They are genuinely **missing** and kept as `count = NA` rows in
#'   the `tbl_now()`.
#'
#' On the way back, [baselinenowcast::as_reporting_triangle()] fills the
#' in-triangle cells with `0` unless they are marked in the tibble as `NA`.
#'
#' @examplesIf requireNamespace("baselinenowcast", quietly = TRUE)
#' # Get a reporting triangle example
#' rt     <- baselinenowcast::example_reporting_triangle
#'
#' # Convert to a tbl_now
#' nowobj <- tbl_now_from_baselinenowcast(rt)
#'
#' # The matrix round-trip is faithful (not-yet-observed `NA` cells are kept).
#' identical(rt, tbl_now_to_baselinenowcast(nowobj))
#' @name tbl_now_baselinenowcast
#' @export
tbl_now_from_baselinenowcast <- function(data, ...,
                                         reference_date = "reference_date",
                                         report_date = "report_date",
                                         count = "count",
                                         delays_unit = NULL,
                                         verbose = TRUE) {
  dots <- list(...)

  # A `reporting_triangle` records its own delay unit. Honour it unless the
  # caller says otherwise: reading the delays as days when the triangle is
  # weekly places the report dates a seventh of the way along, which made
  # `as_tbl_now()` abort outright on any weekly triangle ("report_units must be
  # coarser than or equal to event_units").
  if (is.null(delays_unit)) {
    delays_unit <- attr(data, "delays_unit") %||% "days"
  }

  # A reporting-triangle matrix is expanded to long incremental form; a long
  # data frame is selected and renamed to the canonical column names.
  if (is.matrix(data) || inherits(data, "reporting_triangle")) {
    long_data <- .reporting_triangle_to_long(data, delays_unit = delays_unit)
    extra_message <- "expanded a reporting-triangle matrix to long counts"
    # The last observed report date is the nowcast `now` (future cells were
    # dropped above, so it is the latest report still present).
    if (is.null(dots$now) && nrow(long_data) > 0) {
      dots$now <- max(long_data$report_date)
    }
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
    dots = dots,
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
#' `data.table` keeping every column; any lazy temporal effects are materialised
#' first (see [compute_temporal_effects()]) so their columns are present.
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
#'   from the primary censoring-window width (a 7-day window -> `"weeks"`), and a
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
#'   `w` matches the `tbl_now` unit (`"days"` -> 1 day, `"weeks"` -> 7 days, ...,
#'   or `censoring_window` if supplied);
#' * the secondary event spans `[report_date, report_date + w]` normally, but
#'   for rows flagged by `is_censored` it is left-censored to
#'   `[origin, report_date]` (with `origin` the earliest `event_date`, i.e.
#'   epidist time 0) — encoding the `tbl_now` convention that a censored report
#'   is only known to lie in `[0, report_date]`.
#'
#' The strata, the covariate columns and any materialised temporal-effect columns
#' (holidays, Fourier terms, calendar effects; see [compute_temporal_effects()])
#' are carried onto the epidist data unchanged, so the strata are available as
#' covariates in an epidist model formula (epidist has no separate grouping
#' argument).
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
#' # --- Linelist epidist data (one row per case) ---
#' ll <- epidist::as_epidist_linelist_data(
#'   data.frame(
#'     pdate_lwr = as.Date(c("2020-03-01", "2020-03-02", "2020-03-02")),
#'     sdate_lwr = as.Date(c("2020-03-05", "2020-03-04", "2020-03-06"))
#'   ),
#'   pdate_lwr = "pdate_lwr", sdate_lwr = "sdate_lwr"
#' )
#' # -> a linelist tbl_now ...
#' nowll <- tbl_now_from_epidist(ll)
#' get_data_type(nowll)
#' # ... and back to an epidist_linelist_data
#' tbl_now_to_epidist(nowll)
#'
#' # --- Aggregate epidist data (counts in an `n` column) ---
#' agg <- epidist::as_epidist_aggregate_data(
#'   data.frame(
#'     pdate_lwr = as.Date(c("2020-03-01", "2020-03-02")),
#'     sdate_lwr = as.Date(c("2020-03-05", "2020-03-04")),
#'     n = c(7, 3)
#'   ),
#'   n = "n", pdate_lwr = "pdate_lwr", sdate_lwr = "sdate_lwr"
#' )
#' # -> a count-incidence tbl_now (case_count = "n") ...
#' nowagg <- tbl_now_from_epidist(agg)
#' get_data_type(nowagg)
#' # ... and back to an epidist_aggregate_data (auto-detected from the counts)
#' tbl_now_to_epidist(nowagg)
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
#' columns: the `index` is the **event date** and the report date (plus any
#' strata) becomes part of the `key`.
#'
#' `tbl_now_from_tsibble()` converts a `tbl_ts` into a `tbl_now`. You must say
#' which column is the `report_date`; `event_date` defaults to the tsibble's
#' index ([tsibble::index_var()]).
#'
#' `tbl_now_to_tsibble()` converts a `tbl_now` into a `tbl_ts`, using `index`
#' (`"event_date"`, the default, or `"report_date"`) as the tsibble index and
#' the other date plus the strata as the key. Linelist data is aggregated to
#' `count-incidence` first (a tsibble requires unique index/key combinations).
#' The covariates, the censoring indicator and any materialised temporal-effect
#' columns (see [compute_temporal_effects()]) ride along as measurement columns.
#'
#' @param data A `tbl_ts` (tsibble).
#' @param x A `tbl_now` object.
#' @param event_date Column name of the event date (for `from`); defaults to the
#'   tsibble index.
#' @param report_date Column name of the report date (required for `from`).
#' @param strata Optional character vector of strata columns (`from`). If `NULL`
#'   (default) the tsibble key columns other than the date columns are used.
#' @param index For `to`: which date becomes the tsibble index, `"event_date"`
#'   (default) or `"report_date"`.
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
#' # The tsibble is indexed by the event date; the report date is in the key.
#' ts   <- tbl_now_to_tsibble(nowobj, verbose = FALSE)
#' back <- tbl_now_from_tsibble(ts, report_date = "report_week", verbose = FALSE)
#' @name tbl_now_tsibble
#' @export
tbl_now_from_tsibble <- function(data, report_date, event_date = NULL,
                                 strata = NULL, ..., verbose = TRUE) {
  .need_pkg("tsibble")
  if (missing(report_date) || is.null(report_date)) {
    cli::cli_abort("Please supply the {.arg report_date} column name.")
  }

  # `event_date` defaults to the tsibble index.
  if (is.null(event_date)) {
    if (!inherits(data, "tbl_ts")) {
      cli::cli_abort(
        "{.arg event_date} is required when {.arg data} is not a tsibble."
      )
    }
    event_date <- tsibble::index_var(data)
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
    extra = paste0("event_date taken from the tsibble index: ", event_date)
  )
  result
}

# 3. tbl_now_to_*()  (tbl_now -> package object)------

#' The `epinowcast` timestep implied by a `tbl_now`'s report units
#'
#' `epinowcast` measures `max_delay`, and lays out its reference/report date
#' grids, in `timestep`s. A `tbl_now`'s `.delay` is in report units, so the two
#' agree only when the timestep *is* the report unit; leaving `epinowcast` on its
#' `"day"` default would read a delay of 14 weeks as 14 days and expand the
#' metadata tables to a daily grid the weekly covariates have no values on.
#'
#' @param report_units The object's report units (see [get_report_units()]).
#'
#' @return `"day"` or `"week"`; aborts for units `epinowcast` cannot express.
#'
#' @keywords internal
#' @noRd
.epinowcast_timestep <- function(report_units) {
  switch(report_units,
    days  = "day",
    weeks = "week",
    cli::cli_abort(c(
      "Can't infer an {.pkg epinowcast} {.arg timestep} from report units \\
       {.val {report_units}}.",
      "i" = "{.pkg epinowcast} accepts {.val day}, {.val week}, or a whole \\
             number of days; it does not support calendar months.",
      "i" = "Pass one explicitly, e.g. {.code timestep = 28} for monthly data."
    ))
  )
}

#' Derive the temporal-effect columns on epinowcast's completed date grid
#'
#' [epinowcast::enw_complete_dates()] fills in every (reference, report) cell the
#' data never had, and it extends the reference axis past the data into the
#' nowcast horizon. It knows only its own schema, so any other column — including
#' the temporal effects — comes back `NA` on every row it adds. Carrying the
#' effect columns *through* the completion therefore strands them on the original
#' rows only, which is the wrong way round: the horizon dates are precisely the
#' ones a nowcast has to predict, and so precisely the ones the covariates must
#' cover.
#'
#' The effects are functions of a date alone, so deriving them *from* the
#' completed grid gives every row a real value.
#'
#' @param completed The `data.table` returned by
#'   [epinowcast::enw_complete_dates()].
#' @param x The `tbl_now` the spec belongs to.
#'
#' @return A list with `data` (`completed` plus the effect columns) and `cols`
#'   (their names).
#'
#' @keywords internal
#' @noRd
.epinowcast_temporal_effects <- function(completed, x) {
  specs <- get_temporal_effects(x)
  if (length(specs) == 0) {
    return(list(data = completed, cols = character(0)))
  }

  # epinowcast hands back data.table `IDate`s, which almanac refuses ("Can't
  # convert `x` <IDate> to <date>"), so derive on plain Dates. epinowcast's own
  # columns are left exactly as they were and the effects appended to them.
  on_grid <- as.data.frame(completed)
  date_cols <- intersect(c("reference_date", "report_date"), names(on_grid))
  on_grid[date_cols] <- lapply(on_grid[date_cols], as.Date)
  derived_from <- names(on_grid)

  # Fourier terms are computed from `.event_num` / `.report_num`, which
  # `time_cols_to_numeric()` anchors at the earliest event date. Completion only
  # ever extends the grid forward — it takes its `min_date` from the observations
  # — so re-deriving them here reproduces the object's own anchor, and the
  # seasonal phase carries onto the horizon dates unchanged.
  on_grid <- time_cols_to_numeric(
    on_grid,
    event_date   = "reference_date",
    report_date  = "report_date",
    event_units  = get_event_units(x),
    report_units = get_report_units(x),
    force = TRUE
  )

  for (spec in specs) {
    from_event_date <- identical(spec$date_type, "event_date")
    on_grid <- add_temporal_effects.data.frame(
      on_grid,
      t_effects    = spec$t_effects,
      date_col     = if (from_event_date) "reference_date" else "report_date",
      numeric_col  = if (from_event_date) ".event_num" else ".report_num",
      name_prefix  = if (from_event_date) ".event" else ".report",
      overwrite    = TRUE,
      weekend_days = spec$weekend_days
    )
  }

  # `.event_num` / `.report_num` / `.delay` are scaffolding for the Fourier
  # terms, not covariates, and epinowcast derives its own delay index.
  scaffolding <- c(".event_num", ".report_num", ".delay")
  effect_cols <- setdiff(names(on_grid), c(derived_from, scaffolding))

  list(
    data = data.table::as.data.table(
      dplyr::bind_cols(as.data.frame(completed), on_grid[effect_cols])
    ),
    cols = effect_cols
  )
}

#' @rdname tbl_now_epinowcast
#' @export
tbl_now_to_epinowcast <- function(x, ..., max_delay = NULL,
                                  timestep = NULL,
                                  missing_reference = FALSE,
                                  preprocess = TRUE, verbose = TRUE,
                                  quiet = FALSE) {
  .assert_tbl_now(x, "tbl_now_to_epinowcast")
  .need_pkg("epinowcast")
  .warn_lossy_conversion("epinowcast", quiet)

  # Both epinowcast calls below must agree on the timestep: completing the dates
  # on a daily grid and then preprocessing weekly is an error, and completing
  # daily and preprocessing daily silently puts weekly data on the wrong grid.
  if (is.null(timestep)) {
    timestep <- .epinowcast_timestep(get_report_units(x))
  }

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
  # is_censored has no place in it. The temporal effects are deliberately *not*
  # carried here: they are added after the dates are completed, so they also
  # cover the rows completion adds (see `.epinowcast_temporal_effects()`).
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

  # `missing_reference = FALSE` by default: a tbl_now never carries NA-reference
  # reports (they are dropped on the way in), so the epinowcast default of
  # synthesising padding rows for them would invent observations the data never
  # had. See the Round-trip section.
  completed <- epinowcast::enw_complete_dates(
    observations, by = grouping, max_delay = max_delay,
    missing_reference = missing_reference, timestep = timestep
  )
  with_effects  <- .epinowcast_temporal_effects(completed, x)
  completed     <- with_effects$data
  temporal_cols <- with_effects$cols

  if (verbose) {
    cli::cli_h3("Converting {.cls tbl_now} into an {.pkg epinowcast} object")
    cli::cli_ul()
    cli::cli_li("reference_date <- {.val {event_col}}")
    cli::cli_li("report_date <- {.val {report_col}}")
    cli::cli_li("confirm <- {.val {count_col}}")
    cli::cli_li("by: {.val {if (is.null(grouping)) 'none' else grouping}}")
    if (length(temporal_cols) > 0) {
      cli::cli_li("temporal effects: {.val {temporal_cols}}")
    }
    cli::cli_li("timestep: {.val {timestep}}")
    cli::cli_li("max_delay: {.val {max_delay}} {.emph {timestep}{?s}}")
    cli::cli_li("missing_reference: {.val {missing_reference}}")
    cli::cli_li("preprocess: {.val {preprocess}}")
    cli::cli_end()
  }

  if (!preprocess) {
    return(completed)
  }

  epinowcast::enw_preprocess_data(
    completed, by = grouping, max_delay = max_delay, timestep = timestep, ...
  )
}

#' Resolve the reporting-triangle delay unit for `tbl_now_to_baselinenowcast()`
#'
#' When `delays_unit` is `NULL` it is inferred from the object's time units: this
#' is only well defined when the event and report units agree and are either
#' `"days"` or `"weeks"` (the units \pkg{baselinenowcast}'s delay axis
#' understands). Otherwise the user must supply it.
#'
#' @param x A `tbl_now` object.
#' @param delays_unit The user-supplied value, or `NULL` to infer.
#'
#' @return A single string, `"days"` or `"weeks"` (or the user's value verbatim).
#'
#' @keywords internal
#' @noRd
.baselinenowcast_delays_unit <- function(x, delays_unit) {
  if (!is.null(delays_unit)) {
    return(delays_unit)
  }
  event_units <- get_event_units(x)
  report_units <- get_report_units(x)
  if (identical(event_units, report_units) && event_units %in% c("days", "weeks")) {
    return(event_units)
  }
  cli::cli_abort(c(
    "Could not infer {.arg delays_unit} for the {.pkg baselinenowcast} \\
     reporting triangle.",
    "i" = "It is inferred only when {.field event_units} \\
           ({.val {event_units}}) and {.field report_units} \\
           ({.val {report_units}}) are equal and either {.val days} or \\
           {.val weeks}.",
    "i" = "Please supply {.arg delays_unit} explicitly, e.g. \\
           {.code delays_unit = \"weeks\"}."
  ))
}

#' @rdname tbl_now_baselinenowcast
#' @export
tbl_now_to_baselinenowcast <- function(x, ...,
                                       format = c("matrix", "long", "triangle_list"),
                                       delays_unit = NULL, verbose = TRUE) {
  .assert_tbl_now(x, "tbl_now_to_baselinenowcast")
  format <- match.arg(format)

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

  # Materialise the lazy temporal-effect columns so the long format can carry
  # them as covariates (holidays, Fourier terms, calendar effects).
  materialised   <- .materialize_temporal_effects(x)
  x              <- materialised$x
  temporal_cols  <- materialised$cols

  event_col      <- get_event_date(x)
  report_col     <- get_report_date(x)
  count_col      <- get_case_count(x)
  covariate_cols <- get_covariates(x)
  censored_col   <- get_is_censored(x)
  strata_cols    <- get_strata(x)

  # The long format is a tidy data frame, so it can also carry the strata, the
  # covariates, the temporal-effect columns and the censoring indicator (a user
  # can then build one triangle per stratum). The reporting-triangle *matrix* has
  # no strata dimension, so only the three core columns are kept for it and the
  # strata are pooled below.
  extra_cols <- if (format == "long") {
    c(strata_cols, covariate_cols, temporal_cols, censored_col)
  } else if (format == "triangle_list") {
    # Only the strata are needed: they say how to split. Everything else is
    # dropped, because a triangle has nowhere to put it.
    strata_cols
  } else {
    NULL
  }

  long_data <- x |>
    dplyr::as_tibble() |>
    dplyr::select(
      reference_date = dplyr::all_of(event_col),
      report_date    = dplyr::all_of(report_col),
      count          = dplyr::all_of(count_col),
      dplyr::all_of(extra_cols)
    )

  # A single reporting triangle cannot hold strata: with strata present the long
  # data has one row per (reference_date, report_date, stratum), which
  # `as_reporting_triangle()` rejects as duplicate cells. Pool the strata (summing
  # the counts, keeping a cell NA only when every stratum is NA) and warn.
  if (format == "matrix" && length(strata_cols) > 0) {
    cli::cli_warn(c(
      "{.pkg baselinenowcast} builds a single reporting triangle, which has no \\
       strata dimension; pooling over strata {.val {strata_cols}}.",
      "i" = "For a nowcast per stratum use {.code format = \"triangle_list\"}, \\
             which returns one triangle per stratum."
    ))
    long_data <- long_data |>
      dplyr::group_by(.data$reference_date, .data$report_date) |>
      dplyr::summarise(
        count = if (all(is.na(.data$count))) NA_real_ else sum(.data$count, na.rm = TRUE),
        .groups = "drop"
      )
  }

  # `delays_unit` applies to both triangle formats (the long format has no delay
  # axis). When `NULL` it is inferred from the object's time units (equal
  # event/report units of days or weeks); otherwise the user must supply it.
  if (format %in% c("matrix", "triangle_list")) {
    delays_unit <- .baselinenowcast_delays_unit(x, delays_unit)
  }

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
    if (format %in% c("matrix", "triangle_list")) {
      cli::cli_li("delays_unit: {.val {delays_unit}}")
    }
    cli::cli_end()
  }

  if (format == "long") {
    return(as.data.frame(long_data))
  }

  if (format == "triangle_list") {
    .need_pkg("baselinenowcast")

    # Split into one group per observed strata combination. With no strata the
    # result is still a list -- of length one, named "all" (the same convention
    # `test_delay_drift()` uses) -- so the return type never depends on whether
    # strata happen to be attached.
    if (length(strata_cols) > 0) {
      strata_frame <- as.data.frame(long_data)[, strata_cols, drop = FALSE]
      labels <- do.call(paste, c(unname(as.list(strata_frame)), sep = " | "))
    } else {
      strata_frame <- NULL
      labels <- rep("all", nrow(long_data))
    }
    group_rows <- split(seq_len(nrow(long_data)), labels)

    triangles <- lapply(group_rows, function(rows) {
      core <- as.data.frame(long_data)[
        rows, c("reference_date", "report_date", "count"), drop = FALSE
      ]
      .tbl_now_one_triangle(core, delays_unit = delays_unit, ...)
    })

    # Keep the strata VALUES, one row per element, rather than parsing them back
    # out of the label: a stratum containing the separator would not round-trip.
    strata_values <- if (is.null(strata_frame)) {
      NULL
    } else {
      do.call(
        rbind,
        lapply(group_rows, function(rows) strata_frame[rows[1], , drop = FALSE])
      )
    }
    if (!is.null(strata_values)) rownames(strata_values) <- NULL

    return(structure(
      triangles,
      class         = "tbl_now_triangle_list",
      strata_cols   = strata_cols,
      strata_values = strata_values,
      now           = get_now(x),
      event_col     = event_col,
      report_col    = report_col,
      delays_unit   = delays_unit
    ))
  }

  .need_pkg("baselinenowcast")
  triangle <- .tbl_now_one_triangle(
    as.data.frame(long_data), delays_unit = delays_unit, ...
  )
  return(triangle)
}

#' Build one reporting triangle from canonical long counts
#'
#' Shared by `format = "matrix"` and `format = "triangle_list"` so both restore
#' the not-yet-observed cells to `NA` in exactly the same way.
#'
#' @param core A data frame with `reference_date`, `report_date` and `count`.
#' @param delays_unit Delay unit passed to
#'   [baselinenowcast::as_reporting_triangle()].
#' @param ... Forwarded to [baselinenowcast::as_reporting_triangle()].
#'
#' @return A `reporting_triangle` matrix.
#'
#' @keywords internal
#' @noRd
.tbl_now_one_triangle <- function(core, delays_unit, ...) {
  triangle <- baselinenowcast::as_reporting_triangle(
    core, delays_unit = delays_unit, ...
  )

  # `as_reporting_triangle()` fills every in-triangle cell with 0; restore the
  # not-yet-observed cells (carried as NA-count rows) back to NA so the
  # NA-vs-0 distinction is preserved.
  days_per_unit <- switch(delays_unit,
    days = 1, weeks = 7, months = 30, years = 365, 1
  )
  na_long <- core[is.na(core$count), c("reference_date", "report_date")]
  .restore_reporting_triangle_na(triangle, as.data.frame(na_long), days_per_unit)
}

# #' Convert a `tbl_now` into \pkg{EpiNow2} input
# #'
# #' `r lifecycle::badge("experimental")`
# #'
# #' @description
# #' \pkg{EpiNow2}'s renewal-equation models ([EpiNow2::estimate_infections()],
# #' [EpiNow2::epinow()]) take a **single incidence time series** (`date`,
# #' `confirm`) with no report/delay dimension. For these
# #' (`model = "estimate_infections"`, the default) the `tbl_now` is collapsed to
# #' that single series using the most recently reported counts per `event_date`
# #' (equivalent to [get_latest_reported_cases()]).
# #'
# #' [EpiNow2::estimate_truncation()] is different: it takes **multiple
# #' snapshots** of the same series observed at successive report dates and *does*
# #' use the report dimension. With `model = "estimate_truncation"` the `tbl_now`
# #' is turned into that list of `date`/`confirm` snapshots (one per report date,
# #' earliest first) — precisely the information the report dimension encodes.
# #'
# #' There is intentionally **no** `tbl_now_from_EpiNow2()`: neither a single time
# #' series nor a set of snapshots can, in general, reconstruct the full
# #' event/report structure of a `tbl_now`.
# #'
# #' @param x A `tbl_now` object.
# #' @param model Which \pkg{EpiNow2} model the output targets:
# #'   `"estimate_infections"` (default; a single `date`/`confirm` series for
# #'   [EpiNow2::estimate_infections()] / [EpiNow2::epinow()]) or
# #'   `"estimate_truncation"` (a list of `date`/`confirm` snapshots for
# #'   [EpiNow2::estimate_truncation()]).
# #' @param verbose Logical. Print the choices that were made.
# #' @param ... Forwarded to [data.table::as.data.table()].
# #'
# #' @return For `"estimate_infections"`, a `data.table` with columns `date` and
# #'   `confirm`. For `"estimate_truncation"`, a list of such `data.table`s
# #'   ordered from earliest to latest report snapshot.
# #'
# #' @examplesIf requireNamespace("data.table", quietly = TRUE)
# #' data(mpoxdat)
# #' nowobj <- tbl_now(mpoxdat,
# #'   event_date = "dx_date", report_date = "dx_report_date",
# #'   case_count = "n", strata = "race",
# #'   data_type = "count-incidence", verbose = FALSE
# #' )
# #' # single series for estimate_infections()
# #' tbl_now_to_EpiNow2(nowobj, verbose = FALSE)
# #'
# #' # report-date snapshots for estimate_truncation()
# #' tbl_now_to_EpiNow2(nowobj, model = "estimate_truncation", verbose = FALSE)
# #' @export
# # `tbl_now_to_EpiNow2` is named after the EpiNow2 package (not snake_case).
# tbl_now_to_EpiNow2 <- function( # nolint: object_name_linter.
#     x, ...,
#     model = c("estimate_infections", "estimate_truncation"),
#     verbose = TRUE) {
#   .assert_tbl_now(x, "tbl_now_to_EpiNow2")
#   .need_pkg("data.table")
#   model <- match.arg(model)
#
#   event_col   <- get_event_date(x)
#   strata_cols <- get_strata(x)
#
#   # Cumulative cases for each event date, by report date and strata. Coercing to
#   # cumulative works for any input type (linelist / incidence / cumulative).
#   cumulative <- x |>
#     dplyr::ungroup() |>
#     to_count(to = "count-cumulative")
#   count_col <- get_case_count(cumulative)
#
#   known <- cumulative |>
#     dplyr::as_tibble() |>
#     dplyr::select(
#       date        = dplyr::all_of(event_col),
#       report_date = dplyr::all_of(get_report_date(cumulative)),
#       confirm     = dplyr::all_of(count_col),
#       dplyr::all_of(strata_cols)
#     )
#
#   # The incidence series as known at report date `as_of`: per (date, strata)
#   # take the latest report on or before `as_of`, then sum over strata.
#   build_snapshot <- function(as_of) {
#     known |>
#       dplyr::filter(.data$report_date <= as_of) |>
#       dplyr::group_by(dplyr::across(dplyr::all_of(c("date", strata_cols)))) |>
#       dplyr::slice_max(.data$report_date, n = 1, with_ties = FALSE) |>
#       dplyr::group_by(.data$date) |>
#       dplyr::summarise(
#         confirm = sum(.data$confirm, na.rm = TRUE), .groups = "drop"
#       ) |>
#       dplyr::arrange(.data$date)
#   }
#
#   # estimate_infections: a single series at the current `now`.
#   if (model == "estimate_infections") {
#     series <- build_snapshot(get_now(x))
#     if (verbose) {
#       cli::cli_h3("Converting {.cls tbl_now} to an {.pkg EpiNow2} series")
#       cli::cli_ul()
#       cli::cli_li("date <- {.val {event_col}} (event_date)")
#       cli::cli_li("confirm <- latest reported counts per date")
#       cli::cli_li("rows: {.val {nrow(series)}}")
#       cli::cli_alert_info(
#         "For {.fn estimate_infections} the report dimension is collapsed \\
#          to a single series."
#       )
#       cli::cli_end()
#     }
#     return(data.table::as.data.table(series, ...))
#   }
#
#   # estimate_truncation: one snapshot per distinct report date (earliest first).
#   report_dates <- sort(unique(known$report_date))
#   snapshots <- lapply(report_dates, function(report_date) {
#     data.table::as.data.table(build_snapshot(report_date), ...)
#   })
#
#   if (verbose) {
#     cli::cli_h3(
#       "Converting {.cls tbl_now} into {.pkg EpiNow2} truncation snapshots"
#     )
#     cli::cli_ul()
#     cli::cli_li("date <- {.val {event_col}} (event_date)")
#     cli::cli_li("snapshots: {.val {length(snapshots)}} (one per report date)")
#     cli::cli_alert_info("Pass the list to {.fn EpiNow2::estimate_truncation}.")
#     cli::cli_end()
#   }
#
#   snapshots
# }

#' @rdname tbl_now_data_table
#' @export
tbl_now_to_data_table <- function(x, ..., verbose = TRUE) {
  .assert_tbl_now(x, "tbl_now_to_data_table")
  .need_pkg("data.table")

  # Materialise the lazy temporal-effect columns so they are present as ordinary
  # columns (a data.table keeps every column, but the lazy spec produces none
  # until it is computed).
  x <- .materialize_temporal_effects(x)$x

  if (verbose) {
    cli::cli_h3("Converting {.cls tbl_now} into a {.cls data.table}")
    cli::cli_alert_info(
      "tbl_now attributes are dropped; every column is kept (including the \\
       generated .delay / .event_num / .report_num, the covariates, the \\
       temporal-effect columns and is_censored)."
    )
  }

  # A data.table can hold every column, so keep them all (covariates, the
  # temporal-effect columns and the censoring indicator included). Strip the
  # tbl_now metadata first so the resulting data.table carries none of it.
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
  #.warn_lossy_conversion("epidist", quiet)
  format <- match.arg(format)

  # Materialise the lazy temporal-effect columns so they are carried as extra
  # covariate columns on the epidist data.
  materialised   <- .materialize_temporal_effects(x)
  x              <- materialised$x
  temporal_cols  <- materialised$cols

  covariate_cols <- get_covariates(x)
  censored_col   <- get_is_censored(x)
  strata_cols    <- get_strata(x)

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
    carried_cols <- setdiff(
      c(strata_cols, covariate_cols, temporal_cols, censored_col), upper_bounds
    )
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

  # Strata are carried as ordinary columns so they can be used as covariates in an
  # epidist model formula (epidist estimates the delay distribution; it has no
  # dedicated grouping argument, so the strata travel as data columns).
  carry_cols <- c(strata_cols, covariate_cols, temporal_cols)
  if (length(carry_cols) > 0) {
    epidist_data <- dplyr::bind_cols(
      epidist_data, dplyr::select(obs, dplyr::all_of(carry_cols))
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
    if (length(carry_cols) > 0) {
      cli::cli_li("kept columns (strata/covariates): {.val {carry_cols}}")
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
tbl_now_to_tsibble <- function(x, ..., index = c("event_date", "report_date"),
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

  # Materialise the lazy temporal-effect columns so they ride along as
  # measurement columns (holidays, Fourier terms, calendar effects).
  materialised   <- .materialize_temporal_effects(x)
  x              <- materialised$x
  temporal_cols  <- materialised$cols

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

  # Covariates, the temporal-effect columns, the censoring indicator and the
  # case count ride along as measurement columns; the tbl_now internals are
  # dropped.
  kept_cols <- c(
    index_col, other_col, strata_cols, covariate_cols, temporal_cols,
    censored_col, count_col
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



# -- shared helper for the line-list back-ends ---------------------------------

#' Reduce a `tbl_now` to a one-row-per-case line list
#'
#' \pkg{surveillance} and \pkg{nowcaster} both model an individual-level line
#' list. A `linelist` `tbl_now` already is one; count data has to be expanded
#' back out, which is only meaningful for *incidence* counts, so cumulative data
#' is de-accumulated first.
#'
#' @param x A `tbl_now` object.
#' @param fn Calling function, for error messages.
#'
#' @return A plain data frame with one row per case.
#'
#' @keywords internal
#' @noRd
.tbl_now_expand_to_linelist <- function(x, fn) {
  if (get_data_type(x) == "linelist") {
    return(.strip_tbl_now(x))
  }

  # Cumulative counts are not case counts; de-accumulate before expanding.
  if (get_data_type(x) == "count-cumulative") {
    x <- to_count(x, to = "count-incidence")
  }

  count_col <- get_case_count(x)
  cli::cli_warn(
    "{.fn {fn}} needs a line list; expanding {.val {get_data_type(x)}} counts \\
     in {.val {count_col}} to one row per case."
  )

  observations <- .strip_tbl_now(x)
  counts <- observations[[count_col]]

  # De-accumulation can yield negative increments on a downward revision, and
  # tidyr::uncount() errors on those. Drop them explicitly rather than failing.
  negative <- !is.na(counts) & counts < 0
  if (any(negative)) {
    cli::cli_warn(
      "Dropping {sum(negative)} row{?s} with a negative count (a downward \\
       revision cannot be expanded into cases)."
    )
    observations <- observations[!negative, , drop = FALSE]
  }
  observations <- observations[!is.na(observations[[count_col]]), , drop = FALSE]

  tidyr::uncount(observations, weights = !!rlang::sym(count_col))
}


#' Convert a `tbl_now` into the line list \pkg{surveillance} nowcasts from
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [surveillance::nowcast()] works from an individual-level line list with one
#' column holding the event date and another the report date, named by its
#' `dEventCol` / `dReportCol` arguments. `tbl_now_to_surveillance()` produces
#' exactly that data frame, renaming the two dates to \pkg{surveillance}'s own
#' defaults so the result can be passed straight through.
#'
#' With `format = "sts"` it instead returns the observed epidemic curve as an
#' [surveillance::sts] object via [surveillance::linelist2sts()], which is what
#' \pkg{surveillance}'s plotting and outbreak-detection verbs consume.
#'
#' `now` and the delay unit are *not* baked into the result: pass them from the
#' object with [get_now()] and [get_event_units()], as in the example below.
#'
#' @param x A `tbl_now` object.
#' @param event_col,report_col Names to give the event and report date columns
#'   in the result. Default to \pkg{surveillance}'s own `"dHospital"` and
#'   `"dReport"`, so [surveillance::nowcast()] finds them without further
#'   arguments.
#' @param format `"linelist"` (default) for the data frame
#'   [surveillance::nowcast()] expects, or `"sts"` for an
#'   [surveillance::sts] object of the observed curve.
#' @param aggregate_by Aggregation interval for `format = "sts"`, e.g.
#'   `"1 week"`. `NULL` (default) derives it from the object's event units.
#' @param verbose Logical. Print the choices that were made.
#' @param ... Forwarded to [surveillance::linelist2sts()] when
#'   `format = "sts"`; ignored otherwise.
#'
#' @return A `data.frame` line list (`format = "linelist"`) or an
#'   [surveillance::sts] object (`format = "sts"`).
#'
#' @seealso [tbl_now_to_nowcaster()], [tbl_now_to_epinowcast()]
#'
#' @examplesIf requireNamespace("surveillance", quietly = TRUE)
#' data(denguedat)
#' nowobj <- tbl_now(denguedat,
#'   event_date = "onset_week", report_date = "report_week", verbose = FALSE
#' )
#' sur <- tbl_now_to_surveillance(nowobj, verbose = FALSE)
#' head(sur)
#'
#' # `now` and the aggregation unit come from the object itself:
#' get_now(nowobj)
#'
#' @name tbl_now_surveillance
#' @export
tbl_now_to_surveillance <- function(x, ..., event_col = "dHospital",
                                    report_col = "dReport",
                                    format = c("linelist", "sts"),
                                    aggregate_by = NULL, verbose = TRUE) {
  .assert_tbl_now(x, "tbl_now_to_surveillance")
  .need_pkg("surveillance")
  format <- match.arg(format)

  event_date_col  <- get_event_date(x)
  report_date_col <- get_report_date(x)
  strata_cols     <- get_strata(x)
  covariate_cols  <- get_covariates(x)

  # Temporal effects ride along as ordinary columns so they can be used in a
  # per-stratum loop or a downstream model formula.
  materialised  <- .materialize_temporal_effects(x)
  x             <- materialised$x
  temporal_cols <- materialised$cols

  linelist <- .tbl_now_expand_to_linelist(x, "tbl_now_to_surveillance")

  kept <- c(
    event_date_col, report_date_col, strata_cols, covariate_cols, temporal_cols
  )
  linelist <- as.data.frame(linelist[, kept, drop = FALSE])

  # surveillance indexes by name, so rename the two dates to what it expects.
  names(linelist)[match(event_date_col, names(linelist))]  <- event_col
  names(linelist)[match(report_date_col, names(linelist))] <- report_col

  # surveillance requires plain Dates on both columns.
  linelist[[event_col]]  <- as.Date(linelist[[event_col]])
  linelist[[report_col]] <- as.Date(linelist[[report_col]])

  if (is.null(aggregate_by)) {
    aggregate_by <- switch(get_event_units(x),
      "days"   = "1 day",
      "weeks"  = "1 week",
      "months" = "1 month",
      "1 week"
    )
  }

  if (verbose) {
    cli::cli_h3("Converting {.cls tbl_now} into a {.pkg surveillance} line list")
    cli::cli_ul()
    cli::cli_li("{.arg dEventCol} <- {.val {event_col}}")
    cli::cli_li("{.arg dReportCol} <- {.val {report_col}}")
    cli::cli_li("{.arg aggregate.by} <- {.val {aggregate_by}}")
    cli::cli_li("{.arg now} <- {.val {as.character(get_now(x))}}")
    cli::cli_end()
  }

  if (format == "sts") {
    return(surveillance::linelist2sts(
      linelist, dateCol = event_col, aggregate.by = aggregate_by, ...
    ))
  }

  linelist
}


#' Convert a `tbl_now` into the line list \pkg{nowcaster} nowcasts from
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [nowcaster::nowcasting_inla()] fits an INLA nowcast to an individual-level
#' line list with an onset-date column and a report-date column.
#' `tbl_now_to_nowcaster()` produces that data frame, renaming the two dates to
#' \pkg{nowcaster}'s own vocabulary (`date_onset` / `date_report`) so they can
#' be handed over directly.
#'
#' `nowcasting_inla()` takes those two arguments as **bare column names**
#' (tidy-evaluation), so pass them unquoted:
#' `nowcasting_inla(df, date_onset = date_onset, date_report = date_report)`.
#'
#' \pkg{nowcaster} can stratify, but only through `age_col` + `bins_age`, and only
#' on a **numeric** column it can [cut()] -- despite its help calling `age_col` a
#' "stratum column", a character one errors. When the object carries strata this
#' converter therefore pools them into a single label, codes it `1..n`, and
#' returns it as `stratum_col` (default `"stratum_code"`), with two attributes:
#'
#' * `"nowcaster_bins"` -- breaks that put each level in its own bin, to pass as
#'   `bins_age`.
#' * `"nowcaster_levels"` -- the labels, in code order, to map the results back.
#'
#' The stratified fit then returns an extra `$age` component, one row per
#' stratum per week, alongside the pooled `$total`.
#'
#' @param x A `tbl_now` object.
#' @param event_col,report_col Names to give the event and report date columns
#'   in the result. Default to `"date_onset"` and `"date_report"`.
#' @param stratum_col Name for the numeric strata code column added when the
#'   object carries strata. Default `"stratum_code"`.
#' @param verbose Logical. Print the choices that were made.
#' @param ... Currently unused, for extensibility.
#'
#' @return A `data.frame` line list with one row per case. When the object has
#'   strata it also carries `stratum_col` plus the `"nowcaster_bins"` and
#'   `"nowcaster_levels"` attributes.
#'
#' @seealso [tbl_now_to_surveillance()], [tbl_now_to_epinowcast()]
#'
#' @examplesIf requireNamespace("nowcaster", quietly = TRUE)
#' data(denguedat)
#' nowobj <- tbl_now(denguedat,
#'   event_date = "onset_week", report_date = "report_week", verbose = FALSE
#' )
#' nc <- tbl_now_to_nowcaster(nowobj, verbose = FALSE)
#' head(nc)
#'
#' @name tbl_now_nowcaster
#' @export
tbl_now_to_nowcaster <- function(x, ..., event_col = "date_onset",
                                 report_col = "date_report",
                                 stratum_col = "stratum_code", verbose = TRUE) {
  .assert_tbl_now(x, "tbl_now_to_nowcaster")
  .need_pkg("nowcaster", c(covid19br = "https://covid19br.r-universe.dev"))

  event_date_col  <- get_event_date(x)
  report_date_col <- get_report_date(x)
  strata_cols     <- get_strata(x)
  covariate_cols  <- get_covariates(x)

  materialised  <- .materialize_temporal_effects(x)
  x             <- materialised$x
  temporal_cols <- materialised$cols

  linelist <- .tbl_now_expand_to_linelist(x, "tbl_now_to_nowcaster")

  kept <- c(
    event_date_col, report_date_col, strata_cols, covariate_cols, temporal_cols
  )
  linelist <- as.data.frame(linelist[, kept, drop = FALSE])

  names(linelist)[match(event_date_col, names(linelist))]  <- event_col
  names(linelist)[match(report_date_col, names(linelist))] <- report_col

  linelist[[event_col]]  <- as.Date(linelist[[event_col]])
  linelist[[report_col]] <- as.Date(linelist[[report_col]])

  # nowcaster CAN stratify, through `age_col` + `bins_age` -- but only on a
  # NUMERIC column it can `cut()`. Its own help calls the argument a "stratum
  # column", yet a character one hits `cut()`, and a character `bins_age` hits
  # `if (bins_age == "SI-PNI")` with a vector. So pool the strata into one label,
  # code it 1..n, and supply breaks that put each level in its own bin.
  strata_levels <- NULL
  if (length(strata_cols) > 0) {
    labels <- do.call(
      paste,
      c(lapply(strata_cols, function(c1) as.character(linelist[[c1]])), sep = " | ")
    )
    strata_levels <- sort(unique(labels))
    linelist[[stratum_col]] <- as.numeric(factor(labels, levels = strata_levels))
    attr(linelist, "nowcaster_levels") <- strata_levels
    # Breaks are half-integers so bin k holds exactly code k.
    attr(linelist, "nowcaster_bins") <- c(0, seq_along(strata_levels) + 0.5)
  }

  if (verbose) {
    cli::cli_h3("Converting {.cls tbl_now} into a {.pkg nowcaster} line list")
    cli::cli_ul()
    cli::cli_li("{.arg date_onset} <- {.val {event_col}}")
    cli::cli_li("{.arg date_report} <- {.val {report_col}}")
    if (length(strata_cols)) {
      cli::cli_li(
        "strata {.val {strata_cols}} pooled into {.val {stratum_col}} \\
         ({length(strata_levels)} level{?s}); pass it as {.arg age_col} with \\
         {.code bins_age = attr(x, \"nowcaster_bins\")}"
      )
    }
    cli::cli_end()
  }

  linelist
}



# tbl_now_triangle_list ---------------------------------------------------------

#' One reporting triangle per stratum
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' The object returned by
#' `tbl_now_to_baselinenowcast(x, format = "triangle_list")`: a list of
#' [baselinenowcast::as_reporting_triangle()] matrices, one per observed
#' combination of the object's strata, together with the metadata needed to
#' rebuild a `tbl_now` from it.
#'
#' It is a **thin** class -- it is still a list, so `lapply()`, `[[` and friends
#' work as usual:
#'
#' ```r
#' triangles <- tbl_now_to_baselinenowcast(x, format = "triangle_list")
#' lapply(triangles, baselinenowcast::baselinenowcast)
#' ```
#'
#' The class exists for one reason. \pkg{baselinenowcast} has a function,
#' [baselinenowcast::estimate_and_apply_delays()], whose first argument
#' `retro_reporting_triangles` is *also* a list of triangles -- but a list of
#' **retrospective** snapshots of one series, used to estimate uncertainty, not
#' one triangle per stratum. Passing this object there would be accepted and
#' would silently treat your strata as successive points in time. Printing the
#' object says plainly what it is, so the mistake is visible rather than silent.
#'
#' @param x A `tbl_now_triangle_list`.
#' @param ... Ignored.
#'
#' @return `print()` returns `x` invisibly.
#'
#' @seealso [tbl_now_to_baselinenowcast()], [as_tbl_now()]
#'
#' @name tbl_now_triangle_list
NULL

#' @rdname tbl_now_triangle_list
#' @exportS3Method base::print
print.tbl_now_triangle_list <- function(x, ...) {
  # NOTE: a print method must write to STDOUT. The `cli_*()` family emits
  # *messages*, so its output disappears under `message = FALSE`, `sink()` or
  # `capture.output()` -- which is exactly where a print method is expected to
  # work. The `cat_*()` family is the stdout counterpart.
  strata_cols <- attr(x, "strata_cols")
  dims <- vapply(x, function(one) paste(dim(one), collapse = " x "), character(1))

  cli::cat_rule(
    left = cli::format_inline("{length(x)} reporting triangle{?s} from a {.cls tbl_now}")
  )
  cli::cat_bullet(c(
    if (length(strata_cols) > 0) {
      cli::format_inline("One per stratum ({.val {strata_cols}}): {.val {names(x)}}")
    } else {
      cli::format_inline("No strata; a single triangle named {.val all}")
    },
    cli::format_inline("Delays unit: {.val {attr(x, 'delays_unit')}}"),
    cli::format_inline("Now: {.val {as.character(attr(x, 'now'))}}"),
    cli::format_inline("Dimensions (event x delay): {.val {unname(dims)}}")
  ))
  # NB: build the string with paste0() rather than cli's `\\` line
  # continuation -- `format_inline()` does not strip those, so they would be
  # printed literally.
  cli::cat_line(cli::format_inline(paste0(
    "{cli::symbol$info} This is one triangle per STRATUM. ",
    "{.fn baselinenowcast::estimate_and_apply_delays} expects retrospective ",
    "snapshots of a single series instead -- do not pass this object to it."
  )))
  invisible(x)
}

#' @rdname as_tbl_now
#' @export
as_tbl_now.tbl_now_triangle_list <- function(object, ...) {
  strata_cols   <- attr(object, "strata_cols")
  strata_values <- attr(object, "strata_values")
  delays_unit   <- attr(object, "delays_unit")
  event_col     <- attr(object, "event_col")
  report_col    <- attr(object, "report_col")

  # Expand every triangle back to long incremental counts and re-attach the
  # strata VALUES that were stored alongside it, rather than parsing the element
  # names (a stratum containing the separator would not survive that).
  pieces <- lapply(seq_along(object), function(i) {
    long <- .reporting_triangle_to_long(object[[i]], delays_unit = delays_unit)
    if (length(strata_cols) > 0 && nrow(long) > 0) {
      long <- cbind(long, strata_values[rep(i, nrow(long)), , drop = FALSE])
    }
    long
  })
  long_data <- do.call(rbind, pieces)
  rownames(long_data) <- NULL

  # Back to the caller's own column names.
  names(long_data)[match("reference_date", names(long_data))] <- event_col
  names(long_data)[match("report_date", names(long_data))]    <- report_col

  dots <- list(...)
  if (is.null(dots$now)) dots$now <- attr(object, "now")

  result <- do.call(
    tbl_now,
    c(
      list(
        long_data,
        event_date  = event_col,
        report_date = report_col,
        case_count  = "count",
        data_type   = "count-incidence",
        strata      = if (length(strata_cols) > 0) strata_cols else NULL,
        verbose     = FALSE
      ),
      dots
    )
  )

  .drop_zero_counts(result)
}

# 4. S3 methods on other packages' coercion generics------
#
#    These register a `tbl_now` method on each supported package's own coercion
#    generic, so that package's verb accepts a `tbl_now` directly. Each is a
#    thin wrapper around the matching tbl_now_to_*() converter and is quiet by
#    default (verbose = FALSE) because it is a coercion idiom.


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
