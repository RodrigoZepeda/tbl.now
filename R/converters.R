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
#' @param repo Optional named repository to add to `repos` in the suggested
#'   `install.packages()` call. Defaults to the epinowcast r-universe.
#' @param install Optional literal install instruction, used verbatim instead of
#'   building an `install.packages()` call. Use this for back-ends that are not
#'   served by any CRAN-style repository.
#'
#' @return `NULL`, invisibly (called for its side effect of aborting when the
#'   package is missing).
#'
#' @keywords internal
#' @noRd
.need_pkg <- function(pkg, repo = NULL, install = NULL) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    if (is.null(install)) {
      # Most optional back-ends live on the epinowcast r-universe; `repo` lets a
      # caller point at a different one.
      if (is.null(repo)) {
        repo <- c(epinowcast = "https://epinowcast.r-universe.dev")
      }
      install <- paste0(
        "install.packages(\"", pkg, "\", ",
        "repos = c(options('repos'), ",
        names(repo)[1], " = '", unname(repo)[1], "'))"
      )
    }
    cli::cli_abort(c(
      "Package {.pkg {pkg}} is required for this conversion.",
      "i" = paste0("Install it with: ", install)
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

#' The \pkg{NobBS} `units` string implied by a `tbl_now`'s event units
#'
#' [NobBS::NobBS()] documents `units` as `"1 day"` or `"1 week"` and nothing
#' else, so the object's own `"days"` / `"weeks"` cannot be handed over
#' verbatim. Coarser or index-based grids it cannot model at all: on a
#' `"numeric"` grid the date columns are integer indices, and coercing those
#' with `as.Date()` silently anchors them at the 1970 epoch, producing a line
#' list of plausible-looking but invented dates.
#'
#' @param event_units The object's event units (see [get_event_units()]).
#'
#' @return `"1 day"` or `"1 week"`; aborts for units \pkg{NobBS} cannot model.
#'
#' @keywords internal
#' @noRd
.nobbs_units <- function(event_units) {
  switch(event_units,
    days  = "1 day",
    weeks = "1 week",
    cli::cli_abort(c(
      "{.pkg NobBS} cannot model event units {.val {event_units}}.",
      "i" = "{.fn NobBS::NobBS} accepts {.arg units} of {.val 1 day} or \\
             {.val 1 week} only.",
      "i" = "Aggregate to one of those first, e.g. with {.fn align_weeks}."
    ))
  )
}

#' The \pkg{surveillance} `aggregate.by` string implied by a `tbl_now`'s units
#'
#' [surveillance::linelist2sts()] takes `aggregate.by` from a fixed set of
#' strings, and [surveillance::nowcast()] passes it straight through. A
#' `"numeric"` grid has no dates to aggregate: its event/report columns are
#' integer indices, and `as.Date()` on those anchors them at the 1970 epoch,
#' which is how a numeric-grid conversion used to come back looking like a
#' 1970 outbreak.
#'
#' @param event_units The object's event units (see [get_event_units()]).
#'
#' @return One of `"1 day"`, `"1 week"`, `"1 month"`, `"1 year"`; aborts on a
#'   `"numeric"` grid.
#'
#' @keywords internal
#' @noRd
.surveillance_aggregate_by <- function(event_units) {
  switch(event_units,
    days   = "1 day",
    weeks  = "1 week",
    months = "1 month",
    years  = "1 year",
    cli::cli_abort(c(
      "{.pkg surveillance} cannot aggregate event units {.val {event_units}}.",
      "i" = "{.fn surveillance::linelist2sts} needs calendar dates; a \\
             {.val numeric} grid has only integer indices.",
      "i" = "Pass {.arg aggregate_by} explicitly if you know what the index \\
             steps mean."
    ))
  )
}

#' Warn that negative delays will be dropped by a delay-indexed target
#'
#' A reporting triangle's delay axis starts at 0, and so does \pkg{epinowcast}'s
#' completed grid, so a report that arrived *before* its event has no cell to go
#' in. [baselinenowcast::as_reporting_triangle()] drops it without comment and
#' the cell then reads `0` -- an *observed* zero -- which is the same loss twice
#' over: the case is gone, and the `NA`-vs-`0` distinction is broken at exactly
#' the cell that lost it.
#'
#' `tbl_now()` warns about negative delays when the object is built, but that can
#' be many pipeline steps earlier and says nothing about cases being dropped, so
#' the converter says it again at the point where it actually costs something.
#'
#' @param x A `tbl_now`.
#' @param fn Name of the calling converter, used in the warning.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
.warn_negative_delays <- function(x, fn) {
  delays <- x[[".delay"]]
  if (is.null(delays)) {
    return(invisible(NULL))
  }
  negative <- !is.na(delays) & delays < 0
  n_rows <- sum(negative)
  if (n_rows == 0L) {
    return(invisible(NULL))
  }

  # How many cases that is depends on the data type: a line list is one row per
  # case, incidence counts add up, and a cumulative total is not additive at all
  # -- so for cumulative data only the rows are reported.
  count_col <- get_case_count(x)
  cases <- switch(get_data_type(x),
    "linelist"        = n_rows,
    "count-incidence" = sum(x[[count_col]][negative], na.rm = TRUE),
    NA_real_
  )
  lost <- if (is.na(cases)) {
    ""
  } else {
    cli::format_inline(", carrying {.val {cases}} case{?s},")
  }

  cli::cli_warn(c(
    paste0(
      "{.val {n_rows}} row{?s}{lost} ",
      "{cli::qty(n_rows)}{?has/have} a negative delay ",
      "(earliest {.val {min(delays, na.rm = TRUE)}})."
    ),
    "!" = "{.fn {fn}} indexes by delay from 0, so {cli::qty(n_rows)}{?it/they} \\
           cannot be represented and {cli::qty(n_rows)}{?is/are} dropped.",
    "i" = "The affected cell then reads {.val 0}, which is indistinguishable \\
           from an observed zero.",
    "i" = "Filter them out yourself to choose what happens, e.g. \\
           {.code dplyr::filter(x, .delay >= 0)}."
  ))
  invisible(NULL)
}

#' The daily grid \pkg{EpiNow2} models on, plus its `accumulate` flag
#'
#' \pkg{EpiNow2} 1.9.0 has no `timestep`, `interval` or `period` argument on any
#' of `estimate_infections()`, `epinow()`, `regional_epinow()` or
#' `estimate_truncation()` (checked all four formals). It models a **daily**
#' process, so a weekly series handed over as one row per week is read as one row
#' per day and the fit is silently wrong on the time axis.
#'
#' Its own answer is the `accumulate` column, and its own function for building
#' one is [EpiNow2::fill_missing()] -- which is what this delegates to. An earlier
#' version laid the grid out by hand and got it **wrong in a way nothing would
#' have caught**: it put each period's count on the period's *last* day, where
#' `fill_missing()` leaves it on the date the caller gave and back-fills the days
#' *before* it. Same `accumulate` pattern, dates shifted by six. Reimplementing a
#' target package's own helper is exactly the mistake DEVELOPMENT_SKILL section 4
#' warns about.
#'
#' @param series A data frame with `date` and `confirm`, one row per event period.
#' @param event_units The object's event units (see [get_event_units()]).
#' @param by Optional column name whose groups are expanded separately (the
#'   `region` column).
#'
#' @return `series` on a dense daily grid with a logical `accumulate` column, or
#'   unchanged when the units are already daily.
#'
#' @keywords internal
#' @noRd
.epinow2_grid <- function(series, event_units, by = NULL) {
  # Daily data is already on EpiNow2's grid. Running it through `fill_missing()`
  # would append an all-FALSE `accumulate` column that says nothing, and would
  # force a decision about what a missing day means that the caller has not asked
  # us to make.
  if (identical(event_units, "days")) {
    return(series)
  }
  # Aborts here for units EpiNow2 cannot lay on a daily axis.
  step <- .epinow2_step_days(event_units)
  .need_pkg("EpiNow2")

  # `initial_accumulate` is passed explicitly, not left to be inferred. Two
  # reasons, and the first is not cosmetic:
  #
  #  * with `by`, EpiNow2 1.9.0's inference DROPS each group's first observation
  #    -- a two-region weekly series of 336/167 cases came back as 295/147, with
  #    the grid starting the day AFTER the first report. Passing the value keeps
  #    every case. (Single-series inference is fine; it is the `by` path.)
  #  * inference reads the period off the data, so one missing week is enough to
  #    make it guess wrong. The object already knows its own units.
  #
  # `step` is the right value: every observation, the first included, is the
  # total for a window that many days wide.
  as.data.frame(EpiNow2::fill_missing(
    series,
    missing_dates      = "accumulate",
    obs_column         = "confirm",
    by                 = by,
    initial_accumulate = step
  ))
}

#' Days per event period, for the \pkg{EpiNow2} daily expansion
#'
#' @param event_units The object's event units.
#'
#' @return A positive integer; aborts for units \pkg{EpiNow2} cannot lay on a
#'   daily grid.
#'
#' @keywords internal
#' @noRd
.epinow2_step_days <- function(event_units) {
  switch(event_units,
    days  = 1L,
    weeks = 7L,
    cli::cli_abort(c(
      "{.pkg EpiNow2} cannot model event units {.val {event_units}}.",
      "i" = "It models a {.emph daily} process and has no {.arg timestep}: \\
             non-daily data is laid on a daily grid with an {.field accumulate} \\
             column, which is only well defined for {.val days} and {.val weeks}.",
      "i" = "A {.val numeric} grid has no calendar to expand onto at all.",
      "i" = "Aggregate to days or weeks first, e.g. with {.fn align_weeks}."
    ))
  )
}

#' Paste several stratifying columns into the one label column a back-end takes
#'
#' Several back-ends stratify by ONE column: [NobBS::NobBS.strat()] takes a
#' single `strata` name, [EpiNow2::regional_epinow()] a single `region`, and
#' [surveillance::nowcast()] takes none at all, so a stratified analysis there
#' means splitting the line list yourself. A `tbl_now` may declare any number of
#' stratifying columns, and their interaction -- "nowcast each observed
#' combination separately" -- is exactly what those back-ends mean by one
#' stratum. This pastes them into that label.
#'
#' The separator has to be recoverable, because [tidy()] splits the label back
#' into the original columns. If a stratum VALUE already contains the separator
#' the split is ambiguous, and a silently mispaired stratum is worse than a
#' failed conversion -- so that aborts, naming the separator to choose instead.
#'
#' @param data A data frame carrying the strata columns.
#' @param strata_cols Character vector of stratifying column names.
#' @param sep Separator to paste with.
#' @param fn Calling function, for the error message.
#' @param argument The name of the separator argument to point the user at.
#'
#' @return A character vector of labels, one per row; all `"all"` when there are
#'   no strata.
#'
#' @keywords internal
#' @noRd
.paste_strata_labels <- function(data, strata_cols, sep = " | ",
                                 fn = NULL, argument = "strata_sep") {
  if (length(strata_cols) == 0L) {
    return(rep("all", nrow(data)))
  }
  values <- unname(as.list(data[strata_cols]))
  if (any(vapply(
    values,
    function(column) any(grepl(sep, as.character(column), fixed = TRUE)),
    logical(1)
  ))) {
    cli::cli_abort(c(
      "Cannot combine strata {.val {strata_cols}}{if (is.null(fn)) '' else \
       paste0(' in ', fn)}.",
      "i" = "They are pasted {.val {sep}}-separated into a single label, and a \
             stratum value already contains that separator -- the label could \
             not be split back apart.",
      "i" = "Choose a separator the values do not contain, with \
             {.arg {argument}}."
    ))
  }
  do.call(paste, c(values, sep = sep))
}

#' Add the single pasted strata column a one-column back-end takes
#'
#' Shared by [tbl_now_to_nobbs()] and [tbl_now_to_surveillance()]. Refuses to
#' overwrite a column that is already there: silently replacing a declared
#' covariate called `strata` would lose data.
#'
#' @param data The line list being built.
#' @param strata_cols Character vector of stratifying column names.
#' @param strata_col Name for the new column, or `NULL` to add nothing.
#' @param strata_sep Separator to paste with.
#' @param fn Calling function, for the messages.
#'
#' @return `data`, with the column added when there was anything to add.
#'
#' @keywords internal
#' @noRd
.add_strata_column <- function(data, strata_cols, strata_col, strata_sep, fn) {
  if (length(strata_cols) == 0L || is.null(strata_col)) {
    return(data)
  }
  if (strata_col %in% names(data) && !strata_col %in% strata_cols) {
    cli::cli_abort(c(
      "{.arg strata_col} {.val {strata_col}} is already a column of {.arg x}.",
      "i" = "{.fn {fn}} would overwrite it with the pasted strata label.",
      "i" = "Choose another name with {.arg strata_col}, or drop the column \
             with {.arg strata_col = NULL}."
    ))
  }
  data[[strata_col]] <- .paste_strata_labels(
    data, strata_cols, sep = strata_sep, fn = paste0(fn, "()")
  )
  data
}

#' Collapse a `tbl_now`'s strata into the single `region` column EpiNow2 takes
#'
#' [EpiNow2::regional_epinow()] takes one `region` column, pasted `" | "`-
#' separated -- the same convention as
#' `tbl_now_to_baselinenowcast(format = "triangle_list")`'s names and
#' `.epinowcast_stratum()`.
#'
#' @inheritParams .paste_strata_labels
#'
#' @return A character vector of labels, one per row.
#'
#' @keywords internal
#' @noRd
.epinow2_region <- function(data, strata_cols, sep = " | ") {
  .paste_strata_labels(
    data, strata_cols, sep = sep, fn = "tbl_now_to_EpiNow2()"
  )
}

#' Build the four interval-censoring date columns a delay model needs
#'
#' \pkg{epidist} and [EpiNow2::estimate_dist()] (new in EpiNow2 1.9.0) take the
#' **same** schema: `pdate_lwr`/`pdate_upr` for the primary event and
#' `sdate_lwr`/`sdate_upr` for the secondary one, all in days. This builds it once
#' so both converters agree by construction rather than by two people editing two
#' copies the same way.
#'
#' The primary event spans `[event_date, event_date + w]`, where `w` matches the
#' object's time unit. The secondary spans `[report_date, report_date + w]`
#' normally, and the left-censored `[event_date, report_date]` for a row flagged
#' by `is_censored_report` -- the `tbl_now` convention that a censored report is known
#' only to have happened at or before its report date. The lower bound is that
#' row's OWN event: a delay cannot be negative, and bounding by the earliest
#' event in the data (as this did before 0.20.0) starts the window before the
#' case existed.
#'
#' @param x A `tbl_now`.
#' @param censoring_window Optional width in days; `NULL` derives it from
#'   [get_event_units()].
#'
#' @return A list with `data` (a tibble of the four date columns), `width` (the
#'   window in days) and `censored` (the logical flag vector, for reporting).
#'
#' @keywords internal
#' @noRd
.delay_censoring_windows <- function(x, censoring_window = NULL) {
  event_col    <- get_event_date(x)
  report_col   <- get_report_date(x)
  censored_col <- get_is_censored_report(x)
  obs          <- dplyr::as_tibble(x)

  win <- if (is.null(censoring_window)) {
    .epidist_window_days(get_event_units(x))
  } else {
    censoring_window
  }
  censored <- if (!is.null(censored_col)) {
    as.logical(obs[[censored_col]])
  } else {
    rep(FALSE, nrow(obs))
  }
  censored[is.na(censored)] <- FALSE

  out <- dplyr::tibble(
    pdate_lwr = obs[[event_col]],
    pdate_upr = obs[[event_col]] + win,
    # A censored report is known only to have happened at or before its report
    # date -- so the window is [event_date, report_date]. It is bounded BELOW by
    # the event, not by the earliest event in the data: a delay cannot be
    # negative, and a global origin makes the window start before the case
    # existed for every row except the very first. \pkg{epidist} rejects that
    # outright ("Assertion on 'data$stime_lwr' failed: not >= 0"), and
    # `EpiNow2::estimate_dist()` would fit a delay distribution with mass below
    # zero.
    sdate_lwr = dplyr::if_else(censored, obs[[event_col]], obs[[report_col]]),
    sdate_upr = dplyr::if_else(censored, obs[[report_col]], obs[[report_col]] + win)
  )

  # NOTE on the windows and the `now`, because the two are easy to conflate.
  # `[sdate_lwr, sdate_upr)` brackets WHEN THE REPORT HAPPENED. At weekly
  # resolution all we know is the week, so the window is `[W, W + 7)` -- a
  # half-open interval whose upper bound is the END of that week, not a claim
  # that anything happened on day W + 7. Clamping it at the `now` would not make
  # it more truthful, it would move the report to an earlier week.
  #
  # When observation STOPPED is a different quantity, and it is `obs_date` --
  # handled by the callers, which set it to the end of the `now` period.

  # Both packages require strictly positive widths, and a censored report in the
  # same period as its event collapses to zero. Widen UPWARD: the report is then
  # known to lie somewhere in that period, `[event, event + win)`. Widening
  # downward instead (the old behaviour) pushed the lower bound before the event
  # and produced the negative delays described above.
  collapsed <- out$sdate_upr <= out$sdate_lwr
  out$sdate_upr[collapsed] <- out$sdate_lwr[collapsed] + win

  list(data = out, width = win, censored = censored)
}

#' Warn when a large share of delays are zero
#'
#' A delay distribution with a **point mass at zero** cannot be fitted with a
#' lognormal, a gamma with shape > 1, or a Weibull with shape > 1: all have zero
#' density at zero. The fit does not fail loudly -- it inflates the variance until
#' the density piles up near zero. On a daily COVID series where 57% of cases
#' carried a delay of exactly 0, \pkg{epidist} returned `sigma = 17.9` and an
#' implied mean delay of `1.5e73` days.
#'
#' Interval censoring softens this -- a same-period report gives a window rather
#' than a point -- but does not remove it, which is why that example happened
#' with a censored model.
#'
#' @param x A `tbl_now`.
#' @param target Name of the target, for the message.
#' @param threshold Share of zero delays above which to warn.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
.warn_zero_delays <- function(x, target, threshold = 0.2) {
  delays <- x[[".delay"]]
  if (is.null(delays)) {
    return(invisible(NULL))
  }
  share <- mean(delays == 0, na.rm = TRUE)
  if (is.na(share) || share < threshold) {
    return(invisible(NULL))
  }

  cli::cli_warn(c(
    "{.val {round(100 * share)}}% of delays are exactly zero.",
    "!" = "A lognormal -- and a gamma or Weibull with shape > 1 -- has zero \\
           density at zero, so {.fn {target}} will not fail; it will inflate \\
           the variance until the density piles up near zero.",
    "i" = "Try a family with positive density at zero \\
           ({.code dist = \"exp\"}, or {.code \"gamma\"}/{.code \"weibull\"} \\
           where the shape can go below 1).",
    "i" = "Or model the zero share separately and fit the distribution to the \\
           non-zero delays."
  ))
  invisible(NULL)
}

#' Drop the count rows a delay model cannot represent
#'
#' `epidist_aggregate_data` asserts `n >= 1`. Incremental counts routinely carry
#' zeros -- any report that added nothing -- and de-accumulating a cumulative
#' total that was revised downward gives a negative. Neither is an epidist row:
#' a zero contributes no case to a delay distribution, so dropping it is
#' lossless; a negative is not a number of cases at all, so dropping it loses the
#' revision and has to say so.
#'
#' Without this the conversion failed on essentially any `count-cumulative`
#' input with \pkg{epidist}'s own message, `Assertion on 'data$n' failed:
#' Element 6 is not >= 1`, which says nothing about what to do next.
#'
#' @param x A `tbl_now` holding incremental counts.
#' @param verbose Logical; report how many rows carried no case.
#' @param target Name of the target package, for the messages.
#'
#' @return `x` with only strictly positive counts.
#'
#' @keywords internal
#' @noRd
.drop_unusable_counts <- function(x, verbose = TRUE, target = "epidist") {
  count_col <- get_case_count(x)
  counts <- x[[count_col]]

  negative <- !is.na(counts) & counts < 0
  if (any(negative)) {
    dropped <- sum(negative)
    cli::cli_warn(c(
      "{.val {dropped}} row{?s} carr{?ies/y} a negative count, which \\
       {.pkg {target}} cannot represent.",
      "i" = "These are downward revisions produced by de-accumulating \\
             cumulative totals; they are dropped, so the revision is lost.",
      "i" = "The delay distribution is estimated from the remaining rows."
    ))
  }

  keep <- !is.na(counts) & counts > 0
  if (!any(keep)) {
    cli::cli_abort(c(
      "No usable counts left for {.pkg {target}}.",
      "i" = "It needs {.code n >= 1}, and every row here is zero, negative or \\
             missing.",
      "i" = "Cumulative data de-accumulates to zero wherever a report added \\
             nothing; check {.code to_count(x, to = \"count-incidence\")}."
    ))
  }

  if (isTRUE(verbose) && !all(keep)) {
    cli::cli_inform(
      "Dropped {.val {sum(!keep)}} row{?s} carrying no case (zero, negative \\
       or missing count); {.pkg {target}} needs {.code n >= 1}."
    )
  }

  dplyr::filter(x, keep)
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


#' Drop reports whose delay is beyond a cap
#'
#' Counted the way [tbl_now_to_epinowcast()] counts it, so the same number means
#' the same triangle in both: `max_delay` is the NUMBER of delay periods kept,
#' i.e. delays `0` to `max_delay - 1`, measured in the object's report units.
#' `NULL` keeps every delay, which is `max(.delay) + 1` periods.
#'
#' Capping is a modelling choice rather than a workaround: one straggler with a
#' 330-day delay gives the triangle 331 columns, almost all of them empty, and
#' costs minutes of fitting for no gain.
#'
#' @param x A `tbl_now` object.
#' @param max_delay A single positive whole number, or `NULL`.
#' @param fn Name of the calling converter, for messages.
#' @param verbose Logical.
#'
#' @return A `tbl_now`, filtered when a cap was given.
#'
#' @keywords internal
#' @noRd
.cap_max_delay <- function(x, max_delay, fn, verbose = TRUE) {
  if (is.null(max_delay)) {
    return(x)
  }
  if (!is.numeric(max_delay) || length(max_delay) != 1L || is.na(max_delay) ||
        max_delay < 1 || max_delay != round(max_delay)) {
    cli::cli_abort(
      "{.arg max_delay} must be a single whole number of at least 1, or {.code NULL}."
    )
  }

  delays <- dplyr::pull(x, ".delay")
  keep <- is.na(delays) | delays < max_delay
  dropped <- sum(!keep, na.rm = TRUE)

  if (dropped == 0) {
    return(x)
  }
  out <- dplyr::filter(x, !!rlang::sym(".delay") < !!max_delay | is.na(!!rlang::sym(".delay")))

  if (isTRUE(verbose)) {
    cli::cli_inform(c(
      "i" = cli::format_inline(paste0(
        "{.fn {fn}}: kept delays 0-{max_delay - 1} ",
        "({get_report_units(x)}), dropping {dropped} row{?s}."
      ))
    ))
  }
  out
}

#' Warn that declared covariates did not survive a conversion
#'
#' A covariate is a promise: the user declared it because they want the model to
#' use it. Several targets have nowhere to put one -- a reporting triangle has
#' only (event, delay) cells, and \pkg{EpiNow2}'s series is `date`/`confirm` --
#' so the column is dropped. Silently dropping it means the user believes a
#' covariate is in the model when it is not, which is the worst of the three
#' possible outcomes.
#'
#' Materialised temporal-effect columns count as covariates here: they are
#' exactly the case where somebody has asked for an effect and would otherwise
#' never learn it was ignored.
#'
#' @param x A `tbl_now` object.
#' @param fn Name of the calling converter, for the message.
#' @param kept Character vector of covariate columns that DID survive.
#' @param advice Optional extra bullet: what the user can do instead.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
.warn_dropped_covariates <- function(x, fn, kept = character(0), advice = NULL) {
  declared <- c(
    get_covariates(x) %||% character(0),
    get_temporal_effect_cols(x) %||% character(0)
  )
  dropped <- setdiff(unique(declared), kept)
  if (length(dropped) == 0) {
    return(invisible(NULL))
  }

  message <- c(
    cli::format_inline(paste0(
      "{.fn {fn}}: {length(dropped)} declared covariate{?s} ",
      "({.val {dropped}}) {?is/are} not carried into this format."
    ))
  )
  if (!is.null(advice)) message <- c(message, "i" = advice)
  message <- c(
    message,
    "i" = "The model will not see {cli::qty(length(dropped))}{?it/them}."
  )
  cli::cli_warn(message)
  invisible(NULL)
}

#' Columns a `tbl_now` carries but was never told about
#'
#' Everything that is neither protected (the dates, the count, the censoring
#' flag, the generated `.event_num`/`.report_num`/`.delay`) nor declared as a
#' stratum, a covariate or a materialised temporal effect.
#'
#' `covid_colombia` is the motivating case: it carries `sex`, and an object
#' built without `strata = sex` therefore has TWO rows per
#' `(notification_date, diagnosis_date)` cell. A reporting triangle, a `tsibble`
#' key and an epinowcast observation table all have exactly one slot per cell,
#' so the extra dimension has to go somewhere before the conversion can happen.
#'
#' @param x A `tbl_now` object.
#'
#' @return A character vector of column names, possibly empty.
#'
#' @keywords internal
#' @noRd
.undeclared_cols <- function(x) {
  known <- c(
    get_protected_cols(x),
    get_strata(x) %||% character(0),
    get_covariates(x) %||% character(0),
    get_temporal_effect_cols(x) %||% character(0)
  )
  setdiff(colnames(x), known)
}

#' Pool a `tbl_now` over the columns it was never told about
#'
#' A converter should not make the caller aggregate first. `to_count()` already
#' sums over every column that is neither declared nor protected, so this is
#' just "call it when there is something to pool, and say so".
#'
#' Line lists are left alone: one row IS one case there, so there is nothing to
#' sum, and collapsing would destroy the individual records the target package
#' is being handed.
#'
#' The message is deliberately `verbose`-only rather than a warning. It fires on
#' ordinary, correct usage -- any dataset with a column you did not declare --
#' and a warning on the common path trains people to ignore warnings.
#'
#' @param x A `tbl_now` object.
#' @param fn Name of the calling converter, for the message.
#' @param verbose Logical.
#'
#' @return A `tbl_now`, aggregated when there was anything to aggregate.
#'
#' @keywords internal
#' @noRd
.pool_undeclared <- function(x, fn, verbose = TRUE) {
  extra <- .undeclared_cols(x)
  if (length(extra) == 0 || identical(get_data_type(x), "linelist")) {
    return(x)
  }

  before <- nrow(x)
  pooled <- suppressWarnings(suppressMessages(
    to_count(x, to = get_data_type(x))
  ))

  if (isTRUE(verbose)) {
    # NB: built with paste0(), not cli's `\\` continuation -- `format_inline()`
    # does not strip those and would print them literally. `cli::qty()` supplies
    # the quantity for a plural marker that has no `{}` of its own before it.
    cli::cli_inform(c(
      "i" = cli::format_inline(paste0(
        "{.fn {fn}}: pooled over {length(extra)} undeclared ",
        "column{?s} ({.val {extra}}); {before} rows -> {nrow(pooled)}."
      )),
      "i" = cli::format_inline(paste0(
        "{cli::qty(length(extra))}Declare {?it/them} with {.fn add_strata} ",
        "to nowcast {?it/them} separately."
      ))
    ))
  }

  pooled
}

#' Collapse the censoring indicator before a conversion
#'
#' A censoring flag that is a per-case property rather than a function of the
#' delay -- an administrative "this date is only an upper bound" mark, say --
#' splits a single `(event_date, report_date)` cell into a censored and an
#' uncensored row. A reporting triangle has one slot per cell, so the extra
#' dimension makes the cell non-unique and the target package errors out; the
#' packages that expand back to a line list instead pick the flag up as an
#' unrequested stratifier.
#'
#' Neither resolution loses cases, but both discard the censoring information,
#' so each one warns. [tbl_now_to_epidist()] does *not* call this: estimating a
#' delay distribution is the one job that can use the flag.
#'
#' @param x A `tbl_now`.
#' @param fn Name of the calling converter, used in the warning.
#'
#' @returns `x` with no censoring indicator: counts summed over the flag for
#'   count data, the column dropped for a line list.
#'
#' @noRd
.tbl_now_collapse_censoring <- function(x, fn) {
  # Both axes are collapsed together: a validation-delay flag splits a cell in
  # exactly the same way a report-delay one does.
  censored_col <- intersect(
    c(get_is_censored_report(x), get_is_censored_validation(x)), names(x)
  )
  if (length(censored_col) == 0L) {
    return(x)
  }
  # The attributes go together, before either column does. Going through
  # `remove_is_censored_report()` would validate after clearing only the first
  # of the two, and the object is invalid in between: the second attribute
  # still names a column that has already been summed away.
  drop_flags <- function(obj) {
    attr(obj, "is_censored_report") <- NULL
    attr(obj, "is_censored_validation") <- NULL
    validate_tbl_now(obj)
    obj
  }

  # A line list is one row per case, so the flag never makes a cell non-unique
  # -- it only rides along as an extra column. Dropping it is enough.
  if (get_data_type(x) == "linelist") {
    cli::cli_warn(
      "{.fn {fn}} cannot represent censored delays; dropping the \\
       {.field {censored_col}} column{?s}. Cases are unaffected."
    )
    x[censored_col] <- NULL
    return(drop_flags(x))
  }

  # Counts: collapse the censoring dimension by summing within the cell. Every
  # other column is a grouping key, so nothing but the flag is pooled.
  count_col  <- get_case_count(x)
  group_cols <- setdiff(names(x), c(censored_col, count_col))
  n_before   <- nrow(x)

  collapsed <- .strip_tbl_now(x)
  collapsed[censored_col] <- NULL
  collapsed <- collapsed |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(count_col), \(v) sum(v, na.rm = TRUE)),
      .by = dplyr::all_of(group_cols)
    )

  cli::cli_warn(
    "{.fn {fn}} cannot represent censored delays; summing counts over \\
     {.field {censored_col}} ({n_before} row{?s} -> {nrow(collapsed)}). \\
     Case totals are unchanged."
  )

  # Rebuild the tbl_now around the collapsed data. Going through dplyr verbs
  # would drop the protected columns and demote it to a tibble, so the
  # attributes are carried over directly; they name columns rather than
  # positions, so the reordering `.by` introduces does not matter.
  rebuilt <- attributes(x)
  rebuilt$names     <- names(collapsed)
  rebuilt$row.names <- attr(collapsed, "row.names", exact = TRUE)
  attributes(collapsed) <- rebuilt

  drop_flags(collapsed)
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
#' @param quiet Logical. A *different* channel from `verbose`: `verbose`
#'   controls the informational summary of what the conversion did, while `quiet`
#'   suppresses the lossy-conversion **warning** emitted
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
#' library(data.table)
#' library(epinowcast)
#'
#' ## CRAN asks examples to use at most two cores; data.table would otherwise
#' ## take every one it can find.
#' data.table::setDTthreads(2)
#'
#' # epinowcast's own example data: German COVID-19 hospitalisations by age.
#' obs <- germany_covid19_hosp[location == "DE"][, location := NULL]
#'
#' ## A few weeks and a short delay keep the example quick; preprocessing the whole
#' ## series with `max_delay = 40` costs about ten times as much CPU.
#' recent <- obs[reference_date >= as.Date("2021-10-15")]
#' pobs <- epinowcast::enw_preprocess_data(recent, max_delay = 10, by = "age_group")
#'
#' # From the data.table input format ...
#' nowobj <- tbl_now_from_epinowcast(recent, strata = c("age_group"))
#' nowobj
#'
#' # ... or from a preprocessed epinowcast object.
#' tbl_epi <- tbl_now_from_epinowcast(pobs)
#'
#' # And back out again.
#' preprocessed_tbl <- tbl_now_to_epinowcast(tbl_epi, quiet = TRUE)
#'
#' @inheritSection tbl_now_baselinenowcast Negative delays
#' @inheritSection tbl_now_baselinenowcast Censored delays
#' @seealso
#' [engine_epinowcast()][nowcast_engines] to fit through this package rather than
#' converting by hand; [align_weeks()], because \pkg{epinowcast} lays its grid out
#' in whole timesteps; [complete_zeroes()] to fill the grid;
#' [tidy()][tidy.nowcast] for the fitted result.
#' [as_tbl_now()] for the generic that dispatches to the `*_from_*()` side;
#' [run_nowcast()], which does the conversion for you when you fit through an
#' [engine()]. The
#' [*One dataset, many nowcasts* article](https://rodrigozepeda.github.io/tbl.now/articles/nowcasting-models.html)
#' fits the same data with every supported package.
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
#' @param max_delay Number of delay periods to keep, in the object's report
#'   units: `max_delay = 30` keeps delays `0` to `29`, giving a 30-column
#'   triangle. Counted exactly as [tbl_now_to_epinowcast()] counts it, so the
#'   same number means the same triangle in both. `NULL` (default) keeps every
#'   delay -- which is fine on a short tail and expensive on a long one (see
#'   *Cost of a long delay tail*).
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
#' @param complete For `to` with a triangle format: fill event periods that have
#'   no reports at all with zeroes, out to the object's [get_now()], via
#'   [complete_zeroes()]. `"auto"` (the default) does this for **line-list**
#'   input only, so you do not have to remember
#'   `to_count() |> complete_zeroes()` first. Count data is left exactly as
#'   supplied, because it *can* distinguish an observed zero from a cell that
#'   could not be observed yet (`NA`) and filling those would claim reporting was
#'   complete when it was not. `TRUE` / `FALSE` force either behaviour. Ignored
#'   for `format = "long"`.
#' @param negatives How to handle the negative increments that appear when
#'   `count-cumulative` data is de-accumulated (a downward revision).
#'   `"redistribute"` (default) absorbs each negative into earlier delays with
#'   [baselinenowcast::preprocess_negative_values()], which is what that
#'   function exists for; `"error"` refuses cumulative input instead.
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
#' ## The matrix round-trip is faithful (not-yet-observed `NA` cells are kept).
#' identical(rt, tbl_now_to_baselinenowcast(nowobj))
#' @section Sparse same-period reporting (weekly data especially):
#'
#' `baselinenowcast` divides each observed row by the share of the delay
#' distribution that should have arrived by now. When almost nothing is reported
#' in the same period as the event, that share is tiny for the most recent row
#' and the estimate explodes: on a weekly line list where `P(delay = 0)` is about
#' **0.05**, a final row holding a single case became an estimate of **257 with
#' an upper bound of 1584** against a truth of 15.
#'
#' Completing the triangle to the `now` *always* leaves a final row observable
#' only at delay 0, so no choice of cut-off avoids it. Check the delay PMF before
#' trusting the newest rows:
#'
#' ```r
#' pmf <- baselinenowcast::estimate_delay(triangle)
#' pmf[1]   # share expected to arrive in the same period
#' ```
#'
#' If it is small, follow `baselinenowcast`'s own advice and truncate "to an
#' earlier reference time to ensure a nowcast, not a forecast, is being
#' produced" -- drop trailing rows whose expected observed share is below, say,
#' 10%. Daily data with substantial same-day reporting does not have this
#' problem.
#'
#' @section Capping the delay axis:
#'
#' The triangle gets one column per delay, so a single long straggler makes it
#' very wide and the fit very slow: capping delays at 30 days on a daily series
#' took a fit from **314s to 50s** for a tail carrying under 1% of cases. Use
#' `max_delay`, which counts the way [tbl_now_to_epinowcast()]'s does:
#'
#' ```r
#' tbl_now_to_baselinenowcast(x, max_delay = 30)   # delays 0-29, 30 columns
#' ```
#'
#' Past a point it stops being about speed. \pkg{baselinenowcast} needs **more
#' reference dates than delay columns** -- it spends `max_delay` of them
#' estimating the delay distribution and keeps two back for the uncertainty
#' model -- so a triangle that is as wide as it is tall cannot be fitted at all.
#' A **snapshot ("as of") series** is exactly that shape: every snapshot
#' restates the whole history, so the oldest event date carries a delay as long
#' as the series and almost every cell of that width is a zero. The converter
#' still builds the triangle; [run_nowcast()] is where it is refused, with the
#' cap to use.
#'
#' @section Negative delays:
#'
#' A reporting triangle is indexed by delay from **0**, so a report that arrived
#' *before* its event has no cell to go in.
#' [baselinenowcast::as_reporting_triangle()] drops it, and the cell then reads
#' `0` -- indistinguishable from an observed zero. Both triangle formats
#' therefore **warn**, naming how many rows and cases go, so the loss is not
#' silent; `format = "long"` is a tidy data frame with no delay axis and keeps
#' them. [tbl_now_to_epinowcast()] drops them the same way, and warns the same
#' way.
#'
#' Filter first if you want to decide what happens:
#'
#' ```r
#' x |> dplyr::filter(.delay >= 0) |> tbl_now_to_baselinenowcast()
#' ```
#'
#' @section Censored delays:
#'
#' A censoring indicator that is a property of the **case** rather than of the
#' delay -- an administrative "this date is only an upper bound" mark, say --
#' puts a censored and an uncensored row in the same
#' `(event_date, report_date)` cell. A reporting triangle has one slot per cell,
#' so the extra dimension has to go before the conversion. It is removed
#' automatically, with a warning either way:
#'
#' * **count data**: the counts are summed over the flag, leaving case totals
#'   unchanged;
#' * **line lists**: the column is dropped, leaving one row per case.
#'
#' [tbl_now_to_epidist()] is the exception and keeps the flag: estimating a
#' delay distribution is the one job that can use it.
#'
#' @seealso
#' [engine_baselinenowcast()][nowcast_engines] to fit through this package
#' rather than converting by hand;
#' [to_count()], because a reporting triangle needs non-negative increments and
#' de-accumulating a revised cumulative series can produce negative ones;
#' [complete_zeroes()] to fill the grid first.
#' [as_tbl_now()] for the generic that dispatches to the `*_from_*()` side;
#' [run_nowcast()], which does the conversion for you when you fit through an
#' [engine()]. The
#' [*One dataset, many nowcasts* article](https://rodrigozepeda.github.io/tbl.now/articles/nowcasting-models.html)
#' fits the same data with every supported package.
#'
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
#' @param event_date,report_date The event- and report-date columns, as
#'   [tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
#'   expressions: a bare column name or a string both work.
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
#' @seealso
#' [as.data.table()][tbl_now_coercion_methods], the \pkg{data.table} method that
#' calls this; [as_tibble()][as_tibble.tbl_now] and
#' [as_tsibble()][tbl_now_coercion_methods] for the other exits from the class;
#' [tbl_now()] to build one from the result.
#' [as_tbl_now()] for the generic that dispatches to the `*_from_*()` side;
#' [run_nowcast()], which does the conversion for you when you fit through an
#' [engine()]. The
#' [*One dataset, many nowcasts* article](https://rodrigozepeda.github.io/tbl.now/articles/nowcasting-models.html)
#' fits the same data with every supported package.
#'
#' @name tbl_now_data_table
#' @export
tbl_now_from_data_table <- function(data, event_date, report_date, ...,
                                    verbose = TRUE) {
  if (!inherits(data, "data.table")) {
    cli::cli_warn(
      "{.arg data} is not a {.cls data.table}; coercing to a data frame."
    )
  }

  # Capture before coercion so a bare column name resolves against the data,
  # the way it does everywhere else in the package. Strings keep working:
  # tidy-select accepts them.
  event_quo <- rlang::enquo(event_date)
  report_quo <- rlang::enquo(report_date)

  observations <- as.data.frame(data)

  event_date <- .converter_select_one(event_quo, observations, "event_date")
  report_date <- .converter_select_one(report_quo, observations, "report_date")

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
#'   `is_censored_report = TRUE` with the report taken from `secondary_upper`.
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
#'   for rows flagged by `is_censored_report` it is left-censored to
#'   `[event_date, report_date]` (the report is known only to have happened at
#'   or before its report date, and cannot precede the event, i.e.
#'   epidist time 0) — encoding the `tbl_now` convention that a censored report
#'   is known only to have happened at or before its report date, so the window
#'   is `[event_date, report_date]`.
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
#' @param quiet Logical. A *different* channel from `verbose`: `verbose`
#'   controls the informational summary of what the conversion did, while `quiet`
#'   suppresses the lossy-conversion **warning** emitted
#'   by `tbl_now_to_epidist()`.
#' @param ... Forwarded to [as_tbl_now()] (`from`) or to the relevant epidist
#'   constructor (`to`).
#'
#' @return A `tbl_now` (`from`) or an `epidist_linelist_data` /
#'   `epidist_aggregate_data` object (`to`).
#'
#' @examplesIf requireNamespace("epidist", quietly = TRUE)
#' ## --- Linelist epidist data (one row per case) ---
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
#' ## --- Aggregate epidist data (counts in an `n` column) ---
#' agg <- epidist::as_epidist_aggregate_data(
#'   data.frame(
#'     pdate_lwr = as.Date(c("2020-03-01", "2020-03-02")),
#'     sdate_lwr = as.Date(c("2020-03-05", "2020-03-04")),
#'     n = c(7, 3)
#'   ),
#'   n = "n", pdate_lwr = "pdate_lwr", sdate_lwr = "sdate_lwr"
#' )
#' ## -> a count-incidence tbl_now (case_count = "n") ...
#' nowagg <- tbl_now_from_epidist(agg)
#' get_data_type(nowagg)
#' ## ... and back to an epidist_aggregate_data (auto-detected from the counts)
#' tbl_now_to_epidist(nowagg)
#' @section Delays of zero, and the lognormal:
#'
#' A delay distribution with a **point mass at zero** cannot be fitted with a
#' lognormal (or a gamma, or a Weibull): all have zero density at zero. If a
#' large share of your cases are reported the same period they occur, the fit
#' does not fail loudly -- it inflates the variance until the density piles up
#' near zero. On a daily COVID series where **57%** of cases carried a delay of
#' exactly 0, `epidist` returned `sigma = 17.9` and an implied mean delay of
#' `1.5e73` days.
#'
#' Check before fitting:
#'
#' ```r
#' mean(as.numeric(x[[get_report_date(x)]] - x[[get_event_date(x)]]) == 0)
#' ```
#'
#' If that share is large, model the delay as **discrete**, use a
#' **zero-inflated**/hurdle form, or fit the continuous distribution to the
#' non-zero delays and report the zero share separately.
#'
#' @section Counts \pkg{epidist} cannot use:
#'
#' `epidist_aggregate_data` requires `n >= 1`, and so does
#' [EpiNow2::estimate_dist()] -- with the identical assertion message. Count data
#' routinely holds rows that violate it:
#'
#' * **zeros** -- an `(event, report)` cell where the report added nothing, which
#'   is most cells once [complete_zeroes()] has run, and which de-accumulating a
#'   `count-cumulative` series produces wherever a cumulative total was
#'   unchanged;
#' * **negatives** -- a cumulative total revised *downward*, which de-accumulates
#'   to a negative increment.
#'
#' Both are dropped before the epidist object is built. A zero contributes no
#' case to a delay distribution, so dropping it is lossless and is only reported
#' when `verbose = TRUE`. A negative is not a number of cases at all, so dropping
#' it discards the revision and **warns**. If nothing usable is left the
#' conversion aborts saying so, rather than letting \pkg{epidist}'s own
#' `Assertion on 'data$n' failed` through.
#'
#' @section Model choice for count data:
#'
#' [epidist::as_epidist_marginal_model()] is the one built for aggregated counts,
#' but with **epidist 0.4.0** and **primarycensored 1.5.1** it fails at Stan
#' compilation (its generated code calls `primarycensored_lpmf()` with 8
#' arguments against a 9-argument signature). The latent model is unaffected but
#' expands counts to **one row per case**, so it is only practical on a short
#' window. Check the epidist issue tracker for the current status.
#'
#' @seealso
#' [add] and [validation_delay], since \pkg{epidist} is about
#' delay distributions and a `tbl_now` may carry two of them;
#' [censor_delays_above()] for the long delays that would otherwise dominate a
#' fitted distribution;
#' [tidy()][tidy.epidist_fit] for the fitted result.
#' [as_tbl_now()] for the generic that dispatches to the `*_from_*()` side;
#' [run_nowcast()], which does the conversion for you when you fit through an
#' [engine()]. The
#' [*One dataset, many nowcasts* article](https://rodrigozepeda.github.io/tbl.now/articles/nowcasting-models.html)
#' fits the same data with every supported package.
#'
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
  # `is_censored_report` for those rows.
  is_censored_report_col <- NULL
  if (secondary_upper %in% colnames(observations) &&
      primary_upper %in% colnames(observations)) {
    window <- as.numeric(observations[[primary_upper]] - observations[[primary]])
    gap    <- as.numeric(observations[[secondary_upper]] - observations[[secondary]])
    # A censored row's secondary window starts at its OWN primary event -- the
    # earliest a report could have happened -- and runs to the report date, so
    # it is wider than one observation period. An uncensored row's window is
    # `[report, report + window)`, which starts later than the event unless the
    # report landed on the event date itself; that one case is genuinely
    # ambiguous and is read as uncensored.
    censored <- (observations[[secondary]] == observations[[primary]]) &
      (gap > window)
    censored[is.na(censored)] <- FALSE
    if (any(censored)) {
      observations[[secondary]][censored] <- observations[[secondary_upper]][censored]
      observations[["is_censored_report"]] <- censored
      is_censored_report_col <- "is_censored_report"
    }
  }

  result <- .build_tbl_now(
    observations, dots = dots,
    event_date = primary, report_date = secondary,
    case_count = count_col, is_censored_report = is_censored_report_col,
    data_type = data_type
  )
  .report_from(
    result, "epidist", verbose,
    extra = paste0(
      "format: ", if (is_aggregate) "aggregate" else "linelist",
      " (lower bounds -> event/report dates", if (is_aggregate) ", n -> case_count",
      if (!is.null(is_censored_report_col)) ", left-censored windows -> is_censored_report", ")"
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
#' @param event_date The event-date column (for `from`), as a
#'   [tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
#'   expression -- a bare column name or a string. Defaults to the
#'   tsibble index.
#' @param report_date The report-date column (required for `from`), as a
#'   [tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
#'   expression -- a bare column name or a string.
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
#' @inheritSection tbl_now_baselinenowcast Censored delays
#' @seealso
#' [as_tsibble()][tbl_now_coercion_methods], the \pkg{tsibble} method that calls
#' this; [to_count()], since a tsibble needs unique index/key rows and a line list
#' has to be aggregated first; [align_weeks()] for regular weekly indexes.
#' [as_tbl_now()] for the generic that dispatches to the `*_from_*()` side;
#' [run_nowcast()], which does the conversion for you when you fit through an
#' [engine()]. The
#' [*One dataset, many nowcasts* article](https://rodrigozepeda.github.io/tbl.now/articles/nowcasting-models.html)
#' fits the same data with every supported package.
#'
#' @name tbl_now_tsibble
#' @export
tbl_now_from_tsibble <- function(data, report_date, event_date = NULL,
                                 strata = NULL, ..., verbose = TRUE) {
  .need_pkg("tsibble")
  if (missing(report_date)) {
    cli::cli_abort("Please supply the {.arg report_date} column name.")
  }

  # Resolved with tidy-select so a bare column name works here as it does in
  # `tbl_now()`. Strings keep working, because tidy-select accepts them.
  report_date <- .converter_select_one(
    rlang::enquo(report_date), as.data.frame(data), "report_date"
  )
  event_quo <- rlang::enquo(event_date)
  if (!rlang::quo_is_null(event_quo)) {
    event_date <- .converter_select_one(
      event_quo, as.data.frame(data), "event_date"
    )
  }
  strata_quo <- rlang::enquo(strata)
  if (!rlang::quo_is_null(strata_quo)) {
    strata <- .converter_select_many(strata_quo, as.data.frame(data))
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
  x <- .tbl_now_collapse_censoring(x, "tbl_now_to_epinowcast")
  .need_pkg("epinowcast")
  .warn_dropped_covariates(
    x, "tbl_now_to_epinowcast",
    advice = "{.pkg epinowcast} builds its own reference/report metadata
              ({.val day_of_week}, {.val day}, {.val week}, {.val month}) and
              does not carry extra columns. Use those in a module formula, e.g.
              {.code enw_reference(~ 1 + day_of_week, data = pobs)}."
  )
  .warn_lossy_conversion("epinowcast", quiet)
  # Warn before the cumulative coercion below: `to_count()` re-derives the grid
  # from delay 0, so afterwards the negative rows are simply not there any more.
  .warn_negative_delays(x, "tbl_now_to_epinowcast")

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
  # is_censored_report has no place in it. The temporal effects are deliberately *not*
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
                                       delays_unit = NULL, max_delay = NULL,
                                       complete = "auto",
                                       negatives = c("redistribute", "error"),
                                       verbose = TRUE) {
  .assert_tbl_now(x, "tbl_now_to_baselinenowcast")
  x <- .tbl_now_collapse_censoring(x, "tbl_now_to_baselinenowcast")
  # A reporting triangle has ONE slot per (event, report) cell, so a column the
  # object was never told about -- `sex` in `covid_colombia` -- leaves two rows
  # per cell and the conversion fails outright. Pool it here rather than making
  # the caller aggregate first.
  x <- .pool_undeclared(x, "tbl_now_to_baselinenowcast", verbose = verbose)
  x <- .cap_max_delay(x, max_delay, "tbl_now_to_baselinenowcast", verbose = verbose)
  format <- match.arg(format)
  negatives <- match.arg(negatives)

  # baselinenowcast needs incremental (count-incidence) counts.
  #  - count-incidence: use as-is.
  #  - linelist: aggregating to incidence is well defined.
  #  - count-cumulative: NOT convertible — cumulative totals get revised
  #    downward (cases un-confirmed), so de-accumulating would yield negative
  #    "incidence". Refuse rather than produce nonsense.
  data_type <- get_data_type(x)
  if (data_type == "count-cumulative") {
    # Cumulative totals get revised downward, so de-accumulating them yields
    # NEGATIVE increments. That is not a reason to refuse: baselinenowcast ships
    # `preprocess_negative_values()` for exactly this case ("reporting
    # corrections that can result in negative incremental counts"), which
    # redistributes a negative back into earlier delays. Do that, and say so.
    if (identical(negatives, "error")) {
      cli::cli_abort(c(
        "Cannot convert {.val count-cumulative} data with \\
         {.code negatives = \"error\"}.",
        "i" = "De-accumulating cumulative totals can give negative incidence.",
        "i" = "Use {.code negatives = \"redistribute\"} to let \\
               {.fn baselinenowcast::preprocess_negative_values} absorb them."
      ))
    }
    cli::cli_warn(c(
      "De-accumulating {.val count-cumulative} data into the increments \\
       {.pkg baselinenowcast} needs.",
      "i" = "Downward revisions become negative increments; they are \\
             redistributed into earlier delays with \\
             {.fn baselinenowcast::preprocess_negative_values}."
    ))
    x <- to_count(x, to = "count-incidence")
  } else if (data_type != "count-incidence") {
    cli::cli_warn(
      "baselinenowcast expects incremental counts; converting {.arg x} to \\
       {.val count-incidence} with {.fn to_count}."
    )
    x <- to_count(x, to = "count-incidence")
  }

  # A reporting triangle is a RECTANGULAR grid, so every event period up to the
  # `now` has to exist -- including periods with no reports at all, which have no
  # rows and so would silently shorten the triangle.
  #
  # Complete those ONLY for line-list input. A line list cannot express the
  # difference between "observed zero" and "not observed": absence of a row means
  # both. Count data CAN -- an explicit NA is a cell that could not be observed
  # yet -- and filling those with 0 would tell the model reporting was complete
  # when it was not. So `"auto"` (the default) completes a line list and leaves
  # counts exactly as supplied; `TRUE`/`FALSE` force either way. The long format
  # is a tidy data frame with no grid to complete, so it is never touched.
  should_complete <- switch(as.character(complete),
    "auto"  = identical(data_type, "linelist"),
    "TRUE"  = TRUE,
    "FALSE" = FALSE,
    identical(data_type, "linelist")
  )
  if (should_complete && format %in% c("matrix", "triangle_list")) {
    # `complete_zeroes()` only knows how to step days, weeks and numeric time.
    # On other units, leave the grid alone rather than abort a conversion that
    # would otherwise succeed.
    x <- tryCatch(
      suppressWarnings(complete_zeroes(x)),
      error = function(e) {
        cli::cli_warn(c(
          "Could not complete missing event periods with zeroes.",
          "i" = conditionMessage(e),
          "i" = "The triangle may stop before the {.field now}."
        ))
        x
      }
    )
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
  strata_cols    <- get_strata(x)

  # The long format is a tidy data frame, so it can also carry the strata, the
  # covariates and the temporal-effect columns (a user can then build one
  # triangle per stratum). The reporting-triangle *matrix* has no strata
  # dimension, so only the three core columns are kept for it and the strata are
  # pooled below. No censoring indicator reaches here: every format ends up in a
  # triangle, which has one slot per cell and nowhere to record a delay that is
  # only an upper bound, so `.tbl_now_collapse_censoring()` has already summed
  # the counts over it.
  if (!identical(format, "long")) {
    .warn_dropped_covariates(
      x, "tbl_now_to_baselinenowcast",
      advice = "A reporting triangle has only (event date, delay) cells, with
                nowhere to put a covariate. {.code format = \"long\"} keeps them
                as columns."
    )
  }

  extra_cols <- if (format == "long") {
    c(strata_cols, covariate_cols, temporal_cols)
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
    # The long format is a tidy data frame with no delay axis, so it keeps them.
    .warn_negative_delays(x, "tbl_now_to_baselinenowcast")
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
    # `diagnose_drift()` uses) -- so the return type never depends on whether
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
  triangle <- .restore_reporting_triangle_na(
    triangle, as.data.frame(na_long), days_per_unit
  )

  # De-accumulated cumulative data can carry negative increments; absorb them
  # into earlier delays rather than handing baselinenowcast a triangle it will
  # reject.
  if (any(triangle < 0, na.rm = TRUE)) {
    triangle <- baselinenowcast::preprocess_negative_values(triangle)
  }
  triangle
}

#' Convert between `tbl_now` and \pkg{EpiNow2}
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' \pkg{EpiNow2} takes four different input shapes, one per entry point, so
#' `tbl_now_to_EpiNow2()` is told which one you want with `target` -- named after
#' the \pkg{EpiNow2} function the result is passed to, so it can be handed over
#' unchanged:
#'
#' \describe{
#'   \item{`"estimate_infections"`}{a `data.frame` of `date` / `confirm`, the
#'     series as known at [get_now()]. Also what [EpiNow2::epinow()] takes.}
#'   \item{`"regional_epinow"`}{the same, plus a `region` column built from the
#'     object's strata.}
#'   \item{`"estimate_truncation"`}{a [tbl_now_epinow2_snapshots] list -- one
#'     `date`/`confirm` snapshot per report date, which is the one \pkg{EpiNow2}
#'     model that uses the report dimension a `tbl_now` exists to carry.}
#'   \item{`"estimate_dist"`}{the interval-censored `pdate_lwr` / `pdate_upr` /
#'     `sdate_lwr` / `sdate_upr` / `obs_date` frame that
#'     [EpiNow2::estimate_dist()] fits a **delay distribution** to (new in
#'     \pkg{EpiNow2} 1.9.0). Count data rides along as the `n` weight column.}
#' }
#'
#' `tbl_now_from_EpiNow2()` inverts the snapshot form: snapshot *k* is the series
#' as known at report date *k*, so differencing consecutive snapshots recovers
#' `count-incidence` exactly. There is deliberately **no** inverse for the other
#' three: a single series has no report dimension to recover, and a delay
#' distribution is not case data.
#'
#' @param x A `tbl_now` object.
#' @param data A [tbl_now_epinow2_snapshots], or a plain list of `date`/`confirm`
#'   data frames (e.g. `EpiNow2::example_truncated`), in which case
#'   `report_dates` is required.
#' @param target Which \pkg{EpiNow2} entry point the result is for. See above.
#' @param snapshots For `"estimate_truncation"`: how many snapshots to emit,
#'   taken from the **latest** report dates. `NULL` (default) uses 5, matching
#'   `EpiNow2::example_truncated`. One snapshot per distinct report date is
#'   usually far more than the model can fit.
#' @param accumulate How to handle non-daily data. `"auto"` (default) lays a
#'   weekly series on \pkg{EpiNow2}'s daily grid with an `accumulate` column;
#'   `FALSE` passes the rows through unchanged, which is almost always wrong (see
#'   *Non-daily data*). Ignored for `"estimate_dist"`, which works in censoring
#'   windows rather than on a grid.
#' @param report_dates For `from`: a `Date` vector, one per snapshot, saying when
#'   each was taken. Read from the object's attribute when it has one.
#' @param verbose Logical. Print the choices that were made.
#' @param quiet Logical. A *different* channel from `verbose`: `verbose`
#'   controls the informational summary of what the conversion did, while `quiet`
#'   suppresses the lossy-conversion warning. Set both to keep a conversion
#'   entirely silent.
#' @param ... Forwarded to [as_tbl_now()] (`from`); unused (`to`).
#'
#' @return For `to`, a `data.frame` or a [tbl_now_epinow2_snapshots], according to
#'   `target`. For `from`, a `tbl_now` of `data_type = "count-incidence"`.
#'
#' @section Non-daily data:
#'
#' \pkg{EpiNow2} models a **daily** process. As of 1.9.0 there is no `timestep`,
#' `interval` or `period` argument on any of its entry points, so a weekly series
#' passed as one row per week is read as one row per **day** and the fit is
#' silently wrong on the time axis -- no error, just an epidemic seven times too
#' fast.
#'
#' Its own answer is the `accumulate` column (see [EpiNow2::fill_missing()]): the
#' series is laid on a daily grid and the filler days are marked to be added to
#' the next real observation. `accumulate = "auto"` does this from
#' [get_event_units()]. Units coarser than a week, and the `"numeric"` grid, are
#' refused outright rather than approximated.
#'
#' @section What EpiNow2 will not take:
#'
#' * [EpiNow2::estimate_secondary()] models **two** data streams (cases and
#'   deaths, say) against each other. One `tbl_now` is one stream, so there is no
#'   honest mapping and no target for it.
#' * [EpiNow2::estimate_delay()] takes a bare vector of delays. Its own help now
#'   points at `estimate_dist()` as "the recommended replacement", and it throws
#'   away the censoring a `tbl_now` carries, so there is no target for it either.
#'   If you want it anyway, it is `x$.delay`.
#'
#' @seealso [tbl_now_to_epidist()], which builds the same censoring windows as
#'   `target = "estimate_dist"` -- the two are different front ends onto one
#'   delay-distribution schema.
#'
#' @examplesIf requireNamespace("EpiNow2", quietly = TRUE)
#' data(denguedat)
#' nowobj <- tbl_now(denguedat[1:2000, ],
#'   event_date = "onset_week", report_date = "report_week", verbose = FALSE
#' )
#' ## A single daily series for estimate_infections() -- the weekly data is laid
#' # on EpiNow2's daily grid.
#' head(tbl_now_to_EpiNow2(nowobj, verbose = FALSE, quiet = TRUE))
#'
#' ## Snapshots for estimate_truncation(), which uses the report dimension.
#' snaps <- tbl_now_to_EpiNow2(nowobj,
#'   target = "estimate_truncation", verbose = FALSE, quiet = TRUE
#' )
#' snaps
#'
#' @name tbl_now_EpiNow2
#' @export
# `tbl_now_to_EpiNow2` is named after the EpiNow2 package (not snake_case).
tbl_now_to_EpiNow2 <- function( # nolint: object_name_linter.
    x, ...,
    target = c("estimate_infections", "regional_epinow",
               "estimate_truncation", "estimate_dist"),
    snapshots = NULL,
    accumulate = "auto",
    verbose = TRUE, quiet = FALSE) {
  .assert_tbl_now(x, "tbl_now_to_EpiNow2")
  target <- match.arg(target)
  .need_pkg("EpiNow2")
  .warn_lossy_conversion("EpiNow2", quiet)

  # `estimate_dist` is the one target that can USE a censoring flag -- it fits an
  # interval-censored delay distribution -- exactly as `tbl_now_to_epidist()` is
  # the exception among the other converters. The three date-keyed targets index
  # by date, so a per-case flag would split a cell in two.
  if (target != "estimate_dist") {
    x <- .tbl_now_collapse_censoring(x, "tbl_now_to_EpiNow2")
  }

  if (target == "estimate_dist") {
    return(.epinow2_dist_data(x, verbose = verbose))
  }

  .warn_dropped_covariates(
    x, "tbl_now_to_EpiNow2",
    advice = "The series targets are {.val date}/{.val confirm} only.
              {.fn EpiNow2::estimate_infections} takes its covariate-like
              structure through {.arg gp}, {.arg rt} and {.arg obs}, not through
              columns."
  )

  .epinow2_series_data(
    x, target = target, snapshots = snapshots, accumulate = accumulate,
    verbose = verbose
  )
}

#' Build the interval-censored frame `EpiNow2::estimate_dist()` fits
#'
#' Shares `.delay_censoring_windows()` with [tbl_now_to_epidist()], so the two
#' cannot drift apart. `obs_date` is the object's own [get_now()] -- the date
#' beyond which nothing had been observed, which is what `estimate_dist()` uses
#' for right truncation and what it would otherwise guess as `max(sdate_upr)`.
#'
#' @param x A `tbl_now`.
#' @param verbose Logical.
#'
#' @return A `data.frame` in `estimate_dist()`'s schema.
#'
#' @keywords internal
#' @noRd
.epinow2_dist_data <- function(x, verbose = TRUE) {
  data_type <- get_data_type(x)
  is_count  <- data_type %in% c("count-incidence", "count-cumulative")

  if (data_type == "count-cumulative") {
    x <- to_count(x, to = "count-incidence")
  }
  if (is_count) {
    # estimate_dist() asserts `n >= 1` with the same message epidist uses.
    x <- .drop_unusable_counts(x, verbose = verbose, target = "EpiNow2")
  }

  materialised  <- .materialize_temporal_effects(x)
  x             <- materialised$x
  temporal_cols <- materialised$cols

  # `estimate_dist()` has no grouping argument -- `args$n` is the observation
  # weight, not a stratum count -- so it fits ONE distribution to everything it
  # is given. The strata columns still ride along (EpiNow2 ignores extras, and a
  # caller can split on them), but the pooling has to be said out loud, as it is
  # for `tbl_now_to_baselinenowcast(format = "matrix")`.
  strata_cols <- get_strata(x)
  if (length(strata_cols) > 0) {
    cli::cli_warn(c(
      "{.fn EpiNow2::estimate_dist} fits a single delay distribution and has no \\
       grouping argument; pooling over strata {.val {strata_cols}}.",
      "i" = "The columns are kept, so you can split and fit per stratum yourself.",
      "i" = "For a delay model that takes covariates directly, see \\
             {.fn tbl_now_to_epidist} and {.pkg epidist}."
    ))
  }

  .warn_zero_delays(x, "EpiNow2::estimate_dist")

  windows <- .delay_censoring_windows(x)
  out     <- windows$data
  obs     <- dplyr::as_tibble(x)

  # `obs_date` is when observation STOPPED, which `estimate_dist()` uses for right
  # truncation and asserts is `>= sdate_upr` on every row.
  #
  # A `tbl_now`'s `now` LABELS a period -- for weekly data it is the start of the
  # last observed week -- while `obs_date` is an instant. The instant observation
  # stopped is therefore the END of the `now` period, `now + win`, and nothing was
  # observed after it. That is exactly the user's rule ("nothing after the now"),
  # read at the resolution the data actually has.
  #
  # It also makes the assertion hold by construction: every report is on or
  # before `now`, so every `sdate_upr` is on or before `now + win`. Setting
  # `obs_date` to the bare `now` instead would abort on any report in the final
  # period, and clamping the windows to fit would move those reports to an
  # earlier period, which is worse than either.
  out$obs_date <- get_now(x) + windows$width
  if (is_count) {
    out$n <- obs[[get_case_count(x)]]
  }

  carry_cols <- c(get_strata(x), get_covariates(x), temporal_cols)
  if (length(carry_cols) > 0) {
    out <- dplyr::bind_cols(out, dplyr::select(obs, dplyr::all_of(carry_cols)))
  }

  if (verbose) {
    cli::cli_h3("Converting {.cls tbl_now} for {.fn EpiNow2::estimate_dist}")
    cli::cli_ul()
    cli::cli_li("pdate_lwr <- {.val {get_event_date(x)}}, \\
                 sdate_lwr <- {.val {get_report_date(x)}}")
    cli::cli_li("censoring window: {.val {windows$width}} day{?s} \\
                 (from {.val {get_event_units(x)}})")
    cli::cli_li("obs_date <- {.val {as.character(out$obs_date[1])}} \
                 (the {.field now}, widened to cover the censoring windows)")
    cli::cli_li("left-censored rows: {.val {sum(windows$censored)}}")
    if (is_count) cli::cli_li("n <- {.val {get_case_count(x)}}")
    if (length(carry_cols) > 0) cli::cli_li("kept columns: {.val {carry_cols}}")
    cli::cli_end()
  }

  as.data.frame(out)
}

#' Build the date/confirm series (or snapshots) the count targets take
#'
#' @param x A `tbl_now`.
#' @param target One of the three date-keyed targets.
#' @param snapshots Snapshot count for `estimate_truncation`.
#' @param accumulate `"auto"`, `TRUE` or `FALSE`.
#' @param verbose Logical.
#'
#' @return A `data.frame`, or a `tbl_now_epinow2_snapshots` list.
#'
#' @keywords internal
#' @noRd
.epinow2_series_data <- function(x, target, snapshots, accumulate, verbose) {
  event_col   <- get_event_date(x)
  strata_cols <- get_strata(x)
  event_units <- get_event_units(x)

  # Resolve the grid BEFORE any work: on units EpiNow2 cannot lay on a daily
  # axis there is nothing to build, and failing early beats failing after an
  # expensive aggregation.
  should_accumulate <- switch(as.character(accumulate),
    "auto" = TRUE, "TRUE" = TRUE, "FALSE" = FALSE, TRUE
  )
  if (should_accumulate) .epinow2_step_days(event_units)

  if (target != "regional_epinow" && length(strata_cols) > 0) {
    cli::cli_warn(c(
      "{.fn EpiNow2::{target}} models a single series; pooling over strata \\
       {.val {strata_cols}}.",
      "i" = "For a fit per stratum use {.code target = \"regional_epinow\"}, \\
             which carries them as a {.field region} column."
    ))
  }

  # `get_latest_reported_cases()` / `get_nth_reported_cases()` know about all
  # three data types and the `now` edge; re-deriving the snapshot by hand is how
  # the previous attempt at this converter went wrong.
  as_series <- function(snapshot, keep_strata) {
    frame <- dplyr::as_tibble(snapshot)
    count_col <- get_case_count(snapshot) %||% ".n"
    if (!count_col %in% names(frame)) frame[[count_col]] <- 1
    group_cols <- c(event_col, if (keep_strata) strata_cols)
    frame |>
      dplyr::summarise(
        confirm = sum(.data[[count_col]], na.rm = TRUE),
        .by = dplyr::all_of(group_cols)
      ) |>
      dplyr::rename(date = dplyr::all_of(event_col)) |>
      dplyr::arrange(.data$date) |>
      as.data.frame()
  }

  if (target == "estimate_truncation") {
    return(.epinow2_snapshots(
      x, as_series = as_series, snapshots = snapshots,
      should_accumulate = should_accumulate, event_units = event_units,
      verbose = verbose
    ))
  }

  keep_strata <- identical(target, "regional_epinow")
  series <- as_series(suppressMessages(get_latest_reported_cases(x)), keep_strata)

  by <- NULL
  if (keep_strata) {
    series$region <- .epinow2_region(series, strata_cols)
    series <- series[, c("date", "confirm", "region")]
    by <- "region"
  }
  # A downward revision de-accumulates to a negative, which `obs_opts()` has no
  # way to represent.
  negative <- series$confirm < 0
  if (any(negative)) {
    cli::cli_warn(c(
      "{.val {sum(negative)}} date{?s} carr{?ies/y} a negative count after \\
       de-accumulation; clamping to {.val 0}.",
      "i" = "{.pkg EpiNow2}'s observation model cannot represent a negative \\
             count. The downward revision is lost."
    ))
    series$confirm[negative] <- 0
  }

  if (should_accumulate) {
    series <- .epinow2_grid(series, event_units, by = by)
  }

  if (verbose) {
    cli::cli_h3("Converting {.cls tbl_now} for {.fn EpiNow2::{target}}")
    cli::cli_ul()
    cli::cli_li("date <- {.val {event_col}}, confirm <- latest reported counts")
    if (keep_strata) {
      cli::cli_li("region <- {.val {strata_cols}}")
    }
    cli::cli_li("grid: {.val {if (identical(event_units, 'days')) 'daily' else
                 paste0('daily, accumulated from ', event_units)}}")
    cli::cli_li("rows: {.val {nrow(series)}}")
    cli::cli_end()
  }

  series
}

#' Build one `date`/`confirm` snapshot per report date
#'
#' @param x A `tbl_now`.
#' @param as_series Function turning a snapshot `tbl_now` into a series.
#' @param snapshots How many to emit, latest report dates first.
#' @param should_accumulate,event_units Passed to `.epinow2_grid()`.
#' @param verbose Logical.
#'
#' @return A `tbl_now_epinow2_snapshots`.
#'
#' @keywords internal
#' @noRd
.epinow2_snapshots <- function(x, as_series, snapshots, should_accumulate,
                               event_units, verbose) {
  report_col <- get_report_date(x)
  all_dates  <- sort(unique(x[[report_col]]))
  # EpiNow2's own `example_truncated` ships five. One snapshot per report date is
  # what the first attempt at this converter did, and on a multi-year daily
  # series that is ~1000 Stan data sets and the fit does not finish.
  n_keep <- snapshots %||% 5L
  keep   <- utils::tail(all_dates, n_keep)

  pieces <- lapply(keep, function(as_of) {
    snapshot <- suppressMessages(
      dplyr::filter(x, .data[[report_col]] <= as_of)
    )
    # Every snapshot must carry "a complete vector of dates" (?estimate_truncation),
    # so fill the event periods that had no reports yet with zeroes.
    snapshot <- tryCatch(
      suppressWarnings(suppressMessages(complete_zeroes(snapshot, until = as_of))),
      error = function(e) snapshot
    )
    series <- as_series(suppressMessages(get_latest_reported_cases(snapshot)), FALSE)
    series$confirm[series$confirm < 0] <- 0
    if (should_accumulate) series <- .epinow2_grid(series, event_units)
    series
  })
  # `get_predictions.estimate_truncation()` reorders by nrow; emitting them
  # shortest-to-longest keeps the caller's indices matching the fit's.
  ord <- order(vapply(pieces, nrow, integer(1)))

  if (verbose) {
    cli::cli_h3("Converting {.cls tbl_now} for {.fn EpiNow2::estimate_truncation}")
    cli::cli_ul()
    cli::cli_li("snapshots: {.val {length(pieces)}} of \\
                 {.val {length(all_dates)}} report date{?s}")
    cli::cli_li("report dates: {.val {as.character(keep)}}")
    cli::cli_li("rows per snapshot: {.val {vapply(pieces[ord], nrow, integer(1))}}")
    cli::cli_end()
  }

  structure(
    pieces[ord],
    class        = "tbl_now_epinow2_snapshots",
    report_dates = keep[ord],
    now          = get_now(x),
    event_col    = get_event_date(x),
    report_col   = report_col,
    event_units  = event_units
  )
}

#' Snapshots of one series, as \pkg{EpiNow2} estimates truncation from
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' The object returned by
#' `tbl_now_to_EpiNow2(x, target = "estimate_truncation")`: a list of
#' `date`/`confirm` data frames, one per report date, plus the report dates
#' themselves so the object can be turned back into a `tbl_now`.
#'
#' It is a **thin** class -- still a list, so it can be handed to
#' [EpiNow2::estimate_truncation()] unchanged:
#'
#' ```r
#' snaps <- tbl_now_to_EpiNow2(x, target = "estimate_truncation")
#' EpiNow2::estimate_truncation(snaps)
#' ```
#'
#' The class exists because a bare list of `date`/`confirm` frames does not say
#' *when* each snapshot was taken, and without that the reporting triangle cannot
#' be recovered from it. Printing also distinguishes it from the superficially
#' similar list [EpiNow2::estimate_secondary()] does *not* take.
#'
#' @param x A `tbl_now_epinow2_snapshots`.
#' @param ... Ignored.
#'
#' @return `print()` returns `x` invisibly.
#'
#' @seealso [tbl_now_to_EpiNow2()], [as_tbl_now()]
#'
#' @examplesIf requireNamespace("EpiNow2", quietly = TRUE)
#' data(denguedat)
#' dengue <- tbl_now(denguedat[1:3000, ],
#'   event_date = onset_week, report_date = report_week, verbose = FALSE
#' )
#'
#' # A stack of snapshots: what the series looked like at each of several past
#' ## report dates. EpiNow2::estimate_truncation() uses these to learn how much
#' # the most recent counts are still going to grow.
#' snaps <- tbl_now_to_EpiNow2(dengue,
#'   target = "estimate_truncation", verbose = FALSE, quiet = TRUE
#' )
#'
#' # Printing summarises the stack rather than dumping every snapshot.
#' snaps
#'
#' length(snaps)
#' head(snaps[[1]])
#'
#' @name tbl_now_epinow2_snapshots
NULL

#' @rdname tbl_now_epinow2_snapshots
#' @exportS3Method base::print
print.tbl_now_epinow2_snapshots <- function(x, ...) {
  # NOTE: a print method must write to STDOUT; the `cli_*()` family emits
  # *messages*, which vanish under `message = FALSE` or `capture.output()`.
  report_dates <- attr(x, "report_dates")
  cli::cat_rule(
    left = cli::format_inline(
      "{length(x)} reporting snapshot{?s} from a {.cls tbl_now}"
    )
  )
  cli::cat_bullet(c(
    cli::format_inline(
      "One per report date: {.val {as.character(report_dates)}}"
    ),
    cli::format_inline("Rows each: {.val {unname(vapply(x, nrow, integer(1)))}}"),
    cli::format_inline("Now: {.val {as.character(attr(x, 'now'))}}")
  ))
  cli::cat_line(cli::format_inline(paste0(
    "{cli::symbol$info} Pass this to {.fn EpiNow2::estimate_truncation}. ",
    "{.fn EpiNow2::estimate_secondary} wants a single data frame of ",
    "linked series instead -- not this."
  )))
  invisible(x)
}

#' @rdname tbl_now_EpiNow2
#' @export
tbl_now_from_EpiNow2 <- function(data, ..., report_dates = NULL, # nolint: object_name_linter.
                                 verbose = TRUE) {
  .need_pkg("EpiNow2")
  if (!is.list(data) || is.data.frame(data)) {
    cli::cli_abort(c(
      "{.arg data} must be a list of {.pkg EpiNow2} snapshots.",
      "i" = "A single {.fn EpiNow2::estimate_infections} series carries no \\
             report dimension, so it cannot be turned back into a {.cls tbl_now}."
    ))
  }

  # Read every attribute BEFORE any subsetting: `data[ord]` below returns a plain
  # list, dropping the class and everything attached to it.
  stored_now   <- attr(data, "now")
  report_dates <- report_dates %||% attr(data, "report_dates")
  if (is.null(report_dates)) {
    cli::cli_abort(c(
      "{.arg report_dates} is required for a plain list of snapshots.",
      "i" = "A {.cls date}/{.cls confirm} frame does not record WHEN the \\
             snapshot was taken, and without that the reporting triangle \\
             cannot be recovered.",
      "i" = "An object from {.code tbl_now_to_EpiNow2(target = \"estimate_truncation\")} \\
             carries them already."
    ))
  }
  report_dates <- as.Date(report_dates)
  if (length(report_dates) != length(data)) {
    cli::cli_abort(
      "{.arg report_dates} has {.val {length(report_dates)}} value{?s} but \\
       {.arg data} has {.val {length(data)}} snapshot{?s}."
    )
  }

  # Order matters: snapshot k must be differenced against snapshot k-1, so sort
  # by report date rather than trusting the list order.
  ord          <- order(report_dates)
  data         <- data[ord]
  report_dates <- report_dates[ord]

  # Each snapshot is CUMULATIVE-to-date, so the rows a report date contributes
  # are its snapshot minus the one before it. Written as a loop rather than
  # `lapply()` with `<<-`: the superassignment worked, but it reads as a global
  # write and `checktor` flags it as one.
  pieces   <- vector("list", length(data))
  previous <- NULL
  for (k in seq_along(data)) {
    current <- as.data.frame(data[[k]])
    # Undo the daily expansion, where there was one. `EpiNow2::fill_missing()`
    # leaves each observation on the date it was given and marks the filler days
    # around it, so dropping the filler rows recovers the original grid exactly --
    # no date arithmetic, and nothing to get off by six.
    if ("accumulate" %in% names(current)) {
      current <- current[!current$accumulate, , drop = FALSE]
    }
    current <- current[, c("date", "confirm")]
    current$date <- as.Date(current$date)
    increment <- current
    if (!is.null(previous)) {
      before <- previous$confirm[match(current$date, previous$date)]
      before[is.na(before)] <- 0
      increment$confirm <- current$confirm - before
    }
    previous <- current
    increment$report_date <- report_dates[k]
    pieces[[k]] <- increment
  }
  long <- do.call(rbind, pieces)
  names(long)[names(long) == "date"]    <- "reference_date"
  names(long)[names(long) == "confirm"] <- "count"
  rownames(long) <- NULL

  dots <- list(...)
  if (is.null(dots$now)) dots$now <- stored_now %||% max(report_dates)

  result <- .build_tbl_now(
    long,
    dots = dots,
    event_date = "reference_date",
    report_date = "report_date",
    case_count = "count",
    data_type = "count-incidence"
  )
  result <- .drop_zero_counts(result)

  .report_from(
    result, "EpiNow2", verbose,
    extra = paste0(
      "differenced ", length(data), " snapshots into incremental counts"
    )
  )
  result
}

#' @rdname as_tbl_now
#' @export
as_tbl_now.tbl_now_epinow2_snapshots <- function(object, ...) {
  event_col  <- attr(object, "event_col")
  report_col <- attr(object, "report_col")

  # `verbose` goes through `dots`, not alongside `...`: passing both makes
  # `as_tbl_now(x, verbose = FALSE)` fail with "formal argument 'verbose'
  # matched by multiple actual arguments". Quiet by default, but the caller
  # still wins.
  dots <- list(...)
  if (is.null(dots$verbose)) dots$verbose <- FALSE
  result <- do.call(tbl_now_from_EpiNow2, c(list(object), dots))

  # Back to the caller's own column names.
  out <- dplyr::as_tibble(result)
  names(out)[names(out) == "reference_date"] <- event_col
  names(out)[names(out) == "report_date"]    <- report_col

  # Same trap again, and this is the one that actually bit: `verbose` fixed in
  # the list AND present in `list(...)`.
  rebuild <- dots
  if (is.null(rebuild$now)) rebuild$now <- attr(object, "now")

  do.call(
    tbl_now,
    c(
      list(
        as.data.frame(out[, c(event_col, report_col, "count")]),
        event_date  = event_col,
        report_date = report_col,
        case_count  = "count",
        data_type   = "count-incidence"
      ),
      rebuild
    )
  )
}

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
       temporal-effect columns and is_censored_report)."
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

  # A converter returns a foreign object, so the caller's grouping has nowhere
  # to go -- but left on, it makes the `keep` mutate below run once per group
  # and abort. Every other converter already tolerates a grouped input; this one
  # did not, and the error named `keep` rather than the grouping.
  x <- ungroup(x)

  # Materialise the lazy temporal-effect columns so they are carried as extra
  # covariate columns on the epidist data.
  materialised   <- .materialize_temporal_effects(x)
  x              <- materialised$x
  temporal_cols  <- materialised$cols

  covariate_cols <- get_covariates(x)
  censored_col   <- get_is_censored_report(x)
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

  # ... and it must be at least 1. De-accumulation makes zeros wherever a report
  # added nothing and negatives on a downward revision, and plain incidence data
  # carries zeros too (`complete_zeroes()` puts them there on purpose). Drop what
  # epidist cannot hold, here, rather than let its assertion fire.
  if (format == "aggregate") {
    x <- .drop_unusable_counts(x, verbose = verbose)
  }

  event_col  <- get_event_date(x)
  report_col <- get_report_date(x)
  count_col  <- get_case_count(x)
  units      <- get_event_units(x)
  obs        <- dplyr::as_tibble(x)

  windows      <- .delay_censoring_windows(x, censoring_window)
  epidist_data <- windows$data
  win          <- windows$width
  censored     <- windows$censored

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
    cli::cli_li("left-censored rows ({.field is_censored_report}): {.val {sum(censored)}}")
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
  # A `tsibble` is invalid unless (key, index) is unique, and an undeclared
  # column puts two rows in the same slot. Pool it away, as the other converters
  # now do.
  x <- .pool_undeclared(x, "tbl_now_to_tsibble", verbose = verbose)
  x <- .tbl_now_collapse_censoring(x, "tbl_now_to_tsibble")
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
  count_col      <- get_case_count(x)

  # The chosen date is the tsibble index; the other date plus the strata form
  # the key (so the index/key combination is unique). A censoring indicator
  # would break exactly that uniqueness, which is why
  # `.tbl_now_collapse_censoring()` has already removed it above.
  index_col <- if (index == "report_date") report_col else event_col
  other_col <- if (index == "report_date") event_col else report_col
  key_cols  <- c(other_col, strata_cols)

  # Covariates, the temporal-effect columns and the case count ride along as
  # measurement columns; the tbl_now internals are dropped.
  kept_cols <- c(
    index_col, other_col, strata_cols, covariate_cols, temporal_cols,
    count_col
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
#' \pkg{surveillance} models an individual-level line
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


#' Convert a `tbl_now` into the line list \pkg{NobBS} nowcasts from
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [NobBS::NobBS()] counts **rows**: it takes an individual-level line list with
#' one column for the event date and one for the report date, and treats each row
#' as a case. Handing it `count-incidence` data directly is therefore silently
#' wrong -- a table of 1,174 rows carrying 50,160 cases is nowcast as 1,174
#' cases. This converter expands counts to one row per case first, so the totals
#' NobBS sees are the totals in your data.
#'
#' Trim **before** converting when the series is long: the expansion is one row
#' per case, and `NobBS()`'s own `moving_window` only limits what it *fits*, not
#' what it is handed.
#'
#' @inheritSection tbl_now_baselinenowcast Censored delays
#'
#' @param x A `tbl_now`.
#' @param event_col,report_col Names the two date columns should take in the
#'   result. The defaults match the arguments of [NobBS::NobBS()].
#' @param strata_col Name of the single stratifying column to add, holding every
#'   declared stratum pasted together. This is what [NobBS::NobBS.strat()]'s own
#'   `strata` argument takes. `NULL` leaves it out. Ignored when the object
#'   declares no strata.
#' @param strata_sep Separator used to paste the strata into `strata_col`.
#' @param verbose Print what the conversion did. The `units` line prints the
#'   string [NobBS::NobBS()] itself accepts (`"1 day"` or `"1 week"`), not the
#'   object's own `"days"` / `"weeks"`, so it can be pasted straight into the
#'   call.
#' @param ... Unused, for extensibility.
#'
#' @section Stratified nowcasts:
#'
#' [NobBS::NobBS.strat()] fits one nowcast per stratum, and its `strata`
#' argument names **one** column. A `tbl_now` may declare several -- age group
#' and region, say -- and "nowcast each age-group-and-region separately" is a
#' single stratum as far as NobBS is concerned. So the declared columns are also
#' pasted into one `strata` column, which you hand straight to `NobBS.strat()`:
#'
#' ```r
#' nb <- tbl_now_to_nobbs(x, verbose = FALSE)
#' NobBS::NobBS.strat(nb, now = get_now(x), units = "1 day",
#'                    onset_date = "onset_date", report_date = "report_date",
#'                    strata = "strata")
#' ```
#'
#' The original columns are kept alongside it, so a hand-rolled per-stratum loop
#' can still split on them. Choose a separator your stratum values do not
#' contain: `run_nowcast()` splits the label back into the original columns when
#' it tidies the fit.
#'
#' @section Units NobBS can model:
#'
#' [NobBS::NobBS()] documents `units` as `"1 day"` or `"1 week"` and nothing
#' else, so this converter aborts on any other grid rather than hand back a line
#' list \pkg{NobBS} cannot use. That includes a `"numeric"` grid: its date
#' columns are integer indices, and coercing them with `as.Date()` would anchor
#' them at the 1970 epoch and return a plausible-looking line list of invented
#' dates. Aggregate to days or weeks first (see [align_weeks()]).
#'
#' @returns A `data.frame` with one row per case, ready for [NobBS::NobBS()].
#'   The strata, covariates and temporal-effect columns ride along, plus the
#'   single pasted `strata_col` that [NobBS::NobBS.strat()] takes.
#'
#' @seealso [tbl_now_to_surveillance()], [tbl_now_to_epinowcast()]
#'
#' @examplesIf requireNamespace("NobBS", quietly = TRUE)
#' data(denguedat)
#' nowobj <- tbl_now(denguedat,
#'   event_date = "onset_week", report_date = "report_week", verbose = FALSE
#' )
#' nb <- tbl_now_to_nobbs(nowobj, verbose = FALSE)
#' head(nb)
#'
#' @name tbl_now_nobbs
#' @export
tbl_now_to_nobbs <- function(x, ..., event_col = "onset_date",
                             report_col = "report_date",
                             strata_col = "strata", strata_sep = " | ",
                             verbose = TRUE) {
  .assert_tbl_now(x, "tbl_now_to_nobbs")
  x <- .tbl_now_collapse_censoring(x, "tbl_now_to_nobbs")

  # Resolve the units NobBS will need before doing any work: it accepts "1 day"
  # or "1 week" only, and anything else has to fail here rather than produce a
  # line list NobBS will choke on (or, on a numeric grid, one full of invented
  # 1970 dates).
  nobbs_units <- .nobbs_units(get_event_units(x))

  event_date_col  <- get_event_date(x)
  report_date_col <- get_report_date(x)
  strata_cols     <- get_strata(x)
  covariate_cols  <- get_covariates(x)

  materialised  <- .materialize_temporal_effects(x)
  x             <- materialised$x
  temporal_cols <- materialised$cols

  # The whole point: counts become one row per case.
  linelist <- .tbl_now_expand_to_linelist(x, "tbl_now_to_nobbs")

  kept <- c(
    event_date_col, report_date_col, strata_cols, covariate_cols, temporal_cols
  )
  linelist <- as.data.frame(linelist[, kept, drop = FALSE])

  names(linelist)[match(event_date_col, names(linelist))]  <- event_col
  names(linelist)[match(report_date_col, names(linelist))] <- report_col

  linelist[[event_col]]  <- as.Date(linelist[[event_col]])
  linelist[[report_col]] <- as.Date(linelist[[report_col]])

  # `NobBS.strat()` takes ONE column name, so the declared strata are pasted
  # into one here rather than left for the caller to work out. The originals
  # stay put: a hand-rolled per-stratum loop still wants them.
  stratified <- length(strata_cols) > 0 && !is.null(strata_col)
  linelist <- .add_strata_column(
    linelist, strata_cols, strata_col, strata_sep, "tbl_now_to_nobbs"
  )

  if (verbose) {
    cli::cli_h3("Converting {.cls tbl_now} into a {.pkg NobBS} line list")
    cli::cli_ul()
    cli::cli_li("{.arg onset_date} <- {.val {event_col}}")
    cli::cli_li("{.arg report_date} <- {.val {report_col}}")
    cli::cli_li("{.arg units} <- {.val {nobbs_units}}")
    if (stratified) {
      cli::cli_li(
        "{.arg strata} <- {.val {strata_col}} \\
         ({.val {strata_cols}}, {.val {strata_sep}}-separated)"
      )
    }
    cli::cli_li("rows (one per case): {.val {nrow(linelist)}}")
    cli::cli_end()
  }

  linelist
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
#' With `format = "linelist_list"` it returns **one line list per stratum** as a
#' [tbl_now_surveillance_list], ready to `lapply()` over --
#' [surveillance::nowcast()] has no strata argument, so a stratified analysis is
#' one fit per stratum and this saves splitting by hand.
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
#' @param format One of
#'   * `"linelist"` (default) -- the single data frame
#'     [surveillance::nowcast()] expects;
#'   * `"linelist_list"` -- one line list **per stratum**, as a
#'     [tbl_now_surveillance_list]. Still a plain list, so it goes straight into
#'     `lapply()`; length one and named `"all"` when the object declares no
#'     strata, so the return type does not depend on whether strata happen to be
#'     attached;
#'   * `"sts"` -- an [surveillance::sts] object of the observed curve.
#' @param aggregate_by Aggregation interval, e.g. `"1 week"`. `NULL` (default)
#'   derives it from the object's event units (`"days"` -> `"1 day"`, `"weeks"`
#'   -> `"1 week"`, `"months"` -> `"1 month"`, `"years"` -> `"1 year"`), and
#'   aborts on a `"numeric"` grid, which has integer indices rather than the
#'   calendar dates [surveillance::linelist2sts()] needs. Pass a value
#'   explicitly to override, including on a numeric grid if you know what the
#'   index steps mean.
#' @param strata_col Name of a single column to add, holding every declared
#'   stratum pasted together, for splitting the line list into one fit per
#'   stratum. `NULL` leaves it out. Ignored when the object declares no strata.
#' @param strata_sep Separator used to paste the strata into `strata_col`.
#' @param verbose Logical. Print the choices that were made.
#' @param ... Forwarded to [surveillance::linelist2sts()] when
#'   `format = "sts"`; ignored otherwise.
#'
#' @return A `data.frame` line list (`format = "linelist"`), a
#'   [tbl_now_surveillance_list] (`format = "linelist_list"`) or an
#'   [surveillance::sts] object (`format = "sts"`).
#'
#' @seealso [tbl_now_to_epinowcast()], [tbl_now_surveillance_list]
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
#' @inheritSection tbl_now_baselinenowcast Censored delays

#' @section Stratified nowcasts:
#'
#' [surveillance::nowcast()] models one series and has no strata argument, so a
#' stratified analysis means fitting each stratum separately.
#' `format = "linelist_list"` does the splitting, so the fit is an `lapply()`:
#'
#' ```r
#' pieces <- tbl_now_to_surveillance(x, format = "linelist_list", verbose = FALSE)
#' fits <- lapply(pieces, function(piece) {
#'   surveillance::nowcast(
#'     now = get_now(x), when = get_surveillance_when(x),
#'     data = piece, dEventCol = "dHospital", dReportCol = "dReport",
#'     control = list(dRange = get_surveillance_range(x))
#'   )
#' })
#' ```
#'
#' The `control$dRange` comes from the **whole object**, not from the piece:
#' every stratum has to be laid on the same time axis, or a stratum whose first
#' case arrived late starts its own time on a different day.
#'
#' The default `format = "linelist"` keeps the same information in one frame:
#' the declared strata are pasted into a single `strata` column, so
#' `split(sur, sur$strata)` reproduces the list. The original columns are kept
#' alongside it, so you can split on them instead.
#'
#' @section Cost of expanding counts:
#'
#' \pkg{surveillance} counts rows, so `count-incidence` input is expanded to one
#' row per case. **Trim before converting** on a large series: the package's own
#' windowing arguments (`when`, `control$dRange`) limit what it *fits*, not what
#' it is handed, and a multi-year daily series can expand into millions of rows.
#'
#' @name tbl_now_surveillance
#' @export
tbl_now_to_surveillance <- function(x, ..., event_col = "dHospital",
                                    report_col = "dReport",
                                    format = c("linelist", "linelist_list",
                                               "sts"),
                                    aggregate_by = NULL,
                                    strata_col = "strata", strata_sep = " | ",
                                    verbose = TRUE) {
  .assert_tbl_now(x, "tbl_now_to_surveillance")
  x <- .tbl_now_collapse_censoring(x, "tbl_now_to_surveillance")
  .need_pkg("surveillance")
  format <- match.arg(format)

  # Resolve the aggregation interval BEFORE expanding to a line list: on a
  # `"numeric"` grid there are no dates to aggregate, and the expansion would
  # otherwise coerce the integer indices with `as.Date()` and hand back a
  # perfectly plausible-looking 1970 outbreak.
  if (is.null(aggregate_by)) {
    aggregate_by <- .surveillance_aggregate_by(get_event_units(x))
  }

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

  # `surveillance::nowcast()` has no strata argument at all: a stratified
  # analysis means splitting the line list and fitting each piece. One column
  # holding the whole combination is what `split()` wants, so it is built here
  # rather than left to the caller.
  stratified <- length(strata_cols) > 0 && !is.null(strata_col)
  linelist <- .add_strata_column(
    linelist, strata_cols, strata_col, strata_sep, "tbl_now_to_surveillance"
  )

  if (verbose) {
    cli::cli_h3("Converting {.cls tbl_now} into a {.pkg surveillance} line list")
    cli::cli_ul()
    cli::cli_li("{.arg dEventCol} <- {.val {event_col}}")
    cli::cli_li("{.arg dReportCol} <- {.val {report_col}}")
    cli::cli_li("{.arg aggregate.by} <- {.val {aggregate_by}}")
    cli::cli_li("{.arg now} <- {.val {as.character(get_now(x))}}")
    if (stratified) {
      cli::cli_li(
        "{.field {strata_col}} <- {.val {strata_cols}}, \\
         {.val {strata_sep}}-separated (split on it for a fit per stratum)"
      )
    }
    cli::cli_end()
  }

  if (format == "sts") {
    return(surveillance::linelist2sts(
      linelist, dateCol = event_col, aggregate.by = aggregate_by, ...
    ))
  }

  if (format == "linelist_list") {
    # Split on the strata VALUES rather than on `strata_col`: that column is
    # optional (`strata_col = NULL` drops it) and the list has to come out the
    # same either way. With no strata the labels are all "all", so the result is
    # still a LIST -- of length one -- and the return type never depends on
    # whether strata happen to be attached.
    labels <- .paste_strata_labels(
      linelist, strata_cols, sep = strata_sep, fn = "tbl_now_to_surveillance()"
    )
    pieces <- lapply(
      split(seq_len(nrow(linelist)), labels),
      function(rows) {
        piece <- linelist[rows, , drop = FALSE]
        rownames(piece) <- NULL
        piece
      }
    )
    return(structure(
      pieces,
      class             = "tbl_now_surveillance_list",
      strata_cols       = strata_cols,
      covariate_cols    = covariate_cols,
      strata_sep        = strata_sep,
      # The pasted label column, when there is one. `as_tbl_now()` drops it: it
      # is derived from `strata_cols`, which are still there.
      strata_col        = if (stratified) strata_col else NULL,
      now               = get_now(x),
      # BOTH pairs are needed. The first says what the columns are called in the
      # frames -- what `dEventCol`/`dReportCol` must be given; the second what
      # they were called in the `tbl_now`, so `as_tbl_now()` can put them back.
      event_col         = event_col,
      report_col        = report_col,
      source_event_col  = event_date_col,
      source_report_col = report_date_col,
      event_units       = get_event_units(x),
      report_units      = get_report_units(x),
      aggregate_by      = aggregate_by
    ))
  }

  linelist
}





#' The date grids [surveillance::nowcast()] needs
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [surveillance::nowcast()] takes three dates and two date *grids*, and none of
#' them have defaults you can rely on. These two helpers build the grids from the
#' `tbl_now` itself, so the object stays the single source of truth for what
#' "now" is and how wide a time step is:
#'
#' * `get_surveillance_when()` -- the dates you want **estimated**, passed as
#'   `when`. The most recent `length` steps up to and including [get_now()].
#' * `get_surveillance_range()` -- the **whole** time axis the model is laid on,
#'   passed as `control$dRange`. Every step from the first event to `now`.
#'
#' ```r
#' sur_fit <- surveillance::nowcast(
#'   now  = get_now(x),
#'   when = get_surveillance_when(x, length = 30),
#'   data = tbl_now_to_surveillance(x, verbose = FALSE),
#'   dEventCol = "dHospital", dReportCol = "dReport",
#'   control = list(dRange = get_surveillance_range(x))
#' )
#' ```
#'
#' @section Why `dRange` has to be given explicitly:
#'
#' Left to itself, [surveillance::nowcast()] infers the time axis from the data
#' it was handed -- and a **line list cannot express a zero**. A day on which
#' nothing was reported has no rows, so it is not in the line list, so it is not
#' in the inferred axis. That is exactly the situation at the `now` edge, which
#' is the part you are nowcasting: the last few days are quiet precisely because
#' their reports have not arrived yet, and the axis silently stops short of
#' `now`. Passing `dRange` states the grid instead of letting it be guessed, so
#' the quiet days at the end are modelled as zeros observed so far rather than
#' as days that do not exist.
#'
#' This is also why [complete_zeroes()] is no help here: it can only add zero
#' *counts*, and a line list has no count column to put a zero in.
#'
#' @param x A `tbl_now`.
#' @param length Number of time steps to estimate, counting back from `to`. The
#'   result has `length` elements, the last of which is `to`.
#' @param from First date of the grid. Defaults to the earliest event date in
#'   `x`.
#' @param to Last date of the grid. Defaults to [get_now()].
#' @param by Step, as a [seq.Date()] `by` string (`"1 day"`, `"1 week"`, ...).
#'   Defaults to the object's own event units.
#' @param ... Unused, for extensibility.
#'
#' @returns A `Date` vector, in increasing order.
#'
#' @seealso [tbl_now_to_surveillance()], [get_now()], [get_event_units()]
#'
#' @examplesIf requireNamespace("surveillance", quietly = TRUE)
#' data(denguedat)
#' nowobj <- tbl_now(denguedat,
#'   event_date = "onset_week", report_date = "report_week", verbose = FALSE
#' )
#' get_surveillance_when(nowobj, length = 4)
#' range(get_surveillance_range(nowobj))
#'
#' @name surveillance_grids
NULL

#' @rdname surveillance_grids
#' @export
get_surveillance_when <- function(x, length = 30L, ..., to = NULL, by = NULL) {
  .assert_tbl_now(x, "get_surveillance_when")
  if (!is.numeric(length) || length(length) != 1L || is.na(length) ||
        length < 1) {
    cli::cli_abort(
      "{.arg length} must be a single positive number, not {.val {length}}."
    )
  }
  by <- by %||% .surveillance_aggregate_by(get_event_units(x))
  to <- .surveillance_grid_date(to %||% get_now(x), "to")

  # `seq()` counting BACK from `to` rather than forward from a computed start:
  # on months the two disagree (a month is not a fixed number of days), and it
  # is the `now` end of the grid that has to land exactly.
  grid <- seq(to, by = paste0("-", by), length.out = as.integer(length))
  sort(grid)
}

#' @rdname surveillance_grids
#' @export
get_surveillance_range <- function(x, ..., from = NULL, to = NULL, by = NULL) {
  .assert_tbl_now(x, "get_surveillance_range")
  by <- by %||% .surveillance_aggregate_by(get_event_units(x))
  to <- .surveillance_grid_date(to %||% get_now(x), "to")

  from <- from %||% suppressWarnings(min(x[[get_event_date(x)]], na.rm = TRUE))
  if (!is.finite(unclass(from))) {
    cli::cli_abort(c(
      "{.arg x} has no non-missing event date to start the grid from.",
      "i" = "Pass {.arg from} explicitly."
    ))
  }
  from <- .surveillance_grid_date(from, "from")

  if (from > to) {
    cli::cli_abort(c(
      "{.arg from} ({.val {as.character(from)}}) is after {.arg to} \
       ({.val {as.character(to)}}).",
      "i" = "{.arg to} defaults to {.fn get_now}, so this means every event \
             date in {.arg x} sits after its own {.field now}."
    ))
  }

  seq(from, to, by = by)
}

#' Coerce one end of a surveillance date grid, insisting on a real date
#'
#' @param value The value given for that end of the grid.
#' @param argument Its argument name, for the error message.
#'
#' @return A length-1 `Date`.
#'
#' @keywords internal
#' @noRd
.surveillance_grid_date <- function(value, argument) {
  if (length(value) != 1L || is.na(value)) {
    cli::cli_abort(
      "{.arg {argument}} must be a single non-missing date, not \
       {.val {value}}."
    )
  }
  # A numeric grid is caught upstream by `.surveillance_aggregate_by()`, so
  # anything reaching here should already be a date.
  as.Date(value)
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
#' @examplesIf requireNamespace("baselinenowcast", quietly = TRUE)
#' data(denguedat)
#' dengue <- tbl_now(denguedat[1:3000, ],
#'   event_date = onset_week, report_date = report_week, verbose = FALSE
#' )
#'
#' # One reporting triangle per stratum, in the shape baselinenowcast wants.
#' triangles <- suppressWarnings(
#'   tbl_now_to_baselinenowcast(dengue, format = "triangle_list", verbose = FALSE)
#' )
#'
#' # Printing summarises the set rather than dumping every matrix.
#' triangles
#'
#' # It is a list underneath, so the usual accessors work.
#' length(triangles)
#' names(triangles)
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
  # Same trap as `as_tbl_now.tbl_now_epinow2_snapshots()`: `verbose` must be
  # defaulted INTO `dots`, never passed beside them.
  if (is.null(dots$verbose)) dots$verbose <- FALSE

  result <- do.call(
    tbl_now,
    c(
      list(
        long_data,
        event_date  = event_col,
        report_date = report_col,
        case_count  = "count",
        data_type   = "count-incidence",
        strata      = if (length(strata_cols) > 0) strata_cols else NULL
      ),
      dots
    )
  )

  .drop_zero_counts(result)
}


# tbl_now_surveillance_list ----------------------------------------------------

#' One \pkg{surveillance} line list per stratum
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' The object returned by
#' `tbl_now_to_surveillance(x, format = "linelist_list")`: one individual-level
#' line list per observed combination of the object's strata, together with the
#' metadata needed to rebuild a `tbl_now` from it.
#'
#' It is a **thin** class -- it is still a list of plain data frames, so
#' `lapply()`, `[[` and friends work as usual:
#'
#' ```r
#' pieces <- tbl_now_to_surveillance(x, format = "linelist_list")
#' lapply(pieces, function(piece) {
#'   surveillance::nowcast(
#'     now = get_now(x), when = get_surveillance_when(x),
#'     data = piece, dEventCol = "dHospital", dReportCol = "dReport",
#'     control = list(dRange = get_surveillance_range(x))
#'   )
#' })
#' ```
#'
#' The class exists for the same reason [tbl_now_triangle_list] does: printing
#' says plainly that these are strata rather than something else shaped like a
#' list of line lists, and it carries the `now`, the units and the original
#' date-column names, none of which survive in a bare `split()`.
#'
#' `now` and the time grid are deliberately **not** baked into each piece. The
#' grid must come from the whole object ([get_surveillance_range()]), not from
#' the piece: every stratum has to be laid on the same axis, or a stratum whose
#' first case arrived late starts its own time on a different day.
#'
#' `as_tbl_now()` binds the pieces back together and restores the original
#' date-column names, the strata and the covariates. Two things do **not**
#' survive, because they are not in the line list to survive: count input comes
#' back as a `"linelist"` (one row per case, so the totals are unchanged but the
#' `case_count` column is gone), and materialised temporal-effect columns come
#' back as ordinary columns rather than as a spec.
#'
#' @param x A `tbl_now_surveillance_list`.
#' @param ... Ignored.
#'
#' @return `print()` returns `x` invisibly.
#'
#' @seealso [tbl_now_to_surveillance()], [as_tbl_now()], [tbl_now_triangle_list]
#'
#' @examplesIf requireNamespace("surveillance", quietly = TRUE)
#' data(denguedat)
#' dengue <- tbl_now(denguedat[1:3000, ],
#'   event_date = onset_week, report_date = report_week, verbose = FALSE
#' )
#'
#' ## One line list per stratum, in the shape surveillance::nowcast() wants.
#' linelists <- tbl_now_to_surveillance(dengue,
#'   format = "linelist_list", verbose = FALSE
#' )
#'
#' # Printing summarises the set rather than dumping every data frame.
#' linelists
#'
#' length(linelists)
#' head(linelists[[1]])
#'
#' @name tbl_now_surveillance_list
NULL

#' @rdname tbl_now_surveillance_list
#' @exportS3Method base::print
print.tbl_now_surveillance_list <- function(x, ...) {
  # NOTE: a print method must write to STDOUT. The `cli_*()` family emits
  # *messages*, so its output disappears under `message = FALSE`, `sink()` or
  # `capture.output()`. The `cat_*()` family is the stdout counterpart.
  strata_cols <- attr(x, "strata_cols")

  cli::cat_rule(
    left = cli::format_inline(
      "{length(x)} {.pkg surveillance} line list{?s} from a {.cls tbl_now}"
    )
  )
  cli::cat_bullet(c(
    if (length(strata_cols) > 0) {
      cli::format_inline("One per stratum ({.val {strata_cols}}): {.val {names(x)}}")
    } else {
      cli::format_inline("No strata; a single line list named {.val all}")
    },
    # NB: paste0(), not cli's `\\` line continuation -- `format_inline()` does
    # not strip those, so the backslash would be printed literally.
    cli::format_inline(paste0(
      "Date columns: {.val {attr(x, 'event_col')}} (event), ",
      "{.val {attr(x, 'report_col')}} (report)"
    )),
    cli::format_inline("Rows each: {.val {unname(vapply(x, nrow, integer(1)))}}"),
    cli::format_inline("Now: {.val {as.character(attr(x, 'now'))}}")
  ))
  cli::cat_line(cli::format_inline(paste0(
    "{cli::symbol$info} {.fn lapply} over this, passing ",
    "{.code control$dRange = get_surveillance_range(x)} from the WHOLE object ",
    "so every stratum shares one time axis."
  )))
  invisible(x)
}

#' @rdname as_tbl_now
#' @export
as_tbl_now.tbl_now_surveillance_list <- function(object, ...) {
  strata_cols    <- attr(object, "strata_cols")
  covariate_cols <- attr(object, "covariate_cols")
  event_col      <- attr(object, "event_col")
  report_col     <- attr(object, "report_col")

  linelist <- do.call(rbind, unname(lapply(object, as.data.frame)))
  rownames(linelist) <- NULL

  # Back to the caller's own column names. The pasted `strata` label is dropped:
  # the strata columns it was built from are still there, and keeping both would
  # declare a column that duplicates the others.
  names(linelist)[match(event_col, names(linelist))]  <-
    attr(object, "source_event_col")
  names(linelist)[match(report_col, names(linelist))] <-
    attr(object, "source_report_col")
  strata_col <- attr(object, "strata_col")
  if (!is.null(strata_col) && !strata_col %in% strata_cols) {
    linelist <- linelist[, setdiff(names(linelist), strata_col), drop = FALSE]
  }

  dots <- list(...)
  # Same trap as `as_tbl_now.tbl_now_triangle_list()`: `verbose` and the rest
  # must be defaulted INTO `dots`, never passed beside them, or a caller who
  # supplies one gets a duplicated formal.
  if (is.null(dots$now)) dots$now <- attr(object, "now")
  if (is.null(dots$verbose)) dots$verbose <- FALSE
  if (is.null(dots$event_units)) dots$event_units <- attr(object, "event_units")
  if (is.null(dots$report_units)) dots$report_units <- attr(object, "report_units")

  do.call(
    tbl_now,
    c(
      list(
        linelist,
        event_date  = attr(object, "source_event_col"),
        report_date = attr(object, "source_report_col"),
        data_type   = "linelist",
        strata      = if (length(strata_cols) > 0) strata_cols else NULL,
        covariates  = if (length(covariate_cols) > 0) covariate_cols else NULL
      ),
      dots
    )
  )
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
#' @seealso
#' The `tbl_now_to_*()` functions these delegate to, which take the arguments:
#' [tbl_now_to_epinowcast()], [tbl_now_to_baselinenowcast()],
#' [tbl_now_to_epidist()], [tbl_now_to_tsibble()], [tbl_now_to_data_table()];
#' [as_tbl_now()] to come back the other way;
#' [as_tibble()][as_tibble.tbl_now] to drop to a plain tibble.
#'
#' @examples
#' data(denguedat)
#' dengue <- tbl_now(denguedat[1:3000, ],
#'   event_date = onset_week, report_date = report_week, verbose = FALSE
#' )
#'
#' # These are S3 methods, so the other package's own verb works directly on a
#' # `tbl_now` -- no explicit converter call needed.
#' if (requireNamespace("tsibble", quietly = TRUE)) {
#'   suppressWarnings(tsibble::as_tsibble(dengue))
#' }
#'
#' if (requireNamespace("data.table", quietly = TRUE)) {
#'   head(data.table::as.data.table(dengue))
#' }
#'
#' ## Use the `tbl_now_to_*()` function itself when you need its arguments; these
#' # methods take none beyond `verbose`.
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

#' Resolve one tidy-select column name inside a converter
#'
#' The `tbl_now_from_*()` converters historically took character strings while
#' `tbl_now()` took tidy-select. These two helpers close that gap: a bare column
#' name, a string, or a tidy-select helper all resolve to a single column name.
#'
#' @param quo A quosure captured with [rlang::enquo()].
#' @param data The data frame to resolve against.
#' @param arg Name of the argument, for the error message.
#'
#' @return A single column name.
#'
#' @keywords internal
#' @noRd
.converter_select_one <- function(quo, data, arg) {
  selected <- .tbl_now_eval_select(quo, data)
  if (length(selected) != 1) {
    cli::cli_abort(
      "{.arg {arg}} must select exactly one column; it selected {length(selected)}."
    )
  }
  colnames(data)[selected]
}

#' Resolve several tidy-select column names inside a converter
#'
#' @param quo A quosure captured with [rlang::enquo()].
#' @param data The data frame to resolve against.
#'
#' @return A character vector of column names, possibly empty.
#'
#' @keywords internal
#' @noRd
.converter_select_many <- function(quo, data) {
  selected <- .tbl_now_eval_select(quo, data)
  if (length(selected) == 0) {
    return(NULL)
  }
  colnames(data)[selected]
}
