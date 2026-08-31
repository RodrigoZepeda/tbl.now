#' @title Summarise a `tbl_now`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' `summary()` describes a `tbl_now` the way a nowcaster needs it described:
#' how many cases arrive on each of the object's time axes, how long they take
#' to get there, how sparse the series is, what fraction of the data is
#' censored or still pending, and how far the object reaches.
#'
#' Every block of the summary is also available on its own -- see
#' [nowcast_summary_components] -- and `summary()` is exactly the
#' [dplyr::bind_rows()] of those pieces.
#'
#' @param object A `tbl_now` object.
#' @param ... Unused, for compatibility with the [summary()] generic.
#' @param by_strata Logical. Add one set of rows per stratum on top of the
#'   pooled (`"all"`) rows. Defaults to `TRUE` when the object has strata.
#' @param strata Character vector of columns to stratify by. Defaults to
#'   `get_strata(object)`.
#' @param lags Integer vector of lags for the autocorrelation rows.
#' @param completeness_delays Integer vector of delays for the reporting
#'   completeness rows. Defaults to `0:7`, trimmed to the observed delays.
#' @param growth_k Number of delays for the cumulative growth rows.
#' @param mature_only Logical. Restrict the completeness rows to event dates
#'   old enough to have been fully reported (see [reporting_completeness()]).
#'
#' @section The columns:
#'
#' Every function in this family returns the same schema, so results can be
#' stacked with [dplyr::bind_rows()] and filtered with [dplyr::filter()].
#'
#' \describe{
#'   \item{`component`}{Which block the row belongs to: `"cases"`, `"delay"`,
#'     `"zero_run"`, `"composition"`, `"autocorrelation"`, `"completeness"`,
#'     `"growth"` or `"coverage"`.}
#'   \item{`quantity`}{What the row describes, including the category for the
#'     compositional rows (`"confirmation_type = confirmed"`).}
#'   \item{`stratum`}{Which subset of the data the row describes: `"all"` for
#'     the pooled rows, or the stratum label otherwise.}
#'   \item{`n`}{Number of observations behind the row -- dates for `"cases"`,
#'     runs for `"zero_run"`, data rows for `"delay"` and `"composition"`.}
#'   \item{`total`}{Number of **cases** behind the row.}
#'   \item{`mean`, `sd`}{Mean and standard deviation. For the case-weighted
#'     rows these are the weighted versions, equal to what you would get by
#'     expanding the counts to one row per case.}
#'   \item{`min`, `q25`, `q50`, `q75`, `q90`, `max`}{Quantiles. See the note
#'     below on which estimator is used.}
#'   \item{`prop_zero`}{Proportion of dates on the grid that are exactly zero.}
#'   \item{`prop`}{Proportion of cases in this category (compositional rows).}
#'   \item{`value`}{A single scalar that is not a distribution: an
#'     autocorrelation, a gap, an occupancy.}
#'   \item{`date_min`, `date_max`}{Date range. Present only when the result
#'     contains `"coverage"` rows.}
#'   \item{`unobserved_cells`}{A `"coverage"` row counting the `NA`-count rows
#'     excluded as not yet observed.}
#' }
#'
#' @note
#' **Quantiles are inverse-ECDF (type 1), not [stats::quantile()]'s default.**
#' `q50` is the smallest value whose cumulative weight reaches `0.5`, which for
#' an even number of observations is the upper of the two middle values rather
#' than their average. This is deliberate: it is the same estimator
#' [autoplot.tbl_now()] and [diagnose_drift()] use for the delay quantiles
#' they draw, so the numbers in this table match the numbers in the plots. It
#' also always returns a value that was actually observed, which a half-case
#' delay is not.
#'
#' @details
#' **The date grids.** "Cases per event date" is a statement about a *calendar*,
#' not about the rows present in the data, so each axis is completed to a full
#' grid running from the earliest observed date on that axis to [get_now()],
#' stepping by that axis's units. Dates with no rows count as zeros. This is
#' what makes `prop_zero` and the zero-run lengths meaningful, and it is why a
#' line list -- which cannot represent a zero -- is summarised correctly here.
#'
#' **Not-yet-observed cells are dropped.** An `NA` count means the cell has not
#' been observed yet, unlike a `0`, which was observed and was zero. Such rows
#' carry no cases, so they are excluded rather than allowed to turn every total
#' they touch into `NA`. How many were dropped is reported as the
#' `"unobserved_cells"` coverage row.
#'
#' The grid is **global**: when `by_strata = TRUE` every stratum is summarised
#' on the same grid, so a stratum whose cases start late genuinely shows the
#' leading zeros. Otherwise the strata would not be comparable.
#'
#' **Count-cumulative data gets no delay rows.** A cumulative total is not
#' additive across delays, so a case-weighted delay distribution would be
#' meaningless. The `"growth"` rows take their place, describing how each event
#' date's total grows from one delay to the next. Call
#' `to_count(x, to = "count-incidence")` first if you want the delay
#' distribution -- and note that de-accumulating can produce negative
#' increments.
#'
#' @return A tibble with the columns described above.
#'
#' @seealso [nowcast_summary_components] for the individual blocks.
#'
#' @examples
#' data(denguedat)
#' ndata <- tbl_now(denguedat,
#'   event_date = "onset_week",
#'   report_date = "report_week",
#'   strata = "gender",
#'   verbose = FALSE
#' )
#'
#' # The whole summary: one row per quantity, per stratum.
#' overview <- summary(ndata)
#' overview
#'
#' # It is an ordinary tibble, so pick out the block you want.
#' overview |> dplyr::filter(component == "delay")
#'
#' # How much of each week's eventual total had arrived by delay d? This is the
#' # reporting-delay problem, in one table.
#' overview |>
#'   dplyr::filter(component == "completeness", stratum == "all") |>
#'   dplyr::select(quantity, value)
#'
#' # Pooled rows only, ignoring the strata.
#' summary(ndata, by_strata = FALSE)
#'
#' @name tbl_now_summary
#' @md
#' @exportS3Method base::summary
summary.tbl_now <- function(object, ..., by_strata = NULL, strata = NULL,
                            lags = 1, completeness_delays = NULL,
                            growth_k = 7, mature_only = TRUE) {
  context <- .summary_context(object, by_strata, strata, "summary")

  blocks <- list(
    .summary_cases(context, "event"),
    .summary_cases(context, "report"),
    .summary_cases(context, "confirmation"),
    .summary_zero_runs(context, "event"),
    .summary_zero_runs(context, "report"),
    .summary_zero_runs(context, "confirmation"),
    .summary_autocorrelation(context, "event", lags),
    .summary_autocorrelation(context, "report", lags),
    .summary_composition(context),
    .summary_coverage(context),
    .summary_occupancy(context),
    # The full curve is one row per observed delay per stratum, which on real
    # data is most of the table. `reporting_completeness()` still gives all of
    # it; the summary shows the head, where the information is.
    .summary_completeness(context, completeness_delays %||% 0:7, mature_only)
  )

  # A cumulative total cannot be summed across delays, so the delay
  # distribution is replaced by the maturation ratios rather than approximated.
  if (identical(get_data_type(object), "count-cumulative")) {
    blocks <- c(blocks, list(.summary_growth(context, growth_k)))
  } else {
    blocks <- c(blocks, list(
      .summary_delays(context, "event_to_report"),
      .summary_delays(context, "event_to_confirmation"),
      .summary_delays(context, "report_to_confirmation")
    ))
  }

  .summary_finalise(blocks)
}

# Components ------------------------------------------------------------------

#' @title Individual blocks of a `tbl_now` summary
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [summary()][tbl_now_summary] answers a dozen questions about a `tbl_now` at
#' once. When you only want one of them -- for a report, a dashboard, or a check
#' inside a script -- call that block directly instead of computing the rest and
#' filtering it away.
#'
#' Every one of these returns the same schema as `summary()` itself, so they can
#' be stacked with [dplyr::bind_rows()], compared across datasets, or used alone:
#'
#' * `cases_per_date()` -- case counts per date on one axis.
#' * `delay_summary()` -- the case-weighted delay distribution.
#' * `zero_run_summary()` -- lengths of the runs of consecutive zero dates.
#' * `prop_censored()` -- proportion of cases flagged censored.
#' * `prop_confirmation_type()` -- proportion of cases per confirmation outcome.
#' * `prop_strata()` -- proportion of cases per stratum.
#' * `prop_covariate_levels()` -- proportion of cases per level of each
#'   categorical covariate.
#' * `case_autocorrelation()` -- lagged autocorrelation of the case series.
#' * `date_ranges()` -- totals, date ranges and `now`.
#' * `triangle_occupancy()` -- how full the reporting triangle is, and how
#'   stale the object is.
#' * `reporting_completeness()` -- share of each event date's eventual total
#'   that had arrived by delay `d`.
#' * `cumulative_growth()` -- ratio of one delay's running total to the
#'   previous one's.
#'
#' @param x A `tbl_now` object.
#' @param axis Which time axis to describe: `"event"`, `"report"` or
#'   `"confirmation"`.
#' @param delay Which delay to describe: `"event_to_report"` (the reporting
#'   delay), `"event_to_confirmation"` (the same span measured to the
#'   confirmation, so the two are comparable) or `"report_to_confirmation"`
#'   (the laboratory's turnaround, the `.confirmation_delay` column).
#' @param lags Integer vector of lags.
#' @param delays Integer vector of delays to report completeness at. Defaults
#'   to every observed delay.
#' @param k Number of delays for the growth ratios.
#' @param mature_only Logical. Drop event dates too recent to have been fully
#'   reported. The cutoff is `now` minus the 95th percentile of the delay
#'   distribution -- the same rule [autoplot.tbl_now()] uses.
#' @param by_strata Logical. Add one set of rows per stratum on top of the
#'   pooled (`"all"`) rows. Defaults to `TRUE` when the object has strata.
#' @param strata Character vector of columns to stratify by. Defaults to
#'   `get_strata(x)`.
#'
#' @return A tibble in the schema documented in [tbl_now_summary]: one row per
#' quantity and stratum, with `component`, `quantity` and `stratum` identifying
#' the row and the remaining columns holding whichever statistics apply.
#'
#' @seealso
#' [summary()][tbl_now_summary], which stacks all of these into one table and
#' documents the schema; [diagnose()] for what is *wrong* with the data rather
#' than what is in it; [autoplot()][autoplot.tbl_now] for the same information as
#' pictures. The
#' [*Describing and diagnosing a tbl_now* article](https://rodrigozepeda.github.io/tbl.now/articles/describing-and-diagnosing.html)
#' walks through them in order.
#'
#' @examples
#' data(denguedat)
#' ndata <- tbl_now(denguedat,
#'   event_date = "onset_week",
#'   report_date = "report_week",
#'   strata = "gender",
#'   verbose = FALSE
#' )
#'
#' # How many cases per week of onset, and how long they took to be reported.
#' cases_per_date(ndata, axis = "event")
#' delay_summary(ndata)
#'
#' # How sparse the series is, and how strongly one week predicts the next.
#' zero_run_summary(ndata, axis = "event")
#' case_autocorrelation(ndata, lags = 1)
#'
#' # What the data is made of, and how far it reaches.
#' prop_strata(ndata)
#' prop_censored(ndata)
#' date_ranges(ndata)
#' triangle_occupancy(ndata)
#'
#' # The two that matter most for nowcasting: what share of a week's eventual
#' # total had arrived by delay d, and how fast the total is still growing.
#' reporting_completeness(ndata, delays = 0:3)
#' cumulative_growth(ndata, k = 3)
#'
#' # Every block shares one schema, so they stack.
#' dplyr::bind_rows(
#'   date_ranges(ndata),
#'   delay_summary(ndata)
#' )
#'
#' @name nowcast_summary_components
#' @md
NULL

#' @rdname nowcast_summary_components
#' @export
cases_per_date <- function(x, axis = c("event", "report", "confirmation"),
                           by_strata = NULL, strata = NULL) {
  axis <- match.arg(axis)
  context <- .summary_context(x, by_strata, strata, "cases_per_date")
  .summary_finalise(list(.summary_cases(context, axis)))
}

#' @rdname nowcast_summary_components
#' @export
delay_summary <- function(x,
                          delay = c("event_to_report", "event_to_confirmation",
                                    "report_to_confirmation"),
                          by_strata = NULL, strata = NULL) {
  delay <- match.arg(delay)
  if (identical(get_data_type(x), "count-cumulative")) {
    cli::cli_abort(c(
      "{.fn delay_summary} cannot weight delays by a {.val count-cumulative}
       count: a cumulative total is not additive across delays.",
      "i" = "De-accumulate first with
             {.code to_count(x, to = \"count-incidence\")}, remembering that a
             downward revision becomes a negative increment.",
      "i" = "Or describe the maturation directly with {.fn cumulative_growth}."
    ))
  }
  context <- .summary_context(x, by_strata, strata, "delay_summary")
  .summary_finalise(list(.summary_delays(context, delay)))
}

#' @rdname nowcast_summary_components
#' @export
zero_run_summary <- function(x, axis = c("event", "report", "confirmation"),
                             by_strata = NULL, strata = NULL) {
  axis <- match.arg(axis)
  context <- .summary_context(x, by_strata, strata, "zero_run_summary")
  .summary_finalise(list(.summary_zero_runs(context, axis)))
}

#' @rdname nowcast_summary_components
#' @export
prop_censored <- function(x, by_strata = NULL, strata = NULL) {
  context <- .summary_context(x, by_strata, strata, "prop_censored")
  .summary_finalise(list(.summary_censoring(context)))
}

#' @rdname nowcast_summary_components
#' @export
prop_confirmation_type <- function(x, by_strata = NULL, strata = NULL) {
  context <- .summary_context(x, by_strata, strata, "prop_confirmation_type")
  .summary_finalise(list(.summary_confirmation_types(context)))
}

#' @rdname nowcast_summary_components
#' @export
prop_strata <- function(x, strata = NULL) {
  context <- .summary_context(x, TRUE, strata, "prop_strata")
  .summary_finalise(list(.summary_strata_shares(context)))
}

#' @rdname nowcast_summary_components
#' @export
prop_covariate_levels <- function(x, by_strata = NULL, strata = NULL) {
  context <- .summary_context(x, by_strata, strata, "prop_covariate_levels")
  .summary_finalise(list(.summary_covariate_shares(context)))
}

#' @rdname nowcast_summary_components
#' @export
case_autocorrelation <- function(x, lags = 1,
                                 axis = c("event", "report", "confirmation"),
                                 by_strata = NULL, strata = NULL) {
  axis <- match.arg(axis)
  context <- .summary_context(x, by_strata, strata, "case_autocorrelation")
  .summary_finalise(list(.summary_autocorrelation(context, axis, lags)))
}

#' @rdname nowcast_summary_components
#' @export
date_ranges <- function(x, by_strata = NULL, strata = NULL) {
  context <- .summary_context(x, by_strata, strata, "date_ranges")
  .summary_finalise(list(.summary_coverage(context)))
}

#' @rdname nowcast_summary_components
#' @export
triangle_occupancy <- function(x, by_strata = NULL, strata = NULL) {
  context <- .summary_context(x, by_strata, strata, "triangle_occupancy")
  .summary_finalise(list(.summary_occupancy(context)))
}

#' @rdname nowcast_summary_components
#' @export
reporting_completeness <- function(x, delays = NULL, mature_only = TRUE,
                                   by_strata = NULL, strata = NULL) {
  context <- .summary_context(x, by_strata, strata, "reporting_completeness")
  .summary_finalise(list(.summary_completeness(context, delays, mature_only)))
}

#' @rdname nowcast_summary_components
#' @export
cumulative_growth <- function(x, k = 7, by_strata = NULL, strata = NULL) {
  context <- .summary_context(x, by_strata, strata, "cumulative_growth")
  .summary_finalise(list(.summary_growth(context, k)))
}

# Shared context --------------------------------------------------------------

#' The canonical column order of a summary tibble
#'
#' @return A character vector of column names.
#'
#' @keywords internal
#' @noRd
.summary_schema <- function() {
  c(
    "component", "quantity", "stratum", "n", "total",
    "mean", "sd", "min", "q25", "q50", "q75", "q90", "max",
    "prop_zero", "prop", "value", "date_min", "date_max"
  )
}

#' Everything the summary blocks need, computed once
#'
#' The blocks all work off one case table: a `count-incidence` view of the
#' object with the counts, the three delays and a stratum label attached. It is
#' built once because `to_count()` is the expensive part and every block needs
#' the same answer from it.
#'
#' @param x A `tbl_now` object.
#' @param by_strata Logical or `NULL` (`TRUE` when the object has strata).
#' @param strata Character vector of columns, or `NULL` for `get_strata(x)`.
#' @param fn Calling function, for messages.
#'
#' @return A list with the object, the case table, the covariate columns, the
#'   strata columns and the stratum labels to iterate over.
#'
#' @keywords internal
#' @noRd
.summary_context <- function(x, by_strata, strata, fn) {
  .assert_tbl_now(x, fn)
  x <- ungroup(x)

  if (is.null(by_strata)) {
    by_strata <- get_num_strata(x) > 0 || !is.null(strata)
  }
  check_bool(by_strata, "by_strata")

  strata_cols <- if (isTRUE(by_strata)) {
    .tbl_now_resolve_strata_cols(x, strata)
  } else {
    NULL
  }

  incidence <- suppressMessages(suppressWarnings(
    to_count(x, to = "count-incidence")
  ))
  observations <- .strip_tbl_now(incidence)

  cases <- dplyr::tibble(
    event_date  = observations[[get_event_date(x)]],
    report_date = observations[[get_report_date(x)]],
    count       = as.numeric(observations[[get_case_count(incidence)]]),
    event_to_report = as.numeric(observations[[".delay"]])
  )

  censored_col <- get_is_censored(x)
  if (!is.null(censored_col)) {
    # The checker allows 0/1 as well as TRUE/FALSE, so normalise once here
    # rather than in each block.
    cases$censored <- as.logical(observations[[censored_col]])
  }

  if (has_confirmation(x)) {
    cases$confirmation_date <- observations[[get_confirmation_date(x)]]
    type_col <- get_confirmation_type(x)
    cases$confirmation_type <- if (is.null(type_col)) {
      NA_character_
    } else {
      as.character(observations[[type_col]])
    }
    # Measured from the EVENT, so it is directly comparable with
    # `event_to_report`; `.confirmation_delay` is the laboratory's own
    # turnaround, measured from the report. They are different quantities.
    cases$event_to_confirmation <-
      as.numeric(observations[[".confirmation_num"]] -
                   observations[[".event_num"]])
    cases$report_to_confirmation <- as.numeric(observations[[".confirmation_delay"]])
  }

  cases$stratum <- if (length(strata_cols) > 0) {
    .tbl_now_strata_label(observations, strata_cols)
  } else {
    "all"
  }

  covariate_cols <- get_covariates(x)
  covariates <- if (length(covariate_cols) > 0) {
    observations[, covariate_cols, drop = FALSE]
  } else {
    NULL
  }

  # An `NA` count is a cell that has NOT YET BEEN OBSERVED -- unlike a `0`,
  # which was observed and was zero. Such a cell carries no cases, so it is
  # dropped here rather than allowed to turn every total it touches into `NA`.
  # The count of what was dropped is reported back as a coverage row, so the
  # loss is visible instead of silent.
  observed <- !is.na(cases$count)
  unobserved_cells <- sum(!observed)
  cases <- cases[observed, , drop = FALSE]
  if (!is.null(covariates)) {
    covariates <- covariates[observed, , drop = FALSE]
  }

  labels <- "all"
  if (length(strata_cols) > 0) {
    labels <- c("all", sort(unique(cases$stratum)))
  }

  list(
    x = x, cases = cases, covariates = covariates,
    strata_cols = strata_cols, labels = labels,
    unobserved_cells = unobserved_cells
  )
}

#' A case-table column, or `NULL` when the object does not carry it
#'
#' `$` on a tibble warns for an unknown column, and half the blocks here ask
#' about columns that only exist when the object has a censoring flag or a
#' confirmation process.
#'
#' @param data The case table.
#' @param name Column name.
#'
#' @return The column, or `NULL`.
#'
#' @keywords internal
#' @noRd
.summary_column <- function(data, name) {
  if (name %in% names(data)) data[[name]] else NULL
}

#' Row indices of one stratum of the case table
#'
#' @param context A summary context.
#' @param label A stratum label, or `"all"`.
#'
#' @return A logical vector over the rows of `context$cases`.
#'
#' @keywords internal
#' @noRd
.summary_rows <- function(context, label) {
  if (identical(label, "all")) {
    rep(TRUE, nrow(context$cases))
  } else {
    context$cases$stratum == label
  }
}

#' Bind blocks into the canonical schema
#'
#' Blocks omit the columns they do not populate, so [dplyr::bind_rows()] can
#' infer each column's type instead of being handed a logical `NA` where a
#' `Date` belongs. The numeric columns are then filled in, and the date columns
#' only when some block actually produced dates.
#'
#' @param blocks A list of tibbles.
#'
#' @return A tibble with the columns of `.summary_schema()`, in that order.
#'
#' @keywords internal
#' @noRd
.summary_finalise <- function(blocks) {
  out <- dplyr::bind_rows(blocks)

  if (nrow(out) == 0) {
    out <- dplyr::tibble(
      component = character(0), quantity = character(0), stratum = character(0)
    )
  }

  numeric_columns <- c(
    "total", "mean", "sd", "min", "q25", "q50", "q75", "q90", "max",
    "prop_zero", "prop", "value"
  )
  for (column in numeric_columns) {
    if (!column %in% names(out)) out[[column]] <- NA_real_
  }
  if (!"n" %in% names(out)) out[["n"]] <- NA_integer_

  dplyr::relocate(out, dplyr::any_of(.summary_schema()))
}

# Statistics ------------------------------------------------------------------

#' The probabilities the summary reports, and the columns they land in
#'
#' @return A named numeric vector.
#'
#' @keywords internal
#' @noRd
.summary_probs <- function() {
  c(min = 0, q25 = 0.25, q50 = 0.5, q75 = 0.75, q90 = 0.9, max = 1)
}

#' One distributional row of a summary
#'
#' `weights` are case counts. The mean and standard deviation are the weighted
#' versions, defined so that they equal what expanding the counts to one row
#' per case and calling [mean()] / [stats::sd()] would give. The quantiles come
#' from `.tbl_now_weighted_quantile()`, the estimator the package's plots
#' already use, so the table and the figures agree.
#'
#' @param values Numeric vector.
#' @param weights Numeric weights, or `NULL` for unweighted.
#' @param component,quantity,stratum Row identifiers.
#' @param n,total Overridden counts (see the schema documentation).
#' @param prop_zero Proportion of zeros, or `NULL` to leave it out.
#'
#' @return A one-row tibble.
#'
#' @keywords internal
#' @noRd
.summary_stat_row <- function(values, weights = NULL, component, quantity,
                              stratum, n = NULL, total = NULL,
                              prop_zero = NULL) {
  if (is.null(weights)) weights <- rep(1, length(values))
  keep <- !is.na(values) & !is.na(weights) & weights > 0
  values <- values[keep]
  weights <- weights[keep]

  row <- dplyr::tibble(
    component = component, quantity = quantity, stratum = stratum,
    n = as.integer(n %||% length(values)),
    total = as.numeric(total %||% sum(weights))
  )

  if (length(values) == 0) {
    row$mean <- NA_real_
    row$sd <- NA_real_
    for (name in names(.summary_probs())) row[[name]] <- NA_real_
  } else {
    weight_total <- sum(weights)
    weighted_mean <- sum(weights * values) / weight_total
    row$mean <- weighted_mean
    row$sd <- if (weight_total > 1) {
      sqrt(sum(weights * (values - weighted_mean)^2) / (weight_total - 1))
    } else {
      NA_real_
    }
    for (name in names(.summary_probs())) {
      row[[name]] <- .tbl_now_weighted_quantile(
        values, weights, .summary_probs()[[name]]
      )
    }
  }

  if (!is.null(prop_zero)) row$prop_zero <- as.numeric(prop_zero)
  row
}

# Date grids ------------------------------------------------------------------

#' The column, units and grid end of one time axis
#'
#' @param context A summary context.
#' @param axis `"event"`, `"report"` or `"confirmation"`.
#'
#' @return A list with the case-table column name and the axis units, or `NULL`
#'   when the object does not have that axis.
#'
#' @keywords internal
#' @noRd
.summary_axis <- function(context, axis) {
  x <- context$x
  switch(axis,
    event = list(column = "event_date", units = get_event_units(x)),
    report = list(column = "report_date", units = get_report_units(x)),
    confirmation = if (has_confirmation(x)) {
      list(
        column = "confirmation_date",
        units = get_confirmation_units(x) %||% get_report_units(x)
      )
    } else {
      NULL
    }
  )
}

#' Case counts per date on one axis, on the complete grid
#'
#' Pending confirmations are dropped from the confirmation axis: a pending case
#' has no confirmation date, so counting it would invent an arrival on a date it
#' does not have.
#'
#' @param context A summary context.
#' @param axis `"event"`, `"report"` or `"confirmation"`.
#' @param rows Logical vector selecting the rows of the case table.
#' @param subset Optional further filter, a logical vector over the same rows.
#'
#' @return A numeric vector of totals, one per date of the global grid.
#'
#' @keywords internal
#' @noRd
.summary_series <- function(context, axis, rows, subset = NULL) {
  spec <- .summary_axis(context, axis)
  if (is.null(spec)) return(NULL)

  cases <- context$cases
  dates <- cases[[spec$column]]
  grid <- .summary_grid(context, axis)
  if (length(grid) == 0) return(NULL)

  keep <- rows & !is.na(dates)
  if (!is.null(subset)) keep <- keep & subset

  totals <- rep(0, length(grid))
  if (any(keep)) {
    matched <- match(dates[keep], grid)
    counts <- cases$count[keep]
    valid <- !is.na(matched)
    if (any(valid)) {
      totals <- as.numeric(
        tapply(counts[valid], factor(matched[valid], levels = seq_along(grid)),
               sum, default = 0)
      )
      totals[is.na(totals)] <- 0
    }
  }
  stats::setNames(totals, NULL)
}

#' The complete date grid of one axis
#'
#' Runs from the earliest date observed anywhere in the object on that axis to
#' `now`. It is deliberately global rather than per-stratum, so that strata stay
#' comparable and a stratum that starts late shows its leading zeros.
#'
#' @param context A summary context.
#' @param axis `"event"`, `"report"` or `"confirmation"`.
#'
#' @return A vector of dates (or numbers), possibly empty.
#'
#' @keywords internal
#' @noRd
.summary_grid <- function(context, axis) {
  spec <- .summary_axis(context, axis)
  if (is.null(spec)) return(NULL)

  dates <- context$cases[[spec$column]]
  dates <- dates[!is.na(dates)]
  if (length(dates) == 0) return(dates)

  .tbl_now_date_seq(min(dates), get_now(context$x), spec$units)
}

# Blocks ----------------------------------------------------------------------

#' Case counts per date, for every stratum
#'
#' @param context A summary context.
#' @param axis `"event"`, `"report"` or `"confirmation"`.
#'
#' @return A tibble of `"cases"` rows.
#'
#' @keywords internal
#' @noRd
.summary_cases <- function(context, axis) {
  if (is.null(.summary_axis(context, axis))) return(NULL)

  quantity <- paste0("per_", axis, "_date")
  cases <- context$cases
  types <- .summary_confirmation_levels(context)

  rows <- lapply(context$labels, function(label) {
    selected <- .summary_rows(context, label)
    block <- list(.summary_series_row(
      context, axis, selected, NULL, "cases", quantity, label
    ))

    censored <- .summary_column(cases, "censored")
    if (!is.null(censored)) {
      block <- c(block, list(.summary_series_row(
        context, axis, selected, censored %in% TRUE,
        "cases", paste0("censored_", quantity), label
      )))
    }

    # "Cases per type of validation", but only when there is more than one type
    # to tell apart -- a single-outcome column repeats the row above it.
    if (identical(axis, "confirmation") && length(types) > 1) {
      block <- c(block, lapply(types, function(type) {
        .summary_series_row(
          context, axis, selected, cases[["confirmation_type"]] %in% type,
          "cases", paste0(quantity, " [", type, "]"), label
        )
      }))
    }
    dplyr::bind_rows(block)
  })

  dplyr::bind_rows(rows)
}

#' One `"cases"` row from a completed series
#'
#' @param context A summary context.
#' @param axis The axis.
#' @param rows,subset Row selections, as in `.summary_series()`.
#' @param component,quantity,stratum Row identifiers.
#'
#' @return A one-row tibble.
#'
#' @keywords internal
#' @noRd
.summary_series_row <- function(context, axis, rows, subset,
                                component, quantity, stratum) {
  series <- .summary_series(context, axis, rows, subset)
  if (is.null(series)) {
    return(.summary_stat_row(
      numeric(0), NULL, component, quantity, stratum, n = 0, total = 0
    ))
  }
  .summary_stat_row(
    series, NULL, component, quantity, stratum,
    n = length(series), total = sum(series),
    prop_zero = mean(series == 0)
  )
}

#' The delay distribution, for every stratum
#'
#' @param context A summary context.
#' @param delay Which of the three delays.
#'
#' @return A tibble of `"delay"` rows, or `NULL` when the object has no
#'   confirmation and a confirmation delay was asked for.
#'
#' @keywords internal
#' @noRd
.summary_delays <- function(context, delay) {
  cases <- context$cases
  if (!delay %in% names(cases)) return(NULL)
  types <- .summary_confirmation_levels(context)
  uses_confirmation <- delay %in%
    c("event_to_confirmation", "report_to_confirmation")

  rows <- lapply(context$labels, function(label) {
    selected <- .summary_rows(context, label)
    block <- list(.summary_delay_row(context, delay, selected, NULL, delay, label))

    if (uses_confirmation && length(types) > 1) {
      block <- c(block, lapply(types, function(type) {
        .summary_delay_row(
          context, delay, selected, cases[["confirmation_type"]] %in% type,
          paste0(delay, " [", type, "]"), label
        )
      }))
    }
    dplyr::bind_rows(block)
  })

  dplyr::bind_rows(rows)
}

#' One `"delay"` row
#'
#' @param context A summary context.
#' @param delay Column of the case table holding the delay.
#' @param rows,subset Row selections.
#' @param quantity,stratum Row identifiers.
#'
#' @return A one-row tibble.
#'
#' @keywords internal
#' @noRd
.summary_delay_row <- function(context, delay, rows, subset, quantity, stratum) {
  cases <- context$cases
  keep <- rows
  if (!is.null(subset)) keep <- keep & subset
  values <- cases[[delay]][keep]
  weights <- cases$count[keep]
  .summary_stat_row(values, weights, "delay", quantity, stratum)
}

#' Lengths of the runs of consecutive zero dates
#'
#' @param context A summary context.
#' @param axis The axis.
#'
#' @return A tibble of `"zero_run"` rows.
#'
#' @keywords internal
#' @noRd
.summary_zero_runs <- function(context, axis) {
  if (is.null(.summary_axis(context, axis))) return(NULL)

  rows <- lapply(context$labels, function(label) {
    series <- .summary_series(context, axis, .summary_rows(context, label))
    lengths <- if (is.null(series)) {
      numeric(0)
    } else {
      encoding <- rle(series == 0)
      encoding$lengths[encoding$values]
    }
    .summary_stat_row(
      as.numeric(lengths), NULL, "zero_run", paste0(axis, "_date"), label,
      n = length(lengths), total = sum(lengths)
    )
  })

  dplyr::bind_rows(rows)
}

#' Lagged autocorrelation of the case series
#'
#' @param context A summary context.
#' @param axis The axis.
#' @param lags Integer vector of lags.
#'
#' @return A tibble of `"autocorrelation"` rows.
#'
#' @keywords internal
#' @noRd
.summary_autocorrelation <- function(context, axis, lags) {
  if (is.null(.summary_axis(context, axis))) return(NULL)
  lags <- as.integer(lags)
  if (length(lags) == 0 || any(is.na(lags)) || any(lags < 1)) {
    cli::cli_abort("{.arg lags} must be positive whole numbers.")
  }

  rows <- lapply(context$labels, function(label) {
    series <- .summary_series(context, axis, .summary_rows(context, label))
    dplyr::bind_rows(lapply(lags, function(lag) {
      pairs <- .summary_lagged_correlation(series, lag)
      dplyr::tibble(
        component = "autocorrelation",
        quantity = paste0("per_", axis, "_date lag ", lag),
        stratum = label,
        n = as.integer(pairs$n),
        value = pairs$value
      )
    }))
  })

  dplyr::bind_rows(rows)
}

#' Pearson correlation between a series and its own lag
#'
#' Computed as `cor(y[1:(n - lag)], y[(1 + lag):n])`: the correlation of the
#' lagged pairs. This is **not** the same as [stats::acf()], whose estimator
#' divides by the full series length and centres both halves on the full-series
#' mean. The lagged-pair form is the one a reader can reproduce by hand, and
#' the difference is negligible except on very short series.
#'
#' @param series Numeric vector, or `NULL`.
#' @param lag Positive integer.
#'
#' @return A list with `n` (number of pairs) and `value`.
#'
#' @keywords internal
#' @noRd
.summary_lagged_correlation <- function(series, lag) {
  if (is.null(series) || length(series) <= lag + 1) {
    return(list(n = 0L, value = NA_real_))
  }
  head_values <- series[seq_len(length(series) - lag)]
  tail_values <- series[seq(lag + 1, length(series))]
  if (stats::sd(head_values) == 0 || stats::sd(tail_values) == 0) {
    return(list(n = length(head_values), value = NA_real_))
  }
  list(
    n = length(head_values),
    value = stats::cor(head_values, tail_values)
  )
}

#' Every compositional block
#'
#' @param context A summary context.
#'
#' @return A tibble of `"composition"` rows.
#'
#' @keywords internal
#' @noRd
.summary_composition <- function(context) {
  dplyr::bind_rows(
    .summary_censoring(context),
    .summary_confirmation_types(context),
    .summary_strata_shares(context),
    .summary_covariate_shares(context)
  )
}

#' Proportion of cases flagged as censored
#'
#' @param context A summary context.
#'
#' @return A tibble of `"composition"` rows, or `NULL` when there is no flag.
#'
#' @keywords internal
#' @noRd
.summary_censoring <- function(context) {
  cases <- context$cases
  flag <- .summary_column(cases, "censored")
  if (is.null(flag)) return(NULL)

  rows <- lapply(context$labels, function(label) {
    selected <- .summary_rows(context, label)
    censored <- selected & flag %in% TRUE
    .summary_share_row(
      "censored", label,
      n = sum(censored), total = sum(cases$count[censored]),
      denominator = sum(cases$count[selected])
    )
  })

  dplyr::bind_rows(rows)
}

#' Proportion of cases per confirmation outcome
#'
#' @param context A summary context.
#'
#' @return A tibble of `"composition"` rows, or `NULL` without a confirmation.
#'
#' @keywords internal
#' @noRd
.summary_confirmation_types <- function(context) {
  types <- .summary_confirmation_levels(context)
  if (length(types) == 0) return(NULL)
  cases <- context$cases

  rows <- lapply(context$labels, function(label) {
    selected <- .summary_rows(context, label)
    denominator <- sum(cases$count[selected])
    dplyr::bind_rows(lapply(types, function(type) {
      matched <- selected & cases[["confirmation_type"]] %in% type
      .summary_share_row(
        paste0("confirmation_type = ", type), label,
        n = sum(matched), total = sum(cases$count[matched]),
        denominator = denominator
      )
    }))
  })

  dplyr::bind_rows(rows)
}

#' Proportion of cases per stratum
#'
#' Only ever a pooled statement, so these rows are always `stratum = "all"`:
#' the share of stratum A *within* stratum B is not a quantity.
#'
#' @param context A summary context.
#'
#' @return A tibble of `"composition"` rows, or `NULL` when unstratified.
#'
#' @keywords internal
#' @noRd
.summary_strata_shares <- function(context) {
  if (length(context$strata_cols) == 0) return(NULL)
  cases <- context$cases
  denominator <- sum(cases$count)
  labels <- setdiff(context$labels, "all")

  dplyr::bind_rows(lapply(labels, function(label) {
    matched <- cases$stratum == label
    .summary_share_row(
      paste0("strata = ", label), "all",
      n = sum(matched), total = sum(cases$count[matched]),
      denominator = denominator
    )
  }))
}

#' Proportion of cases per level of each categorical covariate
#'
#' Numeric covariates are skipped: a share per distinct value of a continuous
#' covariate is one row per observation, which describes nothing.
#'
#' @param context A summary context.
#'
#' @return A tibble of `"composition"` rows, or `NULL` when there are none.
#'
#' @keywords internal
#' @noRd
.summary_covariate_shares <- function(context) {
  covariates <- context$covariates
  if (is.null(covariates)) return(NULL)
  cases <- context$cases

  categorical <- names(covariates)[vapply(
    covariates,
    function(column) is.factor(column) || is.character(column) ||
      is.logical(column),
    logical(1)
  )]
  if (length(categorical) == 0) return(NULL)

  rows <- lapply(context$labels, function(label) {
    selected <- .summary_rows(context, label)
    denominator <- sum(cases$count[selected])
    dplyr::bind_rows(lapply(categorical, function(column) {
      values <- as.character(covariates[[column]])
      levels <- sort(unique(values[selected & !is.na(values)]))
      dplyr::bind_rows(lapply(levels, function(level) {
        matched <- selected & values %in% level
        .summary_share_row(
          paste0("covariate: ", column, " = ", level), label,
          n = sum(matched), total = sum(cases$count[matched]),
          denominator = denominator
        )
      }))
    }))
  })

  dplyr::bind_rows(rows)
}

#' One compositional row
#'
#' @param quantity,stratum Row identifiers.
#' @param n Number of data rows in the category.
#' @param total Number of cases in the category.
#' @param denominator Number of cases the proportion is taken out of.
#'
#' @return A one-row tibble.
#'
#' @keywords internal
#' @noRd
.summary_share_row <- function(quantity, stratum, n, total, denominator) {
  dplyr::tibble(
    component = "composition", quantity = quantity, stratum = stratum,
    n = as.integer(n), total = as.numeric(total),
    prop = if (isTRUE(denominator > 0)) total / denominator else NA_real_
  )
}

#' The confirmation outcomes actually present in the data
#'
#' @param context A summary context.
#'
#' @return A character vector, empty when there is no confirmation.
#'
#' @keywords internal
#' @noRd
.summary_confirmation_levels <- function(context) {
  types <- .summary_column(context$cases, "confirmation_type")
  if (is.null(types)) return(character(0))
  present <- unique(types[!is.na(types)])
  # Report them in the package's own order rather than alphabetically, so
  # `confirmed` always comes before `retracted`.
  known <- intersect(.confirmation_levels(), present)
  c(known, sort(setdiff(present, known)))
}

#' Totals, date ranges and `now`
#'
#' @param context A summary context.
#'
#' @return A tibble of `"coverage"` rows.
#'
#' @keywords internal
#' @noRd
.summary_coverage <- function(context) {
  cases <- context$cases
  axes <- c("event", "report", "confirmation")

  rows <- lapply(context$labels, function(label) {
    selected <- .summary_rows(context, label)

    block <- list(dplyr::tibble(
      component = "coverage", quantity = "total_cases", stratum = label,
      n = as.integer(sum(selected)),
      total = sum(cases$count[selected])
    ))

    block <- c(block, lapply(axes, function(axis) {
      spec <- .summary_axis(context, axis)
      if (is.null(spec)) return(NULL)
      dates <- cases[[spec$column]][selected & !is.na(cases[[spec$column]])]
      if (length(dates) == 0) return(NULL)
      dplyr::tibble(
        component = "coverage", quantity = paste0(axis, "_date"),
        stratum = label,
        n = as.integer(length(unique(dates))),
        total = sum(cases$count[selected & !is.na(cases[[spec$column]])]),
        date_min = min(dates), date_max = max(dates)
      )
    }))

    dplyr::bind_rows(block)
  })

  now_value <- get_now(context$x)
  now_row <- dplyr::tibble(
    component = "coverage", quantity = "now", stratum = "all",
    date_min = now_value, date_max = now_value
  )
  unobserved_row <- dplyr::tibble(
    component = "coverage", quantity = "unobserved_cells", stratum = "all",
    n = as.integer(context$unobserved_cells)
  )

  dplyr::bind_rows(c(rows, list(now_row, unobserved_row)))
}

#' How full the reporting triangle is, and how stale the object is
#'
#' The triangle's *possible* cells are the `(event date, delay)` pairs that
#' could have been observed by `now`: every date on the event grid, paired with
#' every delay from `0` to the largest observed one, keeping only the pairs that
#' land on or before `now`. Occupancy is the share of those that carry a
#' non-zero count -- a different kind of sparsity from the zero runs, which look
#' along one axis only.
#'
#' @param context A summary context.
#'
#' @return A tibble of `"coverage"` rows.
#'
#' @keywords internal
#' @noRd
.summary_occupancy <- function(context) {
  cases <- context$cases
  grid <- .summary_grid(context, "event")
  now_value <- get_now(context$x)

  # The denominator is built from the GLOBAL widest delay, for the same reason
  # the date grid is global: a stratum whose cases all arrive same-day would
  # otherwise be measured against a one-column triangle and score a near-perfect
  # occupancy, which says nothing about how it compares with the others.
  all_usable <- !is.na(cases$event_to_report) & !is.na(cases$count) &
    cases$count != 0
  widest_delay <- if (any(all_usable)) {
    max(cases$event_to_report[all_usable])
  } else {
    NA_real_
  }
  room <- if (length(grid) == 0) {
    numeric(0)
  } else {
    .tbl_now_units_between(grid, now_value, get_event_units(context$x))
  }

  # Only the pairs that could already have arrived: an event date one step
  # before `now` cannot yet have a delay of ten. Stratum-independent, by the
  # same argument as `widest_delay`.
  possible <- if (is.na(widest_delay) || length(room) == 0) {
    NA_integer_
  } else {
    sum(pmin(floor(room), widest_delay) + 1)
  }

  rows <- lapply(context$labels, function(label) {
    selected <- .summary_rows(context, label)
    delays <- cases$event_to_report[selected]
    counts <- cases$count[selected]
    usable <- !is.na(delays) & !is.na(counts) & counts != 0

    max_delay <- if (any(usable)) max(delays[usable]) else NA_real_
    occupied <- if (any(usable)) {
      nrow(unique(data.frame(
        event = cases$event_date[selected][usable], delay = delays[usable]
      )))
    } else {
      0L
    }

    block <- list(
      dplyr::tibble(
        component = "coverage", quantity = "max_delay", stratum = label,
        value = as.numeric(max_delay)
      ),
      dplyr::tibble(
        component = "coverage", quantity = "triangle_cells_observed",
        stratum = label, n = as.integer(occupied)
      ),
      dplyr::tibble(
        component = "coverage", quantity = "triangle_cells_possible",
        stratum = label, n = as.integer(possible)
      ),
      dplyr::tibble(
        component = "coverage", quantity = "triangle_occupancy", stratum = label,
        value = if (is.na(possible) || possible == 0) {
          NA_real_
        } else {
          occupied / possible
        }
      )
    )

    for (axis in c("event", "report")) {
      spec <- .summary_axis(context, axis)
      dates <- cases[[spec$column]][selected]
      dates <- dates[!is.na(dates)]
      block <- c(block, list(dplyr::tibble(
        component = "coverage", quantity = paste0("now_gap_", axis),
        stratum = label,
        value = if (length(dates) == 0) {
          NA_real_
        } else {
          .tbl_now_units_between(max(dates), now_value, spec$units)
        }
      )))
    }

    dplyr::bind_rows(block)
  })

  dplyr::bind_rows(rows)
}

#' Share of each event date's eventual total that had arrived by delay `d`
#'
#' @param context A summary context.
#' @param delays Integer vector of delays, or `NULL` for all observed ones.
#' @param mature_only Logical.
#'
#' @return A tibble of `"completeness"` rows.
#'
#' @keywords internal
#' @noRd
.summary_completeness <- function(context, delays, mature_only) {
  cases <- context$cases
  observed <- cases$event_to_report[!is.na(cases$event_to_report) &
                                      cases$count > 0]
  if (length(observed) == 0) return(NULL)

  if (is.null(delays)) {
    delays <- seq(0, max(observed))
  }
  delays <- sort(unique(as.integer(delays)))
  delays <- delays[delays >= 0 & delays <= max(observed)]
  if (length(delays) == 0) return(NULL)

  check_bool(mature_only, "mature_only")
  cutoff <- if (isTRUE(mature_only)) {
    .tbl_now_maturity_threshold(
      context$x,
      dplyr::tibble(delay = cases$event_to_report, weight = cases$count),
      0.95
    )
  } else {
    NA
  }

  rows <- lapply(context$labels, function(label) {
    selected <- .summary_rows(context, label) & !is.na(cases$event_to_report)
    if (!is.na(cutoff)) selected <- selected & cases$event_date <= cutoff
    if (!any(selected)) return(NULL)

    event <- cases$event_date[selected]
    delay <- cases$event_to_report[selected]
    count <- cases$count[selected]
    eventual <- tapply(count, factor(event), sum)
    # An event date with no cases at all has no "share of its total".
    eventual <- eventual[!is.na(eventual) & eventual != 0]
    if (length(eventual) == 0) return(NULL)

    dplyr::bind_rows(lapply(delays, function(d) {
      arrived <- tapply(
        count * (delay <= d), factor(event), sum
      )[names(eventual)]
      arrived[is.na(arrived)] <- 0
      shares <- as.numeric(arrived / eventual)
      row <- .summary_stat_row(
        shares, NULL, "completeness", paste0("delay <= ", d), label,
        n = length(shares), total = sum(arrived)
      )
      row$prop <- sum(arrived) / sum(eventual)
      row
    }))
  })

  dplyr::bind_rows(rows)
}

#' Ratio of one delay's running total to the previous one's
#'
#' Built by cumulating the incidence over a complete `0:k` delay grid within
#' each event date, so a delay with no report carries the previous total
#' forward rather than dropping out.
#'
#' @param context A summary context.
#' @param k Number of delays.
#'
#' @return A tibble of `"growth"` rows.
#'
#' @keywords internal
#' @noRd
.summary_growth <- function(context, k) {
  k <- as.integer(k)
  if (length(k) != 1 || is.na(k) || k < 1) {
    cli::cli_abort("{.arg k} must be a single positive whole number.")
  }
  cases <- context$cases

  rows <- lapply(context$labels, function(label) {
    selected <- .summary_rows(context, label) & !is.na(cases$event_to_report)
    if (!any(selected)) return(NULL)

    event <- factor(cases$event_date[selected])
    delay <- cases$event_to_report[selected]
    count <- cases$count[selected]

    # rows = event dates, columns = delays 0..k, filled with zeros.
    running <- matrix(0, nrow = nlevels(event), ncol = k + 1L)
    inside <- delay >= 0 & delay <= k
    if (any(inside)) {
      cells <- cbind(as.integer(event)[inside], as.integer(delay[inside]) + 1L)
      running[cells] <- running[cells] + count[inside]
    }
    running <- t(apply(running, 1, cumsum))

    dplyr::bind_rows(lapply(seq_len(k), function(d) {
      previous <- running[, d]
      current <- running[, d + 1L]
      # A ratio out of nothing is infinite, not large; those event dates are
      # dropped rather than allowed to dominate the mean.
      usable <- previous > 0
      .summary_stat_row(
        as.numeric(current[usable] / previous[usable]), NULL,
        "growth", paste0("delay ", d), label,
        n = sum(usable), total = sum(current[usable] - previous[usable])
      )
    }))
  })

  dplyr::bind_rows(rows)
}
