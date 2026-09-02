#' Convert between linelist and aggregated count data
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Surveillance data comes in three shapes, and different nowcasting packages
#' want different ones. `to_count()` moves a [tbl_now()] between them:
#'
#' * **`linelist`** -- one row per case. The most detailed shape.
#' * **`count-incidence`** -- one row per (event date, report date) pair, holding
#'   the number of cases reported *on exactly that report date*.
#' * **`count-cumulative`** -- the same grid, but holding the number of cases
#'   known *up to and including* that report date. This is the shape most public
#'   dashboards publish.
#'
#' You can go from `linelist` to either count shape, and back and forth between
#' the two count shapes. You cannot go back to `linelist`: once cases have been
#' added up, the individual rows are gone.
#'
#' @details
#' This is an S3 generic. The package provides a method for `tbl_now` objects,
#' which aggregates into the `case_count` column, creating one named `n` when the
#' object does not already have one.
#'
#' Aggregation sums over every column the object has *not* been told about, so a
#' column you care about should be declared as a strata or covariate first (see
#' [add_strata()]) or it will be summed away.
#'
#' @section Grouping is dropped, and said so:
#' `to_count()` **ungroups**, and warns when it does. It is the one verb in the
#' package that does not put the caller's grouping back, and the reason is that
#' it changes what a row *is*: after aggregating, one row is an (event date,
#' report date) cell rather than one of the rows that were grouped, so the
#' grouping no longer describes anything in the object.
#'
#' A grouping is also not how you keep a column out of the sum. Declare it --
#' [add_strata()] or [add_covariates()] -- and it becomes part of the cell key.
#' The reported-cases getters do respect a grouping, because they select rather
#' than reshape; see [get_latest_reported_cases()].
#'
#' @section Statistical details:
#' Converting `count-cumulative` to `count-incidence` **de-accumulates** the
#' series: within each event date (and grouping), ordered by report date, the
#' increment is that cumulative total minus the previous one. Because published
#' cumulative totals are sometimes revised *downward*, an increment can be
#' **negative**. That is not a bug -- it is a retraction showing through -- but
#' code that requires non-negative counts (for example
#' [tbl_now_to_baselinenowcast()]) must handle or refuse it.
#'
#' @param x A [tbl_now()] object to convert.
#'
#' @param to Character. The data type to produce: `"linelist"`,
#'   `"count-incidence"` or `"count-cumulative"`. Defaults to the object's
#'   current type, i.e. no change.
#'
#' @param ... Additional arguments passed to methods.
#'
#' @note `linelist` data cannot be reconstructed from `count-*` data. Asking for
#' it throws an error, because aggregated data cannot be un-counted.
#'
#' @return A `tbl_now` object of the requested `to` data type, with the counts
#' aggregated into the `case_count` column.
#'
#' @examples
#' data(denguedat)
#' ndata <- tbl_now(denguedat,
#'   event_date = "onset_week",
#'   report_date = "report_week",
#'   strata = "gender"
#' )
#'
#' # A linelist has one row per case ...
#' nrow(ndata)
#'
#' ## ... which becomes one row per (onset week, report week, gender), with the
#' # number of cases in `n`.
#' counts <- to_count(ndata, to = "count-incidence")
#' counts
#'
#' # Cumulative totals instead: how many cases for that onset week were known by
#' # each report week. Within an onset week these only ever go up.
#' to_count(counts, to = "count-cumulative")
#'
#' # Going back to a linelist is impossible -- the individual cases are gone.
#' try(to_count(counts, to = "linelist"))
#'
#' @seealso
#' [tbl_now()] and its *Data types* section for what each shape means;
#' [get_data_type()] to ask an object which shape it currently is;
#' [complete_zeroes()] to fill in the (event, report) pairs where nothing was
#' reported; [get_latest_reported_cases()] to pull out the most recent counts.
#'
#' @export
to_count <- function(x, to = NULL, ...) {
  UseMethod("to_count")
}

#' @export
#' @rdname to_count
to_count.tbl_now <- function(x, to = NULL, ...) {
  # Unlike the other verbs, this one does NOT put the grouping back: aggregating
  # rewrites what a row is, and a grouping is a statement about rows that no
  # longer exist. Returning it silently was the complaint in #61, so say it --
  # and say it only when there is something to say.
  group_columns <- dplyr::group_vars(x)
  if (length(group_columns) > 0) {
    cli::cli_warn(c(
      "{.fn to_count} aggregates, so it dropped the grouping by
       {.val {group_columns}}.",
      "i" = "One row is now an (event, report) cell, not one of the rows you
             grouped, so the grouping no longer describes the object.",
      "i" = "{cli::qty(group_columns)}Declare the column{?s} with
             {.fn add_strata} or {.fn add_covariates} to keep {?it/them} out of
             the aggregation, or regroup afterwards."
    ))
  }
  x <- x |> ungroup()

  # Fill the nulls
  case_count <- get_case_count(x)
  if (is.null(case_count)) case_count <- "n"
  if (is.null(to)) to <- get_data_type(x)

  # Create the grouping vector
  gp_vec <- c(
    get_event_date(x),
    get_report_date(x),
    ".event_num",
    ".report_num",
    get_is_censored_report(x),
    # A confirmed case and its own retraction share an (event, report) pair;
    # summing over them would net one against the other silently.
    .validation_group_cols(x),
    get_strata(x),
    get_temporal_effect_cols(x),
    get_covariates(x)
  )

  # Group by event_date and report_date
  # Group data to generate counts
  x <- x |>
    dplyr::group_by(dplyr::across(dplyr::all_of(gp_vec)))


  # In case it was count just group and sum
  if (get_data_type(x) == "count-incidence" & to == "count-incidence") {
    # Summarise
    x <- x |>
      summarise(!!as.symbol(case_count) := sum(!!as.symbol(case_count)), .groups = "drop")
  } else if (get_data_type(x) == "linelist" & to == "count-incidence") {
    # Change the attribute first to avoid the warning from summarise
    attr(x, "data_type") <- "count-incidence"
    attr(x, "case_count") <- case_count


    # Summarise
    x <- x |>
      summarise(!!as.symbol(case_count) := dplyr::n(), .groups = "drop")
  } else if (get_data_type(x) == "linelist" & to == "count-cumulative") {
    # Go linelist -> count incidence -> count cumulative
    # `x` is grouped by the cell key above; the recursive calls do their own
    # grouping and would otherwise warn about one this function set itself.
    x <- x |>
      ungroup() |>
      to_count(to = "count-incidence") |>
      to_count(to = "count-cumulative")
  } else if (get_data_type(x) == "count-incidence" & to == "count-cumulative") {
    # Summarise
    x <- x |>
      ungroup() |>
      to_count(to = "count-incidence") |> # Just to make sure 1 obs per
      dplyr::group_by(dplyr::across(dplyr::all_of(c(get_event_date(x), get_is_censored_report(x), .validation_group_cols(x), get_strata(x), get_temporal_effect_cols(x), get_covariates(x))))) |>
      dplyr::arrange(dplyr::across(dplyr::all_of(get_report_date(x))), .by_group = TRUE) |>
      dplyr::mutate(!!as.symbol(case_count) := cumsum(!!as.symbol(case_count))) |>
      ungroup()

    attr(x, "data_type") <- "count-cumulative"
  } else if (get_data_type(x) == "count-cumulative" & to == "count-cumulative") {
    x <- x |>
      summarise(!!as.symbol(case_count) := sum(!!as.symbol(case_count)), .groups = "drop")
  } else if (get_data_type(x) == "count-cumulative" & to == "count-incidence") {
    # De-accumulate: within each series (event date x grouping, ordered by report
    # date) the incremental count is the cumulative total minus the previous one.
    # Because cumulative totals can be revised *downward*, an increment can be
    # negative; callers that need non-negative increments must handle that.
    x <- x |>
      ungroup() |>
      to_count(to = "count-cumulative") |> # collapse any duplicate cells first
      dplyr::group_by(dplyr::across(dplyr::all_of(c(get_event_date(x), get_is_censored_report(x), .validation_group_cols(x), get_strata(x), get_temporal_effect_cols(x), get_covariates(x))))) |>
      dplyr::arrange(dplyr::across(dplyr::all_of(get_report_date(x))), .by_group = TRUE) |>
      dplyr::mutate(!!as.symbol(case_count) := !!as.symbol(case_count) - dplyr::lag(!!as.symbol(case_count), default = 0)) |>
      ungroup()

    attr(x, "data_type") <- "count-incidence"
  } else if (get_data_type(x) == "linelist" & to == "linelist") {
    x <- x |>
      ungroup()
  } else {
    cli::cli_abort("Transformation from `data_type` {get_data_type(x)} to {to} not implemented")
  }

  x <- x |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c(get_event_date(x), get_strata(x), get_is_censored_report(x), .validation_group_cols(x), get_covariates(x), get_temporal_effect_cols(x)))))

  # Return the count
  return(x)
}
