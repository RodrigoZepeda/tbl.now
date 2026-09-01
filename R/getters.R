#' @title Read what a `tbl_now` was told about itself
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' When you build a [tbl_now()] you tell it which column is the event date, which
#' is the report date, which are strata, and so on. These functions read that
#' information back.
#'
#' They are how the rest of the package -- and any modelling code you write
#' yourself -- finds the right columns without hard-coding names. Rather than
#' assuming a column is called `onset_week`, write
#' `x[[get_event_date(x)]]` and your code works on any `tbl_now`.
#'
#' @details
#' Most of these return a **column name**, not the column itself. To get the
#' values, index with the name: `x[[get_event_date(x)]]`.
#'
#' A getter returns `NULL` when the object was never told about that attribute,
#' so `is.null(get_strata(x))` is the test for "unstratified". The two counting
#' helpers, `get_num_strata()` and `get_num_covariates()`, return `0` instead,
#' which is usually easier to work with.
#'
#' @param x A `tbl_now` object.
#'
#' @return
#' A column name, a count, or a metadata value, depending on the function:
#'
#' \describe{
#'   \item{`get_event_date()`, `get_report_date()`}{Character. The name of the
#'     column holding the date the event happened / was reported.}
#'   \item{`get_case_count()`}{Character, or `NULL` for linelist data. The name
#'     of the column holding the number of cases.}
#'   \item{`get_strata()`, `get_covariates()`}{Character vector of column names,
#'     or `NULL` when there are none.}
#'   \item{`get_num_strata()`, `get_num_covariates()`}{Integer count, `0` when
#'     there are none.}
#'   \item{`get_is_censored()`}{Character, or `NULL`. The name of the column
#'     flagging reports whose date is only an upper bound.}
#'   \item{`get_now()`}{The `Date` (or number) the nowcast is anchored on.}
#'   \item{`get_event_units()`, `get_report_units()`}{One of `"days"`,
#'     `"weeks"`, `"months"`, `"years"` or `"numeric"` -- the grid each date
#'     lives on.}
#'   \item{`get_data_type()`}{One of `"linelist"`, `"count-incidence"` or
#'     `"count-cumulative"`. See [to_count()].}
#'   \item{`get_temporal_effects()`}{The [temporal_effects()] specification the
#'     object carries, or `NULL`. This is the *request*, not the data.}
#'   \item{`get_temporal_effect_cols()`}{Character vector of the temporal-effect
#'     columns actually materialised in the data by
#'     [compute_temporal_effects()]; `character(0)` when none have been.}
#'   \item{`get_validation_date()`, `get_validation_type()`}{Character, or
#'     `NULL`. The name of the column holding the date a case was resolved, and
#'     of the column holding how it resolved.}
#'   \item{`get_validation_units()`}{The grid the validation date lives on,
#'     or `NULL` when the object carries no validation process.}
#'   \item{`has_validation()`}{`TRUE` when the object carries a validation
#'     date. Every code path must work when it is `FALSE`, because most objects
#'     have no third date.}
#' }
#'
#' @section The validation process, the optional third date:
#'
#' A `tbl_now` may carry a **third** date beyond the event and the report: the
#' date a case was resolved, either confirmed or retracted. Think of influenza:
#' symptom onset is the event, the medical visit is the report, and the
#' laboratory result is the validation -- which can come back negative, in
#' which case the case is *retracted* rather than confirmed.
#'
#' It is optional and most objects do not have one, so `has_validation()` gates
#' the four getters below it: they all return `NULL` on an object that was never
#' given a third date.
#'
#' @seealso
#' [tbl_now_attributes()] to get all of them at once;
#' [add()], [change()][add] and [remove()][add] to set them, including
#' [add_validation_date()][add];
#' [get_latest_confirmed()][validation_counts] and
#' [get_net_confirmed()][validation_counts] to count the outcomes;
#' [validation_delay] for how long resolution takes;
#' [get_latest_reported_cases()][get_latest_first] and friends for reading the
#' counts rather than the metadata.
#'
#' @examples
#' data(denguedat)
#' ndata <- denguedat |>
#'   tbl_now(
#'     event_date = onset_week,
#'     report_date = report_week,
#'     strata = gender,
#'     t_effects = temporal_effects(month_of_year = TRUE),
#'     verbose = FALSE
#'   ) |>
#'   compute_temporal_effects()
#'
#' # The two dates every nowcast needs.
#' get_event_date(ndata)
#' get_report_date(ndata)
#'
#' # Use the name to reach the column, so the code does not depend on it.
#' head(ndata[[get_event_date(ndata)]])
#'
#' # Strata are groups you want separate nowcasts for; covariates are not.
#' get_strata(ndata)
#' get_num_strata(ndata)
#'
#' ## Nothing was declared a covariate, so this is NULL (and the count is 0).
#' get_covariates(ndata)
#' get_num_covariates(ndata)
#'
#' # Likewise for a censoring indicator that was never supplied.
#' get_is_censored(ndata)
#'
#' # The as-of moment, and the calendar grid the dates live on.
#' get_now(ndata)
#' get_event_units(ndata)
#' get_report_units(ndata)
#'
#' # Linelist means one row per case; there is no count column yet.
#' get_data_type(ndata)
#' get_case_count(ndata)
#'
#' ## After to_count() there is one.
#' counts <- to_count(ndata, to = "count-incidence")
#' get_data_type(counts)
#' get_case_count(counts)
#'
#' # The temporal-effects request, versus the columns it actually produced.
#' get_temporal_effects(ndata)
#' get_temporal_effect_cols(ndata)
#'
#' # The third date is optional, so ask before you read it.
#' has_validation(ndata)
#' get_validation_date(ndata)
#'
#' ## Once one is attached, the same name-then-index pattern applies.
#' data(hai_bucaramanga)
#' hai <- hai_bucaramanga |>
#'   dplyr::filter(!is.na(specimen_date), !is.na(report_date)) |>
#'   tbl_now(
#'     event_date = specimen_date, report_date = report_date,
#'     data_type = "linelist", verbose = FALSE
#'   ) |>
#'   add_validation_date(received_date) |>
#'   suppressWarnings()
#'
#' has_validation(hai)
#' get_validation_date(hai)
#' get_validation_units(hai)
#' head(hai[[get_validation_date(hai)]])
#'
#' @name nowcast_data_getters
NULL

#' @rdname nowcast_data_getters
#' @export
get_event_date <- function(x) {
  attr(x, "event_date", exact = TRUE)
}

#' @rdname nowcast_data_getters
#' @export
get_report_date <- function(x) {
  attr(x, "report_date", exact = TRUE)
}

#' @rdname nowcast_data_getters
#' @export
get_strata <- function(x) {
  attr(x, "strata", exact = TRUE)
}

#' @rdname nowcast_data_getters
#' @export
get_num_strata <- function(x) {
  length(get_strata(x))
}

#' @rdname nowcast_data_getters
#' @export
get_covariates <- function(x) {
  attr(x, "covariates", exact = TRUE)
}

#' @rdname nowcast_data_getters
#' @export
get_num_covariates <- function(x) {
  length(get_covariates(x))
}

#' @rdname nowcast_data_getters
#' @export
get_now <- function(x) {
  attr(x, "now", exact = TRUE)
}

#' @rdname nowcast_data_getters
#' @export
get_report_units <- function(x) {
  attr(x, "report_units", exact = TRUE)
}

#' @rdname nowcast_data_getters
#' @export
get_event_units <- function(x) {
  attr(x, "event_units", exact = TRUE)
}

#' @rdname nowcast_data_getters
#' @export
get_data_type <- function(x) {
  attr(x, "data_type", exact = TRUE)
}

#' @rdname nowcast_data_getters
#' @export
get_temporal_effects <- function(x) {
  attr(x, "temporal_effects", exact = TRUE)
}

#' @rdname nowcast_data_getters
#' @export
get_temporal_effect_cols <- function(x) {
  val <- attr(x, "computed_temporal_effect_cols", exact = TRUE)
  if (is.null(val)) character(0) else val
}

#' @rdname nowcast_data_getters
#' @export
get_is_censored <- function(x) {
  attr(x, "is_censored", exact = TRUE)
}

#' @rdname nowcast_data_getters
#' @export
get_case_count <- function(x) {
  attr(x, "case_count", exact = TRUE)
}

#' Columns of a `tbl_now` that dplyr verbs must not drop
#'
#' Returns the union of the user-given protected columns
#' (`get_protected_given_cols()`) and the package-generated ones
#' (`get_protected_generated_cols()`).
#'
#' @param x A `tbl_now` object.
#'
#' @return A character vector of protected column names.
#'
#' @keywords internal
#' @noRd
get_protected_cols <- function(x) {
  # Return the protected columns from x
  c(get_protected_given_cols(x), get_protected_generated_cols(x))
}

#' Protected columns generated by `tbl_now`
#'
#' The auto-computed numeric/delay columns that `tbl_now()` adds and that must
#' not be removed without downgrading the object to a `tibble`.
#'
#' @param x A `tbl_now` object.
#'
#' @return A character vector: `".event_num"`, `".report_num"`, `".delay"`.
#'
#' @keywords internal
#' @noRd
get_protected_generated_cols <- function(x = NULL) {
  # Return the protected columns from x. The validation pair only exists when
  # the object was told about a validation date, so `x` is needed to know
  # whether to include it -- but the argument stays OPTIONAL, because this used
  # to take none and a caller that does not have the object in hand should get
  # the three columns every `tbl_now` has.
  base_columns <- c(".event_num", ".report_num", ".delay")
  if (is.null(x)) {
    return(base_columns)
  }
  c(base_columns, .validation_generated_cols(x))
}

#' Protected columns supplied by the user
#'
#' The columns the user passed to `tbl_now()` (event/report dates, the censoring
#' indicator and, for count data, the case-count column) that must not be
#' removed without downgrading the object to a `tibble`.
#'
#' @param x A `tbl_now` object.
#'
#' @return A named character vector of the user-given protected column names.
#'
#' @keywords internal
#' @noRd
get_protected_given_cols <- function(x) {
  # Return the protected columns from x
  protected_cols <- c(
    "event_date" = get_event_date(x), "report_date" = get_report_date(x),
    "is_censored" = get_is_censored(x),
    "validation_date" = get_validation_date(x),
    "validation_type" = get_validation_type(x)
  )

  if (!is.null(get_data_type(x)) && grepl("count", get_data_type(x))) {
    protected_cols <- c(protected_cols, "case_count" = get_case_count(x))
  }

  protected_cols
}
