#' Create a `tbl_now` object
#'
#' A special `tibble` class that includes information for the nowcast.
#' See the Attributes section for more information.
#'
#' @param data A `data.frame` or `tibble` to be converted.
#'
#' @param event_date \code{\link[dplyr:dplyr_tidy_select]{<`tidy-select`>}}
#' name of the column containing the event date.
#'
#' @param report_date \code{\link[dplyr:dplyr_tidy_select]{<`tidy-select`>}}
#' name of the column containing the report date.
#'
#' @param case_count (optional) \code{\link[dplyr:dplyr_tidy_select]{<`tidy-select`>}} or `NULL`
#' Name of the column with the case counts if `data_type` is "count-incidence"
#' or "count-cumulative".
#'
#' @param is_censored (optional)
#' \code{\link[dplyr:dplyr_tidy_select]{<`tidy-select`>}} or `NULL` (default).
#' The name of a column containing either `TRUE` or `FALSE` indicating whether
#' the `report_date` is correctly specified or corresponds to a `batch` and thus
#' is censored. In other words, if the `report_date` is accurately measured
#' set `is_censored = FALSE` but if the `report_date` corresponds to an error
#' and is only an upper bound of the real report date
#' set `is_censored = TRUE`.
#'
#' @param strata (optional) \code{\link[dplyr:dplyr_tidy_select]{<`tidy-select`>}}
#' or `NULL` (default). Name of different variables (column names) in strata.
#' Strata correspond to variables that are of interest by themselves.
#' For example if it is of interest to generate nowcasts by gender then
#' `gender` is a `strata`.
#'
#' @param covariates (optional) \code{\link[dplyr:dplyr_tidy_select]{<`tidy-select`>}}
#' or `NULL` (default). Name of different variables
#' (column names) that influence the nowcast but are not strata.
#' For example precipitation might influence a dengue nowcast but in general it
#' is not of interest to generate nowcasts by precipitation levels.
#'
#' @param now (optional) Date or `NULL` (default). The date that is considered the `now` of the
#' nowcast. If no `now` is given then the function automatically uses the last
#' `event_date`.
#'
#' @param t_effects (optional) Either `NULL` (default), a [temporal_effects()] object
#' or a character vector with the names of the columns containing the temporal effects.
#'
#' @param event_units (optional) Character. Either "auto" (default), "days",
#' "weeks", "months", "years" or "numeric".
#'
#' @param report_units (optional) Character. Either "auto" (default), "days",
#' "weeks", "months", "years" or "numeric".
#'
#' @param data_type (optional) Character. Either "auto", "linelist" or
#' "count-incidence" or "count-cumulative". See section below for
#' an explanation on data types.
#'
#' @param verbose (optional) Logical. Whether to throw a message. Default = `TRUE`.
#'
#' @param force (optional) Logical. Whether to force computation overwriting pre-existing variables.
#' Default = `FALSE`.
#'
#' @param warn_non_uniqueness (optional) Logical. Whether to throw a warning if data
#' has multiple observations for same event and report date (conditional on covariates
#' and strata)
#'
#' @param ... Additional metadata to be stored as attributes.
#'
#' @section Attributes:
#'
#' The following attributes are part of a `tbl_now` and are validated
#' by the [validate_tbl_now()] function:
#'
#' \describe{
#'   \item{event_date}{Name of the column refering to the event of interest.}
#'   \item{report_date}{Name of the column refering to when the event of interest was reported.}
#'   \item{strata}{Names of the columns corresponding to the strata (for modelling).}
#'   \item{covariates}{Names of the columns corresponding to covariates (for modelling).}
#'   \item{case_count}{Column containing the number of observations for that moment if `data_type` is `count-incidence` or `count-cumulative`.}
#'   \item{temporal_effects}{Names of the columns refering to the temporal effects.}
#'   \item{now}{Date of the `now` for a nowcast.}
#'   \item{is_censored}{Column indicating whether the measurement is noisy (only upper bound) or not.}
#'   \item{event_units}{Either `days`, `weeks`, `months`, `years` or `numeric`. Corresponds to the units of `event_date`}
#'   \item{report_units}{Either `days`, `weeks`, `months`, `years` or `numeric`. Corresponds to the units of `report_date`}
#'   \item{repot_num}{Column where the `report_date` was transformed to numeric values}
#'   \item{event_num}{Column where the `event_date` was transformed to numeric values}
#'   \item{data_type}{Either `linelist`, `count-incidence` or `count-cumulative` depending on whether it is linelist data
#'   or count data with incidence (each report date's incidence) or cumulative (overall known cases at report date)}
#' }
#'
#' You can  list all `tbl_now` related attributes in a specific `tbl_now` with [tbl_now_attributes()].
#'
#' @section Data types:
#' The following data-types are admitted at `tbl_now` objects.
#'
#' *Linelist*
#'
#' Each row is an individual that was reported at `report_date` as happening at `event_date`.
#'
#' ```{r, cache=TRUE}
#' df <- data.frame(
#'  patient     = 1:6,
#'  event_date  = c(rep(as.Date("2020/09/12"), 3),
#'                  rep(as.Date("2020/09/13"), 3)),
#'  report_date = c(as.Date("2020/09/12"),
#'                  as.Date("2020/09/13"),
#'                  as.Date("2020/09/14"),
#'                  as.Date("2020/09/13"),
#'                  as.Date("2020/09/14"),
#'                  as.Date("2020/09/15")))
#' print(df)
#' ```
#'
#' *Count-incidence*
#'
#' Each `report_date`-`event_date` combination contains the total number of
#' cases observed _exactly_ at `report_date` for `event_date`.
#'
#' ```{r, cache=TRUE}
#' df <- data.frame(
#'  n           = c(7, 1, 9, 5, 0, 2),
#'  event_date  = c(rep(as.Date("2020/09/12"), 3),
#'                  rep(as.Date("2020/09/13"), 3)),
#'  report_date = c(as.Date("2020/09/12"),
#'                  as.Date("2020/09/13"),
#'                  as.Date("2020/09/14"),
#'                  as.Date("2020/09/13"),
#'                  as.Date("2020/09/14"),
#'                  as.Date("2020/09/15")))
#' print(df)
#' ```
#'
#' *Count-cumulative*
#'
#' Each `report_date`-`event_date` combination contains the total number of
#' cases observed up until `report_date` for `event_date`. The most recent
#' `report_date` contains the best estimation of cases happening at `event_date`.
#'
#' ```{r, cache=TRUE}
#' df <- data.frame(
#'  n           = c(1,5, 8, 2, 2, 4),
#'  event_date  = c(rep(as.Date("2020/09/12"), 3),
#'                  rep(as.Date("2020/09/13"), 3)),
#'  report_date = c(as.Date("2020/09/12"),
#'                  as.Date("2020/09/13"),
#'                  as.Date("2020/09/14"),
#'                  as.Date("2020/09/13"),
#'                  as.Date("2020/09/14"),
#'                  as.Date("2020/09/15")))
#' print(df)
#' ```
#'
#' The [to_count()] function allows you to easily convert from between different
#' data-types.
#'
#' @examples
#' # The `tbl_now` is a data.frame with additional attributes
#' data(denguedat)
#' ndata <- denguedat %>%
#'   tbl_now(event_date = onset_week, report_date = report_week,
#'     strata = gender)
#'
#' # You can see that it documents the `event_date`, `report_date`, `strata`,
#' # `covariates` as well as the `now`.
#' ndata
#'
#'
#' #A `tbl_now` is an extension of a `tibble` which means normal
#' #`data.frame` operations are permitted
#' ndata$newcolumn <- "something"
#' ndata
#'
#' #Like removing a column
#' ndata[,-4]
#'
#' #Like selecting
#' ndata[1:10,]
#'
#' #You can also apply all dplyr functions:
#' ndata %>%
#'   dplyr::filter(report_week <= as.Date("1991-01-02", format = "%Y-%m-%d"))
#'
#' #Removing an important column automatically transforms to tibble
#' #losing its property
#' suppressWarnings(
#'   ndata %>%
#'     dplyr::select(-onset_week)
#' )
#'
#' #Removing strata just changes the overall structure
#' ndata %>% dplyr::select(-gender)
#'
#' @return An object of class `tbl_now`.
#'
#' @export
#' @md
tbl_now <- function(data,
                    event_date,
                    report_date,
                    strata = NULL,
                    covariates = NULL,
                    case_count = NULL,
                    is_censored = NULL,
                    now = NULL,
                    event_units = "auto",
                    report_units = "auto",
                    data_type = "auto",
                    t_effects = NULL, #FIXME: Do tidy select here too
                    verbose = TRUE,
                    force = FALSE,
                    warn_non_uniqueness = TRUE,
                    ...) {


  #Check the data frame data--------
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a {.code data.frame}")
  }

  if (dplyr::is.grouped_df(data)){
    cli::cli_warn("{.arg data} is grouped by {colnames(dplyr::group_keys(data))}. Ungrouping.")
    data <- data %>% dplyr::ungroup()
  }


  #Get event date column
  event_col_select <- tidyselect::eval_select(rlang::expr({{ event_date }}), data)
  event_date       <- colnames(data)[event_col_select]

  report_col_select <- tidyselect::eval_select(rlang::expr({{ report_date }}), data)
  report_date       <- colnames(data)[report_col_select]

  case_count_select <- tidyselect::eval_select(rlang::expr({{ case_count }}), data)
  case_count        <- colnames(data)[case_count_select]
  if (length(case_count) == 0) case_count <- NULL

  is_censored_select <- tidyselect::eval_select(rlang::expr({{ is_censored }}), data)
  is_censored        <- colnames(data)[is_censored_select]
  if (length(is_censored) == 0) is_censored <- NULL

  strata_select <- tidyselect::eval_select(rlang::expr({{ strata }}), data)
  strata        <- colnames(data)[strata_select]
  if (length(strata) == 0) strata <- NULL

  covariates_select <- tidyselect::eval_select(rlang::expr({{ covariates }}), data)
  covariates        <- colnames(data)[covariates_select]
  if (length(covariates) == 0) covariates <- NULL


  if (length(case_count) > 1){
    cli::cli_abort(
      "{.code case_count} has to be either `NULL` or a character with just one column name."
    )
  }

  #Check the date columns are dates-------
  check_date_columns(data, event_date = event_date, report_date = report_date)

  #Check the strata-----
  num_strata <- length(strata)
  if (!is.null(strata) && num_strata > 1){
    if (!is.character(strata)) {
      cli::cli_abort("{.arg strata} must be either `NULL` or a string of column names")
    }

    for (st in strata){
      if (!(st %in% colnames(data))) {
        cli::cli_abort("{.arg strata} = {st} not found in data")
      }
    }
  }

  #Check the covariates
  num_covariates <- length(covariates)
  if (!is.null(covariates) && num_covariates > 1){
    if (!is.character(covariates)) {
      cli::cli_abort("{.arg covariates} must be either `NULL` or a string of column names")
    }

    for (cv in covariates){
      if (!(cv %in% colnames(data))) {
        cli::cli_abort("{.arg covariates} = {cv} not found in data")
      }
    }
  }

  # Infer automatic variables------

  #Infer the now
  now          <- infer_now(data, now = now, event_date = event_date, report_date = report_date)

  # Infer the date_units whether it is daily, weekly, monthly or yearly
  event_units  <- infer_units(data, date_column = event_date, date_units = event_units)
  report_units <- infer_units(data, date_column = report_date, date_units = report_units)

  # Get whether data is count or line data
  data_type    <- infer_data_type(data, data_type = data_type,
                                  event_date = event_date, report_date = report_date,
                                  strata = strata,
                                  is_censored = is_censored,
                                  case_count = case_count, verbose = verbose)

  # Capture all other attributes
  other_attrs  <- list(...)

  # === 3. Attribute Assignment ===
  data <- dplyr::as_tibble(data)

  # Set the core attributes (if adding new attributes here change in validate_tbl_now too)
  attr(data, "event_date")     <- event_date
  attr(data, "report_date")    <- report_date
  attr(data, "case_count")       <- case_count
  attr(data, "strata")         <- strata
  attr(data, "covariates")     <- covariates
  attr(data, "now")            <- now
  attr(data, "event_units")    <- event_units
  attr(data, "report_units")   <- report_units
  attr(data, "data_type")      <- data_type
  attr(data, "is_censored")     <- is_censored

  # Add all other attributes from ...
  for (attr_name in names(other_attrs)) {
    attr(data, attr_name) <- other_attrs[[attr_name]]
  }

  # Add report_num and event_num the numerical columns
  if (".event_num" %in% colnames(data) && !force){
    cli::cli_abort(
      "Data already has a column named {.val .event_num} which this class uses to save the numeric version of the event_date. Please rename your {.val .event_num} column."
    )
  }
  if (".report_num" %in% colnames(data) && !force){
    cli::cli_abort(
      "Data already has a column named {.val .report_num} which this class uses to save the numeric version of the report_date. Please rename your {.val .report_num} column."
    )
  }

  #Convert time columns to numeric
  data <- time_cols_to_numeric(data, event_date = event_date, report_date = report_date,
                               event_units = event_units, report_units = report_units,
                               force = force)


  # === 4. Class Assignment ===
  # Prepend the S3 class. It will inherit from data.frame.
  class(data) <- c("tbl_now", class(data))

  #Add temporal effects
  if (!is.null(t_effects) && S7::S7_inherits(t_effects, class = temporal_effects)){
    data <- data %>%
      add_temporal_effects(t_effects= t_effects)
  } else if (!is.null(t_effects) && is.character(t_effects)){
    attr(data, "temporal_effects") <- t_effects
  }

  #Validate
  validate_tbl_now(data, warn_non_uniqueness = warn_non_uniqueness)

  return(data)
}


