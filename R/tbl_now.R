#' Create a `tbl_now` object
#'
#' A special `data.frame` class that includes information for the nowcast.
#' See the Attributes section for more information.
#'
#' @param data A `data.frame` to be converted.
#'
#' @param event_date Character. The name of the column containing the event date.
#'
#' @param report_date Character. The name of the column containing the report date.
#'
#' @param is_batched (optional) Character or `NULL` (default). The name of a column containing either
#' `TRUE` or `FALSE` indicating whether the `report_date` is correctly specified
#' or corresponds to a `batch` and thus is censored. In other words, if the
#' `report_date` is accurately measured set `report_date = TRUE` but if
#' the `report_date` corresponds to an error and is only an upper bound of
#' the real, idealized, report date set `is_batched = TRUE`.
#'
#' @param strata (optional) Character vector or `NULL` (default). Name of different variables
#' (column names) in strata. Strata correspond to variables that are of interest
#' by themselves. For example if it is of interest to generate nowcasts by gender then
#' `gender` is a `strata`.
#'
#' @param covariates (optional) Character vector or `NULL` (default). Name of different variables
#' (column names) that influence the nowcast but are not strata.
#' For example precipitation might influence a dengue nowcast but in general it
#' is not of interest to generate nowcasts by precipitation levels.
#'
#' @param now (optional) Date or `NULL` (default). The date that is considered the `now` of the
#' nowcast. If no `now` is given then the function automatically uses the last
#' `event_date`.
#'
#' @param date_units (optional) Character. Either "auto" (default), "weekly",  "daily"
#' or "numeric". Use `numeric` when the temporal effects are given as covariates.
#'
#' @param data_type (optional) Character. Either "auto", "linelist" or
#' "count".
#'
#' @param verbose (optional) Logical. Whether to throw a message. Default = `TRUE`.
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
#'   \item{num_strata}{Number of strata. Corresponds to `length(strata)`.}
#'   \item{covariates}{Names of the columns corresponding to covariates (for modelling).}
#'   \item{num_covariates}{Number of covariates Corresponds to `length(covariates)`.}
#'   \item{now}{Date of the `now` for a nowcast.}
#'   \item{date_units}{Either `daily`, `weekly` or `numeric`. Corresponds to the units of `event_date`
#'   and `report_date`}
#'   \item{data_type}{Either `linelist` or `count` depending on whether it is linelist data
#'   or count data}
#' }
#'
#' @examples
#' # The `tbl_now` is a data.frame with additional attributes
#' data(denguedat)
#' ndata <- tbl_now(denguedat,
#'     event_date = "onset_week",
#'     report_date = "report_week",
#'     strata = "gender")
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
#' ndata <- ndata[,-4]
#' ndata
#'
#' #Like selecting
#' ndata[1:10,]
#' ndata
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
#' @name tbl_now

#' @rdname tbl_now
#' @export
new_tbl_now <- function(data,
                        event_date,
                        report_date,
                        strata = NULL,
                        covariates = NULL,
                        is_batched = NULL,
                        now = NULL,
                        date_units = "auto",
                        data_type = "auto",
                        verbose = TRUE,
                        ...) {

  #Check the data frame data--------
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a {.code data.frame}")
  }

  if (dplyr::is.grouped_df(data)){
    cli::cli_warn("{.arg data} is grouped by {colnames(dplyr::group_keys(data))}. Ungrouping.")
    data <- data %>% dplyr::ungroup()
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
  now        <- infer_now(data, now = now, event_date = event_date,
                          report_date = report_date)

  # Infer the date_units whether it is daily, weekly, monthly or yearly
  date_units <- infer_units(data, date_units = date_units, event_date = event_date,
                            report_date = report_date)

  # Get whether data is count or line data
  data_type  <- infer_data_type(data, data_type = data_type, verbose = verbose)

  # Capture all other attributes
  other_attrs <- list(...)

  # === 3. Attribute Assignment ===
  data <- dplyr::as_tibble(data)

  # Set the core attributes (if adding new attributes here change in validate_tbl_now too)
  attr(data, "event_date")     <- event_date
  attr(data, "report_date")    <- report_date
  attr(data, "num_strata")     <- num_strata
  attr(data, "strata")         <- strata
  attr(data, "num_covariates") <- num_covariates
  attr(data, "covariates")     <- covariates
  attr(data, "now")            <- now
  attr(data, "date_units")     <- date_units
  attr(data, "data_type")      <- data_type

  # Add all other attributes from ...
  for (attr_name in names(other_attrs)) {
    attr(data, attr_name) <- other_attrs[[attr_name]]
  }

  # === 4. Class Assignment ===
  # Prepend the S3 class. It will inherit from data.frame.
  class(data) <- c("tbl_now", class(data))

  #Validate
  validate_tbl_now(data)

  return(data)
}

#' @rdname tbl_now
#' @export
tbl_now <- function(data,
                    event_date,
                    report_date,
                    strata = NULL,
                    covariates = NULL,
                    is_batched = NULL,
                    now = NULL,
                    date_units = "auto",
                    data_type = "auto",
                    verbose = TRUE,
                    ...){

  new_tbl_now(data = data,
              event_date = event_date,
              report_date = report_date,
              strata = strata,
              covariates = covariates,
              is_batched = is_batched,
              now = now,
              date_units = date_units,
              data_type = data_type,
              verbose = verbose,
              ...)

}

