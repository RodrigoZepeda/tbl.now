# This is mostly built using the
# **Extending dplyr with new data frame subclasses** at
# <https://dplyr.tidyverse.org/reference/dplyr_extending.html?q=reconstruct#ref-usage>
# and the code from **ExtendDataframes**
# <https://github.com/joshwlambert/ExtendDataFrames/blob/main/R/subset-reconstruct.R>


#' Validate a tbl_now object
#'
#' @description Checks that an object is a properly constructed `tbl_now`
#' with all required attributes and valid data.
#'
#' @param x An object to validate
#'
#' @inheritParams tbl_now
#'
#' @return Returns `TRUE` invisibly or throws an error. Called for its side effects.
#'
#' @examples
#' \dontrun{
#' data(denguedat)
#' ndata <- tbl_now(denguedat, event_date = "onset_week",
#'   report_date = "report_week", verbose = FALSE)
#'
#' # Validate without errors
#' validate_tbl_now(ndata)
#'
#' # Validate with errors
#' validate_tbl_now(data.frame(x = 1:3))
#'
#' }
#'
#' @keywords internal
validate_tbl_now <- function(x, warn_non_uniqueness = FALSE) {


  #Get required attributes
  required_attrs <- c("event_date", "report_date", "now", "event_units", "report_units",
                      "data_type")

  errors   <- character(0)
  warnings <- character(0)

  # # === 1. Check class ===
  if (!is.data.frame(x)) {
    errors <- c(errors, "Object must inherit from {.code data.frame}")
  }

  # === 2. Check required attributes exist ===
  for (attr_name in required_attrs) {
    if (is.null(attr(x, attr_name, exact = TRUE))) {
      errors <- c(errors, sprintf("Missing required attribute: {.val %s}", attr_name))
    }
  }

  # If required attributes are missing, return early
  if (length(errors) > 0) {
    cli::cli_abort(c("Invalid {.code tbl_now} object:", errors))
  }

  # === 3. Extract attributes for validation ===
  event_date     <- get_event_date(x)
  report_date    <- get_report_date(x)
  strata         <- get_strata(x)
  covariates     <- get_covariates(x)
  now            <- get_now(x)
  report_units   <- get_report_units(x)
  event_units    <- get_event_units(x)
  data_type      <- get_data_type(x)
  is_censored    <- get_is_censored(x)
  case_count     <- get_case_count(x)

  if (data_type == "linelist"){warn_non_uniqueness <- FALSE}

  # === 4. Validate attribute types ===

  # event_date and report_date must be character(1)
  if (!is.character(event_date) || length(event_date) != 1) {
    errors <- c(errors, "Attribute {.val event_date} must be a Date of length 1")
  }

  if (!is.character(report_date) || length(report_date) != 1) {
    errors <- c(errors, "Attribute {.val report_date} must be a Date of length 1")
  }


  # strata must be NULL or character
  if (!is.null(strata) && !is.character(strata)) {
    errors <- c(errors, "Attribute {.val strata} must be {.val NULL} or a character vector")
  }

  # covariates must be NULL or character
  if (!is.null(covariates) && !is.character(covariates)) {
    errors <- c(errors, "Attribute {.val covariates} must be {.val NULL} or a character vector")
  }

  # now must be a Date
  if ((!lubridate::is.Date(now) && !is.integer(now)) || length(now) != 1) {
    errors <- c(errors, "Attribute {.val now}  must be a Date or integer object of length 1")
  }

  # "event_units" and "report_units" must be valid option
  valid_date_units <- c("auto", "days", "weeks", "numeric", "months", "years")
  if (!is.character(report_units) || length(report_units) != 1 ||
      !report_units %in% valid_date_units) {
    errors <- c(errors, sprintf(
      "Attribute {.val report_units} must be one of: {.val %s}",
      paste(valid_date_units, collapse = ", ")
    ))
  }
  if (!is.character(event_units) || length(event_units) != 1 ||
      !event_units %in% valid_date_units) {
    errors <- c(errors, sprintf(
      "Attribute {.val event_units} must be one of: {.val %s}",
      paste(valid_date_units, collapse = ", ")
    ))
  }

  # data_type must be valid option
  valid_data_types <- c("auto", "linelist", "count-incidence","count-cumulative")
  if (!is.character(data_type) || length(data_type) != 1 ||
      !data_type %in% valid_data_types) {
    errors <- c(errors, sprintf(
      "Attribute {.val data_type} must be one of: {.val %s}",
      paste(valid_data_types, collapse = ", ")
    ))
  }

  # is_censored must be NULL or character(1)
  if (!is.null(is_censored) && (length(is_censored) != 1 || !is.character(is_censored))) {
    errors <- c(errors, "Attribute {.val is_censored} must be {.val NULL} or a character vector of length 1")
  }

  # === 5. Validate columns exist in data ===
  if (!is.null(event_date) && !event_date %in% colnames(x)) {
    errors <- c(errors, sprintf("Column {.val %s} (event_date) not found in data", event_date))
  }

  if (!is.null(report_date) && !report_date %in% colnames(x)) {
    errors <- c(errors, sprintf("Column {.val %s} (report_date) not found in data", report_date))
  }

  if (!is.null(is_censored) && length(is_censored) == 1 && !(is_censored %in% colnames(x))) {
    errors <- c(errors, sprintf("Column {.val %s} (is_censored) not found in data", is_censored))
  }

  if (!is.null(strata)) {
    for (st in strata) {
      if (!st %in% colnames(x)) {
        errors <- c(errors, sprintf("Strata column {.val %s} not found in data", st))
      }
    }
  }

  if (!is.null(covariates)) {
    for (cv in covariates) {
      if (!cv %in% colnames(x)) {
        errors <- c(errors, sprintf("Covariate column {.val %s} not found in data", cv))
      }
    }
  }

  # Check once per category--------------

  # Check that no covariate is in strata and viceversa
  if (any(covariates %in% strata)){

    #Get those that are repeated
    repeated_vars <- covariates[which(covariates %in% strata)]

    errors <- c(errors,
                sprintf("Strata variable {.val %s} cannot also be a covariate", repeated_vars))
  }

  # Check that no date is in strata
  if (report_date %in% strata){
    errors <- c(errors, sprintf("Report date {.val %s} cannot be strata", report_date))
  }
  if (event_date %in% strata){
    errors <- c(errors, sprintf("Event date {.val %s} cannot be strata", event_date))
  }

  # Check that no date is in covariate
  if (report_date %in% covariates){
    errors <- c(errors, sprintf("Report date {.val %s} cannot be a covariate", report_date))
  }
  if (event_date %in% covariates){
    errors <- c(errors, sprintf("Event date {.val %s} cannot be a covariate", event_date))
  }

  # Check that is_censored is not a covariate / strata
  if (!is.null(is_censored) && any(is_censored %in% covariates)){
    errors <- c(errors, sprintf("Censored indicator {.val %s} cannot be also a covariate", is_censored))
  }

  if (!is.null(is_censored) && any(is_censored %in% strata)){
    errors <- c(errors, sprintf("Censored indicator {.val %s} cannot be also strata", is_censored))
  }

  # === 6. Validate column types ===
  if (!is.null(event_date) && event_date %in% colnames(x)) {
    if (!lubridate::is.Date(x[[event_date]]) && !is.integer(x[[event_date]])) {
      errors <- c(errors, sprintf("Column '%s' must be of class Date or integer", event_date))
    }
  }

  if (!is.null(report_date) && report_date %in% colnames(x)) {
    if (!lubridate::is.Date(x[[report_date]])  && !is.integer(x[[report_date]])) {
      errors <- c(errors, sprintf("Column '%s' must be of class Date or integer", report_date))
    }
  }

  if (!is.null(is_censored) && length(is_censored) == 1 && is_censored %in% colnames(x)) {
    if (!is.logical(x[[is_censored]])) {
      errors <- c(errors, sprintf("Column '%s' must be logical (TRUE/FALSE)", is_censored))
    }
  }

  #Check they don't have the same report and event dates
  if (get_event_date(x) == get_report_date(x)){
    cli::cli_alert_warning(
      "Object has the same event and report dates with value {.val {get_event_date(x)}}"
    )
  }

  # Removing the case_count
  if (data_type != "linelist" && (is.null(case_count) || !(case_count %in% colnames(x)))){
    errors <- c(errors,
                paste0("Dropped case column ", case_count, " when data_type was ", data_type, "."))
  }

  # === 8. Validate data relationships ===

  if (!is.null(event_date) && !is.null(report_date) &&
      event_date %in% colnames(x) && report_date %in% colnames(x)) {

    # Check that report_date >= event_date (where both are non-NA)
    valid_rows <- !is.na(x[[event_date]]) & !is.na(x[[report_date]])
    if (any(valid_rows)) {
      invalid_dates <- x[[report_date]][valid_rows] < x[[event_date]][valid_rows]
      if (any(invalid_dates, na.rm = TRUE)) {
        warnings <- c(warnings, sprintf(
          "%d row(s) have a `report_date` before `event_date`",
          sum(invalid_dates, na.rm = TRUE)
        ))
      }
    }

    # Check that 'now' is >= max(report_date)
    if (nrow(x) > 0){
      max_report <- max(x[[report_date]], na.rm = TRUE)
      if (!is.na(max_report) && !is.null(now) && lubridate::is.Date(now) && now < max_report) {
        warnings <- c(warnings, sprintf(
          "Attribute 'now' (%s) seems to be in the past (before maximum report_date (%s))",
          as.character(now), as.character(max_report)
        ))
      }
    }
  }

  #FIXME: Throw warning when now is too far in the future or in the past


  #Validate that event_date and report_date don't have repeated values for same strata/covariates----
  if (warn_non_uniqueness){
    current_rows  <- nrow(x)
    distinct_rows <- x %>%
      dplyr::as_tibble() %>%
      dplyr::distinct_at(c(get_report_date(x), get_event_date(x), get_covariates(x), get_strata(x), get_is_censored(x), get_temporal_effects(x))) %>%
      nrow()

    if (current_rows > distinct_rows){
      warnings <- c(warnings,
                    paste0(
                      "*Non-unique*: Data has multiple rows for the same event ({event_date}) and report",
                      "({report_date}) dates. Consider using `to_count()` to aggregate the data or",
                      "`distinct()` to remove repeated observations."))
    }
  }


  # === 9. Return results ===
  if (length(errors) > 0) {
    cli::cli_abort(c("Invalid tbl_now object:", errors))
  }

  if (length(warnings) > 0) {
    for (w in warnings) {
      cli::cli_warn(w)
    }
  }

  return(invisible(TRUE))

}

#' Decides whether `tbl_now` object can be reconstructed from input
#'
#' @description Uses [`tbl_now_reconstruct_internal()`] to determine whether the
#' data input can be reconstructed in a valid `tbl_now` object. If it can
#' not, it is returned as a `data.frame`.
#'
#' @inheritParams tbl_now
#'
#' @return A `tbl_now` object (if the input is valid) or a `data.frame`
#' @keywords internal
tbl_now_reconstruct <- function(data, template) {

  tbl_now_reconstruct_internal(data, template)

}

#' Checks whether the `tbl_now` object is valid
#'
#' @description This is a wrapper for [`validate_tbl_now`] in a [`tryCatch()`]
#' in order to not error if the input object is invalid and returns `TRUE` or
#' `FALSE` on if the object is valid. If the object is valid it can be
#' "reconstructed" and not downgraded to a `data.frame`.
#'
#' @inheritParams tbl_now
#'
#' @return A boolean logical (`TRUE` or `FALSE`)
#' @keywords internal
tbl_now_can_reconstruct <- function(data) {

  # check whether input is valid, ignoring its class
  valid <- tryCatch(
    { validate_tbl_now(data) },
    error = function(cnd) FALSE
  )

  # return boolean
  !isFALSE(valid)
}

#' Reconstruct a tbl_now
#'
#' @inheritParams tbl_now
#'
#' @return A `tbl_now` object or a `data.frame`
#'
#' @keywords internal
tbl_now_reconstruct_internal <- function(data, template){

  # Copy over *all* attributes except the data.frame essentials
  attrs      <- attributes(template)

  keep_attrs <- attrs[setdiff(names(attrs), c("names", "row.names", "class", "groups"))]

  # Enforce protected columns
  protected_cols <- get_protected_cols(template)

  # Check the protected columns and return as data.frame instead
  missing_protected <- setdiff(protected_cols, names(data))
  if (length(missing_protected) > 0) {
    cli::cli_warn("Dropped protected column(?s): {.val {missing_protected}}. Returning a `tibble`")
    return(dplyr::as_tibble(data))
  }

  # Reattach attributes
  for (nm in names(keep_attrs)) {
    attr(data, nm) <- keep_attrs[[nm]]
  }

  # Update strata if columns were dropped
  if (!is.null(get_num_strata(template)) && get_num_strata(template) > 0) {
    #Get the strata still  here
    strata <- intersect(get_strata(template), names(data))

    #Reattach
    attr(data, "strata")      <- strata
  }

  # Update covariates if columns were dropped
  if (!is.null(get_num_covariates(template)) && get_num_covariates(template) > 0) {
    #Get the covariates still  here
    covariates <- intersect(get_covariates(template), names(data))

    #Reattach
    attr(data, "covariates")      <- covariates
  }

  # Update temporal effects if columns were dropped
  if (!is.null(get_temporal_effects(template)) && length(get_temporal_effects(template)) > 0) {
    #Get the temporal effects still  here
    temporal_effects <- intersect(get_temporal_effects(template), names(data))

    #Reattach
    attr(data, "temporal_effects") <- temporal_effects
  }


  # Re-infer now if rows changed----
  original_now <- get_now(template)
  now <- tryCatch(
    infer_now(
      data,
      now = original_now,
      event_date = attrs$event_date,
      report_date = attrs$report_date
    ),
    error = function(e) attrs$now
  )
  attr(data, "now") <- now
  if (!identical(original_now, now)){
    cli::cli_warn("Changed `now` from {.val {original_now}} to {.val {now}}")
  }

  # Restore class
  class(data) <- class(template)

  return(data)

}

#' Check if an object is a tbl_now
#'
#' @param x any R object
#'
#' @return (boolean) `TRUE` if object is a `tbl_now`
#' `FALSE` if not.
#'
#'
#' @examples
#' is_tbl_now(data.frame(x = 1:3))
#'
#' xval <- data.frame(x = 1:3)
#' class(xval) <- c("tbl_now", "data.frame")
#' is_tbl_now(xval)
#' @export
is_tbl_now <- function(x){

  inherits(x, "tbl_now") && tbl_now_can_reconstruct(x)

}

#' Subset function for `tbl_now` with downgrade-on-subsetting
#'
#' @description IF the subsetting invalidates the class then a `data.frame`
#' will be returned.
#'
#' @param x A `tbl_now` object
#' @inheritParams base::subset
#'
#' @return A `tbl_now` object or a `data.frame`
#' @name assign_tbl
#' @export

#' @rdname assign_tbl
#' @export
`[.tbl_now` <- function(x, ...) {
  out <- NextMethod()
  tbl_now_reconstruct(out, x)
}

#' @rdname assign_tbl
#' @export
`[.grouped_tbl_now` <- function(x, ...) {
  out <- NextMethod()
  tbl_now_reconstruct(out, x)
}

#' Set names on `tbl_now` class
#'
#' @description If the modifying the names invalidates the `tbl_now` object
#' the subsetting will return a data frame with the other attributes of the
#' class preserved.
#'
#' @inheritParams base::names
#'
#' @return A `tbl_now` object or a `data.frame`
#' @name names_tbl_now
#' @export

#' @rdname names_tbl_now
#' @export
`names<-.tbl_now` <- function(x, value) {
  out <- NextMethod()
  tbl_now_reconstruct(out, x)
}

#' @rdname names_tbl_now
#' @export
`names<-.grouped_tbl_now` <- function(x, value) {
  out <- NextMethod()
  tbl_now_reconstruct(out, x)
}

#' Set accessor for `tbl_now` class
#'
#' @param x A `tbl_now` object
#' @inheritParams base::Extract
#'
#' @return A `tbl_now` object or a `data.frame`
#' @name money_tbl_now
#' @export

#' @rdname money_tbl_now
#' @export
`$<-.tbl_now` <- function(x, name, value) {
  out <- NextMethod()
  tbl_now_reconstruct(out, x)
}

#' @rdname money_tbl_now
#' @export
`$<-.grouped_tbl_now` <- function(x, name, value) {
  out <- NextMethod()
  tbl_now_reconstruct(out, x)
}

#' @importFrom dplyr dplyr_row_slice
#' @exportS3Method dplyr::dplyr_row_slice
dplyr_row_slice.tbl_now <- function(data, i, ...) {
  out <- NextMethod()
  dplyr_reconstruct(out, data)
}

#' @importFrom dplyr dplyr_col_modify
#' @exportS3Method dplyr::dplyr_col_modify
dplyr_col_modify.tbl_now <- function(data, cols) {
  out <- NextMethod()
  dplyr_reconstruct(out, data)
}

#' @importFrom dplyr dplyr_reconstruct
#' @exportS3Method dplyr::dplyr_reconstruct
dplyr_reconstruct.tbl_now <- function(data, template) {
  tbl_now_reconstruct(data, template)
}

#' @importFrom dplyr group_by
#' @exportS3Method dplyr::group_by
group_by.tbl_now <- function(.data, ..., .add = FALSE, drop = dplyr::group_by_drop_default(.data)) {

  grouped_tbl <- NextMethod()

  if (dplyr::is.grouped_df(grouped_tbl)) {
    # Extract grouping information from default method
    grouping_structure <- dplyr::group_data(grouped_tbl)
    # Restore original attributes and add grouping information
    x <- new_grouped_tbl_now(.data, groups = grouping_structure)
  } else {
    # This is an edge case if no groups are actually provided. Then simply return a regular subclass
    x <- tbl_now(data = .data,
                 event_date = get_event_date(.data),
                 report_date = get_report_date(.data),
                 strata = get_strata(.data),
                 covariates = get_covariates(.data),
                 is_censored = get_is_censored(.data),
                 now = get_now(.data),
                 event_units = get_event_units(.data),
                 report_units = get_event_units(.data),
                 data_type = get_data_type(.data),
                 case_count = get_case_count(.data),
                 verbose = FALSE,
                 force = TRUE,
                 warn_non_uniqueness = FALSE)
  }
  x
}


#' @importFrom dplyr dplyr_row_slice
#' @exportS3Method dplyr::dplyr_row_slice
dplyr_row_slice.grouped_tbl_now  <- function(data, i, ...) {
  out <- NextMethod()
  dplyr_reconstruct(out, data)
}

#' @importFrom dplyr dplyr_col_modify
#' @exportS3Method dplyr::dplyr_col_modify
dplyr_col_modify.grouped_tbl_now  <- function(data, cols) {
  out <- NextMethod()
  dplyr_reconstruct(out, data)
}

#' @importFrom dplyr dplyr_reconstruct
#' @exportS3Method dplyr::dplyr_reconstruct
dplyr_reconstruct.grouped_tbl_now <- function(data, template) {
  # First reconstruct as tbl_now
  reconstructed <- tbl_now_reconstruct(data, template)

  # If reconstruction was successful and template was grouped
  if (is_tbl_now(reconstructed) && dplyr::is_grouped_df(template)) {
    reconstructed <- new_grouped_tbl_now(reconstructed, groups =  dplyr::group_data(template))
  }

  return(reconstructed)
}



# Based on https://www.bio-ai.org/blog/extending-tibbles/
#' Grouped tbl_now
#'
#' @param x A `tbl_now`
#' @param groups Column grops to group_by
#' @keywords internal
new_grouped_tbl_now <- function(x, groups) {
  x <- dplyr::new_grouped_df(x = x, groups = groups, class = c("grouped_tbl_now"))

  #Get name before tbl_df otherwise it gets lost with the methods
  #see blog (https://www.bio-ai.org/blog/extending-tibbles/)
  tbl_df_location <- grep("tbl_df", class(x), fixed = TRUE)
  class(x)        <- append(class(x), "tbl_now", after = tbl_df_location - 1)

  return(x)
}


#' @importFrom dplyr ungroup
#' @exportS3Method dplyr::ungroup
ungroup.grouped_tbl_now <- function(x, ...) {

  # Run default ungrouping.
  tbl <- NextMethod()

  if (dplyr::is_grouped_df(tbl)) {
    # If the tibble is still grouped, we dont need to do anything, as the default dplyr method doesnt call as_tibble
    x <- tbl
  } else {
    # Otherwise the tibble is completely ungrouped and we need to reapply custom attributes, but remove grouping
    # This is most simplest done by simply creating a new tibble subclass
    # This is an edge case if no groups are actually provided. Then simply return a regular subclass
    x <- tbl_now(data = tbl,
                     event_date = get_event_date(x),
                     report_date = get_report_date(x),
                     strata = get_strata(x),
                     covariates = get_covariates(x),
                     is_censored = get_is_censored(x),
                     now = get_now(x),
                     event_units = get_event_units(x),
                     report_units = get_report_units(x),
                     data_type = get_data_type(x),
                     case_count = get_case_count(x),
                     verbose = FALSE,
                     force = TRUE,
                     warn_non_uniqueness = FALSE)
  }
  return(x)
}


#' @importFrom dplyr summarise
#' @exportS3Method dplyr::summarise
summarise.tbl_now <- function(.data, ..., .by = NULL, .groups = NULL) {

 #Remove the tbl_now attribute
class(.data) <- class(.data)[which(!(class(.data) %in% c("grouped_tbl_now","tbl_now")))]

 #Do normal summarise. This is just a hack to avoid error "Can't supply both `.by` and `.groups`."
 if (is.null(.by) & !is.null(.groups)){
   summarised_tbl <- dplyr::summarise(.data, ..., .groups = .groups)
 } else if (!is.null(.by) & is.null(.groups)){
   summarised_tbl <- dplyr::summarise(.data, ..., .by = .by)
 } else if (is.null(.by) & is.null(.groups)) {
   summarised_tbl <- dplyr::summarise(.data, ...)
 } else {
   summarised_tbl <- dplyr::summarise(.data, ..., .by = .by, .groups = .groups)
 }

 result <- tryCatch({
    tbl_now(data = summarised_tbl,
                event_date = get_event_date(.data),
                report_date = get_report_date(.data),
                strata = get_strata(.data),
                covariates = get_covariates(.data),
                is_censored = get_is_censored(.data),
                now = get_now(.data),
                event_units = get_event_units(.data),
                report_units = get_event_units(.data),
                data_type = get_data_type(.data),
                case_count = get_case_count(.data),
                verbose = FALSE,
                force = TRUE,
                warn_non_uniqueness = FALSE)
    },
    error = function(e) {
      cli::cli_warn("Dropping `tbl_now` attributes and converting to `tibble`")
      summarised_tbl
    }
  )

  result
}

#' @importFrom dplyr summarize
#' @exportS3Method dplyr::summarize
summarize.tbl_now <- function(.data, ..., .by = NULL, .groups = NULL) {
  summarise.tbl_now(.data, ..., .by = .by, .groups = .groups)
}

#' @importFrom dplyr summarise
#' @exportS3Method dplyr::summarise
summarise.grouped_tbl_now <- function(.data, ..., .by = NULL, .groups = NULL) {
  summarise.tbl_now(.data, ..., .by = .by, .groups = .groups)
}

#' @importFrom dplyr summarize
#' @exportS3Method dplyr::summarize
summarize.grouped_tbl_now <- function(.data, ..., .by = NULL, .groups = NULL) {
  summarise.tbl_now(.data, ..., .by = .by, .groups = .groups)
}

#' @importFrom dplyr reframe
#' @exportS3Method dplyr::reframe
reframe.tbl_now <- function(.data, ..., .by = NULL) {

  #Remove the tbl_now attribute
  class(.data) <- class(.data)[which(!(class(.data) %in% c("grouped_tbl_now","tbl_now")))]

  #Do normal reframe
  reframed_tbl <- dplyr::reframe(.data, ..., .by = .by)

  result <- tryCatch({
    tbl_now(data = reframed_tbl,
            event_date = get_event_date(.data),
            report_date = get_report_date(.data),
            strata = get_strata(.data),
            covariates = get_covariates(.data),
            is_censored = get_is_censored(.data),
            now = get_now(.data),
            event_units = get_event_units(.data),
            report_units = get_event_units(.data),
            data_type = get_data_type(.data),
            case_count = get_case_count(.data),
            verbose = FALSE,
            force = TRUE,
            warn_non_uniqueness = FALSE)
  },
  error = function(e) {
    cli::cli_warn("Dropping `tbl_now` attributes and converting to `tibble`")
    reframed_tbl
  }
  )

  result
}

#' @importFrom dplyr reframe
#' @exportS3Method dplyr::reframe
reframe.grouped_tbl_now <- function(.data, ..., .by = NULL) {
  reframe.tbl_now(.data, ..., .by = .by)
}

#' @importFrom dplyr rename
#' @exportS3Method dplyr::rename
rename.tbl_now <- function(.data, ...) {

  #This is drawn from dplyr rename source code
  #https://github.com/tidyverse/dplyr/blob/main/R/rename.R
  loc   <- tidyselect::eval_rename(rlang::expr(c(...)), .data)
  .data <- rename_attributes(.data, loc)

  #Use the normal rename
  NextMethod()
}

#' @importFrom dplyr rename_with
#' @exportS3Method dplyr::rename_with
rename_with.tbl_now <- function(.data, .fn, .cols = dplyr::everything(), ...) {

  #Check the cols
  loc        <- tidyselect::eval_select(rlang::enquo(.cols), .data, allow_rename = FALSE)
  names(loc) <- sapply(names(loc), .fn)
  .data      <- rename_attributes(.data, loc)

  NextMethod()


}

#' Function to rename attributes given a `rename` is applied
#'
#' @keywords internal
rename_attributes <- function(.data, loc){

  #Check that .event_num, .report_num, .delay are not renamed
  loc_protected <- which(names(.data) %in% get_protected_generated_cols(.data))
  if (any(loc_protected %in% loc)){
    changed_loc <- names(.data)[loc_protected][which(loc_protected %in% loc)]
    cli::cli_alert_warning(
      "Changing the name of protected columns {.val {changed_loc}} will result in a tibble"
    )
    .data <- dplyr::as_tibble(.data)
  }

  #Check the ones that changed and change in attributes (such as report date)
  protected_given_cols <- get_protected_given_cols(.data)
  loc_changeable       <- which(names(.data) %in% protected_given_cols)
  if (any(loc_changeable %in% loc)){
    changed_names      <- names(.data)[loc_changeable[which(loc_changeable %in% loc)]]
    changed_attributes <- protected_given_cols[which(protected_given_cols %in% changed_names)]
    for (atrb in 1:length(changed_attributes)){
      new_name_pos <- which(names(.data) == changed_attributes[atrb])
      attr(.data, names(changed_attributes)[atrb]) <- names(loc)[which(loc == new_name_pos)]
    }
  }

  #Check the strata
  protected_strata_cols <- get_strata(.data)
  loc_changeable        <- which(names(.data) %in% protected_strata_cols)
  if (any(loc_changeable %in% loc)){
    kept_names <- names(.data)[loc_changeable[which(!(loc_changeable %in% loc))]]
    new_names  <- names(loc)[which(loc %in% loc_changeable)]
    attr(.data, "strata") <- c(kept_names, new_names)
  }

  #Check the covariates
  protected_covariate_cols <- get_covariates(.data)
  loc_changeable           <- which(names(.data) %in% protected_covariate_cols)
  if (any(loc_changeable %in% loc)){
    kept_names <- names(.data)[loc_changeable[which(!(loc_changeable %in% loc))]]
    new_names  <- names(loc)[which(loc %in% loc_changeable)]
    attr(.data, "covariates") <- c(kept_names, new_names)
  }

  return(.data)
}
