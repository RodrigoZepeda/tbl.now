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
#' @return Returns `TRUE` invisibly or throws an error. Called for its side effects.
#'
#' @examples
#' \dontrun{
#' data(denguedat)
#' ndata <- tbl_now(denguedat, event_date = "onset_week",
#'   report_date = "report_week")
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
validate_tbl_now <- function(x) {

  #Get required attributes
  required_attrs <- c("event_date", "report_date", "num_strata",
                      "num_covariates", "now", "event_units", "report_units",
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
  num_strata     <- get_num_strata(x)
  strata         <- get_strata(x)
  num_covariates <- get_num_covariates(x)
  covariates     <- get_covariates(x)
  now            <- get_now(x)
  report_units   <- get_report_units(x)
  event_units    <- get_event_units(x)
  data_type      <- get_data_type(x)
  is_batched     <- get_is_batched(x)

  # === 4. Validate attribute types ===

  # event_date and report_date must be character(1)
  if (!is.character(event_date) || length(event_date) != 1) {
    errors <- c(errors, "Attribute {.val event_date} must be a Date of length 1")
  }

  if (!is.character(report_date) || length(report_date) != 1) {
    errors <- c(errors, "Attribute {.val report_date} must be a Date of length 1")
  }

  # num_strata must be numeric
  if (!is.numeric(num_strata) || length(num_strata) != 1) {
    errors <- c(errors, "Attribute {.val num_strata} must be a single numeric value")
  }

  # num_covariates must be numeric
  if (!is.numeric(num_covariates) || length(num_covariates) != 1) {
    errors <- c(errors, "Attribute {.val num_covariates} must be a single numeric value")
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
  if (!lubridate::is.Date(now) || length(now) != 1) {
    errors <- c(errors, "Attribute {.val now}  must be a Date object of length 1")
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
  valid_data_types <- c("auto", "linelist", "count")
  if (!is.character(data_type) || length(data_type) != 1 ||
      !data_type %in% valid_data_types) {
    errors <- c(errors, sprintf(
      "Attribute {.val data_type} must be one of: {.val %s}",
      paste(valid_data_types, collapse = ", ")
    ))
  }

  # is_batched must be NULL or character(1)
  if (!is.null(is_batched) && (!is.character(is_batched) || length(is_batched) != 1)) {
    errors <- c(errors, "Attribute {.val is_batched} must be {.val NULL} or a character vector of length 1")
  }

  # === 5. Validate columns exist in data ===
  if (!is.null(event_date) && !event_date %in% colnames(x)) {
    errors <- c(errors, sprintf("Column {.val %s} (event_date) not found in data", event_date))
  }

  if (!is.null(report_date) && !report_date %in% colnames(x)) {
    errors <- c(errors, sprintf("Column {.val %s} (report_date) not found in data", report_date))
  }

  if (!is.null(is_batched) && !is_batched %in% colnames(x)) {
    errors <- c(errors, sprintf("Column {.val %s} (is_batched) not found in data", is_batched))
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

  # === 6. Validate column types ===
  if (!is.null(event_date) && event_date %in% colnames(x)) {
    if (!lubridate::is.Date(x[[event_date]])) {
      errors <- c(errors, sprintf("Column '%s' must be of class Date", event_date))
    }
  }

  if (!is.null(report_date) && report_date %in% colnames(x)) {
    if (!lubridate::is.Date(x[[report_date]])) {
      errors <- c(errors, sprintf("Column '%s' must be of class Date", report_date))
    }
  }

  if (!is.null(is_batched) && is_batched %in% colnames(x)) {
    if (!is.logical(x[[is_batched]])) {
      errors <- c(errors, sprintf("Column '%s' must be logical (TRUE/FALSE)", is_batched))
    }
  }

  # === 7. Validate consistency between attributes ===

  # Check num_strata matches length of strata
  if (!is.null(strata) && num_strata != length(strata)) {
    errors <- c(errors, sprintf(
      "Attribute 'num_strata' (%d) does not match length of 'strata' (%d)",
      num_strata, length(strata)
    ))
  }

  # Check num_covariates matches length of covariates
  if (!is.null(covariates) && num_covariates != length(covariates)) {
    errors <- c(errors, sprintf(
      "Attribute 'num_covariates' (%d) does not match length of 'covariates' (%d)",
      num_covariates, length(covariates)
    ))
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
    max_report <- max(x[[report_date]], na.rm = TRUE)
    if (!is.na(max_report) && !is.null(now) && now < max_report) {
      warnings <- c(warnings, sprintf(
        "Attribute 'now' (%s) seems to be in the past (before maximum report_date (%s))",
        as.character(now), as.character(max_report)
      ))
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
  keep_attrs <- attrs[setdiff(names(attrs), c("names", "row.names", "class"))]

  # Enforce protected columns
  protected_cols <- c(attrs[["event_date"]], attrs[["report_date"]], ".event_num", ".report_num")
  if (!is.null(attrs[["is_batched"]])){
    protected_cols <- c(protected_cols, attrs[["is_batched"]])
  }

  if (!is.null(get_data_type(data)) && get_data_type(data) == "count"){
    protected_cols <- c(protected_cols, "n")
  }

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
  if (!is.null(attrs[["num_strata"]]) && attrs[["num_strata"]] > 0) {
    #Get the strata still  here
    strata <- intersect(attrs[["strata"]], names(data))

    #Reattach
    attr(data, "strata")      <- strata
    attr(data, "num_strata")  <- length(strata)
  }

  # Update covariates if columns were dropped
  if (!is.null(attrs[["num_covariates"]]) && attrs[["num_covariates"]] > 0) {
    #Get the strata still  here
    covariates <- intersect(attrs[["covariates"]], names(data))

    #Reattach
    attr(data, "covariates")      <- covariates
    attr(data, "num_covariates")  <- length(covariates)
  }


  # Re-infer now if rows changed----
  original_now <- attrs[["now"]]
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
  data <- dplyr::as_tibble(data)
  class(data) <- c("tbl_now", class(data))

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
#' @export
`[.tbl_now` <- function(x, ...) {
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
#' @export
`names<-.tbl_now` <- function(x, value) {
  out <- NextMethod()
  tbl_now_reconstruct(out, x)
}

#' Set accessor for `tbl_now` class
#'
#' @param x A `tbl_now` object
#' @inheritParams base::Extract
#'
#' @return A `tbl_now` object or a `data.frame`
#' @export
`$<-.tbl_now` <- function(x, name, value) {
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

#' @importFrom dplyr dplyr_reconstruct
#' @exportS3Method dplyr::dplyr_reconstruct
dplyr_reconstruct.grouped_tbl_now <- function(data, template) {

  # First reconstruct as tbl_now
  reconstructed <- tbl_now_reconstruct(data, template)

  # If reconstruction was successful and template was grouped
  if (is_tbl_now(reconstructed) && dplyr::is_grouped_df(template)) {
    # Get the grouping structure from template
    grouping_structure <- dplyr::group_data(template)

    # Recreate the grouped_tbl_now
    reconstructed <- new_grouped_tbl_now(reconstructed, groups = grouping_structure)
  }

  reconstructed
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

# Based on https://www.bio-ai.org/blog/extending-tibbles/
#' Grouped tbl_now
#'
#' @param x A `tbl_now`
#' @param groups Column grops to group_by
#' @keywords internal
new_grouped_tbl_now <- function(x, groups) {
  x <- dplyr::new_grouped_df(x = x, groups = groups, class = c("grouped_tbl_now"))
  tbl_df_location <- grep("tbl_df", class(x), fixed = TRUE)
  class(x) <- append(class(x), "tbl_now", after = tbl_df_location - 1)
  x
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
    x <- new_tbl_now(data = .data,
                     event_date = get_event_date(.data),
                     report_date = get_report_date(.data),
                     strata = get_strata(.data),
                     covariates = get_covariates(.data),
                     is_batched = get_is_batched(.data),
                     now = get_now(.data),
                     event_units = get_event_units(.data),
                     report_units = get_event_units(.data),
                     data_type = get_data_type(.data),
                     verbose = FALSE,
                     force = TRUE)
  }
  x
}

#' @importFrom dplyr ungroup
#' @exportS3Method dplyr::ungroup
ungroup.grouped_tbl_now <- function(x, ...) {

  event_date   <- get_event_date(x)
  report_date  <- get_report_date(x)
  strata       <- get_strata(x)
  covariates   <- get_covariates(x)
  is_batched   <- get_is_batched(x)
  now          <- get_now(x)
  event_units  <- get_event_units(x)
  report_units <- get_event_units(x)
  data_type    <- get_data_type(x)

  # Run default ungrouping.
  tbl <- NextMethod()

  if (dplyr::is_grouped_df(tbl)) {
    # If the tibble is still grouped, we dont need to do anything, as the default dplyr method doesnt call as_tibble
    x <- tbl
  } else {
    # Otherwise the tibble is completely ungrouped and we need to reapply custom attributes, but remove grouping
    # This is most simplest done by simply creating a new tibble subclass
    # This is an edge case if no groups are actually provided. Then simply return a regular subclass
    x <- new_tbl_now(data = tbl,
                     event_date = event_date,
                     report_date = report_date,
                     strata = strata,
                     covariates = covariates,
                     is_batched = is_batched,
                     now = now,
                     event_units = event_units,
                     report_units = report_units,
                     data_type = data_type,
                     verbose = FALSE,
                     force = TRUE)
  }
  x
}


#' @importFrom dplyr summarise
#' @exportS3Method dplyr::summarise
summarise.tbl_now <- function(.data, ..., .by = NULL, .groups = NULL) {

 #Remove the tbl_now attribute
class(.data) <- class(.data)[which(!(class(.data) %in% c("grouped_tbl_now","tbl_now")))]

 #Do normal summarise
 summarised_tbl <- dplyr::summarise(.data, ..., .groups = .groups)

 result <- tryCatch({
    new_tbl_now(data = summarised_tbl,
                event_date = get_event_date(.data),
                report_date = get_report_date(.data),
                strata = get_strata(.data),
                covariates = get_covariates(.data),
                is_batched = get_is_batched(.data),
                now = get_now(.data),
                event_units = get_event_units(.data),
                report_units = get_event_units(.data),
                data_type = get_data_type(.data),
                verbose = FALSE,
                force = TRUE)
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
  summarise.tbl_now(.data, ..., .groups = .groups)
}

#' @importFrom dplyr summarise
#' @exportS3Method dplyr::summarise
summarise.grouped_tbl_now <- function(.data, ..., .by = NULL, .groups = NULL) {
  summarise.tbl_now(.data, ..., .groups = .groups)
}

#' @importFrom dplyr summarize
#' @exportS3Method dplyr::summarize
summarize.grouped_tbl_now <- function(.data, ..., .by = NULL, .groups = NULL) {
  summarise.tbl_now(.data, ..., .groups = .groups)
}
