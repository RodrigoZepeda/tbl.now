#' Change attributes of a `tbl_now` object
#'
#' @description Functions to modify the attributes of a `tbl_now` object.
#'
#' @details Variable selection can be used with the
#' auxiliary dplyr verbs such as [dplyr::starts_with()], [dplyr::all_of()],
#' and [dplyr::where()]. See [dplyr::select()] for additional info.
#'
#' @param x A `tbl_now` object
#'
#' @inheritParams tbl_now
#'
#' @param ... 	\code{\link[dplyr:dplyr_tidy_select]{<`tidy-select`>}} with the columns
#' for the attribute. In the case of `covariates` and `strata` argument `...` can
#' refer to multiple columns.
#'
#' @return A `tbl_now` object with updated attributes
#'
#' @examples
#' data(denguedat)
#' ndata <- tbl_now(denguedat,
#'                  event_date = "onset_week",
#'                  report_date = "report_week",
#'                  verbose = FALSE)
#'
#' # Change the event_date column to a different date column
#' ndata$new_onset_week <- ndata$onset_week - lubridate::days(1)
#' ndata <- ndata %>% change_event_date(new_onset_week)
#' ndata
#'
#' # Change the report_date column to a different column
#' ndata$new_report_week <- ndata$report_week - lubridate::days(1)
#' ndata <- ndata %>% change_report_date(new_report_week)
#' ndata
#'
#' # Change strata to different strata
#' ndata$age_group <- sample(c("< 18","20-60","60+"), nrow(ndata), replace = TRUE)
#' ndata <- ndata %>% change_strata(gender, age_group)
#' ndata
#'
#' # Remove some strata
#' ndata <- remove_strata(ndata, gender)
#' ndata
#'
#' # Add strata
#' ndata <- add_strata(ndata, dplyr::starts_with("gender"))
#' ndata
#'
#' # Change covariates
#' ndata$temperature <- rnorm(nrow(ndata), 25, 4)
#' ndata$humidity    <- rbeta(nrow(ndata), 0.6, 0.4)
#' ndata <- ndata %>% change_covariates(temperature, humidity)
#' ndata
#'
#' # Remove some covariates
#' ndata <- remove_covariate(ndata, temperature)
#' ndata
#'
#' # Add covariates
#' ndata <- add_covariate(ndata, temperature)
#' ndata
#'
#' # Change now
#' ndata <- change_now(ndata, now = as.Date("2025-01-01"))
#' ndata
#'
#' #Change case count column
#' count_data <- ndata %>%
#'   to_count(to = "count-incidence")
#'
#' count_data %>%
#'   dplyr::mutate(n2 = 1.15*n) %>%
#'   change_case_count(n2)
#'
#' #Change is_batched
#' ndata$is_batched <- F
#' ndata %>%
#'   change_is_batched(is_batched)
#'
#' @name change
#' @seealso [add_temporal_effects()]
NULL

#' @rdname change
#' @export
change_now <- function(x, now = NULL) {

  if (!inherits(x, "tbl_now")) {
    cli::cli_abort("{.arg x} must be a {.code tbl_now} object")
  }

  if (!is.null(now) && (!lubridate::is.Date(now) || length(now) != 1)){
    cli::cli_abort("{.arg now} must be a Date of length 1")
  }

  # Re-infer now
  now <- tryCatch(
    infer_now(x, now = now, event_date = get_event_date(x), report_date =  get_report_date(x)),
    error = function(e) get_now(x)
  )
  attr(x, "now") <-  now


  validate_tbl_now(x)

  return(x)
}

#' @rdname change
#' @export
#Change the `event_date` to a different column name
change_event_date <- function(x, event_date) {

  #Get the event date
  value_pos <- tidyselect::eval_select(rlang::expr({{ event_date }}), x)
  value     <- colnames(x)[value_pos]

  if (!inherits(x, "tbl_now")) {
    cli::cli_abort("{.arg x} must be a {.code tbl_now} object")
  }

  if (length(value) != 1) {
    cli::cli_abort("{.arg event_date} must be the name of one column (length 1)")
  }

  if (!lubridate::is.Date(x[[value]])) {
    cli::cli_abort("Column {.val {value}} must be of class Date")
  }

  attr(x, "event_date") <-  value

  # Re-infer now if needed
  now <- tryCatch(
    infer_now(x, now = get_now(x), event_date = value, report_date = get_report_date(x)),
    error = function(e) get_now(x)
  )
  attr(x, "now") <-  now

  validate_tbl_now(x)

  return(x)
}

#' @rdname change
#' @export
#Change the `report_date` to a different column name
change_report_date <- function(x, report_date) {

  #Get the report date
  value_pos <- tidyselect::eval_select(rlang::expr({{ report_date }}), x)
  value     <- colnames(x)[value_pos]


  if (!inherits(x, "tbl_now")) {
    cli::cli_abort("{.arg x} must be a {.code tbl_now} object")
  }

  if (length(value) != 1) {
    cli::cli_abort("{.arg report_date} must be the name of one column (length 1)")
  }

  if (!value %in% colnames(x)) {
    cli::cli_abort("Column {.val {value}} not found in data")
  }

  if (!lubridate::is.Date(x[[value]])) {
    cli::cli_abort("Column {.val {value}} must be of class Date")
  }

  attr(x, "report_date") <-  value

  # Re-infer now if needed
  now <- tryCatch(
    infer_now(x, now = get_now(x), event_date = get_event_date(x), report_date = value),
    error = function(e) get_now(x)
  )
  attr(x, "now") <-  now

  validate_tbl_now(x)

  return(x)
}

#' @rdname change
#' @export
#Change the `case_count` to a different column name
change_case_count <- function(x, case_count) {

  #Get the event date
  value_pos <- tidyselect::eval_select(rlang::expr({{ case_count }}), x)

  if (length(value_pos) == 0){
    value <- NULL
  } else {
    value <- colnames(x)[value_pos]
  }

  if (!inherits(x, "tbl_now")) {
    cli::cli_abort("{.arg x} must be a {.code tbl_now} object")
  }

  if (length(value) > 1) {
    cli::cli_abort("{.arg case_count} must be the name of one column (length 1)")
  }

  if (!is.null(value) && !is.numeric(x[[value]])) {
    cli::cli_abort("Column {.val {value}} must be numeric")
  }

  attr(x, "case_count") <-  value

  validate_tbl_now(x)

  return(x)
}

#' @rdname change
#' @export
#Change the `is_batched` to a different column name
change_is_batched <- function(x, is_batched) {

  #Get the event date
  value_pos <- tidyselect::eval_select(rlang::expr({{ is_batched }}), x)

  if (length(value_pos) == 0){
    value <- NULL
  } else {
    value <- colnames(x)[value_pos]
  }

  if (!inherits(x, "tbl_now")) {
    cli::cli_abort("{.arg x} must be a {.code tbl_now} object")
  }

  if (length(value) > 1) {
    cli::cli_abort("{.arg is_batched} must be the name of one column (length 1)")
  }

  if (!is.null(value) && !rlang::is_logical(x[[value]])) {
    cli::cli_abort("Column {.val {value}} must be logical")
  }

  attr(x, "is_batched") <-  value

  validate_tbl_now(x)

  return(x)
}

#' @rdname change
#' @export
# Change all of the strata to whatever is added in ...
change_strata <- function(x, ...) {

  if (!inherits(x, "tbl_now")) {
    cli::cli_abort("{.arg x} must be a {.code tbl_now} object")
  }

  #Get the values
  value_pos <- tidyselect::eval_select(rlang::expr(c(...)), x)

  if (length(value_pos) == 0){
    value <- NULL
  } else {
    value <- colnames(x)[value_pos]
  }

  #Assign the values
  attr(x, "strata") <-  value

  #Verify it works
  validate_tbl_now(x)

  return(x)

}

#' @rdname change
#' @export
# Remove `value`  from strata
remove_strata <- function(x, ...){

  #Get the values
  value_pos <- tidyselect::eval_select(rlang::expr(c(...)), x)
  value     <- colnames(x)[value_pos]

  #Get the strata that is not value
  strata_to_keep <- get_strata(x)
  strata_to_keep <- strata_to_keep[which(!(strata_to_keep %in% c(value)))]

  change_strata(x, strata_to_keep)
}

#' @rdname change
#' @export
# Adds `value`  to existing strata
add_strata <- function(x, ...) {

  value <- tidyselect::eval_select(rlang::expr(c(...)), x)

  #Add to strata
  change_strata(x, c(value, get_strata(x)))

}


#' @rdname change
#' @export
# Remove all `values`  from strata
remove_all_strata <- function(x) {
  change_strata(x, NULL)
}


#' @rdname change
#' @export
# Change the existing covariates to the ones in value
change_covariates <- function(x, ...) {

  if (!inherits(x, "tbl_now")) {
    cli::cli_abort("{.arg x} must be a {.code tbl_now} object")
  }

  #Get the values
  value_pos <- tidyselect::eval_select(rlang::expr(c(...)), x)

  if (length(value_pos) == 0){
    value <- NULL
  } else {
    value <- colnames(x)[value_pos]
  }

  #Assign the values
  attr(x, "covariates") <-  value

  validate_tbl_now(x)

  return(x)
}

#' @rdname change
#' @export
# Removes the specific covariate `value`
remove_covariate <- function(x, ...) {

  #Get the values
  value_pos <- tidyselect::eval_select(rlang::expr(c(...)), x)
  value     <- colnames(x)[value_pos]

  #Get the strata that is not value
  covariates_to_keep <- get_covariates(x)
  covariates_to_keep <- covariates_to_keep[which(!(covariates_to_keep %in% c(value)))]

  change_covariates(x, covariates_to_keep)
}

#' @rdname change
#' @export
# Adds `value`  to existing covariates
add_covariate <- function(x, ...) {

  #Pass value to covariate
  value <- tidyselect::eval_select(rlang::expr(c(...)), x)

  #Add to strata
  change_covariates(x, c(value, get_covariates(x)))

}


#' @rdname change
#' @export
# Removes all covariates
remove_all_covariates <- function(x) {
  change_covariates(x, NULL)
}



#' @rdname change
#' @export
replace_temporal_effects <- function(x, t_effects) {

  if (!inherits(x, "tbl_now")) {
    cli::cli_abort("{.arg x} must be a {.code tbl_now} object")
  }

  if (!is.null(t_effects) && S7::S7_inherits(t_effects, temporal_effects)) {
    cli::cli_abort("{.arg t_effects} must be {.val NULL} or a `temporal_effects()`")
  }

  #Get the columns to remove
  cols_to_remove <- get_temporal_effects(x)
  x <- x %>% dplyr::select(-dplyr::one_of(cols_to_remove))
  attr(x, "temporal_effects") <- NULL

  if (!is.null(t_effects)){
    x <- add_temporal_effects(x, t_effects = TRUE, overwrite = TRUE)
  }

  x
}

#' @rdname change
#' @export
remove_temporal_effects <- function(x) {

  #Get the strata that is not value
  replace_temporal_effects(x, t_effects = NULL)
}

