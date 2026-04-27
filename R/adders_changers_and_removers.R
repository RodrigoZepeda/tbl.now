#' Change attributes of a `tbl_now` object
#'
#' @description Functions to change the attributes of a `tbl_now` object.
#'
#' @details Variable selection is done via \code{\link[dplyr:dplyr_tidy_select]{<`tidy-select`>}}
#' and can be used with the auxiliary dplyr verbs such as [dplyr::starts_with()], [dplyr::all_of()],
#' and [dplyr::where()]. See [dplyr::select()] for additional info.
#'
#' @inheritParams remove
#' @inheritParams tbl_now
#' @inheritParams validate_tbl_now
#'
#' @return A `tbl_now` object with updated attributes
#'
#' @examples
#' data(denguedat)
#' ndata <- tbl_now(denguedat,
#'                  event_date = onset_week,
#'                  report_date = report_week,
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
#' # Change covariates
#' ndata$temperature <- rnorm(nrow(ndata), 25, 4)
#' ndata$humidity    <- rbeta(nrow(ndata), 0.6, 0.4)
#' ndata <- ndata %>% change_covariates(temperature, humidity)
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
#' #Change is_censored
#' ndata$is_censored <- FALSE
#' ndata <- ndata %>%
#'   change_is_censored(is_censored)
#' ndata
#'
#' # Change covariates
#' ndata$temperature <- rnorm(nrow(ndata), 25, 4)
#' ndata$humidity    <- rbeta(nrow(ndata), 0.6, 0.4)
#' ndata <- ndata %>% change_covariates(temperature, humidity)
#' ndata
#'
#' # Add temporal effects, remove and replace them
#' ndata <- ndata %>%
#'     add_temporal_effects(disease_data,
#'     t_effects= temporal_effects(week_of_year = TRUE, month_of_year = TRUE))
#'
#' #Use the compute to calculate them
#' ndata %>% compute_temporal_effects()
#'
#' #Use replace to change them
#' ndata %>% replace_temporal_effects(t_effects= temporal_effects(seasons = 52))
#'
#' #Use remove to delete them
#' ndata %>% remove_temporal_effects()
#'
#' @md
#' @name change
#' @seealso [add_temporal_effects()] [add] [remove]
NULL

#' Add attributes to a `tbl_now` object
#'
#' @description Functions to add the attributes to a `tbl_now` object.
#'
#' @details Variable selection can be used with the
#' auxiliary dplyr verbs such as [dplyr::starts_with()], [dplyr::all_of()],
#' and [dplyr::where()]. See [dplyr::select()] for additional info.
#'
#' @inheritParams change
#'
#' @return A `tbl_now` object with updated attributes
#'
#' @examples
#' data(denguedat)
#' ndata <- tbl_now(denguedat,
#'                  event_date = onset_week,
#'                  report_date = report_week,
#'                  verbose = FALSE)
#'
#' # Add strata
#' ndata <- ndata %>% add_strata(dplyr::starts_with("gender"))
#' ndata
#'
#' # Add covariates
#' ndata$temperature <- rnorm(nrow(ndata), 25, 4)
#' ndata$humidity    <- rbeta(nrow(ndata), 0.6, 0.4)
#' ndata <- ndata %>% add_covariates(temperature, humidity)
#' ndata
#'
#' @md
#' @name add
#' @seealso [add_temporal_effects()] [change] [remove]
NULL

#' Remove attributes from a `tbl_now` object
#'
#' @description Functions to remove the attributes to a `tbl_now` object.
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
#'                  event_date = onset_week,
#'                  report_date = report_week,
#'                  strata = gender,
#'                  verbose = FALSE)
#'
#' # Add strata
#' ndata <- remove_strata(ndata, gender)
#' ndata
#'
#' # Change covariates
#' ndata$temperature <- rnorm(nrow(ndata), 25, 4)
#' ndata$humidity    <- rbeta(nrow(ndata), 0.6, 0.4)
#' ndata <- ndata %>% add_covariates(temperature, humidity)
#' ndata
#'
#' ndata %>% remove_covariates(temperature, humidity)
#'
#' @name remove
#' @seealso [add_temporal_effects()] [add] [change]
#' @md
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

  #Recalculate the delay when changing event date
  x <- time_cols_to_numeric(x, event_date = value,
                            report_date = get_report_date(x),
                            report_units = get_report_units(x),
                            event_units = get_event_units(x),
                            force = TRUE)

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

  #Recalculate the delay when changing report date
  x <- time_cols_to_numeric(x, event_date = get_event_date(x),
                            report_date = value,
                            report_units = get_report_units(x),
                            event_units = get_event_units(x),
                            force = TRUE)


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
#Change the `is_censored` to a different column name
change_is_censored <- function(x, is_censored) {

  #Get the event date
  value_pos <- tidyselect::eval_select(rlang::expr({{ is_censored }}), x)

  if (length(value_pos) == 0){
    value <- NULL
  } else {
    value <- colnames(x)[value_pos]
  }

  if (!inherits(x, "tbl_now")) {
    cli::cli_abort("{.arg x} must be a {.code tbl_now} object")
  }

  if (length(value) > 1) {
    cli::cli_abort("{.arg is_censored} must be the name of one column (length 1)")
  }

  if (!is.null(value) && !rlang::is_logical(x[[value]])) {
    cli::cli_abort("Column {.val {value}} must be logical")
  }

  attr(x, "is_censored") <-  value

  validate_tbl_now(x)

  return(x)
}

#' @rdname remove
#' @export
# Remove `value`  from is_censored
remove_is_censored <- function(x){
  if (get_data_type(x) == "count-cumulative"){
    cli::cli_alert_warning(
      "Removing is_censored column from count-cumulative data might have unintended consequences. We suggest manually aggregating the data and then calling `tbl_now`"
    )
  }
  change_is_censored(x, NULL)
}

#' @rdname add
#' @export
# Adds `value`  to existing strata
add_is_censored <- function(x, is_censored) {

  if (length(get_is_censored(x)) > 0){
    cli::cli_abort(
      paste0(
        "Already has value {.val {get_is_censored(x)}} as censored indicator.",
        " Use {.help remove_is_censored} to remove it before adding or",
        " {.help change_is_censored} to change it."
      )
    )
  }

  #Add to censored
  change_is_censored(x, {{ is_censored }})

}

#' @rdname change
#' @export
# Change all of the strata to whatever is added in ...
change_strata <- function(x, ..., warn_now = TRUE, warn_non_uniqueness = TRUE) {

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
  validate_tbl_now(x, warn_now = warn_now, warn_non_uniqueness = warn_non_uniqueness)

  return(x)

}

#' @rdname remove
#' @export
# Remove `value`  from strata
remove_strata <- function(x, ...){
  if (get_data_type(x) == "count-cumulative"){
    cli::cli_alert_warning(
      "Removing strata from count-cumulative data might have unintended consequences. We suggest manually aggregating the data and then calling `tbl_now`"
    )
  }
  #Get the values
  value_pos <- tidyselect::eval_select(rlang::expr(c(...)), x)
  value     <- colnames(x)[value_pos]

  #Get the strata that is not value
  strata_to_keep <- get_strata(x)
  strata_to_keep <- strata_to_keep[which(!(strata_to_keep %in% c(value)))]

  change_strata(x, dplyr::all_of(strata_to_keep))
}

#' @rdname add
#' @export
# Adds `value`  to existing strata
add_strata <- function(x, ...) {

  value_names <- colnames(x)[tidyselect::eval_select(rlang::expr(c(...)), x)]

  #Add to strata — pass as all_of() to avoid external-vector deprecation
  change_strata(x, dplyr::all_of(c(value_names, get_strata(x))))

}


#' @rdname remove
#' @export
# Remove all `values`  from strata
remove_all_strata <- function(x) {
  if (get_data_type(x) == "count-cumulative"){
    cli::cli_alert_warning(
      "Removing strata from count-cumulative data might have unintended consequences. We suggest manually aggregating the data and then calling `tbl_now`"
    )
  }
  change_strata(x, NULL)
}


#' @rdname change
#' @export
# Change the existing covariates to the ones in value
change_covariates <- function(x, ..., warn_now = TRUE, warn_non_uniqueness = TRUE) {

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

  validate_tbl_now(x, warn_now = warn_now, warn_non_uniqueness = warn_non_uniqueness)

  return(x)
}

#' @rdname remove
#' @export
# Removes the specific covariate `value`
remove_covariates <- function(x, ...) {
  if (get_data_type(x) == "count-cumulative"){
    cli::cli_alert_warning(
      "Removing covariates from count-cumulative data might have unintended consequences. We suggest manually aggregating the data and then calling `tbl_now`"
    )
  }
  #Get the values
  value_pos <- tidyselect::eval_select(rlang::expr(c(...)), x)
  value     <- colnames(x)[value_pos]

  #Get the strata that is not value
  covariates_to_keep <- get_covariates(x)
  covariates_to_keep <- covariates_to_keep[which(!(covariates_to_keep %in% c(value)))]

  change_covariates(x, dplyr::all_of(covariates_to_keep))
}

#' @rdname add
#' @export
# Adds `value`  to existing covariates
add_covariates <- function(x, ...) {

  #Pass value to covariate
  value_names <- colnames(x)[tidyselect::eval_select(rlang::expr(c(...)), x)]

  #Add to covariates — pass as all_of() to avoid external-vector deprecation
  change_covariates(x, dplyr::all_of(c(value_names, get_covariates(x))))

}


#' @rdname remove
#' @export
# Removes all covariates
remove_all_covariates <- function(x) {
  if (get_data_type(x) == "count-cumulative"){
    cli::cli_alert_warning(
      "Removing covariates from count-cumulative data might have unintended consequences. We suggest manually aggregating the data and then calling `tbl_now`"
    )
  }
  change_covariates(x, NULL)
}



#' @rdname remove
#' @export
replace_temporal_effects <- function(x, t_effects) {
  if (get_data_type(x) == "count-cumulative"){
    cli::cli_alert_warning(
      "Removing temporal effects from count-cumulative data might have unintended consequences. We suggest manually aggregating the data and then calling `tbl_now`"
    )
  }

  if (!inherits(x, "tbl_now")) {
    cli::cli_abort("{.arg x} must be a {.code tbl_now} object")
  }

  if (!is.null(t_effects) && !S7::S7_inherits(t_effects, temporal_effects)) {
    cli::cli_abort("{.arg t_effects} must be {.val NULL} or a `temporal_effects()`")
  }

  # Drop already-computed columns
  cols_to_remove <- get_temporal_effect_cols(x)
  if (length(cols_to_remove) > 0) {
    x <- x %>% dplyr::select(-dplyr::one_of(cols_to_remove))
  }

  # Reset both spec and computed-cols attributes
  attr(x, "temporal_effects")              <- list()
  attr(x, "computed_temporal_effect_cols") <- character(0)

  if (!is.null(t_effects)){
    x <- add_temporal_effects(x, t_effects = t_effects)
  }

  x
}

#' @rdname remove
#' @export
remove_temporal_effects <- function(x) {
  replace_temporal_effects(x, t_effects = NULL)
}

