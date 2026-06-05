#' Update a `tbl_now`
#'
#' Updates a `tbl_now` object with new observations
#' either from another `tbl_now` or a `data.frame`
#'
#' @param object A `tbl_now` object
#' @param ... Additional arguments to pass to `tbl_now`
#' @param new_data Another `tbl_now` with the same `strata`, `covariates`, `is_censored`,
#' and `temporal_effects` or a `data.frame` with additional (newer) data
#' not present in `x`
#'
#' @inheritParams tbl_now
#'
#' @param strata (optional) Whether to keep the strata from `object` ("left"),
#' from `new_data` ("right") or from `both` ("both")
#' @param covariates (optional) Whether to keep the covariates from `object` ("left"),
#' from `new_data` ("right") or from `both` ("both")
#' @param t_effects (optional) Whether to keep the temporal_effects from `object` ("left"),
#' from `new_data` ("right") or from `both` ("both")
#' @param remove_duplicates Whether to remove duplicated rows from data (only applies for `count` data)
#'
#' @note By default it keeps the strata, covariates and temporal effects of `object`. Use
#' the `strata`, `covariates` and `t_effects` arguments to change it.
#'
#' @return A `tbl_now` object with all the properties of `object`
#'
#' @examples
#' data(denguedat)
#' initial_data <- denguedat[1:500,]
#' update_data  <- denguedat[501:1000,]
#'
#' initial_tbl <- tbl_now(denguedat, event_date = "onset_week",
#'                        report_date = "report_week", strata = "gender",
#'                        verbose = FALSE)
#'
#' #Update collapses everything into a single data.frame
#' update(initial_tbl, new_data = update_data)
#'
#' @export
update.tbl_now <- function(object, ..., new_data,
                           strata = "left", covariates = strata,
                           t_effects = strata,
                           now = NULL,
                           remove_duplicates = NULL){

  if (is.null(remove_duplicates) & grepl("count", get_data_type(object))){
    remove_duplicates <- TRUE
  } else {
    remove_duplicates <- FALSE
  }

  if (!is.data.frame(new_data)){
    cli::cli_abort(
      "`new_data` must be a `data.frame`"
    )
  }

  if (is_tbl_now(new_data)){
    update_check_tbl_now_internal(object, new_data)
  } else {
    update_check_data_frame_internal(object, new_data)
  }

  # Resolve the is_censored column name for the result before binding
  cens_obj <- get_is_censored(object)
  cens_new <- if (is_tbl_now(new_data)) get_is_censored(new_data) else NULL

  #Bind rows
  updated_data <- object %>%
    dplyr::bind_rows(new_data)

  # If object had no censored column but new_data does, the old rows will have
  # NA in that column after bind_rows.  Fill those NAs with 0 (= not censored).
  if (is.null(cens_obj) && !is.null(cens_new) && cens_new %in% colnames(updated_data)) {
    updated_data <- updated_data %>%
      dplyr::mutate(
        !!as.symbol(cens_new) := dplyr::coalesce(!!as.symbol(cens_new), FALSE)
      )
    cli::cli_inform(
      "Rows from {.arg object} had no censored column; setting {.code {cens_new} = FALSE} for those rows."
    )
  }

  # Determine which censored column name to register on the result.
  # Priority: object's column first (it was explicitly registered); fall back
  # to new_data's column when object had none.
  result_is_censored <- if (!is.null(cens_obj)) cens_obj else cens_new

  if (grepl("count", get_data_type(object)) && remove_duplicates){
      suppressWarnings(
        updated_data <- updated_data %>%
          dplyr::distinct(dplyr::pick(-dplyr::one_of(get_protected_generated_cols())), .keep_all = TRUE)
      )


  } else if (!grepl("count", get_data_type(object)) && remove_duplicates){
    cli::cli_warn(
      "Cannot remove duplicates from data_type = {.val {get_data_type(object)}}"
    )
  }


  #Re-do the strata----
  if (strata == "left"){

    #Check if exists
    if (!is.null(get_strata(object)) && !all(get_strata(object) %in% colnames(new_data))){
      not_in_new <- get_strata(object)[which(!(get_strata(object) %in% colnames(new_data)))]
      cli::cli_abort(
        "Strata {.val {not_in_new}} was not present in `new_data`."
      )
    }

    #Update
    updated_data <- updated_data %>% change_strata(get_strata(object), warn_now = FALSE, warn_non_uniqueness = FALSE)
  } else if (strata == "right"){

    #Check if exists
    if (!is.null(get_strata(new_data)) && !all(get_strata(new_data) %in% colnames(object))){
      not_in_new <- get_strata(new_data)[which(!(get_strata(new_data) %in% colnames(object)))]
      cli::cli_abort(
        "Strata {.val {not_in_new}} was not present in `object`."
      )
    }

    #Update
    updated_data <- updated_data %>% change_strata(get_strata(new_data), warn_now = FALSE, warn_non_uniqueness = FALSE)

  } else if (strata == "both"){

    #Check if exists for new_data
    if (!is.null(get_strata(new_data)) && !all(get_strata(new_data) %in% colnames(object))){
      not_in_new <- get_strata(new_data)[which(!(get_strata(new_data) %in% colnames(object)))]
      cli::cli_abort(
        "Strata {.val {not_in_new}} was not present in `object`."
      )
    }

    #Check if exists for object
    if (!is.null(get_strata(object)) && !all(get_strata(object) %in% colnames(new_data))){
      not_in_new <- get_strata(object)[which(!(get_strata(object) %in% colnames(new_data)))]
      cli::cli_abort(
        "Strata {.val {not_in_new}} was not present in `new_data`."
      )
    }

    #Update
    updated_data <- updated_data %>% change_strata(unique(c(get_strata(object), get_strata(new_data))), warn_now = FALSE, warn_non_uniqueness = FALSE)

  } else {
    cli::cli_abort(
      "Option for `strata` should be {.val {c('left','right','both')}}. Don't know how to handle value {.val {strata}}"
    )
  }

  #Re-do the covariates----
  if (covariates == "left"){

    #Check if exists
    if (!is.null(get_covariates(object)) && !all(get_covariates(object) %in% colnames(new_data))){
      not_in_new <- get_covariates(object)[which(!(get_covariates(object) %in% colnames(new_data)))]
      cli::cli_abort(
        "Covariate {.val {not_in_new}} was not present in `new_data`."
      )
    }

    #Update
    updated_data <- updated_data %>% change_covariates(get_covariates(object), warn_now = FALSE, warn_non_uniqueness = FALSE)

  } else if (covariates == "right"){

    #Check if exists
    if (!is.null(get_covariates(new_data)) && !all(get_covariates(new_data) %in% colnames(object))){
      not_in_new <- get_covariates(new_data)[which(!(get_covariates(new_data) %in% colnames(object)))]
      cli::cli_abort(
        "Covariate {.val {not_in_new}} was not present in `object`."
      )
    }

    #Update
    updated_data <- updated_data %>%
      change_covariates(get_covariates(new_data), warn_now = FALSE, warn_non_uniqueness = FALSE)

  } else if (covariates == "both"){

    #Check if exists
    if (!is.null(get_covariates(new_data)) && !all(get_covariates(new_data) %in% colnames(object))){
      not_in_new <- get_covariates(new_data)[which(!(get_covariates(new_data) %in% colnames(object)))]
      cli::cli_abort(
        "Covariate {.val {not_in_new}} was not present in `object`."
      )
    }

    if (!is.null(get_covariates(object)) && !all(get_covariates(object) %in% colnames(new_data))){
      not_in_new <- get_covariates(object)[which(!(get_covariates(object) %in% colnames(new_data)))]
      cli::cli_abort(
        "Covariate {.val {not_in_new}} was not present in `new_data`."
      )
    }

    #Update
    updated_data <- updated_data %>%
      change_covariates(unique(c(get_covariates(object), get_covariates(new_data))), warn_now = FALSE, warn_non_uniqueness = FALSE)

  } else {
    cli::cli_abort(
      "Option for `covariates` should be {.val {c('left','right','both')}}. Don't know how to handle value {.val {covariates}}"
    )
  }

  if (!identical(get_temporal_effects(new_data), get_temporal_effects(object)) && (length(get_temporal_effects(new_data)) > 0 | length(get_temporal_effects(object)) > 0)){
    cli::cli_abort("Cannot handle different temporal_effects")
  }

  result <- tbl_now(updated_data,
          event_date = get_event_date(object),
          report_date = get_report_date(object),
          strata = get_strata(updated_data),
          covariates = get_covariates(updated_data),
          is_censored = result_is_censored,
          event_units = get_event_units(object),
          report_units = get_report_units(object),
          data_type = get_data_type(object),
          case_count = get_case_count(object),
          now = now,
          force = TRUE,
          ...)

  # Preserve the lazy temporal-effects spec from the original object
  attr(result, "temporal_effects") <- get_temporal_effects(object)
  result

}


#' Check update when new_data is a tbl_now
#'
#' Checks that the attributes match between object and new_data
#'
#' @inheritParams update.tbl_now
#' @return TRUE (invisibly). Called for its side effects
#' @keywords internal
update_check_tbl_now_internal <- function(object, new_data){

  if (!inherits(object, "tbl_now") | !inherits(new_data, "tbl_now")){
    cli::cli_abort("`object` and `new_data` must be of class `tbl_now`")
  }

  #Check that both objects have the same event and report columns
  if (get_event_date(object) != get_event_date(new_data)){
    cli::cli_abort(
      paste0(
        "`object` has event_date = {.val {get_event_date(object)}} while ",
        "`new_data` has event_date = {.val {get_event_date(new_data)}}. ",
        "They must be the same in order to `update`."
      )
    )
  }

  #Check that both objects have the same event and report columns
  if (get_data_type(object) != get_data_type(new_data)){
    cli::cli_abort(
      paste0(
        "`object` has data_type = {.val {get_data_type(object)}} while ",
        "`new_data` has data_type = {.val {get_data_type(new_data)}}. ",
        "They must be the same in order to `update`."
      )
    )
  }

  #Check that both objects have the same event and report columns
  if (get_event_units(object) != get_event_units(new_data)){
    cli::cli_abort(
      paste0(
        "`object` has event_units = {.val {get_event_units(object)}} while ",
        "`new_data` has event_units = {.val {get_event_units(new_data)}}. ",
        "They must be the same in order to `update`."
      )
    )
  }

  #Check that both objects have the same event and report columns
  if (get_report_units(object) != get_report_units(new_data)){
    cli::cli_abort(
      paste0(
        "`object` has report_units = {.val {get_report_units(object)}} while ",
        "`new_data` has report_units = {.val {get_report_units(new_data)}}. ",
        "They must be the same in order to `update`."
      )
    )
  }

  #Check that both objects have the same event and report columns
  if (!identical(get_case_count(object), get_case_count(new_data))){
    cli::cli_abort(
      paste0(
        "`object` has case_count = {.val {get_case_count(object)}} while ",
        "`new_data` has case_count = {.val {get_case_count(new_data)}}. ",
        "They must be the same in order to `update`."
      )
    )
  }

  # Censored-column compatibility:
  # • Both have one with the SAME name → OK
  # • Only new_data has one               → OK (old rows will be filled with FALSE)
  # • Only object has one                 → OK (new rows must carry the column;
  #                                              update_check_data_frame_internal handles that)
  # • Both have one with DIFFERENT names  → error
  if (!is.null(get_is_censored(object)) && !is.null(get_is_censored(new_data)) &&
      !identical(get_is_censored(object), get_is_censored(new_data))) {
    cli::cli_abort(
      paste0(
        "`object` has is_censored = {.val {get_is_censored(object)}} while ",
        "`new_data` has is_censored = {.val {get_is_censored(new_data)}}. ",
        "They must be the same column name in order to `update`."
      )
    )
  }

  #Check that both objects have the same event and report columns
  if (get_report_date(object) != get_report_date(new_data)){
    cli::cli_abort(
      paste0(
        "`object` has report_date = {.val {get_report_date(object)}} while ",
        "`new_data` has report_date = {.val {get_report_date(new_data)}}. ",
        "They must be the same in order to `update`."
      )
    )
  }

  return(invisible(TRUE))


}

#' Check update when new_data is a data.frame
#'
#' Checks that the attributes of object are columns in new_data
#'
#' @inheritParams update.tbl_now
#'
#' @return TRUE (invisibly). Called for its side effects
#'
#' @keywords internal
update_check_data_frame_internal <- function(object, new_data){

  if (!is.data.frame(new_data)){
    cli::cli_abort(" `new_data` must be a data.frame")
  }

  if (!inherits(object, "tbl_now")){
    cli::cli_abort("`object` must be a tbl_now")
  }

  #Check that both objects have the same event and report columns
  if (!(get_event_date(object) %in% colnames(new_data))){
    cli::cli_abort(
      paste0(
        "`object` has event_date = {.val {get_event_date(object)}} but ",
        "that column was not found in `new_data`."
      )
    )
  }

  #Check that both objects have the same event and report columns
  if (!(get_report_date(object) %in% colnames(new_data))){
    cli::cli_abort(
      paste0(
        "`object` has report_date = {.val {get_report_date(object)}} but ",
        "that column was not found in `new_data`."
      )
    )
  }

  #Check that both objects have the same event and report columns
  if (grepl("count", get_data_type(object)) & !is.null(get_case_count(object)) && !(get_case_count(object) %in% colnames(new_data))){
    cli::cli_abort(
      paste0(
        "`object` has case_count = {.val {get_case_count(object)}} but ",
        "that column was not found in `new_data`."
      )
    )
  }

  #Check that both objects have the same event and report columns
  if (!is.null(get_is_censored(object)) && !(get_is_censored(object) %in% colnames(new_data))){
    cli::cli_abort(
      paste0(
        "`object` has is_censored = {.val {get_is_censored(object)}} but ",
        "that column was not found in `new_data`."
      )
    )
  }


  return(invisible(TRUE))


}
