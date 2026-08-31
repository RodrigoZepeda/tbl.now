#' Set, change and remove the attributes of a `tbl_now`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' A [tbl_now()] remembers which of its columns are the event date, the report
#' date, the strata, and so on. These functions edit that memory after the object
#' has been built -- useful when new columns appear part-way through a pipeline,
#' or when you want to nowcast the same data broken down a different way.
#'
#' There are three verbs, and they differ only in what they do to what is already
#' recorded:
#'
#' * **`add_*()`** keeps what is there and adds to it. `add_strata(x, age_group)`
#'   on an object already stratified by gender leaves you stratified by both.
#' * **`change_*()`** replaces it. `change_strata(x, age_group)` on the same
#'   object leaves you stratified by age group *only*.
#' * **`remove_*()`** takes it away. `remove_all_strata(x)` forgets every
#'   stratum; `remove_strata(x, gender)` forgets just that one.
#'
#' Nothing here touches the data itself. Renaming an attribute does not rename,
#' create or delete a column -- it only changes which existing column the object
#' treats as playing that role.
#'
#' @details
#' Columns are chosen with
#' [tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html), so
#' a bare column name works, and so do the helpers
#' [dplyr::starts_with()], [dplyr::all_of()] and [dplyr::where()]. See
#' [dplyr::select()] for the full set.
#'
#' `update_now()` deserves a note of its own. The `now` of a nowcast is the day
#' you are standing on, and it does **not** move when you filter the data -- an
#' object filtered down to 1992 still believes `now` is 2010, which is usually
#' not what you want. `update_now()` resets it to the latest date actually
#' present:
#'
#' ```{r}
#' data(denguedat)
#' ndata <- tbl_now(denguedat,
#'   event_date = onset_week, report_date = report_week, verbose = FALSE
#' )
#'
#' # `now` is in 2010, because the data runs that far.
#' get_now(ndata)
#'
#' # Filtering the data does not move it ...
#' ndata_1992 <- ndata |>
#'   dplyr::filter(
#'     onset_week <= as.Date("1992/01/01") & report_week <= as.Date("1992/01/01")
#'   )
#' get_now(ndata_1992)
#'
#' # ... but `update_now()` does.
#' get_now(update_now(ndata_1992))
#' ```
#'
#' @param x A `tbl_now` object.
#'
#' @param ... [tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
#' columns for the attribute being set. For `strata` and `covariates` this may
#' name several columns at once.
#'
#' @inheritParams tbl_now
#' @inheritParams validate_tbl_now
#'
#' @return A `tbl_now` object with the attribute updated. The data are returned
#' unchanged; only what the object records about itself differs.
#'
#' @examples
#' data(denguedat)
#' ndata <- tbl_now(denguedat,
#'   event_date = onset_week,
#'   report_date = report_week,
#'   strata = gender,
#'   verbose = FALSE
#' )
#'
#' ## ---- Strata: add, change, remove ------------------------------------
#'
#' ndata$age_group <- sample(c("<18", "18-60", "60+"), nrow(ndata), replace = TRUE)
#'
#' ## `add_strata()` keeps gender and adds age group.
#' get_strata(add_strata(ndata, age_group))
#'
#' ## `change_strata()` replaces gender with age group.
#' get_strata(change_strata(ndata, age_group))
#'
#' ## `remove_strata()` drops one; `remove_all_strata()` drops the lot.
#' get_strata(remove_strata(ndata, gender))
#' get_strata(remove_all_strata(add_strata(ndata, age_group)))
#'
#' ## ---- Covariates behave the same way ---------------------------------
#'
#' # Covariates influence the nowcast but are not of interest in themselves.
#' ndata$temperature <- rnorm(nrow(ndata), 25, 4)
#' ndata$humidity <- rbeta(nrow(ndata), 0.6, 0.4)
#' ndata <- ndata |> add_covariates(temperature, humidity)
#' get_covariates(ndata)
#'
#' ndata |>
#'   remove_covariates(humidity) |>
#'   get_covariates()
#'
#' ## ---- Pointing an attribute at a different column ---------------------
#'
#' ## Suppose onset was recorded a day late and you correct it. `change_event_date()`
#' # tells the object to use the corrected column instead.
#' ndata$corrected_onset <- ndata$onset_week - lubridate::days(1)
#' ndata <- ndata |> change_event_date(corrected_onset)
#' get_event_date(ndata)
#'
#' ## ---- The censoring indicator -----------------------------------------
#'
#' ## TRUE means the report date is only an upper bound (e.g. a backlog dump).
#' ndata$is_censored <- FALSE
#' ndata <- ndata |> add_is_censored(is_censored)
#' get_is_censored(ndata)
#' ndata <- remove_is_censored(ndata)
#'
#' ## ---- `now` -----------------------------------------------------------
#'
#' # Set it by hand ...
#' get_now(change_now(ndata, now = as.Date("2011-01-01")))
#'
#' # ... or snap it back to the latest date actually observed.
#' get_now(update_now(ndata))
#'
#' ## ---- Count data: which column holds the counts ------------------------
#'
#' counts <- to_count(ndata, to = "count-incidence")
#' counts |>
#'   dplyr::mutate(inflated = round(1.15 * n)) |>
#'   change_case_count(inflated) |>
#'   get_case_count()
#'
#' ## ---- Temporal effects --------------------------------------------------
#'
#' # Recorded lazily: `replace_*` swaps the specification, `remove_*` forgets it.
#' ndata <- ndata |>
#'   add_temporal_effects(
#'     t_effects = temporal_effects(week_of_year = TRUE, month_of_year = TRUE)
#'   )
#' get_temporal_effects(ndata)
#'
#' ndata |>
#'   replace_temporal_effects(t_effects = temporal_effects(seasons = 52)) |>
#'   get_temporal_effects()
#'
#' ndata |>
#'   remove_temporal_effects() |>
#'   get_temporal_effects()
#'
#' @seealso
#' [tbl_now()] for setting these when the object is first built;
#' the [getters][nowcast_data_getters] for reading them back;
#' [tbl_now_attributes()] to list them all at once;
#' [add_confirmation()][confirmation_setters] for the third-date attributes;
#' [add_temporal_effects()] and [temporal_effects()] for calendar structure;
#' [update()][update.tbl_now] for appending new rows rather than editing attributes.
#'
#' @name add
#' @aliases change remove
NULL

#' @rdname add
#' @export
change_now <- function(x, now = NULL) {
  if (!inherits(x, "tbl_now")) {
    cli::cli_abort("{.arg x} must be a {.code tbl_now} object")
  }

  if (!is.null(now) && (!lubridate::is.Date(now) || length(now) != 1)) {
    cli::cli_abort("{.arg now} must be a Date of length 1")
  }

  # Re-infer now. The CONFIRMATION date counts: it is an observation like any
  # other, so leaving it out moves `now` backwards past a confirmation that has
  # already happened -- which `validate_tbl_now()` below then rejects.
  now <- tryCatch(
    infer_now(x,
      now = now, event_date = get_event_date(x),
      report_date = get_report_date(x),
      confirmation_date = get_confirmation_date(x)
    ),
    error = function(e) get_now(x)
  )
  attr(x, "now") <- now


  validate_tbl_now(x)

  return(x)
}

#' @rdname add
#' @export
update_now <- function(x) {
  change_now(x)
}


#' @rdname add
#' @export
# Change the `event_date` to a different column name
change_event_date <- function(x, event_date) {
  # Get the event date
  value_pos <- tidyselect::eval_select(rlang::expr({{ event_date }}), x)
  value <- colnames(x)[value_pos]

  if (!inherits(x, "tbl_now")) {
    cli::cli_abort("{.arg x} must be a {.code tbl_now} object")
  }

  if (length(value) != 1) {
    cli::cli_abort("{.arg event_date} must be the name of one column (length 1)")
  }

  if (!lubridate::is.Date(x[[value]])) {
    cli::cli_abort("Column {.val {value}} must be of class Date")
  }

  attr(x, "event_date") <- value

  # Recalculate the delay when changing event date
  x <- time_cols_to_numeric(x,
    event_date = value,
    report_date = get_report_date(x),
    report_units = get_report_units(x),
    event_units = get_event_units(x),
    force = TRUE
  )

  # Re-infer now if needed
  now <- tryCatch(
    infer_now(x, now = get_now(x), event_date = value, report_date = get_report_date(x)),
    error = function(e) get_now(x)
  )
  attr(x, "now") <- now


  validate_tbl_now(x)

  return(x)
}

#' @rdname add
#' @export
# Change the `report_date` to a different column name
change_report_date <- function(x, report_date) {
  # Get the report date
  value_pos <- tidyselect::eval_select(rlang::expr({{ report_date }}), x)
  value <- colnames(x)[value_pos]


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

  attr(x, "report_date") <- value

  # Recalculate the delay when changing report date
  x <- time_cols_to_numeric(x,
    event_date = get_event_date(x),
    report_date = value,
    report_units = get_report_units(x),
    event_units = get_event_units(x),
    force = TRUE
  )


  # Re-infer now if needed
  now <- tryCatch(
    infer_now(x, now = get_now(x), event_date = get_event_date(x), report_date = value),
    error = function(e) get_now(x)
  )
  attr(x, "now") <- now

  validate_tbl_now(x)

  return(x)
}

#' @rdname add
#' @export
# Change the `case_count` to a different column name
change_case_count <- function(x, case_count) {
  # Get the event date
  value_pos <- tidyselect::eval_select(rlang::expr({{ case_count }}), x)

  if (length(value_pos) == 0) {
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

  attr(x, "case_count") <- value

  validate_tbl_now(x)

  return(x)
}

#' @rdname add
#' @export
# Change the `is_censored` to a different column name
change_is_censored <- function(x, is_censored) {
  # Get the event date
  value_pos <- tidyselect::eval_select(rlang::expr({{ is_censored }}), x)

  if (length(value_pos) == 0) {
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

  attr(x, "is_censored") <- value

  validate_tbl_now(x)

  return(x)
}

#' @rdname add
#' @export
# Remove `value`  from is_censored
remove_is_censored <- function(x) {
  if (get_data_type(x) == "count-cumulative") {
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
  if (length(get_is_censored(x)) > 0) {
    cli::cli_abort(
      paste0(
        "Already has value {.val {get_is_censored(x)}} as censored indicator.",
        " Use {.help remove_is_censored} to remove it before adding or",
        " {.help change_is_censored} to change it."
      )
    )
  }

  # Add to censored
  change_is_censored(x, {{ is_censored }})
}

#' @rdname add
#' @export
# Change all of the strata to whatever is added in ...
change_strata <- function(x, ..., warn_now = TRUE, warn_non_uniqueness = TRUE) {
  if (!inherits(x, "tbl_now")) {
    cli::cli_abort("{.arg x} must be a {.code tbl_now} object")
  }

  # Get the values
  value_pos <- tidyselect::eval_select(rlang::expr(c(...)), x)

  if (length(value_pos) == 0) {
    value <- NULL
  } else {
    value <- colnames(x)[value_pos]
  }

  # Assign the values
  attr(x, "strata") <- value

  # Verify it works
  validate_tbl_now(x, warn_now = warn_now, warn_non_uniqueness = warn_non_uniqueness)

  return(x)
}

#' @rdname add
#' @export
# Remove `value`  from strata
remove_strata <- function(x, ...) {
  if (get_data_type(x) == "count-cumulative") {
    cli::cli_alert_warning(
      "Removing strata from count-cumulative data might have unintended consequences. We suggest manually aggregating the data and then calling `tbl_now`"
    )
  }
  # Get the values
  value_pos <- tidyselect::eval_select(rlang::expr(c(...)), x)
  value <- colnames(x)[value_pos]

  # Get the strata that is not value
  strata_to_keep <- get_strata(x)
  strata_to_keep <- strata_to_keep[which(!(strata_to_keep %in% c(value)))]

  change_strata(x, dplyr::all_of(strata_to_keep))
}

#' @rdname add
#' @export
# Adds `value`  to existing strata
add_strata <- function(x, ...) {
  value_names <- colnames(x)[tidyselect::eval_select(rlang::expr(c(...)), x)]

  # Add to strata — pass as all_of() to avoid external-vector deprecation
  change_strata(x, dplyr::all_of(c(value_names, get_strata(x))))
}


#' @rdname add
#' @export
# Remove all `values`  from strata
remove_all_strata <- function(x) {
  if (get_data_type(x) == "count-cumulative") {
    cli::cli_alert_warning(
      "Removing strata from count-cumulative data might have unintended consequences. We suggest manually aggregating the data and then calling `tbl_now`"
    )
  }
  change_strata(x, NULL)
}


#' @rdname add
#' @export
# Change the existing covariates to the ones in value
change_covariates <- function(x, ..., warn_now = TRUE, warn_non_uniqueness = TRUE) {
  if (!inherits(x, "tbl_now")) {
    cli::cli_abort("{.arg x} must be a {.code tbl_now} object")
  }

  # Get the values
  value_pos <- tidyselect::eval_select(rlang::expr(c(...)), x)

  if (length(value_pos) == 0) {
    value <- NULL
  } else {
    value <- colnames(x)[value_pos]
  }

  # Assign the values
  attr(x, "covariates") <- value

  validate_tbl_now(x, warn_now = warn_now, warn_non_uniqueness = warn_non_uniqueness)

  return(x)
}

#' @rdname add
#' @export
# Removes the specific covariate `value`
remove_covariates <- function(x, ...) {
  if (get_data_type(x) == "count-cumulative") {
    cli::cli_alert_warning(
      "Removing covariates from count-cumulative data might have unintended consequences. We suggest manually aggregating the data and then calling `tbl_now`"
    )
  }
  # Get the values
  value_pos <- tidyselect::eval_select(rlang::expr(c(...)), x)
  value <- colnames(x)[value_pos]

  # Get the strata that is not value
  covariates_to_keep <- get_covariates(x)
  covariates_to_keep <- covariates_to_keep[which(!(covariates_to_keep %in% c(value)))]

  change_covariates(x, dplyr::all_of(covariates_to_keep))
}

#' @rdname add
#' @export
# Adds `value`  to existing covariates
add_covariates <- function(x, ...) {
  # Pass value to covariate
  value_names <- colnames(x)[tidyselect::eval_select(rlang::expr(c(...)), x)]

  # Add to covariates — pass as all_of() to avoid external-vector deprecation
  change_covariates(x, dplyr::all_of(c(value_names, get_covariates(x))))
}


#' @rdname add
#' @export
# Removes all covariates
remove_all_covariates <- function(x) {
  if (get_data_type(x) == "count-cumulative") {
    cli::cli_alert_warning(
      "Removing covariates from count-cumulative data might have unintended consequences. We suggest manually aggregating the data and then calling `tbl_now`"
    )
  }
  change_covariates(x, NULL)
}


#' @rdname add
#' @export
replace_temporal_effects <- function(x, t_effects) {
  if (get_data_type(x) == "count-cumulative") {
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
    x <- x |> dplyr::select(-dplyr::one_of(cols_to_remove))
  }

  # Reset both spec and computed-cols attributes
  attr(x, "temporal_effects") <- list()
  attr(x, "computed_temporal_effect_cols") <- character(0)

  if (!is.null(t_effects)) {
    x <- add_temporal_effects(x, t_effects = t_effects)
  }

  x
}

#' @rdname add
#' @export
remove_temporal_effects <- function(x) {
  replace_temporal_effects(x, t_effects = NULL)
}
