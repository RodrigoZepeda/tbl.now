#' Add temporal effect coding to a `tbl_now`
#'
#' @description
#' Takes a `tbl_now` or a `data.frame` and adds the temporal effects `t_effect` as
#' columns
#'
#' @param x A `tbl_now` object or a `data.frame`.
#'
#' @param date_col The column which contains the `<Date>` values from which
#' effects will be calculated. This applies to all `temporal_effects` except
#' for `seasonal`.
#'
#' @param numeric_col The column which contains the values from which
#' the seasonal effects will be calculated. This applies only to seasonal
#' effects. For date-related effects (such as month or day of the week)
#' use `date_col`.
#'
#' @param date_type Either `event_date` (default) or `report_date`
#' to add temporal effects to those columns.
#'
#' @param t_effect A [temporal_effects()] object codifying the
#' temporal effects to be used.
#'
#' @param name_prefix What preffix to add to the column names
#'
#' @param overwrite If `TRUE` ignores that the columns already exist and overwrites them.
#' If `FALSE` it throws an errors if the columns it is creating already exist (default).
#'
#' @param ... Additional arguments (unused)
#'
#' @inheritParams is_weekday
#'
#'
#' @return A `tbl_now` or `data.frame` containing all of the effects as new columns.
#'
#' @examples
#' data(denguedat)
#'
#' # Get disease
#' disease_data <- tbl_now(denguedat,
#'     event_date = "onset_week",
#'     report_date = "report_week",
#'     strata = "gender")
#'
#' # Add an effect for epidemiological week
#' add_temporal_effects(disease_data, t_effect = temporal_effects(week_of_year = TRUE))
#' @name add_temporal_effects
#' @export
add_temporal_effects <- function(x, t_effect = NULL, overwrite = FALSE, ...) {
  UseMethod("add_temporal_effects")
}

#' @export
#' @rdname add_temporal_effects
add_temporal_effects.data.frame <- function(x, t_effect = NULL, overwrite = FALSE,
                                            ...,
                                            date_col = NULL,
                                            numeric_col = NULL,
                                            name_prefix = paste0(".", date_col),
                                            weekend_days = c("Sat","Sun")) {

  #Do nothing if no effect
  if (is.null(t_effect)){
    return(x)
  }

  #Check the class
  if (!S7::S7_inherits(t_effect, class = temporal_effects)){
    cli::cli_abort(
      "`t_effect` should be a {.code t_effect} object. Use `temporal_effects` to create it."
    )
  }

  #Check the column belongs to x.frame
  if (!is.null(date_col) && !(date_col %in% colnames(x))){
    cli::cli_abort(
      "Column {.value {date_col}} is not a column in `x`"
    )
  }

  if (!is.null(numeric_col) && !(numeric_col %in% colnames(x))){
    cli::cli_abort(
      "Column {.value {numeric_col}} is not a column in `x`"
    )
  }

  # Get the initial date (this is for codifying the month and epiweek effects so that they start in 1)
  init_date <- x %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::pull(!!as.symbol(date_col))

  # Add day of the week effect-----
  if (!is.null(t_effect)) {

    if (t_effect@"day_of_week" & !is.null(date_col)) {

      if (paste0(name_prefix,"_day_of_week") %in% colnames(x) && !overwrite){
        cli::cli_abort(
          "Column {.val {name_prefix}_day_of_week} already exists in data. Set `overwrite = TRUE` to overwrite it."
        )
      }

      x <- x %>%
        dplyr::mutate(!!as.symbol(paste0(name_prefix,"_day_of_week")) :=
                        as.integer(lubridate::wday(!!as.symbol(date_col))))
    }

    # Add weekend effect-----
    if (t_effect@weekend & !is.null(date_col)) {

      if (paste0(name_prefix,"_weekend") %in% colnames(x) && !overwrite){
        cli::cli_abort(
          "Column {.val {name_prefix}_weekend} already exists in data. Set `overwrite = TRUE` to overwrite it."
        )
      }

      x <- x %>%
        dplyr::mutate(!!as.symbol(paste0(name_prefix,"_weekend")) :=
                        as.integer(!is_weekday(!!as.symbol(date_col), weekend_days = weekend_days)))
    }

    # Add day of the month effect-----
    if (t_effect@day_of_month & !is.null(date_col)) {

      if (paste0(name_prefix,"_day_of_month") %in% colnames(x) && !overwrite){
        cli::cli_abort(
          "Column {.val {name_prefix}_day_of_month} already exists in data. Set `overwrite = TRUE` to overwrite it."
        )
      }

      x <- x %>%
        dplyr::mutate(!!as.symbol(paste0(name_prefix,"_day_of_month")) :=
                        as.integer(lubridate::day(!!as.symbol(date_col))))
    }

    # Add month effect (centered at current month = 1)-----
    if (t_effect@month_of_year & !is.null(date_col)) {

      if (paste0(name_prefix,"_month_of_year") %in% colnames(x) && !overwrite){
        cli::cli_abort(
          "Column {.val {name_prefix}_month_of_year} already exists in data. Set `overwrite = TRUE` to overwrite it."
        )
      }

      x <- x %>%
        dplyr::mutate(!!as.symbol(paste0(name_prefix,"_month_of_year")) :=
                        as.integer(1 +
                                     ((lubridate::month(!!as.symbol(date_col)) - lubridate::month(init_date)) %% 12)
                        )
        )
    }

    # Add epiweek effect (centered at current week = 1 | week 53 that almost never happens is collapsed to January)-----
    if (t_effect@week_of_year & !is.null(date_col)) {

      if (paste0(name_prefix,"_week_of_year") %in% colnames(x) && !overwrite){
        cli::cli_abort(
          "Column {.val {name_prefix}_week_of_year} already exists in data. Set `overwrite = TRUE` to overwrite it."
        )
      }

      x <- x %>%
        dplyr::mutate(!!as.symbol(paste0(name_prefix,"_week_of_year")) :=
                        as.integer(1 +
                                     ((lubridate::epiweek(!!as.symbol(date_col)) - lubridate::epiweek(init_date)) %% 52)
                        )
        )
    }

    # Add seasons-----
    if (!is.null(t_effect@seasons) & length(t_effect@seasons) > 0 & !is.null(numeric_col)) {
      #For each season add a column
      for (m in t_effect@seasons){

        #Create season name
        season_name <- paste0(name_prefix,"_season_", m)
        #Add the season names
        if ((paste0(season_name, "_cos") %in% colnames(x)) || (paste0(season_name, "_sin") %in% colnames(x)) && !overwrite){
          cli::cli_abort(
            "At least one of the columns: {.val {season_name}_cos} or {.val {season_name}_sin} already exist in data. Set `overwrite = TRUE` to overwrite them."
          )
        }

        #Add the sine and cosine component of the fourier terms
        x <- x %>%
          dplyr::mutate(!!as.symbol(paste0(season_name,"_cos")) := cos(2*base::pi*as.numeric(!!as.symbol(numeric_col)) / m)) %>%
          dplyr::mutate(!!as.symbol(paste0(season_name,"_sin")) := sin(2*base::pi*as.numeric(!!as.symbol(numeric_col))  / m))
      }
    }

    # Add holiday effect-----
    if (!is.null(t_effect@holidays) & length(t_effect@holidays) > 0 & !is.null(date_col)) {
      # Check almanac installation
      if (!rlang::check_installed("almanac")) {
        cli::cli_abort(
          "Please install the `almanac` package to be able to integrate `holiday` effects"
        )
      } else {

        if (paste0(name_prefix,"_holiday") %in% colnames(x) && !overwrite){
          cli::cli_abort(
            "Column {.val {name_prefix}_holiday} already exists in data. Set `overwrite = TRUE` to overwrite it."
          )
        }

        x <- x %>%
          dplyr::mutate(!!as.symbol(paste0(name_prefix,"_holiday")) :=
                          as.integer(
                            almanac::alma_in(!!as.symbol(date_col), t_effect[["holidays"]]))
          )
      }
    }
  }

  return(x)
}

#' @export
#' @rdname add_temporal_effects
add_temporal_effects.tbl_now <- function(x, t_effect = NULL, overwrite = FALSE,  ...,
                                         date_type = "event_date",
                                         weekend_days = c("Sat","Sun")){

  if (date_type == "event_date"){
    date_col    <- get_event_date(x)
    date_name   <- "event"
    numeric_col <- ".event_num"
  } else if (date_type == "report_date"){
    date_col    <- get_report_date(x)
    date_name   <- "report"
    numeric_col <- ".report_num"
  } else {
    cli::cli_abort("Invalid `date_type` use {.val event_date} or {.val report_date}")
  }

  old_cols <- colnames(x)
  x        <- add_temporal_effects.data.frame(x,
                                              t_effect = t_effect,
                                              date_col = date_col,
                                              numeric_col = numeric_col,
                                              name_prefix = paste0(".", date_name), ...)
  new_cols <- colnames(x)

  #Temporal effects are the ones in new_cols but not in old_cols
  temporal_effect_cols        <- setdiff(new_cols, old_cols)
  attr(x, "temporal_effects") <- c(attr(x, "temporal_effects"), temporal_effect_cols)

  return(x)

}
