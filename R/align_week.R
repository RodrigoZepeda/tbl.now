#' Align weeks to a common weekday
#'
#' Aligns all dates in either a `data.frame` or a `tbl.now` so that week
#' boundaries occur on a specified day of the week. This is useful in the
#' context of nowcasting for cases when weekly reports are changed from say
#' Wednesday to Thursday so that delays don't have a decimal point.
#'
#' @details
#' In some cases, to calculate the delay of information, what matters is the
#' week distance (reports from week 3 in week 7) and not the specific date distance
#' (reports from Saturday of week 3 vs Monday in week 7). The `align_weeks`
#' function ensures that all week reports are aligned to the same day of the week
#' (if applied to a `tbl_now`) or
#'
#'
#' @note
#' This is also useful when working with epiweeks
#' or isoweeks where week boundaries may differ between systems or years
#'
#' @param .data A `data.frame` or tibble.
#'
#' @param align_on_day Integer 1–7 indicating the weekday to align to.
#' Uses [lubridate::wday()] numbering (1 = Sunday, 7 = Saturday).
#'
#' @param type Either `"epi"` (default) or `"iso"`. Determines
#'   which week/year functions to use whether [lubridate::epiweek()] or
#'   [lubridate::isoweek()]
#'
#' @param ... Additional arguments to pass to function
#'
#' @param date_col A [tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html) column
#' name containing dates.
#'
#' @param new_date_col Name of the new aligned date column to be created. By default
#' it creates a column named `\{date_col\}_aligned` where `date_col` corresponds
#' to the column name passed to that parameter.
#'
#' @return A tibble identical to `.data` but with an added aligned date column.
#'
#' @examples
#' # DATA.FRAMES:
#' # The function aligns weekly data to be "reported" on the same day of the week
#' df <- data.frame(
#'   date = as.Date(c("2020-10-31", "2022-11-07", "2022-11-13"))
#' )
#'
#' # Align to Sundays
#' align_weeks(df, date_col = date)
#'
#' # Align to Tuesday
#' align_weeks(df, date_col = date, align_on_day = 3)
#'
#' ## TBL_NOWS
#' # If not used you can see the delay has decimal points because
#' # reports (`as_of`) are sometimes on Saturday and sometimes Wednesday
#' data(flusight)
#'
#' #Get the table
#' flutbl <- tbl_now(flusight, event_date = "target_end_date",
#'   report_date = "as_of", case_col = "observation",
#'   strata = c("location_name"))
#'
#' #See that some delays have decimals
#' suppressWarnings(as.numeric(flutbl[413484, ".delay"]))
#'
#' #Align the weeks so that they all start on Sunday
#' flutbl <- flutbl %>% align_weeks()
#'
#' #Delayed decimals are now integer as all weeks start in Sunday!
#' suppressWarnings(as.numeric(flutbl[413484, ".delay"]))
#'
#' @name align_weeks
#' @export
align_weeks <- function(.data, align_on_day = 1, type = "epi", ...){
  UseMethod("align_weeks")
}


#' @export
#' @rdname align_weeks
align_weeks.data.frame <- function(.data,
                       align_on_day = 1,
                       type = "epi",
                       ...,
                       date_col,
                       new_date_col = NULL) {

  #Find the datecol — capture as quosure so .tbl_now_eval_select() can detect
  #character-valued variables and wrap them in all_of(), suppressing the
  #tidyselect "external vector" deprecation warning.
  date_col_quo    <- rlang::enquo(date_col)
  date_col_select <- .tbl_now_eval_select(date_col_quo, .data)
  date_col        <- colnames(.data)[date_col_select]

  if (length(date_col) > 1){
    cli::cli_abort("Can only operate on one column at a time. Columns {date_col} were given.")
  }

  if (is.null(new_date_col)){
    new_date_col <- paste0(date_col, "_aligned")
  }

  # Choose functions properly (avoid ifelse)
  if (!(type %in% c("epi","iso"))){
    cli::cli_abort("Invalid type {.val {type}}. Set {.val epi} for {.help lubridate::epiweek} or {.val iso} for {.help lubridate::isoweek}")
  }
  week_fun <- if (type == "epi") lubridate::epiweek else lubridate::isoweek
  year_fun <- if (type == "epi") lubridate::epiyear else lubridate::isoyear

  .data %>%
    dplyr::mutate(
      !!as.symbol("week_col") := week_fun(!!as.symbol(date_col)),
      !!as.symbol("year_col") := year_fun(!!as.symbol(date_col))
    ) %>%
    week_2_date(
      week_col     = "week_col",
      year_col     = "year_col",
      align_on_day = align_on_day,
      week_fun     = week_fun,
      year_fun     = year_fun,
      date_col_name = new_date_col
    ) %>%
    dplyr::select(-!!as.symbol("week_col"), -!!as.symbol("year_col"))
}

#' @export
#' @rdname align_weeks
align_weeks.tbl_now <- function(.data, align_on_day = 1, type = "epi", ...) {

  event_col  <- get_event_date(.data)
  report_col <- get_report_date(.data)

  .data <- .data %>%
    align_weeks.data.frame(date_col = event_col, align_on_day = align_on_day, type = type, new_date_col = paste0("temp_", event_col)) %>%
    align_weeks.data.frame(date_col = report_col, align_on_day = align_on_day, type = type, new_date_col = paste0("temp_", report_col))

  #Throws the warning that its forcing conversion to tibble which we don't need
  suppressWarnings({
    .data <- .data %>%
      dplyr::select(-!!as.symbol(event_col), -!!as.symbol(report_col), -!!as.symbol(".delay"),
                  -!!as.symbol(".event_num"), -!!as.symbol(".report_num"))
  })

  #Recalculate the now:
  new_now <- align_weeks(data.frame(now = get_now(.data)), date_col = "now",
                         type = type, align_on_day = align_on_day,
                         new_date_col = "new_now") %>%
    dplyr::pull(!!as.symbol("new_now"))

  result <- .data %>%
    dplyr::rename(!!as.symbol(event_col) := !!as.symbol(paste0("temp_", event_col))) %>%
    dplyr::rename(!!as.symbol(report_col) := !!as.symbol(paste0("temp_", report_col))) %>%
    as_tbl_now(event_date = event_col, report_date = report_col, align_weeks = FALSE,
               verbose = FALSE,
               data_type    = get_data_type(.data),
               strata       = get_strata(.data),
               covariates   = get_covariates(.data),
               case_count   = get_case_count(.data),
               is_censored  = get_is_censored(.data),
               event_units  = get_event_units(.data),
               report_units = get_report_units(.data),
               now          = new_now)

  # Preserve the lazy temporal-effects spec (computed cols are invalidated by
  # the date-realignment so they are intentionally dropped)
  attr(result, "temporal_effects") <- get_temporal_effects(.data)
  result

}



#' Convert epidemiological (or ISO) week/year to aligned dates
#'
#' Takes week numbers and year numbers and returns the date corresponding
#' to a specified weekday within that week. This is typically used for
#' aligning epiweek or isoweek data to a consistent weekday.
#'
#' @param .data A data.frame or tibble.
#' @param week_col [tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html) column
#' name containing epidemiological week numbers.
#' @param year_col [tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html) column
#' name containing epidemiological year numbers.
#' @param align_on_day Integer 1–7 ([lubridate::wday()] numbering) indicating
#'   the weekday to align to.
#' @param week_fun Function that extracts week numbers from a date
#'   (either [lubridate::epiweek()], [lubridate::isoweek()]).
#' @param year_fun Function that extracts the epidemiological/ISO year from
#' a date (either [lubridate::epiyear()] or [lubridate::isoyear()]).
#' @param date_col_name Name of the resulting date column.
#'
#' @return The given `data.frame` with a new date column appended.
#'
#' @examples
#' df <- data.frame(
#'   epidemiological_week = 1:5,
#'   epidemiological_year = rep(2024, 5)
#' )
#'
#' df %>%
#'   week_2_date(
#'     week_col = epidemiological_week,
#'     year_col = epidemiological_year
#'   )
#'
#' @export
week_2_date <- function(.data,
                        week_col,
                        year_col,
                        align_on_day = 1,
                        week_fun = lubridate::epiweek,
                        year_fun = lubridate::epiyear,
                        date_col_name = "date") {

  #Stop if date_col_name already in .data
  if (date_col_name %in% colnames(.data)){
    cli::cli_abort("Column {.val {date_col_name}} is already in `.data`. Cannot proceed to transformation. Choose a different `date_col_name`.")
  }

  #Parse the week column
  week_col_select <- tidyselect::eval_select(rlang::expr({{ week_col }}), .data)
  week_col        <- colnames(.data)[week_col_select]

  if (length(week_col) > 1){
    cli::cli_abort("Can only operate on one column at a time. Columns {.val {week_col}} were given.")
  }

  #Parse the year column
  year_col_select <- tidyselect::eval_select(rlang::expr({{ year_col }}), .data)
  year_col        <- colnames(.data)[year_col_select]

  if (length(year_col) > 1){
    cli::cli_abort("Can only operate on one column at a time. Columns {.val {year_col}} were given.")
  }

  #Get all the years
  years <- .data %>% dplyr::pull(dplyr::all_of(year_col))

  yr_min <- min(years) - 1
  yr_max <- max(years) + 1

  # Construct a date range covering the required weeks
  date_tbl <- dplyr::tibble(
    !!as.symbol(date_col_name) := seq(
      as.Date(paste0(yr_min, "-12-24")),
      as.Date(paste0(yr_max, "-01-08")),
      by = "1 day"
    )
  ) %>%
    dplyr::mutate(
      !!as.symbol(week_col) := week_fun(!!as.symbol(date_col_name)),
      !!as.symbol(year_col) := year_fun(!!as.symbol(date_col_name)),
      !!as.symbol("day_of_week") := lubridate::wday(!!as.symbol(date_col_name), label = FALSE)
    ) %>%
    dplyr::filter(!!as.symbol("day_of_week") == align_on_day) %>%
    dplyr::select(-!!as.symbol("day_of_week"))

  dplyr::left_join(.data, date_tbl, by = c(week_col, year_col))
}
