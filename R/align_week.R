#' Align weeks to a common weekday
#'
#' Aligns all dates in a dataset so that week boundaries occur on a
#' specified day of the week. This is useful in the context of nowcasting
#' for cases when weekly reports are changed from say Wednesday to Thursday.
#'
#' @note
#' This is also useful when working with epiweeks
#' or isoweeks where week boundaries may differ between systems or years
#'
#' @param .data A `data.frame` or tibble.
#'
#' @param date_col A \code{\link[dplyr:dplyr_tidy_select]{<`tidy-select`>}} column name containing dates.
#'
#' @param align_on_day Integer 1–7 indicating the weekday to align to.
#' Uses [lubridate::wday()] numbering (1 = Sunday, 7 = Saturday).
#'
#' @param type Either `"epi"` (default) or `"iso"`. Determines
#'   which week/year functions to use whether [lubridate::epiweek()] or
#'   [lubridate::isoweek()]
#'
#' @param new_date_col Name of the new aligned date column to be created. By default
#' it creates a column named `\{date_col\}_aligned` where `date_col` corresponds
#' to the column name passed to that parameter.
#'
#' @return A tibble identical to `.data` but with an added aligned date column.
#'
#' @examples
#' df <- data.frame(
#'   date = as.Date(c("2020-10-31", "2022-11-07", "2022-11-13"))
#' )
#'
#' # Align to Sundays
#' align_week(df, date_col = date)
#'
#' # Align to Tuesday
#' align_week(df, date_col = date, align_on_day = 3)
#'
#' @export
align_week <- function(.data,
                       date_col,
                       align_on_day = 1,
                       type = "epi",
                       new_date_col = NULL) {

  #Find the datecol
  date_col_select <- tidyselect::eval_select(rlang::expr({{ date_col }}), .data)
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

#' Convert epidemiological (or ISO) week/year to aligned dates
#'
#' Takes week numbers and year numbers and returns the date corresponding
#' to a specified weekday within that week. This is typically used for
#' aligning epiweek or isoweek data to a consistent weekday.
#'
#' @param .data A data.frame or tibble.
#' @param week_col \code{\link[dplyr:dplyr_tidy_select]{<`tidy-select`>}} column
#' name containing epidemiological week numbers.
#' @param year_col \code{\link[dplyr:dplyr_tidy_select]{<`tidy-select`>}} column
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
