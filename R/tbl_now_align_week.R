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
#' @param date_col A column name (string or tidy-selection) containing dates.
#'
#' @param align_on_day Integer 1–7 indicating the weekday to align to.
#' Uses [lubridate::wday()] numbering (1 = Sunday, 7 = Saturday).
#'
#' @param type Either `"epiweek"` (default) or `"isoweek"`. Determines
#'   which week/year functions to use.
#'
#' @param new_date_col Name of the new aligned date column to be created.
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
                       type = "epiweek",
                       new_date_col = paste0(deparse(substitute(date_col)), "_aligned")) {

  # Choose functions properly (avoid ifelse)
  week_fun <- if (type == "epiweek") lubridate::epiweek else lubridate::isoweek
  year_fun <- if (type == "epiweek") lubridate::epiyear else lubridate::isoyear

  .data %>%
    dplyr::mutate(
      !!as.symbol("week_col") := week_fun({{ date_col }}),
      !!as.symbol("year_col") := year_fun({{ date_col }})
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
#' @param week_col Column name (string) containing week numbers.
#' @param year_col Column name (string) containing year numbers.
#' @param align_on_day Integer 1–7 (lubridate weekday numbering) indicating
#'   the weekday to align to.
#' @param week_fun Function that extracts week numbers from a date
#'   (e.g., [lubridate::epiweek()], [lubridate::isoweek()]).
#' @param year_fun Function that extracts the epidemiological/ISO year from a date.
#' @param date_col_name Name of the resulting date column.
#'
#' @return The input dataframe with a new date column appended.
#'
#' @examples
#' df <- data.frame(
#'   week_col = 1:5,
#'   year_col = rep(2024, 5)
#' )
#'
#' week_2_date(df, week_col = "week_col", year_col = "year_col")
#'
#' @export
week_2_date <- function(.data,
                        week_col,
                        year_col,
                        align_on_day = 1,
                        week_fun = lubridate::epiweek,
                        year_fun = lubridate::epiyear,
                        date_col_name = "date") {

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
