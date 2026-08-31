#' Put weekly data on a common weekday
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Weekly surveillance data is rarely as tidy as it looks. The same series may be
#' stamped with a Wednesday one year and a Thursday the next, or event dates may
#' fall on a Sunday while reports fall on a Saturday. When that happens the delay
#' between the two stops being a whole number of weeks -- you get delays of 2.86
#' weeks -- and most nowcasting models, which count in whole periods, either
#' refuse the data or quietly round it.
#'
#' `align_weeks()` snaps every date to the same weekday, so week differences come
#' out as integers. `week_2_date()` solves the neighbouring problem: you have
#' epiweek (or ISO week) *numbers* rather than dates, and need real dates to build
#' a `tbl_now` from.
#'
#' @details
#' Applied to a `data.frame`, `align_weeks()` adds an aligned copy of the column
#' you name. Applied to a `tbl_now`, it aligns the event and report dates
#' together and recomputes the delay, so the object stays coherent.
#'
#' Epi weeks and ISO weeks disagree about where a year starts, so `type` picks
#' which convention to use: `"epi"` uses [lubridate::epiweek()] /
#' [lubridate::epiyear()], `"iso"` uses [lubridate::isoweek()] /
#' [lubridate::isoyear()].
#'
#' @param .data A `data.frame`, tibble or `tbl_now`.
#'
#' @param align_on_day Integer 1-7 giving the weekday to align to, in ISO
#' numbering: **1 = Monday**, 2 = Tuesday, ..., **7 = Sunday**. This is
#' [lubridate::wday()] with `week_start = 1`, the same convention
#' [is_weekday()] uses. Defaults to `7` (Sunday), the start of an
#' epidemiological week.
#'
#' @param type Either `"epi"` (default) or `"iso"`, choosing whether week and
#'   year are read with [lubridate::epiweek()] or [lubridate::isoweek()].
#'
#' @param ... Additional arguments passed to methods.
#'
#' @param date_col For the `data.frame` method, the
#' [tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
#' column holding the dates to align.
#'
#' @param new_date_col Name for the aligned column. Defaults to
#' `\{date_col\}_aligned`.
#'
#' @param week_col,year_col For `week_2_date()`, the
#' [tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
#' columns holding the week number and the year.
#'
#' @param week_fun,year_fun For `week_2_date()`, the functions defining the week
#' convention: [lubridate::epiweek()]/[lubridate::epiyear()] (the default) or
#' [lubridate::isoweek()]/[lubridate::isoyear()].
#'
#' @param date_col_name For `week_2_date()`, the name of the date column to
#' create.
#'
#' @return
#' `align_weeks()` returns its input with an aligned date column added
#' (`data.frame` method), or a `tbl_now` whose dates have been aligned and whose
#' `.delay` has been recomputed.
#'
#' `week_2_date()` returns the input `data.frame` with a new date column
#' appended.
#'
#' @note Useful whenever week boundaries differ between systems or between years,
#' which is the normal state of affairs for epiweek and ISO week data.
#'
#' @seealso
#' [tbl_now()], whose `align_weeks = TRUE` argument does this at construction
#' time; [is_weekday()], which numbers weekdays the same way;
#' [complete_zeroes()] for filling the weeks where nothing was reported;
#' [temporal_effects()] for using week-of-year as a model term.
#'
#' @examples
#' ## ---- Plain data frames ------------------------------------------------
#'
#' # Three dates falling on different weekdays.
#' df <- data.frame(date = as.Date(c("2022-11-02", "2022-11-07", "2022-11-13")))
#' weekdays(df$date)
#'
#' # Snap them all back to the Sunday that starts their week.
#' aligned <- align_weeks(df, date_col = date)
#' aligned
#' weekdays(aligned$date_aligned)
#'
#' # Or to Tuesday. Weekday numbers are ISO: 1 = Monday, so Tuesday is 2.
#' align_weeks(df, date_col = date, align_on_day = 2)
#'
#' ## ---- A tbl_now: making the delays whole numbers -------------------------
#'
#' data(flusight)
#'
#' # One state is enough to see the problem.
#' texas <- flusight[flusight$location_name == "Texas", ]
#' flutbl <- tbl_now(texas,
#'   event_date = "target_end_date",
#'   report_date = "as_of", case_count = "observation",
#'   strata = "location_name", verbose = FALSE
#' )
#'
#' # `as_of` is sometimes a Saturday and sometimes a Wednesday, so some delays
#' # land between whole weeks.
#' mean(flutbl$.delay != round(flutbl$.delay))
#'
#' # After aligning, every delay is a whole number of weeks.
#' flutbl <- align_weeks(flutbl)
#' mean(flutbl$.delay != round(flutbl$.delay))
#'
#' ## ---- Week numbers instead of dates --------------------------------------
#'
#' # Data reported as "week 1 of 2024" and so on, with no usable date column.
#' df <- data.frame(
#'   epidemiological_week = 1:5,
#'   epidemiological_year = rep(2024, 5)
#' )
#'
#' ## week_2_date() turns those into the Sunday that starts each epiweek.
#' week_2_date(df,
#'   week_col = epidemiological_week,
#'   year_col = epidemiological_year
#' )
#'
#' @name align_weeks
#' @export
align_weeks <- function(.data, align_on_day = 7, type = "epi", ...) {
  UseMethod("align_weeks")
}


#' @export
#' @rdname align_weeks
align_weeks.data.frame <- function(.data,
                                   align_on_day = 7,
                                   type = "epi",
                                   ...,
                                   date_col,
                                   new_date_col = NULL) {
  # Find the datecol — capture as quosure so .tbl_now_eval_select() can detect
  # character-valued variables and wrap them in all_of(), suppressing the
  # tidyselect "external vector" deprecation warning.
  date_col_quo <- rlang::enquo(date_col)
  date_col_select <- .tbl_now_eval_select(date_col_quo, .data)
  date_col <- colnames(.data)[date_col_select]

  if (length(date_col) > 1) {
    cli::cli_abort("Can only operate on one column at a time. Columns {date_col} were given.")
  }

  if (is.null(new_date_col)) {
    new_date_col <- paste0(date_col, "_aligned")
  }

  # Choose functions properly (avoid ifelse)
  if (!(type %in% c("epi", "iso"))) {
    cli::cli_abort("Invalid type {.val {type}}. Set {.val epi} for {.help lubridate::epiweek} or {.val iso} for {.help lubridate::isoweek}")
  }
  week_fun <- if (type == "epi") lubridate::epiweek else lubridate::isoweek
  year_fun <- if (type == "epi") lubridate::epiyear else lubridate::isoyear

  .data |>
    dplyr::mutate(
      !!as.symbol("week_col") := week_fun(!!as.symbol(date_col)),
      !!as.symbol("year_col") := year_fun(!!as.symbol(date_col))
    ) |>
    week_2_date(
      week_col = "week_col",
      year_col = "year_col",
      align_on_day = align_on_day,
      week_fun = week_fun,
      year_fun = year_fun,
      date_col_name = new_date_col
    ) |>
    dplyr::select(-!!as.symbol("week_col"), -!!as.symbol("year_col"))
}

#' @export
#' @rdname align_weeks
align_weeks.tbl_now <- function(.data, align_on_day = 7, type = "epi", ...) {
  event_col <- get_event_date(.data)
  report_col <- get_report_date(.data)
  # The THIRD date has to be aligned too. Left on its own weekday grid the
  # confirmation delay comes out fractional -- the same trap `.delay` has, and
  # the reason this function exists.
  confirmation_col <- get_confirmation_date(.data)

  .data <- .data |>
    align_weeks.data.frame(date_col = event_col, align_on_day = align_on_day, type = type, new_date_col = paste0("temp_", event_col)) |>
    align_weeks.data.frame(date_col = report_col, align_on_day = align_on_day, type = type, new_date_col = paste0("temp_", report_col))

  if (!is.null(confirmation_col)) {
    .data <- align_weeks.data.frame(
      .data,
      date_col = confirmation_col, align_on_day = align_on_day, type = type,
      new_date_col = paste0("temp_", confirmation_col)
    )
  }

  # Throws the warning that its forcing conversion to tibble which we don't need
  suppressWarnings({
    .data <- .data |>
      dplyr::select(
        -!!as.symbol(event_col), -!!as.symbol(report_col), -!!as.symbol(".delay"),
        -!!as.symbol(".event_num"), -!!as.symbol(".report_num")
      ) |>
      dplyr::select(-dplyr::any_of(c(
        confirmation_col, ".confirmation_num", ".confirmation_delay"
      )))
  })

  # Recalculate the now:
  new_now <- align_weeks(data.frame(now = get_now(.data)),
    date_col = "now",
    type = type, align_on_day = align_on_day,
    new_date_col = "new_now"
  ) |>
    dplyr::pull(!!as.symbol("new_now"))

  renamed <- .data |>
    dplyr::rename(!!as.symbol(event_col) := !!as.symbol(paste0("temp_", event_col))) |>
    dplyr::rename(!!as.symbol(report_col) := !!as.symbol(paste0("temp_", report_col)))
  if (!is.null(confirmation_col)) {
    renamed <- dplyr::rename(
      renamed,
      !!as.symbol(confirmation_col) := !!as.symbol(paste0("temp_", confirmation_col))
    )
  }

  confirmation_args <- if (is.null(confirmation_col)) {
    list()
  } else {
    type_col <- get_confirmation_type(.data)
    list(
      confirmation_date = confirmation_col,
      confirmation_type = if (!is.null(type_col) && type_col %in% colnames(renamed)) {
        type_col
      } else {
        NULL
      },
      confirmation_units = get_confirmation_units(.data) %||% "auto"
    )
  }

  result <- do.call(as_tbl_now, c(
    list(
      renamed,
      event_date = event_col, report_date = report_col, align_weeks = FALSE,
      verbose = FALSE,
      data_type = get_data_type(.data),
      strata = get_strata(.data),
      covariates = get_covariates(.data),
      case_count = get_case_count(.data),
      is_censored = get_is_censored(.data),
      event_units = get_event_units(.data),
      report_units = get_report_units(.data),
      now = new_now
    ),
    confirmation_args
  ))

  # Preserve the lazy temporal-effects spec (computed cols are invalidated by
  # the date-realignment so they are intentionally dropped)
  attr(result, "temporal_effects") <- get_temporal_effects(.data)
  result
}


#' @rdname align_weeks
#' @export
week_2_date <- function(.data,
                        week_col,
                        year_col,
                        align_on_day = 7,
                        week_fun = lubridate::epiweek,
                        year_fun = lubridate::epiyear,
                        date_col_name = "date") {
  # Stop if date_col_name already in .data
  if (date_col_name %in% colnames(.data)) {
    cli::cli_abort("Column {.val {date_col_name}} is already in `.data`. Cannot proceed to transformation. Choose a different `date_col_name`.")
  }

  # Parse the week column
  week_col_select <- tidyselect::eval_select(rlang::expr({{ week_col }}), .data)
  week_col <- colnames(.data)[week_col_select]

  if (length(week_col) > 1) {
    cli::cli_abort("Can only operate on one column at a time. Columns {.val {week_col}} were given.")
  }

  # Parse the year column
  year_col_select <- tidyselect::eval_select(rlang::expr({{ year_col }}), .data)
  year_col <- colnames(.data)[year_col_select]

  if (length(year_col) > 1) {
    cli::cli_abort("Can only operate on one column at a time. Columns {.val {year_col}} were given.")
  }

  # Get all the years
  years <- .data |> dplyr::pull(dplyr::all_of(year_col))

  yr_min <- min(years) - 1
  yr_max <- max(years) + 1

  # Construct a date range covering the required weeks
  date_tbl <- dplyr::tibble(
    !!as.symbol(date_col_name) := seq(
      as.Date(paste0(yr_min, "-12-24")),
      as.Date(paste0(yr_max, "-01-08")),
      by = "1 day"
    )
  ) |>
    dplyr::mutate(
      !!as.symbol(week_col) := week_fun(!!as.symbol(date_col_name)),
      !!as.symbol(year_col) := year_fun(!!as.symbol(date_col_name)),
      !!as.symbol("day_of_week") := lubridate::wday(!!as.symbol(date_col_name), label = FALSE, week_start = 1)
    ) |>
    dplyr::filter(!!as.symbol("day_of_week") == align_on_day) |>
    dplyr::select(-!!as.symbol("day_of_week"))

  dplyr::left_join(.data, date_tbl, by = c(week_col, year_col))
}
