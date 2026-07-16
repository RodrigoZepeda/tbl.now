#' Temporal Effects Class
#'
#' @description `r lifecycle::badge('stable')`
#'
#' The `temporal_effects` class specifies which temporal covariates or effects
#' should be included in a nowcasting model (e.g., day of week, month, holidays, etc.).
#'
#' @param day_of_week Logical. Whether to include an effect for each of the seven days of the week.
#' @param weekend Logical. Whether to include an effect for the weekend vs the weekday.
#' @param day_of_month Logical. Whether to include an effect for the day of the month (1 to 31).
#' @param month_of_year Logical. Whether to include an effect for the month of the year.
#' @param week_of_year Logical. Whether to include an effect for the epidemiological week.
#' @param holiday_lags Non-negative integer (default `0`). Depth `N` of the
#' *after-holiday* effect. When `N > 0` (and `holidays` is supplied) a set of
#' indicator columns `..._holiday_lag_1`, ..., `..._holiday_lag_N` is created,
#' where `..._holiday_lag_k` flags dates that fall exactly `k` **working days**
#' after a holiday. Working days skip weekends (see `weekend_days` in
#' [add_temporal_effects()]) and other holidays, so the effect lands on the first
#' day back at work. Use it to capture a rise in cases just after a holiday.
#' @param weekend_lags Non-negative integer (default `0`). Depth `N` of the
#' *after-weekend* effect. When `N > 0` a set of indicator columns
#' `..._weekend_lag_1`, ..., `..._weekend_lag_N` is created, where
#' `..._weekend_lag_k` flags dates that fall exactly `k` **working days** after a
#' weekend (again skipping weekends and holidays). Use it to capture a rise in
#' cases on the first working day(s) after a weekend.
#' @param seasons Vector. Either `integer(0)` (no seasonal effects) or a positive-numeric
#' vector where each entry is the number of seasons (cycles) to model. The actual Fourier
#' period for the i-th entry is `seasons[i] * season_length[i]`.
#' @param season_length Either a single positive number or a vector of the same length as
#' `seasons`. Specifies the duration (in data units) of each season cycle. Defaults to `1`,
#' meaning the period equals `seasons` directly.
#' Use a value greater than 1 when the data unit is finer than the season.
#' For example, to model 52-week annual seasonality in **daily** data set
#' `seasons = 52, season_length = 7` (period = 364 days).
#' @param holidays Either `NULL` or an [`almanac::rcalendar()`] specifying how to calculate holidays.
#'
#' @details
#' US Federal holidays can be passed by providing the [`almanac::cal_us_federal()`] calendar.
#'
#' Example:
#'
#' ```{r, eval=FALSE}
#' library(almanac)
#' temporal_effects(holidays = cal_us_federal())
#' ```
#'
#' @return
#' An object of class `temporal_effects`.
#'
#' @examples
#' temporal_effects(day_of_week = TRUE, week_of_year = TRUE)
#'
#' # Annual seasonality in weekly data (period = 52 weeks)
#' temporal_effects(seasons = 52)
#'
#' # Annual seasonality in daily data (52 weeks x 7 days = 364-day period)
#' temporal_effects(seasons = 52, season_length = 7)
#'
#' # After-weekend effect: flag the first two working days after a weekend
#' temporal_effects(weekend = TRUE, weekend_lags = 2)
#'
#' if (rlang::is_installed("almanac")) {
#'   cal <- almanac::rcalendar(almanac::hol_christmas())
#'   temporal_effects(holidays = cal, day_of_month = TRUE, seasons = c(7, 365))
#'
#'   # After-holiday effect: flag the first 3 working days back after a holiday
#'   temporal_effects(holidays = cal, holiday_lags = 3)
#' }
#' @md
#' @export
temporal_effects <- S7::new_class(
  "temporal_effects",
  constructor = function(
    day_of_week = FALSE,
    weekend = FALSE,
    day_of_month = FALSE,
    month_of_year = FALSE,
    week_of_year = FALSE,
    holiday_lags = 0,
    weekend_lags = 0,
    seasons = integer(0),
    season_length = 1,
    holidays = NULL
  ) {
    # --- Validation checks ---
    check_bool(day_of_week, "day_of_week")
    check_bool(weekend, "weekend")
    check_bool(day_of_month, "day_of_month")
    check_bool(month_of_year, "month_of_year")
    check_bool(week_of_year, "week_of_year")

    # after-holiday / after-weekend lag depths: single non-negative integer each
    holiday_lags <- .check_lag_depth(holiday_lags, "holiday_lags")
    weekend_lags <- .check_lag_depth(weekend_lags, "weekend_lags")

    # Validate and normalise seasons + season_length
    if (length(seasons) > 0) {
      if (!is.numeric(seasons) || any(seasons <= 0)) {
        cli::cli_abort("{.arg seasons} must be a vector of positive numbers.")
      }
      if (!is.numeric(season_length) || length(season_length) == 0 || any(season_length <= 0)) {
        cli::cli_abort("{.arg season_length} must be a positive number or a vector of positive numbers.")
      }
      if (length(season_length) != 1L && length(season_length) != length(seasons)) {
        cli::cli_abort(
          "{.arg season_length} must be length 1 or the same length as {.arg seasons} ({length(seasons)})."
        )
      }
      # Recycle season_length to match seasons, then deduplicate by computed period
      season_length <- rep_len(as.numeric(season_length), length(seasons))
      seasons <- as.numeric(seasons)
      periods <- seasons * season_length
      keep <- !duplicated(periods)
      seasons <- seasons[keep]
      season_length <- season_length[keep]
    } else {
      seasons <- numeric(0)
      season_length <- numeric(0)
    }

    # Holidays must be NULL or almanac_rcalendar
    if (!is.null(holidays) && !inherits(holidays, "almanac_rcalendar")) {
      cli::cli_abort("Invalid {.arg holidays}. Must be an {.code almanac::rcalendar()} object or NULL.")
    }

    # The after-holiday effect needs a holidays calendar to know where to start.
    if (holiday_lags > 0 && is.null(holidays)) {
      cli::cli_abort(c(
        "{.arg holiday_lags} is {.val {holiday_lags}} but no {.arg holidays} \\
         calendar was supplied.",
        "i" = "Pass an {.code almanac::rcalendar()} via {.arg holidays} so the \\
               after-holiday effect knows which days are holidays."
      ))
    }

    # Construct and return the object
    S7::new_object(
      S7::S7_object(),
      day_of_week   = day_of_week,
      weekend       = weekend,
      day_of_month  = day_of_month,
      month_of_year = month_of_year,
      week_of_year  = week_of_year,
      holiday_lags  = holiday_lags,
      weekend_lags  = weekend_lags,
      seasons       = seasons,
      season_length = season_length,
      holidays      = holidays
    )
  },
  properties = list(
    day_of_week   = S7::class_logical,
    weekend       = S7::class_logical,
    day_of_month  = S7::class_logical,
    month_of_year = S7::class_logical,
    week_of_year  = S7::class_logical,
    holiday_lags  = S7::class_numeric,
    weekend_lags  = S7::class_numeric,
    seasons       = S7::class_numeric,
    season_length = S7::class_numeric,
    holidays      = S7::class_any # to allow NULL or almanac_rcalendar
  )
)

#' Human-readable label for an after-holiday / after-weekend lag depth
#'
#' @param depth A single non-negative integer lag depth.
#'
#' @return A string such as `"first working day"` or `"first 3 working days"`.
#'
#' @keywords internal
#' @noRd
.lag_range_label <- function(depth) {
  if (depth == 1) {
    "first working day"
  } else {
    paste0("first ", depth, " working days")
  }
}

#' Validate a single non-negative integer lag depth
#'
#' @param x The value supplied for `holiday_lags` / `weekend_lags`.
#' @param name The argument name (for the error message).
#'
#' @return The validated depth as a single non-negative integer.
#'
#' @keywords internal
#' @noRd
.check_lag_depth <- function(x, name) {
  if (is.null(x) || length(x) == 0) {
    return(0L)
  }
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 0 ||
      x != as.integer(x)) {
    cli::cli_abort(
      "{.arg {name}} must be a single non-negative integer (the lag depth)."
    )
  }
  as.integer(x)
}

#' Print temporal effects
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Print function for printing the a [temporal_effects()].
#'
#' @param x A temporal_effects object created with [temporal_effects()]
#' @param ... Additional arguments to pass to print.
#'
#' @return The `temporal_effects` object `x`, invisibly. Called for the
#' side effect of printing.
#'
#' @examples
#'
#' print(temporal_effects(day_of_week = TRUE, week_of_year = TRUE))
#'
#' print(temporal_effects(day_of_week = FALSE, week_of_year = FALSE))
#'
#' print(temporal_effects(day_of_week = FALSE, week_of_year = FALSE, seasons = 52))
#'
#' print(temporal_effects(seasons = 52, season_length = 7))
#' @name print
NULL

#' @name print
#' @export
S7::method(print, temporal_effects) <- function(x, ...) {
  # Only the logical (on/off) properties are shown as bare bullets; the numeric
  # lag depths, seasons and holidays are reported separately below.
  logical_props <- Filter(
    function(v) is.logical(v),
    S7::props(x)[!names(S7::props(x)) %in% c("holidays", "seasons", "season_length",
                                             "holiday_lags", "weekend_lags")]
  )
  effects_considered <- names(Filter(isTRUE, logical_props))
  has_holidays <- !is.null(x@holidays)
  has_seasons <- (length(x@seasons) > 0)
  has_holiday_lags <- x@holiday_lags > 0
  has_weekend_lags <- x@weekend_lags > 0

  cli::cli_h1("Temporal Effects")

  if (length(effects_considered) + has_holidays + has_seasons +
        has_holiday_lags + has_weekend_lags > 0) {
    cli::cli_text("The following effects are in place:")
    cli::cli_ul()
    for (eff in effects_considered) {
      cli::cli_li("{.val {eff}}")
    }

    if (has_holiday_lags) {
      holiday_lag_label <- .lag_range_label(x@holiday_lags)
      cli::cli_li("{.val after-holiday} effect: {.emph {holiday_lag_label}}")
    }

    if (has_weekend_lags) {
      weekend_lag_label <- .lag_range_label(x@weekend_lags)
      cli::cli_li("{.val after-weekend} effect: {.emph {weekend_lag_label}}")
    }

    if (has_seasons) {
      # Show periods; include season_length breakdown when it differs from 1
      periods <- x@seasons * x@season_length
      all_unit <- all(x@season_length == 1)
      season_strs <- if (all_unit) {
        as.character(periods)
      } else {
        mapply(function(s, l, p) {
          if (l == 1) as.character(p) else paste0(s, "*", l, "=", p)
        }, x@seasons, x@season_length, periods)
      }
      cli::cli_li("{.val season} periods: {paste(season_strs, collapse = ', ')}")
    }

    if (has_holidays) {
      cli::cli_li("{.val holidays}:")
      if (!is.null(x@holidays$names)) {
        cli::cli_ol()
        cli::cli_li("{.emph {x@holidays$names}}")
        cli::cli_end()
      }
    }
    cli::cli_end()
  } else {
    cli::cli_text("No temporal effects are considered.")
  }

  invisible(x)
}
