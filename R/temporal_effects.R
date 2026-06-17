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
#' if (rlang::is_installed("almanac")) {
#'   cal <- almanac::rcalendar(almanac::hol_christmas())
#'   temporal_effects(holidays = cal, day_of_month = TRUE, seasons = c(7, 365))
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
      seasons       <- as.numeric(seasons)
      periods       <- seasons * season_length
      keep          <- !duplicated(periods)
      seasons       <- seasons[keep]
      season_length <- season_length[keep]
    } else {
      seasons       <- numeric(0)
      season_length <- numeric(0)
    }

    # Holidays must be NULL or almanac_rcalendar
    if (!is.null(holidays) && !inherits(holidays, "almanac_rcalendar")) {
      cli::cli_abort("Invalid {.arg holidays}. Must be an {.code almanac::rcalendar()} object or NULL.")
    }

    # Construct and return the object
    S7::new_object(
      S7::S7_object(),
      day_of_week   = day_of_week,
      weekend       = weekend,
      day_of_month  = day_of_month,
      month_of_year = month_of_year,
      week_of_year  = week_of_year,
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
    seasons       = S7::class_numeric,
    season_length = S7::class_numeric,
    holidays      = S7::class_any      #to allow NULL or almanac_rcalendar
  )
)

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

  effects_considered <- names(Filter(isTRUE, S7::props(x)[names(S7::props(x)) != "holidays"]))
  has_holidays       <- !is.null(x@holidays)
  has_seasons        <- (length(x@seasons) > 0)

  cli::cli_h1("Temporal Effects")

  if (length(effects_considered) + has_holidays + has_seasons > 0) {
    cli::cli_text("The following effects are in place:")
    cli::cli_ul()
    for (eff in effects_considered) {
      cli::cli_li("{.val {eff}}")
    }

    if (has_seasons){
      # Show periods; include season_length breakdown when it differs from 1
      periods     <- x@seasons * x@season_length
      all_unit    <- all(x@season_length == 1)
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

