#' Temporal Effects Class
#'
#' @description
#' The `temporal_effects` class specifies which temporal covariates or effects
#' should be included in a nowcasting model (e.g., day of week, month, holidays, etc.).
#'
#' @param day_of_week Logical. Whether to include an effect for each of the seven days of the week.
#' @param weekend Logical. Whether to include an effect for the weekend vs the weekday.
#' @param day_of_month Logical. Whether to include an effect for the day of the month (1 to 31).
#' @param month_of_year Logical. Whether to include an effect for the month of the year.
#' @param week_of_year Logical. Whether to include an effect for the epidemiological week.
#' @param seasons Vector. Either `integer(0)` or a vector where each entry is the length of the
#' seasons included in the model.
#' @param holidays Either `NULL` or an [`almanac::rcalendar()`] specifying how to calculate holidays.
#'
#' @details
#' US Federal holidays can be passed by providing the [`almanac::cal_us_federal()`] calendar.
#'
#' Example:
#' ```r
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
#' if (rlang::is_installed("almanac")) {
#'   cal <- almanac::rcalendar(almanac::hol_christmas())
#'   temporal_effects(holidays = cal, day_of_month = TRUE, seasons = c(7, 365))
#' }
#'
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
    holidays = NULL
  ) {

    # --- Validation checks ---
    check_bool(day_of_week, "day_of_week")
    check_bool(weekend, "weekend")
    check_bool(day_of_month, "day_of_month")
    check_bool(month_of_year, "month_of_year")
    check_bool(week_of_year, "week_of_year")

    #Check seasons
    seasons <- unique(seasons)

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
    holidays      = S7::class_any      #to allow NULL or almanac_rcalendar
  )
)

#' Print temporal effects
#'
#' Print function for printing the a [temporal_effects()].
#'
#' @param x A temporal_effects object created with [temporal_effects()]
#' @param ... Additional arguments to pass to print.
#' @examples
#' print(temporal_effects(day_of_week = TRUE, week_of_year = TRUE))
#' print(temporal_effects(day_of_week = FALSE, week_of_year = FALSE))
#' print(temporal_effects(day_of_week = FALSE, week_of_year = FALSE, seasons = 52))
#' @export
#' @name print
#' @rdname print
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
      cli::cli_li("{.val season} lengths: {x@seasons}")
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

