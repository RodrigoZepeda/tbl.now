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
#' @param holiday_lags Single integer (default `0`). Signed depth `N` of the
#' *holiday* lag effect; `holidays` must be supplied whenever `N != 0`.
#'
#' When `N > 0` the effect is placed **after** the holiday: indicator columns
#' `..._holiday_lag_1`, ..., `..._holiday_lag_N` are created, where
#' `..._holiday_lag_k` flags dates that fall exactly `k` **working days** after a
#' holiday. Working days skip weekends (see `weekend_days` in
#' [add_temporal_effects()]) and other holidays, so the effect lands on the first
#' day back at work. Use it to capture a rise in cases just after a holiday.
#'
#' When `N < 0` the effect is placed **before** the holiday instead: columns
#' `..._holiday_lead_1`, ..., `..._holiday_lead_|N|` are created, where
#' `..._holiday_lead_k` flags dates that fall exactly `k` working days *before* a
#' holiday. So `holiday_lags = -1` flags Christmas Eve, and `holiday_lags = -3`
#' flags the last three working days leading up to Christmas.
#'
#' @param weekend_lags Single integer (default `0`). Signed depth `N` of the
#' *weekend* lag effect, mirroring `holiday_lags` but resetting on weekend days
#' rather than holidays (and needing no calendar).
#'
#' When `N > 0`, columns `..._weekend_lag_1`, ..., `..._weekend_lag_N` flag dates
#' exactly `k` working days after a weekend, so with Sat/Sun weekends
#' `weekend_lags = 1` flags the Monday. When `N < 0`, columns
#' `..._weekend_lead_1`, ..., `..._weekend_lead_|N|` flag dates `k` working days
#' before a weekend instead: `weekend_lags = -1` flags the Friday, and
#' `weekend_lags = -3` flags the Wednesday, Thursday and Friday.
#'
#' To model both sides of the same break, add two specifications (see the
#' examples).
#'
#' @param seasons Vector. Either `integer(0)` (no seasonal effects) or a positive-numeric
#' vector where each entry is the number of seasons (cycles) to model. The actual Fourier
#' period for the i-th entry is `seasons[i] * season_length[i]`.
#'
#' @param season_length Either a single positive number or a vector of the same length as
#' `seasons`. Specifies the duration (in data units) of each season cycle. Defaults to `1`,
#' meaning the period equals `seasons` directly.
#'
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
#' @section Using a different holiday calendar:
#'
#' `holidays` accepts **any** [`almanac::rcalendar()`].
#'
#' A calendar is a set of *recurrence rules*, so you describe how
#' a holiday is constructed . say "the fourth Thursday of November", and
#'  \pkg{almanac} generates it for every year. In general, you should avoid
#'  hardcoding specific dates (like "18/11/2021").
#'
#' A calendar has four building blocks:
#'
#' * **Built-in holidays.** \pkg{almanac} ships rules for common US holidays:
#'   `hol_us_thanksgiving()`, `hol_us_memorial_day()`, `hol_christmas()`,
#'   `hol_us_election_day()`, and so on. See [`almanac::rholiday()`] for the list.
#'
#' * **Your own holidays.** Anything without a built-in rule is a `yearly()`
#'   recurrence narrowed with `recur_on_*()` and named with
#'   [`almanac::rholiday()`].
#'
#' * **Observance.** [`almanac::hol_observe()`] shifts a fixed-date holiday that
#'   lands on a weekend onto a working day. `adjustment = adj_nearest` gives the
#'   usual US rule (Saturday moves back to Friday, Sunday forward to Monday);
#'   `adj_following` and `adj_preceding` always move one way.
#'
#' * **Editing a calendar.** [`almanac::cal_add()`] and
#'   [`almanac::cal_remove()`] tweak an existing calendar, and
#'   [`almanac::cal_names()`] lists what is in one.
#'
#' Use [`almanac::cal_events()`] to check what you built before modelling with
#' it.
#'
#' ## Worked example: the New York City calendar
#'
#' NYC observes the US federal holidays plus Lincoln's Birthday and Election Day,
#' and calls the October holiday Columbus Day. Only Lincoln's Birthday needs a
#' hand-written rule; everything else is built-in, with `hol_observe()` on the
#' fixed-date holidays.
#'
#' ```{r, eval=FALSE}
#' library(almanac)
#'
#' cal_nyc <- function(since = NULL, until = NULL) {
#'
#'   #Adjust if a holiday happens on a weekend move to the closest date
#'   #i.e. 4th of July on Saturday in 2026 moves to Friday July 3rd
#'   on_weekends <- recur_on_weekends(weekly(since = since, until = until))
#'   observed <- function(x) {
#'     hol_observe(x, adjust_on = on_weekends, adjustment = adj_nearest)
#'   }
#'
#'   # Build a rule for Lincoln's birthday: February 12th, every year.
#'   lincolns_birthday <- yearly(since = since, until = until) |>
#'     recur_on_month_of_year("February") |>
#'     recur_on_day_of_month(12L) |>
#'     rholiday(name = "Lincoln's Birthday")
#'
#'   rcalendar(
#'     #New years day moves to closest weekday
#'     observed(hol_new_years_day(since = since, until = until)),
#'     #MLK day happens that day
#'     hol_us_martin_luther_king_junior_day(since = since, until = until),
#'     #Lincoln's birthday moves to closest weekday
#'     observed(lincolns_birthday),
#'     #President's day happens that day
#'     hol_us_presidents_day(since = since, until = until),
#'     #Memorials day happens that day
#'     hol_us_memorial_day(since = since, until = until),
#'     #Juneteenth is moved to closest weekday
#'     observed(hol_us_juneteenth(since = since, until = until)),
#'     #4th of July is moved to closest weekday
#'     observed(hol_us_independence_day(since = since, until = until)),
#'     #Labor day happens that specific day
#'     hol_us_labor_day(since = since, until = until),
#'     #We can rename what almanac names Indigenous People's day to Columbus
#'     hol_rename(
#'       hol_us_indigenous_peoples_day(since = since, until = until),
#'       "Columbus Day"
#'     ),
#'     #Election day
#'     hol_us_election_day(since = since, until = until),
#'     #Veteran's day moves closest
#'     observed(hol_us_veterans_day(since = since, until = until)),
#'     #Thanksgiving happens that specific Thursday
#'     hol_us_thanksgiving(since = since, until = until),
#'     #Christmas moves to closest day
#'     observed(hol_christmas(since = since, until = until))
#'   )
#' }
#'
#' # Check it before using it. The same rules generate any year you ask for:
#' cal_events(cal_nyc(), year = 2026, observed = TRUE)
#' cal_events(cal_nyc(), year = 2027, observed = TRUE)
#'
#' # Then hand it to temporal_effects() like any other calendar:
#' temporal_effects(holidays = cal_nyc())
#' ```
#'
#' Two of those show the rules we implemented:
#'
#' * **Independence Day is Jul 3, not Jul 4.** Jul 4 2026 is a Saturday, so
#'   `adj_nearest` moves the observance *back* to Friday Jul 3. In 2027 it lands on a
#'   Sunday and moves *forward* to Mon Jul 5.
#' * **Christmas 2027 is observed on Fri Dec 24**, and New Year's Day 2028 is
#'   pulled back to Fri Dec 31 2027 — so it appears in the 2027 events, not 2028.
#'
#' **Note** NYC's Lincoln's Birthday is a floating holiday so
#' consider removing from here.
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
#' # Before-weekend effect: flag the last working day before a weekend (Friday)
#' temporal_effects(weekend = TRUE, weekend_lags = -1)
#'
#' if (rlang::is_installed("almanac")) {
#'   cal <- almanac::rcalendar(almanac::hol_christmas())
#'   temporal_effects(holidays = cal, day_of_month = TRUE, seasons = c(7, 365))
#'
#'   # After-holiday effect: flag the first 3 working days back after a holiday
#'   temporal_effects(holidays = cal, holiday_lags = 3)
#'
#'   # Before-holiday effect: flag the 2 working days leading up to a holiday
#'   temporal_effects(holidays = cal, holiday_lags = -2)
#'
#'   # A calendar of your own: write a rule for the holiday, not a date, and
#'   # almanac generates it for every year (see "Using a different holiday
#'   # calendar" above for a full local calendar).
#'   lincolns_birthday <- almanac::yearly() |>
#'     almanac::recur_on_month_of_year("February") |>
#'     almanac::recur_on_day_of_month(12L) |>
#'     almanac::rholiday(name = "Lincoln's Birthday")
#'
#'   # Add it to the federal calendar and check what you built
#'   cal_local <- almanac::cal_add(almanac::cal_us_federal(), lincolns_birthday)
#'   almanac::cal_events(cal_local, year = 2026, observed = TRUE)
#'
#'   temporal_effects(holidays = cal_local, holiday_lags = 2)
#'
#'   # Both sides of the holiday: add one specification per direction
#'   data(denguedat)
#'   tbl_now(denguedat,
#'     event_date = onset_week, report_date = report_week, verbose = FALSE
#'   ) |>
#'     add_temporal_effects(temporal_effects(holidays = cal, holiday_lags = -2)) |>
#'     add_temporal_effects(temporal_effects(holidays = cal, holiday_lags = 2))
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

    # holiday / weekend lag depths: single signed integer each
    # (positive = after the event, negative = before it)
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

    # The holiday lag effect needs a holidays calendar to know where to start.
    if (holiday_lags != 0 && is.null(holidays)) {
      cli::cli_abort(c(
        "{.arg holiday_lags} is {.val {holiday_lags}} but no {.arg holidays} \\
         calendar was supplied.",
        "i" = "Pass an {.code almanac::rcalendar()} via {.arg holidays} so the \\
               holiday lag effect knows which days are holidays."
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

#' Human-readable label for a signed holiday / weekend lag depth
#'
#' @param depth A single non-zero integer lag depth (negative = before the
#'   event).
#'
#' @return A string such as `"first working day"`, `"first 3 working days"` or
#'   `"last 3 working days"`.
#'
#' @keywords internal
#' @noRd
.lag_range_label <- function(depth) {
  # Positive depths count forwards from the event, negative ones backwards
  # towards it, so the run they describe is the first / last of the working days.
  edge <- if (depth < 0) "last" else "first"
  if (abs(depth) == 1) {
    paste0(edge, " working day")
  } else {
    paste0(edge, " ", abs(depth), " working days")
  }
}

#' Direction label for a signed holiday / weekend lag depth
#'
#' @param depth A single non-zero integer lag depth.
#' @param event `"holiday"` or `"weekend"`.
#'
#' @return A string such as `"after-weekend"` or `"before-holiday"`.
#'
#' @keywords internal
#' @noRd
.lag_direction_label <- function(depth, event) {
  paste0(if (depth < 0) "before-" else "after-", event)
}

#' Validate a single signed integer lag depth
#'
#' @param x The value supplied for `holiday_lags` / `weekend_lags`.
#' @param name The argument name (for the error message).
#'
#' @return The validated depth as a single integer; the sign records the
#'   direction (negative = before the event) and `0` means "off".
#'
#' @keywords internal
#' @noRd
.check_lag_depth <- function(x, name) {
  if (is.null(x) || length(x) == 0) {
    return(0L)
  }
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x != as.integer(x)) {
    cli::cli_abort(c(
      "{.arg {name}} must be a single integer (the lag depth).",
      "i" = "Use a positive depth for an effect {.emph after} the event and a \\
             negative one for an effect {.emph before} it."
    ))
  }
  as.integer(x)
}

#' Print a [temporal_effects()] specification
#'
#' Registered as an S7 method by [S7::methods_register()] in `.onLoad()`, so it
#' needs no NAMESPACE entry of its own (the previous `@export` here produced
#' none). Deliberately `@noRd`: giving it a help page produced an Rd aliased to
#' the bare name `print`, which both squatted the base generic's `?print` topic
#' and tripped CRAN's "examples for unexported functions" check.
#'
#' @param x A temporal_effects object created with [temporal_effects()]
#' @param ... Additional arguments to pass to print.
#'
#' @return The `temporal_effects` object `x`, invisibly. Called for the
#'   side effect of printing.
#'
#' @keywords internal
#' @noRd
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
  has_holiday_lags <- x@holiday_lags != 0
  has_weekend_lags <- x@weekend_lags != 0

  # stdout (`cat_*`), not messages (`cli_*`): a print method's output has to
  # survive `message = FALSE`, `sink()` and `capture.output()`.
  cli::cat_rule(left = "Temporal Effects")

  if (length(effects_considered) + has_holidays + has_seasons +
        has_holiday_lags + has_weekend_lags > 0) {
    cli::cat_line("The following effects are in place:")

    bullets <- vapply(
      effects_considered,
      function(eff) cli::format_inline("{.val {eff}}"),
      character(1),
      USE.NAMES = FALSE
    )

    if (has_holiday_lags) {
      holiday_lag_dir <- .lag_direction_label(x@holiday_lags, "holiday")
      holiday_lag_label <- .lag_range_label(x@holiday_lags)
      bullets <- c(bullets, cli::format_inline(
        "{.val {holiday_lag_dir}} effect: {.emph {holiday_lag_label}}"
      ))
    }

    if (has_weekend_lags) {
      weekend_lag_dir <- .lag_direction_label(x@weekend_lags, "weekend")
      weekend_lag_label <- .lag_range_label(x@weekend_lags)
      bullets <- c(bullets, cli::format_inline(
        "{.val {weekend_lag_dir}} effect: {.emph {weekend_lag_label}}"
      ))
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
      bullets <- c(bullets, cli::format_inline(
        "{.val season} periods: {paste(season_strs, collapse = ', ')}"
      ))
    }

    if (has_holidays) {
      bullets <- c(bullets, cli::format_inline("{.val holidays}:"))
    }

    cli::cat_bullet(bullets)

    if (has_holidays && !is.null(x@holidays$names)) {
      cli::cat_line(cli::format_inline("    {.emph {x@holidays$names}}"))
    }
  } else {
    cli::cat_line("No temporal effects are considered.")
  }

  invisible(x)
}
