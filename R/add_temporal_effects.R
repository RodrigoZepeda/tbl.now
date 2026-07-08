#' Add temporal effect coding to a `tbl_now`
#'
#' @description `r lifecycle::badge("stable")`
#'
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
#' @param t_effects A [temporal_effects()] object codifying the
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
#'   event_date = "onset_week",
#'   report_date = "report_week",
#'   strata = "gender"
#' )
#'
#' # Add an effect for epidemiological week
#' disease_data <- disease_data |>
#'   add_temporal_effects(t_effects = temporal_effects(week_of_year = TRUE))
#'
#' # Use the compute to calculate them
#' disease_data |> compute_temporal_effects()
#'
#' # Use replace to change them
#' disease_data |>
#'   replace_temporal_effects(t_effects = temporal_effects(seasons = 52))
#'
#' # Use remove to delete them
#' disease_data |> remove_temporal_effects()
#' @name add_temporal_effects
#'
#' @seealso [temporal_effects()] [add] [remove] [change]
#'
#' @export
add_temporal_effects <- function(x, t_effects = NULL, overwrite = FALSE, ...) {
  UseMethod("add_temporal_effects")
}

#' Working days elapsed since the most recent "reset" event
#'
#' Powers the *after-holiday* / *after-weekend* lag effects. For each date it
#' counts how many **working days** have passed since the last reset event
#' (a holiday or a weekend day), skipping weekends and holidays so the count
#' lands on the first day back at work. The reset day itself, and any
#' non-working day, get `NA` (they carry no after-effect indicator).
#'
#' @param dates A vector of `<Date>` values (may contain duplicates or gaps).
#' @param reset_event Which day resets the counter: `"holiday"` or `"weekend"`.
#' @param weekend_days Passed to [is_weekday()].
#' @param holidays An [almanac::rcalendar()] or `NULL`.
#'
#' @return An integer vector, the same length as `dates`, giving the number of
#'   working days since the last reset event (`NA` where undefined or where the
#'   date itself is not a working day).
#'
#' @keywords internal
#' @noRd
.working_days_since <- function(dates, reset_event, weekend_days, holidays) {
  valid <- dates[!is.na(dates)]
  if (length(valid) == 0) {
    return(rep(NA_integer_, length(dates)))
  }

  # Build a continuous daily grid so working days can be counted even when the
  # data itself has gaps (weekly data, missing days, ...).
  grid <- seq(min(valid), max(valid), by = "day")

  is_weekend <- !is_weekday(grid, weekend_days = weekend_days)
  is_holiday <- if (!is.null(holidays)) {
    almanac::alma_in(grid, holidays)
  } else {
    rep(FALSE, length(grid))
  }
  is_working <- !is_weekend & !is_holiday
  is_reset   <- if (reset_event == "holiday") is_holiday else is_weekend

  counter <- rep(NA_integer_, length(grid))
  running <- NA_integer_
  for (i in seq_along(grid)) {
    if (is_reset[i]) {
      # The reset day starts a new count; it is not itself "after" the event.
      running <- 0L
    } else if (is_working[i]) {
      if (!is.na(running)) running <- running + 1L
      counter[i] <- running
    }
    # Non-working, non-reset days (e.g. a holiday while counting after a
    # weekend) carry `running` forward without incrementing and keep `NA`.
  }

  counter[match(dates, grid)]
}

#' @export
#' @rdname add_temporal_effects
add_temporal_effects.data.frame <- function(x, t_effects = NULL, overwrite = FALSE,
                                            ...,
                                            date_col = NULL,
                                            numeric_col = NULL,
                                            name_prefix = paste0(".", date_col),
                                            weekend_days = c("Sat", "Sun")) {
  # Do nothing if no effect
  if (is.null(t_effects)) {
    return(x)
  }

  # Check the class
  if (!S7::S7_inherits(t_effects, class = temporal_effects)) {
    cli::cli_abort(
      "`t_effects` should be a {.code temporal_effects} object. Use `temporal_effects` to create it."
    )
  }

  # Check the column belongs to x.frame
  if (!is.null(date_col) && !(date_col %in% colnames(x))) {
    cli::cli_abort(
      "Column {.value {date_col}} is not a column in `x`"
    )
  }

  if (!is.null(numeric_col) && !(numeric_col %in% colnames(x))) {
    cli::cli_abort(
      "Column {.value {numeric_col}} is not a column in `x`"
    )
  }

  if (is.null(date_col)) {
    cli::cli_abort(
      "Please specify a `date_col`"
    )
  }

  if (!is.null(t_effects@seasons) && length(t_effects@seasons) > 0 && is.null(numeric_col)) {
    cli::cli_abort("Please specify a `numeric_col` to calculate the seasons")
  }


  # Get the initial date (this is for codifying the month and epiweek effects so that they start in 1)
  init_date <- x |>
    dplyr::slice_head(n = 1) |>
    dplyr::pull(!!as.symbol(date_col))

  # Add day of the week effect-----
  if (!is.null(t_effects)) {
    if (t_effects@"day_of_week" & !is.null(date_col)) {
      if (paste0(name_prefix, "_day_of_week") %in% colnames(x) && !overwrite) {
        cli::cli_abort(
          "Column {.val {name_prefix}_day_of_week} already exists in data. Set `overwrite = TRUE` to overwrite it."
        )
      }

      x <- x |>
        dplyr::mutate(!!as.symbol(paste0(name_prefix, "_day_of_week")) :=
          as.integer(lubridate::wday(!!as.symbol(date_col))))
    }

    # Add weekend effect-----
    if (t_effects@weekend & !is.null(date_col)) {
      if (paste0(name_prefix, "_weekend") %in% colnames(x) && !overwrite) {
        cli::cli_abort(
          "Column {.val {name_prefix}_weekend} already exists in data. Set `overwrite = TRUE` to overwrite it."
        )
      }

      x <- x |>
        dplyr::mutate(!!as.symbol(paste0(name_prefix, "_weekend")) :=
          as.integer(!is_weekday(!!as.symbol(date_col), weekend_days = weekend_days)))
    }

    # Add day of the month effect-----
    if (t_effects@day_of_month & !is.null(date_col)) {
      if (paste0(name_prefix, "_day_of_month") %in% colnames(x) && !overwrite) {
        cli::cli_abort(
          "Column {.val {name_prefix}_day_of_month} already exists in data. Set `overwrite = TRUE` to overwrite it."
        )
      }

      x <- x |>
        dplyr::mutate(!!as.symbol(paste0(name_prefix, "_day_of_month")) :=
          as.integer(lubridate::day(!!as.symbol(date_col))))
    }

    # Add month effect (centered at current month = 1)-----
    if (t_effects@month_of_year & !is.null(date_col)) {
      if (paste0(name_prefix, "_month_of_year") %in% colnames(x) && !overwrite) {
        cli::cli_abort(
          "Column {.val {name_prefix}_month_of_year} already exists in data. Set `overwrite = TRUE` to overwrite it."
        )
      }

      x <- x |>
        dplyr::mutate(!!as.symbol(paste0(name_prefix, "_month_of_year")) :=
          as.integer(1 +
            ((lubridate::month(!!as.symbol(date_col)) - lubridate::month(init_date)) %% 12)))
    }

    # Add epiweek effect (centered at current week = 1 | week 53 that almost never happens is collapsed to January)-----
    if (t_effects@week_of_year & !is.null(date_col)) {
      if (paste0(name_prefix, "_week_of_year") %in% colnames(x) && !overwrite) {
        cli::cli_abort(
          "Column {.val {name_prefix}_week_of_year} already exists in data. Set `overwrite = TRUE` to overwrite it."
        )
      }

      x <- x |>
        dplyr::mutate(!!as.symbol(paste0(name_prefix, "_week_of_year")) :=
          as.integer(1 +
            ((lubridate::epiweek(!!as.symbol(date_col)) - lubridate::epiweek(init_date)) %% 52)))
    }

    # Add seasons-----
    if (!is.null(t_effects@seasons) & length(t_effects@seasons) > 0 & !is.null(numeric_col)) {
      # For each season add a pair of Fourier (sin/cos) columns
      for (i in seq_along(t_effects@seasons)) {
        # Period = seasons * season_length (season_length defaults to 1 → period = seasons)
        period <- t_effects@seasons[i] * t_effects@season_length[i]

        # Create column name from the actual period
        season_name <- paste0(name_prefix, "_season_", period)

        if ((paste0(season_name, "_cos") %in% colnames(x)) || (paste0(season_name, "_sin") %in% colnames(x)) && !overwrite) {
          cli::cli_abort(
            "At least one of the columns: {.val {season_name}_cos} or {.val {season_name}_sin} already exist in data. Set `overwrite = TRUE` to overwrite them."
          )
        }

        # Add the sine and cosine components of the Fourier term
        x <- x |>
          dplyr::mutate(
            !!as.symbol(paste0(season_name, "_cos")) := cos(2 * base::pi * as.numeric(!!as.symbol(numeric_col)) / period),
            !!as.symbol(paste0(season_name, "_sin")) := sin(2 * base::pi * as.numeric(!!as.symbol(numeric_col)) / period)
          )
      }
    }


    # Add holiday effect-----
    if (!is.null(t_effects@holidays) & length(t_effects@holidays) > 0 & !is.null(date_col)) {
      # Check almanac installation
      if (!rlang::is_installed("almanac")) {
        cli::cli_abort(
          "Please install the `almanac` package to be able to integrate `holiday` effects"
        )
      } else {
        if (paste0(name_prefix, "_holiday") %in% colnames(x) && !overwrite) {
          cli::cli_abort(
            "Column {.val {name_prefix}_holiday} already exists in data. Set `overwrite = TRUE` to overwrite it."
          )
        }

        x <- x |>
          dplyr::mutate(!!as.symbol(paste0(name_prefix, "_holiday")) :=
            as.integer(
              almanac::alma_in(!!as.symbol(date_col), t_effects@holidays)
            ))
      }
    }

    # Add after-holiday lag effects-----
    # Indicator per lag: `_holiday_lag_k` == 1 on the k-th working day after a
    # holiday (weekends and holidays are skipped when counting).
    if (isTRUE(t_effects@holiday_lags > 0) && !is.null(date_col)) {
      if (!rlang::is_installed("almanac")) {
        cli::cli_abort(
          "Please install the `almanac` package to be able to integrate after-holiday effects"
        )
      }
      x <- .add_after_lag_columns(
        x,
        date_col     = date_col,
        depth        = t_effects@holiday_lags,
        base_name    = paste0(name_prefix, "_holiday_lag"),
        reset_event  = "holiday",
        weekend_days = weekend_days,
        holidays     = t_effects@holidays,
        overwrite    = overwrite
      )
    }

    # Add after-weekend lag effects-----
    # Indicator per lag: `_weekend_lag_k` == 1 on the k-th working day after a
    # weekend (weekends and holidays are skipped when counting).
    if (isTRUE(t_effects@weekend_lags > 0) && !is.null(date_col)) {
      x <- .add_after_lag_columns(
        x,
        date_col     = date_col,
        depth        = t_effects@weekend_lags,
        base_name    = paste0(name_prefix, "_weekend_lag"),
        reset_event  = "weekend",
        weekend_days = weekend_days,
        holidays     = t_effects@holidays,
        overwrite    = overwrite
      )
    }
  }

  return(x)
}

#' Append the per-lag indicator columns for an after-holiday / after-weekend effect
#'
#' @param x A data frame.
#' @param date_col Name of the `<Date>` column to derive the effect from.
#' @param depth Number of lags `N`; columns `<base_name>_1` ... `<base_name>_N`
#'   are created.
#' @param base_name Column-name stem, e.g. `".event_holiday_lag"`.
#' @param reset_event `"holiday"` or `"weekend"`, passed to
#'   [.working_days_since()].
#' @param weekend_days Passed to [is_weekday()].
#' @param holidays An [almanac::rcalendar()] or `NULL`.
#' @param overwrite Whether to overwrite pre-existing columns.
#'
#' @return `x` with the `depth` indicator columns appended.
#'
#' @keywords internal
#' @noRd
.add_after_lag_columns <- function(x, date_col, depth, base_name, reset_event,
                                   weekend_days, holidays, overwrite) {
  new_cols <- paste0(base_name, "_", seq_len(depth))
  clash <- new_cols[new_cols %in% colnames(x)]
  if (length(clash) > 0 && !overwrite) {
    cli::cli_abort(
      "Column{cli::qty(clash)}{?s} {.val {clash}} already exist{?s/} in data. \\
       Set `overwrite = TRUE` to overwrite {cli::qty(clash)}{?it/them}."
    )
  }

  since <- .working_days_since(
    x[[date_col]],
    reset_event  = reset_event,
    weekend_days = weekend_days,
    holidays     = holidays
  )

  for (k in seq_len(depth)) {
    x[[paste0(base_name, "_", k)]] <- as.integer(!is.na(since) & since == k)
  }
  x
}

#' @export
#' @rdname add_temporal_effects
add_temporal_effects.tbl_now <- function(x, t_effects = NULL, overwrite = FALSE, ...,
                                         date_type = "event_date",
                                         weekend_days = c("Sat", "Sun")) {
  if (is.null(t_effects)) {
    return(x)
  }

  if (!S7::S7_inherits(t_effects, class = temporal_effects)) {
    cli::cli_abort(
      "`t_effects` should be a {.code temporal_effects} object. Use `temporal_effects` to create it."
    )
  }

  if (!date_type %in% c("event_date", "report_date")) {
    cli::cli_abort("Invalid `date_type` use {.val event_date} or {.val report_date}")
  }

  # Store the spec lazily — columns are computed only when compute_temporal_effects() is called
  new_spec <- list(t_effects = t_effects, date_type = date_type, weekend_days = weekend_days)
  existing_spec <- attr(x, "temporal_effects")
  if (is.null(existing_spec)) existing_spec <- list()
  attr(x, "temporal_effects") <- c(existing_spec, list(new_spec))

  return(x)
}
