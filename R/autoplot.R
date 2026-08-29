#' Default colour palette for `autoplot.tbl_now()`
#'
#' @return A named character vector of hex colours used across the panels.
#'
#' @keywords internal
#' @noRd
.tbl_now_palette <- function() {
  c(
    primary_green  = "#5F7E62",
    light_green    = "#A8BFA9",
    accent_red     = "#B85348",
    near_black     = "#262626",
    dark_green     = "#334335",
    medium_green   = "#7A9E7E",
    light_red      = "#e78b7f",
    muted_green    = "#607060"
  )
}

#' Colours and subtitle for a panel family
#'
#' Every panel belongs to one of two processes, and the whole package uses the
#' same visual grammar for them: **red** for anything reporting-related, **green**
#' for the epidemic (event-date) process. The subtitle says which one the panel
#' describes, so a panel lifted out of the `autoplot()` grid still reads on its
#' own.
#'
#' @param process `"reporting"` or `"epidemic"`.
#' @param palette A named colour palette (see `.tbl_now_palette()`).
#'
#' @return A list with `fill`, `line` and `subtitle`.
#'
#' @keywords internal
#' @noRd
.tbl_now_process_style <- function(process, palette) {
  if (identical(process, "reporting")) {
    list(
      fill     = palette[["light_red"]],
      line     = palette[["accent_red"]],
      subtitle = "Reporting delay process"
    )
  } else {
    list(
      fill     = palette[["light_green"]],
      line     = palette[["primary_green"]],
      subtitle = "Epidemic (event-date) process"
    )
  }
}

#' Shared ggplot2 theme for the diagnostic panels
#'
#' @param palette A named colour palette (see `.tbl_now_palette()`).
#'
#' @return A ggplot2 theme object.
#'
#' @keywords internal
#' @noRd
.tbl_now_theme <- function(palette) {
  ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(face = "bold", colour = palette[["near_black"]]),
      plot.subtitle    = ggplot2::element_text(colour = palette[["muted_green"]]),
      plot.caption     = ggplot2::element_text(colour = palette[["muted_green"]], hjust = 0,
                                               size = 8, lineheight = 1.05),
      plot.caption.position = "plot",
      axis.title       = ggplot2::element_text(colour = palette[["near_black"]]),
      axis.text        = ggplot2::element_text(colour = palette[["near_black"]]),
      panel.grid.minor = ggplot2::element_blank()
    )
}

#' Multiplier converting one delay unit into days
#'
#' Used to place the incompleteness vertical line on a `Date` axis.
#'
#' @param units One of `"days"`, `"weeks"`, `"months"`, `"years"`, `"numeric"`.
#'
#' @return A numeric multiplier (days per unit).
#'
#' @keywords internal
#' @noRd
.tbl_now_units_to_days <- function(units) {
  switch(units,
    days    = 1,
    weeks   = 7,
    months  = 30.4375,
    years   = 365.25,
    numeric = 1,
    1
  )
}

#' Weighted quantile (lower index)
#'
#' Smallest value whose cumulative weight reaches `probability`.
#'
#' @param values Numeric vector of values.
#' @param weights Numeric vector of non-negative weights, same length as `values`.
#' @param probability Target cumulative probability in `(0, 1)`.
#'
#' @return The weighted quantile (a single value), or `NA_real_` if there are
#'   no positive-weight values.
#'
#' @keywords internal
#' @noRd
.tbl_now_weighted_quantile <- function(values, weights, probability) {
  keep <- !is.na(values) & !is.na(weights) & weights > 0
  values <- values[keep]
  weights <- weights[keep]
  if (length(values) == 0) {
    return(NA_real_)
  }
  ordering <- order(values)
  values <- values[ordering]
  weights <- weights[ordering]
  cumulative_weight <- cumsum(weights) / sum(weights)
  values[which(cumulative_weight >= probability)[1]]
}

#' Reporting delays weighted by case counts
#'
#' Works for linelist and count data alike (counts come from `to_count()`).
#'
#' @param object A `tbl_now` object.
#'
#' @return A tibble with columns `delay` and `weight` (positive weights only).
#'
#' @keywords internal
#' @noRd
.tbl_now_delay_distribution <- function(object) {
  incidence <- object |>
    ungroup() |>
    to_count(to = "count-incidence")
  case_count_column <- get_case_count(incidence)
  dplyr::tibble(
    delay  = incidence[[".delay"]],
    weight = incidence[[case_count_column]]
  ) |>
    dplyr::filter(!is.na(.data$delay), !is.na(.data$weight), .data$weight > 0)
}

#' Multiplicative growth of the cumulative count per delay
#'
#' For **count-cumulative** data the "delay distribution" is better read as how the
#' cumulative count *stabilises* with delay. For each event date (and stratum), and
#' each delay `d`, this is the ratio of the cumulative count at delay `d` to the
#' cumulative count at the previous observed delay: a ratio `> 1` is an upward
#' revision (more cases arrived), `< 1` a downward revision, and it converges to
#' `1` as reporting completes. On a log scale a doubling and a halving are
#' symmetric about `1`.
#'
#' @param object A `tbl_now` object (expected `count-cumulative`).
#' @param strata_cols Optional strata columns; when supplied a combined `strata`
#'   label is added and growth is computed within each stratum.
#'
#' @return A tibble with `delay`, `ratio` (and `strata` when grouped); only finite,
#'   positive ratios are kept.
#'
#' @keywords internal
#' @noRd
.tbl_now_cumulative_growth <- function(object, strata_cols = NULL) {
  cumulative <- object |>
    ungroup() |>
    to_count(to = "count-cumulative")
  count_col <- get_case_count(cumulative)
  event_col <- get_event_date(object)
  observations <- dplyr::as_tibble(cumulative)

  group_cols <- "event_date"
  if (!is.null(strata_cols)) {
    observations <- dplyr::mutate(
      observations,
      strata = .tbl_now_strata_label(observations, strata_cols)
    )
    group_cols <- c("strata", "event_date")
  }

  # Total cumulative count per (event date [, stratum], delay), summed over any
  # columns that are not strata, then the ratio to the previous delay.
  observations |>
    dplyr::rename(event_date = dplyr::all_of(event_col), delay = ".delay") |>
    dplyr::summarise(
      cumulative = sum(.data[[count_col]]),
      .by = dplyr::all_of(c(group_cols, "delay"))
    ) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c(group_cols, "delay")))) |>
    dplyr::mutate(
      ratio = .data$cumulative / dplyr::lag(.data$cumulative),
      .by = dplyr::all_of(group_cols)
    ) |>
    dplyr::filter(is.finite(.data$ratio), .data$ratio > 0)
}

#' Observed incidence per event date
#'
#' The latest reported counts per `event_date`, summed over strata.
#'
#' @param object A `tbl_now` object.
#'
#' @return A tibble with columns `event_date` and `case_count`, sorted by date.
#'
#' @keywords internal
#' @noRd
.tbl_now_epidemic_process <- function(object) {
  latest <- get_latest_reported_cases(object)
  case_count_column <- get_case_count(latest)
  event_date_column <- get_event_date(object)
  latest |>
    as.data.frame() |>
    dplyr::group_by(event_date = .data[[event_date_column]]) |>
    dplyr::summarise(
      case_count = sum(.data[[case_count_column]], na.rm = TRUE),
      .groups    = "drop"
    ) |>
    dplyr::arrange(.data$event_date)
}

#' Mean reporting delay per event date
#'
#' Collapses the object to one row per `event_date`, carrying the case-count
#' weighted mean reporting delay. Powers the delay-effect panels (delay by
#' calendar group, and the delay periodogram).
#'
#' @param object A `tbl_now` object.
#'
#' @return A tibble with columns `event_date`, `mean_delay` and `cases`, sorted
#'   by date.
#'
#' @keywords internal
#' @noRd
.tbl_now_delay_per_date <- function(object) {
  incidence <- object |>
    ungroup() |>
    to_count(to = "count-incidence")
  case_count_column <- get_case_count(incidence)
  event_date_column <- get_event_date(object)
  dplyr::tibble(
    event_date = incidence[[event_date_column]],
    delay      = incidence[[".delay"]],
    weight     = incidence[[case_count_column]]
  ) |>
    dplyr::filter(
      !is.na(.data$event_date), !is.na(.data$delay),
      !is.na(.data$weight), .data$weight > 0
    ) |>
    dplyr::group_by(.data$event_date) |>
    dplyr::summarise(
      mean_delay = stats::weighted.mean(.data$delay, .data$weight),
      cases      = sum(.data$weight),
      .groups    = "drop"
    ) |>
    dplyr::arrange(.data$event_date)
}

#' Reported cases per report date
#'
#' The reporting-side twin of `.tbl_now_epidemic_process()`: how many cases were
#' *reported* on each `report_date` (not when they occurred). Powers the
#' `measure = "percent"` version of the reporting-delay calendar panels, which
#' answer "what share of the reports arrive on a weekend?".
#'
#' @param object A `tbl_now` object.
#' @param strata_cols Optional character vector of columns to split on; when
#'   supplied the result carries a `strata` label column.
#'
#' @return A tibble with `report_date`, `case_count` (and `strata`).
#'
#' @keywords internal
#' @noRd
.tbl_now_reporting_process <- function(object, strata_cols = NULL) {
  incidence <- object |>
    ungroup() |>
    to_count(to = "count-incidence")
  case_count_column <- get_case_count(incidence)
  report_date_column <- get_report_date(object)

  plot_data <- dplyr::tibble(
    report_date = incidence[[report_date_column]],
    case_count  = incidence[[case_count_column]]
  )
  if (length(strata_cols) > 0) {
    plot_data$strata <- .tbl_now_strata_label(incidence, strata_cols)
  }

  plot_data |>
    dplyr::filter(!is.na(.data$report_date), !is.na(.data$case_count)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(
      c("report_date", if (length(strata_cols) > 0) "strata")
    ))) |>
    dplyr::summarise(
      case_count = sum(.data$case_count, na.rm = TRUE),
      .groups    = "drop"
    ) |>
    dplyr::arrange(.data$report_date)
}

#' The calendar cycle over which a `"percent"` share is computed
#'
#' A share needs a denominator: the block of time over which the calendar groups
#' add up to 100%. Each grouping has a natural one — the seven weekdays make up a
#' week, the epidemiological weeks make up a year — so a box in the panel is the
#' distribution, across blocks, of that group's share.
#'
#' @param grouping One of `"weekday"`, `"week"`, `"month"`, `"holiday"`,
#'   `"holiday_lag"`.
#'
#' @return A [lubridate::floor_date()] unit, or `NULL` for an unknown grouping.
#'
#' @keywords internal
#' @noRd
.tbl_now_percent_block_unit <- function(grouping) {
  switch(grouping,
    weekday     = "week",
    holiday     = "week",
    holiday_lag = "month",
    week        = "year",
    month       = "year",
    NULL
  )
}

#' Per-block percentage of cases falling in each calendar group
#'
#' @param data A data frame with a date column and a value column.
#' @param grouping The calendar grouping.
#' @param date_col Name of the date column to group on.
#' @param value_col Name of the (case-count) column to share out.
#' @param holiday_config A list from `.tbl_now_holiday_config()`, or `NULL`.
#' @param by_strata Whether `data` carries a `strata` column to keep.
#'
#' @return A tibble with `calendar_group`, `percent` (and `strata`), or `NULL`
#'   when the share cannot be computed (unknown grouping, or non-`Date` dates).
#'
#' @keywords internal
#' @noRd
.tbl_now_percent_shares <- function(data, grouping, date_col, value_col,
                                    holiday_config = NULL, by_strata = FALSE) {
  block_unit <- .tbl_now_percent_block_unit(grouping)
  if (is.null(block_unit) || !lubridate::is.Date(data[[date_col]])) {
    return(NULL)
  }
  grouped <- .tbl_now_add_calendar_group(data, grouping, date_col, holiday_config)
  if (is.null(grouped)) {
    return(NULL)
  }
  grouped <- dplyr::mutate(grouped,
    block = lubridate::floor_date(.data[[date_col]], unit = block_unit)
  )

  keys <- c(if (by_strata) "strata", "block", "calendar_group")
  totals <- grouped |>
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
    dplyr::summarise(value = sum(.data[[value_col]], na.rm = TRUE), .groups = "drop")

  # A block where a group never occurs (a week with no holiday) contributes a
  # genuine 0%, so fill it in rather than letting it drop out of the boxplot.
  totals <- if (by_strata) {
    tidyr::complete(totals, .data$strata, .data$block, .data$calendar_group,
                    fill = list(value = 0))
  } else {
    tidyr::complete(totals, .data$block, .data$calendar_group,
                    fill = list(value = 0))
  }

  totals |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(if (by_strata) "strata", "block")))) |>
    dplyr::mutate(percent = 100 * .data$value / sum(.data$value, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::filter(is.finite(.data$percent))
}

#' Panel shown when `measure = "percent"` cannot be computed
#'
#' @param palette A named colour palette.
#'
#' @return A ggplot object.
#'
#' @keywords internal
#' @noRd
.tbl_now_percent_unavailable <- function(palette) {
  .tbl_now_empty_panel(
    "Percentages need date-based event/report columns", palette
  )
}

# Individual panels-----

#' Panel: empirical delay distribution
#'
#' @param delay_distribution A tibble from `.tbl_now_delay_distribution()`.
#' @param palette A named colour palette.
#'
#' @return A ggplot object.
#'
#' @keywords internal
#' @noRd
.tbl_now_panel_delay <- function(delay_distribution, palette) {
  normalised_weight <- delay_distribution$weight / sum(delay_distribution$weight)
  plot_data <- dplyr::mutate(delay_distribution, normalised_weight = normalised_weight)

  style <- .tbl_now_process_style("reporting", palette)

  ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$delay)) +
    ggplot2::geom_histogram(
      ggplot2::aes(weight = .data$normalised_weight),
      fill = style$fill, colour = style$line,
      binwidth = 1, center = 0
    ) +
    ggplot2::labs(
      title = "Empirical delay distribution",
      subtitle = style$subtitle,
      x = "Reporting delay", y = "Density"
    )
}

#' Panel: cumulative growth per delay (count-cumulative only)
#'
#' Boxplots of the per-delay cumulative growth ratio (see
#' `.tbl_now_cumulative_growth()`), on a log scale with a dashed reference at
#' `1` (no change). The boxes shrink toward `1` as reporting completes.
#'
#' @param growth A tibble from `.tbl_now_cumulative_growth()`.
#' @param palette A named colour palette.
#'
#' @return A ggplot object (or an empty panel when there is nothing to show).
#'
#' @keywords internal
#' @noRd
.tbl_now_panel_delay_cumulative <- function(growth, palette) {
  if (nrow(growth) == 0) {
    return(.tbl_now_empty_panel(
      "Not enough cumulative history for growth ratios", palette
    ))
  }
  style <- .tbl_now_process_style("reporting", palette)

  ggplot2::ggplot(growth, ggplot2::aes(x = .data$delay, y = .data$ratio)) +
    ggplot2::geom_hline(
      yintercept = 1, linetype = "dashed",
      colour = palette[["near_black"]], linewidth = 0.4
    ) +
    ggplot2::geom_boxplot(
      ggplot2::aes(group = .data$delay),
      fill = style$fill, colour = style$line,
      outlier.colour = palette[["near_black"]], outlier.size = 0.6, linewidth = 0.4
    ) +
    ggplot2::scale_y_log10() +
    ggplot2::labs(
      title = "Cumulative growth by delay",
      subtitle = style$subtitle,
      x = "Reporting delay",
      y = "Ratio to the previous delay (1 = stable, log scale)"
    )
}

#' Panel: cumulative growth per delay, split by stratum
#'
#' @param growth_by A tibble from `.tbl_now_cumulative_growth()` carrying `strata`.
#' @param palette A named colour palette.
#'
#' @return A ggplot object (or an empty panel).
#'
#' @keywords internal
#' @noRd
.tbl_now_panel_delay_cumulative_strata <- function(growth_by, palette) {
  if (nrow(growth_by) == 0) {
    return(.tbl_now_empty_panel(
      "Not enough cumulative history for growth ratios", palette
    ))
  }
  ggplot2::ggplot(growth_by, ggplot2::aes(
    x = .data$delay, y = .data$ratio, fill = .data$strata
  )) +
    ggplot2::geom_hline(
      yintercept = 1, linetype = "dashed",
      colour = palette[["near_black"]], linewidth = 0.4
    ) +
    ggplot2::geom_boxplot(
      ggplot2::aes(group = interaction(.data$delay, .data$strata)),
      position = ggplot2::position_dodge2(preserve = "single"),
      outlier.size = 0.5, linewidth = 0.3
    ) +
    ggplot2::scale_y_log10() +
    .tbl_now_strata_fill_scale() +
    ggplot2::labs(
      title = "Cumulative growth by delay",
      subtitle = .tbl_now_process_style("reporting", palette)$subtitle,
      x = "Reporting delay",
      y = "Ratio to the previous delay (1 = stable, log scale)"
    )
}

#' Panel: observed epidemic process
#'
#' @param epidemic_process A tibble from `.tbl_now_epidemic_process()`.
#' @param incomplete_threshold Date/number at which to draw the incompleteness
#'   line, or `NA` to omit it.
#' @param level Completeness level (used only for the annotation label).
#' @param palette A named colour palette.
#' @param holiday_points Optional subset of `epidemic_process` whose event dates
#'   are holidays; drawn as red dots.
#'
#' @return A ggplot object.
#'
#' @keywords internal
#' @noRd
.tbl_now_panel_epidemic <- function(epidemic_process, incomplete_threshold, level,
                                    palette, holiday_points = NULL) {
  base_plot <- ggplot2::ggplot(
    epidemic_process,
    ggplot2::aes(x = .data$event_date, y = .data$case_count)
  ) +
    ggplot2::geom_area(fill = palette[["light_green"]]) +
    ggplot2::geom_line(colour = palette[["primary_green"]])

  # Mark event dates that fall on a holiday (from the temporal-effects spec)
  if (!is.null(holiday_points) && nrow(holiday_points) > 0) {
    base_plot <- base_plot +
      ggplot2::geom_point(
        data = holiday_points,
        ggplot2::aes(x = .data$event_date, y = .data$case_count),
        colour = palette[["accent_red"]], size = 2
      ) +
      ggplot2::geom_point(
        data = holiday_points,
        ggplot2::aes(x = .data$event_date, y = .data$case_count),
        colour = "white", size = 1
      )
  }

  if (!is.na(incomplete_threshold)) {
    base_plot <- base_plot +
      ggplot2::geom_vline(
        xintercept = incomplete_threshold,
        colour = palette[["near_black"]], linetype = "dashed", linewidth = 0.7
      ) +
      ggplot2::annotate(
        "label",
        x = incomplete_threshold, y = Inf,
        label = paste0("Incomplete (<", round(100 * level), "% reported)"),
        vjust = 1.1, hjust = 1.02, size = 3,
        colour = palette[["near_black"]], fill = "white"
      )
  }

  base_plot +
    ggplot2::labs(
      title = "Observed epidemic process",
      subtitle = .tbl_now_process_style("epidemic", palette)$subtitle,
      x = "Event date", y = "Reported cases"
    )
}

#' Event dates that fall on a holiday
#'
#' Looks up the holiday calendar(s) in the object's temporal-effects spec and
#' returns the matching rows of `epidemic_process` (so the caller knows their y
#' position for plotting).
#'
#' @param object A `tbl_now` object.
#' @param epidemic_process A tibble from `.tbl_now_epidemic_process()`.
#'
#' @return The subset of `epidemic_process` rows whose `event_date` is a holiday
#'   (empty when there is no holiday calendar or \pkg{almanac} is unavailable).
#'
#' @keywords internal
#' @noRd
.tbl_now_holiday_points <- function(object, epidemic_process) {
  no_holidays <- dplyr::slice(epidemic_process, 0)

  specs <- get_temporal_effects(object)
  if (length(specs) == 0) {
    return(no_holidays)
  }

  calendars <- Filter(Negate(is.null), lapply(specs, function(s) s$t_effects@holidays))
  if (length(calendars) == 0) {
    return(no_holidays)
  }

  if (!requireNamespace("almanac", quietly = TRUE)) {
    cli::cli_warn("Package {.pkg almanac} is needed to mark holidays; skipping holiday dots.")
    return(no_holidays)
  }
  if (!lubridate::is.Date(epidemic_process$event_date)) {
    return(no_holidays)
  }

  dplyr::slice(
    epidemic_process,
    which(.alma_in_any(epidemic_process$event_date, calendars))
  )
}

#' The holiday / weekend configuration asked for by the temporal-effects spec
#'
#' Reads every [temporal_effects()] spec attached to the object and folds them
#' into the one description the holiday panels need. A `tbl_now` carries one spec
#' per [add_temporal_effects()] call, and each may name its own calendar, weekend
#' effect and holiday-lag depth — including the two-spec idiom that models both
#' sides of a holiday (see `?temporal_effects`).
#'
#' @param object A `tbl_now` object.
#'
#' @return `NULL` when no holiday or weekend effect is attached (the holiday
#'   panels then do not apply). Otherwise a list with:
#'   * `calendars` — a list of [almanac::rcalendar()]s (possibly empty).
#'   * `weekend` — `TRUE` when any spec asks for a weekend effect.
#'   * `weekend_days` — the weekend definition (from the first spec).
#'   * `lag_depths` — the distinct non-zero `holiday_lags` depths, signed
#'     (negative = working days *before* a holiday).
#'
#' @keywords internal
#' @noRd
.tbl_now_holiday_config <- function(object) {
  specs <- get_temporal_effects(object)
  if (length(specs) == 0) {
    return(NULL)
  }

  calendars <- Filter(Negate(is.null), lapply(specs, function(spec) {
    spec$t_effects@holidays
  }))
  has_weekend <- any(vapply(specs, function(spec) {
    isTRUE(spec$t_effects@weekend)
  }, logical(1)))
  lag_depths <- vapply(specs, function(spec) {
    as.integer(spec$t_effects@holiday_lags)
  }, integer(1))

  # Marking holidays needs almanac; a weekend-only effect does not.
  if (length(calendars) > 0 && !requireNamespace("almanac", quietly = TRUE)) {
    cli::cli_warn(
      "Package {.pkg almanac} is needed for the holiday panels; skipping them."
    )
    calendars <- list()
    lag_depths <- integer(0)
  }

  if (length(calendars) == 0 && !has_weekend) {
    return(NULL)
  }

  list(
    calendars    = calendars,
    weekend      = has_weekend,
    # Every spec carries a weekend definition; they agree in all but pathological
    # cases, so the first one speaks for the object.
    weekend_days = specs[[1]]$weekend_days,
    lag_depths   = sort(unique(lag_depths[lag_depths != 0]))
  )
}

#' Which holiday groupings the attached spec supports
#'
#' @param holiday_config A list from `.tbl_now_holiday_config()`, or `NULL`.
#'
#' @return A character vector of groupings (`"holiday"`, `"holiday_lag"`),
#'   possibly empty.
#'
#' @keywords internal
#' @noRd
.tbl_now_holiday_groupings <- function(holiday_config) {
  if (is.null(holiday_config)) {
    return(character(0))
  }
  c(
    "holiday",
    # The lag panel needs a calendar to count from and a depth to count to.
    if (length(holiday_config$calendars) > 0 &&
        length(holiday_config$lag_depths) > 0) {
      "holiday_lag"
    }
  )
}

#' Classify dates as Weekday / Weekend / Holiday
#'
#' Which categories exist depends on what the spec asked for: a holiday calendar
#' alone contrasts holidays with everything else, a weekend effect alone
#' contrasts weekends with working days, and both together give the three-way
#' split. A holiday falling on a weekend counts as a **holiday** — it is the
#' stronger claim about why reporting stopped.
#'
#' @param dates A vector of `<Date>` values.
#' @param holiday_config A list from `.tbl_now_holiday_config()`.
#'
#' @return A factor with the levels from `.tbl_now_holiday_levels()`.
#'
#' @keywords internal
#' @noRd
.tbl_now_holiday_group <- function(dates, holiday_config) {
  has_calendar <- length(holiday_config$calendars) > 0
  is_holiday <- .alma_in_any(dates, holiday_config$calendars)
  is_weekend <- !is_weekday(dates, weekend_days = holiday_config$weekend_days)

  group <- if (holiday_config$weekend) {
    dplyr::if_else(is_weekend, "Weekend", "Weekday")
  } else {
    rep("Non-holiday", length(dates))
  }
  if (has_calendar) {
    group[is_holiday] <- "Holiday"
  }

  factor(group, levels = .tbl_now_holiday_levels(holiday_config))
}

#' The ordered categories of the holiday effect panel
#'
#' @param holiday_config A list from `.tbl_now_holiday_config()`.
#'
#' @return A character vector of factor levels.
#'
#' @keywords internal
#' @noRd
.tbl_now_holiday_levels <- function(holiday_config) {
  has_calendar <- length(holiday_config$calendars) > 0
  c(
    if (holiday_config$weekend) c("Weekday", "Weekend") else "Non-holiday",
    if (has_calendar) "Holiday"
  )
}

#' Assign each date its position relative to the nearest holiday
#'
#' Uses the same working-day counting as the `..._holiday_lag_k` /
#' `..._holiday_lead_k` effect columns, so the panel shows exactly the days those
#' columns flag: weekends and other holidays are skipped, and the count reaches
#' only as far as the depths the spec asked for. Everything else is `"Other"` —
#' the baseline the lag days should be read against.
#'
#' @param dates A vector of `<Date>` values.
#' @param holiday_config A list from `.tbl_now_holiday_config()`.
#'
#' @return A factor with the levels from `.tbl_now_holiday_lag_levels()`.
#'
#' @keywords internal
#' @noRd
.tbl_now_holiday_lag_group <- function(dates, holiday_config) {
  depths <- holiday_config$lag_depths
  days_after  <- max(c(depths[depths > 0], 0L))
  days_before <- max(c(-depths[depths < 0], 0L))

  working_days_from <- function(direction) {
    .working_days_from_reset(
      dates,
      reset_event  = "holiday",
      direction    = direction,
      weekend_days = holiday_config$weekend_days,
      holidays     = holiday_config$calendars
    )
  }
  no_distance <- rep(NA_integer_, length(dates))
  distance_after <- if (days_after > 0) working_days_from("after") else no_distance
  distance_before <- if (days_before > 0) working_days_from("before") else no_distance

  within_after <- !is.na(distance_after) &
    distance_after >= 1L & distance_after <= days_after
  within_before <- !is.na(distance_before) &
    distance_before >= 1L & distance_before <= days_before

  # A date can sit both after one holiday and before the next (the days between
  # Christmas and New Year). Attribute it to whichever holiday is nearer, and to
  # the "after" side when equidistant: the post-holiday rebound is the effect
  # these lags exist to capture.
  label_after <- within_after &
    (!within_before | distance_after <= distance_before)
  label_before <- within_before & !label_after

  group <- rep("Other", length(dates))
  group[label_before] <- paste0(distance_before[label_before], " before")
  group[label_after] <- paste0(distance_after[label_after], " after")
  group[.alma_in_any(dates, holiday_config$calendars)] <- "Holiday"

  factor(group, levels = .tbl_now_holiday_lag_levels(depths))
}

#' The ordered categories of the holiday-lag panel
#'
#' Reads left-to-right as time does: the working days running up to a holiday,
#' the holiday itself, the working days after it, and finally every other day as
#' the reference.
#'
#' @param depths The signed, non-zero `holiday_lags` depths from the spec.
#'
#' @return A character vector of factor levels.
#'
#' @keywords internal
#' @noRd
.tbl_now_holiday_lag_levels <- function(depths) {
  days_after  <- max(c(depths[depths > 0], 0L))
  days_before <- max(c(-depths[depths < 0], 0L))
  c(
    if (days_before > 0) paste0(rev(seq_len(days_before)), " before"),
    "Holiday",
    if (days_after > 0) paste0(seq_len(days_after), " after"),
    "Other"
  )
}

#' Titles for a calendar grouping
#'
#' @param grouping One of `"weekday"`, `"week"` or `"month"`.
#'
#' @return A list with `title` (effect name) and `x` (x-axis label), or `NULL`
#'   for an unknown grouping.
#'
#' @keywords internal
#' @noRd
.tbl_now_calendar_group_spec <- function(grouping) {
  switch(grouping,
    weekday     = list(title = "Day-of-week",   x = "Day of week"),
    week        = list(title = "Week-of-year",  x = "Epidemiological week"),
    month       = list(title = "Month-of-year", x = "Month"),
    holiday     = list(title = "Holiday",       x = "Day type"),
    holiday_lag = list(title = "Holiday lag",   x = "Working days from the nearest holiday"),
    NULL
  )
}

#' Add a `calendar_group` factor derived from a date column
#'
#' @param data A data frame with a date column.
#' @param grouping One of `"weekday"`, `"week"`, `"month"`, `"holiday"` or
#'   `"holiday_lag"`.
#' @param date_col Name of the date column (default `"event_date"`).
#' @param holiday_config A list from `.tbl_now_holiday_config()`. Required by the
#'   `"holiday"` / `"holiday_lag"` groupings, which read the calendar, weekend
#'   definition and lag depths from the temporal-effects spec; ignored by the
#'   others, which need only the date.
#'
#' @return `data` with a `calendar_group` column, or `NULL` for an unknown
#'   grouping (or a holiday grouping with no spec to read).
#'
#' @keywords internal
#' @noRd
.tbl_now_add_calendar_group <- function(data, grouping, date_col = "event_date",
                                        holiday_config = NULL) {
  dates <- data[[date_col]]
  needs_config <- grouping %in% c("holiday", "holiday_lag")
  if (needs_config && is.null(holiday_config)) {
    return(NULL)
  }

  calendar_group <- switch(grouping,
    weekday     = lubridate::wday(dates, label = TRUE, abbr = FALSE, week_start = 1),
    week        = factor(lubridate::epiweek(dates)),
    month       = lubridate::month(dates, label = TRUE, abbr = TRUE),
    holiday     = .tbl_now_holiday_group(dates, holiday_config),
    holiday_lag = .tbl_now_holiday_lag_group(dates, holiday_config),
    return(NULL)
  )
  dplyr::mutate(data, calendar_group = calendar_group)
}

#' Panel: case-count calendar effect (boxplots)
#'
#' With `measure = "normalized"` the boxes show each event date's reported cases
#' divided by the overall mean, so 1 marks an average level. With
#' `measure = "percent"` (what `autoplot()` defaults to) they show the
#' **share of cases** falling in each calendar
#' group, one observation per calendar block (a week for the day-of-week panel, a
#' year for the week-of-year one), so the median and IQR read directly as "x% of
#' the cases happen at the weekend".
#'
#' @param epidemic_process A tibble from `.tbl_now_epidemic_process()`.
#' @param grouping One of `"weekday"`, `"week"`, `"month"`, `"holiday"` or
#'   `"holiday_lag"`.
#' @param palette A named colour palette.
#' @param holiday_config A list from `.tbl_now_holiday_config()`, or `NULL`.
#' @param measure `"normalized"` or `"percent"`.
#'
#' @return A ggplot object (or an empty panel when there are no cases).
#'
#' @keywords internal
#' @noRd
.tbl_now_panel_calendar <- function(epidemic_process, grouping, palette,
                                    holiday_config = NULL,
                                    measure = "normalized") {
  overall_mean <- mean(epidemic_process$case_count, na.rm = TRUE)
  if (is.na(overall_mean) || overall_mean == 0) {
    return(.tbl_now_empty_panel("No cases to compute a calendar effect", palette))
  }

  labels <- .tbl_now_calendar_group_spec(grouping)
  if (is.null(labels)) {
    return(.tbl_now_empty_panel(
      paste0("Calendar effect unavailable for ", grouping), palette
    ))
  }
  style <- .tbl_now_process_style("epidemic", palette)

  if (identical(measure, "percent")) {
    plot_data <- .tbl_now_percent_shares(
      epidemic_process, grouping, "event_date", "case_count", holiday_config
    )
    if (is.null(plot_data)) {
      return(.tbl_now_percent_unavailable(palette))
    }
    value <- "percent"
    y_lab <- "Percent of cases (%)"
    reference <- NULL
  } else {
    grouped <- .tbl_now_add_calendar_group(epidemic_process, grouping,
                                           holiday_config = holiday_config)
    if (is.null(grouped)) {
      return(.tbl_now_empty_panel(
        paste0("Calendar effect unavailable for ", grouping), palette
      ))
    }
    plot_data <- dplyr::mutate(grouped,
      normalized_effect = .data$case_count / overall_mean
    )
    value <- "normalized_effect"
    y_lab <- "Normalized effect (1 = average)"
    reference <- 1
  }

  base_plot <- ggplot2::ggplot(plot_data, ggplot2::aes(
    x = .data$calendar_group, y = .data[[value]]
  )) +
    ggplot2::geom_boxplot(
      fill = style$fill, colour = style$line,
      outlier.colour = palette[["near_black"]], outlier.size = 0.6, linewidth = 0.4
    )

  if (!is.null(reference)) {
    base_plot <- base_plot +
      ggplot2::geom_hline(
        yintercept = reference, linetype = "dashed",
        colour = palette[["near_black"]], linewidth = 0.4
      )
  }

  base_plot +
    ggplot2::labs(
      title = paste0(labels$title, " effect"),
      subtitle = style$subtitle,
      x = labels$x, y = y_lab
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

#' Panel: reporting-delay calendar effect (boxplots)
#'
#' With `measure = "normalized"` the boxes show the case-count
#' weighted **mean reporting delay** per event date, divided by the overall mean
#' delay, grouped by calendar unit: this reveals whether the delay itself has a
#' day-of-week, week-of-year or month-of-year pattern (a "delay effect"). With
#' `measure = "percent"` the panel switches to the **report date** and shows the
#' share of cases *reported* in each calendar group, one observation per calendar
#' block — "x% of the reports arrive at the weekend".
#'
#' @param delay_per_date A tibble from `.tbl_now_delay_per_date()`.
#' @param grouping One of `"weekday"`, `"week"`, `"month"`, `"holiday"` or
#'   `"holiday_lag"`.
#' @param palette A named colour palette.
#' @param holiday_config A list from `.tbl_now_holiday_config()`, or `NULL`.
#' @param measure `"normalized"` or `"percent"`.
#' @param reporting_process A tibble from `.tbl_now_reporting_process()`, needed
#'   only when `measure = "percent"`.
#'
#' @return A ggplot object (or an empty panel when there is nothing to show).
#'
#' @keywords internal
#' @noRd
.tbl_now_panel_delay_calendar <- function(delay_per_date, grouping, palette,
                                          holiday_config = NULL,
                                          measure = "normalized",
                                          reporting_process = NULL) {
  labels <- .tbl_now_calendar_group_spec(grouping)
  if (is.null(labels)) {
    return(.tbl_now_empty_panel(
      paste0("Delay effect unavailable for ", grouping), palette
    ))
  }
  style <- .tbl_now_process_style("reporting", palette)

  if (identical(measure, "percent")) {
    if (is.null(reporting_process) || nrow(reporting_process) == 0) {
      return(.tbl_now_empty_panel("No reports to summarise", palette))
    }
    plot_data <- .tbl_now_percent_shares(
      reporting_process, grouping, "report_date", "case_count", holiday_config
    )
    if (is.null(plot_data)) {
      return(.tbl_now_percent_unavailable(palette))
    }
    value <- "percent"
    title <- paste0(labels$title, " reporting effect")
    x_lab <- sub("^Day of week$", "Day of week (report date)", labels$x)
    y_lab <- "Percent of reported cases (%)"
    reference <- NULL
  } else {
    if (nrow(delay_per_date) == 0) {
      return(.tbl_now_empty_panel("No delays to summarise", palette))
    }
    overall_mean_delay <- mean(delay_per_date$mean_delay, na.rm = TRUE)
    if (is.na(overall_mean_delay) || overall_mean_delay == 0) {
      return(.tbl_now_empty_panel("No delays to compute a delay effect", palette))
    }
    grouped <- .tbl_now_add_calendar_group(delay_per_date, grouping,
                                           holiday_config = holiday_config)
    if (is.null(grouped)) {
      return(.tbl_now_empty_panel(
        paste0("Delay effect unavailable for ", grouping), palette
      ))
    }
    plot_data <- dplyr::mutate(grouped,
      normalized_delay = .data$mean_delay / overall_mean_delay
    )
    value <- "normalized_delay"
    title <- paste0(labels$title, " delay effect")
    x_lab <- labels$x
    y_lab <- "Normalized mean delay (1 = average)"
    reference <- 1
  }

  base_plot <- ggplot2::ggplot(plot_data, ggplot2::aes(
    x = .data$calendar_group, y = .data[[value]]
  )) +
    ggplot2::geom_boxplot(
      fill = style$fill, colour = style$line,
      outlier.colour = palette[["near_black"]], outlier.size = 0.6, linewidth = 0.4
    )

  if (!is.null(reference)) {
    base_plot <- base_plot +
      ggplot2::geom_hline(
        yintercept = reference, linetype = "dashed",
        colour = palette[["near_black"]], linewidth = 0.4
      )
  }

  base_plot +
    ggplot2::labs(
      title = title, subtitle = style$subtitle,
      x = x_lab, y = y_lab
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

#' Calendar groupings to display for a given event unit
#'
#' Daily data gets both a day-of-week and a week-of-year panel.
#'
#' @param event_units The event units (`"days"`, `"weeks"`, `"months"`, ...).
#'
#' @return A character vector of groupings (`"weekday"`, `"week"`, `"month"`),
#'   possibly empty.
#'
#' @keywords internal
#' @noRd
.tbl_now_calendar_groupings <- function(event_units) {
  switch(event_units,
    days   = c("weekday", "week"),
    weeks  = "week",
    months = "month",
    character(0)
  )
}

#' Panel: periodogram of an arbitrary series
#'
#' Shared engine behind the incidence-seasonality and delay-periodicity panels.
#' Draws a periodogram ([stats::spec.pgram()]) and marks the dominant period.
#'
#' @param series A numeric series (ordered by date).
#' @param event_units The event units (used to label the period axis).
#' @param palette A named colour palette.
#' @param title,subtitle Panel title and subtitle.
#' @param line_colour Colour of the periodogram line and dominant-period marker.
#' @param empty_message Message for the empty panel when the series is too short.
#'
#' @return A ggplot object (or an empty panel when the series is too short).
#'
#' @keywords internal
#' @noRd
.tbl_now_periodogram_panel <- function(series, event_units, palette, title,
                                       subtitle, line_colour, empty_message) {
  series <- series[!is.na(series)]
  if (length(series) < 8 || stats::var(series) == 0) {
    return(.tbl_now_empty_panel(empty_message, palette))
  }

  spectrum <- tryCatch(
    stats::spec.pgram(stats::ts(series), detrend = TRUE, taper = 0.1, plot = FALSE),
    error = function(e) NULL
  )
  if (is.null(spectrum)) {
    return(.tbl_now_empty_panel("Could not estimate a periodogram", palette))
  }

  periodogram <- dplyr::tibble(
    period = 1 / spectrum$freq,
    power  = spectrum$spec
  ) |>
    dplyr::filter(.data$period <= length(series) / 2)

  dominant_period <- periodogram$period[which.max(periodogram$power)]
  unit_label <- switch(event_units,
    days = "days", weeks = "weeks", months = "months", years = "years", "units"
  )

  ggplot2::ggplot(periodogram, ggplot2::aes(x = .data$period, y = .data$power)) +
    ggplot2::geom_line(colour = line_colour, linewidth = 0.7) +
    ggplot2::geom_vline(
      xintercept = dominant_period,
      colour = palette[["near_black"]], linetype = "dashed", linewidth = 0.7
    ) +
    ggplot2::annotate(
      "label",
      x = dominant_period, y = Inf,
      label = paste0("~", round(dominant_period, 1), " ", unit_label),
      vjust = 1.1, hjust = -0.05, size = 3,
      colour = palette[["near_black"]], fill = "white"
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = paste0("Period (", unit_label, ")"), y = "Spectral power"
    )
}

#' Panel: cycles periodogram (incidence)
#'
#' A periodogram of the incidence series whose dominant peak suggests a Fourier
#' season length.
#'
#' @param epidemic_process A tibble from `.tbl_now_epidemic_process()`.
#' @param event_units The event units (used to label the period axis).
#' @param palette A named colour palette.
#'
#' @return A ggplot object (or an empty panel when the series is too short).
#'
#' @keywords internal
#' @noRd
.tbl_now_panel_periodogram <- function(epidemic_process, event_units, palette) {
  style <- .tbl_now_process_style("epidemic", palette)
  .tbl_now_periodogram_panel(
    epidemic_process$case_count, event_units, palette,
    title = "Cycles (periodogram)",
    subtitle = style$subtitle,
    line_colour = style$line,
    empty_message = "Too few points to estimate cycles"
  )
}

#' Panel: reporting-delay periodogram
#'
#' A periodogram of the mean-reporting-delay series, whose dominant peak reveals
#' periodicity in the reporting delay itself (e.g. a weekly reporting cycle).
#'
#' @param delay_per_date A tibble from `.tbl_now_delay_per_date()`.
#' @param event_units The event units (used to label the period axis).
#' @param palette A named colour palette.
#'
#' @return A ggplot object (or an empty panel when the series is too short).
#'
#' @keywords internal
#' @noRd
.tbl_now_panel_delay_periodogram <- function(delay_per_date, event_units, palette) {
  style <- .tbl_now_process_style("reporting", palette)
  .tbl_now_periodogram_panel(
    delay_per_date$mean_delay, event_units, palette,
    title = "Cycles (periodogram)",
    subtitle = style$subtitle,
    line_colour = style$line,
    empty_message = "Too few points to estimate delay cycles"
  )
}


# By-strata data + panels-----

#' Resolve which columns to group the by-strata panels on
#'
#' @param object A `tbl_now` object.
#' @param strata `NULL` (use the object's `strata`) or an explicit character
#'   vector of column names to override it.
#'
#' @return A character vector of strata column names (aborts when none are
#'   available or a requested column is missing).
#'
#' @keywords internal
#' @noRd
.tbl_now_resolve_strata_cols <- function(object, strata) {
  strata_cols <- if (is.null(strata)) get_strata(object) else strata
  if (length(strata_cols) == 0) {
    cli::cli_abort(c(
      "{.arg by_strata} is {.val TRUE} but there are no strata to split on.",
      "i" = "Attach strata to the {.cls tbl_now} (see {.fn add_strata}) or pass \\
             the columns directly, e.g. {.code strata = \"gender\"}."
    ))
  }
  if (!is.character(strata_cols)) {
    cli::cli_abort("{.arg strata} must be a character vector of column names.")
  }
  missing_columns <- setdiff(strata_cols, colnames(object))
  if (length(missing_columns) > 0) {
    cli::cli_abort(
      "Strata column{?s} {.val {missing_columns}} {?is/are} not in the data."
    )
  }
  strata_cols
}

#' Combine one or more strata columns into a single label
#'
#' @param data A data frame holding the strata columns.
#' @param strata_cols Character vector of column names.
#'
#' @return A character vector: the columns pasted together with `" / "`.
#'
#' @keywords internal
#' @noRd
.tbl_now_strata_label <- function(data, strata_cols) {
  parts <- lapply(strata_cols, function(column) as.character(data[[column]]))
  do.call(paste, c(parts, sep = " / "))
}

#' A viridis colour / fill scale for the strata grouping
#'
#' @param name Legend title.
#'
#' @return A ggplot2 scale.
#'
#' @keywords internal
#' @noRd
.tbl_now_strata_colour_scale <- function(name = "Strata") {
  ggplot2::scale_colour_viridis_d(name = name, option = "D", end = 0.9)
}

#' @rdname dot-tbl_now_strata_colour_scale
#' @keywords internal
#' @noRd
.tbl_now_strata_fill_scale <- function(name = "Strata") {
  ggplot2::scale_fill_viridis_d(name = name, option = "D", end = 0.9)
}

#' Observed incidence per event date **and stratum**
#'
#' @param object A `tbl_now` object.
#' @param strata_cols Character vector of strata columns.
#'
#' @return A tibble with `event_date`, `case_count` and a combined `strata`
#'   label, sorted by date.
#'
#' @keywords internal
#' @noRd
.tbl_now_epidemic_process_by <- function(object, strata_cols) {
  latest <- get_latest_reported_cases(object)
  case_count_column <- get_case_count(latest)
  event_date_column <- get_event_date(object)
  latest |>
    dplyr::as_tibble() |>
    dplyr::summarise(
      case_count = sum(.data[[case_count_column]], na.rm = TRUE),
      .by = dplyr::all_of(c(event_date_column, strata_cols))
    ) |>
    dplyr::mutate(
      event_date = .data[[event_date_column]],
      strata     = .tbl_now_strata_label(dplyr::pick(dplyr::all_of(strata_cols)), strata_cols)
    ) |>
    dplyr::arrange(.data$strata, .data$event_date)
}

#' Reporting delays weighted by case counts, split by stratum
#'
#' @param object A `tbl_now` object.
#' @param strata_cols Character vector of strata columns.
#'
#' @return A tibble with `strata`, `delay` and a within-stratum `density`.
#'
#' @keywords internal
#' @noRd
.tbl_now_delay_distribution_by <- function(object, strata_cols) {
  incidence <- object |>
    ungroup() |>
    to_count(to = "count-incidence")
  case_count_column <- get_case_count(incidence)
  observations <- as.data.frame(incidence)

  dplyr::tibble(
    strata = .tbl_now_strata_label(observations, strata_cols),
    delay  = observations[[".delay"]],
    weight = observations[[case_count_column]]
  ) |>
    dplyr::filter(!is.na(.data$delay), !is.na(.data$weight), .data$weight > 0) |>
    dplyr::group_by(.data$strata, .data$delay) |>
    dplyr::summarise(weight = sum(.data$weight), .groups = "drop") |>
    dplyr::group_by(.data$strata) |>
    dplyr::mutate(density = .data$weight / sum(.data$weight)) |>
    dplyr::ungroup()
}

#' Mean reporting delay per event date **and stratum**
#'
#' @param object A `tbl_now` object.
#' @param strata_cols Character vector of strata columns.
#'
#' @return A tibble with `strata`, `event_date`, `mean_delay` and `cases`.
#'
#' @keywords internal
#' @noRd
.tbl_now_delay_per_date_by <- function(object, strata_cols) {
  incidence <- object |>
    ungroup() |>
    to_count(to = "count-incidence")
  case_count_column <- get_case_count(incidence)
  event_date_column <- get_event_date(object)
  observations <- as.data.frame(incidence)

  dplyr::tibble(
    strata     = .tbl_now_strata_label(observations, strata_cols),
    event_date = observations[[event_date_column]],
    delay      = observations[[".delay"]],
    weight     = observations[[case_count_column]]
  ) |>
    dplyr::filter(
      !is.na(.data$event_date), !is.na(.data$delay),
      !is.na(.data$weight), .data$weight > 0
    ) |>
    dplyr::group_by(.data$strata, .data$event_date) |>
    dplyr::summarise(
      mean_delay = stats::weighted.mean(.data$delay, .data$weight),
      cases      = sum(.data$weight),
      .groups    = "drop"
    ) |>
    dplyr::arrange(.data$strata, .data$event_date)
}

#' Panel: empirical delay distribution, one dodged bar per stratum
#'
#' @param delay_distribution_by A tibble from `.tbl_now_delay_distribution_by()`.
#' @param palette A named colour palette.
#'
#' @return A ggplot object.
#'
#' @keywords internal
#' @noRd
.tbl_now_panel_delay_strata <- function(delay_distribution_by, palette) {
  if (nrow(delay_distribution_by) == 0) {
    return(.tbl_now_empty_panel("No delays to summarise", palette))
  }
  ggplot2::ggplot(
    delay_distribution_by,
    ggplot2::aes(x = .data$delay, y = .data$density, fill = .data$strata)
  ) +
    ggplot2::geom_col(
      position = ggplot2::position_dodge2(preserve = "single", padding = 0.1),
      width = 0.9
    ) +
    .tbl_now_strata_fill_scale() +
    ggplot2::labs(
      title = "Empirical delay distribution",
      subtitle = .tbl_now_process_style("reporting", palette)$subtitle,
      x = "Reporting delay", y = "Density within stratum"
    )
}

#' Panel: observed epidemic process, one line per stratum (no fill)
#'
#' @param epidemic_process_by A tibble from `.tbl_now_epidemic_process_by()`.
#' @param incomplete_threshold Date/number of the incompleteness line, or `NA`.
#' @param level Completeness level (for the annotation label).
#' @param palette A named colour palette.
#'
#' @return A ggplot object.
#'
#' @keywords internal
#' @noRd
.tbl_now_panel_epidemic_strata <- function(epidemic_process_by, incomplete_threshold,
                                           level, palette) {
  base_plot <- ggplot2::ggplot(
    epidemic_process_by,
    ggplot2::aes(x = .data$event_date, y = .data$case_count, colour = .data$strata)
  ) +
    ggplot2::geom_line(linewidth = 0.6) +
    .tbl_now_strata_colour_scale()

  if (!is.na(incomplete_threshold)) {
    base_plot <- base_plot +
      ggplot2::geom_vline(
        xintercept = incomplete_threshold,
        colour = palette[["near_black"]], linetype = "dashed", linewidth = 0.6
      ) +
      ggplot2::annotate(
        "label",
        x = incomplete_threshold, y = Inf,
        label = paste0("Incomplete (<", round(100 * level), "% reported)"),
        vjust = 1.1, hjust = 1.02, size = 3,
        colour = palette[["near_black"]], fill = "white"
      )
  }

  base_plot +
    ggplot2::labs(
      title = "Observed epidemic process",
      subtitle = .tbl_now_process_style("epidemic", palette)$subtitle,
      x = "Event date", y = "Reported cases"
    )
}

#' Panel: case-count calendar effect, one dodged box per stratum
#'
#' Normalized **per stratum** (each stratum divided by its own mean), so 1 marks
#' that stratum's average and the calendar *pattern* is comparable across strata.
#'
#' @param epidemic_process_by A tibble from `.tbl_now_epidemic_process_by()`.
#' @param grouping One of `"weekday"`, `"week"` or `"month"`.
#' @param palette A named colour palette.
#'
#' @return A ggplot object (or an empty panel).
#'
#' @keywords internal
#' @noRd
.tbl_now_panel_calendar_strata <- function(epidemic_process_by, grouping, palette,
                                           holiday_config = NULL,
                                           measure = "normalized") {
  labels <- .tbl_now_calendar_group_spec(grouping)
  if (is.null(labels)) {
    return(.tbl_now_empty_panel(
      paste0("Calendar effect unavailable for ", grouping), palette
    ))
  }

  if (identical(measure, "percent")) {
    plot_data <- .tbl_now_percent_shares(
      epidemic_process_by, grouping, "event_date", "case_count",
      holiday_config, by_strata = TRUE
    )
    if (is.null(plot_data)) {
      return(.tbl_now_percent_unavailable(palette))
    }
    value <- "percent"
    y_lab <- "Percent of the stratum's cases (%)"
    reference <- NULL
  } else {
    grouped <- .tbl_now_add_calendar_group(epidemic_process_by, grouping,
                                           holiday_config = holiday_config)
    if (is.null(grouped)) {
      return(.tbl_now_empty_panel(
        paste0("Calendar effect unavailable for ", grouping), palette
      ))
    }
    plot_data <- grouped |>
      dplyr::group_by(.data$strata) |>
      dplyr::mutate(
        normalized_effect = .data$case_count / mean(.data$case_count, na.rm = TRUE)
      ) |>
      dplyr::ungroup()
    value <- "normalized_effect"
    y_lab <- "Normalized effect (1 = stratum average)"
    reference <- 1
  }

  base_plot <- ggplot2::ggplot(plot_data, ggplot2::aes(
    x = .data$calendar_group, y = .data[[value]], fill = .data$strata
  )) +
    ggplot2::geom_boxplot(
      position = ggplot2::position_dodge2(preserve = "single"),
      outlier.size = 0.5, linewidth = 0.3
    )

  if (!is.null(reference)) {
    base_plot <- base_plot +
      ggplot2::geom_hline(
        yintercept = reference, linetype = "dashed",
        colour = palette[["near_black"]], linewidth = 0.4
      )
  }

  base_plot +
    .tbl_now_strata_fill_scale() +
    ggplot2::labs(
      title = paste0(labels$title, " effect"),
      subtitle = .tbl_now_process_style("epidemic", palette)$subtitle,
      x = labels$x, y = y_lab
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

#' Panel: reporting-delay calendar effect, one dodged box per stratum
#'
#' Normalized **per stratum** (each stratum's mean delay divided by its own mean
#' delay), so 1 marks that stratum's average delay.
#'
#' @param delay_per_date_by A tibble from `.tbl_now_delay_per_date_by()`.
#' @param grouping One of `"weekday"`, `"week"` or `"month"`.
#' @param palette A named colour palette.
#'
#' @return A ggplot object (or an empty panel).
#'
#' @keywords internal
#' @noRd
.tbl_now_panel_delay_calendar_strata <- function(delay_per_date_by, grouping, palette,
                                                 holiday_config = NULL,
                                                 measure = "normalized",
                                                 reporting_process_by = NULL) {
  labels <- .tbl_now_calendar_group_spec(grouping)
  if (is.null(labels)) {
    return(.tbl_now_empty_panel(
      paste0("Delay effect unavailable for ", grouping), palette
    ))
  }

  if (identical(measure, "percent")) {
    if (is.null(reporting_process_by) || nrow(reporting_process_by) == 0) {
      return(.tbl_now_empty_panel("No reports to summarise", palette))
    }
    plot_data <- .tbl_now_percent_shares(
      reporting_process_by, grouping, "report_date", "case_count",
      holiday_config, by_strata = TRUE
    )
    if (is.null(plot_data)) {
      return(.tbl_now_percent_unavailable(palette))
    }
    value <- "percent"
    title <- paste0(labels$title, " reporting effect")
    x_lab <- sub("^Day of week$", "Day of week (report date)", labels$x)
    y_lab <- "Percent of the stratum's reports (%)"
    reference <- NULL
  } else {
    if (nrow(delay_per_date_by) == 0) {
      return(.tbl_now_empty_panel("No delays to summarise", palette))
    }
    grouped <- .tbl_now_add_calendar_group(delay_per_date_by, grouping,
                                           holiday_config = holiday_config)
    if (is.null(grouped)) {
      return(.tbl_now_empty_panel(
        paste0("Delay effect unavailable for ", grouping), palette
      ))
    }
    plot_data <- grouped |>
      dplyr::group_by(.data$strata) |>
      dplyr::mutate(
        normalized_delay = .data$mean_delay / mean(.data$mean_delay, na.rm = TRUE)
      ) |>
      dplyr::ungroup()
    value <- "normalized_delay"
    title <- paste0(labels$title, " delay effect")
    x_lab <- labels$x
    y_lab <- "Normalized mean delay (1 = stratum average)"
    reference <- 1
  }

  base_plot <- ggplot2::ggplot(plot_data, ggplot2::aes(
    x = .data$calendar_group, y = .data[[value]], fill = .data$strata
  )) +
    ggplot2::geom_boxplot(
      position = ggplot2::position_dodge2(preserve = "single"),
      outlier.size = 0.5, linewidth = 0.3
    )

  if (!is.null(reference)) {
    base_plot <- base_plot +
      ggplot2::geom_hline(
        yintercept = reference, linetype = "dashed",
        colour = palette[["near_black"]], linewidth = 0.4
      )
  }

  base_plot +
    .tbl_now_strata_fill_scale() +
    ggplot2::labs(
      title = title,
      subtitle = .tbl_now_process_style("reporting", palette)$subtitle,
      x = x_lab, y = y_lab
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

#' Panel: periodogram with one line per stratum
#'
#' @param series_by A data frame with columns `strata` and `value`, ordered by
#'   date within each stratum.
#' @param event_units The event units (used to label the period axis).
#' @param palette A named colour palette.
#' @param title,subtitle Panel title and subtitle.
#' @param empty_message Message for the empty panel when no stratum is long
#'   enough.
#'
#' @return A ggplot object (or an empty panel).
#'
#' @keywords internal
#' @noRd
.tbl_now_periodogram_by <- function(series_by, event_units, palette, title,
                                    subtitle, empty_message) {
  strata_levels <- unique(series_by$strata)
  spectra <- lapply(strata_levels, function(stratum) {
    values <- series_by$value[series_by$strata == stratum]
    values <- values[!is.na(values)]
    if (length(values) < 8 || stats::var(values) == 0) {
      return(NULL)
    }
    spectrum <- tryCatch(
      stats::spec.pgram(stats::ts(values), detrend = TRUE, taper = 0.1, plot = FALSE),
      error = function(e) NULL
    )
    if (is.null(spectrum)) {
      return(NULL)
    }
    dplyr::tibble(
      strata = stratum,
      period = 1 / spectrum$freq,
      power  = spectrum$spec
    ) |>
      dplyr::filter(.data$period <= length(values) / 2)
  })

  periodogram <- dplyr::bind_rows(spectra)
  if (nrow(periodogram) == 0) {
    return(.tbl_now_empty_panel(empty_message, palette))
  }

  unit_label <- switch(event_units,
    days = "days", weeks = "weeks", months = "months", years = "years", "units"
  )

  ggplot2::ggplot(periodogram, ggplot2::aes(
    x = .data$period, y = .data$power, colour = .data$strata
  )) +
    ggplot2::geom_line(linewidth = 0.6) +
    .tbl_now_strata_colour_scale() +
    ggplot2::labs(
      title = title, subtitle = subtitle,
      x = paste0("Period (", unit_label, ")"), y = "Spectral power"
    )
}

#' A blank panel carrying an explanatory message
#'
#' @param message The text to display.
#' @param palette A named colour palette.
#'
#' @return A ggplot object with a centred text annotation.
#'
#' @keywords internal
#' @noRd
.tbl_now_empty_panel <- function(message, palette) {
  ggplot2::ggplot() +
    ggplot2::annotate("text",
      x = 0, y = 0, label = message,
      colour = palette[["accent_red"]], size = 3.5
    ) +
    ggplot2::theme_void()
}

#' Apply x-axis limits to a panel without dropping data
#'
#' Uses [ggplot2::coord_cartesian()] so points are clipped, not removed.
#'
#' @param plot A ggplot object.
#' @param xlim A length-2 vector of limits, or `NULL` to leave the panel as-is.
#'
#' @return The (possibly zoomed) ggplot object.
#'
#' @keywords internal
#' @noRd
.tbl_now_apply_xlim <- function(plot, xlim) {
  if (is.null(xlim)) {
    return(plot)
  }
  if (length(xlim) != 2) {
    cli::cli_abort("x-axis limits must be a length-2 vector.")
  }
  plot + ggplot2::coord_cartesian(xlim = xlim)
}


# Panel selection-----

#' The full, ordered set of panel keys applicable to an event unit
#'
#' @param event_units The event units (`"days"`, `"weeks"`, `"months"`, ...).
#' @param holiday_config A list from `.tbl_now_holiday_config()`, or `NULL`. The
#'   holiday panels are available only when the object carries a holiday or
#'   weekend temporal effect for them to describe.
#'
#' @return An ordered character vector of concrete panel keys.
#'
#' @keywords internal
#' @noRd
.tbl_now_all_panel_keys <- function(event_units, holiday_config = NULL) {
  calendar_groupings <- c(
    .tbl_now_calendar_groupings(event_units),
    .tbl_now_holiday_groupings(holiday_config)
  )
  c(
    "delay_distribution",
    "epidemic",
    if (length(calendar_groupings) > 0) paste0("calendar_", calendar_groupings),
    "seasonality",
    if (length(calendar_groupings) > 0) paste0("delay_", calendar_groupings),
    "delay_seasonality"
  )
}

#' Resolve the `panels` argument into an ordered vector of concrete panel keys
#'
#' Expands the aliases (`"all"`, `"calendar"`, `"delay_calendar"`), validates the
#' requested keys, drops (with a warning) any that do not apply to the event
#' unit, and returns them in canonical order.
#'
#' @param panels The user's `panels` argument (character vector or `NULL`).
#' @param event_units The event units.
#' @param holiday_config A list from `.tbl_now_holiday_config()`, or `NULL`.
#'
#' @return An ordered character vector of concrete panel keys.
#'
#' @keywords internal
#' @noRd
.tbl_now_resolve_panels <- function(panels, event_units, holiday_config = NULL) {
  all_keys <- .tbl_now_all_panel_keys(event_units, holiday_config)
  calendar_groupings <- c(
    .tbl_now_calendar_groupings(event_units),
    .tbl_now_holiday_groupings(holiday_config)
  )

  if (is.null(panels)) panels <- "all"
  if (!is.character(panels)) {
    cli::cli_abort("{.arg panels} must be a character vector of panel names.")
  }

  expand_alias <- function(key) {
    switch(key,
      all            = all_keys,
      calendar       = paste0("calendar_", calendar_groupings),
      delay_calendar = paste0("delay_", calendar_groupings),
      key
    )
  }
  requested <- unique(unlist(lapply(panels, expand_alias)))

  known_keys <- c(
    "delay_distribution", "epidemic",
    "calendar_weekday", "calendar_week", "calendar_month",
    "calendar_holiday", "calendar_holiday_lag", "seasonality",
    "delay_weekday", "delay_week", "delay_month",
    "delay_holiday", "delay_holiday_lag", "delay_seasonality"
  )
  aliases <- c("all", "calendar", "delay_calendar")
  unknown <- setdiff(requested, known_keys)
  if (length(unknown) > 0) {
    cli::cli_abort(c(
      "Unknown panel{?s} {.val {unknown}}.",
      "i" = "Choose from {.val {known_keys}}, or the aliases {.val {aliases}}."
    ))
  }

  # A requested panel can be unavailable for two different reasons, and they have
  # two different fixes, so say which one applies.
  holiday_keys <- c(
    "calendar_holiday", "calendar_holiday_lag",
    "delay_holiday", "delay_holiday_lag"
  )
  inapplicable <- setdiff(requested, all_keys)
  unavailable_holiday <- intersect(inapplicable, holiday_keys)
  unavailable_unit <- setdiff(inapplicable, holiday_keys)

  if (length(unavailable_unit) > 0) {
    cli::cli_warn(
      "{cli::qty(unavailable_unit)}Panel{?s} {.val {unavailable_unit}} {?does/do} \\
       not apply to {.val {event_units}} data and {?was/were} skipped."
    )
  }
  if (length(unavailable_holiday) > 0) {
    cli::cli_warn(c(
      "{cli::qty(unavailable_holiday)}Panel{?s} {.val {unavailable_holiday}} \\
       need{?s/} a holiday or weekend temporal effect and {?was/were} skipped.",
      "i" = "Attach one with {.fn add_temporal_effects}, e.g. \\
             {.code temporal_effects(weekend = TRUE, holidays = almanac::cal_us_federal())}. \\
             The lag panels also need a non-zero {.arg holiday_lags}."
    ))
  }
  requested <- intersect(requested, all_keys)

  if (length(requested) == 0) {
    cli::cli_abort("No applicable panels were selected.")
  }

  all_keys[all_keys %in% requested]
}

#' Build a single diagnostic panel by key
#'
#' @param key A concrete panel key (see `.tbl_now_all_panel_keys()`).
#' @param ctx A list of the shared panel data (see `autoplot.tbl_now()`).
#' @param palette A named colour palette.
#'
#' @return A ggplot object.
#'
#' @keywords internal
#' @noRd
.tbl_now_build_panel <- function(key, ctx, palette) {
  if (isTRUE(ctx$by_strata)) {
    return(.tbl_now_build_panel_strata(key, ctx, palette))
  }
  switch(key,
    delay_distribution = if (identical(ctx$data_type, "count-cumulative")) {
      .tbl_now_panel_delay_cumulative(ctx$delay_growth, palette)
    } else {
      .tbl_now_panel_delay(ctx$delay_distribution, palette)
    },
    epidemic = .tbl_now_panel_epidemic(
      ctx$epidemic_process, ctx$incomplete_threshold, ctx$level, palette,
      ctx$holiday_points
    ),
    seasonality = .tbl_now_panel_periodogram(
      ctx$epidemic_process, ctx$event_units, palette
    ),
    delay_seasonality = .tbl_now_panel_delay_periodogram(
      ctx$delay_per_date, ctx$event_units, palette
    ),
    calendar_weekday = .tbl_now_panel_calendar(
      ctx$epidemic_process, "weekday", palette, ctx$holiday_config, ctx$measure
    ),
    calendar_week = .tbl_now_panel_calendar(
      ctx$epidemic_process, "week", palette, ctx$holiday_config, ctx$measure
    ),
    calendar_month = .tbl_now_panel_calendar(
      ctx$epidemic_process, "month", palette, ctx$holiday_config, ctx$measure
    ),
    calendar_holiday = .tbl_now_panel_calendar(
      ctx$epidemic_process, "holiday", palette, ctx$holiday_config, ctx$measure
    ),
    calendar_holiday_lag = .tbl_now_panel_calendar(
      ctx$epidemic_process, "holiday_lag", palette, ctx$holiday_config, ctx$measure
    ),
    delay_weekday = .tbl_now_panel_delay_calendar(
      ctx$delay_per_date, "weekday", palette, ctx$holiday_config,
      ctx$measure, ctx$reporting_process
    ),
    delay_week = .tbl_now_panel_delay_calendar(
      ctx$delay_per_date, "week", palette, ctx$holiday_config,
      ctx$measure, ctx$reporting_process
    ),
    delay_month = .tbl_now_panel_delay_calendar(
      ctx$delay_per_date, "month", palette, ctx$holiday_config,
      ctx$measure, ctx$reporting_process
    ),
    delay_holiday = .tbl_now_panel_delay_calendar(
      ctx$delay_per_date, "holiday", palette, ctx$holiday_config,
      ctx$measure, ctx$reporting_process
    ),
    delay_holiday_lag = .tbl_now_panel_delay_calendar(
      ctx$delay_per_date, "holiday_lag", palette, ctx$holiday_config,
      ctx$measure, ctx$reporting_process
    ),
    .tbl_now_empty_panel(paste0("Unknown panel: ", key), palette)
  )
}

#' Build a single by-strata diagnostic panel by key
#'
#' @param key A concrete panel key (see `.tbl_now_all_panel_keys()`).
#' @param ctx A list of the shared panel data (see `autoplot.tbl_now()`), with
#'   the by-strata frames populated.
#' @param palette A named colour palette.
#'
#' @return A ggplot object.
#'
#' @keywords internal
#' @noRd
.tbl_now_build_panel_strata <- function(key, ctx, palette) {
  incidence_series <- function() {
    dplyr::tibble(
      strata = ctx$epidemic_process_by$strata,
      value  = ctx$epidemic_process_by$case_count
    )
  }
  delay_series <- function() {
    dplyr::tibble(
      strata = ctx$delay_per_date_by$strata,
      value  = ctx$delay_per_date_by$mean_delay
    )
  }

  switch(key,
    delay_distribution = if (identical(ctx$data_type, "count-cumulative")) {
      .tbl_now_panel_delay_cumulative_strata(ctx$delay_growth_by, palette)
    } else {
      .tbl_now_panel_delay_strata(ctx$delay_distribution_by, palette)
    },
    epidemic = .tbl_now_panel_epidemic_strata(
      ctx$epidemic_process_by, ctx$incomplete_threshold, ctx$level, palette
    ),
    seasonality = .tbl_now_periodogram_by(
      incidence_series(), ctx$event_units, palette,
      title = "Cycles (periodogram)",
      subtitle = .tbl_now_process_style("epidemic", palette)$subtitle,
      empty_message = "Too few points to estimate cycles"
    ),
    delay_seasonality = .tbl_now_periodogram_by(
      delay_series(), ctx$event_units, palette,
      title = "Cycles (periodogram)",
      subtitle = .tbl_now_process_style("reporting", palette)$subtitle,
      empty_message = "Too few points to estimate delay cycles"
    ),
    calendar_weekday = .tbl_now_panel_calendar_strata(
      ctx$epidemic_process_by, "weekday", palette, ctx$holiday_config, ctx$measure
    ),
    calendar_week = .tbl_now_panel_calendar_strata(
      ctx$epidemic_process_by, "week", palette, ctx$holiday_config, ctx$measure
    ),
    calendar_month = .tbl_now_panel_calendar_strata(
      ctx$epidemic_process_by, "month", palette, ctx$holiday_config, ctx$measure
    ),
    calendar_holiday = .tbl_now_panel_calendar_strata(
      ctx$epidemic_process_by, "holiday", palette, ctx$holiday_config, ctx$measure
    ),
    calendar_holiday_lag = .tbl_now_panel_calendar_strata(
      ctx$epidemic_process_by, "holiday_lag", palette, ctx$holiday_config, ctx$measure
    ),
    delay_weekday = .tbl_now_panel_delay_calendar_strata(
      ctx$delay_per_date_by, "weekday", palette, ctx$holiday_config,
      ctx$measure, ctx$reporting_process_by
    ),
    delay_week = .tbl_now_panel_delay_calendar_strata(
      ctx$delay_per_date_by, "week", palette, ctx$holiday_config,
      ctx$measure, ctx$reporting_process_by
    ),
    delay_month = .tbl_now_panel_delay_calendar_strata(
      ctx$delay_per_date_by, "month", palette, ctx$holiday_config,
      ctx$measure, ctx$reporting_process_by
    ),
    delay_holiday = .tbl_now_panel_delay_calendar_strata(
      ctx$delay_per_date_by, "holiday", palette, ctx$holiday_config,
      ctx$measure, ctx$reporting_process_by
    ),
    delay_holiday_lag = .tbl_now_panel_delay_calendar_strata(
      ctx$delay_per_date_by, "holiday_lag", palette, ctx$holiday_config,
      ctx$measure, ctx$reporting_process_by
    ),
    .tbl_now_empty_panel(paste0("Unknown panel: ", key), palette)
  )
}

#' The x-axis limit that applies to a given panel key
#'
#' @param key A concrete panel key.
#' @param xlims A named list of the four x-limit arguments.
#'
#' @return A length-2 vector, or `NULL` when the panel takes no x-limit.
#'
#' @keywords internal
#' @noRd
.tbl_now_panel_xlim <- function(key, xlims) {
  if (grepl("^calendar_", key)) {
    return(xlims$calendar_effect)
  }
  switch(key,
    delay_distribution = xlims$delay_distribution,
    epidemic           = xlims$event_date,
    seasonality        = xlims$seasonality,
    NULL
  )
}


# autoplot-----

# Re-export the ggplot2 `autoplot()` generic so users (and examples) can call
# `autoplot()` directly after `library(tbl.now)` without attaching ggplot2.
#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot

#' Diagnostic `autoplot` for a `tbl_now`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Produces a multi-panel diagnostic overview of a `tbl_now` using
#' [ggplot2::ggplot()] and \pkg{patchwork}. Two families of panels are available
#' — one describing the **case counts** and one describing the **reporting
#' delay** — and you choose which to draw with the `panels` argument.
#'
#' **Case-count panels**
#'
#' * `"delay_distribution"` — a (case-count weighted) histogram of the reporting
#'   delay (`.delay`). For **`count-cumulative`** data this panel instead shows the
#'   *cumulative growth by delay*: boxplots (on a log scale, with a dashed
#'   reference at `1`) of the ratio of each event date's cumulative count at a
#'   delay to its cumulative count at the previous delay. A ratio above `1` is an
#'   upward revision, below `1` a downward one, and the boxes converge to `1` as
#'   reporting completes.
#' * `"epidemic"` — the latest reported case counts per `event_date`, with a
#'   dashed vertical line marking where the data become incomplete (less than
#'   `level` of the delay distribution has arrived). Holidays from the attached
#'   [temporal_effects()] spec are marked with dots.
#' * `"calendar_weekday"`, `"calendar_week"`, `"calendar_month"` — boxplots of
#'   the *normalized* case effect (each event date's cases divided by the overall
#'   mean, so 1 is average) by day of week, epidemiological week, or month.
#' * `"calendar_holiday"` — the same normalized boxplots by **day type**. The
#'   categories follow the attached [temporal_effects()] spec: a holiday calendar
#'   and a `weekend` effect together give `Weekday` / `Weekend` / `Holiday`, a
#'   calendar alone gives `Non-holiday` / `Holiday`, and a `weekend` effect alone
#'   gives `Weekday` / `Weekend`. A holiday falling on a weekend counts as a
#'   holiday.
#' * `"calendar_holiday_lag"` — the same normalized boxplots by **position
#'   relative to the nearest holiday**, as asked for by `holiday_lags` (see
#'   [temporal_effects()]): `"2 before"`, `"1 before"`, `"Holiday"`, `"1 after"`,
#'   ..., plus `"Other"` for every other day as the reference. It shows exactly
#'   the days the `..._holiday_lag_k` / `..._holiday_lead_k` columns flag —
#'   weekends and other holidays are skipped when counting working days — so you
#'   can see whether the lags you asked for are the ones that matter.
#' * `"seasonality"` — a **cycles** periodogram of the incidence series whose
#'   dominant peak suggests a Fourier season length for [temporal_effects()].
#'
#' **Reporting-delay panels** (to inspect *delay effects*)
#'
#' * `"delay_weekday"`, `"delay_week"`, `"delay_month"` — boxplots of the
#'   *normalized* mean reporting delay (each event date's mean delay divided by
#'   the overall mean delay, so 1 is average) by day of week, epidemiological
#'   week, or month; these reveal whether the delay itself has a calendar
#'   pattern. Normalizing keeps them on the same scale as the case-count
#'   calendar panels and makes them comparable across strata.
#' * `"delay_holiday"`, `"delay_holiday_lag"` — the reporting-delay twins of the
#'   two holiday panels above: the *normalized* mean delay by day type and by
#'   position relative to the nearest holiday. These are often the more telling
#'   pair — a holiday usually does not change how many cases occur, but it very
#'   much changes how long they take to be reported.
#' * `"delay_seasonality"` — a **cycles** periodogram of the mean-delay series,
#'   whose peak marks a cycle in the reporting delay (e.g. a weekly reporting
#'   rhythm).
#'
#' Every panel is colour-coded by the process it describes — **red** for the
#' reporting-delay panels, **green** for the case-count (epidemic) ones — and
#' says which one it is in its subtitle, so a single panel still reads on its own.
#'
#' Which panels are available depends on the object. The **calendar/delay**
#' panels follow the event unit: daily data offers day-of-week **and**
#' week-of-year panels, weekly data week-of-year, monthly data month-of-year. The
#' four **holiday** panels describe the attached [temporal_effects()] spec, so
#' they appear only when there is one to describe: `"calendar_holiday"` /
#' `"delay_holiday"` need a `holidays` calendar or a `weekend` effect, and the
#' two lag panels additionally need a non-zero `holiday_lags`. Requesting a
#' holiday panel without the matching effect warns and skips it. The spec is read
#' directly, so you do **not** need to call [compute_temporal_effects()] first.
#'
#' The delay panels are computed on the *complete* portion of the series (event
#' dates on or before the incompleteness line) so the recent reporting truncation
#' does not bias them.
#'
#' @param object A `tbl_now` object.
#' @param panels Which panels to draw. Either a vector of the concrete keys
#'   listed above, or one of the aliases `"all"` (default; every applicable
#'   panel), `"calendar"` (the case-count calendar panels) or `"delay_calendar"`
#'   (the reporting-delay calendar panels). Selecting a single panel returns that
#'   panel as a plain \pkg{ggplot2} object instead of a \pkg{patchwork}.
#' @param by_strata Logical (default `FALSE`). When `TRUE`, every panel is split
#'   by stratum: the calendar / delay boxplots become dodged boxes (one per
#'   stratum, side by side), the epidemic process and both periodograms become
#'   one coloured line per stratum (no area fill), and the delay distribution
#'   becomes dodged bars (one per stratum). The boxplots are then normalized
#'   **per stratum** (1 = that stratum's own average) so the calendar pattern is
#'   comparable across strata. Colours use a \pkg{viridis} scale. In this mode the
#'   holiday dots are omitted from the epidemic panel.
#' @param strata Character vector of column names to group by when
#'   `by_strata = TRUE`. `NULL` (default) uses the object's `strata` (see
#'   [get_strata()]); pass a subset (e.g. `strata = "gender"`) to group by only
#'   some of them. Ignored when `by_strata = FALSE`.
#' @param measure How to express the calendar-effect boxplots (the day-of-week,
#'   week-of-year, month-of-year, holiday and holiday-lag panels; every other
#'   panel ignores it).
#'
#'   * `"normalized"`  — the value divided by its overall mean, so `1`
#'     (the dashed line) marks an average level. Case-count panels normalize the
#'     cases per event date; delay panels normalize the mean reporting delay.
#'   * `"percent"` (default) — the **share of cases** falling in each group, as a
#'     percentage, so the box reads directly as "10% of cases at the weekend
#'     versus 90% on weekdays" with the IQR around it. One observation per
#'     calendar block: the seven weekdays (and the day types) are shared out
#'     within each **week**, the holiday lags within each **month**, and the
#'     epidemiological weeks and months within each **year**. The reporting-delay
#'     panels then switch from the event date to the **report date**, so they
#'     answer "what share of the reports *arrive* on a weekend?". Needs `Date`
#'     event/report columns.
#' @param level Completeness level used for the incompleteness line in the
#'   `"epidemic"` panel (and to trim the delay panels). The line is drawn at
#'   `now - q`, where `q` is the `level` quantile of the delay distribution. With
#'   the default `0.95`, the line marks where at least 5 percent of delays are
#'   yet to arrive.
#' @param plotly If `TRUE`, return an interactive \pkg{plotly} widget (the panels
#'   stacked) instead of a static \pkg{patchwork}. Default `FALSE`.
#' @param palette A named character vector of colours. Defaults to the package
#'   palette.
#' @param delay_distribution_xlim,event_date_xlim,calendar_effect_xlim,seasonality_xlim
#'   Optional length-2 vectors giving the x-axis limits for the corresponding
#'   panel (delay-distribution histogram, epidemic process, calendar-effect
#'   boxplots, incidence periodogram). `NULL` (default) lets each panel pick its
#'   own range. For `event_date_xlim` pass `Date`s; the others take numeric
#'   limits.
#' @param ... Unused; present for compatibility with [ggplot2::autoplot()].
#'
#' @return A \pkg{patchwork} object combining the selected panels, or — when a
#'   single panel is selected — that panel as a \pkg{ggplot2} object.
#'
#' @examplesIf requireNamespace("patchwork", quietly = TRUE)
#' data(denguedat)
#' # A recent window keeps the example fast.
#' recent <- denguedat[denguedat$onset_week >= as.Date("2010-01-01"), ]
#' dengue <- tbl_now(recent,
#'   event_date = "onset_week",
#'   report_date = "report_week", strata = "gender", verbose = FALSE
#' )
#' autoplot(dengue)
#'
#' \donttest{
#' # Only the reporting-delay calendar effect
#' autoplot(dengue, panels = "delay_calendar")
#'
#' # A single panel (returned as a plain ggplot)
#' autoplot(dengue, panels = "delay_week")
#'
#' # Split every panel by stratum
#' autoplot(dengue, by_strata = TRUE)
#'
#' # Zoom the delay panel to delays of 0-10 weeks
#' autoplot(dengue, delay_distribution_xlim = c(0, 10))
#' }
#' @seealso
#' [diagnostic_plot()] for the companion gallery, which looks at the *reporting
#' process* -- when reports arrived and whether any of it is artificial -- rather
#' than at the case counts;
#' the panels here as standalone functions: [plot_observed_cases()],
#' [plot_delay_distribution()], [plot_cycles()], [calendar_effect_plots] and
#' [plot_delay_drift()];
#' [summary()][tbl_now_summary] and [diagnose()] for the same information as
#' tables. The
#' [*Describing and diagnosing a tbl_now* article](https://rodrigozepeda.github.io/tbl.now/articles/describing-and-diagnosing.html)
#' reads the panels one at a time.
#'
#' @importFrom rlang .data
#' @importFrom ggplot2 autoplot
#' @exportS3Method ggplot2::autoplot
autoplot.tbl_now <- function(object, ..., panels = "all", by_strata = FALSE,
                             strata = NULL,
                             measure = c("percent", "normalized"),
                             level = 0.95, plotly = FALSE,
                             palette = .tbl_now_palette(),
                             delay_distribution_xlim = NULL,
                             event_date_xlim = NULL,
                             calendar_effect_xlim = NULL,
                             seasonality_xlim = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg ggplot2} is required for {.fn autoplot}.")
  }
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg patchwork} is required for {.fn autoplot}.")
  }
  if (!is_tbl_now(object)) {
    cli::cli_abort("{.arg object} must be a {.cls tbl_now}.")
  }
  if (!is.numeric(level) || length(level) != 1 || level < 0 || level > 1) {
    cli::cli_abort("{.arg level} must be a single number between 0 and 1.")
  }
  if (!rlang::is_bool(by_strata)) {
    cli::cli_abort("{.arg by_strata} must be a single {.val TRUE} or {.val FALSE}.")
  }
  measure <- match.arg(measure)

  object <- ungroup(object)
  event_units <- get_event_units(object)
  data_type <- get_data_type(object)

  # The holiday panels describe the attached temporal-effects spec, so which of
  # them exist depends on the object, not just on its time unit.
  holiday_config <- .tbl_now_holiday_config(object)
  panel_keys <- .tbl_now_resolve_panels(panels, event_units, holiday_config)

  strata_cols <- if (isTRUE(by_strata)) {
    .tbl_now_resolve_strata_cols(object, strata)
  } else {
    NULL
  }

  # `strata` may name columns that are not declared strata on the object. Every
  # panel pre-aggregates with `to_count()`, which sums over each column it was
  # not told to keep, so an undeclared column is summed away before the panels
  # can group on it. Declaring them first makes `autoplot(x, strata = "race")`
  # behave like `autoplot(add_strata(x, race))`.
  undeclared_strata <- setdiff(strata_cols, get_strata(object))
  if (length(undeclared_strata) > 0) {
    object <- add_strata(object, dplyr::all_of(undeclared_strata))
  }

  delay_distribution <- .tbl_now_delay_distribution(object)

  # Date beyond which less than `level` of the delay has arrived
  delay_quantile <- .tbl_now_weighted_quantile(
    delay_distribution$delay, delay_distribution$weight, level
  )
  now_value <- get_now(object)
  incomplete_threshold <- if (is.na(delay_quantile) || level == 1 || level == 0) {
    NA
  } else if (lubridate::is.Date(now_value)) {
    now_value - delay_quantile * .tbl_now_units_to_days(event_units)
  } else {
    now_value - delay_quantile
  }

  # The reporting-delay panels use the complete portion of the series (event
  # dates on or before the incompleteness line) so recent reporting truncation
  # does not bias the mean delay.
  trim_to_complete <- function(delay_by_date) {
    if (!is.na(incomplete_threshold)) {
      complete_rows <- delay_by_date$event_date <= incomplete_threshold
      if (any(complete_rows)) {
        delay_by_date <- dplyr::filter(
          delay_by_date, .data$event_date <= incomplete_threshold
        )
      }
    }
    delay_by_date
  }
  delay_calendar_keys <- grep(
    "^delay_(weekday|week|month|holiday_lag|holiday)$", panel_keys, value = TRUE
  )
  is_percent <- identical(measure, "percent")
  # With `measure = "percent"` the delay calendar panels move to the report date
  # and share out report counts, so they need the reporting process instead of
  # the per-event-date mean delay.
  needs_delay_effects <- "delay_seasonality" %in% panel_keys ||
    (length(delay_calendar_keys) > 0 && !is_percent)
  needs_reporting_process <- is_percent && length(delay_calendar_keys) > 0
  # For count-cumulative data the delay-distribution panel becomes the cumulative
  # growth-ratio panel instead of a histogram of increments.
  is_cumulative <- identical(data_type, "count-cumulative")
  needs_growth <- is_cumulative && "delay_distribution" %in% panel_keys

  if (isTRUE(by_strata)) {
    # Only compute the by-strata frames that the requested panels need.
    epidemic_needed <- any(panel_keys %in% c(
      "epidemic", "calendar_weekday", "calendar_week", "calendar_month",
      "calendar_holiday", "calendar_holiday_lag", "seasonality"
    ))
    ctx <- list(
      by_strata            = TRUE,
      data_type            = data_type,
      event_units          = event_units,
      incomplete_threshold = incomplete_threshold,
      level                = level,
      measure              = measure,
      holiday_config       = holiday_config,
      reporting_process_by = if (needs_reporting_process) {
        .tbl_now_reporting_process(object, strata_cols)
      },
      epidemic_process_by  = if (epidemic_needed) {
        .tbl_now_epidemic_process_by(object, strata_cols)
      },
      delay_distribution_by = if ("delay_distribution" %in% panel_keys && !is_cumulative) {
        .tbl_now_delay_distribution_by(object, strata_cols)
      },
      delay_growth_by = if (needs_growth) {
        .tbl_now_cumulative_growth(object, strata_cols)
      },
      delay_per_date_by = if (needs_delay_effects) {
        trim_to_complete(.tbl_now_delay_per_date_by(object, strata_cols))
      }
    )
  } else {
    epidemic_process <- .tbl_now_epidemic_process(object)
    delay_per_date <- if (needs_delay_effects) {
      trim_to_complete(.tbl_now_delay_per_date(object))
    }
    ctx <- list(
      by_strata            = FALSE,
      data_type            = data_type,
      delay_distribution   = delay_distribution,
      delay_growth         = if (needs_growth) .tbl_now_cumulative_growth(object),
      epidemic_process     = epidemic_process,
      delay_per_date       = delay_per_date,
      event_units          = event_units,
      incomplete_threshold = incomplete_threshold,
      level                = level,
      measure              = measure,
      holiday_config       = holiday_config,
      reporting_process    = if (needs_reporting_process) {
        .tbl_now_reporting_process(object)
      },
      holiday_points       = .tbl_now_holiday_points(object, epidemic_process)
    )
  }

  shared_theme <- .tbl_now_theme(palette)
  xlims <- list(
    delay_distribution = delay_distribution_xlim,
    event_date         = event_date_xlim,
    calendar_effect    = calendar_effect_xlim,
    seasonality        = seasonality_xlim
  )

  built_panels <- lapply(panel_keys, function(key) {
    panel <- .tbl_now_build_panel(key, ctx, palette) + shared_theme
    .tbl_now_apply_xlim(panel, .tbl_now_panel_xlim(key, xlims))
  })

  # A single selected panel is returned as a plain ggplot for convenience.
  if (length(built_panels) == 1) {
    return(.as_plotly(built_panels[[1]], plotly))
  }

  .combine_panels(built_panels, plotly = plotly, ncol = 2, byrow = TRUE,
                  title = "Automatic plot of effects", palette = palette)
}
