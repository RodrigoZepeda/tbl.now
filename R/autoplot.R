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

  ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$delay)) +
    ggplot2::geom_histogram(
      ggplot2::aes(weight = .data$normalised_weight),
      fill = palette[["light_red"]], colour = palette[["accent_red"]],
      binwidth = 1, center = 0
    ) +
    ggplot2::labs(
      title = "Empirical delay distribution",
      x = "Reporting delay", y = "Density"
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
    ggplot2::geom_area(fill = palette[["light_red"]]) +
    ggplot2::geom_line(colour = palette[["accent_red"]])

  # Mark event dates that fall on a holiday (from the temporal-effects spec)
  if (!is.null(holiday_points) && nrow(holiday_points) > 0) {
    base_plot <- base_plot +
      ggplot2::geom_point(
        data = holiday_points,
        ggplot2::aes(x = .data$event_date, y = .data$case_count),
        colour = palette[["medium_green"]], size = 2
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
        colour = palette[["medium_green"]], linetype = "dashed", linewidth = 0.7
      ) +
      ggplot2::annotate(
        "label",
        x = incomplete_threshold, y = Inf,
        label = paste0("Incomplete (<", round(100 * level), "% reported)"),
        vjust = 1.1, hjust = 1.02, size = 3,
        colour = palette[["medium_green"]], fill = "white"
      )
  }

  subtitle <- if (!is.null(holiday_points) && nrow(holiday_points) > 0) {
    "Line: incompleteness threshold; red dots: holidays"
  } else {
    "Events to the right of the line are still incomplete"
  }

  base_plot +
    ggplot2::labs(
      title = "Observed epidemic process",
      subtitle = subtitle,
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
  specs <- get_temporal_effects(object)
  if (length(specs) == 0) {
    return(epidemic_process[0, , drop = FALSE])
  }

  calendars <- Filter(Negate(is.null), lapply(specs, function(s) s$t_effects@holidays))
  if (length(calendars) == 0) {
    return(epidemic_process[0, , drop = FALSE])
  }

  if (!requireNamespace("almanac", quietly = TRUE)) {
    cli::cli_warn("Package {.pkg almanac} is needed to mark holidays; skipping holiday dots.")
    return(epidemic_process[0, , drop = FALSE])
  }
  if (!lubridate::is.Date(epidemic_process$event_date)) {
    return(epidemic_process[0, , drop = FALSE])
  }

  is_holiday <- rep(FALSE, nrow(epidemic_process))
  for (calendar in calendars) {
    is_holiday <- is_holiday | almanac::alma_in(epidemic_process$event_date, calendar)
  }
  epidemic_process[is_holiday, , drop = FALSE]
}

#' Panel: calendar effect (normalized boxplots)
#'
#' Boxplots of a *normalized* effect: each event date's reported cases divided
#' by the overall mean (so 1 marks an average level).
#'
#' @param epidemic_process A tibble from `.tbl_now_epidemic_process()`.
#' @param grouping One of `"weekday"`, `"week"` or `"month"`.
#' @param palette A named colour palette.
#'
#' @return A ggplot object (or an empty panel when there are no cases).
#'
#' @keywords internal
#' @noRd
.tbl_now_panel_calendar <- function(epidemic_process, grouping, palette) {
  overall_mean <- mean(epidemic_process$case_count, na.rm = TRUE)
  if (is.na(overall_mean) || overall_mean == 0) {
    return(.tbl_now_empty_panel("No cases to compute a calendar effect", palette))
  }

  if (grouping == "weekday") {
    grouped <- dplyr::mutate(epidemic_process, calendar_group = lubridate::wday(
      .data$event_date,
      label = TRUE, abbr = FALSE, week_start = 1
    ))
    panel_title <- "Day-of-week effect"
    x_label <- "Day of week"
  } else if (grouping == "week") {
    grouped <- dplyr::mutate(epidemic_process,
      calendar_group = factor(lubridate::epiweek(.data$event_date))
    )
    panel_title <- "Week-of-year effect"
    x_label <- "Epidemiological week"
  } else if (grouping == "month") {
    grouped <- dplyr::mutate(epidemic_process, calendar_group = lubridate::month(
      .data$event_date,
      label = TRUE, abbr = TRUE
    ))
    panel_title <- "Month-of-year effect"
    x_label <- "Month"
  } else {
    return(.tbl_now_empty_panel(
      paste0("Calendar effect unavailable for ", grouping), palette
    ))
  }

  plot_data <- dplyr::mutate(grouped,
    normalized_effect = .data$case_count / overall_mean
  )

  ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$calendar_group, y = .data$normalized_effect)) +
    ggplot2::geom_boxplot(
      fill = palette[["light_red"]], colour = palette[["accent_red"]],
      outlier.colour = palette[["near_black"]], outlier.size = 0.6, linewidth = 0.4
    ) +
    ggplot2::geom_hline(
      yintercept = 1, linetype = "dashed",
      colour = palette[["near_black"]], linewidth = 0.4
    ) +
    ggplot2::labs(
      title = panel_title,
      subtitle = "Normalized distribution of cases (1 = average)",
      x = x_label, y = "Normalized effect"
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

#' Panel: seasonality periodogram
#'
#' A periodogram ([stats::spec.pgram()]) of the incidence series whose dominant
#' peak suggests a Fourier season length.
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
  case_count_series <- epidemic_process$case_count
  if (length(case_count_series) < 8 || stats::var(case_count_series) == 0) {
    return(.tbl_now_empty_panel("Too few points to estimate seasonality", palette))
  }

  spectrum <- tryCatch(
    stats::spec.pgram(stats::ts(case_count_series),
      detrend = TRUE,
      taper = 0.1, plot = FALSE
    ),
    error = function(e) NULL
  )
  if (is.null(spectrum)) {
    return(.tbl_now_empty_panel("Could not estimate a periodogram", palette))
  }

  periodogram <- dplyr::tibble(
    period = 1 / spectrum$freq,
    power  = spectrum$spec
  ) |>
    dplyr::filter(.data$period <= length(case_count_series) / 2)

  dominant_period <- periodogram$period[which.max(periodogram$power)]
  unit_label <- switch(event_units,
    days = "days",
    weeks = "weeks",
    months = "months",
    years = "years",
    "units"
  )

  ggplot2::ggplot(periodogram, ggplot2::aes(x = .data$period, y = .data$power)) +
    ggplot2::geom_line(colour = palette[["light_red"]], linewidth = 0.7) +
    ggplot2::geom_vline(
      xintercept = dominant_period,
      colour = palette[["medium_green"]], linetype = "dashed", linewidth = 0.7
    ) +
    ggplot2::annotate(
      "label",
      x = dominant_period, y = Inf,
      label = paste0("~", round(dominant_period, 1), " ", unit_label),
      vjust = 1.1, hjust = -0.05, size = 3,
      colour = palette[["medium_green"]], fill = "white"
    ) +
    ggplot2::labs(
      title = "Seasonality (periodogram)",
      subtitle = "Peak period suggests a Fourier season length",
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
#' Produces a four-panel diagnostic overview of a `tbl_now` using
#' [ggplot2::ggplot()] and \pkg{patchwork}:
#'
#' 1. **Empirical delay distribution** — a (case-count weighted) kernel
#'    density of the reporting delay (`.delay`).
#' 2. **Observed epidemic process** — the latest reported case counts per
#'    `event_date`, with a dashed vertical line marking where the data become
#'    incomplete (less than `level` of the delay distribution has arrived).
#' 3. **Calendar effect** — boxplots of the *normalized* effect (each event
#'    date's cases divided by the overall mean, so 1 is average) by calendar
#'    group. Daily data shows **both** a day-of-week and a week-of-year panel;
#'    weekly data shows week-of-year; monthly data shows month-of-year.
#' 4. **Seasonality** — a periodogram of the incidence series whose dominant
#'    peak suggests a Fourier season length for [temporal_effects()].
#'
#' If the attached [temporal_effects()] spec contains a holidays calendar, the
#' event dates that fall on a holiday are marked with dots in panel 2.
#'
#' @param object A `tbl_now` object.
#' @param level Completeness level used for the incompleteness line in panel 2.
#'   The line is drawn at `now - q`, where `q` is the `level` quantile of the
#'   delay distribution. With the default `0.95`, the line marks where at least
#'   5 percent of delays are yet to arrive.
#' @param palette A named character vector of colours. Defaults to the package
#'   palette.
#' @param delay_distribution_xlim,event_date_xlim,calendar_effect_xlim,seasonality_xlim
#'   Optional length-2 vectors giving the x-axis limits for the corresponding
#'   panel (delay density, epidemic process, calendar effect, periodogram).
#'   `NULL` (default) lets each panel pick its own range. For `event_date_xlim`
#'   pass `Date`s; the others take numeric limits.
#' @param ... Unused; present for compatibility with [ggplot2::autoplot()].
#'
#' @return A \pkg{patchwork} object combining the four panels.
#'
#' @examplesIf requireNamespace("patchwork", quietly = TRUE)
#' data(denguedat)
#' dengue <- tbl_now(denguedat,
#'   event_date = "onset_week",
#'   report_date = "report_week", verbose = FALSE
#' )
#' autoplot(dengue)
#'
#' # Zoom the delay panel to delays of 0-10 weeks
#' if (FALSE) {
#'   autoplot(dengue, delay_distribution_xlim = c(0, 10))
#' }
#' @importFrom rlang .data
#' @importFrom ggplot2 autoplot
#' @exportS3Method ggplot2::autoplot
autoplot.tbl_now <- function(object, ..., level = 0.95,
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

  object <- ungroup(object)
  event_units <- get_event_units(object)
  delay_distribution <- .tbl_now_delay_distribution(object)
  epidemic_process <- .tbl_now_epidemic_process(object)

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

  shared_theme <- .tbl_now_theme(palette)
  holiday_points <- .tbl_now_holiday_points(object, epidemic_process)

  panel_delay <- .tbl_now_panel_delay(delay_distribution, palette) + shared_theme
  panel_epidemic <- .tbl_now_panel_epidemic(
    epidemic_process, incomplete_threshold,
    level, palette, holiday_points
  ) + shared_theme
  panel_periodogram <- .tbl_now_panel_periodogram(epidemic_process, event_units, palette) + shared_theme

  # One calendar panel per grouping. Daily data yields both a day-of-week and a
  # week-of-year panel.
  calendar_groupings <- .tbl_now_calendar_groupings(event_units)
  if (length(calendar_groupings) == 0) {
    calendar_panels <- list(
      .tbl_now_empty_panel(
        paste0("Calendar effect unavailable for ", event_units, " data"), palette
      ) + shared_theme
    )
  } else {
    calendar_panels <- lapply(calendar_groupings, function(grouping) {
      .tbl_now_apply_xlim(
        .tbl_now_panel_calendar(epidemic_process, grouping, palette) + shared_theme,
        calendar_effect_xlim
      )
    })
  }

  # Apply optional per-panel x-axis limits
  panel_delay <- .tbl_now_apply_xlim(panel_delay, delay_distribution_xlim)
  panel_epidemic <- .tbl_now_apply_xlim(panel_epidemic, event_date_xlim)
  panel_periodogram <- .tbl_now_apply_xlim(panel_periodogram, seasonality_xlim)

  panels <- c(
    list(panel_delay, panel_epidemic),
    calendar_panels,
    list(panel_periodogram)
  )

  patchwork::wrap_plots(panels, ncol = 2) +
    patchwork::plot_annotation(
      title = "Diagnostic plots",
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", colour = palette[["near_black"]])
      )
    )
}
