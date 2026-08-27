# The confirmation process: a THIRD date, after the event and the report.
#
# Influenza is the motivating case. A case has
#
#   event_date         when the person fell ill (symptom onset)
#   report_date        when the system heard about it (the medical visit)
#   confirmation_date  when the laboratory settled it (the test result)
#
# and the test can come back either way, so a report is not the end of the
# story: it can be CONFIRMED (positive) or RETRACTED (negative, the case never
# was one). Until the result arrives the case is PENDING.
#
# The honest timeline is `event_date <= report_date <= confirmation_date`, and
# the package checks it rather than assuming it.
#
# Everything here is optional. An object with no `confirmation_date` behaves
# exactly as it did before this file existed.

#' The values `confirmation_type` may take
#'
#' `"pending"` is the default state of every case: reported, not yet resolved.
#' `"confirmed"` and `"retracted"` are the two ways a case leaves that state.
#' `NA` means the outcome is genuinely unknown -- which is what a
#' `confirmation_date` with no `confirmation_type` gives you, because a date
#' alone cannot say whether the test was positive or negative.
#'
#' @return A character vector of the allowed values.
#'
#' @keywords internal
#' @noRd
.confirmation_levels <- function() {
  c("confirmed", "retracted", "pending")
}

#' Build (or validate) the `confirmation_type` column
#'
#' @param data A data frame.
#' @param confirmation_date Name of the confirmation-date column, or `NULL`.
#' @param confirmation_type Name of the type column, or `NULL`.
#' @param verbose Logical.
#'
#' @return The data frame, with a `confirmation_type` column when one is needed.
#'
#' @keywords internal
#' @noRd
.resolve_confirmation_type <- function(data, confirmation_date, confirmation_type,
                                       verbose = TRUE) {
  if (is.null(confirmation_date)) {
    return(list(data = data, confirmation_type = confirmation_type))
  }

  dates <- data[[confirmation_date]]
  has_date <- !is.na(dates)

  if (is.null(confirmation_type)) {
    # Every case starts pending; a date resolves it -- but a date alone cannot
    # say WHICH way. A negative test has a date too, so calling it "confirmed"
    # would invert the meaning of the data. `NA` and a warning is the honest
    # answer: the outcome is recorded as unknown until the caller says.
    confirmation_type <- ".confirmation_type"
    data[[confirmation_type]] <- ifelse(has_date, NA_character_, "pending")

    if (any(has_date)) {
      allowed <- .confirmation_levels()
      cli::cli_warn(c(
        paste0(
          "{sum(has_date)} row{?s} have a {.arg confirmation_date} but no ",
          "{.arg confirmation_type}, so their outcome is {.val NA}."
        ),
        "i" = paste0(
          "A date alone cannot say whether the case was confirmed or ",
          "retracted -- a negative test has a date too."
        ),
        "i" = paste0(
          "Pass {.arg confirmation_type} (a column of {.val {allowed}}) ",
          "to say which."
        )
      ))
    }
    return(list(data = data, confirmation_type = confirmation_type))
  }

  values <- as.character(data[[confirmation_type]])
  # A row with no date has not been resolved, whatever the column says.
  values[!has_date & is.na(values)] <- "pending"

  allowed <- .confirmation_levels()
  unknown <- setdiff(stats::na.omit(unique(values)), allowed)
  if (length(unknown) > 0) {
    cli::cli_abort(c(
      paste0(
        "{.arg confirmation_type} contains {length(unknown)} unrecognised ",
        "value{?s}: {.val {unknown}}."
      ),
      "i" = "Allowed values are {.val {allowed}}, or {.val NA}."
    ))
  }

  # A resolved outcome with no date is a contradiction worth naming.
  resolved_without_date <- !has_date & values %in% c("confirmed", "retracted")
  if (any(resolved_without_date) && isTRUE(verbose)) {
    cli::cli_warn(
      "{sum(resolved_without_date)} row{?s} are {.val confirmed}/{.val retracted}
       but carry no {.arg confirmation_date}."
    )
  }

  data[[confirmation_type]] <- values
  list(data = data, confirmation_type = confirmation_type)
}

# Getters -----

#' Confirmation attributes of a `tbl_now`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A `tbl_now` may carry a **third** date beyond the event and the report: the
#' date a case was resolved, either confirmed or retracted. Think of influenza:
#' symptom onset is the event, the medical visit is the report, and the
#' laboratory result is the confirmation -- which can come back negative, in
#' which case the case is *retracted* rather than confirmed.
#'
#' These getters return the **column names** the object was told about, not the
#' data. Get the values with `x[[get_confirmation_date(x)]]`, as with every
#' other getter in the package.
#'
#' @param x A `tbl_now` object.
#'
#' @return
#' * `get_confirmation_date()` -- the confirmation-date column name, or `NULL`.
#' * `get_confirmation_type()` -- the outcome column name, or `NULL`.
#' * `get_confirmation_units()` -- `"days"`, `"weeks"`, `"months"`, `"years"`,
#'   `"numeric"`, or `NULL` when the object carries no confirmation.
#' * `has_confirmation()` -- `TRUE` when the object carries a confirmation date.
#'
#' @seealso [add_confirmation()] to attach one, [get_latest_confirmed()] and
#'   [get_net_confirmed()] to count the outcomes, [nowcast_data_getters].
#'
#' @examples
#' data(hai_bucaramanga)
#'
#' # Specimen taken -> received at the laboratory -> result reported.
#' hai <- hai_bucaramanga |>
#'   dplyr::filter(!is.na(specimen_date), !is.na(report_date)) |>
#'   tbl_now(
#'     event_date = specimen_date,
#'     report_date = report_date,
#'     data_type = "linelist",
#'     verbose = FALSE
#'   )
#'
#' has_confirmation(hai)
#' get_confirmation_date(hai)
#'
#' @name confirmation_getters
NULL

#' @rdname confirmation_getters
#' @export
get_confirmation_date <- function(x) {
  attr(x, "confirmation_date", exact = TRUE)
}

#' @rdname confirmation_getters
#' @export
get_confirmation_type <- function(x) {
  attr(x, "confirmation_type", exact = TRUE)
}

#' @rdname confirmation_getters
#' @export
get_confirmation_units <- function(x) {
  attr(x, "confirmation_units", exact = TRUE)
}

#' @rdname confirmation_getters
#' @export
has_confirmation <- function(x) {
  !is.null(get_confirmation_date(x))
}

#' The generated confirmation columns, when the object has any
#'
#' `.confirmation_num` is the confirmation date on the same numeric anchor as
#' `.event_num` and `.report_num`; `.confirmation_delay` is
#' `.confirmation_num - .report_num`, the time from report to resolution. That
#' second one is the quantity [diagnose_confirmation_delay()] compares between
#' confirmed and retracted cases.
#'
#' @param x A `tbl_now` object.
#'
#' @return A character vector, empty when the object carries no confirmation.
#'
#' @keywords internal
#' @noRd
.confirmation_generated_cols <- function(x) {
  if (!has_confirmation(x)) {
    return(character(0))
  }
  c(".confirmation_num", ".confirmation_delay")
}

#' Add `.confirmation_num` and `.confirmation_delay`
#'
#' Anchored on the same earliest event date `time_cols_to_numeric()` uses, so
#' `.event_num`, `.report_num` and `.confirmation_num` are on one scale and
#' differences between them mean what they look like.
#'
#' @param data A data frame that already has `.report_num`.
#' @param event_date,confirmation_date Column names.
#' @param confirmation_units The confirmation date's units.
#' @param force Overwrite reserved columns rather than aborting.
#'
#' @return The data frame with the two columns added.
#'
#' @keywords internal
#' @noRd
.add_confirmation_num <- function(data, event_date, confirmation_date,
                                  confirmation_units, force = FALSE) {
  for (reserved in c(".confirmation_num", ".confirmation_delay")) {
    if (reserved %in% colnames(data) && !force) {
      cli::cli_abort(
        "Data already has a column named {.val {reserved}}, which this class
         uses. Rename it, or pass {.code force = TRUE}."
      )
    }
  }

  anchor <- suppressWarnings(min(data[[event_date]], na.rm = TRUE))
  confirmation <- data[[confirmation_date]]

  data[[".confirmation_num"]] <- if (identical(confirmation_units, "numeric")) {
    as.numeric(confirmation) - as.numeric(anchor)
  } else {
    .date_difference_in_units(confirmation, anchor, confirmation_units)
  }
  data[[".confirmation_delay"]] <- data[[".confirmation_num"]] - data[[".report_num"]]
  data
}

#' Difference between two dates, expressed in a unit
#'
#' @param x,anchor Dates.
#' @param units One of `"days"`, `"weeks"`, `"months"`, `"years"`.
#'
#' @return A numeric vector.
#'
#' @keywords internal
#' @noRd
.date_difference_in_units <- function(x, anchor, units) {
  days <- as.numeric(difftime(x, anchor, units = "days"))
  switch(units,
    days = days,
    weeks = days / 7,
    months = days / 30.4375,
    years = days / 365.25,
    days
  )
}

# Setters -----

#' Attach, change or drop a confirmation process
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A confirmation is the **third** date in a surveillance record: after the event
#' happened and after it was reported, somebody decided whether it was real. For
#' influenza that is the laboratory result -- and it can come back negative, in
#' which case the case is *retracted* rather than confirmed.
#'
#' `add_confirmation()` attaches one to an object that has none;
#' `change_confirmation()` replaces whatever is there; `remove_confirmation()`
#' drops it, leaving an ordinary two-date `tbl_now`.
#'
#' @param x A `tbl_now` object.
#' @param confirmation_date The confirmation-date column (tidy-select: a bare
#'   name or a string).
#' @param confirmation_type Optional column holding `"confirmed"`,
#'   `"retracted"` or `"pending"`. When you leave it out, rows with a
#'   confirmation date get `NA` and a warning: a date on its own cannot say
#'   whether the test came back positive or negative.
#' @param confirmation_units `"auto"` (default) infers the grid from the column,
#'   as `event_units` does.
#'
#' @return A `tbl_now`.
#'
#' @section What attaching one changes:
#'
#' * **`now` moves.** A confirmation is an observation, so the as-of moment
#'   becomes the latest of the report and confirmation dates. Validation refuses
#'   an object whose `now` falls before a confirmation that has already
#'   happened.
#' * **Two columns appear.** `.confirmation_num` is the date on the same numeric
#'   anchor as `.event_num`/`.report_num`; `.confirmation_delay` is the time
#'   from report to resolution. Both are protected, like `.delay`.
#' * **Counting gains a dimension.** [to_count()] groups by the confirmation
#'   date and outcome as well, so a confirmed and a retracted case on the same
#'   `(event, report)` pair stay separate rather than being summed together.
#' * **The timeline is checked.** `event_date <= report_date <=
#'   confirmation_date`; rows that break it are warned about, not silently
#'   accepted.
#'
#' @seealso [confirmation_getters], [get_latest_confirmed()],
#'   [get_net_confirmed()], [diagnose_confirmation_delay()].
#'
#' @examples
#' data(hai_bucaramanga)
#'
#' # specimen taken -> reported -> (here) the laboratory receipt as the
#' # confirmation step.
#' hai <- hai_bucaramanga |>
#'   dplyr::filter(
#'     !is.na(specimen_date), !is.na(report_date), !is.na(received_date)
#'   ) |>
#'   tbl_now(
#'     event_date = specimen_date, report_date = report_date,
#'     data_type = "linelist", verbose = FALSE
#'   )
#'
#' hai <- suppressWarnings(add_confirmation(hai, received_date))
#' has_confirmation(hai)
#' get_confirmation_date(hai)
#'
#' hai <- remove_confirmation(hai)
#' has_confirmation(hai)
#'
#' @name confirmation_setters
NULL

#' @rdname confirmation_setters
#' @export
add_confirmation <- function(x, confirmation_date, confirmation_type = NULL,
                             confirmation_units = "auto") {
  .assert_tbl_now(x, "add_confirmation")
  if (has_confirmation(x)) {
    cli::cli_abort(c(
      "{.arg x} already has a confirmation date
       ({.val {get_confirmation_date(x)}}).",
      "i" = "Use {.fn change_confirmation} to replace it."
    ))
  }
  .set_confirmation(
    x, {{ confirmation_date }}, {{ confirmation_type }}, confirmation_units
  )
}

#' @rdname confirmation_setters
#' @export
change_confirmation <- function(x, confirmation_date, confirmation_type = NULL,
                                confirmation_units = "auto") {
  .assert_tbl_now(x, "change_confirmation")
  .set_confirmation(
    x, {{ confirmation_date }}, {{ confirmation_type }}, confirmation_units
  )
}

#' @rdname confirmation_setters
#' @export
remove_confirmation <- function(x) {
  .assert_tbl_now(x, "remove_confirmation")
  if (!has_confirmation(x)) {
    return(x)
  }

  generated <- c(
    ".event_num", ".report_num", ".delay",
    ".confirmation_num", ".confirmation_delay"
  )
  # A `.confirmation_type` we built ourselves is ours to remove; one the user
  # supplied is their column and stays.
  ours <- if (identical(get_confirmation_type(x), ".confirmation_type")) {
    ".confirmation_type"
  } else {
    character(0)
  }

  bare <- .strip_tbl_now(x)
  bare <- bare[, setdiff(colnames(bare), c(generated, ours)), drop = FALSE]

  tbl_now(
    bare,
    event_date = get_event_date(x), report_date = get_report_date(x),
    case_count = get_case_count(x), strata = get_strata(x),
    covariates = get_covariates(x), is_censored = get_is_censored(x),
    data_type = get_data_type(x),
    event_units = get_event_units(x), report_units = get_report_units(x),
    t_effects = get_temporal_effect_cols(x),
    verbose = FALSE, warn_non_uniqueness = FALSE
  )
}

#' Rebuild a `tbl_now` with a confirmation process attached
#'
#' @inheritParams confirmation_setters
#'
#' @return A `tbl_now`.
#'
#' @keywords internal
#' @noRd
.set_confirmation <- function(x, confirmation_date, confirmation_type,
                              confirmation_units) {
  # Every generated column has to go: `tbl_now()` rebuilds them and refuses to
  # write over one that is already there.
  generated <- c(
    ".event_num", ".report_num", ".delay",
    ".confirmation_num", ".confirmation_delay"
  )
  bare <- .strip_tbl_now(x)
  bare <- bare[, setdiff(colnames(bare), generated), drop = FALSE]

  tbl_now(
    bare,
    event_date = get_event_date(x), report_date = get_report_date(x),
    case_count = get_case_count(x), strata = get_strata(x),
    covariates = get_covariates(x), is_censored = get_is_censored(x),
    confirmation_date = {{ confirmation_date }},
    confirmation_type = {{ confirmation_type }},
    confirmation_units = confirmation_units,
    data_type = get_data_type(x),
    event_units = get_event_units(x), report_units = get_report_units(x),
    t_effects = get_temporal_effect_cols(x),
    verbose = FALSE, warn_non_uniqueness = FALSE
  )
}

#' Columns the confirmation process adds to a grouping
#'
#' A confirmed and a retracted case on the same `(event, report)` pair are two
#' different things, so aggregating over them would sum a case with its own
#' retraction. Grouping keeps them apart -- and keeps the confirmation DATE too,
#' because that is the third time axis the whole feature exists to carry.
#'
#' @param x A `tbl_now` object.
#'
#' @return A character vector, empty when the object carries no confirmation.
#'
#' @keywords internal
#' @noRd
.confirmation_group_cols <- function(x) {
  if (!has_confirmation(x)) {
    return(character(0))
  }
  c(get_confirmation_date(x), ".confirmation_num", get_confirmation_type(x))
}

# Counting outcomes -----

#' Confirmed, retracted and net counts per event date
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Once a `tbl_now` carries a confirmation process, "how many cases were there"
#' has three different answers, and which one you want depends on the question:
#'
#' * **`get_latest_reported_cases()`** (the existing function) counts everything
#'   that was ever *reported*, whatever the laboratory later said. It is what a
#'   nowcast of the reporting process predicts.
#' * **`get_latest_confirmed()`** counts only the cases that came back
#'   **confirmed**. Pending and retracted cases are not counted.
#' * **`get_net_confirmed()`** counts **confirmed minus retracted**: the running
#'   total as a surveillance system would publish it, which can go **down** when
#'   a case is withdrawn.
#'
#' That last one is the quantity a `count-cumulative` stream actually reports,
#' and the one \pkg{diseasenowcasting}'s signed-increment (Skellam / SkNB)
#' likelihood is built for -- see
#' [diseasenowcasting::confirmation_process()].
#'
#' @param x A `tbl_now` with a confirmation process (see [add_confirmation()]).
#'
#' @return A `tibble` with the event-date column, the strata columns and a count
#'   column named after the object's own `case_count` (or `n` for a line list).
#'
#' @section Which date the count is indexed by:
#'
#' By the **event date**, as every other `get_*_cases()` function is. A case
#' confirmed three weeks after onset still belongs to the week it began. If you
#' want counts by confirmation date instead, group on
#' `get_confirmation_date(x)` yourself -- that is a different question (how busy
#' was the laboratory) and this package does not silently answer it.
#'
#' @seealso [get_latest_reported_cases()], [add_confirmation()],
#'   [diagnose_confirmation_delay()].
#'
#' @examples
#' cases <- data.frame(
#'   onset = as.Date("2021-01-04") + c(0, 0, 1, 1, 2),
#'   visit = as.Date("2021-01-05") + c(0, 0, 1, 1, 2),
#'   result = as.Date("2021-01-06") + c(0, 0, 1, 1, 2),
#'   outcome = c("confirmed", "retracted", "confirmed", "confirmed", "retracted")
#' )
#' flu <- tbl_now(cases,
#'   event_date = onset, report_date = visit,
#'   confirmation_date = result, confirmation_type = outcome,
#'   data_type = "linelist", verbose = FALSE
#' )
#'
#' get_latest_reported_cases(flu)   # everything reported
#' get_latest_confirmed(flu)        # only the positives
#' get_net_confirmed(flu)           # positives minus withdrawals
#'
#' @name confirmation_counts
NULL

#' @rdname confirmation_counts
#' @export
get_latest_confirmed <- function(x) {
  .count_by_outcome(x, "get_latest_confirmed", net = FALSE)
}

#' @rdname confirmation_counts
#' @export
get_net_confirmed <- function(x) {
  .count_by_outcome(x, "get_net_confirmed", net = TRUE)
}

#' Count cases per event date, weighting outcomes
#'
#' @param x A `tbl_now`.
#' @param fn Calling function name, for messages.
#' @param net When `TRUE`, retracted cases count `-1`; otherwise they count `0`.
#'
#' @return A tibble.
#'
#' @keywords internal
#' @noRd
.count_by_outcome <- function(x, fn, net, within_delay = NULL) {
  .assert_tbl_now(x, fn)
  if (!has_confirmation(x)) {
    cli::cli_abort(c(
      "{.fn {fn}} needs a confirmation process, and {.arg x} has none.",
      "i" = "Attach one with {.fn add_confirmation}.",
      "i" = "For counts of everything reported, use
             {.fn get_latest_reported_cases}."
    ))
  }

  event_col <- get_event_date(x)
  strata <- get_strata(x) %||% character(0)
  type_col <- get_confirmation_type(x)
  count_in <- get_case_count(x)
  count_out <- count_in %||% "n"

  observations <- dplyr::as_tibble(.declass_tbl_now(dplyr::ungroup(x)))
  outcome <- as.character(observations[[type_col]])

  # confirmed counts +1; retracted counts -1 for a NET total and 0 otherwise;
  # pending and NA never count, because neither is a confirmed case.
  weight <- dplyr::case_when(
    outcome == "confirmed" ~ 1,
    outcome == "retracted" ~ if (isTRUE(net)) -1 else 0,
    .default = 0
  )

  # "Resolved within `within_delay`" -- anything slower has not been resolved
  # AS OF that delay, so it does not count yet.
  if (!is.null(within_delay)) {
    resolved_in_time <- is.finite(observations[[".confirmation_delay"]]) &
      observations[[".confirmation_delay"]] <= within_delay
    weight[!resolved_in_time] <- 0
  }

  size <- if (is.null(count_in)) 1 else observations[[count_in]]
  observations[[".weighted"]] <- weight * size

  out <- observations |>
    dplyr::summarise(
      !!count_out := sum(.data$.weighted, na.rm = TRUE),
      .by = dplyr::all_of(c(event_col, strata))
    ) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c(event_col, strata))))

  out
}

# Does the confirmation delay depend on the outcome? -----

#' Compare confirmation delays between confirmed and retracted cases
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A negative result often comes back faster than a positive one -- or slower,
#' if positives are prioritised. Either way the delay from report to resolution
#' is **not** the same distribution for the two outcomes, and a nowcast that
#' assumes it is will be wrong about how many pending cases are still to be
#' confirmed.
#'
#' `diagnose_confirmation_delay()` compares the two delay distributions;
#' `plot_confirmation_delay()` shows them.
#'
#' @param x A `tbl_now` with a confirmation process.
#' @param by Optional stratum column to compare within; `NULL` (default) pools.
#'
#' @return
#' `diagnose_confirmation_delay()` returns a one-row-per-comparison `tibble` with
#' `stratum`, `n_confirmed`, `n_retracted`, `median_confirmed`,
#' `median_retracted`, `difference`, `statistic` and `p.value`.
#'
#' `plot_confirmation_delay()` returns a `ggplot`.
#'
#' @section The test:
#'
#' A two-sided **Wilcoxon rank-sum** test on the confirmation delays. It is used
#' rather than a t-test because reporting delays are strongly right-skewed and
#' frequently have a point mass at zero, so a difference in means is neither
#' robust nor the quantity of interest -- what matters is whether one outcome
#' resolves systematically sooner.
#'
#' A small p-value says the two delay distributions differ. It does **not** say
#' the difference matters: with tens of thousands of records a one-hour
#' difference is significant and irrelevant, so read `difference` (the gap in
#' median days) alongside it.
#'
#' Rows with a missing or negative delay are dropped, and how many is reported
#' in the `dropped` attribute of the result. A negative confirmation delay means
#' the record is confirmed before it was reported, which the timeline forbids.
#'
#' @seealso [add_confirmation()], [diagnose_drift()] for the same question
#'   about the *reporting* delay over time.
#'
#' @examples
#' cases <- data.frame(
#'   onset = as.Date("2021-01-04") + rep(0:9, each = 4),
#'   visit = as.Date("2021-01-05") + rep(0:9, each = 4),
#'   result = as.Date("2021-01-05") + rep(0:9, each = 4) +
#'     rep(c(1, 1, 5, 6), times = 10),
#'   outcome = rep(c("confirmed", "confirmed", "retracted", "retracted"), times = 10)
#' )
#' flu <- tbl_now(cases,
#'   event_date = onset, report_date = visit,
#'   confirmation_date = result, confirmation_type = outcome,
#'   data_type = "linelist", verbose = FALSE
#' )
#'
#' diagnose_confirmation_delay(flu)
#'
#' @name confirmation_delay
NULL

#' @rdname confirmation_delay
#' @export
diagnose_confirmation_delay <- function(x, by = NULL) {
  delays <- .confirmation_delay_table(x, by, "diagnose_confirmation_delay")

  results <- lapply(split(delays, delays$stratum), function(piece) {
    confirmed <- piece$.confirmation_delay[piece$outcome == "confirmed"]
    retracted <- piece$.confirmation_delay[piece$outcome == "retracted"]

    if (length(confirmed) < 2 || length(retracted) < 2) {
      return(dplyr::tibble(
        stratum = piece$stratum[1],
        n_confirmed = length(confirmed), n_retracted = length(retracted),
        median_confirmed = stats::median(confirmed),
        median_retracted = stats::median(retracted),
        difference = NA_real_, statistic = NA_real_, p.value = NA_real_
      ))
    }

    test <- suppressWarnings(stats::wilcox.test(confirmed, retracted))
    dplyr::tibble(
      stratum = piece$stratum[1],
      n_confirmed = length(confirmed), n_retracted = length(retracted),
      median_confirmed = stats::median(confirmed),
      median_retracted = stats::median(retracted),
      difference = stats::median(confirmed) - stats::median(retracted),
      statistic = unname(test$statistic), p.value = test$p.value
    )
  })

  out <- dplyr::bind_rows(results)
  attr(out, "dropped") <- attr(delays, "dropped")
  out
}

#' @rdname confirmation_delay
#' @export
plot_confirmation_delay <- function(x, by = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg ggplot2} is required for {.fn plot_confirmation_delay}.")
  }
  delays <- .confirmation_delay_table(x, by, "plot_confirmation_delay")
  palette <- .tbl_now_palette()

  ggplot2::ggplot(
    delays,
    ggplot2::aes(
      x = .data$.confirmation_delay, y = .data$outcome, fill = .data$outcome
    )
  ) +
    ggplot2::geom_boxplot(outlier.alpha = 0.25, width = 0.6) +
    ggplot2::facet_wrap("stratum", scales = "free_y") +
    # RED: this is the reporting process -- when we found out -- not the
    # epidemic process.
    ggplot2::scale_fill_manual(
      values = c(
        confirmed = palette[["light_red"]], retracted = palette[["accent_red"]]
      ),
      guide = "none"
    ) +
    ggplot2::labs(
      x = paste0("Confirmation delay (", get_confirmation_units(x), ")"),
      y = NULL,
      title = "Time from report to resolution",
      subtitle = "Reporting delay process"
    ) +
    ggplot2::theme_minimal(base_size = 10)
}

#' The confirmation delays, tidied for comparison
#'
#' @param x A `tbl_now`.
#' @param by Optional stratum column.
#' @param fn Calling function, for messages.
#'
#' @return A tibble of `stratum`, `outcome` and `.confirmation_delay`, with a
#'   `dropped` attribute counting the unusable rows.
#'
#' @keywords internal
#' @noRd
.confirmation_delay_table <- function(x, by, fn) {
  .assert_tbl_now(x, fn)
  if (!has_confirmation(x)) {
    cli::cli_abort(c(
      "{.fn {fn}} needs a confirmation process, and {.arg x} has none.",
      "i" = "Attach one with {.fn add_confirmation}."
    ))
  }

  type_col <- get_confirmation_type(x)
  observations <- dplyr::as_tibble(.declass_tbl_now(dplyr::ungroup(x)))
  observations$outcome <- as.character(observations[[type_col]])

  if (!is.null(by)) {
    if (!by %in% colnames(observations)) {
      cli::cli_abort("Column {.val {by}} was not found in {.arg x}.")
    }
    observations$stratum <- as.character(observations[[by]])
  } else {
    observations$stratum <- "all"
  }

  before <- nrow(observations)
  usable <- observations |>
    dplyr::filter(
      .data$outcome %in% c("confirmed", "retracted"),
      !is.na(.data$.confirmation_delay),
      .data$.confirmation_delay >= 0
    ) |>
    dplyr::select("stratum", "outcome", ".confirmation_delay")

  if (nrow(usable) == 0) {
    cli::cli_abort(c(
      "No usable confirmation delays.",
      "i" = "Rows need a {.val confirmed} or {.val retracted} outcome and a
             non-negative delay."
    ))
  }

  attr(usable, "dropped") <- before - nrow(usable)
  usable
}

#' Confirmation arguments for rebuilding a `tbl_now`
#'
#' Several verbs rebuild the object by calling `tbl_now()` with an explicit list
#' of attributes (`summarise()`, `reframe()`, `update()`, ...). Every such list
#' is a place the confirmation process can be silently dropped, which is how a
#' three-date object quietly becomes a two-date one -- and the `now` moves
#' backwards with it.
#'
#' This returns the arguments to splice into that call, and returns nothing when
#' the object has no confirmation or the rebuilt data no longer carries its
#' columns.
#'
#' @param x The original `tbl_now`.
#' @param data The rebuilt data frame.
#'
#' @return A named list, possibly empty, for `do.call(tbl_now, ...)`.
#'
#' @keywords internal
#' @noRd
.confirmation_rebuild_args <- function(x, data) {
  confirmation_date <- get_confirmation_date(x)
  if (is.null(confirmation_date) || !confirmation_date %in% colnames(data)) {
    return(list())
  }
  type_col <- get_confirmation_type(x)
  list(
    confirmation_date = confirmation_date,
    confirmation_type = if (!is.null(type_col) && type_col %in% colnames(data)) {
      type_col
    } else {
      NULL
    },
    confirmation_units = get_confirmation_units(x) %||% "auto"
  )
}

#' @rdname confirmation_counts
#'
#' @param delay Longest confirmation delay to count, in the object's
#'   confirmation units. `get_nth_confirmed(x, delay = 7)` answers "how many
#'   cases per event date had been resolved within a week of being reported".
#' @export
get_nth_confirmed <- function(x, delay) {
  .assert_tbl_now(x, "get_nth_confirmed")
  if (!is.numeric(delay) || length(delay) != 1L || is.na(delay)) {
    cli::cli_abort("{.arg delay} must be a single number.")
  }
  .count_by_outcome(x, "get_nth_confirmed", net = FALSE, within_delay = delay)
}

#' @rdname confirmation_counts
#' @export
get_initial_confirmed <- function(x) {
  # Delay 0: resolved in the same period it was reported -- the rapid-test case.
  .count_by_outcome(x, "get_initial_confirmed", net = FALSE, within_delay = 0)
}

#' Mark long confirmation delays as censored
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' The confirmation counterpart of [censor_delays_above()]. A case still waiting
#' for a laboratory result long after it was reported is, in practice, never
#' going to be resolved -- and treating its delay as a real observation drags
#' the estimated delay distribution to the right. This marks those cases as
#' **pending** rather than letting an implausible delay stand.
#'
#' @param data A `tbl_now` with a confirmation process.
#' @param max_delay Longest confirmation delay to keep, in the object's
#'   confirmation units.
#' @param quiet Suppress the summary message.
#'
#' @return A `tbl_now`. Rows beyond `max_delay` have their
#'   `confirmation_type` set to `"pending"` and their confirmation date set to
#'   `NA`, because a resolution you refuse to believe is not a resolution.
#'
#' @seealso [censor_delays_above()] for the reporting delay,
#'   [diagnose_confirmation_delay()].
#'
#' @examples
#' cases <- data.frame(
#'   onset = as.Date("2021-01-04") + 0:4,
#'   visit = as.Date("2021-01-05") + 0:4,
#'   result = as.Date("2021-01-05") + 0:4 + c(1, 2, 1, 90, 2),
#'   outcome = rep("confirmed", 5)
#' )
#' flu <- tbl_now(cases,
#'   event_date = onset, report_date = visit,
#'   confirmation_date = result, confirmation_type = outcome,
#'   data_type = "linelist", verbose = FALSE
#' )
#'
#' # The 90-day resolution becomes "pending" again.
#' table(censor_confirmation_delays_above(flu, 30)[["outcome"]])
#'
#' @export
censor_confirmation_delays_above <- function(data, max_delay, quiet = FALSE) {
  .assert_tbl_now(data, "censor_confirmation_delays_above")
  if (!has_confirmation(data)) {
    cli::cli_abort(c(
      "{.fn censor_confirmation_delays_above} needs a confirmation process.",
      "i" = "Attach one with {.fn add_confirmation}."
    ))
  }
  if (!is.numeric(max_delay) || length(max_delay) != 1L || max_delay < 0) {
    cli::cli_abort("{.arg max_delay} must be a single non-negative number.")
  }

  delays <- data[[".confirmation_delay"]]
  too_long <- is.finite(delays) & delays > max_delay

  if (any(too_long)) {
    confirmation_col <- get_confirmation_date(data)
    type_col <- get_confirmation_type(data)
    # Both together: a `confirmation_type` of "confirmed" with no date is the
    # contradiction `tbl_now()` warns about, so the outcome goes back to
    # "pending" at the same time as the date is removed.
    data[[confirmation_col]][too_long] <- NA
    data[[type_col]][too_long] <- "pending"
    data[[".confirmation_num"]][too_long] <- NA_real_
    data[[".confirmation_delay"]][too_long] <- NA_real_
  }

  if (!isTRUE(quiet)) {
    cli::cli_inform(c(
      "i" = paste0(
        "Returned {sum(too_long)} case{?s} with a confirmation delay > ",
        "{max_delay} {get_confirmation_units(data)} to {.val pending}."
      )
    ))
  }
  data
}

#' How much of each day has been resolved
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' The share of each event date's cases that are **confirmed**, **retracted** or
#' still **pending**, as of the object's `now`.
#'
#' This is the picture of the *resolution front*. The oldest dates are almost
#' entirely resolved; the most recent ones are mostly pending, because the
#' laboratory has not caught up yet. Where that front sits tells you how far
#' back the confirmed counts can be trusted -- and a day that is 80% pending is
#' a day whose confirmed count means very little.
#'
#' @param x A `tbl_now` with a confirmation process.
#' @param by Optional stratum column to facet by.
#' @param proportion When `TRUE` (default) the bands are shares summing to 1;
#'   `FALSE` shows the counts instead, which keeps the epidemic curve visible.
#'
#' @return A `ggplot`.
#'
#' @section Reading it:
#'
#' The `pending` band widening towards the right is normal and expected -- it is
#' the same right-truncation a nowcast exists to correct, one axis over. What is
#' *not* normal is a pending band that stays wide far from the `now`: those cases
#' were reported and then never resolved, and they will never be. Consider
#' [censor_confirmation_delays_above()].
#'
#' A `retracted` share that changes over time is worth investigating: it usually
#' means the testing criteria or the case definition changed, not that the
#' disease did.
#'
#' @section Colours:
#'
#' `confirmed` is drawn in the palette's green (it is a real case -- the
#' epidemic process), `retracted` in the accent red (it was removed by the
#' reporting process), and `pending` in grey (not yet known either way).
#'
#' @seealso [diagnose_confirmation_delay()], [get_latest_confirmed()].
#'
#' @examples
#' cases <- data.frame(
#'   onset = as.Date("2021-01-04") + rep(0:9, each = 4),
#'   visit = as.Date("2021-01-05") + rep(0:9, each = 4),
#'   result = as.Date("2021-01-07") + rep(0:9, each = 4),
#'   outcome = rep(c("confirmed", "confirmed", "retracted", "pending"), times = 10)
#' )
#' cases$result[cases$outcome == "pending"] <- as.Date(NA)
#' flu <- tbl_now(cases,
#'   event_date = onset, report_date = visit,
#'   confirmation_date = result, confirmation_type = outcome,
#'   data_type = "linelist", verbose = FALSE
#' )
#'
#' plot_confirmation_status(flu)
#'
#' @export
plot_confirmation_status <- function(x, by = NULL, proportion = TRUE) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg ggplot2} is required for {.fn plot_confirmation_status}.")
  }
  .assert_tbl_now(x, "plot_confirmation_status")
  if (!has_confirmation(x)) {
    cli::cli_abort(c(
      "{.fn plot_confirmation_status} needs a confirmation process.",
      "i" = "Attach one with {.fn add_confirmation}."
    ))
  }

  event_col <- get_event_date(x)
  type_col <- get_confirmation_type(x)
  count_col <- get_case_count(x)
  palette <- .tbl_now_palette()

  observations <- dplyr::as_tibble(.declass_tbl_now(dplyr::ungroup(x)))
  observations$.outcome <- factor(
    dplyr::coalesce(as.character(observations[[type_col]]), "pending"),
    levels = c("confirmed", "retracted", "pending")
  )
  observations$.size <- if (is.null(count_col)) 1 else observations[[count_col]]
  observations$.stratum <- if (is.null(by)) {
    "all"
  } else {
    if (!by %in% colnames(observations)) {
      cli::cli_abort("Column {.val {by}} was not found in {.arg x}.")
    }
    as.character(observations[[by]])
  }

  shares <- observations |>
    dplyr::summarise(
      .cases = sum(.data$.size, na.rm = TRUE),
      .by = dplyr::all_of(c(event_col, ".stratum", ".outcome"))
    ) |>
    tidyr::complete(
      !!rlang::sym(event_col), .data$.stratum, .data$.outcome,
      fill = list(.cases = 0)
    )

  plot <- ggplot2::ggplot(
    shares,
    ggplot2::aes(
      x = .data[[event_col]], y = .data$.cases, fill = .data$.outcome
    )
  ) +
    ggplot2::geom_area(position = if (isTRUE(proportion)) "fill" else "stack") +
    ggplot2::scale_fill_manual(
      name = NULL,
      values = c(
        confirmed = palette[["primary_green"]],
        retracted = palette[["accent_red"]],
        pending = "#c9cec9"
      ),
      drop = FALSE
    ) +
    ggplot2::labs(
      x = event_col,
      y = if (isTRUE(proportion)) "Share of cases" else "Cases",
      title = "How much of each day has been resolved",
      subtitle = paste0("As of ", format(get_now(x)))
    ) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(legend.position = "top")

  if (isTRUE(proportion)) {
    plot <- plot + ggplot2::scale_y_continuous(labels = scales::percent)
  }
  if (!is.null(by)) {
    plot <- plot + ggplot2::facet_wrap(".stratum")
  }
  plot
}
