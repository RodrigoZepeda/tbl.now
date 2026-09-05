# The validation process: a THIRD date, after the event and the report.
#
# Influenza is the motivating case. A case has
#
#   event_date         when the person fell ill (symptom onset)
#   report_date        when the system heard about it (the medical visit)
#   validation_date  when the laboratory settled it (the test result)
#
# and the test can come back either way, so a report is not the end of the
# story: it can be CONFIRMED (positive) or RETRACTED (negative, the case never
# was one). Until the result arrives the case is PENDING.
#
# The honest timeline is `event_date <= report_date <= validation_date`, and
# the package checks it rather than assuming it.
#
# Everything here is optional. An object with no `validation_date` behaves
# exactly as it did before this file existed.

#' The values `validation_type` may take
#'
#' `"pending"` is the default state of every case: reported, not yet resolved.
#' `"confirmed"` and `"retracted"` are the two ways a case leaves that state.
#' `NA` means the outcome is genuinely unknown -- which is what a
#' `validation_date` with no `validation_type` gives you, because a date
#' alone cannot say whether the test was positive or negative.
#'
#' @return A character vector of the allowed values.
#'
#' @keywords internal
#' @noRd
.validation_levels <- function() {
  c("confirmed", "retracted", "pending")
}

#' Check a user dictionary of `validation_type` labels
#'
#' Surveillance data is not always recorded in English, and recoding it by hand
#' before every call is exactly the kind of step that gets forgotten. A
#' dictionary lets the object do it once: the names are the labels as they
#' appear in the data, the values are the canonical outcomes.
#'
#' @param validation_levels A named character vector, or `NULL`.
#'
#' @return `validation_levels`, unchanged, or `NULL`.
#'
#' @keywords internal
#' @noRd
.check_validation_levels <- function(validation_levels) {
  if (is.null(validation_levels)) {
    return(NULL)
  }

  allowed <- .validation_levels()
  labels <- names(validation_levels)

  if (!is.character(validation_levels) || is.null(labels) ||
    anyNA(labels) || !all(nzchar(labels))) {
    cli::cli_abort(c(
      "{.arg validation_levels} must be a NAMED character vector.",
      "i" = "The names are the labels in your data, the values are the
             canonical outcomes.",
      "*" = "For example
             {.code c(confirmado = \"confirmed\", pendiente = \"pending\")}."
    ))
  }

  if (anyDuplicated(labels) > 0) {
    repeated <- unique(labels[duplicated(labels)])
    cli::cli_abort(
      "{.arg validation_levels} maps {.val {repeated}} more than once."
    )
  }

  unknown <- setdiff(unique(unname(validation_levels)), allowed)
  if (length(unknown) > 0) {
    cli::cli_abort(c(
      "{.arg validation_levels} maps to {length(unknown)} value{?s} that
       {?is/are} not an outcome: {.val {unknown}}.",
      "i" = "The right-hand side must be one of {.val {allowed}}."
    ))
  }

  # Recoding runs again on every rebuild (`summarise()`, `update()`, ...), so a
  # dictionary that renames a canonical value into a DIFFERENT one would flip
  # the column back and forth. Mapping a canonical value to itself is fine.
  canonical <- intersect(labels, allowed)
  flipped <- canonical[unname(validation_levels[canonical]) != canonical]
  if (length(flipped) > 0) {
    cli::cli_abort(c(
      "{.arg validation_levels} remaps the canonical value{?s}
       {.val {flipped}} to something else.",
      "i" = "That recoding is not repeatable: every rebuild of the object
             would apply it again."
    ))
  }

  validation_levels
}

#' Translate `validation_type` values through the dictionary
#'
#' @param values A character vector of outcomes as recorded.
#' @param validation_levels The dictionary, or `NULL`.
#'
#' @return A character vector of canonical outcomes.
#'
#' @keywords internal
#' @noRd
.recode_validation_type <- function(values, validation_levels) {
  if (is.null(validation_levels) || length(values) == 0) {
    return(values)
  }
  hit <- match(values, names(validation_levels))
  ifelse(is.na(hit), values, unname(validation_levels)[hit])
}

#' Build (or validate) the `validation_type` column
#'
#' @param data A data frame.
#' @param validation_date Name of the validation-date column, or `NULL`.
#' @param validation_type Name of the type column, or `NULL`.
#' @param validation_levels A named character dictionary of non-canonical
#'   labels, or `NULL`.
#' @param verbose Logical.
#'
#' @return The data frame, with a `validation_type` column when one is needed.
#'
#' @keywords internal
#' @noRd
.resolve_validation_type <- function(data, validation_date, validation_type,
                                       validation_levels = NULL,
                                       verbose = TRUE) {
  if (is.null(validation_date)) {
    return(list(data = data, validation_type = validation_type))
  }

  dates <- data[[validation_date]]
  has_date <- !is.na(dates)

  if (is.null(validation_type)) {
    # Every case starts pending; a date resolves it -- but a date alone cannot
    # say WHICH way. A negative test has a date too, so calling it "confirmed"
    # would invert the meaning of the data. `NA` and a warning is the honest
    # answer: the outcome is recorded as unknown until the caller says.
    validation_type <- ".validation_type"
    data[[validation_type]] <- ifelse(has_date, NA_character_, "pending")

    if (any(has_date)) {
      allowed <- .validation_levels()
      cli::cli_warn(c(
        paste0(
          "{sum(has_date)} row{?s} have a {.arg validation_date} but no ",
          "{.arg validation_type}, so their outcome is {.val NA}."
        ),
        "i" = paste0(
          "A date alone cannot say whether the case was confirmed or ",
          "retracted -- a negative test has a date too."
        ),
        "i" = paste0(
          "Pass {.arg validation_type} (a column of {.val {allowed}}) ",
          "to say which."
        )
      ))
    }
    return(list(data = data, validation_type = validation_type))
  }

  # The dictionary runs FIRST, so everything downstream -- the check below, the
  # counts, the plots -- only ever sees the canonical four.
  values <- .recode_validation_type(
    as.character(data[[validation_type]]), validation_levels
  )
  # A row with no date has not been resolved, whatever the column says.
  values[!has_date & is.na(values)] <- "pending"

  allowed <- .validation_levels()
  unknown <- setdiff(stats::na.omit(unique(values)), allowed)
  if (length(unknown) > 0) {
    hint <- if (is.null(validation_levels)) {
      "Map them with {.arg validation_levels}, e.g.
       {.code c(confirmado = \"confirmed\")}."
    } else {
      "{.arg validation_levels} maps none of them."
    }
    cli::cli_abort(c(
      paste0(
        "{.arg validation_type} contains {length(unknown)} unrecognised ",
        "value{?s}: {.val {unknown}}."
      ),
      "i" = "Allowed values are {.val {allowed}}, or {.val NA}.",
      "i" = hint
    ))
  }

  # A resolved outcome with no date is a contradiction worth naming.
  resolved_without_date <- !has_date & values %in% c("confirmed", "retracted")
  if (any(resolved_without_date) && isTRUE(verbose)) {
    cli::cli_warn(
      "{sum(resolved_without_date)} row{?s} are {.val confirmed}/{.val retracted}
       but carry no {.arg validation_date}."
    )
  }

  data[[validation_type]] <- values
  list(data = data, validation_type = validation_type)
}

# Getters -----

# The four getters are documented on `nowcast_data_getters` in R/getters.R,
# alongside `get_event_date()` and `get_report_date()`: the third date is one
# more thing the object was told about itself, not a separate idea, and a reader
# holding a `tbl_now` should find all of its attributes on one page.

#' @rdname nowcast_data_getters
#' @export
get_validation_date <- function(x) {
  attr(x, "validation_date", exact = TRUE)
}

#' @rdname nowcast_data_getters
#' @export
get_validation_type <- function(x) {
  attr(x, "validation_type", exact = TRUE)
}

#' @rdname nowcast_data_getters
#' @export
get_validation_units <- function(x) {
  attr(x, "validation_units", exact = TRUE)
}

#' @rdname nowcast_data_getters
#' @export
get_is_censored_validation <- function(x) {
  attr(x, "is_censored_validation", exact = TRUE)
}

#' @rdname nowcast_data_getters
#' @export
get_validation_levels <- function(x) {
  attr(x, "validation_levels", exact = TRUE)
}

#' @rdname nowcast_data_getters
#' @export
has_validation <- function(x) {
  !is.null(get_validation_date(x))
}

#' The generated validation columns, when the object has any
#'
#' `.validation_num` is the validation date on the same numeric anchor as
#' `.event_num` and `.report_num`; `.validation_delay` is
#' `.validation_num - .report_num`, the time from report to resolution. That
#' second one is the quantity [diagnose_validation_delay()] compares between
#' confirmed and retracted cases.
#'
#' @param x A `tbl_now` object.
#'
#' @return A character vector, empty when the object carries no validation.
#'
#' @keywords internal
#' @noRd
.validation_generated_cols <- function(x) {
  if (!has_validation(x)) {
    return(character(0))
  }
  c(".validation_num", ".validation_delay")
}

#' Add `.validation_num` and `.validation_delay`
#'
#' Anchored on the same earliest event date `time_cols_to_numeric()` uses, so
#' `.event_num`, `.report_num` and `.validation_num` are on one scale and
#' differences between them mean what they look like.
#'
#' @param data A data frame that already has `.report_num`.
#' @param event_date,validation_date Column names.
#' @param validation_units The validation date's units.
#' @param force Overwrite reserved columns rather than aborting.
#'
#' @return The data frame with the two columns added.
#'
#' @keywords internal
#' @noRd
.add_validation_num <- function(data, event_date, validation_date,
                                  validation_units, force = FALSE) {
  for (reserved in c(".validation_num", ".validation_delay")) {
    if (reserved %in% colnames(data) && !force) {
      cli::cli_abort(
        "Data already has a column named {.val {reserved}}, which this class
         uses. Rename it, or pass {.code force = TRUE}."
      )
    }
  }

  anchor <- suppressWarnings(min(data[[event_date]], na.rm = TRUE))
  validation <- data[[validation_date]]

  data[[".validation_num"]] <- if (identical(validation_units, "numeric")) {
    as.numeric(validation) - as.numeric(anchor)
  } else {
    .date_difference_in_units(validation, anchor, validation_units)
  }
  data[[".validation_delay"]] <- data[[".validation_num"]] - data[[".report_num"]]
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

# The three verbs are documented on `add` in R/adders_changers_and_removers.R,
# alongside `change_event_date()`: attaching a third date is one more attribute
# edit, and a reader looking for "how do I tell the object about this column"
# should find every answer on one page.

#' @rdname add
#' @export
change_is_censored_validation <- function(x, is_censored_validation) {
  .assert_tbl_now(x, "change_is_censored_validation")

  value_pos <- tidyselect::eval_select(
    rlang::expr({{ is_censored_validation }}), x
  )
  value <- if (length(value_pos) == 0) NULL else colnames(x)[value_pos]

  if (length(value) > 1) {
    cli::cli_abort(
      "{.arg is_censored_validation} must be the name of one column (length 1)"
    )
  }

  if (!is.null(value) && !has_validation(x)) {
    cli::cli_abort(c(
      "{.arg x} has no validation process, so a validation delay cannot be
       censored.",
      "i" = "Attach one with {.fn add_validation_date} first."
    ))
  }

  if (!is.null(value) && !rlang::is_logical(x[[value]])) {
    cli::cli_abort("Column {.val {value}} must be logical")
  }

  attr(x, "is_censored_validation") <- value

  validate_tbl_now(x)

  return(x)
}

#' @rdname add
#' @export
add_is_censored_validation <- function(x, is_censored_validation) {
  if (length(get_is_censored_validation(x)) > 0) {
    cli::cli_abort(
      paste0(
        "Already has value {.val {get_is_censored_validation(x)}} as the ",
        "validation censoring indicator.",
        " Use {.help remove_is_censored_validation} to remove it before adding",
        " or {.help change_is_censored_validation} to change it."
      )
    )
  }

  change_is_censored_validation(x, {{ is_censored_validation }})
}

#' @rdname add
#' @export
remove_is_censored_validation <- function(x) {
  .assert_tbl_now(x, "remove_is_censored_validation")
  change_is_censored_validation(x, NULL)
}

#' @rdname add
#' @export
add_validation_date <- function(x, validation_date, validation_type = NULL,
                             validation_units = "auto",
                             validation_levels = NULL) {
  .assert_tbl_now(x, "add_validation_date")
  if (has_validation(x)) {
    cli::cli_abort(c(
      "{.arg x} already has a validation date
       ({.val {get_validation_date(x)}}).",
      "i" = "Use {.fn change_validation_date} to replace it."
    ))
  }
  .set_validation(
    x, {{ validation_date }}, {{ validation_type }}, validation_units,
    validation_levels
  )
}

#' @rdname add
#' @export
change_validation_date <- function(x, validation_date, validation_type = NULL,
                                validation_units = "auto",
                                validation_levels = NULL) {
  .assert_tbl_now(x, "change_validation_date")
  .set_validation(
    x, {{ validation_date }}, {{ validation_type }}, validation_units,
    validation_levels
  )
}

#' @rdname add
#' @export
remove_validation_date <- function(x) {
  .assert_tbl_now(x, "remove_validation_date")
  if (!has_validation(x)) {
    return(x)
  }

  generated <- c(
    ".event_num", ".report_num", ".delay",
    ".validation_num", ".validation_delay"
  )
  # A `.validation_type` we built ourselves is ours to remove; one the user
  # supplied is their column and stays.
  ours <- if (identical(get_validation_type(x), ".validation_type")) {
    ".validation_type"
  } else {
    character(0)
  }

  # A censoring flag we built ourselves goes with the process it belonged to;
  # one the user supplied is their column and stays as data, unreferenced.
  if (identical(get_is_censored_validation(x), ".is_censored_validation")) {
    ours <- c(ours, ".is_censored_validation")
  }

  bare <- .strip_tbl_now(x)
  bare <- bare[, setdiff(colnames(bare), c(generated, ours)), drop = FALSE]

  tbl_now(
    bare,
    event_date = get_event_date(x), report_date = get_report_date(x),
    case_count = get_case_count(x), strata = get_strata(x),
    covariates = get_covariates(x), is_censored_report = get_is_censored_report(x),
    data_type = get_data_type(x),
    event_units = get_event_units(x), report_units = get_report_units(x),
    t_effects = get_temporal_effect_cols(x),
    verbose = FALSE, warn_non_uniqueness = FALSE
  )
}

#' Rebuild a `tbl_now` with a validation process attached
#'
#' @inheritParams tbl_now
#'
#' @return A `tbl_now`.
#'
#' @keywords internal
#' @noRd
.set_validation <- function(x, validation_date, validation_type,
                              validation_units, validation_levels = NULL) {
  # Every generated column has to go: `tbl_now()` rebuilds them and refuses to
  # write over one that is already there.
  generated <- c(
    ".event_num", ".report_num", ".delay",
    ".validation_num", ".validation_delay"
  )
  bare <- .strip_tbl_now(x)
  bare <- bare[, setdiff(colnames(bare), generated), drop = FALSE]

  tbl_now(
    bare,
    event_date = get_event_date(x), report_date = get_report_date(x),
    case_count = get_case_count(x), strata = get_strata(x),
    covariates = get_covariates(x), is_censored_report = get_is_censored_report(x),
    validation_date = {{ validation_date }},
    validation_type = {{ validation_type }},
    validation_units = validation_units,
    validation_levels = validation_levels %||% get_validation_levels(x),
    is_censored_validation = get_is_censored_validation(x),
    data_type = get_data_type(x),
    event_units = get_event_units(x), report_units = get_report_units(x),
    t_effects = get_temporal_effect_cols(x),
    verbose = FALSE, warn_non_uniqueness = FALSE
  )
}

#' Columns the validation process adds to a grouping
#'
#' A confirmed and a retracted case on the same `(event, report)` pair are two
#' different things, so aggregating over them would sum a case with its own
#' retraction. Grouping keeps them apart -- and keeps the validation DATE too,
#' because that is the third time axis the whole feature exists to carry.
#'
#' @param x A `tbl_now` object.
#'
#' @return A character vector, empty when the object carries no validation.
#'
#' @keywords internal
#' @noRd
.validation_group_cols <- function(x) {
  if (!has_validation(x)) {
    return(character(0))
  }
  # The censoring flag joins them for the same reason: a resolution whose delay
  # is only a bound is not the same observation as one measured exactly, so
  # summing the two together would report a bound as a fact.
  c(
    get_validation_date(x), ".validation_num", get_validation_type(x),
    get_is_censored_validation(x)
  )
}

# Does the validation delay depend on the outcome? -----

#' Compare validation delays between confirmed and retracted cases
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A negative result often comes back faster than a positive one -- or slower,
#' if positives are prioritised. Either way the delay from report to resolution
#' is **not** the same distribution for the two outcomes, and a nowcast that
#' assumes it is will be wrong about how many pending cases are still to be
#' confirmed.
#'
#' `diagnose_validation_delay()` compares the two delay distributions;
#' `plot_validation_delay()` shows them.
#'
#' @param x A `tbl_now` with a validation process.
#' @param by Optional stratum column to compare within; `NULL` (default) pools.
#'
#' @return
#' `diagnose_validation_delay()` returns a one-row-per-comparison `tibble` with
#' `stratum`, `n_confirmed`, `n_retracted`, `median_confirmed`,
#' `median_retracted`, `difference`, `statistic` and `p.value`.
#'
#' `plot_validation_delay()` returns a `ggplot`.
#'
#' @section The test:
#'
#' A two-sided **Wilcoxon rank-sum** test on the validation delays. It is used
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
#' in the `dropped` attribute of the result. A negative validation delay means
#' the record is validated before it was reported, which the timeline forbids.
#'
#' @seealso
#' [add_validation_date()][add] to attach a validation process;
#' [censor_validation_delays_above()][censoring] for resolutions that
#' never arrive; [validated_cases] for counting the outcomes;
#' [diagnose_drift()] for the same question about the *reporting* delay over time.
#' The [*Diagnosing a tbl_now* article](https://rodrigozepeda.github.io/tbl.now/articles/diagnosing-a-tbl-now.html)
#' puts this alongside the other checks.
#'
#' @param linewidth Multiplier on the box outlines of
#'   `plot_validation_delay()`. Default `1` (drawn at `0.5`).
#' @param palette A named colour palette (see [tbl_now_palette()]).
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
#'   validation_date = result, validation_type = outcome,
#'   data_type = "linelist", verbose = FALSE
#' )
#'
#' # Retractions here come back about four days later than validations, and
#' # the test says so.
#' diagnose_validation_delay(flu)
#'
#' # The same comparison as a picture.
#' plot_validation_delay(flu)
#'
#' @name validation_delay
NULL

#' @rdname validation_delay
#' @export
diagnose_validation_delay <- function(x, by = NULL) {
  delays <- .validation_delay_table(x, by, "diagnose_validation_delay")

  results <- lapply(split(delays, delays$stratum), function(piece) {
    confirmed <- piece$.validation_delay[piece$outcome == "confirmed"]
    retracted <- piece$.validation_delay[piece$outcome == "retracted"]

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

#' @rdname validation_delay
#' @export
plot_validation_delay <- function(x, by = NULL, linewidth = 1,
                                  palette = .tbl_now_palette()) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg ggplot2} is required for {.fn plot_validation_delay}.")
  }
  .tbl_now_check_palette(palette, "plot_validation_delay")
  .tbl_now_check_size(linewidth, "linewidth")
  delays <- .validation_delay_table(x, by, "plot_validation_delay")

  ggplot2::ggplot(
    delays,
    ggplot2::aes(
      x = .data$.validation_delay, y = .data$outcome, fill = .data$outcome
    )
  ) +
    ggplot2::geom_boxplot(outlier.alpha = 0.25, width = 0.6,
                          linewidth = 0.5 * linewidth) +
    ggplot2::facet_wrap("stratum", scales = "free_y") +
    # RED: this is the reporting process -- when we found out -- not the
    # epidemic process.
    ggplot2::scale_fill_manual(
      values = c(
        confirmed = palette[["reporting_light"]], retracted = palette[["reporting"]]
      ),
      guide = "none"
    ) +
    ggplot2::labs(
      x = paste0("Validation delay (", get_validation_units(x), ")"),
      y = NULL,
      title = "Time from report to resolution",
      subtitle = "Reporting delay process"
    ) +
    .tbl_now_theme(palette)
}

#' The validation delays, tidied for comparison
#'
#' @param x A `tbl_now`.
#' @param by Optional stratum column.
#' @param fn Calling function, for messages.
#'
#' @return A tibble of `stratum`, `outcome` and `.validation_delay`, with a
#'   `dropped` attribute counting the unusable rows.
#'
#' @keywords internal
#' @noRd
.validation_delay_table <- function(x, by, fn) {
  .assert_tbl_now(x, fn)
  if (!has_validation(x)) {
    cli::cli_abort(c(
      "{.fn {fn}} needs a validation process, and {.arg x} has none.",
      "i" = "Attach one with {.fn add_validation_date}."
    ))
  }

  type_col <- get_validation_type(x)
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
      !is.na(.data$.validation_delay),
      .data$.validation_delay >= 0
    ) |>
    dplyr::select("stratum", "outcome", ".validation_delay")

  if (nrow(usable) == 0) {
    cli::cli_abort(c(
      "No usable validation delays.",
      "i" = "Rows need a {.val confirmed} or {.val retracted} outcome and a
             non-negative delay."
    ))
  }

  attr(usable, "dropped") <- before - nrow(usable)
  usable
}

#' Validation arguments for rebuilding a `tbl_now`
#'
#' Several verbs rebuild the object by calling `tbl_now()` with an explicit list
#' of attributes (`summarise()`, `reframe()`, `update()`, ...). Every such list
#' is a place the validation process can be silently dropped, which is how a
#' three-date object quietly becomes a two-date one -- and the `now` moves
#' backwards with it.
#'
#' This returns the arguments to splice into that call, and returns nothing when
#' the object has no validation or the rebuilt data no longer carries its
#' columns.
#'
#' @param x The original `tbl_now`.
#' @param data The rebuilt data frame.
#'
#' @return A named list, possibly empty, for `do.call(tbl_now, ...)`.
#'
#' @keywords internal
#' @noRd
.validation_rebuild_args <- function(x, data) {
  validation_date <- get_validation_date(x)
  if (is.null(validation_date) || !validation_date %in% colnames(data)) {
    return(list())
  }
  type_col <- get_validation_type(x)
  censored_col <- get_is_censored_validation(x)
  list(
    validation_date = validation_date,
    validation_type = if (!is.null(type_col) && type_col %in% colnames(data)) {
      type_col
    } else {
      NULL
    },
    validation_units = get_validation_units(x) %||% "auto",
    validation_levels = get_validation_levels(x),
    is_censored_validation = if (!is.null(censored_col) &&
      censored_col %in% colnames(data)) {
      censored_col
    } else {
      NULL
    }
  )
}

#' @rdname censoring
#' @export
censor_validation_delays_above <- function(x, max_delay, verbose = TRUE) {
  .assert_tbl_now(x, "censor_validation_delays_above")
  if (!has_validation(x)) {
    cli::cli_abort(c(
      "{.fn censor_validation_delays_above} needs a validation process.",
      "i" = "Attach one with {.fn add_validation_date}."
    ))
  }
  if (!is.numeric(max_delay) || length(max_delay) != 1L || max_delay < 0) {
    cli::cli_abort("{.arg max_delay} must be a single non-negative number.")
  }

  delays <- x[[".validation_delay"]]
  too_long <- is.finite(delays) & delays > max_delay

  # `add_is_censored_validation()` refuses a `grouped_tbl_now`, so the grouping
  # comes off for the write and goes back on afterwards. `.censor_mark()` does
  # the merge itself -- a delay you have already decided not to take at face
  # value does not become exact because a later, looser threshold was applied.
  group_columns <- dplyr::group_vars(x)
  x <- .tbl_now_regroup(
    .censor_mark(ungroup(x), too_long, axis = "validation"),
    group_columns
  )

  if (isTRUE(verbose)) {
    cli::cli_inform(c(
      "i" = paste0(
        "Marked {sum(too_long)} case{?s} with a validation delay > ",
        "{max_delay} {get_validation_units(x)} as censored."
      ),
      "*" = "That delay is now a lower bound (is_censored_validation)."
    ))
  }
  x
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
#' @param x A `tbl_now` with a validation process.
#' @param by Optional stratum column to facet by.
#' @param proportion When `TRUE` (default) the bands are shares summing to 1;
#'   `FALSE` shows the counts instead, which keeps the epidemic curve visible.
#' @param palette A named colour palette (see [tbl_now_palette()]). The plot is
#'   drawn entirely with stacked areas, so it takes no `size` or `linewidth`.
#'
#' @return A `ggplot`.
#'
#' @section Reading it:
#'
#' The `pending` band widening towards the right is normal and expected -- it is
#' the same right-truncation a nowcast exists to correct, one axis over. What is
#' *not* normal is a pending band that stays wide far from the `now`: those cases
#' were reported and then never resolved, and they will never be. Consider
#' [censor_validation_delays_above()].
#'
#' A `retracted` share that changes over time is worth investigating: it usually
#' means the testing criteria or the case definition changed, not that the
#' disease did.
#'
#' @section Colours:
#'
#' `confirmed` is drawn with the palette's `epidemic` role (it is a real case --
#' the epidemic process), `retracted` with `reporting` (it was removed by the
#' reporting process), and `pending` with the neutral `pending` role (not yet
#' known either way). Override any of them through `palette`.
#'
#' @seealso [diagnose_validation_delay()], [validated_cases].
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
#'   validation_date = result, validation_type = outcome,
#'   data_type = "linelist", verbose = FALSE
#' )
#'
#' plot_validation_status(flu)
#'
#' @export
plot_validation_status <- function(x, by = NULL, proportion = TRUE,
                                  palette = .tbl_now_palette()) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg ggplot2} is required for {.fn plot_validation_status}.")
  }
  .assert_tbl_now(x, "plot_validation_status")
  .tbl_now_check_palette(palette, "plot_validation_status")
  if (!has_validation(x)) {
    cli::cli_abort(c(
      "{.fn plot_validation_status} needs a validation process.",
      "i" = "Attach one with {.fn add_validation_date}."
    ))
  }

  event_col <- get_event_date(x)
  type_col <- get_validation_type(x)
  count_col <- get_case_count(x)

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
        confirmed = palette[["epidemic"]],
        retracted = palette[["reporting"]],
        pending = palette[["pending"]]
      ),
      drop = FALSE
    ) +
    ggplot2::labs(
      x = event_col,
      y = if (isTRUE(proportion)) "Share of cases" else "Cases",
      title = "How much of each day has been resolved",
      subtitle = paste0("As of ", format(get_now(x)))
    ) +
    .tbl_now_theme(palette) +
    ggplot2::theme(legend.position = "top")

  if (isTRUE(proportion)) {
    plot <- plot + ggplot2::scale_y_continuous(labels = scales::percent)
  }
  if (!is.null(by)) {
    plot <- plot + ggplot2::facet_wrap(".stratum")
  }
  plot
}
