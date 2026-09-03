#' Function to get an attribute from an object or default value
#'
#' Checks if attribute exists in object and returns `default` if not
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' @param x An object with attribute `name`
#' @param name The name of the attribute in `x`
#' @param default (optional) The default value of attribute `name` in `x`
#'
#' @return The value of attribute `name` in `x`, or `default` if the attribute
#' is not present.
#'
#' @keywords internal
#' @noRd
attr_default <- function(x, name, default = NULL) {
  val <- attr(x, name, exact = TRUE)
  if (is.null(val)) default else val
}

#' Is a date a weekday or a weekend?
#'
#' @description `r lifecycle::badge('stable')`
#'
#' Reporting almost always slows down at the weekend, which is one of the
#' strongest and most predictable patterns in surveillance data. This tells you
#' which days are which, and lets you say what "weekend" means -- it is Friday
#' and Saturday in much of the Middle East, and Sunday alone in some countries.
#'
#' @param date A Date (or POSIXt) object. May be a vector.
#' @param weekend_days A character or numeric vector defining which days count as
#'   the weekend. Defaults to Saturday and Sunday.
#'
#'   * Character: day names or abbreviations, case-insensitive --
#'     `c("Mon", "Tuesday", "wed", ...)`.
#'
#'   * Numeric: integers 1-7 in [lubridate::wday()] numbering with
#'     `week_start = 1`, so **1 = Monday** and 7 = Sunday.
#'
#' @return A logical vector, `TRUE` where the date is a weekday and `FALSE` where
#' it falls on the weekend.
#'
#' @seealso
#' [temporal_effects()] and [add_temporal_effects()], which use this to build the
#' day-of-week and weekend terms a model can fit;
#' [plot_day_of_week_effects()][calendar_effect_plots] to see the effect in the
#' data; [align_weeks()], whose `align_on_day` uses this same ISO numbering.
#'
#' @examples
#' is_weekday(as.Date("2020-04-22")) # TRUE (Wed)
#' is_weekday(as.Date("2020-04-19")) # FALSE (Sun)
#'
#' ## Middle East weekend (Fri - Sat)
#' is_weekday(as.Date("2020-04-17"), weekend_days = c("Fri", "Sat"))
#'
#' # Weekend only on Friday
#' is_weekday(as.Date("2020-04-17"), weekend_days = "Friday")
#' is_weekday(as.Date("2020-04-18"), weekend_days = "Friday")
#'
#' ## Weekend on Sun - Mon (numeric: 7 = Sun, 1 = Mon)
#' is_weekday(as.Date("2020-04-20"), weekend_days = c(7, 1))
#'
#' @export
#' @md
is_weekday <- function(date, weekend_days = c("Sat", "Sun")) {
  # Convert weekend_days to numeric wday indices
  weekend_idx <- if (is.numeric(weekend_days)) {
    as.integer(weekend_days)
  } else {
    all_days <- lubridate::wday(1:7, label = TRUE, abbr = TRUE, week_start = 1) |> sort()
    weekend_days_clean <- tolower(substr(weekend_days, 1, 3))
    day_lookup <- tolower(substr(as.character(all_days), 1, 3))
    match(weekend_days_clean, day_lookup)
  }

  # Invalid weekend specification
  if (any(is.na(weekend_idx)) || any(weekend_idx < 1 | weekend_idx > 7)) {
    cli::cli_abort("Invalid `weekend_days` provided. Must be integer (1 to 7) or day names: {lubridate::wday(1:7, label = TRUE, abbr = TRUE)}")
  }

  # Compute wday with given start-of-week
  d <- lubridate::wday(date, week_start = 1)

  # TRUE if weekday (not part of weekend)
  !d %in% weekend_idx
}

#' List what a `tbl_now` was told about itself
#'
#' @description `r lifecycle::badge('stable')`
#'
#' A [tbl_now()] remembers which of its columns is the event date, which is the
#' report date, which are strata, and so on. That information is stored as
#' attributes on the object. `tbl_now_attributes()` shows you those, and only
#' those -- the bookkeeping attributes every `tibble` carries are left out.
#'
#' Use it when you want to check what an object thinks it is, especially after a
#' long `dplyr` pipeline.
#'
#' @param x A [tbl_now()] object.
#'
#' @return A named list of the attributes specific to the `tbl_now` class (those
#' not shared with a plain `tibble`). Attributes are only present when they were
#' set, so an object with no strata has no `strata` element.
#'
#' @seealso
#' [tbl_now()] and its *Attributes* section for what each attribute means; the
#' [getters][nowcast_data_getters] for reading one attribute at a time;
#' [change()] and [add()] for setting them; [validate_tbl_now()] to check they
#' are coherent.
#'
#' @examples
#' data(denguedat)
#' df_now <- tbl_now(denguedat,
#'   event_date = onset_week,
#'   report_date = report_week, strata = gender, verbose = FALSE
#' )
#'
#' ## `attributes()` returns everything, including tibble internals like `names`
#' # and `row.names`.
#' attributes(df_now) |> names()
#'
#' ## `tbl_now_attributes()` returns only what makes it a tbl_now.
#' tbl_now_attributes(df_now) |> names()
#'
#' # And their values: the roles it recorded, plus the `now` of the nowcast.
#' tbl_now_attributes(df_now)[c("event_date", "report_date", "strata", "now")]
#'
#' @export
tbl_now_attributes <- function(x) {
  if (!is_tbl_now(x)) {
    cli::cli_abort("Object is not a `tbl_now`")
  }

  # Get the attributes of a tbl now
  default_attributes <- tbl_now(
    data.frame(x = as.integer(0), y = as.integer(0)), "x", "y",
    data_type = "linelist",
    verbose = FALSE,
    event_units = "numeric",
    report_units = "numeric"
  ) |>
    attributes()

  tibble_attributes <- dplyr::tibble() |> attributes()

  # Everything on `x` that a bare tibble does not have. Diffing against the
  # DEFAULT tbl_now instead would silently drop any attribute the default does
  # not happen to carry -- which is every optional one (`validation_date`,
  # `validation_type`, `validation_units`), the exact case a user asks about.
  own <- setdiff(
    names(attributes(x)),
    c(names(tibble_attributes), names(default_attributes)[
      names(default_attributes) %in% names(tibble_attributes)
    ])
  )

  attributes(x)[own]
}

#' The complete grid of dates between two bounds, in an axis's own units
#'
#' `seq.Date()` matches its `by` string with `pmatch()`, so the plural unit
#' names the attributes carry (`"days"`, `"weeks"`, ...) work directly -- but
#' `"numeric"` is not a calendar unit and has to step by 1 instead. Every grid
#' in the package needs that distinction, and it was inlined in three different
#' shapes before this existed.
#'
#' @param from,to The bounds, `Date` or numeric.
#' @param units One of `"days"`, `"weeks"`, `"months"`, `"years"`, `"numeric"`.
#'
#' @return A vector of the same type as `from`, empty when `from > to`.
#'
#' @keywords internal
#' @noRd
.tbl_now_date_seq <- function(from, to, units) {
  if (length(from) == 0 || length(to) == 0 || is.na(from) || is.na(to)) {
    return(from[0])
  }
  if (from > to) {
    return(from[0])
  }
  if (identical(units, "numeric") || !lubridate::is.Date(from)) {
    seq(from, to, by = 1)
  } else {
    seq(from, to, by = as.character(units))
  }
}

#' How many units separate two dates
#'
#' Wraps `.date_difference_in_units()` so that `"numeric"` axes, which are not
#' dates at all, take the plain difference.
#'
#' @param from,to The bounds, `Date` or numeric. Either may be a vector.
#' @param units One of `"days"`, `"weeks"`, `"months"`, `"years"`, `"numeric"`.
#'
#' @return A numeric vector: `to - from`, expressed in `units`.
#'
#' @keywords internal
#' @noRd
.tbl_now_units_between <- function(from, to, units) {
  if (identical(units, "numeric") || !lubridate::is.Date(from)) {
    as.numeric(to) - as.numeric(from)
  } else {
    .date_difference_in_units(to, from, units)
  }
}

#' Compare probabilities up to floating-point noise
#'
#' Quantile levels are written as decimals and then arithmetic is done on them
#' (`1 - 2 * lower`, `(1 - level) / 2`), which does not round-trip: the two
#' expressions above disagree in the last bit, so `%in%` and `setdiff()` on
#' quantile levels silently match nothing. Compare with this instead.
#'
#' @param x,y Numeric vectors, recycled against each other.
#' @param tolerance Absolute tolerance.
#'
#' @return A logical vector.
#'
#' @keywords internal
#' @noRd
.near <- function(x, y, tolerance = 1e-8) {
  abs(x - y) < tolerance
}

#' Rebuild a `tbl_now` from modified data, carrying every attribute over
#'
#' The verbs that reshape a `tbl_now` all have to hand its attributes back to
#' [tbl_now()] one by one, and every such list is a place an attribute can be
#' dropped in silence -- which is how strata, and later the validation process,
#' each went missing from a rebuild. This is that list, in one place.
#'
#' The generated columns (`.event_num`, `.report_num`, `.delay`,
#' `.validation_num`, `.validation_delay`) are removed first, because
#' [tbl_now()] recomputes them and refuses to overwrite one that is already
#' there.
#'
#' `...` overrides any argument of [tbl_now()]; an override of `NULL` is
#' honoured (it drops the attribute) rather than being read as "not supplied".
#'
#' Note `.set_validation()` in `R/validation.R` does NOT go through this: it
#' receives its column names as tidyselect quosures (`{{ }}`), which cannot
#' survive a `do.call()` on a list.
#'
#' @param x The `tbl_now` whose attributes are being carried over.
#' @param data The rebuilt data frame (may be `x` itself).
#' @param ... Arguments of [tbl_now()] to override.
#'
#' @return A `tbl_now`.
#'
#' @keywords internal
#' @noRd
.tbl_now_rebuild <- function(x, data, ...) {
  generated <- c(
    ".event_num", ".report_num", ".delay",
    ".validation_num", ".validation_delay"
  )
  bare <- .strip_tbl_now(data)
  bare <- bare[, setdiff(colnames(bare), generated), drop = FALSE]

  args <- list(
    bare,
    event_date = get_event_date(x),
    report_date = get_report_date(x),
    case_count = get_case_count(x),
    strata = get_strata(x),
    covariates = get_covariates(x),
    is_censored_report = get_is_censored_report(x),
    data_type = get_data_type(x),
    now = get_now(x),
    event_units = get_event_units(x),
    report_units = get_report_units(x),
    t_effects = intersect(get_temporal_effect_cols(x), colnames(bare)),
    verbose = FALSE,
    warn_non_uniqueness = FALSE
  )
  args <- c(args, .validation_rebuild_args(x, bare))

  overrides <- list(...)
  # Single-bracket assignment so an override of NULL sets the element to NULL
  # instead of deleting it -- `args$strata <- NULL` would silently mean
  # "keep whatever was there".
  for (nm in names(overrides)) args[nm] <- overrides[nm]

  out <- do.call(tbl_now, args)

  # The lazy spec survives a reshape even when the materialised columns do not.
  attr(out, "temporal_effects") <- get_temporal_effects(x)
  out
}

#' Put a caller's grouping back after a verb that had to ungroup
#'
#' Several `tbl.now` verbs rebuild the object (`tbl_now()` refuses a grouped
#' data frame, and `add_is_censored_report()` refuses a `grouped_tbl_now`), so they
#' ungroup first. Losing the grouping is a change the caller did not ask for.
#'
#' @param x A `tbl_now`.
#' @param group_columns Character vector from [dplyr::group_vars()].
#'
#' @return `x`, grouped again by whichever of those columns still exist.
#'
#' @keywords internal
#' @noRd
.tbl_now_regroup <- function(x, group_columns) {
  group_columns <- intersect(group_columns, colnames(x))
  if (length(group_columns) == 0) {
    return(x)
  }
  dplyr::group_by(x, dplyr::across(dplyr::all_of(group_columns)))
}
