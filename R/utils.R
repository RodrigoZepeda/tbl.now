#' Function to get an attribute from an object or default value
#'
#' Checks if attribute exists in object and returns `default` if not
#'
#' `r lifecycle::badge('experimental')`
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

#' Check whether a date is a weekday vs weekend (international definition)
#'
#' @description `r lifecycle::badge('stable')`
#'
#' Function that checks whether a date object is a weekday or weekend.
#'
#' @param date A Date (or POSIXt) object.
#' @param weekend_days A character or numeric vector defining weekend days.
#'
#'   * Numeric: must be integers in 1-7 corresponding to [lubridate::wday()] when `week_start = 1`.
#'
#'   * Character: any of c("Mon","Tuesday","wed",...) case-insensitive.
#'   Defaults to Saturday and Sunday (weekend_days = c("Sat", "Sun")).
#'
#'
#' @return A logical vector: TRUE if weekday, FALSE if weekend.
#'
#' @examples
#' is_weekday(as.Date("2020-04-22")) # TRUE (Wed)
#' is_weekday(as.Date("2020-04-19")) # FALSE (Sun)
#'
#' # Middle East weekend (Fri - Sat)
#' is_weekday(as.Date("2020-04-17"), weekend_days = c("Fri", "Sat"))
#'
#' # Weekend only on Friday
#' is_weekday(as.Date("2020-04-17"), weekend_days = "Friday")
#' is_weekday(as.Date("2020-04-18"), weekend_days = "Friday")
#'
#' # Weekend on Sun - Mon (numeric: 7 = Sun, 1 = Mon)
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

#' Show the names of the attributes of a `tbl_now`
#'
#' @description `r lifecycle::badge('stable')`
#'
#' Shows exclusively the attributes of a `tbl_now` that are not part
#' of the attributes of a `tibble` (or a grouped tibble).
#'
#' @inheritParams add
#'
#' @return A named list with the attributes that are specific to the `tbl_now`
#' class (i.e. those not shared with a plain `tibble`).
#'
#' @examples
#' data(denguedat)
#' df_now <- tbl_now(denguedat,
#'   event_date = onset_week,
#'   report_date = report_week, strata = gender, verbose = FALSE
#' )
#'
#' # Attributes gets all attributes
#' attributes(df_now) |> names()
#'
#' # tbl_now_attributes gets only those associated to the `tbl_now` class
#' tbl_now_attributes(df_now) |> names()
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
  # not happen to carry -- which is every optional one (`confirmation_date`,
  # `confirmation_type`, `confirmation_units`), the exact case a user asks about.
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
#' Wraps [.date_difference_in_units()] so that `"numeric"` axes, which are not
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
