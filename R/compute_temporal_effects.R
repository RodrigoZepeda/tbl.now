#' @rdname add_temporal_effects
#' @export
compute_temporal_effects <- function(x, overwrite = FALSE) {
  if (!inherits(x, "tbl_now")) {
    cli::cli_abort("{.arg x} must be a {.code tbl_now} object.")
  }

  specs <- attr(x, "temporal_effects", exact = TRUE)

  if (is.null(specs) || length(specs) == 0) {
    return(x)
  }

  old_cols <- colnames(x)

  for (spec in specs) {
    t_eff <- spec$t_effects
    date_type <- spec$date_type
    weekend_days <- spec$weekend_days

    if (date_type == "event_date") {
      date_col <- get_event_date(x)
      numeric_col <- ".event_num"
      name_prefix <- ".event"
      units <- get_event_units(x)
    } else {
      date_col <- get_report_date(x)
      numeric_col <- ".report_num"
      name_prefix <- ".report"
      units <- get_report_units(x)
    }

    x <- add_temporal_effects.data.frame(
      x,
      t_effects    = t_eff,
      date_col     = date_col,
      numeric_col  = numeric_col,
      name_prefix  = name_prefix,
      overwrite    = overwrite,
      weekend_days = weekend_days,
      units        = units
    )
  }

  new_cols <- colnames(x)
  computed_cols <- setdiff(new_cols, old_cols)

  existing_computed <- attr(x, "computed_temporal_effect_cols", exact = TRUE)
  if (is.null(existing_computed)) existing_computed <- character(0)
  attr(x, "computed_temporal_effect_cols") <- c(existing_computed, computed_cols)

  return(x)
}

#' Optionally materialise temporal effects, then drop the `tbl_now` classes
#'
#' Helper for the [tibble::as_tibble()] / [as.data.frame()] methods below.
#' When `compute` is `TRUE` it materialises the lazy temporal-effect spec (on a
#' copy, so the input is left unchanged); it then always strips the `tbl_now`
#' classes so the standard tibble / data.frame coercion takes over without
#' re-dispatching to the `tbl_now` methods.
#'
#' With `compute = FALSE` (the default) this is just a class strip. That default
#' is deliberate: \pkg{dplyr} calls [tibble::as_tibble()] / [as.data.frame()] on
#' `tbl_now` objects internally as cheap, non-materialising coercions (e.g.
#' `group_by()` -> `compute_groups()`, and the data mask used by `slice()` /
#' `mutate()` / `filter()`). Materialising in those code paths would break the
#' lazy contract and recurse, so we only materialise when the *caller* asks for
#' it via `compute_temporal_effects = TRUE`.
#'
#' @param x A `tbl_now` object.
#' @param compute Whether to materialise the temporal-effect spec first.
#'
#' @return A declassed data frame, carrying the materialised temporal-effect
#'   columns when `compute = TRUE`.
#'
#' @keywords internal
#' @noRd
.coerce_prep_tbl_now <- function(x, compute) {
  if (isTRUE(compute)) {
    # Materialise on ungrouped data so the calendar effects use a single global
    # reference date. On a grouped object `compute_temporal_effects()` would
    # derive a per-group reference (length > 1), giving a recycling warning and
    # wrong centring. The result is declassed below, so grouping is dropped
    # anyway.
    if (dplyr::is_grouped_df(x)) x <- dplyr::ungroup(x)
    x <- .materialize_temporal_effects(x)$x
  }
  .declass_tbl_now(x)
}

#' Coerce a `tbl_now` to a tibble or a data frame
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [tibble::as_tibble()] and [as.data.frame()] methods for `tbl_now`. They drop
#' the `tbl_now` class and metadata, returning a plain `tibble` / `data.frame`.
#'
#' Set `compute_temporal_effects = TRUE` to **materialise the lazy
#' [temporal_effects()] specification** with [compute_temporal_effects()] first,
#' so the holiday / Fourier / calendar columns are present in the result. The
#' input `tbl_now` is left unchanged (the spec is materialised on a copy).
#'
#' The default is `FALSE` on purpose: \pkg{dplyr} uses these coercions
#' internally as cheap declassers (for example inside `group_by()` and the data
#' mask behind `mutate()` / `filter()` / `slice()`). Materialising there would
#' both break the lazy `temporal_effects` contract and recurse, so materialising
#' is strictly opt-in. To get a modelling-ready frame with the effects computed,
#' either pass `compute_temporal_effects = TRUE` here or call
#' [compute_temporal_effects()] on the `tbl_now` beforehand.
#'
#' @param x A `tbl_now` (or `grouped_tbl_now`) object.
#' @param ... Passed on to the underlying [tibble::as_tibble()] /
#'   [base::as.data.frame()].
#' @param compute_temporal_effects Logical (default `FALSE`). When `TRUE`,
#'   materialise the lazy [temporal_effects()] spec before coercing so the
#'   temporal-effect columns are included.
#'
#' @return A `tibble` (for `as_tibble()`) or a `data.frame` (for
#'   `as.data.frame()`); it carries the temporal-effect columns when
#'   `compute_temporal_effects = TRUE`.
#'
#' @seealso [compute_temporal_effects()], [temporal_effects()]
#'
#' @examples
#' data(denguedat)
#' df_now <- tbl_now(denguedat,
#'   event_date  = onset_week,
#'   report_date = report_week,
#'   t_effects   = temporal_effects(week_of_year = TRUE),
#'   verbose     = FALSE
#' )
#'
#' ## Plain coercion leaves the spec lazy (no temporal-effect columns):
#' ".event_week_of_year" %in% names(tibble::as_tibble(df_now)) # FALSE
#'
#' # Opt in to materialise the holiday / Fourier / calendar columns:
#' as_tibble(df_now, compute_temporal_effects = TRUE)
#' as.data.frame(df_now, compute_temporal_effects = TRUE)
#'
#' @name as_tibble.tbl_now
NULL

# Re-export tibble's `as_tibble()` generic so `as_tibble(<tbl_now>)` works after
# `library(tbl.now)` without attaching tibble (mirrors the ggplot2 `autoplot`
# re-export).
#' @importFrom tibble as_tibble
#' @export
tibble::as_tibble

#' @rdname as_tibble.tbl_now
#' @exportS3Method tibble::as_tibble
as_tibble.tbl_now <- function(x, ..., compute_temporal_effects = FALSE) {
  tibble::as_tibble(.coerce_prep_tbl_now(x, compute_temporal_effects), ...)
}

#' @rdname as_tibble.tbl_now
#' @exportS3Method tibble::as_tibble
as_tibble.grouped_tbl_now <- function(x, ..., compute_temporal_effects = FALSE) {
  tibble::as_tibble(.coerce_prep_tbl_now(x, compute_temporal_effects), ...)
}

#' @rdname as_tibble.tbl_now
#' @exportS3Method base::as.data.frame
as.data.frame.tbl_now <- function(x, ..., compute_temporal_effects = FALSE) {
  as.data.frame(.coerce_prep_tbl_now(x, compute_temporal_effects), ...)
}

#' @rdname as_tibble.tbl_now
#' @exportS3Method base::as.data.frame
as.data.frame.grouped_tbl_now <- function(x, ..., compute_temporal_effects = FALSE) {
  as.data.frame(.coerce_prep_tbl_now(x, compute_temporal_effects), ...)
}
