#' Compute temporal effects for a `tbl_now`
#'
#' @description
#' Materializes the lazy [temporal_effects()] specification stored in a `tbl_now`
#' into actual columns.  [add_temporal_effects()] and the `t_effects` argument of
#' [tbl_now()] only *record* what should be computed; this function performs the
#' computation.  If no temporal effects have been configured the object is
#' returned unchanged.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param x A `tbl_now` object.
#' @param overwrite Logical.  When `TRUE`, existing columns are silently
#'   overwritten.  When `FALSE` (default) an error is thrown if a column that
#'   would be created already exists.
#'
#' @return A `tbl_now` with the temporal-effect columns appended.  The
#'   `temporal_effects` attribute (the specification) is **preserved** so the
#'   spec is always visible when printing; in addition a new internal attribute
#'   `computed_temporal_effect_cols` records the names of the columns that were
#'   just created.
#'
#' @seealso [add_temporal_effects()], [temporal_effects()], [remove_temporal_effects()]
#'
#' @examples
#' data(denguedat)
#'
#' # 1. Define the spec (lazy — no columns are added yet)
#' df_now <- tbl_now(denguedat,
#'   event_date  = onset_week,
#'   report_date = report_week,
#'   t_effects   = temporal_effects(week_of_year = TRUE, month_of_year = TRUE),
#'   verbose     = FALSE
#' )
#'
#' # No temporal-effect columns yet:
#' ".event_week_of_year" %in% names(df_now) # FALSE
#'
#' # 2. Materialise the columns
#' df_computed <- compute_temporal_effects(df_now)
#' ".event_week_of_year" %in% names(df_computed) # TRUE
#'
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
    } else {
      date_col <- get_report_date(x)
      numeric_col <- ".report_num"
      name_prefix <- ".report"
    }

    x <- add_temporal_effects.data.frame(
      x,
      t_effects    = t_eff,
      date_col     = date_col,
      numeric_col  = numeric_col,
      name_prefix  = name_prefix,
      overwrite    = overwrite,
      weekend_days = weekend_days
    )
  }

  new_cols <- colnames(x)
  computed_cols <- setdiff(new_cols, old_cols)

  existing_computed <- attr(x, "computed_temporal_effect_cols", exact = TRUE)
  if (is.null(existing_computed)) existing_computed <- character(0)
  attr(x, "computed_temporal_effect_cols") <- c(existing_computed, computed_cols)

  return(x)
}
