# =============================================================================
# Inject a known batch into a tbl_now (for validation and teaching)
# =============================================================================
# NOTE ON PACKAGE PLACEMENT.  Model-free; destined for `tbl.now`.  See the header
# of `35_batch_screen_tbl_now.R`.
#
# This implements the deterministic release mechanism exactly as the theory
# defines it.  Let `H` be a set of "closed" report dates.  Define the
# next-open-date map
#
#     rho(u) = min{ v >= u : v not in H }
#
# and move every report whose report date falls in `H` to `rho(report date)`.
# Nothing is created and nothing is destroyed: each report keeps its event date
# and only its *report* date changes, and it can only ever move later.  That is
# precisely the definition of a transport, and it is what `batch_screen()` is
# built to detect.
# =============================================================================

#' Inject a batch into a `tbl_now` by withholding and then releasing reports
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Simulates a reporting system that is **closed** on a given set of report dates
#' and releases its accumulated backlog on the next open date.  Reports keep
#' their event dates and merely move *later* on the report axis, so no cases are
#' created or destroyed -- the defining property of a batch.  Useful for checking
#' that [batch_screen()] and [batch_shape_test()] recover a batch you planted.
#'
#' @details
#' # The mathematics
#'
#' A batch is a **transport**: a rule that moves an item's report date later while
#' leaving its event date untouched, creating and destroying nothing.  This
#' function implements the deterministic case exactly.  Let \eqn{H} be the set of
#' closed report dates and define the *next-open-date* map
#'
#' \deqn{\varrho(u) = \min\{\, v \ge u : v \notin H \,\}.}
#'
#' Every item with ideal report date \eqn{r^\star} is observed at
#' \eqn{r = \varrho(r^\star) \ge r^\star}, so its delay can only grow.  A maximal
#' closed run \eqn{\{b-L,\dots,b-1\}} followed by an open date \eqn{b} therefore
#' produces the four textbook symptoms at \eqn{b} -- a volume spike, inflated
#' delays, many contributing event dates, and \eqn{L} preceding empty dates --
#' from this single mechanism.  Because mass is conserved, the window total
#' spanning the run and the release is unchanged, which is exactly the invariant
#' [batch_screen()] tests (see its **Details**).
#'
#' # What the mechanism does
#'
#' Every report whose report date lies in `closed_dates` is re-stamped with the
#' first report date at or after it that is *not* closed.  Consequently:
#'
#' * the closed dates report nothing (the **deficit**);
#' * the release date reports its own items *plus* the whole backlog (the **spike**);
#' * items released late have inflated **delays**;
#' * the release date draws on an unusually large number of distinct **event dates**.
#'
#' All four symptoms come from the one mechanism, which is why they should not be
#' treated as four independent pieces of evidence.
#'
#' # Reports that never come back
#'
#' If a closed run extends to the end of the report axis there is no open date to
#' release into.  Those reports are then unobservable -- a stall that has not yet
#' cleared is indistinguishable from data loss, an honest identification failure.
#' `drop_unreleased = TRUE` (default) discards them, reproducing exactly what a
#' real analyst would see.
#'
#' # Cumulative data
#'
#' For `"count-cumulative"` data a report announces a running total.  When two
#' reports for the same event date are pushed onto the same release date, only
#' the later one survives: it is that report date's final word on the total.
#'
#' @param data A [tbl_now()] object.
#' @param closed_dates A vector of report dates on which the reporting system is
#'   closed. Must be coercible to the class of the report-date column.
#' @param drop_unreleased Logical; drop reports whose closed run never reopens
#'   before the end of the report axis. Default `TRUE`.
#' @param verbose Logical; report what was moved. Default `TRUE`.
#'
#' @returns A new `tbl_now` with the same event dates, strata and data type, and
#'   modified report dates.
#'
#' @seealso [batch_screen()], [batch_shape_test()]
#'
#' @examples
#' library(tbl.now)
#' data(denguedat, package = "tbl.now")
#'
#' dengue_tbl <- tbl_now(
#'   denguedat,
#'   event_date  = onset_week,
#'   report_date = report_week,
#'   data_type   = "linelist",
#'   verbose     = FALSE
#' )
#'
#' # Close the reporting desk for three consecutive weeks
#' closed <- as.Date(c("1990-06-04", "1990-06-11", "1990-06-18"))
#' batched_tbl <- simulate_batch(dengue_tbl, closed_dates = closed, verbose = FALSE)
#'
#' @export
simulate_batch <- function(data,
                           closed_dates,
                           drop_unreleased = TRUE,
                           verbose         = TRUE) {
  .batch_experimental_warning("simulate_batch")
  .batch_check_tbl_now(data)

  observations <- as.data.frame(data)
  event_col      <- get_event_date(data)
  report_col     <- get_report_date(data)
  data_type      <- get_data_type(data)
  strata_cols    <- get_strata(data)
  case_count_col <- get_case_count(data)
  report_unit    <- get_report_units(data) %||% "days"

  closed_dates <- .batch_coerce_dates(closed_dates, observations[[report_col]])
  if (length(closed_dates) == 0L) {
    cli::cli_abort("`closed_dates` is empty; there is no batch to simulate.")
  }

  # The open report-date grid the backlog can be released onto.
  report_grid <- seq(
    from = min(observations[[report_col]], na.rm = TRUE),
    to   = max(observations[[report_col]], na.rm = TRUE),
    by   = as.character(report_unit)
  )
  open_dates <- setdiff(report_grid, closed_dates)
  open_dates <- sort(.batch_restore_date_class(open_dates, report_grid))
  if (length(open_dates) == 0L) {
    cli::cli_abort("Every report date is closed; there is nowhere to release the backlog.")
  }

  # rho(u) = first open date at or after u; NA when the run never reopens.  Keep
  # the pre-move report date as a column so it survives the filtering below and
  # can order colliding cumulative reports.
  observations$.original_report <- observations[[report_col]]
  released_reports <- .batch_next_open_date(observations$.original_report, open_dates)

  moved_count      <- sum(!is.na(released_reports) & released_reports != observations$.original_report)
  unreleased_count <- sum(is.na(released_reports))

  observations[[report_col]] <- released_reports
  if (drop_unreleased) {
    observations <- observations[!is.na(observations[[report_col]]), , drop = FALSE]
  } else {
    still_held <- is.na(observations[[report_col]])
    observations[[report_col]][still_held] <- max(open_dates)
  }

  # A cumulative report announces a running total: when two reports for one event
  # date land on the same release date, the later one is that date's final word.
  if (identical(data_type, "count-cumulative")) {
    observations <- .batch_collapse_cumulative(
      observations, event_col, report_col, strata_cols
    )
  }
  observations$.original_report <- NULL

  if (verbose) {
    cli::cli_alert_info(
      "Moved {moved_count} report{?s} from {length(closed_dates)} closed date{?s}."
    )
    if (unreleased_count > 0L) {
      cli::cli_alert_warning(
        "{unreleased_count} report{?s} never reopened before the end of the report axis \\
         and {?was/were} {if (drop_unreleased) 'dropped' else 'held to the last open date'}."
      )
    }
  }

  .batch_rebuild_tbl_now(
    observations, data, event_col, report_col, case_count_col, strata_cols, data_type
  )
}

#' First open date at or after each report date (`NA` when none exists).
#'
#' `findInterval()` on the sorted open dates gives the position of the last open
#' date at or before each report date.  If that open date *is* the report date the
#' desk was open and nothing moves; otherwise the report is released one position
#' further along.  A report whose closed run never reopens falls past the end of
#' `open_dates` and becomes `NA`.
#' @keywords internal
#' @noRd
.batch_next_open_date <- function(report_dates, open_dates) {
  last_open_position <- findInterval(report_dates, open_dates)

  desk_was_open <- last_open_position >= 1L &
    open_dates[pmax(last_open_position, 1L)] == report_dates
  release_position <- ifelse(desk_was_open, last_open_position, last_open_position + 1L)

  # Class-preserving NA vector (Date stays Date).
  released <- open_dates[rep(NA_integer_, length(report_dates))]
  releasable <- release_position >= 1L & release_position <= length(open_dates)
  released[releasable] <- open_dates[release_position[releasable]]
  released
}

#' Keep the last cumulative report per (event, stratum, release date).
#'
#' Ordering is by the *original* report date, so "last" means the most recent
#' statement about the running total, not an arbitrary row order.
#' @keywords internal
#' @noRd
.batch_collapse_cumulative <- function(observations, event_col, report_col, strata_cols) {
  grouping_cols <- c(event_col, report_col, strata_cols)
  observations |>
    dplyr::arrange(!!!rlang::syms(grouping_cols), .data$.original_report) |>
    dplyr::group_by(!!!rlang::syms(grouping_cols)) |>
    dplyr::slice_tail(n = 1) |>
    dplyr::ungroup() |>
    as.data.frame()
}

#' Rebuild a `tbl_now` with the same roles after the report dates were moved.
#'
#' Note this carries the column roles and `data_type`, not any temporal-effect or
#' covariate annotations; re-attach those afterwards if you need them.
#' @keywords internal
#' @noRd
.batch_rebuild_tbl_now <- function(observations, data, event_col, report_col,
                                   case_count_col, strata_cols, data_type) {
  # `as.data.frame()` on a tbl_now exposes tbl.now's own bookkeeping columns
  # (`.event_num`, `.report_num`, `.delay`).  `tbl_now()` refuses to rebuild while
  # they are present, so drop every dot-prefixed column before handing it back.
  internal_columns <- grep("^\\.", names(observations), value = TRUE)
  if (length(internal_columns) > 0L) {
    observations <- observations[, setdiff(names(observations), internal_columns), drop = FALSE]
  }

  tbl_now_arguments <- list(
    observations,
    event_date  = as.symbol(event_col),
    report_date = as.symbol(report_col),
    data_type   = data_type,
    verbose     = FALSE
  )
  if (!is.null(case_count_col) && case_count_col %in% names(observations)) {
    tbl_now_arguments$case_count <- as.symbol(case_count_col)
  }
  if (length(strata_cols) > 0L) {
    tbl_now_arguments$strata <- dplyr::all_of(strata_cols)
  }
  existing_now <- tryCatch(get_now(data), error = function(e) NULL)
  if (!is.null(existing_now)) {
    tbl_now_arguments$now <- existing_now
  }

  suppressWarnings(do.call(tbl_now, tbl_now_arguments))
}

#' Coerce user-supplied closed dates to the class of the report column.
#' @keywords internal
#' @noRd
.batch_coerce_dates <- function(closed_dates, report_dates) {
  target_class <- class(report_dates)[1]
  coerced <- tryCatch(
    methods::as(closed_dates, target_class),
    error = function(e) {
      cli::cli_abort(
        "`closed_dates` could not be coerced to {.cls {target_class}}, the class of the report column."
      )
    }
  )
  unique(coerced)
}

#' `setdiff()` strips the Date class; restore it from the grid it came from.
#' @keywords internal
#' @noRd
.batch_restore_date_class <- function(stripped_dates, template_dates) {
  restored <- template_dates[template_dates %in% stripped_dates]
  restored
}
