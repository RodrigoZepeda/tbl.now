# `diagnose()` is a STRUCTURAL health check: everything it reports can be
# decided by looking at the object, deterministically, with no model and no
# hypothesis test. Drift and batches are therefore *signposts* rather than
# answers -- see `.diagnose_signposts()`.
#
# The findings engine is shared with `validate_tbl_now()`: one implementation,
# two presentations. `validate_tbl_now()` re-emits the findings as the `cli`
# conditions it has always emitted; `diagnose()` returns them as data. The two
# knobs that keep that honest are
#
#   * `floor`  -- the least severe status to report. `validate_tbl_now()` stops
#                 at `"note"` (it needs exactly one of them, see
#                 `.diagnose_validation_alerts()`), `diagnose()` reports
#                 everything.
#   * `deep`   -- whether to do work that costs a pass over the data and only
#                 ever yields a note. `validate_tbl_now()` is called from
#                 `tbl_now_can_reconstruct()`, i.e. on EVERY dplyr verb, so it
#                 must not grow an extra scan of the data per column.

# The generic -----------------------------------------------------------------

#' @title Diagnose a `tbl_now`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' `diagnose()` is a structural health check. It looks for the things that make
#' a nowcast wrong before any model is fitted -- dates out of order, missing
#' values, repeated rows, units that disagree, data after `now`, event dates too
#' recent to be complete -- and returns them as a tibble of findings, sorted
#' worst first.
#'
#' It is **deterministic and runs no statistical test.** Whether the reporting
#' delay drifts, and whether reports arrive in batches, are questions about a
#' *distribution*, not about the object's structure; `diagnose()` emits a
#' `"not_run"` signpost naming the function to call instead
#' ([diagnose_drift()], [diagnose_batches()]) rather than quietly running a test
#' whose method, window and multiplicity correction you did not choose.
#'
#' Every block is also available on its own -- see
#' [nowcast_diagnose_components] -- and `diagnose()` is exactly the
#' [dplyr::bind_rows()] of those pieces.
#'
#' @param x A `tbl_now` object.
#' @param ... Unused, for extensibility.
#' @param checks Character vector of checks to run, a subset of
#'   `c("declarations", "ordering", "missing", "duplicates", "units",
#'   "negatives", "now", "truncation", "strata", "signposts")`. Defaults to all
#'   of them.
#' @param by_strata Logical. Add one set of rows per stratum, for the checks
#'   that are naturally per-stratum (missingness, negative increments,
#'   right-truncation, the gap to `now`). Defaults to `TRUE` when the object has
#'   strata. The checks that are statements about the object as a whole
#'   (declarations, units, duplicates, ordering) are always reported once, with
#'   `stratum = "all"`.
#' @param strata Character vector of columns to stratify by. Defaults to
#'   `get_strata(x)`.
#' @param warn_non_uniqueness Logical. Run the duplicate-row check. Defaults to
#'   `TRUE` here, unlike [validate_tbl_now()], where it defaults to `FALSE`
#'   because it runs on every `dplyr` verb.
#'
#' @section The columns:
#'
#' Every function in this family returns the same schema, so results can be
#' stacked with [dplyr::bind_rows()] and filtered with [dplyr::filter()].
#'
#' \describe{
#'   \item{`check`}{Which block the row belongs to: `"declarations"`,
#'     `"ordering"`, `"missing"`, `"duplicates"`, `"units"`, `"negatives"`,
#'     `"now"`, `"truncation"`, `"strata"` or `"signposts"`.}
#'   \item{`scope`}{What the row is about: a column name, a time axis, a pair of
#'     axes, or `"all"`.}
#'   \item{`stratum`}{Which subset of the data the row describes: `"all"` for
#'     the pooled rows, or the stratum label otherwise.}
#'   \item{`status`}{An **ordered factor**, worst first, so the tibble sorts
#'     itself: `error` > `warning` > `note` > `ok` > `not_run` > `skipped`. See
#'     the section below.}
#'   \item{`n_affected`}{How many rows (or cases, or dates) the finding is
#'     about.}
#'   \item{`n_total`}{How many were considered.}
#'   \item{`prop`}{`n_affected / n_total`.}
#'   \item{`message`}{One human sentence, already formatted.}
#'   \item{`hint`}{What to do about it, or `NA`.}
#'   \item{`rows`}{A list-column of offending row indices, so
#'     `x[result$rows[[1]], ]` goes straight to the bad rows. Empty when the
#'     finding is not about particular rows, or when it was computed on a
#'     de-accumulated view whose rows are not the object's own.}
#' }
#'
#' @section What the statuses mean:
#'
#' \describe{
#'   \item{`error`}{[validate_tbl_now()] aborts on this. The object is not a
#'     usable `tbl_now`.}
#'   \item{`warning`}{[validate_tbl_now()] warns about this.}
#'   \item{`note`}{A `diagnose()`-only observation worth your attention. It is
#'     deliberately never promoted to a warning: `validate_tbl_now()` runs on
#'     every `dplyr` verb, and a new warning there would turn a quiet
#'     construction into a noisy one for data that has always been accepted.}
#'   \item{`ok`}{The check ran and found nothing.}
#'   \item{`not_run`}{A signpost: this question needs a statistical test, and
#'     `message` names the call that answers it.}
#'   \item{`skipped`}{Could not be assessed -- no confirmation process, the
#'     wrong data type, or an optional package that is not installed.}
#' }
#'
#' @return A tibble with the columns described above, sorted worst first.
#'
#' @seealso [nowcast_diagnose_components] for the individual blocks,
#'   [summary.tbl_now()] for the descriptive counterpart,
#'   [validate_tbl_now()] for the abort/warn presentation of the same findings.
#'
#' @examples
#' data(denguedat)
#' ndata <- tbl_now(denguedat,
#'   event_date = "onset_week",
#'   report_date = "report_week",
#'   strata = "gender",
#'   verbose = FALSE
#' )
#'
#' # Everything, worst first
#' diagnose(ndata)
#'
#' # Only what needs acting on
#' diagnose(ndata) |> dplyr::filter(status <= "note")
#'
#' # One block
#' diagnose(ndata, checks = "units")
#'
#' @md
#' @export
diagnose <- function(x, ...) {
  UseMethod("diagnose")
}

#' @rdname diagnose
#' @export
diagnose.default <- function(x, ...) {
  cli::cli_abort(
    "{.fn diagnose} needs a {.cls tbl_now} object. Got {.cls {class(x)}}."
  )
}

#' @rdname diagnose
#' @export
diagnose.tbl_now <- function(x, ..., checks = NULL, by_strata = NULL,
                             strata = NULL, warn_non_uniqueness = TRUE) {
  .tbl_now_findings(
    x,
    checks = checks, by_strata = by_strata, strata = strata,
    warn_non_uniqueness = warn_non_uniqueness, fn = "diagnose"
  )
}

# Components ------------------------------------------------------------------

#' @title Individual blocks of a `tbl_now` diagnosis
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Each function returns one block of [diagnose()], in the same schema, so they
#' can be stacked with [dplyr::bind_rows()] or used on their own.
#'
#' * `diagnose_declarations()` -- the attributes and the columns they name:
#'   types, existence, collisions, columns the object was never told about, and
#'   temporal effects that were added but never materialised.
#' * `diagnose_ordering()` -- the `event <= report <= confirmation` timeline.
#' * `diagnose_missing()` -- `NA` values, per column and per stratum. An `NA`
#'   *count* is reported neutrally: in a reporting triangle it means *not yet
#'   observed*, which is correct data rather than a defect.
#' * `diagnose_duplicates()` -- rows that repeat on the full key.
#' * `diagnose_units()` -- the declared units against each other, against the
#'   calendar the dates actually land on, and against the delay they produce.
#' * `diagnose_negatives()` -- negative counts, and the negative increments a
#'   downward revision leaves behind when cumulative data is de-accumulated.
#' * `diagnose_now()` -- anything dated after `now`, and how stale the object is.
#' * `diagnose_truncation()` -- how many recent event dates are still immature,
#'   and how much of their eventual total is probably still missing.
#' * `diagnose_strata()` -- the smallest and the sparsest stratum, and the
#'   confirmations still pending.
#' * `diagnose_signposts()` -- the questions [diagnose()] deliberately does not
#'   answer, and the call that answers each one.
#'
#' @inheritParams diagnose
#'
#' @return A tibble in the schema documented in [diagnose()].
#'
#' @seealso [diagnose()], which stacks all of these.
#'
#' @examples
#' data(denguedat)
#' ndata <- tbl_now(denguedat,
#'   event_date = "onset_week",
#'   report_date = "report_week",
#'   strata = "gender",
#'   verbose = FALSE
#' )
#'
#' diagnose_declarations(ndata)
#' diagnose_ordering(ndata)
#' diagnose_missing(ndata)
#' diagnose_units(ndata)
#' diagnose_now(ndata)
#' diagnose_signposts(ndata)
#'
#' @name nowcast_diagnose_components
#' @md
NULL

#' @rdname nowcast_diagnose_components
#' @export
diagnose_declarations <- function(x, by_strata = NULL, strata = NULL) {
  .tbl_now_findings(x, "declarations", by_strata, strata,
    fn = "diagnose_declarations"
  )
}

#' @rdname nowcast_diagnose_components
#' @export
diagnose_ordering <- function(x, by_strata = NULL, strata = NULL) {
  .tbl_now_findings(x, "ordering", by_strata, strata, fn = "diagnose_ordering")
}

#' @rdname nowcast_diagnose_components
#' @export
diagnose_missing <- function(x, by_strata = NULL, strata = NULL) {
  .tbl_now_findings(x, "missing", by_strata, strata, fn = "diagnose_missing")
}

#' @rdname nowcast_diagnose_components
#' @export
diagnose_duplicates <- function(x, by_strata = NULL, strata = NULL,
                                warn_non_uniqueness = TRUE) {
  .tbl_now_findings(x, "duplicates", by_strata, strata,
    warn_non_uniqueness = warn_non_uniqueness, fn = "diagnose_duplicates"
  )
}

#' @rdname nowcast_diagnose_components
#' @export
diagnose_units <- function(x, by_strata = NULL, strata = NULL) {
  .tbl_now_findings(x, "units", by_strata, strata, fn = "diagnose_units")
}

#' @rdname nowcast_diagnose_components
#' @export
diagnose_negatives <- function(x, by_strata = NULL, strata = NULL) {
  .tbl_now_findings(x, "negatives", by_strata, strata,
    fn = "diagnose_negatives"
  )
}

#' @rdname nowcast_diagnose_components
#' @export
diagnose_now <- function(x, by_strata = NULL, strata = NULL) {
  .tbl_now_findings(x, "now", by_strata, strata, fn = "diagnose_now")
}

#' @rdname nowcast_diagnose_components
#' @export
diagnose_truncation <- function(x, by_strata = NULL, strata = NULL) {
  .tbl_now_findings(x, "truncation", by_strata, strata,
    fn = "diagnose_truncation"
  )
}

#' @rdname nowcast_diagnose_components
#' @export
diagnose_strata <- function(x, by_strata = NULL, strata = NULL) {
  .tbl_now_findings(x, "strata", by_strata, strata, fn = "diagnose_strata")
}

#' @rdname nowcast_diagnose_components
#' @export
diagnose_signposts <- function(x, by_strata = NULL, strata = NULL) {
  .tbl_now_findings(x, "signposts", by_strata, strata,
    fn = "diagnose_signposts"
  )
}

# The engine ------------------------------------------------------------------

#' Every check the engine knows how to run, in reporting order
#'
#' @return A character vector of check slugs.
#'
#' @keywords internal
#' @noRd
.diagnose_checks <- function() {
  c(
    "declarations", "ordering", "missing", "duplicates", "units",
    "negatives", "now", "truncation", "strata", "signposts"
  )
}

#' The checks `validate_tbl_now()` presents as conditions
#'
#' The rest of the engine is `diagnose()`-only. This subset is also what keeps
#' `validate_tbl_now()` cheap: it runs on every `dplyr` verb, so it must not
#' grow a check that costs a pass over the data.
#'
#' @return A character vector of check slugs.
#'
#' @keywords internal
#' @noRd
.diagnose_validation_checks <- function() {
  c("declarations", "ordering", "missing", "duplicates", "now")
}

#' The `"note"` findings `validate_tbl_now()` emits, and how
#'
#' `validate_tbl_now()` shows errors and warnings, and would drown the console
#' if it also showed every note. It emits exactly one, as an alert rather than a
#' warning, because that is how the object has reported it since it existed and
#' downgrading a message to silence (or upgrading it to a warning) would change
#' what `tbl_now()` does on data it has always accepted.
#'
#' @return A data frame of `check` / `scope` pairs.
#'
#' @keywords internal
#' @noRd
.diagnose_validation_alerts <- function() {
  data.frame(check = "declarations", scope = "same_columns")
}

#' The status levels, worst first
#'
#' @return A character vector.
#'
#' @keywords internal
#' @noRd
.diagnose_status_levels <- function() {
  c("error", "warning", "note", "ok", "not_run", "skipped")
}

#' Is a status severe enough to be reported at this context's floor?
#'
#' Also used to gate work: a check that would only ever produce a note skips the
#' computation entirely when notes are not wanted.
#'
#' @param context A diagnose context.
#' @param status A status label.
#'
#' @return `TRUE` or `FALSE`.
#'
#' @keywords internal
#' @noRd
.diagnose_wants <- function(context, status) {
  levels <- .diagnose_status_levels()
  match(status, levels) <= match(context$floor, levels)
}

#' The findings tibble, the one implementation behind `diagnose()` and
#' `validate_tbl_now()`
#'
#' @param x A `tbl_now` object (or, from `validate_tbl_now()`, anything at all).
#' @param checks Character vector of check slugs, or `NULL` for all of them.
#' @param by_strata,strata As in [diagnose()].
#' @param warn_non_uniqueness,warn_now Gates kept for `validate_tbl_now()`'s
#'   arguments of the same name.
#' @param floor Least severe status to report.
#' @param deep Whether to run work that costs a pass over the data for a
#'   note-level finding.
#' @param assert Whether to abort when `x` is not a `tbl_now`. `FALSE` for
#'   `validate_tbl_now()`, which is what decides that in the first place.
#' @param fn Calling function, for messages.
#'
#' @return A tibble of findings, sorted worst first.
#'
#' @keywords internal
#' @noRd
.tbl_now_findings <- function(x, checks = NULL, by_strata = NULL, strata = NULL,
                              warn_non_uniqueness = TRUE, warn_now = TRUE,
                              floor = "skipped", deep = TRUE, assert = TRUE,
                              fn = "diagnose") {
  checks <- checks %||% .diagnose_checks()
  checks <- match.arg(checks, .diagnose_checks(), several.ok = TRUE)

  if (isTRUE(assert) && !inherits(x, "tbl_now")) {
    .assert_tbl_now(x, fn)
  }

  # Pre-flight. Every check below assumes the attributes exist and name real
  # columns, so this stage short-circuits instead of feeding the rest garbage.
  preflight <- .diagnose_preflight(x)
  if (nrow(preflight) > 0) {
    return(.diagnose_finalise(list(preflight), floor))
  }

  context <- .diagnose_context(
    x, by_strata, strata, fn,
    warn_non_uniqueness = warn_non_uniqueness, warn_now = warn_now,
    floor = floor, deep = deep
  )

  # The second short-circuit, and for the same reason as the pre-flight one: an
  # attribute that names no column, or names one of the wrong type, makes every
  # check after it either meaningless or an error of its own. This runs even
  # when `checks` did not ask for it, so that a component called on its own
  # reports the broken declaration rather than tripping over it.
  declarations <- .diagnose_declarations(context)
  fatal <- declarations[as.character(declarations$status) == "error", ,
    drop = FALSE
  ]
  if (nrow(fatal) > 0) {
    return(.diagnose_finalise(list(fatal), floor))
  }

  blocks <- lapply(checks, function(check) {
    switch(check,
      declarations = declarations,
      ordering     = .diagnose_ordering(context),
      missing      = .diagnose_missing(context),
      duplicates   = .diagnose_duplicates(context),
      units        = .diagnose_units(context),
      negatives    = .diagnose_negatives(context),
      now          = .diagnose_now(context),
      truncation   = .diagnose_truncation(context),
      strata       = .diagnose_strata(context),
      signposts    = .diagnose_signposts(context)
    )
  })

  .diagnose_finalise(blocks, floor)
}

#' Everything the checks need, computed once
#'
#' @inheritParams .tbl_now_findings
#'
#' @return A list. `cache` is an environment so that a block which pays for a
#'   [to_count()] can hand it to the next one.
#'
#' @keywords internal
#' @noRd
.diagnose_context <- function(x, by_strata, strata, fn, warn_non_uniqueness,
                              warn_now, floor, deep) {
  x <- ungroup(x)

  if (is.null(by_strata)) {
    by_strata <- get_num_strata(x) > 0 || !is.null(strata)
  }
  check_bool(by_strata, "by_strata")

  strata_cols <- if (isTRUE(by_strata)) {
    .tbl_now_resolve_strata_cols(x, strata)
  } else {
    character(0)
  }

  # Aligned to the rows of `x`, so a per-stratum finding can still report the
  # offending row indices of the object the user is holding.
  row_stratum <- if (length(strata_cols) > 0) {
    .tbl_now_strata_label(x, strata_cols)
  } else {
    rep("all", nrow(x))
  }
  labels <- if (length(strata_cols) > 0) {
    c("all", sort(unique(row_stratum)))
  } else {
    "all"
  }

  list(
    x = x, strata_cols = strata_cols, labels = labels,
    row_stratum = row_stratum,
    warn_non_uniqueness = warn_non_uniqueness, warn_now = warn_now,
    floor = floor, deep = isTRUE(deep),
    cache = new.env(parent = emptyenv())
  )
}

#' The summary context, built at most once per diagnosis
#'
#' The case-weighted blocks all need the same `count-incidence` view of the
#' object with the delays and the stratum label attached, which is exactly what
#' the summary family already builds. Reuse it rather than growing a second
#' description of the same table.
#'
#' @param context A diagnose context.
#'
#' @return A summary context, or `NULL` when one cannot be built.
#'
#' @keywords internal
#' @noRd
.diagnose_summary <- function(context) {
  if (!exists("summary", envir = context$cache, inherits = FALSE)) {
    assign(
      "summary",
      tryCatch(
        suppressMessages(suppressWarnings(.summary_context(
          context$x, length(context$strata_cols) > 0,
          if (length(context$strata_cols) > 0) context$strata_cols else NULL,
          "diagnose"
        ))),
        error = function(e) NULL
      ),
      envir = context$cache
    )
  }
  get("summary", envir = context$cache, inherits = FALSE)
}

#' Row indices of one stratum of `x`
#'
#' @param context A diagnose context.
#' @param label A stratum label, or `"all"`.
#'
#' @return A logical vector over the rows of `context$x`.
#'
#' @keywords internal
#' @noRd
.diagnose_rows <- function(context, label) {
  if (identical(label, "all")) {
    rep(TRUE, nrow(context$x))
  } else {
    context$row_stratum == label
  }
}

# Rows ------------------------------------------------------------------------

#' The canonical column order of a findings tibble
#'
#' @return A character vector of column names.
#'
#' @keywords internal
#' @noRd
.diagnose_schema <- function() {
  c(
    "check", "scope", "stratum", "status", "n_affected", "n_total",
    "prop", "message", "hint", "rows"
  )
}

#' One finding
#'
#' @param check,scope,stratum Row identifiers.
#' @param status One of `.diagnose_status_levels()`.
#' @param n_affected,n_total Counts; `prop` is their ratio.
#' @param message One formatted sentence.
#' @param hint What to do about it, or `NA`.
#' @param rows Offending row indices.
#'
#' @return A one-row tibble.
#'
#' @keywords internal
#' @noRd
.diagnose_row <- function(check, scope, status, message, stratum = "all",
                          n_affected = NA_real_, n_total = NA_real_,
                          hint = NA_character_, rows = integer(0)) {
  n_affected <- as.numeric(n_affected)
  n_total <- as.numeric(n_total)
  dplyr::tibble(
    check = check, scope = scope, stratum = stratum,
    status = factor(status, levels = .diagnose_status_levels(), ordered = TRUE),
    n_affected = n_affected,
    n_total = n_total,
    prop = if (is.na(n_total) || n_total == 0) NA_real_ else n_affected / n_total,
    message = message, hint = as.character(hint),
    rows = list(as.integer(rows))
  )
}

#' A finding whose status depends on whether anything was found
#'
#' @param found Number of offending rows.
#' @param status Status to use when `found > 0`.
#' @param clean Message for the `"ok"` case.
#' @inheritParams .diagnose_row
#'
#' @return A one-row tibble.
#'
#' @keywords internal
#' @noRd
.diagnose_count_row <- function(check, scope, found, n_total, status, message,
                                clean, stratum = "all", hint = NA_character_,
                                rows = integer(0)) {
  if (found > 0) {
    .diagnose_row(check, scope, status, message,
      stratum = stratum, n_affected = found, n_total = n_total,
      hint = hint, rows = rows
    )
  } else {
    .diagnose_row(check, scope, "ok", clean,
      stratum = stratum, n_affected = 0, n_total = n_total
    )
  }
}

#' Format one message, without styling and on one line
#'
#' `cli::format_inline()` emits ANSI escapes when the console supports them,
#' which is right for a condition and wrong for a column of a tibble. It also
#' keeps the source indentation of a template written across several lines,
#' which `cli_warn()` would have re-wrapped. The findings are data first: strip
#' the styling and the line breaks once, here, so both presentations read the
#' same string.
#'
#' @param ... Passed to [cli::format_inline()].
#' @param .envir Evaluation environment for the inline expressions.
#'
#' @return A character scalar.
#'
#' @keywords internal
#' @noRd
.diagnose_text <- function(..., .envir = parent.frame()) {
  formatted <- cli::ansi_strip(cli::format_inline(..., .envir = .envir))
  trimws(gsub("[[:space:]]+", " ", formatted))
}

#' Bind blocks into the canonical schema, worst first
#'
#' @param blocks A list of tibbles.
#' @param floor Least severe status to keep.
#'
#' @return A tibble with the columns of `.diagnose_schema()`, in that order.
#'
#' @keywords internal
#' @noRd
.diagnose_finalise <- function(blocks, floor = "skipped") {
  out <- dplyr::bind_rows(blocks)

  if (nrow(out) == 0) {
    return(dplyr::tibble(
      check = character(0), scope = character(0), stratum = character(0),
      status = factor(character(0),
        levels = .diagnose_status_levels(), ordered = TRUE
      ),
      n_affected = numeric(0), n_total = numeric(0), prop = numeric(0),
      message = character(0), hint = character(0), rows = list()
    ))
  }

  out$status <- factor(as.character(out$status),
    levels = .diagnose_status_levels(), ordered = TRUE
  )
  keep <- as.integer(out$status) <=
    match(floor, .diagnose_status_levels())
  out <- out[keep, , drop = FALSE]

  out <- dplyr::arrange(
    out, .data$status, .data$check, .data$scope, .data$stratum
  )
  dplyr::relocate(out, dplyr::any_of(.diagnose_schema()))
}

# Pre-flight ------------------------------------------------------------------

#' The checks that must pass before any other check means anything
#'
#' @param x The object.
#'
#' @return A tibble of `error` findings, empty when the object is well formed.
#'
#' @keywords internal
#' @noRd
.diagnose_preflight <- function(x) {
  rows <- list()

  if (!is.data.frame(x)) {
    rows <- c(rows, list(.diagnose_row(
      "class", "class", "error",
      .diagnose_text("Object must inherit from {.code data.frame}")
    )))
  }

  required <- c(
    "event_date", "report_date", "now", "event_units", "report_units",
    "data_type"
  )
  for (name in required) {
    if (is.null(attr(x, name, exact = TRUE))) {
      rows <- c(rows, list(.diagnose_row(
        "attributes", name, "error",
        .diagnose_text("Missing required attribute: {.val {name}}")
      )))
    }
  }

  dplyr::bind_rows(rows)
}

# Checks ----------------------------------------------------------------------

#' The attributes, the columns they name, and the columns they do not
#'
#' @param context A diagnose context.
#'
#' @return A tibble of findings.
#'
#' @keywords internal
#' @noRd
.diagnose_declarations <- function(context) {
  x <- context$x
  event_date <- get_event_date(x)
  report_date <- get_report_date(x)
  strata <- get_strata(x)
  covariates <- get_covariates(x)
  now <- get_now(x)
  report_units <- get_report_units(x)
  event_units <- get_event_units(x)
  data_type <- get_data_type(x)
  is_censored <- get_is_censored(x)
  case_count <- get_case_count(x)

  rows <- list()
  error <- function(scope, message, hint = NA_character_) {
    rows[[length(rows) + 1L]] <<-
      .diagnose_row("declarations", scope, "error", message, hint = hint)
  }

  # -- attribute types --------------------------------------------------------
  valid_units <- c("auto", "days", "weeks", "numeric", "months", "years")
  valid_types <- c("auto", "linelist", "count-incidence", "count-cumulative")

  if (!is.character(event_date) || length(event_date) != 1) {
    error("event_date", .diagnose_text(
      "Attribute {.val event_date} must be a character vector of length 1"
    ))
  }
  if (!is.character(report_date) || length(report_date) != 1) {
    error("report_date", .diagnose_text(
      "Attribute {.val report_date} must be a character vector of length 1"
    ))
  }
  if (!is.null(strata) && !is.character(strata)) {
    error("strata", .diagnose_text(
      "Attribute {.val strata} must be {.val NULL} or a character vector"
    ))
  }
  if (!is.null(covariates) && !is.character(covariates)) {
    error("covariates", .diagnose_text(
      "Attribute {.val covariates} must be {.val NULL} or a character vector"
    ))
  }
  if ((!lubridate::is.Date(now) && !is.integer(now)) || length(now) != 1) {
    error("now", .diagnose_text(
      "Attribute {.val now} must be a Date or integer object of length 1"
    ))
  }
  for (pair in list(
    list("report_units", report_units), list("event_units", event_units)
  )) {
    if (!is.character(pair[[2]]) || length(pair[[2]]) != 1 ||
      !pair[[2]] %in% valid_units) {
      error(pair[[1]], .diagnose_text(
        "Attribute {.val {pair[[1]]}} must be one of: {.val {valid_units}}"
      ))
    }
  }
  if (!is.character(data_type) || length(data_type) != 1 ||
    !data_type %in% valid_types) {
    error("data_type", .diagnose_text(
      "Attribute {.val data_type} must be one of: {.val {valid_types}}"
    ))
  }
  if (!is.null(is_censored) &&
    (length(is_censored) != 1 || !is.character(is_censored))) {
    error("is_censored", .diagnose_text(
      "Attribute {.val is_censored} must be {.val NULL} or a character vector
       of length 1"
    ))
  }

  # -- the columns those attributes name --------------------------------------
  named_column <- function(value) {
    is.character(value) && length(value) == 1 && !is.na(value)
  }
  if (named_column(event_date) && !event_date %in% colnames(x)) {
    error(event_date, .diagnose_text(
      "Column {.val {event_date}} (event_date) not found in data"
    ))
  }
  if (named_column(report_date) && !report_date %in% colnames(x)) {
    error(report_date, .diagnose_text(
      "Column {.val {report_date}} (report_date) not found in data"
    ))
  }
  if (named_column(is_censored) && !is_censored %in% colnames(x)) {
    error(is_censored, .diagnose_text(
      "Column {.val {is_censored}} (is_censored) not found in data"
    ))
  }
  for (column in strata) {
    if (is.character(column) && !column %in% colnames(x)) {
      error(column, .diagnose_text(
        "Strata column {.val {column}} not found in data"
      ))
    }
  }
  for (column in covariates) {
    if (is.character(column) && !column %in% colnames(x)) {
      error(column, .diagnose_text(
        "Covariate column {.val {column}} not found in data"
      ))
    }
  }

  # -- one column, one role ---------------------------------------------------
  for (column in intersect(covariates, strata)) {
    error(column, .diagnose_text(
      "Strata variable {.val {column}} cannot also be a covariate"
    ))
  }
  if (named_column(report_date) && report_date %in% strata) {
    error(report_date, .diagnose_text(
      "Report date {.val {report_date}} cannot be strata"
    ))
  }
  if (named_column(event_date) && event_date %in% strata) {
    error(event_date, .diagnose_text(
      "Event date {.val {event_date}} cannot be strata"
    ))
  }
  if (named_column(report_date) && report_date %in% covariates) {
    error(report_date, .diagnose_text(
      "Report date {.val {report_date}} cannot be a covariate"
    ))
  }
  if (named_column(event_date) && event_date %in% covariates) {
    error(event_date, .diagnose_text(
      "Event date {.val {event_date}} cannot be a covariate"
    ))
  }
  if (!is.null(is_censored) && any(is_censored %in% covariates)) {
    error("is_censored", .diagnose_text(
      "Censored indicator {.val {is_censored}} cannot be also a covariate"
    ))
  }
  if (!is.null(is_censored) && any(is_censored %in% strata)) {
    error("is_censored", .diagnose_text(
      "Censored indicator {.val {is_censored}} cannot be also strata"
    ))
  }

  # -- column types -----------------------------------------------------------
  for (pair in list(list(event_date, "event"), list(report_date, "report"))) {
    column <- pair[[1]]
    if (named_column(column) && column %in% colnames(x) &&
      !lubridate::is.Date(x[[column]]) && !is.integer(x[[column]])) {
      error(column, .diagnose_text(
        "Column {.val {column}} must be of class Date or integer"
      ))
    }
  }
  if (named_column(is_censored) && is_censored %in% colnames(x) &&
    !is.logical(x[[is_censored]])) {
    error(is_censored, .diagnose_text(
      "Column {.val {is_censored}} must be logical (TRUE/FALSE)"
    ))
  }

  if (identical(data_type, "count-incidence") ||
    identical(data_type, "count-cumulative")) {
    if (is.null(case_count) || !case_count %in% colnames(x)) {
      error("case_count", .diagnose_text(
        "Dropped case column {case_count %||% 'NULL'} when data_type was
         {data_type}."
      ))
    }
  }

  # -- notes ------------------------------------------------------------------
  # Every note below is O(1) in the number of rows, which is what lets
  # `validate_tbl_now()` run this block on every dplyr verb.
  if (.diagnose_wants(context, "note")) {
    if (named_column(event_date) && named_column(report_date) &&
      identical(event_date, report_date)) {
      rows[[length(rows) + 1L]] <- .diagnose_row(
        "declarations", "same_columns", "note",
        .diagnose_text(
          "Object has the same event and report dates with value
           {.val {event_date}}"
        ),
        hint = .diagnose_text(
          "Every delay is then zero by construction; there is nothing to
           nowcast."
        )
      )
    }

    undeclared <- .undeclared_cols(x)
    rows[[length(rows) + 1L]] <- .diagnose_count_row(
      "declarations", "undeclared", length(undeclared), ncol(x), "note",
      .diagnose_text(
        "{length(undeclared)} column{?s} {.val {undeclared}} {?is/are} not
         declared as strata or covariates."
      ),
      clean = .diagnose_text("Every column is declared or protected."),
      hint = .diagnose_text(
        "Declare {cli::qty(length(undeclared))}{?it/them} with {.code strata = }
         to model {?it/them} separately, or let {.fn to_count} pool
         {?it/them} away -- which is what the {.fn tbl_now_to_} converters do."
      )
    )

    pending_effects <- length(get_temporal_effects(x)) > 0 &&
      length(get_temporal_effect_cols(x)) == 0
    rows[[length(rows) + 1L]] <- .diagnose_row(
      "declarations", "temporal_effects",
      if (pending_effects) "note" else "ok",
      if (pending_effects) {
        .diagnose_text(
          "{length(get_temporal_effects(x))} temporal effect{?s} {?is/are}
           declared but no column has been materialised."
        )
      } else {
        .diagnose_text(
          "{length(get_temporal_effect_cols(x))} temporal effect column{?s}
           {?is/are} materialised."
        )
      },
      n_affected = length(get_temporal_effects(x)),
      n_total = length(get_temporal_effects(x)),
      hint = if (pending_effects) {
        .diagnose_text(
          "Temporal effects are lazy: call {.fn compute_temporal_effects} to
           create the columns, or let a converter do it for you."
        )
      } else {
        NA_character_
      }
    )
  }

  dplyr::bind_rows(rows)
}

#' The `event <= report <= confirmation` timeline
#'
#' @param context A diagnose context.
#'
#' @return A tibble of findings.
#'
#' @keywords internal
#' @noRd
.diagnose_ordering <- function(context) {
  x <- context$x
  event <- x[[get_event_date(x)]]
  report <- x[[get_report_date(x)]]
  confirmation <- if (has_confirmation(x)) {
    x[[get_confirmation_date(x)]]
  } else {
    NULL
  }

  rows <- list()

  before_event <- which(!is.na(event) & !is.na(report) & report < event)
  rows[[1]] <- .diagnose_count_row(
    "ordering", "event_to_report", length(before_event), nrow(x), "warning",
    .diagnose_text(
      "{length(before_event)} row{?s} {?has/have} a `report_date` before `event_date`"
    ),
    clean = .diagnose_text("Every report is on or after its event."),
    hint = .diagnose_text(
      "A negative reporting delay is not a delay; the two date columns may be
       swapped, or the rows may be data-entry errors."
    ),
    rows = before_event
  )

  if (is.null(confirmation)) {
    rows[[2]] <- .diagnose_row(
      "ordering", "report_to_confirmation", "skipped",
      .diagnose_text("The object carries no confirmation process.")
    )
    rows[[3]] <- .diagnose_row(
      "ordering", "event_to_confirmation", "skipped",
      .diagnose_text("The object carries no confirmation process.")
    )
    return(dplyr::bind_rows(rows))
  }

  before_report <- which(
    !is.na(confirmation) & !is.na(report) & confirmation < report
  )
  shown <- utils::head(before_report, 5)
  rows[[2]] <- .diagnose_count_row(
    "ordering", "report_to_confirmation", length(before_report), nrow(x),
    "warning",
    .diagnose_text(
      "{length(before_report)} row{?s} {?is/are} confirmed BEFORE they were reported."
    ),
    clean = .diagnose_text("Every confirmation is on or after its report."),
    hint = .diagnose_text(
      "The timeline is {.code event_date <= report_date <=
       confirmation_date}; a negative confirmation delay is not a delay.
       {cli::qty(length(shown))}First affected row{?s}: {.val {shown}}."
    ),
    rows = before_report
  )

  # The transitive case. A row whose `report_date` is missing escapes the check
  # above entirely, so a confirmation before the event goes unnoticed there.
  before_all <- which(
    !is.na(confirmation) & !is.na(event) & confirmation < event
  )
  rows[[3]] <- .diagnose_count_row(
    "ordering", "event_to_confirmation", length(before_all), nrow(x), "note",
    .diagnose_text(
      "{length(before_all)} row{?s} {?is/are} confirmed BEFORE the event happened."
    ),
    clean = .diagnose_text("Every confirmation is on or after its event."),
    hint = .diagnose_text(
      "A row with a missing {.field report_date} is only caught here."
    ),
    rows = before_all
  )

  dplyr::bind_rows(rows)
}

#' `NA` values, per column and per stratum
#'
#' @param context A diagnose context.
#'
#' @return A tibble of findings.
#'
#' @keywords internal
#' @noRd
.diagnose_missing <- function(context) {
  x <- context$x
  rows <- list()

  # The two dates are the only columns `validate_tbl_now()` has ever warned
  # about, and the only ones scanned when notes are not wanted.
  for (axis in c("event", "report")) {
    column <- if (identical(axis, "event")) {
      get_event_date(x)
    } else {
      get_report_date(x)
    }
    offending <- which(is.na(x[[column]]))
    rows[[length(rows) + 1L]] <- .diagnose_count_row(
      "missing", column, length(offending), nrow(x), "warning",
      .diagnose_text(
        "{length(offending)} row{?s} {?has/have} NA values in the {.field {axis}_date}
         column {.val {column}}."
      ),
      clean = .diagnose_text(
        "No missing values in the {.field {axis}_date} column {.val {column}}."
      ),
      hint = .diagnose_text(
        "A row with no {axis} date cannot be placed on the reporting triangle."
      ),
      rows = offending
    )
  }

  if (!context$deep || !.diagnose_wants(context, "note")) {
    return(dplyr::bind_rows(rows))
  }

  # -- everything else, per stratum -------------------------------------------
  case_count <- get_case_count(x)
  censoring <- get_is_censored(x)
  confirmation <- if (has_confirmation(x)) get_confirmation_date(x) else NULL
  confirmation_type <- if (has_confirmation(x)) {
    get_confirmation_type(x)
  } else {
    NULL
  }

  columns <- c(case_count, confirmation, confirmation_type, censoring,
               get_covariates(x))
  columns <- intersect(unique(columns), colnames(x))

  for (column in columns) {
    missing <- is.na(x[[column]])
    # An `NA` COUNT is not a defect: in a reporting triangle it means the cell
    # has not been observed yet, which is different from an observed zero. The
    # same goes for a confirmation date that has not come back. Say so, rather
    # than inviting the user to "fix" correct data.
    neutral <- identical(column, case_count) || identical(column, confirmation)
    for (label in context$labels) {
      selected <- .diagnose_rows(context, label)
      offending <- which(selected & missing)
      rows[[length(rows) + 1L]] <- .diagnose_count_row(
        "missing", column, length(offending), sum(selected), "note",
        .diagnose_text(
          "{length(offending)} row{?s} {?has/have} NA values in {.val {column}}."
        ),
        clean = .diagnose_text("No missing values in {.val {column}}."),
        stratum = label,
        hint = if (neutral) {
          .diagnose_text(
            "An NA here means {.emph not yet observed}, which is correct data
             rather than a defect: it is what tells a nowcast the cell is
             still open."
          )
        } else {
          .diagnose_text(
            "Rows with a missing {.val {column}} are dropped by anything that
             groups on it."
          )
        },
        rows = offending
      )
    }
  }

  # A missing stratum value is a statement about the object, not about one
  # stratum: the rows it affects are exactly the ones with no stratum to be in.
  for (column in intersect(get_strata(x), colnames(x))) {
    offending <- which(is.na(x[[column]]))
    rows[[length(rows) + 1L]] <- .diagnose_count_row(
      "missing", column, length(offending), nrow(x), "note",
      .diagnose_text(
        "{length(offending)} row{?s} {?has/have} NA values in the stratum column
         {.val {column}}."
      ),
      clean = .diagnose_text(
        "No missing values in the stratum column {.val {column}}."
      ),
      hint = .diagnose_text(
        "Those rows form a stratum of their own, labelled {.val NA}."
      ),
      rows = offending
    )
  }

  dplyr::bind_rows(rows)
}

#' Rows that repeat on the full key
#'
#' @param context A diagnose context.
#'
#' @return A tibble of findings.
#'
#' @keywords internal
#' @noRd
.diagnose_duplicates <- function(context) {
  x <- context$x

  if (identical(get_data_type(x), "linelist")) {
    return(.diagnose_row(
      "duplicates", "key", "skipped",
      .diagnose_text(
        "A line list is one row per case, so identical rows are two cases
         rather than a repeat."
      )
    ))
  }
  if (!isTRUE(context$warn_non_uniqueness)) {
    return(.diagnose_row(
      "duplicates", "key", "skipped",
      .diagnose_text("Not requested ({.code warn_non_uniqueness = FALSE}).")
    ))
  }

  # Any column that legitimately distinguishes two rows belongs in the key. The
  # confirmation columns are here because a case and its own retraction share an
  # (event, report) pair and are still two different rows -- left out, every
  # confirmed/retracted pair came out as an "exact duplicate", and the advice to
  # call `distinct()` would have deleted the retraction.
  key_cols <- unique(c(
    get_report_date(x), get_event_date(x), get_covariates(x), get_strata(x),
    get_is_censored(x), get_temporal_effect_cols(x),
    .confirmation_group_cols(x)
  ))
  key_cols <- intersect(key_cols, colnames(x))
  repeated <- which(duplicated(
    as.data.frame(.strip_tbl_now(x))[, key_cols, drop = FALSE]
  ))

  # Naming the culprit matters. The usual cause is a column the object was never
  # told about -- `sex` in `covid_colombia` -- and advising `distinct()` there
  # sends people in a circle: those rows ARE distinct, they differ in the
  # undeclared column.
  extra <- .undeclared_cols(x)
  cause <- if (length(extra) > 0) {
    .diagnose_text(
      "{length(extra)} column{?s} {.val {extra}} {?is/are} not declared, so
       {?it splits/they split} each cell into several rows."
    )
  } else {
    .diagnose_text("The rows are exact duplicates of one another.")
  }
  fix <- if (length(extra) > 0) {
    .diagnose_text(paste0(
      "{cli::qty(length(extra))}Declare {?it/them} with {.code strata = } to ",
      "model {?it/them} separately, or {.fn to_count} to pool {?it/them} ",
      "away. The {.fn tbl_now_to_} converters pool undeclared columns for ",
      "you, so this is a warning rather than an error."
    ))
  } else {
    .diagnose_text("Use {.fn dplyr::distinct} to drop the repeats.")
  }

  event_date <- get_event_date(x)
  report_date <- get_report_date(x)
  .diagnose_count_row(
    "duplicates", "key", length(repeated), nrow(x), "warning",
    .diagnose_text(
      "*Non-unique*: {length(repeated)} row{?s} {?shares/share} an
       ({event_date}, {report_date}) combination."
    ),
    clean = .diagnose_text(
      "Every row is unique on ({event_date}, {report_date}) and everything the
       object declares."
    ),
    hint = paste(cause, fix),
    rows = repeated
  )
}

#' The declared units, against each other and against the calendar
#'
#' @param context A diagnose context.
#'
#' @return A tibble of findings.
#'
#' @keywords internal
#' @noRd
.diagnose_units <- function(context) {
  x <- context$x
  event_units <- get_event_units(x)
  report_units <- get_report_units(x)
  confirmation_units <- if (has_confirmation(x)) {
    get_confirmation_units(x)
  } else {
    NULL
  }

  rows <- list()

  # -- 1. the declarations against each other ---------------------------------
  # `time_cols_to_numeric()` enforces the first two rules at construction, so
  # this row is mostly reachable through `change_event_date()` and friends. The
  # confirmation rule is not enforced anywhere: `.confirmation_num` is measured
  # in the CONFIRMATION units while `.event_num` and `.report_num` are in the
  # EVENT units, so `.confirmation_delay`, which subtracts one from the other,
  # only means anything when the two agree.
  order <- c("days", "weeks", "months", "years")
  declared <- c(event = event_units, report = report_units)
  if (!is.null(confirmation_units)) {
    declared <- c(declared, confirmation = confirmation_units)
  }

  # An unrecognised unit string is already an `error` from
  # `.diagnose_declarations()`; comparing it with the others would only add a
  # second, less useful, complaint about the same attribute.
  if (!all(declared %in% c(order, "numeric"))) {
    return(.diagnose_row(
      "units", "declared", "skipped",
      .diagnose_text(
        "{.val {setdiff(declared, c(order, \"numeric\"))}} is not a unit this
         class knows about."
      )
    ))
  }
  numeric_axes <- declared == "numeric"

  problems <- character(0)
  if (any(numeric_axes) && !all(numeric_axes)) {
    problems <- c(problems, .diagnose_text(
      "{.val numeric} is mixed with calendar units
       ({.val {declared[!numeric_axes]}}); an axis is either a calendar or a
       number line, never both."
    ))
  }
  if (!any(numeric_axes)) {
    if (match(report_units, order) < match(event_units, order)) {
      problems <- c(problems, .diagnose_text(
        "{.field report_units} ({.val {report_units}}) is finer than
         {.field event_units} ({.val {event_units}})."
      ))
    }
    if (!is.null(confirmation_units) &&
      !identical(confirmation_units, event_units)) {
      problems <- c(problems, .diagnose_text(
        "{.field confirmation_units} ({.val {confirmation_units}}) differs from
         {.field event_units} ({.val {event_units}}), so
         {.code .confirmation_delay} subtracts one scale from another."
      ))
    }
  }

  rows[[1]] <- .diagnose_count_row(
    "units", "declared", length(problems), length(declared), "note",
    paste(problems, collapse = " "),
    clean = .diagnose_text(
      "The declared units agree: {.val {declared}}."
    ),
    hint = .diagnose_text(
      "Set them with {.code event_units = } / {.code report_units = } /
       {.code confirmation_units = }, or move every axis to {.val numeric}."
    )
  )

  # -- 2. the declarations against the calendar the dates land on -------------
  # Every column is measured against ONE anchor, the earliest event date, which
  # is the anchor `time_cols_to_numeric()` uses for `.event_num` and
  # `.report_num`. Checking each column against its own minimum instead would
  # miss the pitfall this exists for: two weekly columns, each perfectly
  # regular, sitting on different weekdays.
  anchor <- suppressWarnings(min(x[[get_event_date(x)]], na.rm = TRUE))
  axes <- list(
    list(name = "event", column = get_event_date(x), units = event_units),
    list(name = "report", column = get_report_date(x), units = report_units)
  )
  if (!is.null(confirmation_units)) {
    axes <- c(axes, list(list(
      name = "confirmation", column = get_confirmation_date(x),
      units = confirmation_units
    )))
  }

  for (axis in axes) {
    offending <- .diagnose_off_grid(x[[axis$column]], axis$units, anchor)
    rows[[length(rows) + 1L]] <- .diagnose_count_row(
      "units", paste0(axis$name, "_grid"), length(offending),
      sum(!is.na(x[[axis$column]])), "note",
      .diagnose_text(
        "{.val {axis$column}} is declared {.val {axis$units}} but
         {length(offending)} of its dates {?does/do} not sit on the same grid as the
         earliest event date ({.val {as.character(anchor)}})."
      ),
      clean = .diagnose_text(
        "{.val {axis$column}} lands on the object's {.val {axis$units}} grid."
      ),
      hint = .diagnose_text(
        "Weekly columns on different weekdays are the usual cause; fix them
         with {.fn align_weeks} rather than rounding."
      ),
      rows = offending
    )
  }

  # -- 3. the delay those units produce ---------------------------------------
  delay <- x[[".delay"]]
  fractional <- if (is.null(delay)) {
    integer(0)
  } else {
    which(!is.na(delay) & abs(delay - round(delay)) > sqrt(.Machine$double.eps))
  }
  rows[[length(rows) + 1L]] <- .diagnose_count_row(
    "units", "delay", length(fractional), nrow(x), "note",
    .diagnose_text(
      "{length(fractional)} row{?s} {?has/have} a fractional {.code .delay}."
    ),
    clean = .diagnose_text("Every {.code .delay} is a whole number of units."),
    hint = .diagnose_text(
      "A fractional delay is what a converter chokes on: the two date columns
       are on different grids. {.fn align_weeks} is the fix for weekly data."
    ),
    rows = fractional
  )

  dplyr::bind_rows(rows)
}

#' Dates that do not sit on the grid their units claim
#'
#' Congruence with `anchor`, the date `.event_num` and `.report_num` are
#' measured from. Days and numeric axes are always on grid.
#'
#' @param dates A date or numeric vector.
#' @param units One of the unit strings.
#' @param anchor The date the grid starts at.
#'
#' @return Integer positions of the off-grid values.
#'
#' @keywords internal
#' @noRd
.diagnose_off_grid <- function(dates, units, anchor) {
  if (is.null(dates) || !lubridate::is.Date(dates) ||
    !lubridate::is.Date(anchor) || length(anchor) != 1 || is.na(anchor)) {
    return(integer(0))
  }
  observed <- !is.na(dates)
  if (!any(observed)) {
    return(integer(0))
  }
  off <- switch(units,
    weeks = as.numeric(dates - anchor) %% 7 != 0,
    months = lubridate::mday(dates) != lubridate::mday(anchor),
    years = lubridate::yday(dates) != lubridate::yday(anchor),
    rep(FALSE, length(dates))
  )
  which(observed & off)
}

#' Negative counts, and the negative increments a revision leaves behind
#'
#' @param context A diagnose context.
#'
#' @return A tibble of findings.
#'
#' @keywords internal
#' @noRd
.diagnose_negatives <- function(context) {
  x <- context$x
  data_type <- get_data_type(x)

  if (identical(data_type, "linelist")) {
    return(.diagnose_row(
      "negatives", "count", "skipped",
      .diagnose_text("A line list has no count column to go negative.")
    ))
  }

  if (identical(data_type, "count-incidence")) {
    counts <- x[[get_case_count(x)]]
    rows <- lapply(context$labels, function(label) {
      selected <- .diagnose_rows(context, label)
      offending <- which(selected & !is.na(counts) & counts < 0)
      .diagnose_count_row(
        "negatives", "count", length(offending), sum(selected), "note",
        .diagnose_text(
          "{length(offending)} row{?s} {?carries/carry} a negative incidence count
           (total {round(sum(counts[offending]), 2)})."
        ),
        clean = .diagnose_text("No negative incidence counts."),
        stratum = label,
        hint = .diagnose_text(
          "A negative incidence is a downward revision recorded as if it were a
           new arrival. Most nowcasting engines refuse it."
        ),
        rows = offending
      )
    })
    return(dplyr::bind_rows(rows))
  }

  # count-cumulative: the negatives only appear once the totals are
  # de-accumulated, and the rows of that view are not the rows of `x`, so the
  # indices are deliberately left empty.
  summary <- .diagnose_summary(context)
  if (is.null(summary)) {
    return(.diagnose_row(
      "negatives", "increment", "skipped",
      .diagnose_text("The cumulative totals could not be de-accumulated.")
    ))
  }
  cases <- summary$cases
  rows <- lapply(context$labels, function(label) {
    selected <- if (identical(label, "all")) {
      rep(TRUE, nrow(cases))
    } else {
      cases$stratum == label
    }
    offending <- selected & !is.na(cases$count) & cases$count < 0
    .diagnose_count_row(
      "negatives", "increment", sum(offending), sum(selected), "note",
      .diagnose_text(
        "{sum(offending)} de-accumulated increment{?s} {?is/are} negative
         (total {round(sum(cases$count[offending]), 2)})."
      ),
      clean = .diagnose_text("Every cumulative total is non-decreasing."),
      stratum = label,
      hint = .diagnose_text(
        "A cumulative total that goes down is a revision.
         {.code to_count(x, to = \"count-incidence\")} shows the increments;
         the row indices are its rows, not this object's, so none are given."
      )
    )
  })
  dplyr::bind_rows(rows)
}

#' Anything dated after `now`, and how stale the object is
#'
#' @param context A diagnose context.
#'
#' @return A tibble of findings.
#'
#' @keywords internal
#' @noRd
.diagnose_now <- function(context) {
  x <- context$x
  now_value <- get_now(x)
  rows <- list()

  # A confirmation is an OBSERVATION, so nothing can have been confirmed after
  # the as-of moment. This is the same rule `now` already obeys for reports;
  # breaking it means the object claims to know something it could not have.
  if (has_confirmation(x)) {
    confirmation <- x[[get_confirmation_date(x)]]
    latest <- suppressWarnings(max(confirmation, na.rm = TRUE))
    after <- which(!is.na(confirmation) & confirmation > now_value)
    rows[[length(rows) + 1L]] <- .diagnose_count_row(
      "now", "confirmation_date", length(after), nrow(x), "error",
      .diagnose_text(
        "The latest confirmation ({.val {as.character(latest)}}) is AFTER
         {.field now} ({.val {as.character(now_value)}}). Nothing can be
         confirmed after the as-of moment."
      ),
      clean = .diagnose_text("No confirmation is dated after {.field now}."),
      hint = .diagnose_text(
        "Move {.field now} forward with {.fn change_now}, or drop the rows."
      ),
      rows = after
    )

    allowed <- .confirmation_levels()
    type_col <- get_confirmation_type(x)
    if (!is.null(type_col) && type_col %in% colnames(x)) {
      values <- as.character(x[[type_col]])
      unknown <- setdiff(stats::na.omit(unique(values)), allowed)
      offending <- which(values %in% unknown)
      rows[[length(rows) + 1L]] <- .diagnose_count_row(
        "now", "confirmation_type", length(offending), nrow(x), "error",
        .diagnose_text(
          "{.field confirmation_type} has unrecognised value{?s}:
           {.val {unknown}}."
        ),
        clean = .diagnose_text(
          "Every {.field confirmation_type} is one of {.val {allowed}}."
        ),
        hint = .diagnose_text("The allowed values are {.val {allowed}}."),
        rows = offending
      )
    }
  }

  report <- x[[get_report_date(x)]]
  if (nrow(x) > 0 && isTRUE(context$warn_now)) {
    max_report <- suppressWarnings(max(report, na.rm = TRUE))
    stale <- !is.na(max_report) && !is.null(now_value) &&
      lubridate::is.Date(now_value) && now_value < max_report
    rows[[length(rows) + 1L]] <- .diagnose_count_row(
      "now", "report_date", as.numeric(stale), 1, "warning",
      .diagnose_text(
        "Attribute 'now' ({as.character(now_value)}) seems to be in the past
         (before maximum report_date ({as.character(max_report)}))"
      ),
      clean = .diagnose_text("{.field now} is on or after the last report."),
      hint = .diagnose_text(
        "Set it with {.fn change_now}, or let {.fn update_now} take the maximum."
      ),
      # Only worth a pass over the column once we know there is something to
      # find: this runs on every dplyr verb.
      rows = if (stale) which(!is.na(report) & report > now_value) else integer(0)
    )
  }

  if (!.diagnose_wants(context, "note")) {
    return(dplyr::bind_rows(rows))
  }

  event <- x[[get_event_date(x)]]
  after_now <- which(!is.na(event) & event > now_value)
  rows[[length(rows) + 1L]] <- .diagnose_count_row(
    "now", "event_date", length(after_now), nrow(x), "note",
    .diagnose_text(
      "{length(after_now)} row{?s} {?has/have} an event date after {.field now}
       ({.val {as.character(now_value)}})."
    ),
    clean = .diagnose_text("No event is dated after {.field now}."),
    hint = .diagnose_text(
      "An event that has not happened yet cannot have been reported; the rows
       are usually a typo in the year."
    ),
    rows = after_now
  )

  if (!context$deep) {
    return(dplyr::bind_rows(rows))
  }

  # The gap to `now` is already computed, per stratum, by the summary family --
  # do not derive it a second time. This is the internal that
  # `triangle_occupancy()` wraps, called on the context this diagnosis has
  # already paid for rather than on a second `to_count()` of the same object.
  summary <- .diagnose_summary(context)
  occupancy <- if (is.null(summary)) {
    NULL
  } else {
    tryCatch(
      suppressMessages(suppressWarnings(.summary_occupancy(summary))),
      error = function(e) NULL
    )
  }
  if (is.null(occupancy)) {
    rows[[length(rows) + 1L]] <- .diagnose_row(
      "now", "gap", "skipped",
      .diagnose_text("The reporting triangle could not be laid out.")
    )
    return(dplyr::bind_rows(rows))
  }

  for (axis in c("event", "report")) {
    quantity <- paste0("now_gap_", axis)
    units <- if (identical(axis, "event")) {
      get_event_units(x)
    } else {
      get_report_units(x)
    }
    piece <- occupancy[occupancy$quantity == quantity, , drop = FALSE]
    for (index in seq_len(nrow(piece))) {
      gap <- piece$value[index]
      # "1 weeks" reads as a typo; the unit names are stored plural.
      unit_label <- if (isTRUE(gap == 1)) sub("s$", "", units) else units
      rows[[length(rows) + 1L]] <- .diagnose_row(
        "now", quantity,
        if (is.na(gap) || gap <= 0) "ok" else "note",
        if (is.na(gap)) {
          .diagnose_text("No {axis} date to measure the gap from.")
        } else {
          .diagnose_text(
            "The last {axis} date is {round(gap, 2)} {unit_label} before
             {.field now} ({.val {as.character(now_value)}})."
          )
        },
        stratum = piece$stratum[index],
        n_affected = gap, n_total = NA_real_,
        hint = .diagnose_text(
          "Everything in that window is still arriving; it is what a nowcast is
           for, and it is also what makes the last points of any plot look
           like a decline."
        )
      )
    }
  }

  dplyr::bind_rows(rows)
}

#' How much of the recent data has not arrived yet
#'
#' @param context A diagnose context.
#'
#' @return A tibble of findings.
#'
#' @keywords internal
#' @noRd
.diagnose_truncation <- function(context) {
  x <- context$x
  summary <- .diagnose_summary(context)
  if (is.null(summary)) {
    return(.diagnose_row(
      "truncation", "event_date", "skipped",
      .diagnose_text("The delay distribution could not be built.")
    ))
  }

  cases <- summary$cases
  usable <- !is.na(cases$event_to_report) & !is.na(cases$count) &
    cases$count > 0
  if (!any(usable)) {
    return(.diagnose_row(
      "truncation", "event_date", "skipped",
      .diagnose_text("No positive counts with a delay to measure maturity by.")
    ))
  }

  # The same maturity rule `autoplot()` and `reporting_completeness()` use:
  # `now` minus the 95th percentile of the delay distribution. One rule, one
  # place.
  cutoff <- .tbl_now_maturity_threshold(
    x,
    dplyr::tibble(
      delay = cases$event_to_report[usable], weight = cases$count[usable]
    ),
    0.95
  )
  if (is.na(cutoff)) {
    return(.diagnose_row(
      "truncation", "event_date", "skipped",
      .diagnose_text("The delay distribution has no 95th percentile to cut at.")
    ))
  }

  now_value <- get_now(x)
  event_units <- get_event_units(x)
  mature <- usable & cases$event_date <= cutoff

  rows <- lapply(context$labels, function(label) {
    selected <- if (identical(label, "all")) {
      usable
    } else {
      usable & cases$stratum == label
    }
    immature <- selected & cases$event_date > cutoff
    dates <- sort(unique(cases$event_date[immature]))

    if (length(dates) == 0) {
      return(.diagnose_row(
        "truncation", "event_date", "ok",
        .diagnose_text("Every event date is old enough to be complete."),
        stratum = label, n_affected = 0,
        n_total = length(unique(cases$event_date[selected]))
      ))
    }

    # Share of an event date's eventual total that has arrived by delay d,
    # estimated on the MATURE dates only -- the immature ones are the very
    # thing being corrected for.
    reference <- mature & (identical(label, "all") | cases$stratum == label)
    if (!any(reference)) reference <- mature
    weights <- cases$count[reference]
    delays <- cases$event_to_report[reference]
    eventual <- sum(weights)

    # Both of these are one pass rather than one pass per date: on a
    # multi-year daily series there are hundreds of immature dates and
    # hundreds of thousands of rows, and the nested form is quadratic.
    slot <- match(cases$event_date[immature], dates)
    observed <- as.numeric(tapply(
      cases$count[immature], factor(slot, levels = seq_along(dates)), sum
    ))
    observed[is.na(observed)] <- 0

    ages <- floor(.tbl_now_units_between(dates, now_value, event_units))
    arrived <- if (eventual <= 0) {
      rep(NA_real_, length(ages))
    } else {
      # `findInterval()` on the sorted delays is the count of cases whose delay
      # is at most `age`, ties included -- the empirical CDF, read at the age
      # each immature date has reached.
      ordering <- order(delays)
      running <- cumsum(weights[ordering])
      position <- findInterval(ages, delays[ordering])
      ifelse(position == 0, 0, running[pmax(position, 1L)]) / eventual
    }

    still_missing <- sum(
      ifelse(is.na(arrived) | arrived <= 0, NA_real_, observed / arrived - observed),
      na.rm = TRUE
    )
    share <- still_missing / (still_missing + sum(observed))

    .diagnose_row(
      "truncation", "event_date", "note",
      .diagnose_text(
        "{length(dates)} event date{?s} {?is/are} younger than the 95th
         percentile of the delay, so {?its/their} counts are still filling in;
         an estimated {round(100 * share, 1)}%
         {cli::qty(length(dates))}of {?its/their} eventual total has not
         arrived."
      ),
      stratum = label,
      n_affected = length(dates),
      n_total = length(unique(cases$event_date[selected])),
      hint = .diagnose_text(
        "This is right-truncation, and it is the reason to nowcast rather than
         a defect. Cut the series at {.val {as.character(cutoff)}} to describe
         it instead."
      )
    )
  })

  dplyr::bind_rows(rows)
}

#' The smallest and the sparsest stratum, and the confirmations still pending
#'
#' @param context A diagnose context.
#'
#' @return A tibble of findings.
#'
#' @keywords internal
#' @noRd
.diagnose_strata <- function(context) {
  x <- context$x
  rows <- list()

  if (length(context$strata_cols) == 0) {
    rows[[1]] <- .diagnose_row(
      "strata", "size", "skipped",
      .diagnose_text("The object has no strata to compare.")
    )
  } else {
    summary <- .diagnose_summary(context)
    if (is.null(summary)) {
      rows[[1]] <- .diagnose_row(
        "strata", "size", "skipped",
        .diagnose_text("The strata could not be counted.")
      )
    } else {
      # No threshold decides what is "too small to fit": that depends on the
      # engine and on the epidemic. Name the extremes and let the reader judge.
      shares <- .summary_strata_shares(summary)
      if (nrow(shares) > 0) {
        smallest <- shares[which.min(shares$total), ]
        rows[[length(rows) + 1L]] <- .diagnose_row(
          "strata", "size", "note",
          .diagnose_text(
            "The smallest stratum is {.val {sub('^strata = ', '',
             smallest$quantity)}} with {round(smallest$total)} case{?s},
             {round(100 * smallest$prop, 1)}% of the total."
          ),
          stratum = sub("^strata = ", "", smallest$quantity),
          n_affected = smallest$total,
          n_total = sum(shares$total)
        )
      }

      occupancy <- .summary_cases(summary, "event")
      occupancy <- occupancy[
        occupancy$quantity == "per_event_date" & occupancy$stratum != "all", ,
        drop = FALSE
      ]
      if (nrow(occupancy) > 0 && any(!is.na(occupancy$prop_zero))) {
        sparsest <- occupancy[which.max(occupancy$prop_zero), ]
        rows[[length(rows) + 1L]] <- .diagnose_row(
          "strata", "sparsity", "note",
          .diagnose_text(
            "The sparsest stratum is {.val {sparsest$stratum}}:
             {round(100 * sparsest$prop_zero, 1)}% of the event dates on the
             grid carry no cases at all."
          ),
          stratum = sparsest$stratum,
          n_affected = sparsest$prop_zero * sparsest$n,
          n_total = sparsest$n,
          hint = .diagnose_text(
            "A stratum that is mostly zeros is the one a per-stratum fit will
             struggle with; pooling it is often better than fitting it."
          )
        )
      }
    }
  }

  # -- the confirmation backlog ----------------------------------------------
  if (!has_confirmation(x)) {
    rows[[length(rows) + 1L]] <- .diagnose_row(
      "strata", "pending", "skipped",
      .diagnose_text("The object carries no confirmation process.")
    )
    return(dplyr::bind_rows(rows))
  }

  summary <- .diagnose_summary(context)
  if (is.null(summary)) {
    rows[[length(rows) + 1L]] <- .diagnose_row(
      "strata", "pending", "skipped",
      .diagnose_text("The confirmations could not be counted.")
    )
    return(dplyr::bind_rows(rows))
  }

  cases <- summary$cases
  pending <- cases$confirmation_type %in% "pending"
  turnaround <- cases$report_to_confirmation
  typical <- .tbl_now_weighted_quantile(
    turnaround[!pending], cases$count[!pending], 0.5
  )
  now_value <- get_now(x)
  report_units <- get_report_units(x)
  waited <- .tbl_now_units_between(cases$report_date, now_value, report_units)

  for (label in context$labels) {
    selected <- if (identical(label, "all")) {
      rep(TRUE, nrow(cases))
    } else {
      cases$stratum == label
    }
    open <- selected & pending
    total <- sum(cases$count[selected])
    overdue <- if (is.na(typical)) {
      0
    } else {
      sum(cases$count[open & !is.na(waited) & waited > typical])
    }
    open_cases <- sum(cases$count[open])
    share <- if (total > 0) round(100 * open_cases / total, 1) else NA_real_
    # With nothing resolved yet there is no turnaround to be overdue against,
    # so say that rather than printing "longer than NA days".
    against <- if (is.na(typical)) {
      .diagnose_text(
        "nothing has been confirmed yet, so there is no turnaround to compare
         them with"
      )
    } else {
      .diagnose_text(
        "{round(overdue)} of them have waited longer than the median turnaround
         of {round(typical, 2)} {report_units}"
      )
    }
    rows[[length(rows) + 1L]] <- .diagnose_count_row(
      "strata", "pending", open_cases, total, "note",
      .diagnose_text(
        "{round(open_cases)} case{?s} {?is/are} still pending, {share}% of the
         stratum; {against}."
      ),
      clean = .diagnose_text("No confirmation is still pending."),
      stratum = label,
      hint = .diagnose_text(
        "A pending case has no confirmation date, so it is invisible to
         anything counting arrivals on the confirmation axis."
      )
    )
  }

  dplyr::bind_rows(rows)
}

#' The questions `diagnose()` deliberately does not answer
#'
#' Drift and batching are statements about a distribution, not about the
#' object's structure. Answering them means choosing a method, a maturity
#' window and a multiplicity correction, and `diagnose()` has no business
#' choosing those on the user's behalf -- so it points at the function that
#' does, instead of quietly running one.
#'
#' @param context A diagnose context.
#'
#' @return A tibble of findings.
#'
#' @keywords internal
#' @noRd
.diagnose_signposts <- function(context) {
  x <- context$x
  confirmed <- has_confirmation(x)
  has_modifiedmk <- requireNamespace("modifiedmk", quietly = TRUE)

  signpost <- function(check, scope, call, why) {
    if (identical(scope, "confirmation") && !confirmed) {
      return(.diagnose_row(
        check, scope, "skipped",
        .diagnose_text("The object carries no confirmation process.")
      ))
    }
    if (identical(check, "signposts") && !has_modifiedmk &&
      grepl("diagnose_drift", call, fixed = TRUE)) {
      return(.diagnose_row(
        check, scope, "skipped",
        .diagnose_text(
          "Package {.pkg modifiedmk} is not installed, so the drift test cannot
           be run."
        ),
        hint = .diagnose_text(
          "Install it with {.code install.packages(\"modifiedmk\")}."
        )
      ))
    }
    .diagnose_row(check, scope, "not_run", paste0("Run: ", call), hint = why)
  }

  drift_why <- .diagnose_text(
    "{.fn diagnose} runs no statistical test: a trend test needs a method, a
     maturity window and an alpha, and those are the caller's to choose."
  )
  batch_why <- .diagnose_text(
    "{.fn diagnose} runs no statistical test: batch detection needs a
     look-back, a null model and a multiplicity correction."
  )

  dplyr::bind_rows(
    signpost("signposts", "report", "diagnose_drift(x, axis = \"report\")", drift_why),
    signpost(
      "signposts", "confirmation",
      "diagnose_drift(x, axis = \"confirmation\")", drift_why
    ),
    signpost("signposts", "report_batches", "diagnose_batches(x, axis = \"report\")", batch_why),
    signpost(
      "signposts", "confirmation_batches",
      "diagnose_batches(x, axis = \"confirmation\")", batch_why
    )
  )
}

# The `validate_tbl_now()` presentation ---------------------------------------

#' Re-emit findings as the conditions `validate_tbl_now()` has always emitted
#'
#' @param findings A findings tibble.
#'
#' @return `NULL`, invisibly. Aborts when there is an `error` finding.
#'
#' @keywords internal
#' @noRd
.tbl_now_emit_findings <- function(findings) {
  status <- as.character(findings$status)

  # The pre-flight failures abort on their own, and with their own header: the
  # checks after them all assume the attributes exist.
  preflight <- findings$check %in% c("class", "attributes")
  if (any(preflight)) {
    cli::cli_abort(c(
      "Invalid {.code tbl_now} object:",
      .diagnose_escape(findings$message[preflight])
    ))
  }

  errors <- status == "error"
  if (any(errors)) {
    cli::cli_abort(c(
      "Invalid tbl_now object:", .diagnose_escape(findings$message[errors])
    ))
  }

  for (index in which(status == "warning")) {
    message <- .diagnose_escape(findings$message[index])
    hint <- findings$hint[index]
    cli::cli_warn(if (is.na(hint)) {
      message
    } else {
      c(message, "i" = .diagnose_escape(hint))
    })
  }

  alerts <- .diagnose_validation_alerts()
  informing <- status == "note" &
    paste(findings$check, findings$scope) %in%
      paste(alerts$check, alerts$scope)
  for (index in which(informing)) {
    cli::cli_alert_warning(.diagnose_escape(findings$message[index]))
  }

  invisible(NULL)
}

#' Protect a formatted message from a second round of `cli` interpolation
#'
#' The findings carry text that has already been through
#' [cli::format_inline()], and a stratum label or a column name is free to
#' contain a brace. Doubling them makes `cli` render one.
#'
#' @param text A character vector.
#'
#' @return The same vector, with braces doubled.
#'
#' @keywords internal
#' @noRd
.diagnose_escape <- function(text) {
  gsub("}", "}}", gsub("{", "{{", text, fixed = TRUE), fixed = TRUE)
}
