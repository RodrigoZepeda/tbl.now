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
#'   \item{`skipped`}{Could not be assessed -- no validation process, the
#'     wrong data type, or an optional package that is not installed.}
#' }
#'
#' @return A tibble with the columns described above, sorted worst first.
#'
#' @seealso
#' [nowcast_diagnose_components] for the individual blocks;
#' [summary()][tbl_now_summary] for the descriptive counterpart -- what is in the
#' data rather than what is wrong with it;
#' [validate_tbl_now()] for the same findings raised as errors and warnings;
#' [diagnostic_plot()] for the picture version. The
#' [*Diagnosing a tbl_now* article](https://rodrigozepeda.github.io/tbl.now/articles/diagnosing-a-tbl-now.html)
#' goes through the findings one at a time.
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
#' # One block on its own
#' diagnose(ndata, checks = "units")
#'
#' ## `diagnose()` never stops your pipeline -- it hands back a table for you to
#' ## read. Use validate_tbl_now() when you want a broken object to be an error.
#' nrow(diagnose(ndata))
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
#' * `diagnose_ordering()` -- the `event <= report <= validation` timeline.
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
#'   validations still pending.
#' * `diagnose_signposts()` -- the questions [diagnose()] deliberately does not
#'   answer, and the call that answers each one.
#'
#' @inheritParams diagnose
#'
#' @return A tibble in the schema documented in [diagnose()].
#'
#' @seealso
#' [diagnose()], which stacks all of these and sorts them worst-first;
#' [validate_tbl_now()] for the same findings raised as errors and warnings;
#' [nowcast_summary_components] for what *is* in the data rather than what is
#' wrong with it; [diagnose_drift()], [diagnose_changepoint()] and
#' [diagnose_batches()] for the statistical tests `diagnose_signposts()` points
#' you at. The
#' [*Diagnosing a tbl_now* article](https://rodrigozepeda.github.io/tbl.now/articles/diagnosing-a-tbl-now.html)
#' explains how to read each finding.
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
#' # Is the object described correctly, and do the dates make sense?
#' diagnose_declarations(ndata)
#' diagnose_ordering(ndata)
#' diagnose_units(ndata)
#' diagnose_now(ndata)
#'
#' # Is anything missing, repeated, negative, or cut off at the recent edge?
#' diagnose_missing(ndata)
#' diagnose_duplicates(ndata)
#' diagnose_negatives(ndata)
#' diagnose_truncation(ndata)
#'
#' # Are the strata usable, and which statistical tests does the data call for?
#' diagnose_strata(ndata)
#' diagnose_signposts(ndata)
#'
#' ## Each returns the same schema, so they stack the way diagnose() stacks them.
#' dplyr::bind_rows(
#'   diagnose_units(ndata),
#'   diagnose_now(ndata)
#' )
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
  if (length(preflight) > 0) {
    return(.diagnose_finalise(preflight, floor))
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
  fatal <- Filter(
    function(finding) identical(finding$status, "error"), declarations
  )
  if (length(fatal) > 0) {
    return(.diagnose_finalise(fatal, floor))
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
#' @param message One sentence, usually still a deferred `.diagnose_text()`.
#' @param hint What to do about it, or `NA`.
#' @param rows Offending row indices.
#'
#' @return A `diagnose_finding`: a plain list, NOT a one-row tibble. A tibble
#'   costs about 2 ms to build and a finding is worth about 2 microseconds as a
#'   list, and a clean object produces ten of them on every `dplyr` verb.
#'   `.diagnose_finalise()` assembles the survivors into the one tibble anybody
#'   sees. `status` stays a character here for the same reason: the factor is
#'   built once, there, over the whole column.
#'
#' @keywords internal
#' @noRd
.diagnose_row <- function(check, scope, status, message, stratum = "all",
                          n_affected = NA_real_, n_total = NA_real_,
                          hint = NA_character_, rows = integer(0)) {
  n_affected <- as.numeric(n_affected)
  n_total <- as.numeric(n_total)
  structure(
    list(
      check = check, scope = scope, stratum = stratum, status = status,
      n_affected = n_affected,
      n_total = n_total,
      prop = if (is.na(n_total) || n_total == 0) {
        NA_real_
      } else {
        n_affected / n_total
      },
      message = message, hint = hint, rows = as.integer(rows)
    ),
    class = "diagnose_finding"
  )
}

#' Flatten whatever a block returned into a flat list of findings
#'
#' Blocks build their findings in nested lists, and some hand back a single one.
#' Anything that is not a finding or a list of them is a bug in the block, so it
#' aborts rather than being quietly dropped.
#'
#' @param x A finding, or an arbitrarily nested list of them.
#'
#' @return A flat list of `diagnose_finding` objects.
#'
#' @keywords internal
#' @noRd
.diagnose_block <- function(x) {
  if (inherits(x, "diagnose_finding")) {
    return(list(x))
  }
  if (is.null(x)) {
    return(list())
  }
  if (!is.list(x) || is.data.frame(x)) {
    cli::cli_abort(
      "Internal error: a findings block held a {.cls {class(x)[1]}}.",
      .internal = TRUE
    )
  }
  flattened <- lapply(x, .diagnose_block)
  unlist(flattened, recursive = FALSE, use.names = FALSE) %||% list()
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

#' Hold one message, to be formatted only if anything will read it
#'
#' `cli::format_inline()` emits ANSI escapes when the console supports them,
#' which is right for a condition and wrong for a column of a tibble. It also
#' keeps the source indentation of a template written across several lines,
#' which `cli_warn()` would have re-wrapped. The findings are data first: strip
#' the styling and the line breaks once, in `.diagnose_chr()`, so both
#' presentations read the same string.
#'
#' **This returns a template, not a string.** Formatting is what a finding
#' costs: `{.val {x}}` runs to about 3.5 ms and a hint carrying a vector of row
#' numbers to about 15 ms, and a block builds one of each whether or not the
#' check found anything. Most of them are then thrown away -- a clean object
#' validated at `floor = "note"` formats eleven messages and reports one -- and
#' since `validate_tbl_now()` runs on every `dplyr` verb, that waste was the
#' single largest cost in the class. `.diagnose_finalise()` filters first and
#' formats afterwards, so only a finding somebody will actually read is paid
#' for.
#'
#' Combine two of these with `.diagnose_join()`, not `paste()`.
#'
#' @param ... Passed to [cli::format_inline()] when the text is finally needed.
#' @param .envir Evaluation environment for the inline expressions. It is
#'   CLONED rather than held, because deferring changes WHEN the template is
#'   read: most of these are built inside a `for` loop over columns, axes or
#'   strata, and one that read the live frame would report the loop variable's
#'   LAST value in every row. `rlang::env_clone()` copies the bindings without
#'   forcing any promise among them, and at 0.01 ms it is a rounding error
#'   against the 0.5-15 ms it saves.
#'
#' @return A `diagnose_text` object.
#'
#' @keywords internal
#' @noRd
.diagnose_text <- function(..., .envir = parent.frame()) {
  .new_diagnose_text(
    list(list(args = list(...), envir = rlang::env_clone(.envir)))
  )
}

#' @param chunks A list whose elements are either character scalars or
#'   `list(args =, envir =)` templates.
#' @noRd
.new_diagnose_text <- function(chunks) {
  structure(list(chunks = chunks), class = "diagnose_text")
}

#' Concatenate message fragments without formatting any of them
#'
#' The replacement for `paste()` over `.diagnose_text()` results: pasting would
#' force each one, which is the cost this is all here to avoid.
#'
#' @param ... `diagnose_text` objects, character scalars, or lists of either.
#'
#' @return A `diagnose_text` object whose text is the fragments joined by a
#'   single space.
#'
#' @keywords internal
#' @noRd
.diagnose_join <- function(...) {
  parts <- list(...)
  chunks <- list()
  for (part in parts) {
    if (inherits(part, "diagnose_text")) {
      chunks <- c(chunks, part$chunks)
    } else if (is.list(part)) {
      chunks <- c(chunks, do.call(.diagnose_join, part)$chunks)
    } else if (length(part) > 0) {
      chunks <- c(chunks, as.list(as.character(part)))
    }
  }
  .new_diagnose_text(chunks)
}

#' Format a deferred message into the one line the findings carry
#'
#' @param x A `diagnose_text` object, or anything already character.
#'
#' @return A character scalar.
#'
#' @keywords internal
#' @noRd
.diagnose_chr <- function(x) {
  if (!inherits(x, "diagnose_text")) {
    return(as.character(x))
  }
  if (length(x$chunks) == 0) {
    return("")
  }
  parts <- vapply(x$chunks, function(chunk) {
    if (is.character(chunk)) {
      return(chunk)
    }
    formatted <- cli::ansi_strip(
      do.call(cli::format_inline, c(chunk$args, list(.envir = chunk$envir)))
    )
    trimws(gsub("[[:space:]]+", " ", formatted))
  }, character(1), USE.NAMES = FALSE)
  paste(parts, collapse = " ")
}

#' Bind blocks into the canonical schema, worst first
#'
#' @param blocks A finding, or an arbitrarily nested list of them.
#' @param floor Least severe status to keep.
#'
#' @return A tibble with the columns of `.diagnose_schema()`, in that order.
#'
#' @keywords internal
#' @noRd
.diagnose_finalise <- function(blocks, floor = "skipped") {
  findings <- .diagnose_block(blocks)
  levels <- .diagnose_status_levels()

  # Drop first, format second. The findings below the floor are the ones nobody
  # will read, and formatting a message is what a finding costs -- see
  # `.diagnose_text()`.
  if (length(findings) > 0) {
    status <- vapply(findings, function(f) f$status, character(1),
      USE.NAMES = FALSE
    )
    findings <- findings[match(status, levels) <= match(floor, levels)]
  }

  if (length(findings) == 0) {
    return(dplyr::tibble(
      check = character(0), scope = character(0), stratum = character(0),
      status = factor(character(0), levels = levels, ordered = TRUE),
      n_affected = numeric(0), n_total = numeric(0), prop = numeric(0),
      message = character(0), hint = character(0), rows = list()
    ))
  }

  field_chr <- function(field) {
    vapply(findings, function(f) f[[field]], character(1), USE.NAMES = FALSE)
  }
  field_num <- function(field) {
    vapply(findings, function(f) f[[field]], numeric(1), USE.NAMES = FALSE)
  }

  check <- field_chr("check")
  scope <- field_chr("scope")
  stratum <- field_chr("stratum")
  status <- factor(field_chr("status"), levels = levels, ordered = TRUE)
  n_affected <- field_num("n_affected")
  n_total <- field_num("n_total")
  prop <- field_num("prop")

  # `method = "radix"` because that is the C-locale collation `dplyr::arrange()`
  # uses by default; the default `order()` would sort in the session locale and
  # a findings tibble would come back in a different order abroad.
  index <- order(as.integer(status), check, scope, stratum, method = "radix")
  findings <- findings[index]

  .as_diagnosis(dplyr::tibble(
    check = check[index], scope = scope[index], stratum = stratum[index],
    status = status[index],
    n_affected = n_affected[index],
    n_total = n_total[index],
    prop = prop[index],
    message = vapply(findings, function(f) .diagnose_chr(f$message),
      character(1),
      USE.NAMES = FALSE
    ),
    hint = vapply(findings, function(f) .diagnose_chr(f$hint), character(1),
      USE.NAMES = FALSE
    ),
    rows = lapply(findings, function(f) f$rows)
  ))
}

#' Tag a findings tibble so it prints as a report
#'
#' The class goes on in `.diagnose_finalise()`, which is the one exit both
#' `diagnose()` and every `diagnose_*()` component share -- so a component
#' prints the same way the whole report does, and stacking components with
#' [dplyr::bind_rows()] keeps the class rather than silently reverting to a
#' plain tibble.
#'
#' @param x A findings tibble.
#'
#' @return `x` with `"tbl_now_diagnosis"` prepended to its class.
#'
#' @keywords internal
#' @noRd
.as_diagnosis <- function(x) {
  if (inherits(x, "tbl_now_diagnosis")) {
    return(x)
  }
  class(x) <- c("tbl_now_diagnosis", class(x))
  x
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

  .diagnose_block(rows)
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
  is_censored_report <- get_is_censored_report(x)
  is_censored_validation <- get_is_censored_validation(x)
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
  if (!is.null(is_censored_report) &&
    (length(is_censored_report) != 1 || !is.character(is_censored_report))) {
    error("is_censored_report", .diagnose_text(
      "Attribute {.val is_censored_report} must be {.val NULL} or a character vector
       of length 1"
    ))
  }
  if (!is.null(is_censored_validation) &&
    (length(is_censored_validation) != 1 ||
      !is.character(is_censored_validation))) {
    error("is_censored_validation", .diagnose_text(
      "Attribute {.val is_censored_validation} must be {.val NULL} or a
       character vector of length 1"
    ))
  }
  # A validation delay only exists once there is a validation date to measure
  # it from; a flag without one names a bound on nothing.
  if (!is.null(is_censored_validation) && !has_validation(x)) {
    error("is_censored_validation", .diagnose_text(
      "Attribute {.val is_censored_validation} is set but the object carries no
       {.field validation_date}"
    ), hint = .diagnose_text(
      "Attach one with {.fn add_validation_date}, or drop the flag with
       {.fn remove_is_censored_validation}."
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
  if (named_column(is_censored_report) && !is_censored_report %in% colnames(x)) {
    error(is_censored_report, .diagnose_text(
      "Column {.val {is_censored_report}} (is_censored_report) not found in data"
    ))
  }
  if (named_column(is_censored_validation) &&
    !is_censored_validation %in% colnames(x)) {
    error(is_censored_validation, .diagnose_text(
      "Column {.val {is_censored_validation}} (is_censored_validation) not
       found in data"
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
  if (!is.null(is_censored_report) && any(is_censored_report %in% covariates)) {
    error("is_censored_report", .diagnose_text(
      "Censored indicator {.val {is_censored_report}} cannot be also a covariate"
    ))
  }
  if (!is.null(is_censored_report) && any(is_censored_report %in% strata)) {
    error("is_censored_report", .diagnose_text(
      "Censored indicator {.val {is_censored_report}} cannot be also strata"
    ))
  }
  if (!is.null(is_censored_validation) &&
    any(is_censored_validation %in% covariates)) {
    error("is_censored_validation", .diagnose_text(
      "Censored indicator {.val {is_censored_validation}} cannot be also a
       covariate"
    ))
  }
  if (!is.null(is_censored_validation) &&
    any(is_censored_validation %in% strata)) {
    error("is_censored_validation", .diagnose_text(
      "Censored indicator {.val {is_censored_validation}} cannot be also strata"
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
  if (named_column(is_censored_report) && is_censored_report %in% colnames(x) &&
    !is.logical(x[[is_censored_report]])) {
    error(is_censored_report, .diagnose_text(
      "Column {.val {is_censored_report}} must be logical (TRUE/FALSE)"
    ))
  }
  if (named_column(is_censored_validation) &&
    is_censored_validation %in% colnames(x) &&
    !is.logical(x[[is_censored_validation]])) {
    error(is_censored_validation, .diagnose_text(
      "Column {.val {is_censored_validation}} must be logical (TRUE/FALSE)"
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

  .diagnose_block(rows)
}

#' The `event <= report <= validation` timeline
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
  validation <- if (has_validation(x)) {
    x[[get_validation_date(x)]]
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

  if (is.null(validation)) {
    rows[[2]] <- .diagnose_row(
      "ordering", "report_to_validation", "skipped",
      .diagnose_text("The object carries no validation process.")
    )
    rows[[3]] <- .diagnose_row(
      "ordering", "event_to_validation", "skipped",
      .diagnose_text("The object carries no validation process.")
    )
    return(.diagnose_block(rows))
  }

  before_report <- which(
    !is.na(validation) & !is.na(report) & validation < report
  )
  shown <- utils::head(before_report, 5)
  rows[[2]] <- .diagnose_count_row(
    "ordering", "report_to_validation", length(before_report), nrow(x),
    "warning",
    .diagnose_text(
      "{length(before_report)} row{?s} {?is/are} validated BEFORE they were reported."
    ),
    clean = .diagnose_text("Every validation is on or after its report."),
    hint = .diagnose_text(
      "The timeline is {.code event_date <= report_date <=
       validation_date}; a negative validation delay is not a delay.
       {cli::qty(length(shown))}First affected row{?s}: {.val {shown}}."
    ),
    rows = before_report
  )

  # The transitive case. A row whose `report_date` is missing escapes the check
  # above entirely, so a validation before the event goes unnoticed there.
  before_all <- which(
    !is.na(validation) & !is.na(event) & validation < event
  )
  rows[[3]] <- .diagnose_count_row(
    "ordering", "event_to_validation", length(before_all), nrow(x), "note",
    .diagnose_text(
      "{length(before_all)} row{?s} {?is/are} validated BEFORE the event happened."
    ),
    clean = .diagnose_text("Every validation is on or after its event."),
    hint = .diagnose_text(
      "A row with a missing {.field report_date} is only caught here."
    ),
    rows = before_all
  )

  .diagnose_block(rows)
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
    return(.diagnose_block(rows))
  }

  # -- everything else, per stratum -------------------------------------------
  case_count <- get_case_count(x)
  censoring <- get_is_censored_report(x)
  validation <- if (has_validation(x)) get_validation_date(x) else NULL
  validation_type <- if (has_validation(x)) {
    get_validation_type(x)
  } else {
    NULL
  }

  columns <- c(case_count, validation, validation_type, censoring,
               get_covariates(x))
  columns <- intersect(unique(columns), colnames(x))

  for (column in columns) {
    missing <- is.na(x[[column]])
    # An `NA` COUNT is not a defect: in a reporting triangle it means the cell
    # has not been observed yet, which is different from an observed zero. The
    # same goes for a validation date that has not come back. Say so, rather
    # than inviting the user to "fix" correct data.
    neutral <- identical(column, case_count) || identical(column, validation)
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

  .diagnose_block(rows)
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
  # validation columns are here because a case and its own retraction share an
  # (event, report) pair and are still two different rows -- left out, every
  # confirmed/retracted pair came out as an "exact duplicate", and the advice to
  # call `distinct()` would have deleted the retraction.
  key_cols <- unique(c(
    get_report_date(x), get_event_date(x), get_covariates(x), get_strata(x),
    get_is_censored_report(x), get_temporal_effect_cols(x),
    .validation_group_cols(x)
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
    hint = .diagnose_join(cause, fix),
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
  validation_units <- if (has_validation(x)) {
    get_validation_units(x)
  } else {
    NULL
  }

  rows <- list()

  # -- 1. the declarations against each other ---------------------------------
  # `time_cols_to_numeric()` enforces the first two rules at construction, so
  # this row is mostly reachable through `change_event_date()` and friends. The
  # validation rule is not enforced anywhere: `.validation_num` is measured
  # in the CONFIRMATION units while `.event_num` and `.report_num` are in the
  # EVENT units, so `.validation_delay`, which subtracts one from the other,
  # only means anything when the two agree.
  order <- c("days", "weeks", "months", "years")
  declared <- c(event = event_units, report = report_units)
  if (!is.null(validation_units)) {
    declared <- c(declared, validation = validation_units)
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

  # A list, not a character vector: these are deferred templates, and `c()` on
  # one would drop its class.
  problems <- list()
  if (any(numeric_axes) && !all(numeric_axes)) {
    problems <- c(problems, list(.diagnose_text(
      "{.val numeric} is mixed with calendar units
       ({.val {declared[!numeric_axes]}}); an axis is either a calendar or a
       number line, never both."
    )))
  }
  if (!any(numeric_axes)) {
    if (match(report_units, order) < match(event_units, order)) {
      problems <- c(problems, list(.diagnose_text(
        "{.field report_units} ({.val {report_units}}) is finer than
         {.field event_units} ({.val {event_units}})."
      )))
    }
    if (!is.null(validation_units) &&
      !identical(validation_units, event_units)) {
      problems <- c(problems, list(.diagnose_text(
        "{.field validation_units} ({.val {validation_units}}) differs from
         {.field event_units} ({.val {event_units}}), so
         {.code .validation_delay} subtracts one scale from another."
      )))
    }
  }

  rows[[1]] <- .diagnose_count_row(
    "units", "declared", length(problems), length(declared), "note",
    .diagnose_join(problems),
    clean = .diagnose_text(
      "The declared units agree: {.val {declared}}."
    ),
    hint = .diagnose_text(
      "Set them with {.code event_units = } / {.code report_units = } /
       {.code validation_units = }, or move every axis to {.val numeric}."
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
  if (!is.null(validation_units)) {
    axes <- c(axes, list(list(
      name = "validation", column = get_validation_date(x),
      units = validation_units
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

  .diagnose_block(rows)
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
    return(.diagnose_block(rows))
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
  .diagnose_block(rows)
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

  # A validation is an OBSERVATION, so nothing can have been confirmed after
  # the as-of moment. This is the same rule `now` already obeys for reports;
  # breaking it means the object claims to know something it could not have.
  if (has_validation(x)) {
    validation <- x[[get_validation_date(x)]]
    latest <- suppressWarnings(max(validation, na.rm = TRUE))
    after <- which(!is.na(validation) & validation > now_value)
    rows[[length(rows) + 1L]] <- .diagnose_count_row(
      "now", "validation_date", length(after), nrow(x), "error",
      .diagnose_text(
        "The latest validation ({.val {as.character(latest)}}) is AFTER
         {.field now} ({.val {as.character(now_value)}}). Nothing can be
         confirmed after the as-of moment."
      ),
      clean = .diagnose_text("No validation is dated after {.field now}."),
      hint = .diagnose_text(
        "Move {.field now} forward with {.fn change_now}, or drop the rows."
      ),
      rows = after
    )

    allowed <- .validation_levels()
    type_col <- get_validation_type(x)
    if (!is.null(type_col) && type_col %in% colnames(x)) {
      values <- as.character(x[[type_col]])
      unknown <- setdiff(stats::na.omit(unique(values)), allowed)
      offending <- which(values %in% unknown)
      rows[[length(rows) + 1L]] <- .diagnose_count_row(
        "now", "validation_type", length(offending), nrow(x), "error",
        .diagnose_text(
          "{.field validation_type} has unrecognised value{?s}:
           {.val {unknown}}."
        ),
        clean = .diagnose_text(
          "Every {.field validation_type} is one of {.val {allowed}}."
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
    return(.diagnose_block(rows))
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
    return(.diagnose_block(rows))
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
    return(.diagnose_block(rows))
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

  .diagnose_block(rows)
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

  .diagnose_block(rows)
}

#' The smallest and the sparsest stratum, and the validations still pending
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

  # -- the validation backlog ----------------------------------------------
  if (!has_validation(x)) {
    rows[[length(rows) + 1L]] <- .diagnose_row(
      "strata", "pending", "skipped",
      .diagnose_text("The object carries no validation process.")
    )
    return(.diagnose_block(rows))
  }

  summary <- .diagnose_summary(context)
  if (is.null(summary)) {
    rows[[length(rows) + 1L]] <- .diagnose_row(
      "strata", "pending", "skipped",
      .diagnose_text("The validations could not be counted.")
    )
    return(.diagnose_block(rows))
  }

  cases <- summary$cases
  pending <- cases$validation_type %in% "pending"
  turnaround <- cases$report_to_validation
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
      clean = .diagnose_text("No validation is still pending."),
      stratum = label,
      hint = .diagnose_text(
        "A pending case has no validation date, so it is invisible to
         anything counting arrivals on the validation axis."
      )
    )
  }

  .diagnose_block(rows)
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
  confirmed <- has_validation(x)
  has_modifiedmk <- requireNamespace("modifiedmk", quietly = TRUE)

  signpost <- function(check, scope, call, why) {
    if (identical(scope, "validation") && !confirmed) {
      return(.diagnose_row(
        check, scope, "skipped",
        .diagnose_text("The object carries no validation process.")
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

  .diagnose_block(list(
    signpost("signposts", "report", "diagnose_drift(x, axis = \"report\")", drift_why),
    signpost(
      "signposts", "validation",
      "diagnose_drift(x, axis = \"validation\")", drift_why
    ),
    signpost("signposts", "report_batches", "diagnose_batches(x, axis = \"report\")", batch_why),
    signpost(
      "signposts", "validation_batches",
      "diagnose_batches(x, axis = \"validation\")", batch_why
    )
  ))
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

# Printing ---------------------------------------------------------------------

#' What each status looks like on the console
#'
#' @return A named list, one entry per status, holding the bullet symbol, the
#'   colour to draw it in and the heading the block gets.
#'
#' @keywords internal
#' @noRd
.diagnose_status_style <- function() {
  list(
    error   = list(symbol = cli::symbol$cross,  colour = cli::col_red,     heading = "Errors"),
    warning = list(symbol = "!",                colour = cli::col_yellow,  heading = "Warnings"),
    note    = list(symbol = cli::symbol$info,   colour = cli::col_blue,    heading = "Notes"),
    ok      = list(symbol = cli::symbol$tick,   colour = cli::col_green,   heading = "Passed"),
    not_run = list(symbol = cli::symbol$arrow_right, colour = cli::col_cyan, heading = "Not run"),
    skipped = list(symbol = cli::symbol$line,   colour = cli::col_grey,    heading = "Skipped")
  )
}

#' Print a `tbl_now` diagnosis
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Prints the findings [diagnose()] returned as a report: the errors, warnings
#' and notes in full, each with its hint, and the checks that passed, that were
#' deliberately not run, and that could not be assessed as one line each.
#'
#' The object is an ordinary tibble underneath, so
#' `print(tibble::as_tibble(x))` gives the table and every `dplyr` verb still
#' works on it.
#'
#' @param x A findings tibble, from [diagnose()] or one of the
#'   [nowcast_diagnose_components].
#' @param ... Unused.
#' @param all Logical. Spell out the `ok` and `skipped` findings too, instead of
#'   counting them. Defaults to `FALSE`, and to `TRUE` when there is nothing
#'   else to report -- a block that found nothing wrong would otherwise print an
#'   empty report.
#'
#' @return `x`, invisibly.
#'
#' @seealso [diagnose()], [nowcast_diagnose_components]
#'
#' @examples
#' data(denguedat)
#' ndata <- tbl_now(denguedat,
#'   event_date = "onset_week", report_date = "report_week",
#'   strata = "gender", verbose = FALSE
#' )
#'
#' diagnose(ndata)
#'
#' # Every finding, including the ones that passed.
#' print(diagnose(ndata), all = TRUE)
#'
#' # Still a tibble.
#' print(tibble::as_tibble(diagnose(ndata)))
#'
#' @name print.tbl_now_diagnosis
#' @md
#' @exportS3Method base::print
print.tbl_now_diagnosis <- function(x, ..., all = FALSE) {
  # The class survives `select()` as well as `filter()`, and a report cannot be
  # written from columns that are no longer there. Fall back to the tibble
  # rather than erroring on a pipeline that is perfectly reasonable --
  # `diagnose(x) |> select(scope, status, message)` is how the articles read it.
  required <- c("check", "scope", "stratum", "status", "message", "hint")
  if (length(setdiff(required, names(x))) > 0) {
    return(NextMethod())
  }

  # stdout (`cat_*`), not messages (`cli_*`): print output must survive
  # `message = FALSE`, `sink()` and `capture.output()`.
  cli::cat_rule(left = cli::format_inline("Diagnosis of a {.cls tbl_now}"))

  if (nrow(x) == 0) {
    cli::cat_line(cli::format_inline("{.emph No findings.}"))
    return(invisible(x))
  }

  status <- as.character(x$status)
  styles <- .diagnose_status_style()
  counts <- vapply(names(styles), function(s) sum(status == s), integer(1))

  cli::cat_line(.diagnose_tally(counts))

  # `ok` and `skipped` are the bulk of a healthy report and say the same thing
  # every time; the point of the printed form is the handful of rows that do
  # not. When they are ALL there is, spelling them out is the only way the
  # report says anything at all.
  actionable <- c("error", "warning", "note", "not_run")
  spell_everything <- isTRUE(all) || sum(counts[actionable]) == 0
  spelled <- if (spell_everything) names(styles) else actionable

  # Spelled-out blocks first, in severity order, then the one-line tallies for
  # whatever was collapsed -- so the reader never has to step over a summary
  # line to reach the next finding.
  seen_hints <- character(0)
  for (level in intersect(names(styles), spelled)) {
    rows <- x[status == level, , drop = FALSE]
    if (nrow(rows) == 0) {
      next
    }
    style <- styles[[level]]

    cli::cat_line()
    cli::cat_line(cli::style_bold(paste0(style$heading, " (", nrow(rows), ")")))
    for (index in seq_len(nrow(rows))) {
      # The messages have already been through `cli::format_inline()` in
      # `.diagnose_finalise()`, so they go to `cat()` as they are: inlining a
      # second time would interpret a brace in a column name or a stratum label.
      cli::cat_line(paste0(
        style$colour(style$symbol), " ",
        cli::style_bold(.diagnose_where(rows[index, ])), " ",
        rows$message[index]
      ))
      hint <- rows$hint[index]
      # One hint per distinct text: the per-stratum findings repeat a single
      # sentence once per stratum, which is right in the table and noise here.
      if (!is.na(hint) && !hint %in% seen_hints) {
        seen_hints <- c(seen_hints, hint)
        cli::cat_line(paste0(
          "  ", cli::col_grey(cli::symbol$arrow_right), " ",
          cli::col_grey(hint)
        ))
      }
    }
  }

  collapsed <- setdiff(names(styles), spelled)
  if (any(status %in% collapsed)) {
    cli::cat_line()
  }
  for (level in collapsed) {
    rows <- x[status == level, , drop = FALSE]
    if (nrow(rows) == 0) {
      next
    }
    style <- styles[[level]]
    where <- unique(paste0(rows$check, "/", rows$scope))
    cli::cat_line(paste0(
      style$colour(style$symbol), " ",
      cli::format_inline(
        "{nrow(rows)} {tolower(style$heading)}: {.field {where}}"
      )
    ))
  }

  cli::cat_line()
  cli::cat_line(cli::col_grey(cli::format_inline(paste0(
    "{cli::symbol$info} {nrow(x)} finding{?s}. ",
    "Use {.code dplyr::filter()} or {.code tibble::as_tibble()} for the table."
  ))))

  invisible(x)
}

#' The one-line tally under the header
#'
#' @param counts A named integer vector, one entry per status.
#'
#' @return A single string.
#'
#' @keywords internal
#' @noRd
.diagnose_tally <- function(counts) {
  # `ok`, `not_run` and `skipped` name what HAPPENED to a check rather than what
  # was found, so they read the same at any count; the three that name a finding
  # take a plural.
  singular <- c(
    error = "error", warning = "warning", note = "note",
    ok = "passed", not_run = "not run", skipped = "skipped"
  )
  plural <- c(
    error = "errors", warning = "warnings", note = "notes",
    ok = "passed", not_run = "not run", skipped = "skipped"
  )
  styles <- .diagnose_status_style()
  present <- counts[counts > 0]
  if (length(present) == 0) {
    return(cli::col_grey("Nothing to report."))
  }
  parts <- vapply(names(present), function(level) {
    count <- present[[level]]
    wording <- if (count == 1L) singular[[level]] else plural[[level]]
    styles[[level]]$colour(paste0(count, " ", wording))
  }, character(1), USE.NAMES = FALSE)
  paste0(paste(parts, collapse = cli::col_grey(", ")), cli::col_grey("."))
}

#' Where a finding is, as `check/scope [stratum]`
#'
#' @param row One row of a findings tibble.
#'
#' @return A single string.
#'
#' @keywords internal
#' @noRd
.diagnose_where <- function(row) {
  where <- paste0(row$check, "/", row$scope)
  if (identical(row$stratum, "all")) {
    return(paste0(where, ":"))
  }
  paste0(where, " [", row$stratum, "]:")
}

