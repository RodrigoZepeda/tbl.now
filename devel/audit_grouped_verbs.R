# Audit: does any exported verb misbehave when its `tbl_now` is grouped?
#
# DEVELOPMENT_SKILL.md sec. 8 ("Every new function gets a grouped test") exists
# because this bug has now appeared six times: `censor_delays_above()`,
# `censor_validation_delays_above()`, `align_weeks()`, `complete_zeroes()` and
# `tbl_now_to_epidist()` and `aggregate_time_units()`. A grouped object is a DIFFERENT CLASS
# (`grouped_tbl_now`), `tbl_now()` refuses a grouped data frame, and the
# `add_*()` setters refuse a `grouped_tbl_now` -- so a function written and
# tested ungrouped can abort, drop the grouping, or quietly compute a different
# answer the first time a user pipes it after `group_by()`.
#
# Run from the package root:
#
#   Rscript devel/audit_grouped_verbs.R
#
# It reports three things, and only the first two are always bugs:
#
#   ABORTS            - errors when grouped, works ungrouped. Always a bug.
#   DIFFERENT ANSWER  - returns different data. Almost always a bug.
#   GROUPS LOST       - returns a `tbl_now` with no grouping. A bug UNLESS the
#                       verb aggregates and drops it deliberately; see issue #61
#                       for `to_count()` and the reported-cases getters.
#
# Known-benign entries as of 2026-09-01: `as_tibble` (a grouped object carries
# an extra `groups` attribute, which is the point) and the three verbs in #61.
#
# The fix is nearly always: capture `dplyr::group_vars(x)`, `ungroup()`, do the
# work, and finish with `.tbl_now_regroup(x, group_columns)`.

# Apply the new rule to every exported function whose first argument takes a
# tbl_now: does it abort when grouped, drop the grouping, or change the answer?
suppressMessages(devtools::load_all("."))
suppressMessages(library(dplyr))

df <- data.frame(
  onset    = as.Date("2024-01-07") + 7 * rep(0:9, each = 2),
  reported = as.Date("2024-01-14") + 7 * rep(0:9, each = 2),
  sex      = rep(c("F", "M"), 10),
  n        = rep(c(3L, 5L), 10)
)
counts <- tbl_now(df, event_date = onset, report_date = reported,
                  case_count = n, strata = sex,
                  data_type = "count-incidence", units = "weeks", verbose = FALSE)

exported <- getNamespaceExports("tbl.now")
candidates <- Filter(function(nm) {
  f <- get(nm, envir = asNamespace("tbl.now"))
  if (!is.function(f)) return(FALSE)
  a <- names(formals(f))
  length(a) > 0 && a[1] %in% c("x", ".data", "object", "data")
}, exported)

quiet <- function(e) suppressWarnings(suppressMessages(force(e)))
rows <- list()
for (nm in sort(candidates)) {
  f <- get(nm, envir = asNamespace("tbl.now"))
  ung <- tryCatch(quiet(f(counts)), error = function(e) e)
  if (inherits(ung, "error")) next            # needs more args; not comparable
  grp <- tryCatch(quiet(f(counts |> group_by(sex))), error = function(e) e)
  if (inherits(grp, "error")) {
    rows[[nm]] <- c(nm, "ABORTS", substr(conditionMessage(grp), 1, 55)); next
  }
  if (!is.data.frame(ung) || !is.data.frame(grp)) next   # plots, scalars
  lost <- is_tbl_now(ung) && is_tbl_now(grp) && length(group_vars(grp)) == 0
  same <- isTRUE(all.equal(as_tibble(ungroup(ung)), as_tibble(ungroup(grp))))
  if (lost || !same) {
    rows[[nm]] <- c(nm, if (lost) "GROUPS LOST" else "DIFFERENT ANSWER", "")
  }
}
if (length(rows) == 0) cat("no problems found\n") else
  for (r in rows) cat(sprintf("%-34s %-17s %s\n", r[1], r[2], r[3]))
