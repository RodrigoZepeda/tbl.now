# devel/audit_test_isolation.R -- find test files that leak into a namespace.
#
# `testthat::local_mocked_s3_method()` restores what it found in the S3 methods
# table on exit. When the owning package is only LOADED and not ATTACHED, it
# finds nothing, restores nothing, and the real method is gone for the rest of
# the session -- see DEVELOPMENT_SKILL.md section 9. The file that does it still
# passes; the victim is some later file, which is exactly the failure a per-file
# diff catches and a green test run does not.
#
# Runs every test file in ONE process, as testthat does, snapshotting the S3
# tables and namespace bindings of every modelling package between files.
#
#   NOT_CRAN=true Rscript devel/audit_test_isolation.R
#
# VALIDATE IT BEFORE YOU TRUST A CLEAN RUN. An audit that cannot see the bug it
# is looking for reports "no leaks" just as cheerfully as one that can, and this
# one nearly did: three separate sensitivity checks came back empty because the
# thing being checked was wrong, not because the suite was clean. Restore the
# pre-fix file and confirm the audit still catches it:
#
#   git show 435efad:tests/testthat/test-converter-epinow2.R \
#     > tests/testthat/test-converter-epinow2.R
#   # -> REMOVED  EpiNow2 :: S3 table  get_predictions.estimate_infections
#
# Result on 2026-08-28, validated that way: **zero REMOVED across 58 files**.
# Two CHANGED entries, both benign and both checked rather than assumed:
#   * base :: S3 table  print.rlib_error_3_0 and friends -- rlang re-registering
#     its own condition methods. Body identical.
#   * tbl.now :: S3 table  nowcast_fit.scoretoy and friends -- `register_scoretoy()`
#     re-run as each file re-sources `helper-nowcast.R`. `identical()` and even
#     `body()` say FALSE here, but only because of `srcref` attributes; formals
#     match and `deparse()` matches, so it is the same function.
#
# That srcref trap is worth remembering: compare `deparse()`, not `body()`.
root <- normalizePath(".")
suppressMessages(pkgload::load_all(root, quiet = TRUE))
suppressMessages(library(testthat))

PKGS <- c(
  "tbl.now", "EpiNow2", "epinowcast", "baselinenowcast", "NobBS",
  "surveillance", "diseasenowcasting", "epidist", "scoringutils", "generics",
  "dplyr", "ggplot2", "utils", "base", "stats"
)
PKGS <- Filter(function(p) requireNamespace(p, quietly = TRUE), PKGS)
cat("watching:", paste(PKGS, collapse = ", "), "\n")

grab <- function(env) {
  if (!is.environment(env)) return(list())
  names <- ls(env, all.names = TRUE)
  stats::setNames(lapply(names, function(n) {
    tryCatch(get(n, envir = env, inherits = FALSE), error = function(e) NULL)
  }), names)
}

snapshot <- function() {
  out <- list()
  for (p in PKGS) {
    ns <- asNamespace(p)
    out[[paste0(p, " :: namespace")]] <- grab(ns)
    out[[paste0(p, " :: S3 table")]] <-
      grab(tryCatch(get(".__S3MethodsTable__.", envir = ns, inherits = FALSE),
                    error = function(e) NULL))
  }
  out
}

compare <- function(before, after) {
  findings <- character()
  for (scope in names(before)) {
    a <- before[[scope]]; b <- after[[scope]]
    gone <- setdiff(names(a), names(b))
    added <- setdiff(names(b), names(a))
    common <- intersect(names(a), names(b))
    changed <- common[!vapply(common, function(n) identical(a[[n]], b[[n]]),
                              logical(1))]
    if (length(gone)) findings <- c(findings, sprintf("  REMOVED  %-26s %s", scope, paste(gone, collapse = ", ")))
    if (length(changed)) findings <- c(findings, sprintf("  CHANGED  %-26s %s", scope, paste(changed, collapse = ", ")))
    if (length(added)) findings <- c(findings, sprintf("  added    %-26s %s", scope, paste(added, collapse = ", ")))
  }
  findings
}

setwd(file.path(root, "tests", "testthat"))
files <- sort(list.files(".", pattern = "^test-.*\\.R$"))
state <- snapshot()
leaks <- 0L

for (f in files) {
  invisible(capture.output(suppressMessages(suppressWarnings(
    tryCatch(test_file(f, reporter = SilentReporter$new()), error = function(e) NULL)
  ))))
  now <- snapshot()
  found <- compare(state, now)
  if (length(found)) {
    leaks <- leaks + 1L
    cat("\n### ", f, "\n", sep = "")
    cat(paste(found, collapse = "\n"), "\n")
  }
  state <- now
}
cat(sprintf("\n==== %d of %d files changed a watched namespace ====\n", leaks, length(files)))
