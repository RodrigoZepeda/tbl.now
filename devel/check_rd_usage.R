#!/usr/bin/env Rscript
#
# Every function/method Rd file must carry a `\usage{}` section.
#
# CRAN's r-devel Debian incoming check NOTEs "Rd files without \usage"; the
# released `R CMD check` on win-builder and mac-builder does NOT, so an Rd that
# roxygen emitted from `#' @usage NULL` sails through every pre-submission check
# the author usually runs and only fails once CRAN sees it.
#
# `@usage NULL` is the usual cause. It is legitimate for a re-export stub or a
# data doc, and only for those. Anything else needs a real `\usage` -- for an
# S3/S7 method registered by hand, write it explicitly with `\method{}{}`.
#
# Run from the package root:
#
#   Rscript devel/check_rd_usage.R

rd_files <- list.files("man", pattern = "[.]Rd$", full.names = TRUE)

offending <- Filter(function(f) {
  rd <- paste(readLines(f, warn = FALSE), collapse = "\n")
  has_usage <- grepl("\\usage{", rd, fixed = TRUE)
  # \docType{data}, \docType{package} and \docType{import} (reexports) are
  # allowed to have no \usage.
  exempt <- grepl("\\docType{data}", rd, fixed = TRUE) ||
    grepl("\\docType{package}", rd, fixed = TRUE) ||
    grepl("\\docType{import}", rd, fixed = TRUE)
  !has_usage && !exempt
}, rd_files)

if (length(offending)) {
  cat("Rd files without \\usage (CRAN r-devel NOTEs these):\n")
  cat(paste0("  ", basename(offending)), sep = "\n")
  quit(status = 1)
}

cat("OK: every function/method Rd file has a \\usage section.\n")
