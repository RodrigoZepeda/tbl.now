# devel/measure_tests.R -- measure per-test timings for the CRAN and NOT_CRAN paths.
#
#   env -u NOT_CRAN Rscript devel/measure_tests.R cran   # the path CRAN runs
#   NOT_CRAN=true   Rscript devel/measure_tests.R full   # the path devtools::check() runs
#
# Writes devel/timings_<tag>.rds with one row per test (file, test, real, skipped).
#
# NOTE: testthat::test_local() calls local_assume_not_on_cran(), which sets
# NOT_CRAN="true" when the variable is empty -- it can NEVER measure the CRAN
# path. test_check() does not, which is why this script uses it.
args <- commandArgs(trailingOnly = TRUE)
tag <- if (length(args)) args[[1]] else "cran"

library(testthat)
library(tbl.now)

if (identical(tag, "cran")) {
  stopifnot("NOT_CRAN is set -- this is not the CRAN path" = testthat:::on_cran())
} else {
  stopifnot("NOT_CRAN is not set -- this is not the developer path" = !testthat:::on_cran())
}

root <- normalizePath(".")
setwd(file.path(root, "tests"))
t0 <- proc.time()
res <- test_check("tbl.now", reporter = ListReporter$new(), stop_on_failure = FALSE)
wall <- proc.time() - t0
df <- as.data.frame(res)
saveRDS(df, file.path(root, "devel", paste0("timings_", tag, ".rds")))

cat("\n=====", tag, "=====\n")
cat("tests   :", nrow(df), "\n")
cat("skipped :", sum(df$skipped), "\n")
cat("failed  :", sum(df$failed), "\n")
cat("error   :", sum(df$error), "\n")
cat("test s  :", round(sum(df$real), 1), "\n")
cat("wall s  :", round(wall[["elapsed"]], 1), "\n\n")
by_file <- aggregate(real ~ file, df, sum)
by_file <- by_file[order(-by_file$real), ]
print(utils::head(by_file, 20), row.names = FALSE)
cat("\n-- slowest tests --\n")
slow <- df[order(-df$real), c("file", "test", "real")]
print(utils::head(slow, 25), row.names = FALSE)
