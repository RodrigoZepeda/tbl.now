# Guard against the CRAN "examples for unexported functions" rejection.
#
# `R CMD check --as-cran` does NOT test this: it is a CRAN-reviewer-side script.
# That is why tbl.now passed every local check while being bounced twice on it.
#
# The failure mode is an Rd page that carries \examples but for which *nothing on
# the page is exported*. That happens most easily with a roxygen `@name` /
# `@rdname` topic that is not a real object (e.g. `@name tbl_now_coerce` on a
# NULL block, with only unexported S3 methods hanging off it via `@rdname`).
#
# NOTE: CRAN's report pairs function names with file names in a misaligned way --
# it has named exported functions living in entirely different files
# (`update_now()`, `censor_delays_above()`). Trust the *file* list, not the names.
#
# Run with:  Rscript devel/check_unexported_examples.R

suppressMessages(devtools::load_all(quiet = TRUE))

ns <- asNamespace("tbl.now")
exports <- getNamespaceExports("tbl.now")

rd_files <- sort(list.files("man", pattern = "\\.Rd$", full.names = TRUE))
rows <- list()

for (f in rd_files) {
  rd <- tools::parse_Rd(f)
  tags <- vapply(rd, function(x) attr(x, "Rd_tag"), character(1))

  nm <- trimws(paste(unlist(rd[tags == "\\name"][[1]]), collapse = ""))
  aliases <- trimws(vapply(
    rd[tags == "\\alias"],
    function(x) paste(unlist(x), collapse = ""),
    character(1)
  ))
  doc_type <- if (any(tags == "\\docType")) {
    trimws(paste(unlist(rd[tags == "\\docType"][[1]]), collapse = ""))
  } else {
    ""
  }

  rows[[length(rows) + 1]] <- data.frame(
    file           = basename(f),
    name           = nm,
    is_data        = doc_type == "data",
    has_examples   = any(tags == "\\examples"),
    n_exported     = length(intersect(aliases, exports)),
    name_is_object = exists(nm, envir = ns, inherits = FALSE),
    stringsAsFactors = FALSE
  )
}
df <- do.call(rbind, rows)

# Datasets are legitimately "unexported" (they ship via LazyData), so exclude them.
risk <- df[df$has_examples & !df$is_data & df$n_exported == 0, ]

# Highest severity: nothing exported AND the topic name is not even a real object.
fatal <- risk[!risk$name_is_object, ]

cat("Rd pages scanned:", nrow(df), "\n\n")

cat("== FATAL: examples, nothing exported, and \\name is not a real object ==\n")
if (!nrow(fatal)) {
  cat("  (none)\n")
} else {
  print(fatal[, c("file", "name")], row.names = FALSE)
}

cat("\n== REVIEW: examples + no exported alias, but \\name IS a real object ==\n")
cat("   (documented S3 methods -- accepted by CRAN in the 2026-08 reviews)\n")
ok <- risk[risk$name_is_object, ]
if (!nrow(ok)) cat("  (none)\n") else print(ok[, c("file", "name")], row.names = FALSE)

if (nrow(fatal)) {
  stop("Found ", nrow(fatal), " Rd page(s) with examples but nothing exported.")
}
invisible(NULL)
