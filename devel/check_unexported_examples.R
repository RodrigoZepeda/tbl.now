# Guard against the CRAN "examples for unexported functions" rejection.
#
# `R CMD check --as-cran` does NOT test this: it is a CRAN-reviewer-side script.
# That is why tbl.now passed every local check while being bounced twice on it.
#
# WHAT CRAN IS OBJECTING TO
# An Rd page that carries \examples while *nothing on the page is exported*.
#
# READING CRAN'S REPORT
# Their message pairs function names with file names in a misaligned way: it has
# named exported functions that live in entirely different files (`update_now()`,
# `censor_delays_above()`), and once reported `as_tbl_now.tbl_now()` "in
# check_bool.Rd" when that page's example only called `check_bool()`. Trust the
# FILE list, never the function names.
#
# THE RULE (a page is safe only if BOTH hold)
#   1. \usage carries \method{generic}{class} markup, and
#   2. \name is a real object (a function defined in R/, or a registered method).
# Either alone is insufficient, verified against the two rejected submissions:
#   * tbl_now_coerce.Rd  had \method{} markup but \name was a phantom topic
#     (`@name tbl_now_coerce` on a NULL block) -> criterion 2 catches it.
#   * print.Rd           had a real-ish `print` binding (an S7 method
#     registration puts one in the namespace, so exists() is fooled -- this is
#     why we resolve names statically from R/ and NAMESPACE, not with exists())
#     but no \method{} markup -> criterion 1 catches it.
#
# Run with:  Rscript devel/check_unexported_examples.R

ns_lines <- readLines("NAMESPACE", warn = FALSE)
exports <- gsub('"', "", sub("^export\\((.*)\\)$", "\\1", grep("^export\\(", ns_lines, value = TRUE)))
s3 <- grep("^S3method\\(", ns_lines, value = TRUE)
s3_methods <- gsub('"', "", sub("^.*::", "", sub("^S3method\\(([^,]+),\\s*(.*)\\)$", "\\1.\\2", s3)))

# Every top-level function defined in R/ (resolved statically on purpose).
defined <- unlist(lapply(
  list.files("R", pattern = "\\.R$", full.names = TRUE),
  function(f) {
    L <- readLines(f, warn = FALSE)
    d <- grep("^[`a-zA-Z._][^ ]*\\s*(<-|=)\\s*function\\s*\\(", L, value = TRUE)
    gsub("`", "", sub("^\\s*(.*?)\\s*(<-|=)\\s*function.*$", "\\1", d))
  }
))
real_objects <- unique(c(defined, s3_methods, exports))

rows <- list()
for (f in sort(list.files("man", pattern = "\\.Rd$", full.names = TRUE))) {
  rd <- tools::parse_Rd(f)
  tags <- vapply(rd, function(x) attr(x, "Rd_tag"), character(1))

  nm <- trimws(paste(unlist(rd[tags == "\\name"][[1]]), collapse = ""))
  aliases <- trimws(vapply(
    rd[tags == "\\alias"], function(x) paste(unlist(x), collapse = ""), character(1)
  ))
  doc_type <- if (any(tags == "\\docType")) {
    trimws(paste(unlist(rd[tags == "\\docType"][[1]]), collapse = ""))
  } else {
    ""
  }
  # Raw text: parse_Rd() turns \method{}{} into an Rd tag, so the literal
  # markup is not recoverable from the parsed tree.
  raw <- paste(readLines(f, warn = FALSE), collapse = "\n")

  rows[[length(rows) + 1]] <- data.frame(
    file         = basename(f),
    name         = nm,
    is_data      = doc_type == "data",
    has_examples = any(tags == "\\examples"),
    n_exported   = length(intersect(aliases, exports)),
    s3_markup    = grepl("\\method{", raw, fixed = TRUE),
    name_is_real = nm %in% real_objects,
    stringsAsFactors = FALSE
  )
}
df <- do.call(rbind, rows)

# Datasets are legitimately "unexported" (they ship via LazyData), so exclude them.
risk <- df[df$has_examples & !df$is_data & df$n_exported == 0, ]
safe <- risk$s3_markup & risk$name_is_real
fatal <- risk[!safe, ]

cat("Rd pages scanned:", nrow(df), "\n\n")
cat("== FATAL: has \\examples, nothing exported, and not a real documented S3 method ==\n")
if (!nrow(fatal)) {
  cat("  (none)\n")
} else {
  print(fatal[, c("file", "name", "s3_markup", "name_is_real")], row.names = FALSE)
}

cat("\n== OK: no exported alias, but a genuine documented S3 method page ==\n")
if (!any(safe)) cat("  (none)\n") else print(risk[safe, c("file", "name")], row.names = FALSE)

if (nrow(fatal)) {
  stop("Found ", nrow(fatal), " Rd page(s) with examples but nothing exported.")
}
cat("\nOK - no Rd page has examples while nothing on it is exported.\n")
invisible(NULL)
