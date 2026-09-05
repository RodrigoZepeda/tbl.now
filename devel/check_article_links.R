#!/usr/bin/env Rscript
#
# Every link to one of THIS package's own articles must resolve to an article
# that exists, with the same capitalisation.
#
# Nothing else checks this. `R CMD check` does not look at articles at all, and
# `pkgdown::check_pkgdown()` checks the REFERENCE index, not article URLs -- so
# a renamed or deleted article leaves dead links in `@seealso` blocks, in the
# other articles, and in `inst/fragments/learning-more.Rmd`, and the first
# person to find out is a reader who clicks one.
#
# Capitalisation matters and is the failure that hides best: GitHub Pages is
# case-sensitive while macOS is not, so `articles/Example.html` against
# `example.Rmd` works on the author's machine and 404s on the site.
#
# Run from the package root:
#
#   Rscript devel/check_article_links.R

article_slugs <- function() {
  files <- c(
    list.files("vignettes", pattern = "[.]Rmd$", full.names = TRUE),
    list.files(file.path("vignettes", "articles"), pattern = "[.]Rmd$",
               full.names = TRUE)
  )
  tools::file_path_sans_ext(basename(files))
}

# Where a link can hide. `NEWS.md` is deliberately excluded: its old entries
# describe what was true when they were written, and an article removed three
# releases ago is meant to still be named there.
source_files <- function() {
  c(
    list.files("R", pattern = "[.]R$", full.names = TRUE),
    list.files("vignettes", pattern = "[.]Rmd$", full.names = TRUE),
    list.files(file.path("vignettes", "articles"), pattern = "[.]Rmd$",
               full.names = TRUE),
    list.files(file.path("inst", "fragments"), pattern = "[.]Rmd$",
               full.names = TRUE),
    Filter(file.exists, c("README.Rmd", "README.md", "_pkgdown.yml", "SKILL.md"))
  )
}

slugs <- article_slugs()
broken <- list()

# Anchored on THIS package's site. A bare `articles/x.html` also matches
# pillar's, lifecycle's and almanac's pkgdown sites, all of which are linked
# from here and none of which we can validate -- an unanchored pattern reports
# four other packages' articles as broken and buries the one that is.
prose_patterns <- c(
  "rodrigozepeda[.]github[.]io/tbl[.]now/articles/[A-Za-z0-9._-]+[.]html",
  "vignette\\(\"[A-Za-z0-9._-]+\"\\)"
)

# `_pkgdown.yml` writes its paths relative, and every one of them is ours, so
# the anchor above would match nothing there.
config_pattern <- "articles/[A-Za-z0-9._-]+[.]html"

slug_of <- function(hit) {
  slug <- sub("[.]html$", "", sub("^.*articles/", "", hit))
  gsub("^vignette\\(\"|\"\\)$", "", slug)
}

for (path in source_files()) {
  lines <- readLines(path, warn = FALSE)
  is_config <- basename(path) == "_pkgdown.yml"
  patterns <- if (is_config) config_pattern else prose_patterns

  for (pattern in patterns) {
    hits <- regmatches(lines, gregexpr(pattern, lines))
    for (index in seq_along(hits)) {
      line_hits <- hits[[index]]
      # A `redirects:` entry is `["<gone>.html", "<lives here now>.html"]`, and
      # naming an article that no longer exists is the entire point of one.
      # Only the DESTINATION has to resolve, and it is the last match on the
      # line.
      if (is_config && length(line_hits) > 1) {
        line_hits <- line_hits[length(line_hits)]
      }
      for (hit in line_hits) {
        slug <- slug_of(hit)
        if (!slug %in% slugs) {
          broken <- c(broken, list(list(
            file = path, line = index, slug = slug
          )))
        }
      }
    }
  }
}

if (length(broken) == 0) {
  cat("OK: every link to a tbl.now article resolves (", length(slugs),
      " articles).\n", sep = "")
  quit(status = 0)
}

cat("Dead article links:\n")
for (item in broken) {
  cat(sprintf("  %s:%d  -> %s\n", item$file, item$line, item$slug))
}
cat("\nArticles that exist:\n  ", paste(sort(slugs), collapse = "\n  "), "\n",
    sep = "")
quit(status = 1)
