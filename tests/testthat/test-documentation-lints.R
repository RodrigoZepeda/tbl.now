test_that("articles and vignettes end with learning-more", {
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
  rmds <- list.files(
    file.path(root, "vignettes"),
    pattern = "[.]Rmd$",
    recursive = TRUE,
    full.names = TRUE
  )

  last_chunk <- vapply(rmds, function(path) {
    lines <- readLines(path, warn = FALSE)
    chunk_headers <- grep("^```\\{r ", lines, value = TRUE)
    if (length(chunk_headers) == 0L) return(NA_character_)
    chunk_headers[[length(chunk_headers)]]
  }, character(1))

  expect_named(last_chunk, rmds)
  expect_true(
    all(grepl("^```\\{r learning-more\\b", last_chunk)),
    info = paste(names(last_chunk)[!grepl("^```\\{r learning-more\\b", last_chunk)],
                 collapse = "\n")
  )
})

test_that("public docs do not reference removed as_scoringutils() helper", {
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
  docs <- c(
    file.path(root, "README.Rmd"),
    file.path(root, "README.md"),
    file.path(root, "SKILL.md"),
    list.files(file.path(root, "vignettes"), pattern = "[.]Rmd$",
               recursive = TRUE, full.names = TRUE)
  )

  hits <- unlist(lapply(docs, function(path) {
    lines <- readLines(path, warn = FALSE)
    matched <- grep("(^|[^.[:alnum:]_])as_scoringutils\\s*\\(", lines, value = TRUE)
    if (length(matched) == 0L) return(character(0))
    paste0(path, ":", seq_along(lines)[
      grepl("(^|[^.[:alnum:]_])as_scoringutils\\s*\\(", lines)
    ], ": ", matched)
  }), use.names = FALSE)

  expect_equal(length(hits), 0L, info = paste(hits, collapse = "\n"))
})
