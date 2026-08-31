# ensemble_precompute ----------------------------------------------------------
#
# Builds `vignettes/articles/ensemble-comparison.rds`, the file
# `vignettes/articles/ensemble-nowcasting.Rmd` reads its results back from.
#
# Run with:  Rscript data-raw/ensemble_precompute.R
# Output:    vignettes/articles/ensemble-comparison.rds
#
# ------------------------------------------------------------------------------
# IT PURLS THE ARTICLE AND RUNS THE ARTICLE'S OWN CHUNKS.
#
# This replaces `data-raw/ensemble_comparison.R`, which kept a second copy of
# every call next to a comment asking whoever edited one to remember the other.
# That does not work -- the article never runs what it prints, so the drift is
# invisible until a reader copies a line off the page. See the same conversion
# for `nowcasting-models.Rmd` in `data-raw/nowcast_models_precompute.R`, and
# DEVELOPMENT_SKILL section 7.
#
# Chunks that only DISPLAY cached results are marked `purl = FALSE` in the
# article, so they are not extracted: they are what consumes this file.
#
# WHAT CHANGED FROM THE OLD SCRIPT. It fitted three epidemics (dengue, mpox,
# covid) on the theory that "an ensemble buys insurance against picking the wrong
# model" is only testable across outbreaks that differ. True -- but the article
# never displayed mpox or covid anywhere. Two thirds of the run was cached and
# shown to nobody. Running the article's own code means fitting what the article
# actually uses, which is dengue. If the cross-epidemic claim is wanted, the
# article needs a section that shows it, and this script will then fit it.
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
})
devtools::load_all(".", quiet = TRUE)

ARTICLE <- "vignettes/articles/ensemble-nowcasting.Rmd"
OUT     <- "vignettes/articles/ensemble-comparison.rds"

set.seed(20260824)

# -- 1. purl -------------------------------------------------------------------

# `purl()` evaluates chunk OPTIONS in this environment, so it needs both names
# or every conditional chunk logs an "object not found" error while still being
# extracted -- burying a real failure in noise.
purl_env <- new.env(parent = globalenv())
purl_env$run_models <- TRUE
purl_env$has <- function(pkg) requireNamespace(pkg, quietly = TRUE)

script <- tempfile(fileext = ".R")
invisible(knitr::purl(ARTICLE, output = script, documentation = 0,
                      envir = purl_env, quiet = TRUE))
message("purled ", ARTICLE, " -> ", length(readLines(script)), " lines")

# -- 2. run it -----------------------------------------------------------------

old_flag <- Sys.getenv("TBL_NOW_RUN_MODELS", unset = NA)
Sys.setenv(TBL_NOW_RUN_MODELS = "true")
on.exit({
  if (is.na(old_flag)) Sys.unsetenv("TBL_NOW_RUN_MODELS")
  else Sys.setenv(TBL_NOW_RUN_MODELS = old_flag)
}, add = TRUE)

# THE GLOBAL ENVIRONMENT, deliberately. knitr evaluates chunks in `globalenv()`,
# and an article that registers its own backend by writing `nowcast_fit.mymodel()`
# at top level relies on that: S3 dispatch finds a method like that only in the
# global environment or the method registry, not in some private environment we
# happened to evaluate into. (`custom-nowcast-models.Rmd` is built entirely on
# this, and it is why that article needs no precompute at all.) Everything this
# script owns is prefixed `.pc_` to stay out of the article's way.
article <- globalenv()
started <- Sys.time()
message("running the article's chunks ...")

# NOT `source()`: it abandons every completed fit the moment a later line fails.
# Expression by expression means one run reports EVERY failure, and the fits that
# did work are still there to be saved.
failures <- list()
expressions <- parse(script)
for (i in seq_along(expressions)) {
  tryCatch(
    eval(expressions[[i]], envir = article),
    error = function(e) {
      label <- paste(utils::head(deparse(expressions[[i]]), 1), collapse = "")
      message("  ! FAILED: ", label, "\n    ", conditionMessage(e))
      failures[[length(failures) + 1L]] <<- list(
        code = deparse(expressions[[i]]), message = conditionMessage(e)
      )
    }
  )
}
message("done in ",
        round(as.numeric(difftime(Sys.time(), started, units = "mins")), 1),
        " min",
        if (length(failures)) paste0(" -- ", length(failures), " chunk(s) FAILED")
        else "")

get_object <- function(name) {
  if (!exists(name, envir = article, inherits = FALSE)) return(NULL)
  get(name, envir = article, inherits = FALSE)
}

# -- 3. the manifest -----------------------------------------------------------
#
# The article's objects, by name. This is the one thing that has to stay in step,
# and it is a NAME rather than a copy of the call: rename something in the
# article and this stops with a list of what is missing.

NEEDED <- c(
  "dengue", "snapshot", "now",            # the data
  "baseline", "dnc", "enw", "nobbs", "sur", "en2",   # one per engine card
  "members", "ensemble", "by_sex",        # the ensemble and the stratified fit
  "backtest", "fitted_weights"            # the learned weights
)

MISSING <- NEEDED[!vapply(NEEDED, exists, logical(1),
                          envir = article, inherits = FALSE)]
if (length(MISSING)) {
  message("\nthe article did not define: ", paste(MISSING, collapse = ", "),
          "\n  -- either the chunk that builds it failed above, or it was ",
          "renamed and NEEDED needs updating.")
}

# -- 4. what the article shows -------------------------------------------------

# Stan and JAGS fits are large and version-fragile, so the file stores what the
# article DISPLAYS, not the objects themselves.
captured_print <- function(object) {
  if (is.null(object)) return(NULL)
  # Capture BOTH streams: several print methods write to the message connection,
  # so capturing stdout alone returns nothing.
  tryCatch({
    from_message <- NULL
    from_stdout <- utils::capture.output(
      from_message <- utils::capture.output(print(object), type = "message")
    )
    if (length(from_stdout)) c(from_stdout, from_message) else from_message
  }, error = function(e) paste("<print failed:", conditionMessage(e), ">"))
}

quietly <- function(expr) {
  tryCatch(suppressWarnings(suppressMessages(expr)),
           error = function(e) {
             message("  ! ", conditionMessage(e))
             NULL
           })
}

.pc_baseline  <- get_object("baseline")
.pc_ensemble  <- get_object("ensemble")
.pc_by_sex    <- get_object("by_sex")
.pc_dengue    <- get_object("dengue")
.pc_snapshot  <- get_object("snapshot")
.pc_backtest  <- get_object("backtest")

.pc_display <- list(
  # The running example.
  baseline_print       = captured_print(.pc_baseline),
  baseline_quantiles   = quietly(as_tibble(.pc_baseline)),
  baseline_draws       = quietly(as_tibble(.pc_baseline, type = "draws")),
  baseline_tidy        = quietly(tidy(.pc_baseline)),
  baseline_predictions = if (!is.null(.pc_baseline)) .pc_baseline@predictions else NULL,

  # One card per engine.
  card_dnc   = captured_print(get_object("dnc")),
  card_enw   = captured_print(get_object("enw")),
  card_nobbs = captured_print(get_object("nobbs")),
  card_sur   = captured_print(get_object("sur")),
  card_en2   = captured_print(get_object("en2")),

  # The stratified fit.
  stratified_quantiles = quietly(as_tibble(.pc_by_sex)),
  stratified_tidy      = quietly(tidy(.pc_by_sex)),

  # The ensemble.
  ensemble_print       = captured_print(.pc_ensemble),
  ensemble_tidy        = quietly(tidy(.pc_ensemble)),
  ensemble_predictions = if (!is.null(.pc_ensemble)) .pc_ensemble@predictions else NULL,

  # Scoring, exactly the call the article shows.
  baseline_score = quietly(
    score_nowcast(.pc_baseline, truth = .pc_dengue) |>
      dplyr::filter(.data[[get_event_date(.pc_dengue)]] > get_now(.pc_snapshot) - 56)
  ),

  event_date = if (!is.null(.pc_snapshot)) get_event_date(.pc_snapshot) else NULL,
  now        = if (!is.null(.pc_snapshot)) get_now(.pc_snapshot) else NULL,
  truth      = quietly(tbl.now:::.eventual_counts(.pc_dengue))
)

# -- 5. the backtest and the weights it learns ---------------------------------

.pc_backtest_tidy <- quietly(tidy(.pc_backtest))
.pc_backtest_summary <- if (!is.null(.pc_backtest_tidy)) {
  .pc_backtest_tidy |>
    summarise(
      mean_wis    = round(mean(.data$wis, na.rm = TRUE), 1),
      coverage_90 = round(mean(.data$coverage_90, na.rm = TRUE), 2),
      .by = c("method", "now")
    ) |>
    arrange(.data$now, .data$mean_wis)
} else {
  NULL
}

.pc_weights <- get_object("fitted_weights")

# -- 6. save -------------------------------------------------------------------

.pc_cached <- list(
  display          = .pc_display,
  backtest_tidy    = .pc_backtest_tidy,
  backtest_summary = .pc_backtest_summary,
  weights          = .pc_weights,
  members          = if (!is.null(get_object("members"))) names(get_object("members")) else NULL,
  built            = Sys.Date(),
  source           = ARTICLE
)

# An hour of Stan and JAGS sampling ends here, so this write is the one step
# that must not throw away everything before it. It has already happened once:
# the whole run finished, `saveRDS()` hit `Operation not permitted` (macOS had
# revoked the app's access to the folder mid-run), and every fit was lost.
#
# So: try the real path, and on ANY failure fall back to a temp file and say
# loudly where it went, so the result can be moved into place by hand rather
# than refitted.
.pc_write <- function(object, path) {
  tryCatch(
    {
      saveRDS(object, path, compress = "xz")
      message("wrote ", path, " (",
              format(file.size(path) / 1024, digits = 4), " KB)")
      TRUE
    },
    error = function(e) {
      fallback <- file.path(tempdir(), basename(path))
      message("\n!! could not write ", path, ": ", conditionMessage(e))
      rescued <- tryCatch(
        {
          saveRDS(object, fallback, compress = "xz")
          TRUE
        },
        error = function(e2) {
          message("!! and the fallback failed too: ", conditionMessage(e2))
          FALSE
        }
      )
      if (rescued) {
        message("!! the run is SAVED at:\n     ", fallback,
                "\n   Move it to ", path, " rather than refitting -- ",
                "the fits above are in it.")
      }
      FALSE
    }
  )
}

.pc_saved <- .pc_write(.pc_cached, OUT)

# -- 7. report -----------------------------------------------------------------

.pc_empty <- names(.pc_display)[vapply(.pc_display, is.null, logical(1))]
if (length(.pc_empty)) {
  message("\nEMPTY display slots (the article will render blanks):\n  ",
          paste(.pc_empty, collapse = "\n  "))
}

# The file is saved BEFORE this on purpose: a partial rebuild that cost an hour
# of sampling is worth keeping on disk. The non-zero exit says it is not
# publishable.
if (length(failures) || length(MISSING) || length(.pc_empty) || !.pc_saved) {
  if (length(failures)) {
    message("\nchunks that failed:")
    for (f in failures) {
      message("  * ", paste(utils::head(f$code, 3), collapse = " "),
              "\n      ", f$message)
    }
  }
  stop("the rebuild is incomplete -- ",
       if (.pc_saved) paste0(OUT, " was written but should NOT be committed ")
       else "and it could not be written to its own path ",
       "until the problems above are fixed.", call. = FALSE)
}
message("\nrebuild complete. Members: ", paste(.pc_cached$members, collapse = ", "))
