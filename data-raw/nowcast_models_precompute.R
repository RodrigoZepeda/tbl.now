# nowcast_models_precompute ----------------------------------------------------
#
# Builds `vignettes/articles/nowcast-comparison.rds`, the file the article
# `vignettes/articles/nowcasting-models.Rmd` reads its results back from.
#
# Run with:  Rscript data-raw/nowcast_models_precompute.R
# Output:    vignettes/articles/nowcast-comparison.rds
#
# ------------------------------------------------------------------------------
# WHY THIS SCRIPT DOES NOT CONTAIN ANY MODEL CODE
#
# It used to. `data-raw/nowcast_comparison.R` held its own copy of every fit,
# and the article held another, and a comment at the top asked whoever edited
# one to remember to edit the other. That does not work: the two drifted, and
# the drift is invisible, because the article never runs the code it shows -- it
# prints cached output next to it. A reader copying a call off the page got
# something that had not been run in months.
#
# So this script does not restate the fits. It PURLS THE ARTICLE and runs the
# article's own chunks:
#
#   1. `knitr::purl()` extracts every chunk into a plain R script, in order,
#      with `run_models = TRUE` so the heavy fits are live;
#   2. the script is sourced into one environment;
#   3. the objects the article displays are read back OUT of that environment,
#      by name, and saved.
#
# The code that produced every number in the article is therefore, literally,
# the code printed above that number.
#
# Chunks that only DISPLAY cached results (`show_fit()`, `show_tidy()`,
# `nowcast_panels()`, the figures) are marked `purl = FALSE` in the article, so
# they are not extracted -- they are what consumes this file, and running them
# here would need the file that does not exist yet.
#
# ------------------------------------------------------------------------------
# THE ONE THING THAT STILL HAS TO STAY IN STEP
#
# `DISPLAYED` and `PANELS` below name the article's objects. That is unavoidable
# -- something has to say which variable holds the epinowcast fit -- but it is a
# name, not a copy of the call, and `check_manifest()` stops with a list of what
# is missing rather than quietly saving a shorter file. Rename an object in the
# article and this script fails on the next run.
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
})
devtools::load_all(".", quiet = TRUE)

ARTICLE <- "vignettes/articles/nowcasting-models.Rmd"
OUT     <- "vignettes/articles/nowcast-comparison.rds"

N_HORIZON <- 30L   # days the article's figures show

# Every stochastic engine draws from R's RNG. Seeding once here pins the whole
# run, which is what makes a rebuild reproducible; it does NOT survive running
# a subset, which is why this script has no "just refit one engine" mode.
set.seed(20260824)

# -- 1. purl the article -------------------------------------------------------

# `purl()` evaluates chunk OPTIONS (`eval=run_models && has("NobBS")`) in this
# environment, so it needs both names -- otherwise every conditional chunk logs
# an "object not found" error while still being extracted, which buries a real
# failure in noise.
purl_env <- new.env(parent = globalenv())
purl_env$run_models <- TRUE
purl_env$has <- function(pkg) requireNamespace(pkg, quietly = TRUE)

script <- tempfile(fileext = ".R")
invisible(knitr::purl(ARTICLE, output = script, documentation = 0,
                      envir = purl_env, quiet = TRUE))
message("purled ", ARTICLE, " -> ", length(readLines(script)), " lines")

# -- 2. run it -----------------------------------------------------------------

# `TBL_NOW_RUN_MODELS` is what the article's setup chunk reads to decide whether
# to fit. Setting it here rather than overwriting `run_models` afterwards keeps
# the article's own line the single decision point.
old_flag <- Sys.getenv("TBL_NOW_RUN_MODELS", unset = NA)
Sys.setenv(TBL_NOW_RUN_MODELS = "true")
on.exit({
  if (is.na(old_flag)) Sys.unsetenv("TBL_NOW_RUN_MODELS")
  else Sys.setenv(TBL_NOW_RUN_MODELS = old_flag)
}, add = TRUE)

article <- new.env(parent = globalenv())
started <- Sys.time()
message("running the article's chunks (this takes tens of minutes) ...")

# NOT `source()`. Some of these fits take forty minutes, and `source()` abandons
# every one of them the moment a later line fails -- which is how the first run
# of this script threw away two epinowcast fits over a mistyped column name in
# the chunk after them. Evaluate expression by expression instead, record what
# broke, and carry on: a run then reports EVERY failure at once rather than one
# per forty minutes, and the fits that did work are still there to be saved.
# The article writes temporal-effect column names BY HAND, inside model
# formulas (`~ 1 + .event_season_365_sin`). Nothing checks those: a formula is a
# quoted call, so neither `codetools::findGlobals()` nor the parser sees the
# names, and the mistake only surfaces when the model is fitted -- which on the
# first run of this script was AFTER forty minutes of sampling in an earlier
# chunk. `seasons = 365` makes `.event_season_365_*`; the article said `_52_`.
#
# So check them up front. Every `.event_*` / `.report_*` token an expression
# mentions has to be a column of something already built, or the expression is
# skipped rather than fitted.
tbl_now_columns <- function(env) {
  # NOT a chain of early returns. `enw_preprocess_data()` returns a `data.table`
  # -- which IS a data frame -- whose covariates live one level down, inside the
  # `metareference` list-column. Returning `names()` on the outer table and
  # stopping there hides exactly the columns this check exists to find, and the
  # preflight then skips a perfectly good fit. Collect from every level.
  unique(unlist(lapply(ls(env), function(nm) {
    value <- get(nm, envir = env, inherits = FALSE)
    found <- character(0)
    if (is.data.frame(value) || is.list(value)) {
      found <- c(found, names(value))
    }
    # A list OF data frames -- `tbl_now_to_surveillance(format =
    # "linelist_list")`, a `triangle_list` -- names its elements after the
    # STRATA, so `names()` above returns stratum labels rather than columns.
    # Look one level in, or the effect columns those pieces carry are invisible
    # to the preflight and a perfectly good fit is skipped.
    if (!is.data.frame(value) && is.list(value)) {
      found <- c(found, unlist(lapply(value, function(piece) {
        if (is.data.frame(piece)) names(piece) else NULL
      })))
    }
    for (nested in c("metareference", "metareport", "metadelay")) {
      if (is.list(value) && nested %in% names(value)) {
        found <- c(found, unlist(lapply(value[[nested]], names)))
      }
    }
    found
  })))
}

effect_tokens <- function(expression) {
  tokens <- all.names(expression, unique = TRUE)
  grep("^\\.(event|report)_", tokens, value = TRUE)
}

failures <- list()
expressions <- parse(script)
for (i in seq_along(expressions)) {
  wanted <- effect_tokens(expressions[[i]])
  unknown <- setdiff(wanted, tbl_now_columns(article))
  if (length(unknown)) {
    label <- paste(utils::head(deparse(expressions[[i]]), 1), collapse = "")
    message("  ! SKIPPED: ", label,
            "\n    no such column: ", paste(unknown, collapse = ", "))
    failures[[length(failures) + 1L]] <- list(
      code = deparse(expressions[[i]]),
      message = paste0(
        "names no existing column: ", paste(unknown, collapse = ", "),
        ". Temporal-effect columns carry the PERIOD in their name -- check ",
        "get_temporal_effect_cols() on the object being fitted."
      )
    )
    next
  }
  tryCatch(
    eval(expressions[[i]], envir = article),
    error = function(e) {
      label <- paste(utils::head(deparse(expressions[[i]]), 1), collapse = "")
      message("  ! FAILED: ", label,
              if (nchar(label) >= 60) " ..." else "",
              "\n    ", conditionMessage(e))
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

# `display` key -> the article's object. Keys are what `show_fit()` /
# `show_tidy()` ask for in the article; values are the variables the article
# assigns.
DISPLAYED <- c(
  diseasenowcasting          = "dnc_fit",
  diseasenowcasting_seasonal = "dnc_seasonal",
  baselinenowcast            = "nowcast_samples",
  epinowcast                 = "enw_fit",
  NobBS                      = "nobbs_fit",
  NobBS_quantiles            = "nobbs_quantiles",
  surveillance               = "sur_fit",
  EpiNow2                    = "epinow2_fit",
  EpiNow2_dist               = "dist_fit",
  epidist                    = "delay_model"
)

# The figures need a nowcast per stratum as well as the pooled one. `pooled` is
# the fit the article's "Simple nowcast" section builds; `stratified` the one
# its "With strata and effects" section builds. A stratified fit already carries
# its own stratum labels, so nothing here refits anything per stratum -- that
# was the old script's approach and it did NOT match what the article showed.
PANELS <- list(
  diseasenowcasting = list(pooled = "dnc_fit",         stratified = "dnc_seasonal"),
  baselinenowcast   = list(pooled = "nowcast_samples", stratified = "nowcasts_by_stratum"),
  epinowcast        = list(pooled = "enw_fit",         stratified = "enw_seasonal_fit"),
  NobBS             = list(pooled = "nobbs_fit",       stratified = "stratified_nobbs"),
  surveillance      = list(pooled = "sur_fit",         stratified = "sur_by_stratum"),
  EpiNow2           = list(pooled = "epinow2_fit",     stratified = "regional_fit")
)

# Extra `tidy()` calls the article shows with arguments. The call is written
# here exactly as the article prints it, one line above the result.
TIDY_EXTRAS <- list(
  baselinenowcast_probs = quote(tidy(nowcast_samples, probs = c(0.05, 0.5, 0.95))),
  NobBS_quantiles_probs = quote(tidy(nobbs_quantiles, probs = c(0.1, 0.5, 0.9)))
)

# Reported at the END rather than here: a missing object is almost always a
# failed chunk, and the run is more useful if it also says what it DID manage to
# fit. `MISSING` is carried down to the final report.
MISSING <- {
  wanted <- unique(c(DISPLAYED, unlist(PANELS, use.names = FALSE)))
  wanted[!vapply(wanted, exists, logical(1), envir = article, inherits = FALSE)]
}
if (length(MISSING)) {
  message("\nthe article did not define: ", paste(MISSING, collapse = ", "),
          "\n  -- either the chunk that builds it failed above, or it was ",
          "renamed and DISPLAYED / PANELS needs updating.")
}

# -- 4. what the article prints ------------------------------------------------

# Stan / JAGS / RTMB fits are large and version-fragile, so the file stores what
# the article SHOWS rather than the objects themselves: the fit's own print
# output, and its `tidy()` table.
capture_display <- function(object) {
  if (is.null(object)) return(NULL)
  # Capture BOTH streams: several print methods (diseasenowcasting's among them)
  # write to the message connection, so capturing stdout alone returns nothing.
  printed <- tryCatch({
    from_message <- NULL
    from_stdout <- utils::capture.output(
      from_message <- utils::capture.output(print(object), type = "message")
    )
    if (length(from_stdout)) c(from_stdout, from_message) else from_message
  }, error = function(e) paste("<print failed:", conditionMessage(e), ">"))

  tidied <- tryCatch(
    suppressWarnings(suppressMessages(generics::tidy(object))),
    error = function(e) NULL
  )
  list(printed = printed, tidy = tidied)
}

display <- lapply(DISPLAYED, function(name) capture_display(get_object(name)))
names(display) <- names(DISPLAYED)

for (key in names(TIDY_EXTRAS)) {
  display[[key]] <- list(
    printed = NULL,
    tidy = tryCatch(
      suppressWarnings(suppressMessages(eval(TIDY_EXTRAS[[key]], envir = article))),
      error = function(e) {
        message("  ! ", key, ": ", conditionMessage(e))
        NULL
      }
    )
  )
}

# -- 5. the nowcasts the figures plot ------------------------------------------

covid_now      <- get_object("covid_now")
covid_seasonal <- get_object("covid_seasonal")
cutoff         <- get_object("cutoff")
NOW            <- get_now(covid_now)
grid           <- sort(unique(covid_now[[get_event_date(covid_now)]]))
horizon        <- utils::tail(grid, N_HORIZON)

# Engines report on their own grids; snap onto the data's daily grid. CEILING,
# not floor: a label carries the cases that fall on or before it.
snap_to_grid <- function(dates, grid) {
  dates <- as.Date(dates)
  idx <- vapply(dates, function(d) {
    hit <- which(grid >= d)
    if (length(hit)) hit[1L] else NA_integer_
  }, integer(1))
  out <- rep(as.Date(NA), length(idx))
  out[!is.na(idx)] <- grid[idx[!is.na(idx)]]
  out
}

# A stratified fit is either one object that knows its own strata (epinowcast,
# NobBS.strat, diseasenowcasting) or a named LIST of one-series fits
# (baselinenowcast's triangle list, surveillance's `split()` loop). `tidy()`
# handles both -- `tidy.list()` recognises the list shapes by structure -- so
# there is nothing engine-specific here.
tidy_one <- function(object) {
  if (is.null(object)) return(NULL)
  tidied <- tryCatch(
    suppressWarnings(suppressMessages(generics::tidy(object))),
    error = function(e) {
      message("  ! tidy failed: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(tidied) || !nrow(tidied)) return(NULL)
  tidied
}

# `tidy()` labels an unstratified fit "all"; the figures call that panel "Total".
normalise_stratum <- function(stratum) {
  ifelse(stratum %in% c("all", "All", NA), "Total", as.character(stratum))
}

collect <- function(package, spec) {
  pieces <- list()
  for (kind in c("pooled", "stratified")) {
    tidied <- tidy_one(get_object(spec[[kind]]))
    if (is.null(tidied)) {
      message("  ! ", package, " [", kind, "]: nothing to tidy")
      next
    }
    if (kind == "pooled") tidied$stratum <- "Total"
    pieces[[kind]] <- tidied
  }
  if (!length(pieces)) return(NULL)
  bind_rows(pieces) |>
    transmute(
      package    = package,
      stratum    = normalise_stratum(.data$stratum),
      event_date = snap_to_grid(.data$event_date, grid),
      estimate   = .data$estimate,
      lower      = .data$conf.low,
      upper      = .data$conf.high
    ) |>
    filter(!is.na(.data$event_date)) |>
    # A pooled fit and a stratified fit can both claim "Total" if the stratified
    # one happened to be unstratified after all. Keep the pooled row.
    distinct(.data$package, .data$stratum, .data$event_date, .keep_all = TRUE)
}

message("tidying fits ...")
nowcasts <- bind_rows(lapply(names(PANELS), function(pkg) {
  message("  - ", pkg)
  collect(pkg, PANELS[[pkg]])
}))
# An empty result is possible when a run failed early, and `arrange()` on a
# zero-COLUMN tibble errors on names that are not there. Keep the shape.
if (!nrow(nowcasts)) {
  nowcasts <- tibble::tibble(
    package = character(), stratum = character(),
    event_date = as.Date(character()),
    estimate = numeric(), lower = numeric(), upper = numeric()
  )
} else {
  nowcasts <- arrange(nowcasts, .data$package, .data$stratum, .data$event_date)
}

# A Stan or MCMC fit that does not converge does not error -- it returns
# numbers. One cached EpiNow2 fit had its upper bound at 1e8 for a stratum whose
# observed maximum was 842, and nothing caught it until somebody read the
# numbers. So read the numbers.
IMPLAUSIBLE_FACTOR <- 100

# -- 6. what actually happened -------------------------------------------------

data(covid_colombia, envir = environment())

truth_for <- function(full, seen, label) {
  full |>
    group_by(.data$notification_date) |>
    summarise(truth = sum(.data$n), .groups = "drop") |>
    left_join(
      seen |>
        group_by(.data$notification_date) |>
        summarise(observed = sum(.data$n), .groups = "drop"),
      by = "notification_date"
    ) |>
    mutate(observed = coalesce(.data$observed, 0L), stratum = label) |>
    rename(event_date = "notification_date")
}

# Everything notified before the cut, INCLUDING the reports that arrived after
# it -- the eventual truth the nowcast is trying to reach.
full_window <- covid_colombia |> filter(.data$notification_date < cutoff)
seen_window <- covid_colombia |>
  filter(.data$notification_date < cutoff, .data$diagnosis_date < cutoff)

truth <- bind_rows(
  truth_for(full_window, seen_window, "Total"),
  bind_rows(lapply(split(full_window, full_window$sex), function(g) {
    lvl <- as.character(g$sex[1])
    truth_for(g, seen_window |> filter(.data$sex == lvl), lvl)
  }))
)

implausible <- nowcasts |>
  filter(!is.na(.data$estimate)) |>
  group_by(.data$package, .data$stratum) |>
  summarise(worst = max(c(.data$estimate, .data$upper), na.rm = TRUE),
            .groups = "drop") |>
  left_join(
    truth |>
      group_by(.data$stratum) |>
      summarise(observed = max(.data$truth, na.rm = TRUE), .groups = "drop"),
    by = "stratum"
  ) |>
  filter(.data$worst > IMPLAUSIBLE_FACTOR * .data$observed)

if (nrow(implausible)) {
  print(as.data.frame(implausible))
  stop("the fits above did not converge: the largest estimate or upper bound is ",
       "more than ", IMPLAUSIBLE_FACTOR, "x the observed maximum. Refusing to ",
       "cache them.", call. = FALSE)
}

# -- 7. save -------------------------------------------------------------------

comparison <- list(
  nowcasts = nowcasts,
  truth    = truth,
  display  = display,
  meta = list(
    now           = NOW,
    cut           = cutoff,
    horizon       = horizon,
    n_days        = length(grid),
    n_cases       = sum(seen_window$n),
    built_on      = Sys.Date(),
    package_order = names(PANELS),
    source        = ARTICLE
  )
)

saveRDS(comparison, OUT, compress = "xz")
message("wrote ", OUT, " (",
        format(file.size(OUT) / 1024^2, digits = 3), " MB)")

# -- 8. sanity check -----------------------------------------------------------

message("\nnowcasts by package and stratum:")
print(table(nowcasts$package, nowcasts$stratum))

missing_panels <- setdiff(
  paste(rep(names(PANELS), each = 3),
        c("Total", "Female", "Male"), sep = " / "),
  paste(nowcasts$package, nowcasts$stratum, sep = " / ")
)
if (length(missing_panels)) {
  message("\nMISSING panels (the figures will have gaps):\n  ",
          paste(missing_panels, collapse = "\n  "))
}

# The file is saved BEFORE this, deliberately: a partial rebuild that cost an
# hour of sampling is worth keeping on disk even though it is not publishable.
# The non-zero exit is what says it is not publishable.
if (length(failures) || length(MISSING) || length(missing_panels)) {
  if (length(failures)) {
    message("\nchunks that failed:")
    for (f in failures) {
      message("  * ", paste(utils::head(f$code, 3), collapse = " "),
              "\n      ", f$message)
    }
  }
  stop("the rebuild is incomplete -- ", OUT, " was written but should NOT be ",
       "committed until the problems above are fixed.", call. = FALSE)
}
message("\nrebuild complete.")
