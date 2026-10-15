# Checkpointing for `nowcast_backtest()`.
#
# One rule keeps this safe under every `future` plan: ONLY THE MAIN R SESSION
# TOUCHES THE FILE. A worker (`multisession`, `cluster`, ...) returns its fit like
# any other value and never sees the path, so there is nothing for two processes
# to fight over and no assumption that workers share a filesystem. The main
# session reads the file once before the fits and rewrites it after each wave of
# finished fits, through a temporary file and a rename, so an interruption in the
# middle of a write leaves the previous checkpoint intact.
#
# The file is one list:
#   version      1L
#   fingerprint  what the fits were computed from (see `.backtest_fingerprint()`)
#   fits         a list of `list(label, now, fit)`, where `fit` is exactly what
#                `.backtest_fit()` returns (`timing` and `result`)

#' Validate `checkpoint_file`
#'
#' @param checkpoint_file The user's `checkpoint_file`.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
.check_checkpoint_file <- function(checkpoint_file) {
  if (is.null(checkpoint_file)) {
    return(invisible(NULL))
  }
  if (!is.character(checkpoint_file) || length(checkpoint_file) != 1L ||
      is.na(checkpoint_file) || !nzchar(checkpoint_file)) {
    cli::cli_abort(
      "{.arg checkpoint_file} must be a single file path, or {.code NULL}."
    )
  }
  if (dir.exists(checkpoint_file)) {
    cli::cli_abort(c(
      "{.arg checkpoint_file} must be a file, not a directory.",
      "x" = "{.path {checkpoint_file}} is a directory."
    ))
  }
  invisible(NULL)
}

#' Reduce an object to something that hashes the same in every session
#'
#' An engine argument can be a function or formula, and hashing one drags in its
#' environment -- which differs between sessions even when nothing meaningful
#' changed. Functions, formulas and calls are replaced by their source text and
#' environments by a placeholder; everything else is left alone.
#'
#' @param x Any object.
#'
#' @return An object that [rlang::hash()] can fingerprint.
#'
#' @keywords internal
#' @noRd
.stable_form <- function(x) {
  if (is.function(x) || is.language(x)) {
    return(paste(deparse(x), collapse = "\n"))
  }
  if (is.environment(x)) {
    return("<environment>")
  }
  if (is.list(x)) {
    return(lapply(x, .stable_form))
  }
  x
}

#' What a backtest's fits were computed from
#'
#' Everything that, if changed, would make a saved fit answer a different
#' question. `now_dates` is deliberately absent -- fits at other dates can be
#' added to a checkpoint -- and so is `parallel`, which does not change the fits.
#' Engines are fingerprinted one by one, by label, so an engine can be added to
#' a checkpoint without invalidating the others.
#'
#' @param x The full `tbl_now`.
#' @param engines The labelled engines from `.collect_engines()`.
#' @param seed,keep_draws,truth_axis,truth_type As in [nowcast_backtest()].
#'
#' @return A list.
#'
#' @keywords internal
#' @noRd
.backtest_fingerprint <- function(x, engines, seed, keep_draws, truth_axis,
                                  truth_type) {
  plain <- dplyr::as_tibble(ungroup(x))
  data <- rlang::hash(list(
    columns = as.list(plain),
    now = get_now(x),
    event_date = get_event_date(x),
    report_date = get_report_date(x),
    case_count = get_case_count(x),
    strata = get_strata(x),
    data_type = get_data_type(x)
  ))
  engine_hash <- vapply(engines, function(e) {
    # The label is the lookup key, not part of the model.
    e$label <- NULL
    rlang::hash(.stable_form(unclass(e)))
  }, character(1))

  list(
    data = data,
    seed = if (is.null(seed)) NULL else as.numeric(seed),
    keep_draws = isTRUE(keep_draws),
    truth_axis = truth_axis,
    truth_type = truth_type,
    engines = engine_hash
  )
}

#' Read a checkpoint
#'
#' @param file Path to the checkpoint.
#'
#' @return The saved state, or `NULL` when there is no file yet.
#'
#' @keywords internal
#' @noRd
.checkpoint_read <- function(file) {
  if (!file.exists(file)) {
    return(NULL)
  }
  state <- tryCatch(readRDS(file), error = function(e) e)
  valid <- !inherits(state, "error") && is.list(state) &&
    identical(state$version, 1L) &&
    all(c("fingerprint", "fits") %in% names(state))
  if (!valid) {
    cli::cli_abort(c(
      "{.path {file}} is not a {.fn nowcast_backtest} checkpoint.",
      "i" = "Delete it, or choose another {.arg checkpoint_file}, to start a \\
             new backtest."
    ))
  }
  state
}

#' Write a checkpoint without ever leaving a half-written file
#'
#' Writes to a temporary file in the same folder and renames it over the target.
#' Called only from the main R session; see the note at the top of this file.
#'
#' @param file Path to the checkpoint.
#' @param state The state to save.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
.checkpoint_write <- function(file, state) {
  folder <- dirname(file)
  if (!dir.exists(folder)) {
    dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  }
  temporary <- tempfile(
    pattern = paste0(".", basename(file), "-"), tmpdir = folder,
    fileext = ".tmp"
  )
  on.exit(unlink(temporary), add = TRUE)

  written <- tryCatch(
    {
      saveRDS(state, temporary)
      TRUE
    },
    error = function(e) FALSE,
    warning = function(w) FALSE
  )
  # `file.rename()` is atomic within a filesystem. Where it cannot replace an
  # existing file (some Windows setups) fall back to copying over it.
  moved <- written && (suppressWarnings(file.rename(temporary, file)) ||
    isTRUE(suppressWarnings(file.copy(temporary, file, overwrite = TRUE))))
  if (!moved) {
    cli::cli_abort(c(
      "Could not write the checkpoint to {.path {file}}.",
      "i" = "Check that the folder exists and is writable."
    ))
  }
  invisible(NULL)
}

#' Check a saved checkpoint against the current call
#'
#' @param state The saved state.
#' @param current The current `.backtest_fingerprint()`.
#' @param file The checkpoint path, for the message.
#'
#' @return `state`, with the fingerprint's engines updated to include the
#'   current ones. Aborts when the checkpoint came from a different backtest.
#'
#' @keywords internal
#' @noRd
.checkpoint_reconcile <- function(state, current, file) {
  saved <- state$fingerprint

  described <- c(
    data = "the data", seed = "{.arg seed}",
    keep_draws = "{.arg keep_draws}", truth_axis = "{.arg truth_axis}",
    truth_type = "{.arg truth_type}"
  )
  changed <- names(described)[!vapply(
    names(described), function(field) identical(saved[[field]], current[[field]]),
    logical(1)
  )]
  shared <- intersect(names(saved$engines), names(current$engines))
  changed_engines <- shared[saved$engines[shared] != current$engines[shared]]

  if (length(changed) > 0L || length(changed_engines) > 0L) {
    bullets <- c(
      unname(described[changed]),
      if (length(changed_engines) > 0L) {
        "the engine{?s} {.val {changed_engines}}"
      }
    )
    names(bullets) <- rep("x", length(bullets))
    cli::cli_abort(c(
      "The checkpoint {.path {file}} was made by a different backtest.",
      "!" = "These differ from the checkpoint:",
      bullets,
      "i" = "Resuming would mix fits from two different backtests. Use a new \\
             {.arg checkpoint_file}, or delete this one, to start again."
    ))
  }

  state$fingerprint$engines <- c(
    saved$engines[setdiff(names(saved$engines), names(current$engines))],
    current$engines
  )
  state
}

#' Where a fit is in the checkpoint
#'
#' @param fits The `fits` list of a checkpoint.
#' @param label,now_date The engine label and retrospective date.
#'
#' @return The position, or `0L`.
#'
#' @keywords internal
#' @noRd
.checkpoint_position <- function(fits, label, now_date) {
  hit <- which(vapply(fits, function(entry) {
    identical(entry$label, label) && as.numeric(entry$now) == as.numeric(now_date)
  }, logical(1)))
  if (length(hit) == 0L) 0L else hit[[1]]
}

#' Run the backtest fits, saving them as they finish
#'
#' Without a checkpoint this is the plain sequential or parallel map. With one,
#' the fits already saved are reused and the rest run in waves -- one fit at a
#' time sequentially, `future::nbrOfWorkers()` at a time in parallel -- with the
#' file updated after each wave. A fit that failed is retried, not reused.
#'
#' @param tasks The list of backtest tasks, in output order.
#' @param fit_args A list of the remaining `.backtest_fit()` arguments.
#' @param parallel Whether to run the fits with `future`.
#' @param checkpoint `NULL`, or a list with `file` and `fingerprint`.
#' @param verbose Whether to report progress.
#'
#' @return A list of `.backtest_fit()` results, one per task, in task order.
#'
#' @keywords internal
#' @noRd
.backtest_run_tasks <- function(tasks, fit_args, parallel, checkpoint = NULL,
                                verbose = TRUE) {
  run_wave <- function(wave) {
    if (parallel) {
      .backtest_parallel_map(wave, fit_args)
    } else {
      lapply(wave, .backtest_run_task, fit_args = fit_args)
    }
  }
  if (is.null(checkpoint)) {
    return(run_wave(tasks))
  }

  file <- checkpoint$file
  state <- .checkpoint_read(file)
  state <- if (is.null(state)) {
    list(version = 1L, fingerprint = checkpoint$fingerprint, fits = list())
  } else {
    .checkpoint_reconcile(state, checkpoint$fingerprint, file)
  }

  done <- vapply(tasks, function(task) {
    position <- .checkpoint_position(state$fits, task$label, task$now_date)
    position > 0L && !is.null(state$fits[[position]]$fit$result)
  }, logical(1))
  pending <- tasks[!done]

  if (isTRUE(verbose) && length(tasks) > 0L && any(done)) {
    cli::cli_alert_info(
      "Resuming from {.path {file}}: {sum(done)} of {length(tasks)} fit{?s} \\
       already done."
    )
  }

  if (length(pending) > 0L) {
    # Written before the first fit, so an unwritable path fails now rather than
    # after hours of fitting.
    .checkpoint_write(file, state)

    wave_size <- if (parallel) max(1L, as.integer(future::nbrOfWorkers())) else 1L
    for (start in seq(1L, length(pending), by = wave_size)) {
      wave <- pending[start:min(length(pending), start + wave_size - 1L)]
      fits <- run_wave(wave)
      for (i in seq_along(wave)) {
        entry <- list(
          label = wave[[i]]$label, now = wave[[i]]$now_date, fit = fits[[i]]
        )
        position <- .checkpoint_position(
          state$fits, entry$label, entry$now
        )
        if (position == 0L) {
          position <- length(state$fits) + 1L
        }
        state$fits[[position]] <- entry
      }
      .checkpoint_write(file, state)
    }
  }

  lapply(tasks, function(task) {
    state$fits[[.checkpoint_position(state$fits, task$label, task$now_date)]]$fit
  })
}
