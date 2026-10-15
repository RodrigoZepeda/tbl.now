#' Combine backtests
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' Joins [nowcast_backtest()] results that were run separately into one, so that
#' an expensive backtest never has to be repeated:
#'
#' * **Different models, same dates.** Backtest each engine on its own (perhaps
#'   on different machines, or as it becomes available) and combine them to
#'   compare their scores, derive [nowcast_weights()] or build a
#'   [nowcast_ensemble()].
#' * **Same model, different dates.** Backtest last year's dates once, and next
#'   year backtest only the new dates and add them to the old result.
#'
#' Combining backtests of *the same data, dates and engines* gives the object a
#' single call would have -- the tables have the same rows in the same order:
#'
#' ```r
#' bt1 <- nowcast_backtest(x, engine_a, now_dates = my_dates)
#' bt2 <- nowcast_backtest(x, engine_b, now_dates = my_dates)
#' backtest_combine(bt1, bt2)
#' # is the same as
#' nowcast_backtest(x, engine_a, engine_b, now_dates = my_dates)
#' ```
#'
#' (Only the `elapsed_seconds` of the `timings` differ.)
#'
#' @details
#' The backtests must agree on the event date, the strata, `truth_axis`,
#' `truth_type`, `keep_draws` and the quantile levels; otherwise they are not
#' measuring the same thing and the call aborts.
#'
#' Two backtests must not both have a successful fit of the same method at the
#' same `now` date: that would be two answers to one question, so the call
#' aborts. A fit that *failed* in one and succeeded in another is fine -- the
#' success is kept, which lets you re-run only the failures and combine.
#'
#' Scores are kept as they were computed. When the backtests were run at
#' different times the data may have been revised in between, so each was
#' scored against its own `truth`. The combined `truth` is the one from the
#' backtest with the latest `now` date, extended with any event dates only the
#' others have; the call warns when an event date it shares has a different
#' observed count in the others. Re-run the earlier backtest if you need every
#' score against the same truth.
#'
#' Methods that cover different dates are allowed (that is how you add dates to
#' one model only), and comparisons -- [nowcast_weights()], the print method --
#' then use only the targets every method scored, and say so. Use
#' `only_common_dates = TRUE` to drop the dates that not every method has.
#'
#' @param ... Two or more [nowcast_backtest()] objects, or a single list of
#'   them. The order of the methods in the result follows the order given.
#' @param only_common_dates Logical. Keep only the `now` dates at which every
#'   method has a successful fit. Default `FALSE` keeps them all.
#'
#' @return A `nowcast_backtest`; see [nowcast_backtest()].
#'
#' @seealso [nowcast_backtest()], whose `checkpoint_file` argument resumes an
#'   interrupted backtest, [nowcast_weights()] and [nowcast_ensemble()].
#'
#' @examples
#' data(denguedat)
#' recent <- subset(denguedat, onset_week >= as.Date("2010-06-01"))
#' dengue <- tbl_now(recent,
#'   event_date = onset_week, report_date = report_week, verbose = FALSE
#' )
#' dates <- as.Date(c("2010-10-04", "2010-11-15"))
#'
#' # Different models, same dates: backtest separately, then combine.
#' narrow <- nowcast_backtest(dengue,
#'   example_engine(spread = 0.1, label = "narrow"),
#'   now_dates = dates, verbose = FALSE
#' )
#' wide <- nowcast_backtest(dengue,
#'   example_engine(spread = 0.5, label = "wide"),
#'   now_dates = dates, verbose = FALSE
#' )
#' both <- backtest_combine(narrow, wide)
#' both$methods
#'
#' # Same model, new dates: add them to what you already have.
#' later <- nowcast_backtest(dengue,
#'   example_engine(spread = 0.1, label = "narrow"),
#'   now_dates = as.Date("2010-11-22"), verbose = FALSE
#' )
#' backtest_combine(narrow, later)$now_dates
#'
#' @export
backtest_combine <- function(..., only_common_dates = FALSE) {
  if (!rlang::is_bool(only_common_dates)) {
    cli::cli_abort("{.arg only_common_dates} must be {.code TRUE} or {.code FALSE}.")
  }

  backtests <- rlang::list2(...)
  if (length(backtests) == 1L && is.list(backtests[[1]]) &&
      !inherits(backtests[[1]], "nowcast_backtest")) {
    backtests <- backtests[[1]]
  }
  if (length(backtests) == 0L) {
    cli::cli_abort("{.fn backtest_combine} needs at least one {.cls nowcast_backtest}.")
  }
  for (i in seq_along(backtests)) {
    if (!inherits(backtests[[i]], "nowcast_backtest")) {
      cli::cli_abort(c(
        "Every argument must be a {.cls nowcast_backtest}.",
        "x" = "Argument {i} is of class {.cls {class(backtests[[i]])}}."
      ))
    }
  }
  backtests <- unname(backtests)
  .check_backtests_compatible(backtests)

  # The order the methods came in, which is the order a single call with all the
  # engines would have used.
  method_order <- unique(unlist(lapply(backtests, function(b) {
    unique(c(b$timings$.method, b$methods))
  })))
  by_fit_order <- function(table) {
    dplyr::arrange(table, .data$.now, match(.data$.method, method_order))
  }

  timings <- .combine_backtest_timings(backtests) |> by_fit_order()
  scores <- dplyr::bind_rows(lapply(backtests, `[[`, "scores")) |> by_fit_order()
  predictions <- dplyr::bind_rows(lapply(backtests, `[[`, "predictions")) |>
    by_fit_order()
  draw_tables <- lapply(backtests, `[[`, "draws")
  draw_tables <- draw_tables[!vapply(draw_tables, is.null, logical(1))]
  draws <- if (length(draw_tables) == 0L) {
    NULL
  } else {
    dplyr::bind_rows(draw_tables) |> by_fit_order()
  }
  now_dates <- sort(unique(do.call(c, lapply(backtests, `[[`, "now_dates"))))

  first <- backtests[[1]]
  combined <- structure(
    list(
      scores = scores,
      predictions = predictions,
      draws = draws,
      timings = timings,
      truth = .combine_backtest_truth(backtests),
      methods = unique(scores$.method),
      now_dates = now_dates,
      keep_draws = first$keep_draws,
      event_date = first$event_date,
      strata = first$strata,
      truth_axis = first$truth_axis,
      truth_type = first$truth_type
    ),
    class = "nowcast_backtest"
  )

  if (only_common_dates) {
    combined <- .restrict_backtest_to_common_dates(combined)
  }
  combined
}

#' Refuse to combine backtests that answer different questions
#'
#' @param backtests A list of `nowcast_backtest` objects.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
.check_backtests_compatible <- function(backtests) {
  first <- backtests[[1]]
  levels_of <- function(b) sort(unique(b$predictions$.quantile_level))

  for (field in c("event_date", "strata", "truth_axis", "truth_type", "keep_draws")) {
    same <- vapply(backtests, function(b) identical(b[[field]], first[[field]]), logical(1))
    if (!all(same)) {
      cli::cli_abort(c(
        "Backtests must share the same {.field {field}} to be combined.",
        "x" = "Backtest{?s} {which(!same)} differ{?s/} from backtest 1.",
        "i" = "Backtests of different data or scored against a different truth \\
               are not comparable."
      ))
    }
  }
  same_levels <- vapply(
    backtests, function(b) isTRUE(all.equal(levels_of(b), levels_of(first))),
    logical(1)
  )
  if (!all(same_levels)) {
    cli::cli_abort(c(
      "Backtests must report the same quantile levels to be combined.",
      "x" = "Backtest{?s} {which(!same_levels)} differ{?s/} from backtest 1.",
      "i" = "The weighted interval score averages over the levels reported, so \\
             models summarised differently are not comparable."
    ))
  }
  invisible(NULL)
}

#' Combine the fit timings, refusing two successes for one fit
#'
#' The timings hold one row per attempted fit, so they are the record of which
#' (method, date) fits each backtest has -- including the failures, which have
#' no scores.
#'
#' @param backtests A list of `nowcast_backtest` objects.
#'
#' @return A tibble of timings, one row per (method, `now`).
#'
#' @keywords internal
#' @noRd
.combine_backtest_timings <- function(backtests) {
  timings <- dplyr::bind_rows(lapply(backtests, `[[`, "timings"))
  successes <- timings |>
    dplyr::filter(.data$success) |>
    dplyr::count(.data$.method, .data$.now)
  clashes <- successes |>
    dplyr::filter(.data$n > 1L)
  if (nrow(clashes) > 0L) {
    clashes$label <- paste0(clashes$.method, " at ", as.character(clashes$.now))
    cli::cli_abort(c(
      "Backtests must not overlap: {nrow(clashes)} fit{?s} appear{?s/} in more \\
       than one.",
      "x" = "Fitted more than once: {.val {utils::head(clashes$label, 5)}}.",
      "i" = "Combine backtests of different dates or different methods, or drop \\
             the duplicates first."
    ))
  }

  # A failure is superseded by a success for the same fit; among failures only
  # the latest is kept.
  timings |>
    dplyr::filter(
      if (any(.data$success)) .data$success else dplyr::row_number() == dplyr::n(),
      .by = c(".method", ".now")
    )
}

#' Combine the truth tables of several backtests
#'
#' @param backtests A list of `nowcast_backtest` objects.
#'
#' @return The truth of the backtest with the latest `now` date, plus the event
#'   dates only the others have. Warns when a shared cell disagrees.
#'
#' @keywords internal
#' @noRd
.combine_backtest_truth <- function(backtests) {
  latest <- max(vapply(backtests, function(b) as.numeric(max(b$now_dates)), numeric(1)))
  primary <- max(which(vapply(
    backtests, function(b) as.numeric(max(b$now_dates)) == latest, logical(1)
  )))
  truth <- backtests[[primary]]$truth
  key <- c(backtests[[primary]]$event_date, backtests[[primary]]$strata)

  disagree <- 0L
  extra <- list()
  for (i in setdiff(seq_along(backtests), primary)) {
    other <- backtests[[i]]$truth
    shared <- dplyr::inner_join(
      truth, other, by = key, suffix = c("", ".other")
    )
    disagree <- disagree + sum(shared$.observed != shared$.observed.other, na.rm = TRUE)
    extra[[length(extra) + 1L]] <- dplyr::anti_join(other, truth, by = key)
  }

  if (disagree > 0L) {
    cli::cli_warn(c(
      "The combined backtests disagree on {disagree} observed count{?s}.",
      "i" = "The counts of the backtest with the latest {.field now} are used \\
             as {.field truth}; the scores were computed against each backtest's \\
             own truth."
    ))
  }
  extra <- dplyr::bind_rows(extra)
  if (nrow(extra) > 0L) {
    truth <- dplyr::bind_rows(truth, extra) |>
      dplyr::arrange(dplyr::across(dplyr::all_of(key)))
  }
  truth
}

#' Keep only the dates every method has a successful fit for
#'
#' @param backtest A combined `nowcast_backtest`.
#'
#' @return The restricted `nowcast_backtest`.
#'
#' @keywords internal
#' @noRd
.restrict_backtest_to_common_dates <- function(backtest) {
  fitted <- backtest$scores |>
    dplyr::distinct(.data$.method, .data$.now)
  common <- fitted |>
    dplyr::count(.data$.now) |>
    dplyr::filter(.data$n == length(unique(fitted$.method))) |>
    dplyr::pull(".now")

  if (length(common) == 0L) {
    cli::cli_abort(c(
      "No {.field now} date has a successful fit for every method.",
      "i" = "Use {.code only_common_dates = FALSE} to keep them all."
    ))
  }
  for (table in c("scores", "predictions", "draws", "timings")) {
    if (!is.null(backtest[[table]])) {
      backtest[[table]] <- dplyr::filter(backtest[[table]], .data$.now %in% common)
    }
  }
  backtest$now_dates <- backtest$now_dates[backtest$now_dates %in% common]
  backtest$methods <- unique(backtest$scores$.method)
  backtest
}
