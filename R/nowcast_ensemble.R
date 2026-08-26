# Combining several nowcasts into one.
#
# Two combination rules are supported:
#
#   "quantile"    Vincentization: average the members' quantiles level by
#                 level. Always applicable, because every backend produces
#                 quantiles.
#   "linear_pool" Pool the members' posterior draws (a mixture distribution),
#                 then re-summarise. Needs draws from every member, and is
#                 usually wider than the quantile average.

#' Combine several nowcasts into an ensemble
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' Takes the nowcasts produced by different modelling packages on the *same*
#' `tbl_now` and combines them into a single [tbl_nowcast]. Ensembles are
#' routinely better calibrated than any of their members, and because
#' [run_nowcast()] puts every backend on the same tidy footing, combining them
#' needs no reshaping on your side.
#'
#' @param ... [tbl_nowcast] objects, or a single list of them. Named arguments
#'   rename the members.
#' @param type How to combine the members:
#'
#'   \describe{
#'     \item{`"quantile"` (default)}{Average the members' predictive quantiles
#'       level by level (Vincentization). Applicable to every backend.}
#'     \item{`"linear_pool"`}{Pool the members' posterior draws into a mixture
#'       distribution and re-summarise it. Requires that every member returned
#'       draws, and generally yields wider intervals.}
#'   }
#'
#' @param weights Either the string `"equal"` (default), a numeric vector (named
#'   by method, or in the order the members were given), or one of
#'   `"inverse_score"` / `"optim"`. The last two require `backtest` and are
#'   passed to [nowcast_weights()].
#' @param backtest A [nowcast_backtest()] object, required when `weights` is
#'   `"inverse_score"` or `"optim"`.
#' @param quantile_levels Quantile levels to report the ensemble at. Defaults to
#'   the levels shared by all members.
#' @param n_draws Number of draws in the pooled sample when
#'   `type = "linear_pool"`. Default `4000`.
#' @param name Name to record as the ensemble's `method`. Default
#'   `"ensemble"`.
#' @param verbose Logical. Whether to report the weights that were used.
#'
#' @return A [tbl_nowcast] whose `fit` property is the list of member nowcasts
#'   and whose `metadata` holds the `weights` and the combination `type`.
#'
#' @seealso [run_nowcast()], [nowcast_backtest()], [nowcast_weights()]
#'
#' @examples
#' toy <- function(method, shift) {
#'   predictions <- data.frame(
#'     onset_week = as.Date("2020-01-05"),
#'     .quantile_level = c(0.25, 0.5, 0.75),
#'     .value = c(8, 10, 13) + shift
#'   )
#'   tbl_nowcast(predictions = predictions, method = method, event_date = "onset_week")
#' }
#'
#' nowcast_ensemble(toy("a", 0), toy("b", 4), verbose = FALSE)
#'
#' # Unequal weights
#' nowcast_ensemble(toy("a", 0), toy("b", 4), weights = c(a = 0.75, b = 0.25), verbose = FALSE)
#'
#' @export
nowcast_ensemble <- function(..., type = c("quantile", "linear_pool"),
                             weights = "equal", backtest = NULL,
                             quantile_levels = NULL, n_draws = 4000L,
                             name = "ensemble", verbose = TRUE) {
  type <- match.arg(type)
  members <- .collect_nowcasts(...)

  if (length(members) < 2) {
    cli::cli_abort("An ensemble needs at least two nowcasts; got {length(members)}.")
  }

  .check_ensemble_compatibility(members)
  weights <- .resolve_weights(weights, members, backtest)

  if (isTRUE(verbose)) {
    cli::cli_alert_info(
      "Combining {length(members)} nowcast{?s} ({type}) with weights \\
       {.val {paste0(names(weights), ' = ', signif(weights, 3))}}."
    )
  }

  event_col <- members[[1]]@event_date
  strata <- members[[1]]@strata
  key <- c(event_col, strata)

  combined <- switch(type,
    quantile = .ensemble_quantiles(members, weights, key, quantile_levels),
    linear_pool = .ensemble_linear_pool(members, weights, key, quantile_levels, n_draws)
  )

  tbl_nowcast(
    predictions = combined$predictions,
    draws = combined$draws,
    method = name,
    fit = members,
    now = members[[1]]@now,
    event_date = event_col,
    strata = strata,
    data = members[[1]]@data,
    call = match.call(),
    metadata = list(weights = weights, type = type, members = names(weights))
  )
}

#' Collect the nowcasts passed to `nowcast_ensemble()`
#'
#' Accepts either loose `tbl_nowcast` arguments or a single list of them, and
#' names each member (by its `method` when the user gave no name).
#'
#' @param ... The arguments of [nowcast_ensemble()].
#'
#' @return A named list of `tbl_nowcast` objects.
#'
#' @keywords internal
#' @noRd
.collect_nowcasts <- function(...) {
  members <- list(...)
  if (length(members) == 1 && is.list(members[[1]]) && !is_tbl_nowcast(members[[1]])) {
    members <- members[[1]]
  }

  not_nowcasts <- !vapply(members, is_tbl_nowcast, logical(1))
  if (any(not_nowcasts)) {
    cli::cli_abort(
      "Every element passed to {.fn nowcast_ensemble} must be a {.cls tbl_nowcast}; \\
       element{?s} {.val {which(not_nowcasts)}} {?is/are} not."
    )
  }

  given <- names(members)
  methods <- vapply(members, function(m) m@method, character(1))
  if (is.null(given)) {
    given <- rep("", length(members))
  }
  given[given == ""] <- methods[given == ""]
  # Two members from the same package would otherwise share a name.
  names(members) <- make.unique(given, sep = "_")
  members
}

#' Check that a set of nowcasts can be combined
#'
#' @param members A list of `tbl_nowcast` objects.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
.check_ensemble_compatibility <- function(members) {
  event_cols <- unique(vapply(members, function(m) m@event_date, character(1)))
  if (length(event_cols) > 1) {
    cli::cli_abort(
      "The nowcasts use different event-date columns ({.val {event_cols}}); \\
       they must come from the same {.cls tbl_now}."
    )
  }

  strata <- lapply(members, function(m) sort(m@strata))
  if (length(unique(strata)) > 1) {
    cli::cli_abort(c(
      "The nowcasts are stratified differently and cannot be combined.",
      "i" = "Strata found: {.val {unique(unlist(strata))}}."
    ))
  }

  nows <- unique(unlist(lapply(members, function(m) m@now)))
  if (length(nows) > 1) {
    cli::cli_warn(
      "The nowcasts have different {.field now} dates ({.val {nows}}); \\
       combining them anyway."
    )
  }

  invisible(NULL)
}

#' Resolve the `weights` argument of `nowcast_ensemble()`
#'
#' @param weights The user's `weights` argument.
#' @param members The named list of member nowcasts.
#' @param backtest A `nowcast_backtest` or `NULL`.
#'
#' @return A named numeric vector summing to 1, aligned with `members`.
#'
#' @keywords internal
#' @noRd
.resolve_weights <- function(weights, members, backtest) {
  member_names <- names(members)

  if (is.character(weights)) {
    weights <- match.arg(weights, c("equal", "inverse_score", "optim"))

    if (weights == "equal") {
      return(stats::setNames(rep(1 / length(members), length(members)), member_names))
    }

    if (is.null(backtest)) {
      cli::cli_abort(c(
        "{.code weights = \"{weights}\"} needs a {.arg backtest}.",
        "i" = "Produce one with {.fn nowcast_backtest} and pass it in."
      ))
    }

    fitted <- nowcast_weights(backtest, type = weights)
    # The backtest is keyed by method name, the members by their (possibly
    # user-given) names; fall back to the method when the name is unknown.
    methods <- vapply(members, function(m) m@method, character(1))
    lookup <- ifelse(member_names %in% names(fitted), member_names, methods)
    missing_members <- setdiff(lookup, names(fitted))
    if (length(missing_members) > 0) {
      cli::cli_abort(
        "The backtest has no scores for method{?s} {.val {missing_members}}."
      )
    }
    weights <- unname(fitted[lookup])
    return(stats::setNames(weights / sum(weights), member_names))
  }

  if (!is.numeric(weights)) {
    cli::cli_abort("{.arg weights} must be a numeric vector or one of {.val {c('equal', 'inverse_score', 'optim')}}.")
  }
  if (any(weights < 0)) {
    cli::cli_abort("{.arg weights} must be non-negative.")
  }

  if (!is.null(names(weights))) {
    missing_members <- setdiff(member_names, names(weights))
    if (length(missing_members) > 0) {
      cli::cli_abort("{.arg weights} has no entry for {.val {missing_members}}.")
    }
    weights <- weights[member_names]
  } else {
    if (length(weights) != length(members)) {
      cli::cli_abort(
        "{.arg weights} has length {length(weights)} but there {?is/are} \\
         {length(members)} nowcast{?s}."
      )
    }
    weights <- stats::setNames(weights, member_names)
  }

  if (sum(weights) == 0) {
    cli::cli_abort("{.arg weights} must not sum to zero.")
  }
  weights / sum(weights)
}

#' Quantile levels shared by every member
#'
#' @param members A list of `tbl_nowcast` objects.
#' @param quantile_levels The user's requested levels, or `NULL`.
#'
#' @return A sorted numeric vector.
#'
#' @keywords internal
#' @noRd
.common_quantile_levels <- function(members, quantile_levels) {
  available <- lapply(members, function(m) sort(unique(m@predictions$.quantile_level)))
  common <- Reduce(intersect, available)

  if (is.null(quantile_levels)) {
    if (length(common) == 0) {
      cli::cli_abort(c(
        "The nowcasts share no quantile levels.",
        "i" = "Refit them with the same {.arg quantile_levels}."
      ))
    }
    if (length(unique(available)) > 1) {
      cli::cli_warn(
        "The nowcasts were summarised at different quantile levels; \\
         using the {length(common)} level{?s} they share."
      )
    }
    return(sort(common))
  }

  quantile_levels <- sort(unique(as.numeric(quantile_levels)))
  missing_levels <- setdiff(quantile_levels, common)
  if (length(missing_levels) > 0) {
    cli::cli_abort(
      "Level{?s} {.val {missing_levels}} {?is/are} not available in every nowcast."
    )
  }
  quantile_levels
}

#' Keep only the targets EVERY member predicted
#'
#' A prediction target one member covers and another does not is not something
#' an ensemble can combine. Both combination rules here group by the target and
#' average what they find, so a target with a single member present comes out as
#' that member's own value -- carrying full weight, labelled as the ensemble,
#' with nothing to say it is not one. `EpiNow2` makes this concrete: it forecasts
#' one period past the end of the data, so before that was trimmed it contributed
#' a lone extra week at the `now` edge, which is the part of the picture people
#' actually read.
#'
#' @param members A named list of `tbl_nowcast` objects.
#' @param key Character vector of the columns identifying a prediction target.
#'
#' @return `members`, restricted to the targets common to all of them.
#'
#' @keywords internal
#' @noRd
.common_targets <- function(members, key) {
  if (length(members) < 2L) {
    return(members)
  }
  targets <- lapply(members, function(m) {
    unique(dplyr::select(m@predictions, dplyr::all_of(key)))
  })
  shared <- Reduce(function(a, b) dplyr::inner_join(a, b, by = key), targets)

  dropped <- vapply(targets, function(t) nrow(t) - nrow(shared), numeric(1))
  if (any(dropped > 0)) {
    losers <- names(members)[dropped > 0]
    cli::cli_warn(c(
      "Dropping {.val {max(dropped)}} prediction target{?s} that not every \\
       member covers.",
      "i" = "{.val {losers}} predict{?s/} {cli::qty(length(losers))}\\
             {?a target/targets} the other{?s} do{?es/} not; averaging over the \\
             member{?s} that happen{?s/} to be present would report one \\
             member's own value as the ensemble's.",
      "i" = "{.val {nrow(shared)}} shared target{?s} remain{?s/}."
    ))
  }

  lapply(members, function(m) {
    m@predictions <- dplyr::semi_join(m@predictions, shared, by = key)
    if (!is.null(m@draws)) {
      m@draws <- dplyr::semi_join(m@draws, shared, by = key)
    }
    m
  })
}

#' Weighted average of the members' quantiles
#'
#' @param members A named list of `tbl_nowcast` objects.
#' @param weights A named numeric vector summing to 1.
#' @param key Character vector of the columns identifying a prediction target.
#' @param quantile_levels Requested levels, or `NULL`.
#'
#' @return A list with `predictions` and `draws` (always `NULL` here).
#'
#' @keywords internal
#' @noRd
.ensemble_quantiles <- function(members, weights, key, quantile_levels) {
  members <- .common_targets(members, key)
  quantile_levels <- .common_quantile_levels(members, quantile_levels)

  stacked <- purrr_map_dfr(names(members), function(nm) {
    members[[nm]]@predictions |>
      dplyr::filter(.data$.quantile_level %in% quantile_levels) |>
      dplyr::select(dplyr::all_of(c(key, ".quantile_level", ".value"))) |>
      dplyr::mutate(.weight = unname(weights[nm]))
  })

  predictions <- stacked |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(key, ".quantile_level")))) |>
    dplyr::summarise(
      .value = stats::weighted.mean(.data$.value, w = .data$.weight, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c(key, ".quantile_level"))))

  list(predictions = predictions, draws = NULL)
}

#' Weighted linear pool of the members' draws
#'
#' Each member contributes a share of `n_draws` proportional to its weight, so
#' the pooled sample *is* the mixture distribution.
#'
#' @param members A named list of `tbl_nowcast` objects.
#' @param weights A named numeric vector summing to 1.
#' @param key Character vector of the columns identifying a prediction target.
#' @param quantile_levels Requested levels, or `NULL`.
#' @param n_draws Size of the pooled sample.
#'
#' @return A list with `predictions` and `draws`.
#'
#' @keywords internal
#' @noRd
.ensemble_linear_pool <- function(members, weights, key, quantile_levels, n_draws) {
  without_draws <- names(members)[vapply(members, function(m) is.null(m@draws), logical(1))]
  if (length(without_draws) > 0) {
    cli::cli_abort(c(
      "{.code type = \"linear_pool\"} needs draws from every member, but \\
       {.val {without_draws}} {?has/have} none.",
      "i" = "Use {.code type = \"quantile\"} instead."
    ))
  }

  members <- .common_targets(members, key)
  quantile_levels <- .common_quantile_levels(members, quantile_levels)
  # Give each member a whole number of draws that respects its weight, with the
  # rounding remainder going to the heaviest members.
  shares <- .allocate_draws(weights, n_draws)

  pooled <- purrr_map_dfr(names(members), function(nm) {
    take <- shares[[nm]]
    if (take == 0) {
      return(NULL)
    }
    member_draws <- members[[nm]]@draws
    available <- unique(member_draws$.draw)
    chosen <- sample(available, size = take, replace = take > length(available))

    dplyr::tibble(.draw_id = seq_along(chosen), .draw_source = chosen) |>
      # A member can be sampled more than once when it is asked for more draws
      # than it has, so the join is deliberately many-to-many.
      dplyr::inner_join(member_draws,
        by = c(".draw_source" = ".draw"), relationship = "many-to-many"
      ) |>
      dplyr::select(dplyr::all_of(c(key, ".value")), ".draw_id") |>
      dplyr::mutate(.member = nm)
  })

  draws <- pooled |>
    dplyr::mutate(.draw = dplyr::dense_rank(paste(.data$.member, .data$.draw_id))) |>
    dplyr::select(dplyr::all_of(c(key, ".draw", ".value")))

  list(
    predictions = .draws_to_quantiles(draws, key, quantile_levels),
    draws = draws
  )
}

#' Split `n_draws` between members proportionally to their weights
#'
#' @param weights A named numeric vector summing to 1.
#' @param n_draws Total number of draws.
#'
#' @return A named integer vector summing to `n_draws`.
#'
#' @keywords internal
#' @noRd
.allocate_draws <- function(weights, n_draws) {
  exact <- weights * n_draws
  allocated <- floor(exact)
  remaining <- n_draws - sum(allocated)
  if (remaining > 0) {
    order_by_remainder <- order(exact - allocated, decreasing = TRUE)
    allocated[utils::head(order_by_remainder, remaining)] <-
      allocated[utils::head(order_by_remainder, remaining)] + 1
  }
  stats::setNames(as.integer(allocated), names(weights))
}

#' `purrr::map_dfr()` without the purrr dependency
#'
#' @param x A vector to iterate over.
#' @param f A function returning a data frame (or `NULL`).
#'
#' @return A single data frame.
#'
#' @keywords internal
#' @noRd
purrr_map_dfr <- function(x, f) {
  dplyr::bind_rows(lapply(x, f))
}
