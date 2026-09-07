# A line list cannot record a period in which nothing was reported: those rows
# are simply absent. Every back-end that lays the data on a grid must therefore
# complete it ITSELF, or the reference axis stops at the last period that has a
# report -- short of the `now`, which is the period a nowcast is about.
#
# That is #67, in `nowcast_fit.baselinenowcast()`: it built its per-stratum
# triangles from `format = "long"`, which is deliberately never completed, and
# fitted 54 reference times where the completed counts gave 81. The audit that
# followed found the same defect in `tbl_now_to_EpiNow2()`, whose series stopped
# at the last event date carrying a report (and whose `estimate_truncation`
# snapshots silently kept their `complete_zeroes()` failure, because that
# function REFUSES a line list and the `tryCatch()` swallowed the refusal).
#
# The reference object is the COMPLETED count-incidence one. Count data can say
# "observed zero" and a line list cannot, so `to_count() |> complete_zeroes()`
# is the count object carrying the same information -- and the whole point is
# that a user must not have to know to call it.

q <- function(expr) suppressWarnings(suppressMessages(expr))

#' A daily line list whose last three event days carry no reports at all
#'
#' Reports for earlier days still arrive up to day 38, so `now` is day 38 while
#' the last event day present is day 36. Nothing in the line list says the two
#' intervening days exist.
linelist_gap_pair <- function(n_strata = 0L) {
  origin <- as.Date("2021-01-04")
  cells <- tidyr::expand_grid(.period = 0:39, .delay = 0:2)
  if (n_strata > 0) {
    cells <- tidyr::expand_grid(cells, sex = c("F", "M"))
  }
  cells <- cells |>
    dplyr::mutate(
      event = origin + .data$.period,
      report = .data$event + .data$.delay,
      n = 3L
    ) |>
    dplyr::filter(.data$.period <= 36)

  linelist <- cells |>
    dplyr::select(dplyr::any_of(c("event", "report", "sex")), "n") |>
    tidyr::uncount(!!as.symbol("n")) |>
    tbl_now(
      event_date = event, report_date = report,
      strata = if (n_strata > 0) "sex" else NULL,
      data_type = "linelist", units = "days", verbose = FALSE
    )

  list(
    linelist = linelist,
    counts = q(complete_zeroes(to_count(linelist, to = "count-incidence")))
  )
}

test_that("the fixture really does hide the last event days", {
  pair <- linelist_gap_pair()

  # If this ever stops being true the equivalence tests below prove nothing.
  expect_lt(
    max(dplyr::pull(pair$linelist, "event")),
    get_now(pair$linelist)
  )
  expect_equal(
    max(dplyr::pull(pair$counts, "event")),
    get_now(pair$linelist)
  )
})

# One test per engine rather than a loop, so a failure names the engine and one
# broken back-end does not hide the others.
for (this_engine in available_engines()) {
  local({
    engine_name <- this_engine

    test_that(paste0(engine_name, ": a linelist nowcast matches completed counts"), {
      skip_on_cran()
      skip_if_not_installed(engine_name)

      pair <- linelist_gap_pair()
      spec <- do.call(engine, c(list(engine_name), utils::modifyList(
        engine_args(engine_name, pair$linelist),
        engine_dry_args(engine_name)
      )))

      set.seed(4242)
      from_linelist <- q(run_nowcast(pair$linelist, spec, verbose = FALSE))
      set.seed(4242)
      from_counts <- q(run_nowcast(pair$counts, spec, verbose = FALSE))

      tidy_linelist <- generics::tidy(from_linelist)
      tidy_counts <- generics::tidy(from_counts)

      # The axis reaches the `now`: the days the line list could not mention are
      # back. `NobBS` and `epinowcast` report a window rather than the whole
      # series, so it is the LAST date that matters, not the count of them.
      expect_equal(max(tidy_linelist$event_date), get_now(pair$linelist))

      # Same object, same seed, same nowcast.
      expect_equal(
        as.data.frame(tidy_linelist),
        as.data.frame(tidy_counts)
      )
    })
  })
}

test_that("baselinenowcast: a STRATIFIED linelist matches completed counts (#67)", {
  skip_on_cran()
  skip_if_not_installed("baselinenowcast")

  # The stratified path is the one #67 broke, and it broke it in the fit method
  # rather than in the converter, so an unstratified test would have passed
  # throughout.
  pair <- linelist_gap_pair(n_strata = 1L)

  set.seed(4242)
  from_linelist <- q(run_nowcast(
    pair$linelist, engine_baselinenowcast(draws = 25), verbose = FALSE
  ))
  set.seed(4242)
  from_counts <- q(run_nowcast(
    pair$counts, engine_baselinenowcast(draws = 25), verbose = FALSE
  ))

  tidy_linelist <- generics::tidy(from_linelist)
  expect_setequal(unique(tidy_linelist$stratum), c("F", "M"))
  expect_equal(max(tidy_linelist$event_date), get_now(pair$linelist))
  expect_equal(
    as.data.frame(tidy_linelist),
    as.data.frame(generics::tidy(from_counts))
  )
})
