# Converters must accept LINELIST input directly, and -- for the targets that
# aggregate into a grid (reporting triangles, preprocessed objects) -- give the
# same answer as handing them the equivalent count-incidence object.
#
# The point is that a user should not have to know to call
# `to_count() |> complete_zeroes()` first. If a converter ever regresses on
# that, these fail.

equivalence_linelist <- function() {
  data(denguedat, envir = environment())
  denguedat |>
    dplyr::filter(
      onset_week  < as.Date("2002-07-22"),
      report_week < as.Date("2002-07-22"),
      onset_week >= as.Date("2001-01-01")
    ) |>
    tbl_now(
      event_date = onset_week, report_date = report_week,
      data_type = "linelist", verbose = FALSE
    )
}

equivalence_pair <- function() {
  linelist <- equivalence_linelist()
  list(linelist = linelist, counts = to_count(linelist, to = "count-incidence"))
}

test_that("baselinenowcast: a linelist matches COMPLETED count-incidence", {
  skip_if_not_installed("baselinenowcast")
  pair <- equivalence_pair()

  from_linelist <- suppressWarnings(
    tbl_now_to_baselinenowcast(pair$linelist, verbose = FALSE)
  )
  # A line list cannot express "observed zero" versus "not observed", so it is
  # completed automatically. The equivalent count object is the COMPLETED one --
  # not the bare one, which may legitimately carry NA cells.
  from_counts <- suppressWarnings(
    tbl_now_to_baselinenowcast(complete_zeroes(pair$counts), verbose = FALSE)
  )

  expect_equal(unclass(from_linelist), unclass(from_counts))
  expect_s3_class(from_linelist, "reporting_triangle")

  # The triangle must reach the `now`: an event week with no reports has no
  # rows, and without completion it would silently stop short.
  expect_equal(
    as.Date(utils::tail(rownames(from_linelist), 1)),
    get_now(pair$linelist)
  )
})

test_that("baselinenowcast leaves COUNT input exactly as supplied", {
  skip_if_not_installed("baselinenowcast")
  pair <- equivalence_pair()

  # Count data can distinguish an observed zero from a cell that could not be
  # observed yet. Completing it silently would claim reporting was complete when
  # it was not, so `complete = "auto"` must NOT touch it.
  auto <- suppressWarnings(tbl_now_to_baselinenowcast(pair$counts, verbose = FALSE))
  off  <- suppressWarnings(
    tbl_now_to_baselinenowcast(pair$counts, complete = FALSE, verbose = FALSE)
  )
  expect_equal(unclass(auto), unclass(off))

  # ... and forcing completion is available when wanted.
  forced <- suppressWarnings(
    tbl_now_to_baselinenowcast(pair$counts, complete = TRUE, verbose = FALSE)
  )
  expect_gte(nrow(forced), nrow(auto))
})

test_that("baselinenowcast: `complete = FALSE` stops at the last observed week", {
  skip_if_not_installed("baselinenowcast")
  linelist <- equivalence_linelist()

  completed <- suppressWarnings(
    tbl_now_to_baselinenowcast(linelist, verbose = FALSE)
  )
  bare <- suppressWarnings(
    tbl_now_to_baselinenowcast(linelist, complete = FALSE, verbose = FALSE)
  )
  expect_lte(nrow(bare), nrow(completed))
})

test_that("epinowcast: a linelist matches COMPLETED count-incidence", {
  skip_if_not_installed("epinowcast")
  pair <- equivalence_pair()
  triangle_of <- function(x) {
    as.data.frame(epinowcast::enw_get_data(x, "reporting_triangle"))
  }
  from_linelist <- suppressWarnings(suppressMessages(
    tbl_now_to_epinowcast(pair$linelist, verbose = FALSE, quiet = TRUE)
  ))
  from_counts <- suppressWarnings(suppressMessages(
    tbl_now_to_epinowcast(pair$counts, verbose = FALSE, quiet = TRUE)
  ))
  expect_equal(triangle_of(from_linelist), triangle_of(from_counts))
})

test_that("baselinenowcast triangle_list: linelist matches count-incidence", {
  skip_if_not_installed("baselinenowcast")
  linelist <- equivalence_linelist() |> add_strata("gender")
  counts <- to_count(linelist, to = "count-incidence")

  from_linelist <- suppressWarnings(
    tbl_now_to_baselinenowcast(linelist, format = "triangle_list", verbose = FALSE)
  )
  from_counts <- suppressWarnings(
    tbl_now_to_baselinenowcast(complete_zeroes(counts), format = "triangle_list",
                               verbose = FALSE)
  )

  expect_equal(names(from_linelist), names(from_counts))
  for (nm in names(from_linelist)) {
    expect_equal(unclass(from_linelist[[nm]]), unclass(from_counts[[nm]]))
  }
})

test_that("epinowcast: linelist and count-incidence give the same reporting triangle", {
  skip_if_not_installed("epinowcast")
  pair <- equivalence_pair()

  from_linelist <- suppressWarnings(suppressMessages(
    tbl_now_to_epinowcast(pair$linelist, verbose = FALSE, quiet = TRUE)
  ))
  from_counts <- suppressWarnings(suppressMessages(
    tbl_now_to_epinowcast(pair$counts, verbose = FALSE, quiet = TRUE)
  ))

  triangle_of <- function(x) {
    as.data.frame(epinowcast::enw_get_data(x, "reporting_triangle"))
  }
  expect_equal(triangle_of(from_linelist), triangle_of(from_counts))
})

test_that("surveillance: a linelist round-trips to one row per case", {
  skip_if_not_installed("surveillance")
  pair <- equivalence_pair()

  from_linelist <- tbl_now_to_surveillance(pair$linelist, verbose = FALSE)
  from_counts <- suppressWarnings(
    tbl_now_to_surveillance(pair$counts, verbose = FALSE)
  )

  # surveillance wants individual cases, so counts are expanded back out. The
  # row COUNT and the delay distribution must match the original line list.
  expect_equal(nrow(from_linelist), nrow(pair$linelist))
  expect_equal(nrow(from_counts), nrow(from_linelist))
  expect_equal(
    sort(as.numeric(from_counts$dReport - from_counts$dHospital)),
    sort(as.numeric(from_linelist$dReport - from_linelist$dHospital))
  )
})


test_that("every converter accepts LINELIST input without erroring", {
  linelist <- equivalence_linelist()

  converters <- list(
    baselinenowcast = function(x) tbl_now_to_baselinenowcast(x, verbose = FALSE),
    epinowcast      = function(x) tbl_now_to_epinowcast(x, verbose = FALSE, quiet = TRUE),
    epidist         = function(x) tbl_now_to_epidist(x, verbose = FALSE),
    surveillance    = function(x) tbl_now_to_surveillance(x, verbose = FALSE),
    data.table      = function(x) tbl_now_to_data_table(x, verbose = FALSE),
    tsibble         = function(x) tbl_now_to_tsibble(x, verbose = FALSE)
  )

  for (nm in names(converters)) {
    skip_if_not_installed(nm)
    expect_no_error(
      suppressWarnings(suppressMessages(converters[[nm]](linelist))),
      message = paste0(nm, " failed on linelist input")
    )
  }
})

test_that("every converter accepts COUNT-INCIDENCE input without erroring", {
  counts <- to_count(equivalence_linelist(), to = "count-incidence")

  converters <- list(
    baselinenowcast = function(x) tbl_now_to_baselinenowcast(x, verbose = FALSE),
    epinowcast      = function(x) tbl_now_to_epinowcast(x, verbose = FALSE, quiet = TRUE),
    epidist         = function(x) tbl_now_to_epidist(x, verbose = FALSE),
    surveillance    = function(x) tbl_now_to_surveillance(x, verbose = FALSE),
    data.table      = function(x) tbl_now_to_data_table(x, verbose = FALSE),
    tsibble         = function(x) tbl_now_to_tsibble(x, verbose = FALSE)
  )

  for (nm in names(converters)) {
    skip_if_not_installed(nm)
    expect_no_error(
      suppressWarnings(suppressMessages(converters[[nm]](counts))),
      message = paste0(nm, " failed on count-incidence input")
    )
  }
})

# ---- the grid must reach the `now`, not the last row (#67 and its audit) ----
#
# `equivalence_pair()` above has a report for every event week, so a converter
# that stops at the last row it was handed still looks right. This fixture does
# not: its last three event days carry no reports at all, which a line list has
# no way to record, so `now` is three days past the last event date present.
#
# `test-engines-linelist-equivalence.R` asserts the same property one level up,
# on the fitted nowcast.

gap_linelist <- function(n_strata = 0L) {
  origin <- as.Date("2021-01-04")
  cells <- tidyr::expand_grid(.period = 0:39, .delay = 0:2)
  if (n_strata > 0) {
    cells <- tidyr::expand_grid(cells, sex = c("F", "M"))
  }
  cells |>
    dplyr::mutate(
      event = origin + .data$.period,
      report = .data$event + .data$.delay,
      n = 3L
    ) |>
    dplyr::filter(.data$.period <= 36) |>
    dplyr::select(dplyr::any_of(c("event", "report", "sex")), "n") |>
    tidyr::uncount(!!as.symbol("n")) |>
    tbl_now(
      event_date = event, report_date = report,
      strata = if (n_strata > 0) "sex" else NULL,
      data_type = "linelist", units = "days", verbose = FALSE
    )
}

gap_pair <- function(n_strata = 0L) {
  linelist <- gap_linelist(n_strata)
  list(
    linelist = linelist,
    counts = suppressWarnings(suppressMessages(
      complete_zeroes(to_count(linelist, to = "count-incidence"))
    ))
  )
}

test_that("baselinenowcast reaches the `now` when the last days are silent", {
  skip_if_not_installed("baselinenowcast")
  pair <- gap_pair()

  from_linelist <- suppressWarnings(suppressMessages(
    tbl_now_to_baselinenowcast(pair$linelist, verbose = FALSE)
  ))
  from_counts <- suppressWarnings(suppressMessages(
    tbl_now_to_baselinenowcast(pair$counts, verbose = FALSE)
  ))

  expect_equal(
    as.Date(utils::tail(rownames(from_linelist), 1)), get_now(pair$linelist)
  )
  expect_equal(unclass(from_linelist), unclass(from_counts))
})

test_that("epinowcast reaches the `now` when the last days are silent", {
  skip_if_not_installed("epinowcast")
  pair <- gap_pair()

  reference_dates <- function(x) {
    sort(unique(as.Date(
      epinowcast::enw_get_data(x, "reporting_triangle")$reference_date
    )))
  }
  from_linelist <- suppressWarnings(suppressMessages(
    tbl_now_to_epinowcast(pair$linelist, verbose = FALSE, quiet = TRUE)
  ))
  from_counts <- suppressWarnings(suppressMessages(
    tbl_now_to_epinowcast(pair$counts, verbose = FALSE, quiet = TRUE)
  ))

  expect_equal(max(reference_dates(from_linelist)), get_now(pair$linelist))
  expect_equal(reference_dates(from_linelist), reference_dates(from_counts))
})

test_that("EpiNow2 series reach the `now` when the last days are silent", {
  skip_if_not_installed("EpiNow2")

  # `.epinow2_series_data()` built its series from `get_latest_reported_cases()`
  # and, on daily data, handed it straight on: the line list stopped two days
  # short of the `now` while the completed counts did not. `complete = "auto"`
  # is what closes that.
  for (target in c("estimate_infections", "regional_epinow")) {
    pair <- gap_pair(n_strata = if (target == "regional_epinow") 1L else 0L)
    from_linelist <- suppressWarnings(suppressMessages(
      tbl_now_to_EpiNow2(pair$linelist, target = target,
                         verbose = FALSE, quiet = TRUE)
    ))
    from_counts <- suppressWarnings(suppressMessages(
      tbl_now_to_EpiNow2(pair$counts, target = target,
                         verbose = FALSE, quiet = TRUE)
    ))

    expect_equal(max(from_linelist$date), get_now(pair$linelist))
    expect_equal(from_linelist, from_counts)
  }
})

test_that("EpiNow2 snapshots reach their own `as_of`", {
  skip_if_not_installed("EpiNow2")

  # `.epinow2_snapshots()` completes each snapshot with `complete_zeroes()`,
  # which REFUSES a line list -- and its `tryCatch()` returned the short
  # snapshot rather than reporting that. `estimate_truncation()` wants "a
  # complete vector of dates" in every snapshot, so this mattered.
  pair <- gap_pair()
  from_linelist <- suppressWarnings(suppressMessages(
    tbl_now_to_EpiNow2(pair$linelist, target = "estimate_truncation",
                       verbose = FALSE, quiet = TRUE)
  ))
  from_counts <- suppressWarnings(suppressMessages(
    tbl_now_to_EpiNow2(pair$counts, target = "estimate_truncation",
                       verbose = FALSE, quiet = TRUE)
  ))

  expect_equal(
    lapply(from_linelist, function(s) s$date),
    lapply(from_counts, function(s) s$date)
  )
  # Each snapshot runs to the report date it was taken on.
  report_dates <- attr(from_linelist, "report_dates")
  for (i in seq_along(from_linelist)) {
    expect_equal(max(from_linelist[[i]]$date), report_dates[[i]])
  }
})

test_that("EpiNow2 leaves COUNT input exactly as supplied", {
  skip_if_not_installed("EpiNow2")

  # Count data can distinguish an observed zero from a cell that could not be
  # observed yet, so `complete = "auto"` must not touch it -- the same contract
  # `tbl_now_to_baselinenowcast()` keeps.
  counts <- to_count(gap_linelist(), to = "count-incidence")
  auto <- suppressWarnings(suppressMessages(
    tbl_now_to_EpiNow2(counts, verbose = FALSE, quiet = TRUE)
  ))
  off <- suppressWarnings(suppressMessages(
    tbl_now_to_EpiNow2(counts, complete = FALSE, verbose = FALSE, quiet = TRUE)
  ))
  expect_equal(auto, off)

  forced <- suppressWarnings(suppressMessages(
    tbl_now_to_EpiNow2(counts, complete = TRUE, verbose = FALSE, quiet = TRUE)
  ))
  expect_gt(nrow(forced), nrow(auto))
  expect_equal(max(forced$date), get_now(counts))
})

test_that("NobBS and surveillance line lists are unchanged by completion", {
  # Both take a LINE LIST, and both are handed the `now` by their fit method
  # (`NobBS(now =)`, `get_surveillance_range()`), so the empty periods are the
  # back-end's business rather than the converter's. What must hold is that
  # completing the counts adds no cases: a completed zero expands to no rows.
  pair <- gap_pair()

  skip_if_not_installed("NobBS")
  nobbs_of <- function(x) {
    suppressWarnings(suppressMessages(tbl_now_to_nobbs(x, verbose = FALSE))) |>
      dplyr::as_tibble() |>
      dplyr::arrange(!!as.symbol("onset_date"), !!as.symbol("report_date"))
  }
  expect_equal(nobbs_of(pair$linelist), nobbs_of(pair$counts))

  skip_if_not_installed("surveillance")
  surveillance_of <- function(x) {
    suppressWarnings(suppressMessages(
      tbl_now_to_surveillance(x, verbose = FALSE)
    )) |>
      dplyr::as_tibble() |>
      dplyr::arrange(!!as.symbol("dHospital"), !!as.symbol("dReport"))
  }
  expect_equal(surveillance_of(pair$linelist), surveillance_of(pair$counts))
})

test_that("epidist gets the same delay distribution either way", {
  skip_if_not_installed("epidist")

  # `epidist` fits a DELAY DISTRIBUTION and has no event grid, so there is
  # nothing to complete -- but the counts must still expand back to the same
  # cases, which is the property that would break if they did not.
  pair <- gap_pair()
  delays_of <- function(x) {
    frame <- suppressWarnings(suppressMessages(
      tbl_now_to_epidist(x, verbose = FALSE)
    ))
    weight <- if ("n" %in% names(frame)) frame$n else rep(1, nrow(frame))
    tapply(weight, frame$sdate_lwr - frame$pdate_lwr, sum)
  }
  expect_equal(delays_of(pair$linelist), delays_of(pair$counts))
})
