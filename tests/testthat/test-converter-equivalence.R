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

test_that("nowcaster: a linelist round-trips to one row per case", {
  skip_if_not_installed("nowcaster")
  pair <- equivalence_pair()

  from_linelist <- tbl_now_to_nowcaster(pair$linelist, verbose = FALSE)
  from_counts <- suppressWarnings(
    tbl_now_to_nowcaster(pair$counts, verbose = FALSE)
  )

  expect_equal(nrow(from_linelist), nrow(pair$linelist))
  expect_equal(nrow(from_counts), nrow(from_linelist))
  expect_equal(
    sort(as.numeric(from_counts$date_report - from_counts$date_onset)),
    sort(as.numeric(from_linelist$date_report - from_linelist$date_onset))
  )
})

test_that("every converter accepts LINELIST input without erroring", {
  linelist <- equivalence_linelist()

  converters <- list(
    baselinenowcast = function(x) tbl_now_to_baselinenowcast(x, verbose = FALSE),
    epinowcast      = function(x) tbl_now_to_epinowcast(x, verbose = FALSE, quiet = TRUE),
    epidist         = function(x) tbl_now_to_epidist(x, verbose = FALSE),
    surveillance    = function(x) tbl_now_to_surveillance(x, verbose = FALSE),
    nowcaster       = function(x) tbl_now_to_nowcaster(x, verbose = FALSE),
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
    nowcaster       = function(x) tbl_now_to_nowcaster(x, verbose = FALSE),
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
