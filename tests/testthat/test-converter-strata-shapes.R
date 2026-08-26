# Strata: none / one / several, and a level with no rows.
#
# `test-converters.R` covers ONE stratifying column whose levels happen to sit
# in alphabetical order in the data (`denguedat$gender`). Two things only go
# wrong outside that case:
#
#   * `split()` orders its groups alphabetically, while the data is in data
#     order. If the labels come from one and the values from the other, two
#     strata swap without any error -- every total is still a real total, just
#     attached to the wrong stratum.
#   * with SEVERAL stratifying columns a single label has to encode a tuple, and
#     the label has to survive the trip back.
#
# A factor level with no rows is included because it is the natural way to get
# an empty group out of `split()`.

q <- function(expr) suppressWarnings(suppressMessages(expr))

# Two stratifying columns, deliberately awkward:
#   * `g1` is a FACTOR whose data order ("zulu" first) is the reverse of its
#     alphabetical order, and which carries a third level, "never", with no rows;
#   * `g2` is a character column, also reversed ("y" before "x").
# The counts are powers of ten per group, so a mispaired label is arithmetically
# unmistakable.
awkward_strata_tbl_now <- function() {
  d <- data.frame(
    ev = as.Date("2024-01-01") + rep(c(0, 0, 1, 1, 2, 2), 2),
    rp = as.Date("2024-01-01") + rep(c(0, 1, 1, 2, 2, 3), 2),
    g1 = factor(
      rep(c("zulu", "alpha"), each = 6),
      levels = c("alpha", "zulu", "never")
    ),
    g2 = rep(c("y", "x"), 6),
    n  = c(10, 1, 20, 2, 30, 3, 100, 4, 200, 5, 300, 6)
  )
  tbl_now(
    d,
    event_date = "ev", report_date = "rp", case_count = "n",
    strata = c("g1", "g2"), data_type = "count-incidence", verbose = FALSE
  )
}

# The truth every test below compares against: 15 / 600 / 6 / 60.
awkward_strata_totals <- function() {
  data.frame(
    label = c("alpha | x", "alpha | y", "zulu | x", "zulu | y"),
    total = c(15, 600, 6, 60),
    stringsAsFactors = FALSE
  )
}

# --- triangle_list: labels must stay attached to their own counts -------------

test_that("triangle_list pairs each label with its own stratum's counts", {
  skip_if_not_installed("baselinenowcast")
  x  <- awkward_strata_tbl_now()
  tl <- q(tbl_now_to_baselinenowcast(x, format = "triangle_list",
                                     verbose = FALSE))
  truth <- awkward_strata_totals()

  expect_named(tl, truth$label)
  # The decisive assertion: the total inside each triangle must match the label
  # printed on it, not merely be one of the four totals.
  totals <- vapply(tl, function(tri) sum(tri, na.rm = TRUE), numeric(1))
  expect_equal(unname(totals[truth$label]), truth$total)
})

test_that("triangle_list keeps strata VALUES, not just the pasted label", {
  skip_if_not_installed("baselinenowcast")
  x  <- awkward_strata_tbl_now()
  tl <- q(tbl_now_to_baselinenowcast(x, format = "triangle_list",
                                     verbose = FALSE))
  values <- attr(tl, "strata_values")

  expect_equal(attr(tl, "strata_cols"), c("g1", "g2"))
  expect_equal(names(values), c("g1", "g2"))
  expect_equal(nrow(values), length(tl))
  # Row i of `strata_values` describes element i of the list -- the property the
  # separator-in-a-stratum-name hazard depends on.
  expect_equal(
    do.call(paste, c(unname(as.list(values)), sep = " | ")),
    names(tl)
  )
})

test_that("a factor level with no rows produces no triangle", {
  skip_if_not_installed("baselinenowcast")
  x  <- awkward_strata_tbl_now()
  tl <- q(tbl_now_to_baselinenowcast(x, format = "triangle_list",
                                     verbose = FALSE))

  # "never" is a declared level of `g1` with no observations. It must not become
  # an empty triangle, and it must not become an `NA` label either.
  expect_equal(length(tl), 4L)
  expect_false(any(grepl("never", names(tl))))
  expect_false(anyNA(attr(tl, "strata_values")$g1))
})

test_that("the triangle_list round trip preserves every stratum's total", {
  skip_if_not_installed("baselinenowcast")
  x    <- awkward_strata_tbl_now()
  tl   <- q(tbl_now_to_baselinenowcast(x, format = "triangle_list",
                                       verbose = FALSE))
  back <- q(as_tbl_now(tl))

  expect_equal(get_strata(back), c("g1", "g2"))
  rebuilt <- back |>
    dplyr::as_tibble() |>
    dplyr::summarise(
      total = sum(.data$count, na.rm = TRUE),
      .by = c("g1", "g2")
    ) |>
    dplyr::arrange(.data$g1, .data$g2)
  expect_equal(
    paste(rebuilt$g1, rebuilt$g2, sep = " | "),
    awkward_strata_totals()$label
  )
  expect_equal(rebuilt$total, awkward_strata_totals()$total)
})

# --- pooling for the single matrix -------------------------------------------

test_that("pooling several strata into one matrix keeps the grand total", {
  skip_if_not_installed("baselinenowcast")
  x <- awkward_strata_tbl_now()
  expect_warning(
    mat <- suppressMessages(
      tbl_now_to_baselinenowcast(x, format = "matrix", verbose = FALSE)
    ),
    "pooling over strata"
  )
  # Pooling is lossy in the strata dimension and ONLY in that dimension.
  expect_equal(sum(mat, na.rm = TRUE), sum(awkward_strata_totals()$total))
})

# --- the other back-ends carry both columns ----------------------------------

test_that("every column back-end carries both stratifying columns", {
  x <- awkward_strata_tbl_now()
  total_cases <- sum(awkward_strata_totals()$total)

  # epidist / tsibble / data.table keep counts; NobBS and surveillance count
  # rows, so they must come back with one row per case.
  expectations <- list(
    tbl_now_to_epidist      = list(pkg = "epidist",      rows = NULL),
    tbl_now_to_tsibble      = list(pkg = "tsibble",      rows = NULL),
    tbl_now_to_data_table   = list(pkg = "data.table",   rows = NULL),
    tbl_now_to_nobbs        = list(pkg = "NobBS",        rows = total_cases),
    tbl_now_to_surveillance = list(pkg = "surveillance", rows = total_cases)
  )

  for (fn in names(expectations)) {
    spec <- expectations[[fn]]
    skip_if_not_installed(spec$pkg)
    out <- q(get(fn)(x, verbose = FALSE))
    expect_true(all(c("g1", "g2") %in% names(out)), info = fn)
    if (!is.null(spec$rows)) expect_equal(nrow(out), spec$rows, info = fn)
  }
})

# --- the ONE-column back-ends -------------------------------------------------
#
# Carrying the columns along is not enough for a back-end whose stratified entry
# point names a SINGLE column: `NobBS::NobBS.strat(strata = )` takes one name,
# `EpiNow2::regional_epinow()` takes one `region`, and `surveillance::nowcast()`
# takes none at all, so a stratified fit there means splitting the line list.
# For those three the converter has to hand back the pasted column itself --
# otherwise there is no argument the user can write.
#
# This is the case the test above does NOT cover, and did not catch.

test_that("tbl_now_to_nobbs emits the single column NobBS.strat takes", {
  skip_if_not_installed("NobBS")
  x  <- awkward_strata_tbl_now()
  nb <- q(tbl_now_to_nobbs(x, verbose = FALSE))

  expect_true("strata" %in% names(nb))
  # One label per observed combination -- and the empty "never" level is not one.
  expect_setequal(unique(nb$strata), awkward_strata_totals()$label)
  # The label describes its OWN row, which is what stops two strata swapping.
  expect_equal(nb$strata, paste(nb$g1, nb$g2, sep = " | "))
})

test_that("tbl_now_to_surveillance emits a column to split the line list on", {
  skip_if_not_installed("surveillance")
  x   <- awkward_strata_tbl_now()
  sur <- q(tbl_now_to_surveillance(x, verbose = FALSE))

  expect_true("strata" %in% names(sur))
  expect_equal(sur$strata, paste(sur$g1, sur$g2, sep = " | "))
  # Splitting on it must reproduce each stratum's own case total.
  totals <- vapply(split(sur, sur$strata), nrow, integer(1))
  truth  <- awkward_strata_totals()
  expect_equal(unname(totals[truth$label]), truth$total)
})

test_that("tbl_now_to_EpiNow2 emits the single `region` regional_epinow takes", {
  skip_if_not_installed("EpiNow2")
  x   <- awkward_strata_tbl_now()
  reg <- q(tbl_now_to_EpiNow2(x, target = "regional_epinow",
                              verbose = FALSE, quiet = TRUE))

  expect_true("region" %in% names(reg))
  expect_setequal(unique(reg$region), awkward_strata_totals()$label)
  totals <- vapply(split(reg$confirm, reg$region), sum, numeric(1))
  truth  <- awkward_strata_totals()
  expect_equal(unname(totals[truth$label]), truth$total)
})

test_that("the pasted separator is user-chosen", {
  skip_if_not_installed("NobBS")
  skip_if_not_installed("surveillance")
  x <- awkward_strata_tbl_now()

  nb <- q(tbl_now_to_nobbs(x, strata_sep = "/", verbose = FALSE))
  expect_setequal(unique(nb$strata), c("alpha/x", "alpha/y", "zulu/x", "zulu/y"))

  sur <- q(tbl_now_to_surveillance(x, strata_col = "who", strata_sep = "_",
                                   verbose = FALSE))
  expect_true("who" %in% names(sur))
  expect_false("strata" %in% names(sur))

  # `NULL` opts out entirely: a caller who only wants the original columns.
  expect_false("strata" %in% names(
    q(tbl_now_to_nobbs(x, strata_col = NULL, verbose = FALSE))
  ))
})

test_that("a stratum value holding the separator aborts rather than mispastes", {
  skip_if_not_installed("NobBS")
  d <- data.frame(
    ev = as.Date("2024-01-01") + c(0, 1),
    rp = as.Date("2024-01-02") + c(0, 1),
    g1 = c("a | b", "c"),
    g2 = c("x", "y")
  )
  x <- tbl_now(d, event_date = "ev", report_date = "rp",
               strata = c("g1", "g2"), verbose = FALSE)

  # "a | b" + "x" and "a" + "b | x" paste to the same label. Splitting it back
  # apart in `tidy()` would attach the estimate to the wrong stratum, so this
  # has to fail loudly -- and say which argument fixes it.
  expect_error(q(tbl_now_to_nobbs(x, verbose = FALSE)), "strata_sep")
  expect_setequal(
    unique(q(tbl_now_to_nobbs(x, strata_sep = "@@", verbose = FALSE))$strata),
    c("a | b@@x", "c@@y")
  )
})

test_that("the pasted column refuses to overwrite an existing one", {
  skip_if_not_installed("NobBS")
  d <- data.frame(
    ev     = as.Date("2024-01-01") + c(0, 1),
    rp     = as.Date("2024-01-02") + c(0, 1),
    g1     = c("a", "c"),
    strata = c("keep", "me")
  )
  x <- tbl_now(d, event_date = "ev", report_date = "rp",
               strata = "g1", covariates = "strata", verbose = FALSE)

  expect_error(q(tbl_now_to_nobbs(x, verbose = FALSE)), "already a column")
  # Naming it something else is the documented way out.
  out <- q(tbl_now_to_nobbs(x, strata_col = ".stratum", verbose = FALSE))
  expect_equal(out$strata, c("keep", "me"))
  expect_equal(out$.stratum, c("a", "c"))
})

test_that("a single stratum still gets the pasted column, as its own values", {
  skip_if_not_installed("NobBS")
  d <- data.frame(
    ev = as.Date("2024-01-01") + c(0, 1),
    rp = as.Date("2024-01-02") + c(0, 1),
    g1 = factor(c("zulu", "alpha"), levels = c("alpha", "zulu"))
  )
  x <- tbl_now(d, event_date = "ev", report_date = "rp", strata = "g1",
               verbose = FALSE)
  nb <- q(tbl_now_to_nobbs(x, verbose = FALSE))

  # Data order, not factor order: row 1 is "zulu".
  expect_equal(nb$strata, c("zulu", "alpha"))
})

# --- the MANY-column back-ends ------------------------------------------------
#
# The mirror image: these accept a vector of columns, so pasting would be the
# mistake. This pins that they get the columns THEMSELVES.

test_that("the many-column back-ends take the columns, not a pasted label", {
  x <- awkward_strata_tbl_now()

  # epidist keeps its strata as ordinary columns for the model formula.
  if (requireNamespace("epidist", quietly = TRUE)) {
    ed <- q(tbl_now_to_epidist(x, verbose = FALSE))
    expect_true(all(c("g1", "g2") %in% names(ed)))
    expect_false("strata" %in% names(ed))
  }

  # tsibble puts every stratum in the key.
  if (requireNamespace("tsibble", quietly = TRUE)) {
    ts <- q(tbl_now_to_tsibble(x, verbose = FALSE))
    expect_true(all(c("g1", "g2") %in% tsibble::key_vars(ts)))
    expect_false("strata" %in% names(ts))
  }

  # data.table is a straight hand-off.
  if (requireNamespace("data.table", quietly = TRUE)) {
    dt <- q(tbl_now_to_data_table(x, verbose = FALSE))
    expect_true(all(c("g1", "g2") %in% names(dt)))
    expect_false("strata" %in% names(dt))
  }
})


test_that("epinowcast gets one group per observed strata combination", {
  skip_on_cran()
  skip_if_not_installed("epinowcast")
  x   <- awkward_strata_tbl_now()
  enw <- q(tbl_now_to_epinowcast(x, verbose = FALSE, quiet = TRUE))

  expect_equal(unlist(enw$by), c("g1", "g2"))
  # Four observed combinations; the empty "never" level must not add a fifth.
  expect_equal(length(unique(enw$metareference[[1]]$.group)), 4L)
})

test_that("a tsibble key round-trips several strata", {
  skip_if_not_installed("tsibble")
  x  <- awkward_strata_tbl_now()
  ts <- q(tbl_now_to_tsibble(x, verbose = FALSE))

  # The report date plus every stratum make up the key; the event date is the
  # index. That is what keeps index/key unique.
  expect_equal(tsibble::index_var(ts), "ev")
  expect_setequal(tsibble::key_vars(ts), c("rp", "g1", "g2"))

  back <- q(tbl_now_from_tsibble(ts, report_date = "rp", verbose = FALSE))
  expect_setequal(get_strata(back), c("g1", "g2"))
  expect_equal(sum(back$n), sum(awkward_strata_totals()$total))
})

# --- the label helper `tidy()` uses ------------------------------------------

test_that(".epinowcast_stratum pastes several `by` columns and ignores others", {
  # `summary()` carries columns that are NOT in `by` (`location` on the shipped
  # example). Only the `by` columns may become the label.
  summary_table <- data.frame(
    g1       = c("alpha", "zulu"),
    g2       = c("x", "y"),
    location = "DE",
    stringsAsFactors = FALSE
  )
  expect_equal(
    tbl.now:::.epinowcast_stratum(summary_table, list(by = list(c("g1", "g2")))),
    c("alpha | x", "zulu | y")
  )
  # No grouping at all -> one label per row, all "all".
  expect_equal(
    tbl.now:::.epinowcast_stratum(summary_table, list(by = list(NULL))),
    c("all", "all")
  )
})
