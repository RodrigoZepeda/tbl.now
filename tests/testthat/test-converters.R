library(dplyr, quietly = TRUE, warn.conflicts = FALSE)

# ============================================================
# epinowcast
# ============================================================

test_that("tbl_now_from_epinowcast builds a count-cumulative tbl_now", {
  skip_if_not_installed("epinowcast")

  obs <- head(epinowcast::germany_covid19_hosp, 300)
  res <- tbl_now_from_epinowcast(obs, strata = c("location", "age_group"),
                                 verbose = FALSE)

  expect_true(is_tbl_now(res))
  expect_equal(get_data_type(res), "count-cumulative")
  expect_equal(get_event_date(res), "reference_date")
  expect_equal(get_report_date(res), "report_date")
  expect_equal(get_case_count(res), "confirm")
  expect_setequal(get_strata(res), c("location", "age_group"))
})

test_that("tbl_now_from_epinowcast auto-detects strata", {
  skip_if_not_installed("epinowcast")

  obs <- head(epinowcast::germany_covid19_hosp, 100)
  res <- tbl_now_from_epinowcast(obs, verbose = FALSE)
  # location + age_group are the non-core columns
  expect_setequal(get_strata(res), c("location", "age_group"))
})

test_that("tbl_now_from_epinowcast errors on missing columns", {
  skip_if_not_installed("epinowcast")
  bad <- data.frame(a = 1, b = 2)
  expect_error(tbl_now_from_epinowcast(bad, verbose = FALSE), "not found")
})

test_that("tbl_now_from_epinowcast forwards ... to tbl_now (now)", {
  skip_if_not_installed("epinowcast")
  obs <- head(epinowcast::germany_covid19_hosp, 100)
  fixed_now <- as.Date("2021-04-20")
  res <- suppressWarnings(
    tbl_now_from_epinowcast(obs, verbose = FALSE, now = fixed_now)
  )
  expect_equal(get_now(res), fixed_now)
})

test_that("tbl_now_to_epinowcast returns an enw_preprocess_data object", {
  skip_if_not_installed("epinowcast")

  obs <- head(epinowcast::germany_covid19_hosp, 200)
  nowobj <- tbl_now_from_epinowcast(obs, strata = c("location", "age_group"),
                                    verbose = FALSE)
  ep <- tbl_now_to_epinowcast(nowobj, verbose = FALSE)
  expect_s3_class(ep, "enw_preprocess_data")
})

test_that("tbl_now_to_epinowcast preprocess=FALSE returns completed data.table", {
  skip_if_not_installed("epinowcast")

  obs <- head(epinowcast::germany_covid19_hosp, 200)
  nowobj <- tbl_now_from_epinowcast(obs, strata = c("location", "age_group"),
                                    verbose = FALSE)
  comp <- tbl_now_to_epinowcast(nowobj, preprocess = FALSE, verbose = FALSE)
  expect_s3_class(comp, "data.table")
  expect_true(all(c("reference_date", "report_date", "confirm") %in% names(comp)))
})

test_that("tbl_now_to_epinowcast errors on non-tbl_now", {
  skip_if_not_installed("epinowcast")
  expect_error(tbl_now_to_epinowcast(data.frame(a = 1)), "tbl_now")
})

# ============================================================
# baselinenowcast
# ============================================================

test_that("tbl_now_from_baselinenowcast expands a reporting-triangle matrix", {
  skip_if_not_installed("baselinenowcast")

  rt  <- baselinenowcast::example_reporting_triangle
  res <- tbl_now_from_baselinenowcast(rt, verbose = FALSE)

  expect_true(is_tbl_now(res))
  expect_equal(get_data_type(res), "count-incidence")
  expect_equal(get_case_count(res), "count")
  # NAs in the triangle are dropped → fewer than nrow*ncol rows
  expect_true(nrow(res) <= prod(dim(rt)))
  expect_true(all(res[["count"]] >= 0))
})

test_that("tbl_now_from_baselinenowcast reads the long data.frame", {
  skip_if_not_installed("baselinenowcast")

  res <- tbl_now_from_baselinenowcast(head(baselinenowcast::syn_nssp_df, 400),
                                      verbose = FALSE)
  expect_true(is_tbl_now(res))
  expect_equal(get_data_type(res), "count-incidence")
})

test_that("tbl_now_to_baselinenowcast long returns the expected columns", {
  skip_if_not_installed("baselinenowcast")

  res <- tbl_now_from_baselinenowcast(head(baselinenowcast::syn_nssp_df, 400),
                                      verbose = FALSE)
  long <- tbl_now_to_baselinenowcast(res, format = "long", verbose = FALSE)
  expect_s3_class(long, "data.frame")
  expect_equal(names(long), c("reference_date", "report_date", "count"))
})

test_that("tbl_now_to_baselinenowcast matrix returns a reporting_triangle", {
  skip_if_not_installed("baselinenowcast")

  res <- tbl_now_from_baselinenowcast(head(baselinenowcast::syn_nssp_df, 400),
                                      verbose = FALSE)
  mx <- suppressMessages(
    tbl_now_to_baselinenowcast(res, format = "matrix", verbose = FALSE)
  )
  expect_s3_class(mx, "reporting_triangle")
  expect_true(is.matrix(mx))
})

test_that("baselinenowcast long round-trip preserves counts", {
  skip_if_not_installed("baselinenowcast")

  orig <- head(baselinenowcast::syn_nssp_df, 400)
  res  <- tbl_now_from_baselinenowcast(orig, verbose = FALSE)
  long <- tbl_now_to_baselinenowcast(res, format = "long", verbose = FALSE)
  expect_equal(sum(long$count), sum(orig$count))
})

# ============================================================
# EpiNow2 (to only)
# ============================================================

test_that("tbl_now_to_EpiNow2 collapses count data to a date/confirm series", {
  skip_if_not_installed("data.table")

  data(mpoxdat)
  nowobj <- tbl_now(mpoxdat, event_date = "dx_date", report_date = "dx_report_date",
                    case_count = "n", strata = "race",
                    data_type = "count-incidence", verbose = FALSE)
  series <- tbl_now_to_EpiNow2(nowobj, verbose = FALSE)

  expect_s3_class(series, "data.table")
  expect_equal(names(series), c("date", "confirm"))
  # one row per unique event date
  expect_equal(nrow(series), dplyr::n_distinct(mpoxdat$dx_date))
  # dates sorted ascending
  expect_false(is.unsorted(series$date))
})

test_that("tbl_now_to_EpiNow2 works on linelist data", {
  skip_if_not_installed("data.table")

  data(denguedat)
  nowobj <- tbl_now(denguedat, event_date = "onset_week", report_date = "report_week",
                    verbose = FALSE)
  series <- tbl_now_to_EpiNow2(nowobj, verbose = FALSE)
  expect_equal(names(series), c("date", "confirm"))
  expect_equal(sum(series$confirm), nrow(denguedat))
})

test_that("tbl_now_to_EpiNow2 errors on non-tbl_now", {
  skip_if_not_installed("data.table")
  expect_error(tbl_now_to_EpiNow2(data.frame(a = 1)), "tbl_now")
})

# ============================================================
# data.table
# ============================================================

test_that("tbl_now_from_data_table builds a tbl_now", {
  skip_if_not_installed("data.table")

  data(denguedat)
  dt  <- data.table::as.data.table(denguedat)
  res <- tbl_now_from_data_table(dt, event_date = "onset_week",
                                 report_date = "report_week", verbose = FALSE)
  expect_true(is_tbl_now(res))
  expect_equal(get_event_date(res), "onset_week")
})

test_that("tbl_now_to_data_table returns a data.table and drops tbl_now class", {
  skip_if_not_installed("data.table")

  data(denguedat)
  nowobj <- tbl_now(denguedat, event_date = "onset_week", report_date = "report_week",
                    verbose = FALSE)
  dt <- tbl_now_to_data_table(nowobj, verbose = FALSE)
  expect_s3_class(dt, "data.table")
  expect_false(is_tbl_now(dt))
})

test_that("data.table round-trip preserves row count", {
  skip_if_not_installed("data.table")

  data(denguedat)
  dt   <- data.table::as.data.table(denguedat)
  res  <- tbl_now_from_data_table(dt, event_date = "onset_week",
                                  report_date = "report_week", verbose = FALSE)
  back <- tbl_now_to_data_table(res, verbose = FALSE)
  expect_equal(nrow(back), nrow(denguedat))
})

test_that("tbl_now_from_data_table forwards ... (strata) to tbl_now", {
  skip_if_not_installed("data.table")
  data(denguedat)
  dt  <- data.table::as.data.table(denguedat)
  res <- tbl_now_from_data_table(dt, event_date = "onset_week",
                                 report_date = "report_week",
                                 strata = "gender", verbose = FALSE)
  expect_equal(get_strata(res), "gender")
})

# ============================================================
# epidist
# ============================================================

test_that("tbl_now_from_epidist linelist maps lower bounds to dates", {
  ll <- data.frame(
    pdate_lwr = as.Date(c("2020-03-01", "2020-03-02", "2020-03-03")),
    sdate_lwr = as.Date(c("2020-03-05", "2020-03-04", "2020-03-08"))
  )
  res <- tbl_now_from_epidist(ll, event_units = "days", report_units = "days",
                              verbose = FALSE)
  expect_true(is_tbl_now(res))
  expect_equal(get_data_type(res), "linelist")
  expect_equal(get_event_date(res), "pdate_lwr")
  expect_equal(get_report_date(res), "sdate_lwr")
})

test_that("tbl_now_from_epidist interval attaches upper bounds as covariates + warns", {
  iv <- data.frame(
    pdate_lwr = as.Date(c("2020-03-01", "2020-03-02")),
    pdate_upr = as.Date(c("2020-03-02", "2020-03-03")),
    sdate_lwr = as.Date(c("2020-03-05", "2020-03-04")),
    sdate_upr = as.Date(c("2020-03-06", "2020-03-05"))
  )
  expect_warning(
    res <- tbl_now_from_epidist(iv, format = "interval",
                                event_units = "days", report_units = "days",
                                verbose = FALSE),
    "Interval-censored"
  )
  expect_setequal(get_covariates(res), c("pdate_upr", "sdate_upr"))
})

test_that("tbl_now_from_epidist errors on missing columns", {
  expect_error(
    tbl_now_from_epidist(data.frame(a = 1), verbose = FALSE),
    "not found"
  )
})

test_that("tbl_now_to_epidist linelist builds an epidist_linelist_data object", {
  skip_if_not_installed("epidist")

  ll <- data.frame(
    pdate_lwr = as.Date(c("2020-03-01", "2020-03-02", "2020-03-03")),
    sdate_lwr = as.Date(c("2020-03-05", "2020-03-04", "2020-03-08"))
  )
  res <- tbl_now_from_epidist(ll, event_units = "days", report_units = "days",
                              verbose = FALSE)
  out <- suppressMessages(tbl_now_to_epidist(res, format = "linelist", verbose = FALSE))
  expect_true(epidist::is_epidist_linelist_data(out))
})

test_that("tbl_now_to_epidist interval requires upper-bound columns", {
  skip_if_not_installed("epidist")

  ll <- data.frame(
    pdate_lwr = as.Date(c("2020-03-01", "2020-03-02")),
    sdate_lwr = as.Date(c("2020-03-05", "2020-03-04"))
  )
  res <- tbl_now_from_epidist(ll, event_units = "days", report_units = "days",
                              verbose = FALSE)
  expect_error(
    tbl_now_to_epidist(res, format = "interval", verbose = FALSE),
    "primary_upper"
  )
})

test_that("tbl_now_to_epidist interval round-trips via covariates", {
  skip_if_not_installed("epidist")

  iv <- data.frame(
    pdate_lwr = as.Date(c("2020-03-01", "2020-03-02")),
    pdate_upr = as.Date(c("2020-03-02", "2020-03-03")),
    sdate_lwr = as.Date(c("2020-03-05", "2020-03-04")),
    sdate_upr = as.Date(c("2020-03-06", "2020-03-05"))
  )
  res <- suppressWarnings(
    tbl_now_from_epidist(iv, format = "interval",
                         event_units = "days", report_units = "days", verbose = FALSE)
  )
  out <- suppressWarnings(suppressMessages(
    tbl_now_to_epidist(res, format = "interval",
                       primary_upper = "pdate_upr", secondary_upper = "sdate_upr",
                       verbose = FALSE)
  ))
  expect_true(epidist::is_epidist_linelist_data(out))
})

test_that("tbl_now_to_epidist errors on non-tbl_now", {
  skip_if_not_installed("epidist")
  expect_error(tbl_now_to_epidist(data.frame(a = 1)), "tbl_now")
})

# ============================================================
# verbose messaging
# ============================================================

test_that("from_* verbose prints a conversion summary", {
  ll <- data.frame(
    pdate_lwr = as.Date(c("2020-03-01", "2020-03-02", "2020-03-03")),
    sdate_lwr = as.Date(c("2020-03-05", "2020-03-04", "2020-03-08"))
  )
  expect_message(
    tbl_now_from_epidist(ll, event_units = "days", report_units = "days",
                         verbose = TRUE),
    "tbl_now"
  )
})

# ============================================================
# tsibble
# ============================================================

test_that("tbl_now_to_tsibble builds a tsibble with the chosen index and key", {
  skip_if_not_installed("tsibble")

  data(denguedat)
  nowobj <- tbl_now(denguedat, event_date = "onset_week", report_date = "report_week",
                    strata = "gender", verbose = FALSE)
  ts <- suppressWarnings(tbl_now_to_tsibble(nowobj, verbose = FALSE))

  expect_s3_class(ts, "tbl_ts")
  expect_equal(tsibble::index_var(ts), "report_week")
  expect_setequal(tsibble::key_vars(ts), c("onset_week", "gender"))
})

test_that("tbl_now_to_tsibble index = event_date uses event_date as index", {
  skip_if_not_installed("tsibble")

  data(denguedat)
  nowobj <- tbl_now(denguedat, event_date = "onset_week", report_date = "report_week",
                    verbose = FALSE)
  ts <- suppressWarnings(tbl_now_to_tsibble(nowobj, index = "event_date", verbose = FALSE))
  expect_equal(tsibble::index_var(ts), "onset_week")
})

test_that("tbl_now_from_tsibble rebuilds a tbl_now and recovers strata", {
  skip_if_not_installed("tsibble")

  data(denguedat)
  nowobj <- tbl_now(denguedat, event_date = "onset_week", report_date = "report_week",
                    strata = "gender", verbose = FALSE)
  ts   <- suppressWarnings(tbl_now_to_tsibble(nowobj, verbose = FALSE))
  back <- tbl_now_from_tsibble(ts, event_date = "onset_week", verbose = FALSE)

  expect_true(is_tbl_now(back))
  expect_equal(get_event_date(back), "onset_week")
  expect_equal(get_report_date(back), "report_week")   # from the tsibble index
  expect_equal(get_strata(back), "gender")
})

test_that("tbl_now_from_tsibble errors without event_date", {
  skip_if_not_installed("tsibble")
  data(denguedat)
  nowobj <- tbl_now(denguedat, event_date = "onset_week", report_date = "report_week",
                    verbose = FALSE)
  ts <- suppressWarnings(tbl_now_to_tsibble(nowobj, verbose = FALSE))
  expect_error(tbl_now_from_tsibble(ts), "event_date")
})

test_that("tbl_now_to_tsibble errors on non-tbl_now", {
  skip_if_not_installed("tsibble")
  expect_error(tbl_now_to_tsibble(data.frame(a = 1)), "tbl_now")
})

# ============================================================
# as_tbl_now methods for tbl_now_to_* output classes
# ============================================================

test_that("as_tbl_now.tbl_ts round-trips a tsibble", {
  skip_if_not_installed("tsibble")

  data(denguedat)
  nowobj <- tbl_now(denguedat, event_date = "onset_week", report_date = "report_week",
                    strata = "gender", verbose = FALSE)
  ts   <- suppressWarnings(tbl_now_to_tsibble(nowobj, verbose = FALSE))
  back <- as_tbl_now(ts, event_date = "onset_week", verbose = FALSE)

  expect_true(is_tbl_now(back))
  expect_equal(get_strata(back), "gender")
})

test_that("as_tbl_now.tbl_ts errors without event_date", {
  skip_if_not_installed("tsibble")
  data(denguedat)
  nowobj <- tbl_now(denguedat, event_date = "onset_week", report_date = "report_week",
                    verbose = FALSE)
  ts <- suppressWarnings(tbl_now_to_tsibble(nowobj, verbose = FALSE))
  expect_error(as_tbl_now(ts), "event_date")
})

test_that("as_tbl_now.enw_preprocess_data round-trips epinowcast", {
  skip_if_not_installed("epinowcast")

  obs    <- head(epinowcast::germany_covid19_hosp, 300)
  nowobj <- tbl_now_from_epinowcast(obs, strata = c("location", "age_group"),
                                    verbose = FALSE)
  enw    <- tbl_now_to_epinowcast(nowobj, verbose = FALSE)
  back   <- as_tbl_now(enw, verbose = FALSE)

  expect_true(is_tbl_now(back))
  expect_equal(get_data_type(back), "count-cumulative")
  expect_setequal(get_strata(back), c("location", "age_group"))
})

test_that("as_tbl_now.reporting_triangle round-trips baselinenowcast", {
  skip_if_not_installed("baselinenowcast")

  rt   <- baselinenowcast::example_reporting_triangle
  back <- as_tbl_now(rt, verbose = FALSE)
  expect_true(is_tbl_now(back))
  expect_equal(get_data_type(back), "count-incidence")
})

test_that("as_tbl_now.epidist_linelist_data round-trips epidist", {
  skip_if_not_installed("epidist")

  nowobj <- tbl_now(
    data.frame(pdate_lwr = as.Date(c("2020-03-01", "2020-03-02")),
               sdate_lwr = as.Date(c("2020-03-05", "2020-03-04"))),
    event_date = "pdate_lwr", report_date = "sdate_lwr",
    event_units = "days", report_units = "days", verbose = FALSE
  )
  epi  <- suppressMessages(tbl_now_to_epidist(nowobj, verbose = FALSE))
  back <- as_tbl_now(epi, verbose = FALSE)
  expect_true(is_tbl_now(back))
})

test_that("as_tbl_now.data.table round-trips a data.table", {
  skip_if_not_installed("data.table")

  data(denguedat)
  nowobj <- tbl_now(denguedat, event_date = "onset_week", report_date = "report_week",
                    verbose = FALSE)
  dt   <- tbl_now_to_data_table(nowobj, verbose = FALSE)
  back <- as_tbl_now(dt, event_date = "onset_week", report_date = "report_week",
                     verbose = FALSE)
  expect_true(is_tbl_now(back))
})

test_that("as_tbl_now.data.table errors without event/report dates", {
  skip_if_not_installed("data.table")
  dt <- data.table::as.data.table(data.frame(a = 1, b = 2))
  expect_error(as_tbl_now(dt), "event_date")
})

# ============================================================
# Internal helpers & error/fallback branches
# ============================================================

test_that(".need_pkg aborts for a missing package", {
  expect_error(
    tbl.now:::.need_pkg("a_package_that_surely_does_not_exist_xyz"),
    "required"
  )
})

test_that(".reporting_triangle_to_long needs reference dates as row names", {
  m <- matrix(1:4, nrow = 2)
  expect_error(tbl.now:::.reporting_triangle_to_long(m), "row names")
})

test_that(".reporting_triangle_to_long falls back to 0-based delays for non-numeric colnames", {
  m <- matrix(c(1, 2, NA, 4), nrow = 2,
              dimnames = list(c("2020-01-01", "2020-01-02"), c("a", "b")))
  res <- tbl.now:::.reporting_triangle_to_long(m, delays_unit = "days")
  expect_s3_class(res, "data.frame")
  expect_setequal(names(res), c("reference_date", "report_date", "count"))
  # NA cell dropped → 3 rows
  expect_equal(nrow(res), 3L)
})

test_that("tbl_now_from_baselinenowcast errors on a long df missing columns", {
  expect_error(
    tbl_now_from_baselinenowcast(data.frame(a = 1, b = 2, c = 3), verbose = FALSE),
    "not found"
  )
})

test_that("tbl_now_from_tsibble needs report_date for a non-tsibble input", {
  skip_if_not_installed("tsibble")
  expect_error(
    tbl_now_from_tsibble(data.frame(ev = 1, rp = 2), event_date = "ev"),
    "report_date"
  )
})

# ============================================================
# from_* verbose summaries (strata / covariates / case_count lines)
# ============================================================

test_that("tbl_now_from_epinowcast verbose prints strata and case_count", {
  skip_if_not_installed("epinowcast")
  obs <- head(epinowcast::germany_covid19_hosp, 200)
  expect_message(
    tbl_now_from_epinowcast(obs, strata = c("location", "age_group"), verbose = TRUE),
    "case_count"
  )
})

test_that("tbl_now_from_epidist interval verbose prints covariates", {
  iv <- data.frame(
    pdate_lwr = as.Date(c("2020-03-01", "2020-03-02")),
    pdate_upr = as.Date(c("2020-03-02", "2020-03-03")),
    sdate_lwr = as.Date(c("2020-03-05", "2020-03-04")),
    sdate_upr = as.Date(c("2020-03-06", "2020-03-05"))
  )
  expect_message(
    suppressWarnings(
      tbl_now_from_epidist(iv, format = "interval",
                           event_units = "days", report_units = "days", verbose = TRUE)
    ),
    "covariates"
  )
})

# ============================================================
# to_* verbose printing blocks + data-type coercion warnings
# ============================================================

# Shared count fixtures (weekly, strata + counts)
make_incidence_now <- function() {
  tbl_now(
    data.frame(
      ev  = as.Date("2020-01-01") + rep(c(0, 7, 14), each = 2),
      rp  = as.Date("2020-01-01") + c(0, 7, 7, 14, 14, 21),
      grp = rep(c("A", "B"), 3),
      n   = c(3, 2, 4, 1, 5, 2)
    ),
    event_date = "ev", report_date = "rp", case_count = "n", strata = "grp",
    data_type = "count-incidence", event_units = "weeks", report_units = "weeks",
    verbose = FALSE
  )
}

test_that("tbl_now_to_epinowcast verbose prints the conversion summary", {
  skip_if_not_installed("epinowcast")
  cumul <- to_count(make_incidence_now(), to = "count-cumulative")
  expect_message(
    suppressWarnings(tbl_now_to_epinowcast(cumul, verbose = TRUE)),
    "epinowcast"
  )
})

test_that("tbl_now_to_epinowcast warns + coerces non-cumulative input", {
  skip_if_not_installed("epinowcast")
  expect_warning(
    suppressMessages(tbl_now_to_epinowcast(make_incidence_now(), verbose = FALSE)),
    "cumulative"
  )
})

test_that("tbl_now_to_baselinenowcast verbose prints the conversion summary", {
  skip_if_not_installed("baselinenowcast")
  expect_message(
    tbl_now_to_baselinenowcast(make_incidence_now(), format = "long", verbose = TRUE),
    "baselinenowcast"
  )
})

test_that("tbl_now_to_baselinenowcast warns + coerces linelist input", {
  # linelist -> count-incidence is the supported coercion path
  data(denguedat)
  ll <- tbl_now(denguedat, event_date = "onset_week", report_date = "report_week",
                verbose = FALSE)
  expect_warning(
    suppressMessages(tbl_now_to_baselinenowcast(ll, format = "long", verbose = FALSE)),
    "incremental"
  )
})

test_that("tbl_now_to_baselinenowcast errors on count-cumulative input", {
  skip_if_not_installed("baselinenowcast")
  # Cumulative totals can be revised downward, so de-accumulating them would give
  # negative incidence; the converter must refuse rather than produce nonsense.
  cumul <- to_count(make_incidence_now(), to = "count-cumulative")
  expect_error(
    tbl_now_to_baselinenowcast(cumul, format = "long", verbose = FALSE),
    "count-cumulative"
  )
})

test_that("tbl_now_to_EpiNow2 verbose prints the conversion summary", {
  skip_if_not_installed("data.table")
  expect_message(
    tbl_now_to_EpiNow2(make_incidence_now(), verbose = TRUE),
    "EpiNow2"
  )
})

test_that("tbl_now_to_data_table verbose prints the conversion summary", {
  skip_if_not_installed("data.table")
  expect_message(
    tbl_now_to_data_table(make_incidence_now(), verbose = TRUE),
    "data.table"
  )
})

test_that("tbl_now_to_tsibble verbose prints the conversion summary", {
  skip_if_not_installed("tsibble")
  expect_message(
    tbl_now_to_tsibble(make_incidence_now(), verbose = TRUE),
    "tsibble"
  )
})

test_that("tbl_now_to_epidist verbose prints the conversion summary", {
  skip_if_not_installed("epidist")
  data(denguedat)
  ll <- tbl_now(denguedat, event_date = "onset_week", report_date = "report_week",
                verbose = FALSE)
  expect_message(
    suppressWarnings(tbl_now_to_epidist(ll, format = "linelist", verbose = TRUE)),
    "epidist"
  )
})

# ============================================================
# Remaining reachable branches
# ============================================================

test_that("tbl_now_from_data_table warns when input is not a data.table", {
  data(denguedat)
  expect_warning(
    tbl_now_from_data_table(as.data.frame(denguedat),
                            event_date = "onset_week", report_date = "report_week",
                            verbose = FALSE),
    "not a"
  )
})

test_that("tbl_now_from_epinowcast sets strata to NULL when there are no extra columns", {
  skip_if_not_installed("epinowcast")
  obs <- data.frame(
    reference_date = as.Date("2021-01-01") + rep(c(0, 7, 14), each = 2),
    report_date    = as.Date("2021-01-01") + c(0, 7, 7, 14, 14, 21),
    confirm        = c(1, 3, 2, 5, 4, 7)
  )
  res <- tbl_now_from_epinowcast(obs, event_units = "weeks", report_units = "weeks",
                                 verbose = FALSE)
  expect_null(get_strata(res))
})

test_that("tbl_now_from_tsibble sets strata to NULL when the key holds only dates", {
  skip_if_not_installed("tsibble")
  df <- data.frame(
    event  = as.Date("2021-01-01") + rep(c(0, 7, 14), each = 2),
    report = as.Date("2021-01-01") + c(0, 7, 7, 14, 14, 21),
    n      = c(1, 3, 2, 5, 4, 7)
  )
  ts  <- tsibble::as_tsibble(df, index = report, key = event)
  res <- tbl_now_from_tsibble(ts, event_date = "event",
                              event_units = "weeks", report_units = "weeks",
                              verbose = FALSE)
  expect_null(get_strata(res))
})

test_that("tbl_now_from_epidist interval errors on missing bound columns", {
  expect_error(
    tbl_now_from_epidist(data.frame(pdate_lwr = as.Date("2020-01-01")),
                         format = "interval", verbose = FALSE),
    "not found"
  )
})

test_that("tbl_now_to_epidist interval errors when named upper-bound columns are absent", {
  skip_if_not_installed("epidist")
  data(denguedat)
  ll <- tbl_now(denguedat, event_date = "onset_week", report_date = "report_week",
                verbose = FALSE)
  expect_error(
    suppressWarnings(
      tbl_now_to_epidist(ll, format = "interval",
                         primary_upper = "no_such_col", secondary_upper = "nope",
                         verbose = FALSE)
    ),
    "Upper-bound"
  )
})

# ============================================================
# from_* converters strip explicit zeros (minimal tbl_now)
# ============================================================

test_that("tbl_now_from_epinowcast drops zero-confirm rows (minimal tbl_now)", {
  skip_if_not_installed("epinowcast")
  obs <- head(epinowcast::germany_covid19_hosp, 300)
  res <- tbl_now_from_epinowcast(obs, strata = c("location", "age_group"),
                                 verbose = FALSE)
  expect_equal(sum(res[["confirm"]] == 0, na.rm = TRUE), 0L)
})

test_that("tbl_now_from_baselinenowcast drops zero cells but preserves totals", {
  skip_if_not_installed("baselinenowcast")
  m <- matrix(c(10, 5, 0,  8, 0, 2,  6, 0, 0), nrow = 3, byrow = TRUE,
              dimnames = list(c("2020-01-01", "2020-01-02", "2020-01-03"),
                              c("0", "1", "2")))
  res <- tbl_now_from_baselinenowcast(m, verbose = FALSE)
  expect_equal(sum(res[["count"]] == 0), 0L)
  expect_equal(sum(res[["count"]]), sum(m))   # totals preserved
  expect_equal(nrow(res), sum(m > 0))          # one row per non-zero cell
})
