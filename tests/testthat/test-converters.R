library(dplyr, quietly = TRUE, warn.conflicts = FALSE)

# ============================================================
# epinowcast
# ============================================================

test_that("tbl_now_from_epinowcast builds a count-cumulative tbl_now", {
  skip_if_not_installed("epinowcast")

  obs <- head(epinowcast::germany_covid19_hosp, 300)
  res <- tbl_now_from_epinowcast(obs,
    strata = c("location", "age_group"),
    verbose = FALSE
  )

  expect_true(is_tbl_now(res))
  expect_equal(get_data_type(res), "count-cumulative")
  expect_equal(get_event_date(res), "reference_date")
  expect_equal(get_report_date(res), "report_date")
  expect_equal(get_case_count(res), "confirm")
  expect_setequal(get_strata(res), c("location", "age_group"))
})

test_that("tbl_now_from_epinowcast auto-detects strata", {
  skip_on_cran()
  skip_if_not_installed("epinowcast")

  obs <- head(epinowcast::germany_covid19_hosp, 100)
  res <- tbl_now_from_epinowcast(obs, verbose = FALSE)
  # location + age_group are the non-core columns
  expect_setequal(get_strata(res), c("location", "age_group"))
})

test_that("tbl_now_from_epinowcast errors on missing columns", {
  skip_on_cran()
  skip_if_not_installed("epinowcast")
  bad <- data.frame(a = 1, b = 2)
  expect_error(tbl_now_from_epinowcast(bad, verbose = FALSE), "not found")
})

test_that("tbl_now_from_epinowcast forwards ... to tbl_now (now)", {
  skip_on_cran()
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
  nowobj <- tbl_now_from_epinowcast(obs,
    strata = c("location", "age_group"),
    verbose = FALSE
  )
  ep <- tbl_now_to_epinowcast(nowobj, verbose = FALSE, quiet = TRUE)
  expect_s3_class(ep, "enw_preprocess_data")
})

test_that("tbl_now_to_epinowcast preprocess=FALSE returns completed data.table", {
  skip_on_cran()
  skip_if_not_installed("epinowcast")

  obs <- head(epinowcast::germany_covid19_hosp, 200)
  nowobj <- tbl_now_from_epinowcast(obs,
    strata = c("location", "age_group"),
    verbose = FALSE
  )
  comp <- tbl_now_to_epinowcast(nowobj, preprocess = FALSE, verbose = FALSE,
                                quiet = TRUE)
  expect_s3_class(comp, "data.table")
  expect_true(all(c("reference_date", "report_date", "confirm") %in% names(comp)))
})

test_that("tbl_now_to_epinowcast errors on non-tbl_now", {
  skip_on_cran()
  skip_if_not_installed("epinowcast")
  expect_error(tbl_now_to_epinowcast(data.frame(a = 1)), "tbl_now")
})

test_that("epinowcast round-trip preserves every observation (no info lost)", {
  skip_on_cran()
  skip_if_not_installed("epinowcast")
  skip_if_not_installed("data.table")
  library(data.table)

  obs  <- as.data.table(subset(epinowcast::germany_covid19_hosp, location == "DE"))
  obs$location <- NULL

  # A max_delay beyond the longest observed delay (~82 days) so nothing is
  # truncated: max_confirm / cum_prop_reported are then fully recoverable.
  pobs <- suppressWarnings(
    epinowcast::enw_preprocess_data(obs, max_delay = 100, by = "age_group")
  )

  tbl_epi <- tbl_now_from_epinowcast(pobs, verbose = FALSE)
  rt <- tbl_now_to_epinowcast(tbl_epi, quiet = TRUE, verbose = FALSE)

  # `identical()` / positional `all.equal()` fail only because the `.group`
  # indices and the `age_group` factor levels may be reassigned (cosmetic).
  # Sort both observation tables by their keys and compare column-by-column to
  # confirm no information is lost in the round-trip.
  align <- function(x) {
    d <- data.table::as.data.table(x)
    d$age_group <- as.character(d$age_group)
    data.table::setorderv(d, c("reference_date", "age_group", "delay"))
    d[]
  }
  o1 <- align(pobs$obs[[1]])
  o2 <- align(rt$obs[[1]])
  expect_equal(ncol(o1), ncol(o2))
  expect_equal(nrow(o1), nrow(o2))
  expect_equal(o1$reference_date, o2$reference_date)
  expect_equal(o1$report_date, o2$report_date)
  expect_equal(o1$age_group, o2$age_group)
  expect_equal(o1$confirm, o2$confirm)
  expect_equal(o1$max_confirm, o2$max_confirm)         # full history retained
  expect_equal(o1$cum_prop_reported, o2$cum_prop_reported)
})


# ============================================================
# baselinenowcast
# ============================================================

test_that("tbl_now_from_baselinenowcast expands a reporting-triangle matrix", {
  skip_if_not_installed("baselinenowcast")

  rt <- baselinenowcast::example_reporting_triangle
  res <- tbl_now_from_baselinenowcast(rt, verbose = FALSE)

  expect_true(is_tbl_now(res))
  expect_equal(get_data_type(res), "count-incidence")
  expect_equal(get_case_count(res), "count")

  # The last observed report date becomes `now`; future cells (report > now)
  # are dropped, within-window missing cells are kept as NA-count rows.
  refs  <- as.Date(rownames(rt))
  delay <- as.integer(colnames(rt))
  report <- outer(as.integer(refs), delay, `+`)        # report = ref + delay
  now    <- max(report[!is.na(rt)])
  expect_equal(as.integer(get_now(res)), now)
  expect_equal(nrow(res), sum(report <= now))          # future cells dropped
  expect_equal(sum(is.na(res[["count"]])), sum(is.na(rt) & report <= now))
  expect_true(all(res[["count"]] >= 0, na.rm = TRUE))
})

test_that("baselinenowcast matrix round-trip preserves NA vs 0 exactly", {
  skip_on_cran()
  skip_if_not_installed("baselinenowcast")

  rt     <- baselinenowcast::example_reporting_triangle
  nowobj <- tbl_now_from_baselinenowcast(rt, verbose = FALSE)
  back   <- suppressMessages(
    tbl_now_to_baselinenowcast(nowobj, verbose = FALSE, quiet = TRUE)
  )
  expect_identical(rt, back)
})

test_that("tbl_now_to_baselinenowcast infers delays_unit from the object units", {
  skip_if_not_installed("baselinenowcast")
  data(denguedat)
  weekly <- tbl_now(denguedat[1:2000, ],
    event_date = "onset_week", report_date = "report_week", verbose = FALSE
  )
  inferred <- suppressWarnings(tbl_now_to_baselinenowcast(weekly, verbose = FALSE))
  explicit <- suppressWarnings(
    tbl_now_to_baselinenowcast(weekly, delays_unit = "weeks", verbose = FALSE)
  )
  # weekly units -> inferred "weeks"
  expect_identical(inferred, explicit)
  expect_equal(attr(inferred, "delays_unit"), "weeks")

  # long format never needs delays_unit
  expect_no_error(
    suppressWarnings(tbl_now_to_baselinenowcast(weekly, format = "long", verbose = FALSE))
  )
})

test_that("tbl_now_to_baselinenowcast errors when delays_unit cannot be inferred", {
  skip_on_cran()
  monthly <- tbl_now(
    data.frame(
      ev = seq(as.Date("2020-01-01"), by = "month", length.out = 30),
      rp = seq(as.Date("2020-02-01"), by = "month", length.out = 30),
      n = 1:30
    ),
    event_date = ev, report_date = rp, case_count = n,
    data_type = "count-incidence", event_units = "months", report_units = "months",
    verbose = FALSE
  )
  expect_error(
    tbl_now_to_baselinenowcast(monthly, verbose = FALSE),
    "delays_unit"
  )
})

test_that("tbl_now_from_baselinenowcast reads the long data.frame", {
  skip_on_cran()
  skip_if_not_installed("baselinenowcast")

  res <- tbl_now_from_baselinenowcast(head(baselinenowcast::syn_nssp_df, 400),
    verbose = FALSE
  )
  expect_true(is_tbl_now(res))
  expect_equal(get_data_type(res), "count-incidence")
})

test_that("tbl_now_to_baselinenowcast long returns the expected columns", {
  skip_on_cran()
  skip_if_not_installed("baselinenowcast")

  res <- tbl_now_from_baselinenowcast(head(baselinenowcast::syn_nssp_df, 400),
    verbose = FALSE
  )
  long <- tbl_now_to_baselinenowcast(res, format = "long", verbose = FALSE,
                                     quiet = TRUE)
  expect_s3_class(long, "data.frame")
  expect_equal(names(long), c("reference_date", "report_date", "count"))
})

test_that("tbl_now_to_baselinenowcast matrix returns a reporting_triangle", {
  skip_on_cran()
  skip_if_not_installed("baselinenowcast")

  res <- tbl_now_from_baselinenowcast(head(baselinenowcast::syn_nssp_df, 400),
    verbose = FALSE
  )
  mx <- suppressMessages(
    tbl_now_to_baselinenowcast(res, format = "matrix", verbose = FALSE,
                               quiet = TRUE)
  )
  expect_s3_class(mx, "reporting_triangle")
  expect_true(is.matrix(mx))
})

test_that("baselinenowcast long round-trip preserves counts", {
  skip_on_cran()
  skip_if_not_installed("baselinenowcast")

  orig <- head(baselinenowcast::syn_nssp_df, 400)
  res <- tbl_now_from_baselinenowcast(orig, verbose = FALSE)
  long <- tbl_now_to_baselinenowcast(res, format = "long", verbose = FALSE,
                                     quiet = TRUE)
  expect_equal(sum(long$count), sum(orig$count))
})

test_that("baselinenowcast round-trip preserves everything", {
  skip_on_cran()
  skip_if_not_installed("baselinenowcast")

  rt     <- baselinenowcast::example_reporting_triangle

  # Convert to a tbl_now
  nowobj <- tbl_now_from_baselinenowcast(rt,verbose = FALSE)

  # The matrix round-trip is faithful (not-yet-observed `NA` cells are kept).
  expect_identical(rt, tbl_now_to_baselinenowcast(nowobj, verbose = FALSE))

})





# # ============================================================
# # EpiNow2 (to only)
# # ============================================================
#
# test_that("tbl_now_to_EpiNow2 collapses count data to a date/confirm series", {
#   skip_if_not_installed("data.table")
#
#   data(mpoxdat)
#   nowobj <- tbl_now(mpoxdat,
#     event_date = "dx_date", report_date = "dx_report_date",
#     case_count = "n", strata = "race",
#     data_type = "count-incidence", verbose = FALSE
#   )
#   series <- tbl_now_to_EpiNow2(nowobj, verbose = FALSE)
#
#   expect_s3_class(series, "data.table")
#   expect_equal(names(series), c("date", "confirm"))
#   # one row per unique event date
#   expect_equal(nrow(series), dplyr::n_distinct(mpoxdat$dx_date))
#   # dates sorted ascending
#   expect_false(is.unsorted(series$date))
# })
#
# test_that("tbl_now_to_EpiNow2 works on linelist data", {
#   skip_if_not_installed("data.table")
#
#   data(denguedat)
#   nowobj <- tbl_now(denguedat,
#     event_date = "onset_week", report_date = "report_week",
#     verbose = FALSE
#   )
#   series <- tbl_now_to_EpiNow2(nowobj, verbose = FALSE)
#   expect_equal(names(series), c("date", "confirm"))
#   expect_equal(sum(series$confirm), nrow(denguedat))
# })
#
# test_that("tbl_now_to_EpiNow2 errors on non-tbl_now", {
#   skip_if_not_installed("data.table")
#   expect_error(tbl_now_to_EpiNow2(data.frame(a = 1)), "tbl_now")
# })

# ============================================================
# data.table
# ============================================================

test_that("tbl_now_from_data_table builds a tbl_now", {
  skip_if_not_installed("data.table")

  data(denguedat)
  dt <- data.table::as.data.table(denguedat)
  res <- tbl_now_from_data_table(dt,
    event_date = "onset_week",
    report_date = "report_week", verbose = FALSE
  )
  expect_true(is_tbl_now(res))
  expect_equal(get_event_date(res), "onset_week")
})

test_that("tbl_now_to_data_table returns a data.table and drops tbl_now class", {
  skip_if_not_installed("data.table")

  data(denguedat)
  nowobj <- tbl_now(denguedat,
    event_date = "onset_week", report_date = "report_week",
    verbose = FALSE
  )
  dt <- tbl_now_to_data_table(nowobj, verbose = FALSE)
  expect_s3_class(dt, "data.table")
  expect_false(is_tbl_now(dt))
})

test_that("data.table round-trip preserves row count", {
  skip_on_cran()
  skip_if_not_installed("data.table")

  data(denguedat)
  dt <- data.table::as.data.table(denguedat)
  res <- tbl_now_from_data_table(dt,
    event_date = "onset_week",
    report_date = "report_week", verbose = FALSE
  )
  back <- tbl_now_to_data_table(res, verbose = FALSE)
  expect_equal(nrow(back), nrow(denguedat))
})

test_that("data.table round-trip preserves row count", {
  skip_on_cran()
  skip_if_not_installed("data.table")

  data(denguedat)
  dt <- data.table::as.data.table(denguedat)
  nowobj <- tbl_now_from_data_table(dt,
                                    event_date = "onset_week",
                                    report_date = "report_week", verbose = FALSE
  )
  dt2 <- tbl_now_to_data_table(nowobj, verbose = FALSE)
  expect_identical(dt, dt2[,1:3])
})

test_that("tbl_now_from_data_table forwards ... (strata) to tbl_now", {
  skip_on_cran()
  skip_if_not_installed("data.table")
  data(denguedat)
  dt <- data.table::as.data.table(denguedat)
  res <- tbl_now_from_data_table(dt,
    event_date = "onset_week",
    report_date = "report_week",
    strata = "gender", verbose = FALSE
  )
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
  res <- tbl_now_from_epidist(ll,
    event_units = "days", report_units = "days",
    verbose = FALSE
  )
  expect_true(is_tbl_now(res))
  expect_equal(get_data_type(res), "linelist")
  expect_equal(get_event_date(res), "pdate_lwr")
  expect_equal(get_report_date(res), "sdate_lwr")
})

test_that("tbl_now_from_epidist interval attaches upper bounds as covariates + warns", {
  skip_on_cran()
  iv <- data.frame(
    pdate_lwr = as.Date(c("2020-03-01", "2020-03-02")),
    pdate_upr = as.Date(c("2020-03-02", "2020-03-03")),
    sdate_lwr = as.Date(c("2020-03-05", "2020-03-04")),
    sdate_upr = as.Date(c("2020-03-06", "2020-03-05"))
  )
  expect_warning(
    res <- tbl_now_from_epidist(iv,
      format = "interval",
      event_units = "days", report_units = "days",
      verbose = FALSE
    ),
    "Interval-censored"
  )
  expect_setequal(get_covariates(res), c("pdate_upr", "sdate_upr"))
})

test_that("tbl_now_from_epidist errors on missing columns", {
  skip_on_cran()
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
  res <- tbl_now_from_epidist(ll,
    event_units = "days", report_units = "days",
    verbose = FALSE
  )
  out <- suppressMessages(
    tbl_now_to_epidist(res, format = "linelist", verbose = FALSE, quiet = TRUE)
  )
  expect_true(epidist::is_epidist_linelist_data(out))
})

test_that("tbl_now_to_epidist interval requires upper-bound columns", {
  skip_on_cran()
  skip_if_not_installed("epidist")

  ll <- data.frame(
    pdate_lwr = as.Date(c("2020-03-01", "2020-03-02")),
    sdate_lwr = as.Date(c("2020-03-05", "2020-03-04"))
  )
  res <- tbl_now_from_epidist(ll,
    event_units = "days", report_units = "days",
    verbose = FALSE
  )
  expect_error(
    tbl_now_to_epidist(res, format = "interval", verbose = FALSE, quiet = TRUE),
    "primary_upper"
  )
})

test_that("tbl_now_to_epidist interval round-trips via covariates", {
  skip_on_cran()
  skip_if_not_installed("epidist")

  iv <- data.frame(
    pdate_lwr = as.Date(c("2020-03-01", "2020-03-02")),
    pdate_upr = as.Date(c("2020-03-02", "2020-03-03")),
    sdate_lwr = as.Date(c("2020-03-05", "2020-03-04")),
    sdate_upr = as.Date(c("2020-03-06", "2020-03-05"))
  )
  res <- suppressWarnings(
    tbl_now_from_epidist(iv,
      format = "interval",
      event_units = "days", report_units = "days", verbose = FALSE
    )
  )
  out <- suppressWarnings(suppressMessages(
    tbl_now_to_epidist(res,
      format = "interval",
      primary_upper = "pdate_upr", secondary_upper = "sdate_upr",
      verbose = FALSE
    )
  ))
  expect_true(epidist::is_epidist_linelist_data(out))
})

test_that("tbl_now_to_epidist errors on non-tbl_now", {
  skip_on_cran()
  skip_if_not_installed("epidist")
  expect_error(tbl_now_to_epidist(data.frame(a = 1)), "tbl_now")
})

test_that("tbl_now_to_epidist auto builds aggregate data from counts", {
  skip_on_cran()
  skip_if_not_installed("epidist")
  ct <- tbl_now(
    dplyr::tibble(
      ev = as.Date(c("2020-03-01", "2020-03-02", "2020-03-04")),
      rp = as.Date(c("2020-03-05", "2020-03-04", "2020-03-10")),
      n  = c(5, 3, 2)
    ),
    event_date = ev, report_date = rp, case_count = n,
    data_type = "count-incidence", event_units = "days", report_units = "days",
    verbose = FALSE
  )
  out <- suppressMessages(tbl_now_to_epidist(ct, verbose = FALSE, quiet = TRUE))
  expect_true(epidist::is_epidist_aggregate_data(out))
  expect_true("n" %in% names(out))
  expect_equal(out$n, c(5, 3, 2))
})

test_that("tbl_now_from_epidist reads aggregate data as count-incidence", {
  skip_on_cran()
  skip_if_not_installed("epidist")
  agg <- epidist::as_epidist_aggregate_data(
    data.frame(
      pdate_lwr = as.Date(c("2020-03-01", "2020-03-02")),
      sdate_lwr = as.Date(c("2020-03-05", "2020-03-04")),
      n = c(4, 6)
    ),
    n = "n", pdate_lwr = "pdate_lwr", sdate_lwr = "sdate_lwr"
  )
  res <- suppressMessages(tbl_now_from_epidist(agg, verbose = FALSE))
  expect_equal(get_data_type(res), "count-incidence")
  expect_equal(get_case_count(res), "n")
})

test_that("epidist aggregate count round-trip preserves counts and alignment", {
  skip_on_cran()
  skip_if_not_installed("epidist")
  ct <- tbl_now(
    dplyr::tibble(
      ev = as.Date(c("2020-03-01", "2020-03-02", "2020-03-04")),
      rp = as.Date(c("2020-03-05", "2020-03-04", "2020-03-10")),
      n  = c(5, 3, 2)
    ),
    event_date = ev, report_date = rp, case_count = n,
    data_type = "count-incidence", event_units = "days", report_units = "days",
    verbose = FALSE
  )
  agg  <- suppressMessages(tbl_now_to_epidist(ct, verbose = FALSE, quiet = TRUE))
  back <- suppressMessages(tbl_now_from_epidist(agg, verbose = FALSE))
  expect_equal(get_data_type(back), "count-incidence")
  expect_equal(back[["n"]], c(5, 3, 2))
  # epidist's numeric time aligns with tbl_now's generated columns.
  expect_equal(back[[".event_num"]], agg$ptime_lwr)
  expect_equal(back[[".delay"]], agg$stime_lwr - agg$ptime_lwr)
})

test_that("tbl_now_to_epidist uses a 7-day window for weekly units", {
  skip_on_cran()
  skip_if_not_installed("epidist")
  wt <- tbl_now(
    dplyr::tibble(
      ev = as.Date(c("2020-03-01", "2020-03-08")),
      rp = as.Date(c("2020-03-15", "2020-03-22"))
    ),
    event_date = ev, report_date = rp, data_type = "linelist",
    event_units = "weeks", report_units = "weeks", verbose = FALSE
  )
  out <- suppressMessages(tbl_now_to_epidist(wt, verbose = FALSE, quiet = TRUE))
  expect_equal(as.numeric(out$pdate_upr - out$pdate_lwr), c(7, 7))
  # units are recovered from the window width on the way back.
  back <- suppressMessages(tbl_now_from_epidist(out, verbose = FALSE))
  expect_equal(get_event_units(back), "weeks")
})

test_that("epidist is_censored becomes a [origin, report] window and round-trips", {
  skip_on_cran()
  skip_if_not_installed("epidist")
  ct <- tbl_now(
    dplyr::tibble(
      ev   = as.Date(c("2020-03-01", "2020-03-02", "2020-03-04", "2020-03-05")),
      rp   = as.Date(c("2020-03-05", "2020-03-04", "2020-03-10", "2020-03-09")),
      cens = c(FALSE, FALSE, TRUE, TRUE)
    ),
    event_date = ev, report_date = rp, is_censored = cens,
    data_type = "linelist", event_units = "days", report_units = "days",
    verbose = FALSE
  )
  out <- suppressMessages(tbl_now_to_epidist(ct, verbose = FALSE, quiet = TRUE))
  # censored rows are left-censored to epidist time 0.
  expect_equal(out$stime_lwr, c(4, 3, 0, 0))

  back <- suppressMessages(tbl_now_from_epidist(out, verbose = FALSE))
  expect_equal(get_is_censored(back), "is_censored")
  expect_equal(back[["is_censored"]], c(FALSE, FALSE, TRUE, TRUE))
  # the true report dates are recovered for the censored rows.
  expect_equal(back[[get_report_date(back)]], ct[["rp"]])
})

test_that("tbl_now_to_epidist aggregate errors on linelist input", {
  skip_on_cran()
  skip_if_not_installed("epidist")
  ll <- tbl_now(
    dplyr::tibble(ev = as.Date("2020-03-01"), rp = as.Date("2020-03-05")),
    event_date = ev, report_date = rp, data_type = "linelist",
    event_units = "days", report_units = "days", verbose = FALSE
  )
  expect_error(
    suppressMessages(
      tbl_now_to_epidist(ll, format = "aggregate", verbose = FALSE, quiet = TRUE)
    ),
    "aggregate"
  )
})

test_that("as_epidist_aggregate_data.tbl_now dispatches with counts", {
  skip_if_not_installed("epidist")
  ct <- tbl_now(
    dplyr::tibble(ev = as.Date(c("2020-03-01", "2020-03-02")),
                  rp = as.Date(c("2020-03-05", "2020-03-04")),
                  n  = c(2, 3)),
    event_date = ev, report_date = rp, case_count = n,
    data_type = "count-incidence", event_units = "days", report_units = "days",
    verbose = FALSE
  )
  out <- suppressMessages(epidist::as_epidist_aggregate_data(ct, quiet = TRUE))
  expect_true(epidist::is_epidist_aggregate_data(out))
})

test_that("epidist long round-trip preserves linelist", {
  skip_on_cran()
  skip_if_not_installed("epidist")

  # --- Linelist epidist data (one row per case) ---
  ll <- epidist::as_epidist_linelist_data(
    data.frame(
      pdate_lwr = as.Date(c("2020-03-01", "2020-03-02", "2020-03-02")),
      sdate_lwr = as.Date(c("2020-03-05", "2020-03-04", "2020-03-06"))
    ),
    pdate_lwr = "pdate_lwr", sdate_lwr = "sdate_lwr"
  )
  # -> a linelist tbl_now ...
  nowll <- tbl_now_from_epidist(ll, verbose = FALSE)

  ll  <- ll[,sort(colnames(ll))]
  ll2 <- tbl_now_to_epidist(nowll, quiet = TRUE, verbose = FALSE)
  expect_equal(ll, ll2[,sort(colnames(ll2))])


  # --- Aggregate epidist data (counts in an `n` column) ---
  agg <- epidist::as_epidist_aggregate_data(
    data.frame(
      pdate_lwr = as.Date(c("2020-03-01", "2020-03-02")),
      sdate_lwr = as.Date(c("2020-03-05", "2020-03-04")),
      n = c(7, 3)
    ),
    n = "n", pdate_lwr = "pdate_lwr", sdate_lwr = "sdate_lwr"
  )
  # -> a count-incidence tbl_now (case_count = "n") ...
  nowagg <- tbl_now_from_epidist(agg, verbose = FALSE)

  agg  <- agg[,sort(colnames(agg))]
  ll2  <- tbl_now_to_epidist(nowagg, quiet = TRUE, verbose = FALSE)
  expect_equal(agg, ll2[,sort(colnames(ll2))])
})



# ============================================================
# verbose messaging
# ============================================================

test_that("from_* verbose prints a conversion summary", {
  skip_on_cran()
  ll <- data.frame(
    pdate_lwr = as.Date(c("2020-03-01", "2020-03-02", "2020-03-03")),
    sdate_lwr = as.Date(c("2020-03-05", "2020-03-04", "2020-03-08"))
  )
  expect_message(
    tbl_now_from_epidist(ll,
      event_units = "days", report_units = "days",
      verbose = TRUE
    ),
    "tbl_now"
  )
})

# ============================================================
# tsibble
# ============================================================

test_that("tbl_now_to_tsibble builds a tsibble with the chosen index and key", {
  skip_if_not_installed("tsibble")

  data(denguedat)
  nowobj <- tbl_now(denguedat,
    event_date = "onset_week", report_date = "report_week",
    strata = "gender", verbose = FALSE
  )
  ts <- suppressWarnings(tbl_now_to_tsibble(nowobj, verbose = FALSE))

  expect_s3_class(ts, "tbl_ts")
  # The event date is the index; the report date and strata are the key.
  expect_equal(tsibble::index_var(ts), "onset_week")
  expect_setequal(tsibble::key_vars(ts), c("report_week", "gender"))
})

test_that("tbl_now_to_tsibble index = report_date uses report_date as index", {
  skip_on_cran()
  skip_if_not_installed("tsibble")

  data(denguedat)
  nowobj <- tbl_now(denguedat,
    event_date = "onset_week", report_date = "report_week",
    verbose = FALSE
  )
  ts <- suppressWarnings(tbl_now_to_tsibble(nowobj, index = "report_date", verbose = FALSE))
  expect_equal(tsibble::index_var(ts), "report_week")
})

test_that("tbl_now_from_tsibble rebuilds a tbl_now and recovers strata", {
  skip_if_not_installed("tsibble")

  data(denguedat)
  nowobj <- tbl_now(denguedat,
    event_date = "onset_week", report_date = "report_week",
    strata = "gender", verbose = FALSE
  )
  ts <- suppressWarnings(tbl_now_to_tsibble(nowobj, verbose = FALSE))
  back <- tbl_now_from_tsibble(ts, report_date = "report_week", verbose = FALSE)

  expect_true(is_tbl_now(back))
  expect_equal(get_event_date(back), "onset_week") # from the tsibble index
  expect_equal(get_report_date(back), "report_week")
  expect_equal(get_strata(back), "gender")
})

test_that("tbl_now_from_tsibble errors without report_date", {
  skip_on_cran()
  skip_if_not_installed("tsibble")
  data(denguedat)
  nowobj <- tbl_now(denguedat,
    event_date = "onset_week", report_date = "report_week",
    verbose = FALSE
  )
  ts <- suppressWarnings(tbl_now_to_tsibble(nowobj, verbose = FALSE))
  expect_error(tbl_now_from_tsibble(ts), "report_date")
})

test_that("tbl_now_to_tsibble errors on non-tbl_now", {
  skip_on_cran()
  skip_if_not_installed("tsibble")
  expect_error(tbl_now_to_tsibble(data.frame(a = 1)), "tbl_now")
})

# ============================================================
# as_tbl_now methods for tbl_now_to_* output classes
# ============================================================

test_that("as_tbl_now.tbl_ts round-trips a tsibble", {
  skip_if_not_installed("tsibble")

  data(denguedat)
  nowobj <- tbl_now(denguedat,
    event_date = "onset_week", report_date = "report_week",
    strata = "gender", verbose = FALSE
  )
  ts <- suppressWarnings(tbl_now_to_tsibble(nowobj, verbose = FALSE))
  back <- as_tbl_now(ts, report_date = "report_week", verbose = FALSE)

  expect_true(is_tbl_now(back))
  expect_equal(get_strata(back), "gender")
})

test_that("as_tbl_now.tbl_ts errors without report_date", {
  skip_on_cran()
  skip_if_not_installed("tsibble")
  data(denguedat)
  nowobj <- tbl_now(denguedat,
    event_date = "onset_week", report_date = "report_week",
    verbose = FALSE
  )
  ts <- suppressWarnings(tbl_now_to_tsibble(nowobj, verbose = FALSE))
  expect_error(as_tbl_now(ts), "report_date")
})

test_that("as_tbl_now.enw_preprocess_data round-trips epinowcast", {
  skip_if_not_installed("epinowcast")

  obs <- head(epinowcast::germany_covid19_hosp, 300)
  nowobj <- tbl_now_from_epinowcast(obs,
    strata = c("location", "age_group"),
    verbose = FALSE
  )
  enw <- tbl_now_to_epinowcast(nowobj, verbose = FALSE, quiet = TRUE)
  back <- as_tbl_now(enw, verbose = FALSE)

  expect_true(is_tbl_now(back))
  expect_equal(get_data_type(back), "count-cumulative")
  expect_setequal(get_strata(back), c("location", "age_group"))
})

test_that("as_tbl_now.reporting_triangle round-trips baselinenowcast", {
  skip_if_not_installed("baselinenowcast")

  rt <- baselinenowcast::example_reporting_triangle
  back <- as_tbl_now(rt, verbose = FALSE)
  expect_true(is_tbl_now(back))
  expect_equal(get_data_type(back), "count-incidence")
})

test_that("as_tbl_now.epidist_linelist_data round-trips epidist", {
  skip_if_not_installed("epidist")

  nowobj <- tbl_now(
    data.frame(
      pdate_lwr = as.Date(c("2020-03-01", "2020-03-02")),
      sdate_lwr = as.Date(c("2020-03-05", "2020-03-04"))
    ),
    event_date = "pdate_lwr", report_date = "sdate_lwr",
    event_units = "days", report_units = "days", verbose = FALSE
  )
  epi <- suppressMessages(tbl_now_to_epidist(nowobj, verbose = FALSE, quiet = TRUE))
  back <- as_tbl_now(epi, verbose = FALSE)
  expect_true(is_tbl_now(back))
})

test_that("as_tbl_now.data.table round-trips a data.table", {
  skip_if_not_installed("data.table")

  data(denguedat)
  nowobj <- tbl_now(denguedat,
    event_date = "onset_week", report_date = "report_week",
    verbose = FALSE
  )
  dt <- tbl_now_to_data_table(nowobj, verbose = FALSE)
  back <- as_tbl_now(dt,
    event_date = "onset_week", report_date = "report_week",
    verbose = FALSE
  )
  expect_true(is_tbl_now(back))
})

test_that("as_tbl_now.data.table errors without event/report dates", {
  skip_on_cran()
  skip_if_not_installed("data.table")
  dt <- data.table::as.data.table(data.frame(a = 1, b = 2))
  expect_error(as_tbl_now(dt), "event_date")
})

# ============================================================
# Internal helpers & error/fallback branches
# ============================================================

test_that(".need_pkg aborts for a missing package", {
  skip_on_cran()
  expect_error(
    tbl.now:::.need_pkg("a_package_that_surely_does_not_exist_xyz"),
    "required"
  )
})

test_that(".reporting_triangle_to_long needs reference dates as row names", {
  skip_on_cran()
  m <- matrix(1:4, nrow = 2)
  expect_error(tbl.now:::.reporting_triangle_to_long(m), "row names")
})

test_that(".reporting_triangle_to_long falls back to 0-based delays for non-numeric colnames", {
  skip_on_cran()
  m <- matrix(c(1, 2, NA, 4),
    nrow = 2,
    dimnames = list(c("2020-01-01", "2020-01-02"), c("a", "b"))
  )
  res <- tbl.now:::.reporting_triangle_to_long(m, delays_unit = "days")
  expect_s3_class(res, "data.frame")
  expect_setequal(names(res), c("reference_date", "report_date", "count"))
  # NA cell kept (not dropped) → one row per cell, with the NA preserved.
  expect_equal(nrow(res), 4L)
  expect_equal(sum(is.na(res$count)), 1L)
})

test_that("tbl_now_from_baselinenowcast errors on a long df missing columns", {
  skip_on_cran()
  expect_error(
    tbl_now_from_baselinenowcast(data.frame(a = 1, b = 2, c = 3), verbose = FALSE),
    "not found"
  )
})

test_that("tbl_now_from_tsibble needs event_date for a non-tsibble input", {
  skip_on_cran()
  skip_if_not_installed("tsibble")
  # report_date supplied, but event_date cannot default from a non-tsibble.
  expect_error(
    tbl_now_from_tsibble(data.frame(ev = 1, rp = 2), report_date = "rp"),
    "event_date"
  )
})

# ============================================================
# from_* verbose summaries (strata / covariates / case_count lines)
# ============================================================

test_that("tbl_now_from_epinowcast verbose prints strata and case_count", {
  skip_on_cran()
  skip_if_not_installed("epinowcast")
  obs <- head(epinowcast::germany_covid19_hosp, 200)
  expect_message(
    tbl_now_from_epinowcast(obs, strata = c("location", "age_group"), verbose = TRUE),
    "case_count"
  )
})

test_that("tbl_now_from_epidist interval verbose prints covariates", {
  skip_on_cran()
  iv <- data.frame(
    pdate_lwr = as.Date(c("2020-03-01", "2020-03-02")),
    pdate_upr = as.Date(c("2020-03-02", "2020-03-03")),
    sdate_lwr = as.Date(c("2020-03-05", "2020-03-04")),
    sdate_upr = as.Date(c("2020-03-06", "2020-03-05"))
  )
  expect_message(
    suppressWarnings(
      tbl_now_from_epidist(iv,
        format = "interval",
        event_units = "days", report_units = "days", verbose = TRUE
      )
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
  skip_on_cran()
  skip_if_not_installed("epinowcast")
  cumul <- to_count(make_incidence_now(), to = "count-cumulative")
  expect_message(
    suppressWarnings(tbl_now_to_epinowcast(cumul, verbose = TRUE, quiet = TRUE)),
    "epinowcast"
  )
})

test_that("tbl_now_to_epinowcast warns + coerces non-cumulative input", {
  skip_on_cran()
  skip_if_not_installed("epinowcast")
  expect_warning(
    suppressMessages(tbl_now_to_epinowcast(make_incidence_now(), verbose = FALSE,
                                           quiet = TRUE)),
    "cumulative"
  )
})

test_that("tbl_now_to_baselinenowcast verbose prints the conversion summary", {
  skip_on_cran()
  skip_if_not_installed("baselinenowcast")
  expect_message(
    tbl_now_to_baselinenowcast(make_incidence_now(), format = "long",
                               verbose = TRUE, quiet = TRUE),
    "baselinenowcast"
  )
})

test_that("tbl_now_to_baselinenowcast warns + coerces linelist input", {
  skip_on_cran()
  # linelist -> count-incidence is the supported coercion path
  data(denguedat)
  ll <- tbl_now(denguedat,
    event_date = "onset_week", report_date = "report_week",
    verbose = FALSE
  )
  expect_warning(
    suppressMessages(tbl_now_to_baselinenowcast(ll, format = "long",
                                                verbose = FALSE, quiet = TRUE)),
    "incremental"
  )
})

test_that("tbl_now_to_baselinenowcast errors on count-cumulative input", {
  skip_on_cran()
  skip_if_not_installed("baselinenowcast")
  # Cumulative totals can be revised downward, so de-accumulating them would give
  # negative incidence; the converter must refuse rather than produce nonsense.
  cumul <- to_count(make_incidence_now(), to = "count-cumulative")
  expect_error(
    tbl_now_to_baselinenowcast(cumul, format = "long", verbose = FALSE,
                               quiet = TRUE),
    "count-cumulative"
  )
})

# test_that("tbl_now_to_EpiNow2 verbose prints the conversion summary", {
#   skip_if_not_installed("data.table")
#   expect_message(
#     tbl_now_to_EpiNow2(make_incidence_now(), verbose = TRUE),
#     "EpiNow2"
#   )
# })

test_that("tbl_now_to_data_table verbose prints the conversion summary", {
  skip_on_cran()
  skip_if_not_installed("data.table")
  expect_message(
    tbl_now_to_data_table(make_incidence_now(), verbose = TRUE),
    "data.table"
  )
})

test_that("tbl_now_to_tsibble verbose prints the conversion summary", {
  skip_on_cran()
  skip_if_not_installed("tsibble")
  expect_message(
    tbl_now_to_tsibble(make_incidence_now(), verbose = TRUE),
    "tsibble"
  )
})

test_that("tbl_now_to_epidist verbose prints the conversion summary", {
  skip_on_cran()
  skip_if_not_installed("epidist")
  data(denguedat)
  ll <- tbl_now(denguedat,
    event_date = "onset_week", report_date = "report_week",
    verbose = FALSE
  )
  expect_message(
    suppressWarnings(tbl_now_to_epidist(ll, format = "linelist", verbose = TRUE)),
    "epidist"
  )
})

# ============================================================
# Remaining reachable branches
# ============================================================

test_that("tbl_now_from_data_table warns when input is not a data.table", {
  skip_on_cran()
  data(denguedat)
  expect_warning(
    tbl_now_from_data_table(as.data.frame(denguedat),
      event_date = "onset_week", report_date = "report_week",
      verbose = FALSE
    ),
    "not a"
  )
})

test_that("tbl_now_from_epinowcast sets strata to NULL when there are no extra columns", {
  skip_on_cran()
  skip_if_not_installed("epinowcast")
  obs <- data.frame(
    reference_date = as.Date("2021-01-01") + rep(c(0, 7, 14), each = 2),
    report_date    = as.Date("2021-01-01") + c(0, 7, 7, 14, 14, 21),
    confirm        = c(1, 3, 2, 5, 4, 7)
  )
  res <- tbl_now_from_epinowcast(obs,
    event_units = "weeks", report_units = "weeks",
    verbose = FALSE
  )
  expect_null(get_strata(res))
})

test_that("tbl_now_from_tsibble sets strata to NULL when the key holds only dates", {
  skip_on_cran()
  skip_if_not_installed("tsibble")
  df <- data.frame(
    event  = as.Date("2021-01-01") + rep(c(0, 7, 14), each = 2),
    report = as.Date("2021-01-01") + c(0, 7, 7, 14, 14, 21),
    n      = c(1, 3, 2, 5, 4, 7)
  )
  # Event date is the index, report date is the only key column (no strata).
  ts <- tsibble::as_tsibble(df, index = event, key = report)
  res <- tbl_now_from_tsibble(ts,
    report_date = "report",
    event_units = "weeks", report_units = "weeks",
    verbose = FALSE
  )
  expect_null(get_strata(res))
})

test_that("tbl_now_from_epidist interval errors on missing bound columns", {
  skip_on_cran()
  expect_error(
    tbl_now_from_epidist(data.frame(pdate_lwr = as.Date("2020-01-01")),
      format = "interval", verbose = FALSE
    ),
    "not found"
  )
})

test_that("tbl_now_to_epidist interval errors when named upper-bound columns are absent", {
  skip_on_cran()
  skip_if_not_installed("epidist")
  data(denguedat)
  ll <- tbl_now(denguedat,
    event_date = "onset_week", report_date = "report_week",
    verbose = FALSE
  )
  expect_error(
    suppressWarnings(
      tbl_now_to_epidist(ll,
        format = "interval",
        primary_upper = "no_such_col", secondary_upper = "nope",
        verbose = FALSE
      )
    ),
    "not found"
  )
})

# ============================================================
# from_* converters strip explicit zeros (minimal tbl_now)
# ============================================================

test_that("tbl_now_from_epinowcast drops zero-confirm rows (minimal tbl_now)", {
  skip_on_cran()
  skip_if_not_installed("epinowcast")
  obs <- head(epinowcast::germany_covid19_hosp, 300)
  res <- tbl_now_from_epinowcast(obs,
    strata = c("location", "age_group"),
    verbose = FALSE
  )
  expect_equal(sum(res[["confirm"]] == 0, na.rm = TRUE), 0L)
})

test_that("tbl_now_from_baselinenowcast drops zero cells but preserves totals", {
  skip_on_cran()
  skip_if_not_installed("baselinenowcast")
  m <- matrix(c(10, 5, 0, 8, 0, 2, 6, 0, 0),
    nrow = 3, byrow = TRUE,
    dimnames = list(
      c("2020-01-01", "2020-01-02", "2020-01-03"),
      c("0", "1", "2")
    )
  )
  res <- tbl_now_from_baselinenowcast(m, verbose = FALSE)
  expect_equal(sum(res[["count"]] == 0), 0L)
  expect_equal(sum(res[["count"]]), sum(m)) # totals preserved
  expect_equal(nrow(res), sum(m > 0)) # one row per non-zero cell
})

# ============================================================
# epinowcast: from a preprocessed object (not just raw input)
# ============================================================

test_that("tbl_now_from_epinowcast accepts a preprocessed enw_preprocess_data object", {
  skip_on_cran()
  skip_if_not_installed("epinowcast")

  obs <- head(epinowcast::germany_covid19_hosp, 400)
  from_raw <- tbl_now_from_epinowcast(obs,
    strata = c("location", "age_group"),
    verbose = FALSE
  )
  pre <- tbl_now_to_epinowcast(from_raw, verbose = FALSE, quiet = TRUE)

  from_pre <- tbl_now_from_epinowcast(pre, verbose = FALSE)
  expect_true(is_tbl_now(from_pre))
  expect_equal(get_data_type(from_pre), "count-cumulative")
  # grouping is auto-detected from the preprocessed object's `by`
  expect_setequal(get_strata(from_pre), c("location", "age_group"))
})

test_that("epinowcast preprocessed round-trip preserves rows (up to max_delay)", {
  skip_on_cran()
  skip_if_not_installed("epinowcast")

  obs <- head(epinowcast::germany_covid19_hosp, 400)
  from_raw <- tbl_now_from_epinowcast(obs,
    strata = c("location", "age_group"),
    verbose = FALSE
  )
  pre <- tbl_now_to_epinowcast(from_raw, verbose = FALSE, quiet = TRUE)
  from_pre <- tbl_now_from_epinowcast(pre, verbose = FALSE)

  # the round-trip recovers the same observations
  expect_equal(nrow(from_pre), nrow(from_raw))
  expect_equal(sum(from_pre[["confirm"]]), sum(from_raw[["confirm"]]))
})

# # ============================================================
# # EpiNow2: estimate_truncation snapshots
# # ============================================================
#
# test_that("tbl_now_to_EpiNow2 model='estimate_truncation' returns a list of snapshots", {
#   skip_if_not_installed("data.table")
#
#   data(mpoxdat)
#   tn <- tbl_now(mpoxdat,
#     event_date = "dx_date", report_date = "dx_report_date",
#     case_count = "n", strata = "race", data_type = "count-incidence",
#     verbose = FALSE
#   )
#   snaps <- tbl_now_to_EpiNow2(tn, model = "estimate_truncation", verbose = FALSE)
#
#   expect_type(snaps, "list")
#   expect_true(length(snaps) >= 2)
#   expect_true(all(vapply(snaps, function(s) all(names(s) == c("date", "confirm")), logical(1))))
#   # snapshots grow (later vintages have at least as many dates as earlier ones)
#   expect_true(nrow(snaps[[length(snaps)]]) >= nrow(snaps[[1]]))
# })
#
# test_that("EpiNow2 truncation's final snapshot equals the estimate_infections series", {
#   skip_if_not_installed("data.table")
#
#   data(mpoxdat)
#   tn <- tbl_now(mpoxdat,
#     event_date = "dx_date", report_date = "dx_report_date",
#     case_count = "n", strata = "race", data_type = "count-incidence",
#     verbose = FALSE
#   )
#   inf <- tbl_now_to_EpiNow2(tn, verbose = FALSE)
#   snaps <- tbl_now_to_EpiNow2(tn, model = "estimate_truncation", verbose = FALSE)
#
#   expect_equal(as.data.frame(snaps[[length(snaps)]]),
#     as.data.frame(inf),
#     ignore_attr = TRUE
#   )
# })
#
# test_that("tbl_now_to_EpiNow2 errors on an unknown model", {
#   skip_if_not_installed("data.table")
#   data(denguedat)
#   tn <- tbl_now(denguedat,
#     event_date = "onset_week", report_date = "report_week",
#     verbose = FALSE
#   )
#   expect_error(tbl_now_to_EpiNow2(tn, model = "nonsense"), "should be|arg")
# })

# ============================================================
# Covariate / is_censored preservation in tabular outputs
# ============================================================

make_rich_now <- function() {
  d <- dplyr::tibble(
    ev   = as.Date("2020-01-01") + rep(c(0, 7, 14), each = 2),
    rp   = as.Date("2020-01-01") + c(0, 7, 7, 14, 14, 21),
    grp  = rep(c("A", "B"), 3),
    temp = c(10, 11, 12, 13, 14, 15),
    flag = c(FALSE, TRUE, FALSE, FALSE, TRUE, FALSE),
    n    = c(3, 2, 4, 1, 5, 2)
  )
  tbl_now(d, event_date = ev, report_date = rp, case_count = n, strata = grp,
          covariates = temp, is_censored = flag, data_type = "count-incidence",
          event_units = "weeks", report_units = "weeks", verbose = FALSE)
}

test_that("tbl_now_to_baselinenowcast long keeps covariates and is_censored", {
  skip_on_cran()
  skip_if_not_installed("baselinenowcast")
  long <- tbl_now_to_baselinenowcast(make_rich_now(), format = "long",
                                     verbose = FALSE, quiet = TRUE)
  expect_true(all(c("temp", "flag") %in% names(long)))
})

test_that("tbl_now_to_baselinenowcast matrix keeps only the core columns", {
  skip_on_cran()
  skip_if_not_installed("baselinenowcast")
  mx <- suppressMessages(
    tbl_now_to_baselinenowcast(make_rich_now(), format = "matrix",
                               delays_unit = "weeks", verbose = FALSE,
                               quiet = TRUE)
  )
  expect_s3_class(mx, "reporting_triangle")
})

test_that("tbl_now_to_tsibble keeps covariates/is_censored as measurements", {
  skip_on_cran()
  skip_if_not_installed("tsibble")
  ts <- tbl_now_to_tsibble(make_rich_now(), verbose = FALSE)
  expect_true(all(c("temp", "flag") %in% names(ts)))
  # they are measurement columns, NOT part of the key
  expect_false(any(c("temp", "flag") %in% tsibble::key_vars(ts)))
})

test_that("tbl_now_to_data_table keeps every column", {
  skip_on_cran()
  skip_if_not_installed("data.table")
  dt <- tbl_now_to_data_table(make_rich_now(), verbose = FALSE)
  expect_true(all(c("temp", "flag") %in% names(dt)))
})

test_that("tbl_now_to_epidist linelist carries covariates and is_censored", {
  skip_on_cran()
  skip_if_not_installed("epidist")
  ll <- tbl_now(
    dplyr::tibble(
      ev   = as.Date(c("2020-03-01", "2020-03-02")),
      rp   = as.Date(c("2020-03-05", "2020-03-04")),
      temp = c(10, 11)
    ),
    event_date = ev, report_date = rp, covariates = temp,
    data_type = "linelist", event_units = "days", report_units = "days",
    verbose = FALSE
  )
  out <- suppressMessages(tbl_now_to_epidist(ll, format = "linelist",
                                             verbose = FALSE, quiet = TRUE))
  expect_true("temp" %in% names(out))
})

# ============================================================
# Reverse S3 methods on the other packages' generics
# ============================================================

test_that("as_tsibble.tbl_now dispatches to tbl_now_to_tsibble", {
  skip_if_not_installed("tsibble")
  ts <- suppressWarnings(tsibble::as_tsibble(make_rich_now()))
  expect_s3_class(ts, "tbl_ts")
})

test_that("as.data.table.tbl_now dispatches to tbl_now_to_data_table", {
  skip_if_not_installed("data.table")
  dt <- data.table::as.data.table(make_rich_now())
  expect_s3_class(dt, "data.table")
  expect_false(is_tbl_now(dt))
})

test_that("as_reporting_triangle.tbl_now dispatches (matrix format)", {
  skip_if_not_installed("baselinenowcast")
  rt <- suppressMessages(
    baselinenowcast::as_reporting_triangle(make_rich_now(), delays_unit = "weeks",
                                           quiet = TRUE)
  )
  expect_s3_class(rt, "reporting_triangle")
})

test_that("as_epidist_linelist_data.tbl_now dispatches to tbl_now_to_epidist", {
  skip_if_not_installed("epidist")
  ll <- tbl_now(
    dplyr::tibble(ev = as.Date(c("2020-03-01", "2020-03-02")),
                  rp = as.Date(c("2020-03-05", "2020-03-04"))),
    event_date = ev, report_date = rp, data_type = "linelist",
    event_units = "days", report_units = "days", verbose = FALSE
  )
  out <- suppressMessages(epidist::as_epidist_linelist_data(ll, quiet = TRUE))
  expect_true(epidist::is_epidist_linelist_data(out))
})

# ============================================================
# Lossy-conversion warning + attribute stripping
# ============================================================

# The custom attributes the tbl_now class attaches; none of them should ride
# along onto a converted-out object.
tbl_now_meta_attrs <- c(
  "event_date", "report_date", "case_count", "strata", "covariates", "now",
  "event_units", "report_units", "data_type", "is_censored",
  "temporal_effects", "computed_temporal_effect_cols"
)

test_that("tbl_now_to_epinowcast warns it is lossy, silenced by quiet = TRUE", {
  skip_on_cran()
  skip_if_not_installed("epinowcast")
  obs <- head(epinowcast::germany_covid19_hosp, 200)
  nowobj <- tbl_now_from_epinowcast(obs,
    strata = c("location", "age_group"), verbose = FALSE
  )
  expect_warning(
    suppressMessages(tbl_now_to_epinowcast(nowobj, verbose = FALSE)),
    "lossy"
  )
  expect_no_warning(
    suppressMessages(
      tbl_now_to_epinowcast(nowobj, verbose = FALSE, quiet = TRUE)
    )
  )
})


test_that("tbl_now_to_data_table drops every tbl_now attribute", {
  skip_on_cran()
  skip_if_not_installed("data.table")
  data(denguedat)
  dt <- data.table::as.data.table(denguedat)
  nowobj <- tbl_now_from_data_table(dt,
    event_date = "onset_week", report_date = "report_week", verbose = FALSE
  )
  dt2 <- tbl_now_to_data_table(nowobj, verbose = FALSE)

  expect_false(is_tbl_now(dt2))
  expect_length(intersect(names(attributes(dt2)), tbl_now_meta_attrs), 0)
  # The original three columns are recovered exactly (no leftover attributes).
  expect_equal(dt, dt2[, 1:3])
})

test_that("tbl_now_to_tsibble drops every tbl_now attribute", {
  skip_on_cran()
  skip_if_not_installed("tsibble")
  data(denguedat)
  nowobj <- tbl_now(denguedat,
    event_date = "onset_week", report_date = "report_week", verbose = FALSE
  )
  ts <- suppressWarnings(tbl_now_to_tsibble(nowobj, verbose = FALSE))

  expect_true(tsibble::is_tsibble(ts))
  expect_length(intersect(names(attributes(ts)), tbl_now_meta_attrs), 0)
})

# ============================================================
# Temporal effects carried into converters as covariates
# ============================================================

# A daily count-incidence tbl_now carrying a *lazy* temporal-effects spec
# (day_of_week + a Fourier season). No columns are materialised on it.
make_temporal_now <- function() {
  d <- dplyr::tibble(
    ev  = as.Date("2020-01-01") + rep(0:6, each = 2),
    rp  = as.Date("2020-01-01") + rep(0:6, each = 2) + c(0L, 1L),
    grp = rep(c("A", "B"), 7),
    n   = c(3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9, 7)
  )
  tbl_now(d,
    event_date = ev, report_date = rp, case_count = n, strata = grp,
    data_type = "count-incidence", event_units = "days", report_units = "days",
    t_effects = temporal_effects(day_of_week = TRUE, seasons = 7),
    verbose = FALSE
  )
}

# The column names compute_temporal_effects() would create for that spec.
temporal_now_cols <- c(
  ".event_day_of_week", ".event_season_7_cos", ".event_season_7_sin"
)

test_that("converting a lazy spec does not mutate the input tbl_now", {
  skip_on_cran()
  skip_if_not_installed("data.table")
  x <- make_temporal_now()
  invisible(tbl_now_to_data_table(x, verbose = FALSE))
  # The spec is still there but no columns have been materialised on `x`.
  expect_length(get_temporal_effect_cols(x), 0)
  expect_false(any(temporal_now_cols %in% names(x)))
})

test_that("tbl_now_to_data_table materialises temporal effects as columns", {
  skip_on_cran()
  skip_if_not_installed("data.table")
  dt <- tbl_now_to_data_table(make_temporal_now(), verbose = FALSE)
  expect_true(all(temporal_now_cols %in% names(dt)))
})

test_that("tbl_now_to_tsibble carries temporal effects as measurements", {
  skip_on_cran()
  skip_if_not_installed("tsibble")
  ts <- tbl_now_to_tsibble(make_temporal_now(), verbose = FALSE)
  expect_true(all(temporal_now_cols %in% names(ts)))
  # they are measurement columns, NOT part of the key
  expect_false(any(temporal_now_cols %in% tsibble::key_vars(ts)))
})

test_that("tbl_now_to_baselinenowcast long carries temporal effects", {
  skip_on_cran()
  skip_if_not_installed("baselinenowcast")
  long <- tbl_now_to_baselinenowcast(make_temporal_now(), format = "long",
                                     verbose = FALSE)
  expect_true(all(temporal_now_cols %in% names(long)))
})

test_that("tbl_now_to_baselinenowcast matrix cannot carry temporal effects", {
  skip_on_cran()
  skip_if_not_installed("baselinenowcast")
  mx <- suppressMessages(
    tbl_now_to_baselinenowcast(make_temporal_now(), format = "matrix",
                               verbose = FALSE)
  )
  expect_s3_class(mx, "reporting_triangle")
})

test_that("tbl_now_to_epidist carries temporal effects as covariates", {
  skip_on_cran()
  skip_if_not_installed("epidist")
  out <- suppressMessages(
    tbl_now_to_epidist(make_temporal_now(), verbose = FALSE, quiet = TRUE)
  )
  expect_true(all(temporal_now_cols %in% names(out)))
})

test_that("tbl_now_to_epinowcast carries temporal effects into obs + metareference", {
  skip_on_cran()
  skip_if_not_installed("epinowcast")
  pre <- suppressMessages(suppressWarnings(
    tbl_now_to_epinowcast(make_temporal_now(), verbose = FALSE, quiet = TRUE)
  ))
  expect_true(all(temporal_now_cols %in% names(pre$obs[[1]])))
  # event-based effects land in the per-reference-date metareference table,
  # which is what epinowcast's reference module reads covariates from.
  expect_true(all(temporal_now_cols %in% names(pre$metareference[[1]])))
})

test_that("temporal effects cover every completed row, including the horizon", {
  skip_on_cran()
  skip_if_not_installed("epinowcast")
  skip_if_not_installed("almanac")

  # Weekly data with a calendar effect and a Fourier term. `enw_complete_dates()`
  # fills the (reference, report) grid and extends the reference axis past the
  # data; the effects are derived from that grid, so no row can be left NA.
  data(denguedat)
  dengue <- denguedat[denguedat$onset_week >= as.Date("2009-01-01"), ]
  weekly <- tbl_now(dengue,
    event_date = onset_week, report_date = report_week,
    data_type = "linelist", verbose = FALSE
  ) |>
    add_temporal_effects(
      temporal_effects(week_of_year = TRUE, seasons = 52,
                       holidays = almanac::rcalendar(almanac::hol_christmas()),
                       holiday_lags = 1)
    )

  completed <- suppressWarnings(
    tbl_now_to_epinowcast(weekly, verbose = FALSE, quiet = TRUE, preprocess = FALSE)
  )
  effect_cols <- grep("^[.]event", names(completed), value = TRUE)
  expect_setequal(
    effect_cols,
    c(".event_week_of_year", ".event_season_52_cos", ".event_season_52_sin",
      ".event_holiday", ".event_holiday_lag_1")
  )
  for (column in effect_cols) {
    expect_false(anyNA(completed[[column]]), label = column)
  }

  # And they still cover every row once epinowcast lays out its metadata tables.
  preprocessed <- suppressWarnings(
    tbl_now_to_epinowcast(weekly, verbose = FALSE, quiet = TRUE)
  )
  metareference <- preprocessed$metareference[[1]]
  for (column in intersect(effect_cols, names(metareference))) {
    expect_false(anyNA(metareference[[column]]), label = column)
  }
})

test_that("effects on the completed grid match what the object itself computes", {
  skip_on_cran()
  skip_if_not_installed("epinowcast")
  skip_if_not_installed("almanac")

  # Deriving from the completed grid must reproduce the object's own values: the
  # Fourier anchor (earliest event date) and the calendar are the same, so the
  # only difference is the extra horizon rows.
  data(denguedat)
  dengue <- denguedat[denguedat$onset_week >= as.Date("2009-01-01"), ]
  effect_columns <- c(".event_week_of_year", ".event_season_52_cos",
                      ".event_season_52_sin", ".event_holiday")
  weekly <- tbl_now(dengue,
    event_date = onset_week, report_date = report_week,
    data_type = "linelist", verbose = FALSE
  ) |>
    add_temporal_effects(
      temporal_effects(week_of_year = TRUE, seasons = 52,
                       holidays = almanac::rcalendar(almanac::hol_christmas()))
    )

  from_object <- compute_temporal_effects(weekly) |>
    as.data.frame() |>
    dplyr::distinct(reference_date = onset_week,
                    dplyr::across(dplyr::all_of(effect_columns)))
  from_grid <- suppressWarnings(
    tbl_now_to_epinowcast(weekly, verbose = FALSE, quiet = TRUE, preprocess = FALSE)
  ) |>
    as.data.frame() |>
    dplyr::mutate(reference_date = as.Date(.data$reference_date)) |>
    dplyr::distinct(.data$reference_date,
                    dplyr::across(dplyr::all_of(effect_columns)))

  shared <- dplyr::inner_join(from_object, from_grid, by = "reference_date",
                              suffix = c("_object", "_grid"))
  expect_gt(nrow(shared), 0)
  for (column in effect_columns) {
    expect_equal(shared[[paste0(column, "_object")]],
                 shared[[paste0(column, "_grid")]], label = column)
  }

  # The grid additionally covers the horizon weeks the data never reached.
  expect_gt(
    length(setdiff(from_grid$reference_date, from_object$reference_date)), 0
  )
})

test_that("holiday effects are carried as a covariate column", {
  skip_on_cran()
  skip_if_not_installed("data.table")
  skip_if_not_installed("almanac")
  cal <- almanac::rcalendar(almanac::hol_new_years_day())
  x <- tbl_now(
    dplyr::tibble(
      ev = as.Date("2020-01-01") + 0:3,
      rp = as.Date("2020-01-01") + 0:3 + 1L,
      n  = c(1, 2, 3, 4)
    ),
    event_date = ev, report_date = rp, case_count = n,
    data_type = "count-incidence", event_units = "days", report_units = "days",
    t_effects = temporal_effects(holidays = cal), verbose = FALSE
  )
  dt <- tbl_now_to_data_table(x, verbose = FALSE)
  expect_true(".event_holiday" %in% names(dt))
  # 2020-01-01 is New Year's Day -> flagged 1, the rest 0.
  expect_equal(dt[[".event_holiday"]], c(1L, 0L, 0L, 0L))
})

test_that("already-computed temporal effects are not recomputed or duplicated", {
  skip_on_cran()
  skip_if_not_installed("data.table")
  x  <- compute_temporal_effects(make_temporal_now())
  dt <- tbl_now_to_data_table(x, verbose = FALSE)
  expect_true(all(temporal_now_cols %in% names(dt)))
  # no duplicated column names introduced by a second computation
  expect_false(any(duplicated(names(dt))))
})

# ---------------------------------------------------------------------------
# epinowcast timestep (weekly data must not be laid out on a daily grid)
# ---------------------------------------------------------------------------

test_that(".epinowcast_timestep maps report units onto what epinowcast accepts", {
  skip_on_cran()
  expect_identical(.epinowcast_timestep("days"), "day")
  expect_identical(.epinowcast_timestep("weeks"), "week")

  # epinowcast supports only "day", "week" or a whole number of days, so units it
  # cannot express must say so rather than silently fall back to "day".
  expect_error(.epinowcast_timestep("months"), "timestep")
  expect_error(.epinowcast_timestep("years"), "timestep")
  expect_error(.epinowcast_timestep("numeric"), "timestep")
})

test_that("tbl_now_to_epinowcast puts weekly data on a weekly grid", {
  skip_on_cran()
  skip_if_not_installed("epinowcast")

  data(denguedat)
  dengue <- denguedat[denguedat$onset_week >= as.Date("2009-01-01"), ]
  weekly <- tbl_now(dengue,
    event_date = onset_week, report_date = report_week,
    data_type = "linelist", verbose = FALSE
  )
  expect_identical(get_report_units(weekly), "weeks")

  enw <- suppressWarnings(
    tbl_now_to_epinowcast(weekly, verbose = FALSE, quiet = TRUE)
  )
  reference_dates <- sort(unique(enw$metareference[[1]]$date))

  # The reference grid must step in weeks, not days: `.delay` is measured in the
  # object's report units, so a daily grid would both misread `max_delay` (weeks
  # read as days) and strand the weekly covariates on rows that do not exist.
  expect_true(all(diff(reference_dates) == 7))
})

test_that("tbl_now_to_epinowcast reads max_delay in the inferred timestep", {
  skip_on_cran()
  skip_if_not_installed("epinowcast")

  data(denguedat)
  dengue <- denguedat[denguedat$onset_week >= as.Date("2009-01-01"), ]
  weekly <- tbl_now(dengue,
    event_date = onset_week, report_date = report_week,
    data_type = "linelist", verbose = FALSE
  )
  max_delay_weeks <- as.integer(max(as.data.frame(weekly)$.delay)) + 1L

  completed <- suppressWarnings(
    tbl_now_to_epinowcast(weekly, verbose = FALSE, quiet = TRUE, preprocess = FALSE)
  )
  observed_delays <- as.numeric(
    completed$report_date - completed$reference_date
  ) / 7

  # max_delay counts timesteps, so on weekly data it must bound the delay in
  # WEEKS. Left on epinowcast's "day" default it would have truncated a 14-week
  # delay range at 15 days (~2 weeks).
  expect_lte(max(observed_delays), max_delay_weeks)
  expect_gt(max(observed_delays), 2)
})

test_that("tbl_now_to_epinowcast infers a daily timestep for daily data", {
  skip_on_cran()
  skip_if_not_installed("epinowcast")

  set.seed(3)
  days <- seq(as.Date("2023-01-01"), as.Date("2023-03-31"), by = "day")
  rows <- do.call(rbind, lapply(days, function(day) {
    max_delay <- rpois(1, 3)
    data.frame(onset = day, reported = day + 0:max_delay, n = rpois(max_delay + 1, 5))
  }))
  daily <- tbl_now(rows,
    event_date = onset, report_date = reported, case_count = n,
    data_type = "count-incidence", verbose = FALSE
  )
  expect_identical(get_report_units(daily), "days")

  enw <- suppressWarnings(
    tbl_now_to_epinowcast(daily, verbose = FALSE, quiet = TRUE)
  )
  reference_dates <- sort(unique(enw$metareference[[1]]$date))
  expect_true(all(diff(reference_dates) == 1))
})

# -- strata are carried into the model converters (issue: strata passing) ------

make_strata_tbl <- function() {
  data(denguedat, package = "tbl.now", envir = environment())
  d <- denguedat[denguedat$onset_week >= as.Date("2009-06-01"), ]
  tbl_now(d, event_date = onset_week, report_date = report_week,
          strata = gender, data_type = "linelist", verbose = FALSE)
}

test_that("tbl_now_to_baselinenowcast carries strata in long format and pools for the matrix", {
  skip_on_cran()
  skip_if_not_installed("baselinenowcast")
  tn <- make_strata_tbl()

  long <- suppressWarnings(suppressMessages(
    tbl_now_to_baselinenowcast(tn, format = "long", verbose = FALSE)
  ))
  expect_true("gender" %in% names(long))

  # a single triangle has no strata dimension: pool with a warning, not a crash
  expect_warning(
    mat <- tbl_now_to_baselinenowcast(tn, format = "matrix", verbose = FALSE),
    "pooling over strata"
  )
  expect_true(is.matrix(mat))
})

test_that("tbl_now_to_epidist carries strata as a column", {
  skip_on_cran()
  skip_if_not_installed("epidist")
  tn <- make_strata_tbl()
  ed <- suppressWarnings(suppressMessages(tbl_now_to_epidist(tn, verbose = FALSE)))
  expect_true("gender" %in% names(ed))
})

test_that("tbl_now_to_epinowcast passes strata as grouping", {
  skip_on_cran()
  skip_if_not_installed("epinowcast")
  tn  <- make_strata_tbl()
  enw <- suppressWarnings(suppressMessages(
    tbl_now_to_epinowcast(tn, verbose = FALSE, quiet = TRUE)
  ))
  expect_equal(length(unique(enw$metareference[[1]]$.group)), 2L)
})

# format = "triangle_list" ------------------------------------------------------

make_strata_tbl_now <- function() {
  data(denguedat, envir = environment())
  denguedat |>
    dplyr::filter(onset_week >= as.Date("2010-01-01")) |>
    tbl_now(
      event_date = onset_week, report_date = report_week,
      strata = gender, data_type = "linelist", verbose = FALSE
    )
}

test_that("format = 'triangle_list' returns one triangle per stratum", {
  skip_if_not_installed("baselinenowcast")
  x  <- make_strata_tbl_now()
  tl <- suppressWarnings(
    tbl_now_to_baselinenowcast(x, format = "triangle_list", verbose = FALSE)
  )

  expect_s3_class(tl, "tbl_now_triangle_list")
  # Thin class: it must still behave as a plain list.
  expect_true(is.list(tl))
  expect_setequal(names(tl), unique(as.character(x$gender)))
  for (triangle in tl) expect_s3_class(triangle, "reporting_triangle")
  expect_equal(attr(tl, "strata_cols"), "gender")
  expect_equal(attr(tl, "now"), get_now(x))
})

test_that("format = 'triangle_list' is length-1 and named 'all' without strata", {
  skip_if_not_installed("baselinenowcast")
  x <- remove_all_strata(make_strata_tbl_now())
  tl <- suppressWarnings(
    tbl_now_to_baselinenowcast(x, format = "triangle_list", verbose = FALSE)
  )

  # The return type must not depend on whether strata happen to be attached.
  expect_s3_class(tl, "tbl_now_triangle_list")
  expect_length(tl, 1L)
  expect_named(tl, "all")
})

test_that("as_tbl_now() rebuilds a tbl_now from a triangle list, strata and all", {
  skip_if_not_installed("baselinenowcast")
  x  <- make_strata_tbl_now()
  tl <- suppressWarnings(
    tbl_now_to_baselinenowcast(x, format = "triangle_list", verbose = FALSE)
  )
  back <- as_tbl_now(tl)

  expect_true(is_tbl_now(back))
  expect_equal(get_strata(back), "gender")
  expect_equal(get_now(back), get_now(x))
  expect_equal(get_event_units(back), get_event_units(x))
  expect_setequal(unique(as.character(back$gender)), unique(as.character(x$gender)))

  # No cases gained or lost, per stratum.
  original <- x |> as.data.frame() |> dplyr::count(gender, name = "n")
  rebuilt  <- back |>
    as.data.frame() |>
    dplyr::summarise(n = sum(count, na.rm = TRUE), .by = gender)
  expect_equal(
    rebuilt$n[order(rebuilt$gender)],
    as.numeric(original$n[order(original$gender)])
  )
})

test_that("a weekly reporting triangle survives the round-trip through as_tbl_now()", {
  skip_if_not_installed("baselinenowcast")
  # Regression: `delays_unit` was not read from the triangle's own attribute, so
  # weekly delays were expanded as days and `as_tbl_now()` aborted with
  # "report_units must be coarser than or equal to event_units".
  x <- remove_all_strata(make_strata_tbl_now())
  triangle <- suppressWarnings(tbl_now_to_baselinenowcast(x, verbose = FALSE))
  expect_equal(attr(triangle, "delays_unit"), "weeks")

  back <- suppressWarnings(suppressMessages(as_tbl_now(triangle)))
  expect_true(is_tbl_now(back))
  expect_equal(get_event_units(back), "weeks")
  expect_equal(
    max(as.numeric(back$report_date - back$reference_date)),
    max(as.numeric(x$report_week - x$onset_week))
  )
})
