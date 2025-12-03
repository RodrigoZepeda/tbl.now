test_that("update keeps everything similar when nothing new is observed", {
  data(denguedat)

  initial_data  <- denguedat[1:500,]
  initial_tbl   <- tbl_now(denguedat, event_date = "onset_week",
                           report_date = "report_week", strata = "gender",
                           verbose = FALSE) %>%
    to_count(to = "count-incidence")

  #Update doesn't matter if has old values
  expect_equal(
    initial_tbl,
    update(initial_tbl, new_data = initial_tbl)
  )

  subset1 <- initial_tbl %>% dplyr::filter(report_week <= as.Date("1990-06-11"))
  subset2 <- initial_tbl %>% dplyr::filter(report_week > as.Date("1990-06-11"))

  #Partitioning the data doesn't change the update
  expect_equal(
    initial_tbl %>% dplyr::arrange(onset_week, report_week, gender),
    update(subset1, new_data = subset2) %>% dplyr::arrange(onset_week, report_week, gender)
  )

  #Partitioning the data and keeping everything doesn't change the update
  expect_equal(
    initial_tbl %>% dplyr::arrange(onset_week, report_week, gender),
    update(subset1, new_data = initial_tbl) %>% dplyr::arrange(onset_week, report_week, gender)
  )

  #Update with empty tbl
  expect_equal(
    initial_tbl,
    update(initial_tbl, new_data = initial_tbl %>% dplyr::filter(.delay > Inf))
  )

})
