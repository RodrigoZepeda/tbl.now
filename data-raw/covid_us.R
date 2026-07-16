# =============================================================================
# covid_us: CDC COVID-19 Case Surveillance Public Use Data, aggregated for the
# batch-detection case study.
#
# Source file (14 GB, 106M rows) is NOT shipped; download it from
#   https://data.cdc.gov/Case-Surveillance/COVID-19-Case-Surveillance-Public-Use-Data/vbim-akqf/about_data
# and point `csv` at it. We read it with duckdb (streaming) because it does not
# fit in memory, keep cases whose event (cdc_case_earliest_dt) AND report
# (cdc_report_dt) both fall between 2020-01-01 and 2021-12-31 -- a self-consistent
# "as of the end of 2021" snapshot, so the epidemic and its reporting are seen
# over the same two years -- drop the handful of rows whose report precedes the
# event (data-entry errors), and tally by (event date, report date). The result
# is a compact count-incidence frame.
# =============================================================================

library(duckdb)
library(DBI)

csv <- "COVID-19_Case_Surveillance_Public_Use_Data_20260710.csv"

con <- dbConnect(duckdb::duckdb())
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
dbExecute(con, "SET threads TO 6; SET memory_limit='6GB';")

rd <- sprintf(
  "read_csv_auto('%s', header=true, ignore_errors=true, nullstr='')", csv
)

covid_us <- dbGetQuery(con, sprintf("
  SELECT cdc_case_earliest_dt AS cdc_case_earliest_dt,
         cdc_report_dt        AS cdc_report_dt,
         count(*)             AS n
  FROM %s
  WHERE cdc_case_earliest_dt IS NOT NULL
    AND cdc_report_dt IS NOT NULL
    AND cdc_case_earliest_dt BETWEEN DATE '2020-01-01' AND DATE '2021-12-31'
    AND cdc_report_dt >= cdc_case_earliest_dt
    AND cdc_report_dt <= DATE '2021-12-31'
  GROUP BY 1, 2
  ORDER BY 1, 2", rd))

covid_us$cdc_case_earliest_dt <- as.Date(covid_us$cdc_case_earliest_dt)
covid_us$cdc_report_dt        <- as.Date(covid_us$cdc_report_dt)
covid_us$n                    <- as.integer(covid_us$n)

usethis::use_data(covid_us, overwrite = TRUE, compress = "xz")
