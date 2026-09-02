# =============================================================================
# covid_us: CDC COVID-19 Case Surveillance Public Use Data, aggregated for the
# batch-detection case study AND for the validation process.
#
# Source file (14 GB, 106M rows) is NOT shipped; download it from
#   https://data.cdc.gov/Case-Surveillance/COVID-19-Case-Surveillance-Public-Use-Data/vbim-akqf/about_data
# and point `csv` at it. We read it with duckdb (streaming) because it does not
# fit in memory.
#
# THE THREE DATES. The file carries four. `cdc_case_earliest_dt` is derived --
# CDC defines it as the earliest of the others -- and under the filter below it
# equals `onset_dt` for 99.997% of rows, so it is dropped as redundant. The
# three that are kept form the only chain that runs forward in time:
#
#   onset_dt       the event      symptoms begin
#   pos_spec_dt    the report     the first positive specimen is collected
#   cdc_report_dt  the validation the case is registered at CDC with a status
#
# so we keep only cases where all three are present and correctly ordered, and
# whose whole history falls inside 2020 -- a self-consistent "as of the end of
# 2020" snapshot. Rows out of order are data-entry errors (they are ~3% of the
# rows that have all three dates) and rows missing a date cannot be placed on
# the chain at all.
#
# `current_status` is kept VERBATIM, in CDC's own words, rather than recoded to
# the package's vocabulary: mapping it is what `tbl_now(validation_levels = )`
# is for, and the help page and the vignettes use it that way.
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
  SELECT onset_dt       AS onset_dt,
         pos_spec_dt    AS pos_spec_dt,
         cdc_report_dt  AS cdc_report_dt,
         current_status AS current_status,
         sex            AS sex,
         count(*)       AS n
  FROM %s
  WHERE onset_dt IS NOT NULL
    AND pos_spec_dt IS NOT NULL
    AND cdc_report_dt IS NOT NULL
    AND onset_dt BETWEEN DATE '2020-01-01' AND DATE '2020-12-31'
    AND cdc_report_dt <= DATE '2020-12-31'
    AND onset_dt <= pos_spec_dt
    AND pos_spec_dt <= cdc_report_dt
  GROUP BY 1, 2, 3, 4, 5
  ORDER BY 1, 2, 3, 4, 5", rd))

covid_us$onset_dt       <- as.Date(covid_us$onset_dt)
covid_us$pos_spec_dt    <- as.Date(covid_us$pos_spec_dt)
covid_us$cdc_report_dt  <- as.Date(covid_us$cdc_report_dt)
covid_us$n              <- as.integer(covid_us$n)

usethis::use_data(covid_us, overwrite = TRUE, compress = "xz")
