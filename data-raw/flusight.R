# flusight ---------------------------------------------------------------------
#
# FluSight weekly influenza hospital-admission targets (CDC NHSN), as published
# in the FluSight forecast hub's `target-data/time-series.csv`.
#
# NOTE: this script re-downloads the CURRENT upstream file. The dataset shipped
# in data/flusight.rda is the snapshot downloaded on 2025-11-12; re-running this
# will replace it with whatever upstream holds today. Only run it when you
# intend to refresh the snapshot, and update the date in R/data.R if you do.

library(tidyverse)

flusight <- read_csv(
  paste0(
    "https://raw.githubusercontent.com/cdcepi/FluSight-forecast-hub/",
    "refs/heads/main/target-data/time-series.csv"
  )
)

flusight <- flusight |>
  select(-location, -target, -weekly_rate) |>
  # The upstream file itself ships exact duplicate rows -- 39,139 of them in the
  # 2025-11-12 snapshot (issue #25). They are true duplicates: every repeated
  # (as_of, target_end_date, location_name) key carries an identical
  # `observation`, with no conflicting values anywhere, so dropping them is
  # lossless and makes that triple a unique key. `target` has a single level
  # ("wk inc flu hosp") and `location` maps 1:1 onto `location_name`, so the
  # duplication is upstream and not an artefact of the `select()` above.
  distinct()

stopifnot(
  sum(duplicated(flusight)) == 0L,
  sum(duplicated(flusight[c("as_of", "target_end_date", "location_name")])) == 0L
)

usethis::use_data(flusight, overwrite = TRUE, compress = "xz")
