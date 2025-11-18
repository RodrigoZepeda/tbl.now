library(tidyverse)
flusight <- read_csv("https://raw.githubusercontent.com/cdcepi/FluSight-forecast-hub/refs/heads/main/target-data/time-series.csv")
flusight <- flusight %>%
  select(-location, -target, -weekly_rate)
save(flusight, file = "data/flusight.rda")
