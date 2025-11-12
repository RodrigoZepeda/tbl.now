library(tidyverse)
flusight <- read_csv("data-raw/time-series.csv")
flusight <- flusight %>%
  select(-location, -target, -weekly_rate)
save(flusight, file = "data/flusight.rda")
