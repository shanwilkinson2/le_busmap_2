# get HLE

library(tidyverse)
library(janitor)

# HLE at birth MSOA (census based) id: 94359

hle <- read.csv("https://fingertips.phe.org.uk/api/all_data/csv/for_one_indicator?indicator_id=94359") |>
  clean_names()

hle_2 <- hle |>
  filter(area_type == "MSOA" & age == "All ages") |>
  mutate(metric = "HLE") |>
  select(indicator_name, area_code, area_name, 
         time_period, time_period_sortable, sex, metric, value)|>
  group_by(area_code, sex) |>
  # if there are multiple years, keep the latest
  slice_max(time_period_sortable, n = 1)

# save
saveRDS(hle_2, "healthy_life_expectancy.RDS")
