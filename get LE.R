# get life expectancy from fingertips local health

library(tidyverse)
library(janitor)

# LE at birth MSOA id: 93283

le <- read.csv("https://fingertips.phe.org.uk/api/all_data/csv/for_one_indicator?indicator_id=93283") |>
  clean_names()

le_2 <- le |>
  filter(area_type == "MSOA" & age == "All ages") |>
  mutate(metric = "LE") |>
  select(indicator_name, area_code, area_name, 
         time_period, time_period_sortable, sex, metric, value)|>
  group_by(area_code, sex) |>
  # if there are multiple years, keep the latest
  slice_max(time_period_sortable, n = 1)

# save
saveRDS(le_2, "life_expectancy.RDS")
