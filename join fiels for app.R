# join for app 

library(tidyverse)

# read in files

le <- readRDS("life_expectancy.RDS")
hle <- readRDS("healthy_life_expectancy.RDS")
bus <- readRDS("bus_routes.RDS")

le_hle <- bind_rows(
  le |> select(area_code, area_name, time_period, sex, metric, value),
  hle |> select(area_code, area_name, time_period, sex, metric, value),
)

bus_le_hle <- bus |>
  left_join(le_hle, by = c("MSOA21CD" = "area_code"), relationship = "many-to-many")
