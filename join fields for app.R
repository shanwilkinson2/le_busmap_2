# join for app 

library(tidyverse)
library(sf)

# read in files

le <- readRDS("life_expectancy.RDS")
hle <- readRDS("healthy_life_expectancy.RDS")
bus <- readRDS("bus_routes.RDS")
            
# join - long form
le_hle <- bind_rows(
  le |> select(area_code, area_name, time_period, sex, metric, value),
  hle |> select(area_code, area_name, time_period, sex, metric, value),
)

le_hle_2 <- le_hle |>
  # calculate years not in good health
  group_by(area_code, area_name, time_period, sex) |>
  summarise(
    value = first(value[metric == "Life Expectancy"]) -
      first(value[metric == "Healthy Life Expectancy"]),
    .groups = "drop"
  ) |>
  mutate(metric = "Years not in good health") |>
  bind_rows(le_hle)

bus_le_hle <- bus |>
  left_join(le_hle_2, by = c("MSOA21CD" = "area_code"), relationship = "many-to-many") |>
  mutate(route_shortname_description = paste0(route_short_name, ": ", route_description,
                                              " (", agency_name, ")"),
         # will make renderDT table sorts work better e.g. dropdown for factor
         sex = as.factor(sex), 
         metric = as.factor(metric), 
         stop_sequence = as.integer(stop_sequence)
         ) |>
  # some routes going to Scotland don't have any values, remove stops with no LA 
  filter(!is.na(la)) |>
  # turn to a geospatial file 
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326)

# summary dataset
  route_summaries <- bus_le_hle |>
    st_drop_geometry() |>
    group_by(route_short_name, agency_name, route_description, route_shortname_description, 
             sex, metric) |>
    summarise(
      num_stops = max(stop_sequence, na.rm = TRUE),
      min = min(value, na.rm = TRUE),
      max = max(value, na.rm = TRUE),
      median = median(value, na.rm = TRUE), 
      range = max-min,
      .groups = "drop"
    ) 

# routes with a stop in which borough 
  route_boroughs <- bus_le_hle |>
    distinct(route_short_name, agency_name, route_shortname_description, la) %>%
    filter(la %in% 
             c("Bolton", "Bury", "Manchester", "Oldham", "Rochdale", 
               "Salford", "Stockport", "Tameside", "Trafford", "Wigan"))
  
# save
  saveRDS(bus_le_hle, "./app/bus_le_hle.RDS")
  saveRDS(route_summaries, "./app/route_summaries.RDS")
  saveRDS(route_boroughs, "./app/route_boroughs.RDS")
