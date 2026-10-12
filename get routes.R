# get bus routes

library(tidyverse)
library(sf)
library(stringr)

# download data first
# https://data.bus-data.dft.gov.uk/timetable/download/ - GTFS north west

# get msoa boundaries (BGC)
# 2021 MSOA from https://geoportal.statistics.gov.uk/search?q=BDY_MSOA%20DEC_2021&sort=Title%7Ctitle%7Casc

#### data table contents ########################################################
#
# agency
# phone is mostly blank, noc is some kind of code
#   "agency_id", "agency_name", "agency_url", "agency_timezone", "agency_lang",
#   "agency_phone", "agency_noc"  

# routes
# route long name is all NA, route type = 3, 200, 0, 4
#   "route_id", "agency_id", "route_short_name", "route_long_name", "route_type"  

# trips
#   "route_id", "service_id", "trip_id", "trip_headsign", "direction_id",
#   "block_id", "shape_id", "wheelchair_accessible", "vehicle_journey_code" 

# stops
#   "stop_id", "stop_code", "stop_name", "stop_lat", "stop_lon", "wheelchair_boarding",
#   "location_type", "parent_station", "platform_code" 

# stop_times
#   "trip_id", "arrival_time", "departure_time", "stop_id", "stop_sequence", 
#   "stop_headsign", "pickup_type", "drop_off_type", "shape_dist_traveled", "timepoint"  

# shapes
# gives the actual route (rather than straightline betweeen stops)
#   "shape_id", "shape_pt_lat", "shape_pt_lon", "shape_pt_sequence", "shape_dist_traveled"

################################################################################

# load in route data 
  dir <- "gtfs_north_west"
  rd <- function(f) read_csv(file.path(dir, f), show_col_types = FALSE)
  
  routes <- rd("routes.txt"); trips <- rd("trips.txt")
  stop_times <- rd("stop_times.txt"); stops <- rd("stops.txt")
  agency <- rd("agency.txt"); shapes <- rd("shapes.txt")

# load in MSOA data  
  wanted_las <- c("Bolton", "Bury", "Manchester", "Oldham", "Rochdale", 
                  "Salford", "Stockport", "Tameside", "Trafford", "Wigan")
  wanted_las <- "Bolton"
  
  msoas <- st_read("Middle_layer_Super_Output_Areas_December_2021_Boundaries_EW_BGC_V3_-4477917303172606123.geojson")
  msoas$la <- str_sub(msoas$MSOA21NM, 1, -5) 
  msoas$wanted <- ifelse(msoas$la %in% wanted_las, TRUE, FALSE)
  # msoas <- msoas |> st_transform(27700) # British national grid  
  
# get routes that have at least 1 stop in the wanted area ########################
  
# join stops to msoa
  stops_msoa <- stops |>
    st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326, remove = FALSE) |>
    st_join(msoas |> select(MSOA21CD, MSOA21NM, la, wanted), join = st_within) |>
    st_drop_geometry() |>
    distinct(stop_id, .keep_all = TRUE) 
    
# get routes that include wanted stops  
route_stops <- stop_times |> 
  distinct(trip_id, stop_id) |>
  left_join(trips |> select(trip_id, route_id, trip_headsign), by = "trip_id") |>
  distinct(route_id, stop_id) |>
  left_join(routes |> select(route_id, agency_id, route_short_name), by = "route_id") |>
  left_join(agency |> select(agency_id, agency_name), by = "agency_id") |>
  left_join(stops_msoa |> select(stop_id, stop_name, stop_lat, stop_lon, 
                                 MSOA21CD, MSOA21NM, la, wanted), by = "stop_id") |>
  distinct(route_short_name, agency_name, stop_id, .keep_all = TRUE) |>
  group_by(route_short_name, agency_name) |>
  filter(any(wanted)) |>
  ungroup()

# shorter list - route shortname & operator that have at least 1 stop within the desired area
  # some route_short_name can come up under multiple route_ids
  wanted_routes <- route_stops |>
    distinct(route_short_name, agency_name)

##### get 1 for each route_short_name (the one with the most stops) #######

# end points & stops per trip
  trip_info <- stop_times |>
    arrange(trip_id, stop_sequence) |>
    group_by(trip_id) |>
    summarise(first_stop = first(stop_id), last_stop = last(stop_id),
      n_stops = n(), .groups = "drop") |>
    left_join(stops |> select(stop_id, first_name = stop_name), 
              by = c("first_stop" = "stop_id")) |>
    left_join(stops |> select(stop_id, last_name = stop_name), 
              by = c("last_stop" = "stop_id")) |>
    left_join(trips |> select(trip_id, trip_headsign, direction_id), by = "trip_id")

  patterns <- trips |>
    select(trip_id, route_id) |>
    left_join(routes |> select(route_id, agency_id, route_short_name), by = "route_id") |>
    left_join(agency |> select(agency_id, agency_name), by = "agency_id") |>
    semi_join(wanted_routes, by = c("route_short_name", "agency_name")) |>
    left_join(trip_info, by = "trip_id") |>
    distinct() |>
    group_by(route_short_name, agency_name) |>
    mutate(
      both_directions = all(c(0,1) %in% direction_id),
      num_versions = n()) |>
    ungroup()

# longest per direction to get name
longest_pattern <- patterns |>
  group_by(route_short_name, agency_name, direction_id) |>
  slice_max(n_stops, n= 1, with_ties = FALSE) |>
  ungroup() |>
  # remove stop number/ letter or stand number/ letter from the name 
  mutate(across(.cols = c(first_name, last_name, trip_headsign), 
                \(x) x |>
                  str_remove(fixed("(Manchester Metrolink)")) |>
                  str_replace(regex("^(.*)\\b(stop|stand|std)\\b.*$", ignore_case = TRUE), "\\1") |>
                  str_remove("[\\s(\\[-]+$") |>
                  str_trim()))
              

# make a name from the longest version of each route
# trip headsign for each direction, first stop & last stop for routes with only 1 direction
# national express & flixbus have the full route name in the headsign, so just that
route_description <- longest_pattern |>
  group_by(route_short_name, agency_name) |>
  summarise(
    route_description = if(str_detect(first(agency_name), regex("^national express|flixbus", ignore_case = TRUE))) {
      first(trip_headsign)
    } else if (n_distinct(direction_id, na.rm = TRUE) == 2) {
      paste(unique(trip_headsign[order(direction_id)]), collapse = " - ")
    }  else {
      f <- first(first_name)
      h <- first(trip_headsign)
      if(isTRUE(str_to_lower(f) == str_to_lower(h))){
        paste0(f, " (circular)")
      } else {
        paste(na.omit(c(f, h)), collapse = " - ")
      }
    }, 
    .groups = "drop"
  )

###### get route of stops of longest version of route shortname (1 direction only) ########
  
longest_per_route <- patterns |>
  group_by(route_short_name, agency_name) |>
  slice_max(n_stops, n= 1, with_ties = FALSE) |>
  ungroup() |>
  select(trip_id, route_id, agency_id, agency_name, route_short_name) |>
  left_join(stop_times |> select(trip_id, stop_id, stop_sequence), by = "trip_id") |>
  left_join(stops_msoa |> 
              select(stop_id, stop_code, stop_name, stop_lat, stop_lon,  
              la, MSOA21CD, MSOA21NM),
            by = "stop_id"
            ) |>
  # join in name
  left_join(route_description, by = c("route_short_name", "agency_name"))

# save 
saveRDS(longest_per_route, "bus_routes.RDS")
