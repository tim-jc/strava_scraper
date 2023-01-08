
# Define values -----------------------------------------------------------

# SQLite DB connection
con <- DBI::dbConnect(RSQLite::SQLite(), "~/Documents/Coding/R/Strava/strava_data.db")

# Manual activities to exclude (Strava API call fails if these aren't removed)
activities_to_remove <- c(6317473692, 5686021341, 1358368751, 1222988207, 574040145, 543631692)

# Peaks units / conversions
peaks_units <- tribble(
  ~metric, ~display_name, ~multiplier, ~units,
  "cadence", "cadence", 1, "rpm",
  "watts", "power", 1, "w",
  "heartrate", "heart rate", 1, "bpm",
  "velocity_smooth", "speed", 2.23694, "mph"
)

# Functions ---------------------------------------------------------------

get_stream_data <- function(activity_id, display_map = F) {
  
  str_glue("Starting activity {activity_id}.") %>% print()

  stream_types <- c("time", "latlng", "distance", "altitude", "velocity_smooth", "heartrate", "cadence", "watts", "temp", "moving", "grade_smooth") %>% 
    str_flatten(collapse = ",")
    
  stream <- GET(url = str_glue("https://www.strava.com/api/v3/activities/{activity_id}/streams/{stream_types}"),
                stoken)
  
  if(stream$status_code == 429) {
    stop("Rate limit exceeded")
  }
  
  stream_to_load <- stream$content %>%
    rawToChar() %>%
    jsonlite::fromJSON() %>% 
    unnest(data) %>% 
    group_by(type) %>% 
    mutate(record_id = row_number()) %>% 
    ungroup()
           
  if(any(stream_to_load$type == "latlng")) {
    stream_to_load <- stream_to_load %>% 
      mutate(value = data[,1])
    
    stream_to_load <- stream_to_load %>% 
      select(record_id, type, value) %>% 
      pivot_wider(names_from = "type") %>% 
      rename(lat = latlng) %>% 
      left_join(stream_to_load %>%
                  filter(type == "latlng") %>% 
                  transmute(record_id, lng = data[,2])) %>% 
      select(-record_id) %>% 
      mutate(id = activity_id)
  } else {
    stream_to_load <- stream_to_load %>% 
      select(record_id, type, data) %>% 
      pivot_wider(names_from = "type",
                  values_from = "data") %>% 
      select(-record_id) %>% 
      mutate(id = activity_id)
  }   
  
  if(display_map) {
    
    leaflet::leaflet() %>% 
      leaflet::addTiles() %>% 
      leaflet::addPolylines(stream_to_load$lng, stream_to_load$lat) %>% 
      print()
    
  }
  
  dbWriteTable(con, "ride_streams", stream_to_load, append = T)
  
  str_glue("Activity {activity_id} appended to database.") %>% print()
  
  Sys.sleep(8) # if loading multiple activities, prevents exceeding the 100 request in 15 minute rate limit
  
}

calculate_activity_peaks <- function(activity_id,
                                     peaks_for = c("cadence", "heartrate", "watts", "velocity_smooth"),
                                     peak_time_ranges = c(5, 10, 12, 20, 30, 60, 120, 300, 360, 600, 720, 1200, 1800, 3600)) {
  
  # Get stream for activity
  stream_sql <- tbl(con, "ride_streams") %>% 
    filter(id == activity_id) %>%
    select(time, all_of(peaks_for)) %>% collect()
  
  peak_time_ranges <- peak_time_ranges[peak_time_ranges <= max(stream_sql$time)]
  
  # Get all time peaks
  # Calculate best ever and best this year
  peaks_sql <- tbl(con, "ride_peaks") %>% 
    filter(peak > 0) %>% 
    left_join(tbl(con, "activity_list") %>% select(id, start_date_local, sport_type), by = "id") %>% 
    collect()
  
  if(activity_id %in% peaks_sql$id) {
    stop(str_glue("Activity ID {activity_id} has already had peaks calculated. Remove from SQL table if re-calculation needed."))
  }
  
  best_peaks <- peaks_sql %>%
    filter(year(start_date_local) == year(Sys.Date())) %>% 
    mutate(peak_period = "Current year") %>% 
    bind_rows(peaks_sql %>% mutate(peak_period = "All time")) %>% 
    filter(!(sport_type == "VirtualRide" & metric == "velocity_smooth")) %>% # exclude speed metrics from virtual rides
    group_by(peak_period, metric, time_range) %>% 
    slice_max(peak, n = 3, with_ties = F) %>% 
    mutate(rank = rank(-peak))
  
  # Ensure a row present for every second of the entire activity (no data recorded when stopped)
  # Then establish the width of any data gaps for later smoothing
  # Large data gaps will mean bike stopped - values should be set to zero - but small gaps can be filled
  peaks <- tibble(time = seq(0,max(stream_sql$time),1)) %>% 
    left_join(stream_sql, by = "time") %>% 
    pivot_longer(-time, names_to = "metric") %>% 
    arrange(metric, time) %>% 
    mutate(has_data = !is.na(value),
           has_data_group_id = row_number(),
           prev_has_data = lag(has_data),
           has_data_group_id = case_when(is.na(prev_has_data) ~ has_data_group_id,
                                 has_data != prev_has_data ~ has_data_group_id,
                                 T ~ NA_integer_)) %>% 
    fill(has_data_group_id, .direction = "down") %>% 
    group_by(has_data_group_id) %>% 
    mutate(gap_size = n(),
           val2 = value) %>% 
    ungroup() %>% 
    fill(val2, .direction = "down") %>% 
    mutate(value = case_when(is.na(value) & gap_size <= 10 ~ val2,
                             is.na(value) ~ 0,
                             T ~ value)) %>% 
    select(-matches("has_data"), -gap_size, -val2) %>% 
    nest(data = !metric) %>% 
    crossing(time_range = peak_time_ranges) %>% 
    mutate(id = activity_id,
           time_range_means = map2(data, time_range, ~slide_dbl(.x$value, .f = mean, .after = (.y - 1), .complete = T)),
           peak = map_dbl(time_range_means, ~max(., na.rm = T))) %>% 
    select(id, metric, time_range, peak)
  
  # Compare peaks
  peaks_compare <- best_peaks %>% 
    left_join(peaks %>% select(metric, time_range, current_peak = peak),
              by = c("metric", "time_range")) %>% 
    filter(current_peak > peak) %>% 
    slice_min(rank) %>% 
    left_join(peaks_units, by = "metric") %>% 
    mutate(msg = str_glue("{peak_period}: {if_else(rank == 1,'B',str_c(scales::ordinal(rank), ' b'))}est {if_else(time_range < 60, str_c(time_range,'s'), str_c(time_range/60,'min'))} {display_name} - {round(current_peak*multiplier,1)}{units}"))
  
  if(nrow(peaks_compare) > 0) {
    walk(peaks_compare$msg, print)
  }
  
  # Append to peaks table
  dbWriteTable(con, "ride_peaks", peaks, append = T)
  
  str_glue("Peak efforts for activity {activity_id} appended to database.") %>% print()
 
}

draw_map <- function(activity_bbox, draw_bbox = F, activity_types = "Ride") {
  
  activity_id <- activity_bbox[[1]]
  bbox <- activity_bbox[[2]]
  
  # Limit activity ids to those of the selected types
  all_type_id <- tbl(con, "activity_list") %>% 
    filter(type %in% activity_types) %>% 
    pull(id)
  
  activity_id <- activity_id[activity_id %in% all_type_id]
  
  
  streams <- tbl(con, "ride_streams") %>%
    select(id, lat, lng) %>% 
    filter(id %in% activity_id) %>% 
    collect() %>% 
    filter(row_number() %% 10 == 1) # select every 10th row -> speed up the plotting of the map
  
  # create base map
  leaflet_map <- leaflet::leaflet() %>% 
    leaflet::addTiles(urlTemplate = "https://tiles.stadiamaps.com/tiles/outdoors/{z}/{x}/{y}{r}.png")
  
  # Add on each ride
  for(ride_id in unique(streams$id)) {
    
    ride_stream <- streams %>% filter(id == ride_id)
    
    leaflet_map <- leaflet_map %>% 
      leaflet::addPolylines(ride_stream$lng, ride_stream$lat,
                            opacity = 1,
                            weight = 2,
                            color = "#48494B")
  }
  
  # Draw on bounding boxes, if draw_bbox set to TRUE
  if(draw_bbox & !is.na(bbox)) {
    
    for(i in 1:length(bbox)) {
      leaflet_map <- leaflet_map %>% 
        leaflet::addRectangles(
          lng1=bbox[[i]][4], lat1=bbox[[i]][2],
          lng2=bbox[[i]][3], lat2=bbox[[i]][1],
          fillColor = "transparent", 
          color = "#FF0000",
          weight = 1
        )
    }
    
  }
  
  print(leaflet_map)
  print(activity_id)
}

# Ride finder functions ---------------------------------------------------

# A set of functions for finding rides, all consistently named. Each takes a vector of activity ids and returns a vector of
# activity ids so they can be daisy chained together. Simpler than having one mega function, and easier to build extra
# filtering functionality later

find_rides_starting <- function(activity_bbox = list(NA, NA), start_dates = NA, start_years = NA, start_location = NA, min_distance = NA_real_) {
  
  # start_dates - vector of one or more dates
  # start_years - vector of one or more years
  activity_id <- activity_bbox[[1]]
  prev_bbox <- activity_bbox[[2]]
  
  # start_location - bounding box passed as NSEW vector, e.g. c(51, 50, 1, 0))
  
  if(all(is.na(activity_id))) {
    activity_id <- tbl(con, "activity_list") %>% pull(id) %>% unique()
  }
  
  # If bounding box supplied (i.e. 4 element list)
  if(length(start_location) == 4) {
    
    bbox_north <- start_location[1]
    bbox_south <- start_location[2]
    bbox_east <- start_location[3]
    bbox_west <- start_location[4]
    
    if(bbox_north <= bbox_south) {stop(str_glue("North value supplied ({bbox_north}) must be greater that South value ({bbox_south})"))}
    
    if(bbox_east <= bbox_west) {stop(str_glue("East value supplied ({bbox_east}) must be greater that West value ({bbox_west})"))}
    
    activity_id <- tbl(con, "activity_list") %>%
      filter(start_lat <= bbox_north,
             start_lat >= bbox_south,
             start_lng <= bbox_east,
             start_lng >= bbox_west,
             id %in% activity_id) %>%
      pull(id) %>% unique()
    
  }
  
  # If vector of dates supplied
  if(all(!is.na(start_dates))) {
    activity_id <- tbl(con, "activity_list") %>% 
      collect() %>% 
      mutate(start_date_local = as.Date(start_date_local)) %>% 
      filter(start_date_local %in% start_dates,
             id %in% activity_id) %>%
      pull(id) %>% unique()
  }
  
  # If vector of years supplied
  if(!is.na(start_years)) {
    activity_id <- tbl(con, "activity_list") %>% 
      collect() %>% 
      mutate(start_yr = year(start_date_local)) %>% 
      filter(start_yr %in% start_years,
             id %in% activity_id) %>%
      pull(id) %>% unique()
  }
  
  # If minimum distance supplied
  if(!is.na(min_distance)) {
    activity_id <- tbl(con, "activity_list") %>% 
      collect() %>% 
      mutate(distance_miles = distance / 1609.34) %>% 
      filter(distance_miles >=  min_distance,
             id %in% activity_id) %>%
      pull(id) %>% unique()
  }
  
  if(all(!is.na(prev_bbox))) {
    if(is.list(prev_bbox)) {
      prev_bbox <- append(prev_bbox, list(start_location))
    } else {
      prev_bbox <- list(prev_bbox, start_location)  
    }
  } else {
    prev_bbox <- list(start_location)
  }
  
  
  
  activity_bbox <- list(activity_id, prev_bbox)
  
  return(activity_bbox)
  
}

find_rides_visiting <- function(activity_bbox = list(NA, NA), visiting_location = NA) {
  
  activity_id <- activity_bbox[[1]]
  prev_bbox <- activity_bbox[[2]]
  
  # visiting_location - bounding box passed as NSEW vector, e.g. c(51, 50, 1, 0))
  
  if(all(is.na(activity_id))) {
    activity_id <- tbl(con, "activity_list") %>% pull(id) %>% unique()
  }
  
  # If bounding box supplied (i.e. 4 element list)
  if(length(visiting_location) == 4) {
    
    bbox_north <- visiting_location[1]
    bbox_south <- visiting_location[2]
    bbox_east <- visiting_location[3]
    bbox_west <- visiting_location[4]
    
    if(bbox_north <= bbox_south) {stop(str_glue("North value supplied ({bbox_north}) must be greater that South value ({bbox_south})"))}
    
    if(bbox_east <= bbox_west) {stop(str_glue("East value supplied ({bbox_east}) must be greater that West value ({bbox_west})"))}
    
    activity_id <- tbl(con, "ride_streams") %>%
      filter(lat <= bbox_north,
             lat >= bbox_south,
             lng <= bbox_east,
             lng >= bbox_west,
             id %in% activity_id) %>%
      pull(id) %>% unique()
    
  }
  
  if(all(!is.na(prev_bbox))) {
    if(is.list(prev_bbox)) {
      prev_bbox <- append(prev_bbox, list(visiting_location))
    } else {
      prev_bbox <- list(prev_bbox, visiting_location)  
    }
  } else {
    prev_bbox <- list(visiting_location)
  }
  
  activity_bbox <- list(activity_id, prev_bbox)
   
  return(activity_bbox)
}

# Need to build a nice map plotting function
