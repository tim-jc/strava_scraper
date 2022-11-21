
# Define values -----------------------------------------------------------

# SQLite DB connection
con <- DBI::dbConnect(RSQLite::SQLite(), "~/Documents/Coding/R/Strava/strava_data.db")

# Manual activities to exclude (Strava API call fails if these aren't removed)
activities_to_remove <- c(6317473692, 5686021341, 1358368751, 1222988207, 574040145, 543631692)

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

draw_map <- function(activity_bbox, draw_bbox = F, show_virtual_rides = F) {
  
  activity_id <- activity_bbox[[1]]
  bbox <- activity_bbox[[2]]
  
  
  if(!show_virtual_rides) {
    virtual_id <- tbl(con, "activity_list")  %>% 
      filter(type == "VirtualRide") %>% 
      pull(id)
    
    activity_id <- activity_id[!activity_id %in% virtual_id]
    
  }
  
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

find_rides_starting <- function(activity_bbox = list(NA, NA), start_dates = NA, start_years = NA, start_location = NA) {
  
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
    
    activity_id <- tbl(con, "activity_list") %>%
      filter(lat <= bbox_north,
             lat >= bbox_south,
             lng <= bbox_east,
             lng >= bbox_west,
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
