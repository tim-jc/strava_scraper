
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
