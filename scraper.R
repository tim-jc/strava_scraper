# scraper.R
# retrieve new activites from Strava and 
# add to local SQLite database

# libraries
library(tidyverse)
library(lubridate)
library(httr)
library(DBI)

# clear the memory
rm(list=ls(all = TRUE))

source("scraper_functions.R")
source("config.R")

# Define values -----------------------------------------------------------

# Check if strava authenication token exists; create if not
if(file.exists(".httr-oauth")) {
  stoken <- httr::config(token = readRDS('.httr-oauth')[[1]])
} else {
  stoken <- httr::config(token = strava_oauth(app_name, app_client_id, app_secret, app_scope="activity:read_all", cache = T))
}

# Get data ----------------------------------------------------------------

# Connect to SQLite DB, find data already loaded
activities_loaded <- tbl(con, "activity_list") %>% pull(id) %>% unique()
streams_loaded <- tbl(con, "ride_streams") %>% pull(id) %>% unique()

# Update activities -------------------------------------------------------

# Get 200 most recent activities
all_activity_id <- GET(url = "https://www.strava.com/api/v3/athlete/activities",
                       stoken, 
                       query = list(per_page = 200))

# Find new activities and process
new_activities_to_load <- all_activity_id$content %>%
  rawToChar() %>%
  jsonlite::fromJSON() %>%
  filter(!id %in% c(activities_loaded, activities_to_remove)) %>% 
  mutate(start_lat = map_dbl(start_latlng, 1),
         start_lng = map_dbl(start_latlng, 2),
         end_lat = map_dbl(end_latlng, 1),
         end_lng = map_dbl(end_latlng, 2),
         strava_link = str_glue("https://www.strava.com/activities/{id}"),
         ride_start = str_replace_all(start_date_local, "T|Z", " ") %>% as.POSIXct(),
         ride_start = format(ride_start, "%Y-%m-%d %H:%M:%S")) %>% 
  select(-c(athlete, map, start_latlng, end_latlng)) %>% 
  as_tibble()

# Activity list
if(nrow(new_activities_to_load) > 0) {dbWriteTable(con, "activity_list", new_activities_to_load, append = T)}

# Update streams ----------------------------------------------------------

streams_to_get <- c(activities_loaded, new_activities_to_load$id)[!c(activities_loaded, new_activities_to_load$id) %in% streams_loaded]

walk(streams_to_get, ~get_stream_data(.x))
