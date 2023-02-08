# scraper.R
# retrieve new activites from Strava and 
# add to local SQLite database

# libraries
library(tidyverse)
library(lubridate)
library(DBI)
library(slider)
library(stravR)

# clear the memory
rm(list=ls(all = TRUE))

source("scraper_functions.R")
source("config.R")

# Define values -----------------------------------------------------------

stoken <- get_strava_token(app_name, app_client_id, app_secret)

# Get data ----------------------------------------------------------------

# Connect to SQLite DB, find data already loaded
activities_loaded <- tbl(con, "activities") %>% pull(id) %>% unique()
streams_loaded <- tbl(con, "streams") %>% pull(id) %>% unique()

# Update activities -------------------------------------------------------

# Get 200 most recent activities and find new activities
new_activities_to_load <- get_activity_data(strava_token = stoken,
                                            activities_to_exclude = c(activities_loaded, activities_to_remove))

# Write new activities to DB
if(nrow(new_activities_to_load) > 0) {dbWriteTable(con, "activities", new_activities_to_load, append = T)}

# Update streams ----------------------------------------------------------

streams_to_get <- c(activities_loaded, new_activities_to_load$id)[!c(activities_loaded, new_activities_to_load$id) %in% streams_loaded]

# Get stream data from Strava and append to DB
for(strm in streams_to_get) {

  stream_to_load <- get_stream_data(activity_id = strm, strava_token = stoken, display_map = T)
  dbWriteTable(con, "streams", stream_to_load, append = T)
  str_glue("Activity {strm} appended to database.") %>% print()
    
}

# Calculate peak performances from new activities and append to DB
walk(streams_to_get, calculate_activity_peaks)

# Run data quality checks (orphan activities, rides without peaks, etc)
# Function to do this needs to be written!!

# Render dashboard
rmarkdown::render("index.Rmd", output_file = "index.html", output_dir = "docs/")
