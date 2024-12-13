# scraper.R
# retrieve new activites from Strava and 
# add to MySQL database on RaspberryPi

# libraries
library(tidyverse)
library(lubridate)
library(DBI)
library(slider)
library(stravR)

# clear the memory
rm(list=ls(all = TRUE))

# wd
setwd("~/Documents/Coding/R/Strava/strava_scraper/")

# establish project root
here::i_am("scraper.R")

# Functions
here::here("config.R") %>% source()
here::here("scraper_functions.R") %>% source()



# Define values -----------------------------------------------------------

# strava access token
stoken <- get_strava_token(app_name, app_client_id, app_secret)

# Initial ntfy message
ntfy_msg <- "No new activities"

# Get data ----------------------------------------------------------------

# Connect to MySQL/MariaDB, find data already loaded
activities_loaded <- tbl(con, "activities") %>% select(strava_id) %>% distinct() %>% pull(strava_id)
streams_loaded <- tbl(con, "streams") %>% select(strava_id) %>% distinct() %>% pull(strava_id)

# Update activities -------------------------------------------------------

# Get 200 most recent activities and find new activities
new_activities_to_load <- get_activity_data(strava_token = stoken,
                                            activities_to_exclude = c(activities_loaded, activities_to_remove))

# Write new activities to DB
if(nrow(new_activities_to_load) > 0) {
  dbWriteTable(con, "activities", new_activities_to_load, append = T)
  ntfy_msg <- str_glue("{nrow(new_activities_to_load)} activit{if_else(nrow(new_activities_to_load) == 1,'y','ies')} loaded\n")
  }


# Update streams and peaks ------------------------------------------------

streams_to_get <- c(activities_loaded, new_activities_to_load$strava_id)[!c(activities_loaded, new_activities_to_load$strava_id) %in% streams_loaded]

# Get stream data from Strava and append to DB
for(strm in streams_to_get) {

  stream_to_load <- get_stream_data(activity_id = strm, strava_token = stoken, display_map = T)
  dbWriteTable(con, "streams", stream_to_load, append = T)
  str_glue("Activity {strm} appended to database.") %>% print()
    
}

# Calculate peak performances from new activities and append to database
walk(streams_to_get, calculate_activity_peaks)

# Calculate power summary
walk(streams_to_get, calculate_power_summary)

# Data quality checks -----------------------------------------------------

check_data_quality()

# Render and publish ------------------------------------------------------

# Render dashboard
rmarkdown::render(here::here("index.Rmd"), output_file = "index.html", output_dir = here::here("docs/"))

# Push updated dashboard to git
publish_to_git()

# Send notification
send_ntfy_message(ntfy_msg)
