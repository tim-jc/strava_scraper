# scraper.R
# retrieve new activities from Strava and 
# add to MySQL database on RaspberryPi

# clear environment -------------------------------------------------------

rm(list = ls(all = TRUE))

# project setup -----------------------------------------------------------

setwd(normalizePath("~/Documents/Coding/R/Strava/strava_scraper"))

here::i_am("scraper.R")

# source runtime helpers --------------------------------------------------

source(here::here("runtime_helpers.R"))

# details for log file ----------------------------------------------------

log_message("SCRAPER START")
log_message(glue::glue("R VERSION: {R.version.string}"))
log_message(glue::glue("WORKING DIR: {getwd()}"))
log_message(glue::glue("HOME: {Sys.getenv(\"HOME\")}"))
log_message(glue::glue("USER: {Sys.getenv(\"USER\")}"))
log_message(glue::glue("RSTUDIO: {Sys.getenv(\"RSTUDIO\")}"))

# runtime arguments -------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)

run_mode <- ifelse(
  length(args) > 0,
  args[1],
  "manual"
)

log_message(glue::glue("RUN MODE: {run_mode}"))

# libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(DBI)
library(slider)
library(glue)
library(here)
library(stravR)


# environment -------------------------------------------------------------

# load environment variables
readRenviron(here::here(".Renviron"))

# validate required environment variables
assert_env_var <- function(x) {
  val <- Sys.getenv(x)
  if(val == "") {stop(glue::glue("Environment variable '{x}' not set."))}
}

required_env_vars <- c(
  "STRAVA_APP_NAME",
  "STRAVA_CLIENT_ID",
  "STRAVA_APP_SECRET",
  "DB_NAME",
  "DB_HOST",
  "DB_PORT",
  "DB_USER",
  "DB_PASSWORD",
  "RSTUDIO_PANDOC"
)

purrr::walk(
  required_env_vars,
  assert_env_var
)

# set pandoc environment
Sys.setenv(
  RSTUDIO_PANDOC = Sys.getenv("RSTUDIO_PANDOC")
)

# config ------------------------------------------------------------------

config <- list(
  
  strava = list(
    app_name = Sys.getenv("STRAVA_APP_NAME"),
    client_id = Sys.getenv("STRAVA_CLIENT_ID"),
    app_secret = Sys.getenv("STRAVA_APP_SECRET")
  ),
  
  scraper = list(
    schedule = Sys.getenv("SCRAPER_SCHEDULE")
  ),
  
  db = list(
    name = Sys.getenv("DB_NAME"),
    host = Sys.getenv("DB_HOST"),
    port = as.integer(Sys.getenv("DB_PORT")),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD")
  )
  
)

# database connection -----------------------------------------------------

log_message("Attempting DB connection...")

con <- connect_to_db()

on.exit(
  DBI::dbDisconnect(con),
  add = TRUE
)

# source main functions ---------------------------------------------------

here::here("scraper_functions.R") %>% source()

# Define values -----------------------------------------------------------

# strava access token
stoken <- get_strava_token(
  config$strava$app_name,
  config$strava$client_id,
  config$strava$app_secret
)

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
  ntfy_msg <- str_glue(
    "{nrow(new_activities_to_load)} activit{if_else(nrow(new_activities_to_load) == 1,'y','ies')} loaded\n",
    "Run mode: {run_mode}\n",
    "Completed: {format(Sys.time(), \"%Y-%m-%d %H:%M:%S\")}")
} else {
  ntfy_msg <- str_glue(
    "No new activities found\n",
    "Run mode: {run_mode}\n",
    "Completed: {format(Sys.time(), \"%Y-%m-%d %H:%M:%S\")}"
  )
}


# Update streams and peaks ------------------------------------------------

streams_to_get <- c(activities_loaded, new_activities_to_load$strava_id)[!c(activities_loaded, new_activities_to_load$strava_id) %in% streams_loaded]

# Get stream data from Strava and append to DB
for(strm in streams_to_get) {

  stream_to_load <- get_stream_data(activity_id = strm, strava_token = stoken, display_map = T)
  dbWriteTable(con, "streams", stream_to_load, append = T)
  log_message(glue::glue("Activity {strm} appended to database."))
    
}

# Calculate peak performances from new activities and append to database
walk(streams_to_get, calculate_activity_peaks, con = con)

# Calculate power summary
walk(streams_to_get, calculate_power_summary, con = con)

# Data quality checks -----------------------------------------------------

check_data_quality(con)

# Render and publish ------------------------------------------------------

# Render dashboard
rmarkdown::render(here::here("index.Rmd"), output_file = "index.html", output_dir = here::here("docs/"))

# Push updated dashboard to git
publish_to_git()

# Send notification
send_ntfy_message(ntfy_msg)

# Final messages for log

log_message("Disconnecting database connection...")

DBI::dbDisconnect(con)

log_message("Scraper complete.")
