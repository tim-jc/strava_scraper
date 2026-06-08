# scraper.R
# retrieve new activities from Strava and 
# add to MySQL database on RaspberryPi

# clear memory ------------------------------------------------------------

rm(list = ls(all = TRUE))

# details for log file ----------------------------------------------------

cat("\n=================================================\n")
cat("SCRAPER START:", as.character(Sys.time()), "\n")
cat("R VERSION:", R.version.string, "\n")
cat("WORKING DIR:", getwd(), "\n")
cat("HOME:", Sys.getenv("HOME"), "\n")
cat("USER:", Sys.getenv("USER"), "\n")
cat("RSTUDIO:", Sys.getenv("RSTUDIO"), "\n")
cat("=================================================\n")
flush.console()

# runtime arguments -------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)

run_mode <- ifelse(
  length(args) > 0,
  args[1],
  "manual"
)

cat(glue::glue(
  "RUN MODE: {run_mode}\n"
))

flush.console()

# libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(DBI)
library(slider)
library(glue)
library(here)
library(stravR)

# project setup -----------------------------------------------------------

setwd(normalizePath("~/Documents/Coding/R/Strava/strava_scraper"))

here::i_am("scraper.R")

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
  
  db = list(
    name = Sys.getenv("DB_NAME"),
    host = Sys.getenv("DB_HOST"),
    port = as.integer(Sys.getenv("DB_PORT")),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD")
  )
  
)


# source db functions -----------------------------------------------------

here::here("db_helpers.R") %>% source()

# database connection -----------------------------------------------------

cat("Attempting DB connection...\n")

con <- connect_to_db()

# source main functions ---------------------------------------------------

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
  ntfy_msg <- str_glue(
    "{nrow(new_activities_to_load)} activit{if_else(nrow(new_activities_to_load) == 1,'y','ies')} loaded\n",
    "Run mode: {run_mode}\n",
    "Completed: {Sys.time()}")
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

# Final messages for log

cat("Disconnecting database connection...\n")
flush.console()

DBI::dbDisconnect(con)

cat("Scraper complete.\n")
flush.console()
