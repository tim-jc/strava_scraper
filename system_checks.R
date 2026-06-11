check_cron_schedule <- function() {

  environ_schedule <- Sys.getenv("SCRAPER_SCHEDULE")

  cron <- tryCatch(
    system("crontab -l", intern = TRUE),
    error = function(e) character(0)
  )

  start_index <- match(
    "# >>> STRAVA_SCRAPER_START >>>",
    cron
  )

  if(is.na(start_index)) {
    return(FALSE)
  }

  cron_line <- cron[start_index + 2]

  cron_schedule <- str_split_1(
    cron_line,
    "\\s+"
  ) |>
    (\(x) paste(x[1:5], collapse = " "))()

  match <- identical(
    environ_schedule,
    cron_schedule
  )
  
  if(!match) {
    
    log_message("Mismatch between cron and .Renviron schedule. Run install_cron_job() to reset to .Renviron values")
    
  }
  

}
