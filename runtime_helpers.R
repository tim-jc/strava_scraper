connect_to_db <- function(max_attempts = 5,
                          wait_seconds = 30) {
  
  for(i in seq_len(max_attempts)) {
    
    cat(glue::glue(
      "DB connection attempt {i}/{max_attempts}\n"
    ))
    
    con <- tryCatch(
      
      DBI::dbConnect(
        RMariaDB::MariaDB(),
        dbname = config$db$name,
        host = config$db$host,
        port = config$db$port,
        user = config$db$user,
        password = config$db$password
      ),
      
      error = function(e) {
        
        cat("Connection failed:\n")
        cat(conditionMessage(e), "\n")
        
        NULL
        
      }
      
    )
    
    if(!is.null(con)) {
      
      cat("DB connection successful.\n")
      
      return(con)
      
    }
    
    Sys.sleep(wait_seconds)
    
  }
  
  stop("Unable to connect to database.")
  
}

log_message <- function(msg) {
  
  timestamp <- format(
    Sys.time(),
    "%Y-%m-%d %H:%M:%S"
  )
  
  message(
    glue::glue(
      "[{timestamp}] {msg}"
    )
  )
  
  flush.console()
  
}

install_cron_job <- function() {
  
  schedule <- Sys.getenv("SCRAPER_SCHEDULE")
  
  if(schedule == "") {
    stop("SCRAPER_SCHEDULE environment variable not set.")
  }
  
  scraper_path <- normalizePath(
    here::here("scraper.R"),
    winslash = "/"
  )
  
  log_path <- normalizePath(
    here::here("scraper.log"),
    winslash = "/"
  )
  
  existing <- tryCatch(
    system("crontab -l", intern = TRUE),
    error = function(e) character(0)
  )

  start_string <- "# >>> STRAVA_SCRAPER_START >>>"
  end_string <- "# <<< STRAVA_SCRAPER_END <<<"
  
  remove_start <- match(start_string, existing)
  remove_end <- match(end_string, existing)
  
  if(xor(is.na(remove_start), is.na(remove_end))) {
    stop("Managed cron block appears corrupted.")
  }
  
  if(!is.na(remove_start)) {
    cron_to_keep <- existing[-(remove_start:remove_end)]
  } else {
    cron_to_keep <- existing
  }
  
  cron_to_keep <- cron_to_keep[cron_to_keep != ""]
  
  cron_block <- c(
    "",
    start_string,
    "## desc: Strava scraper update and dashboard refresh",
    glue::glue(
      "{schedule} /usr/local/bin/Rscript '{scraper_path}' cron >> '{log_path}' 2>&1"
    ),
    end_string
  )
  
  cron_to_write <- c(
    cron_to_keep,
    cron_block
  )
  
  cron_file <- tempfile(fileext = ".cron")
  
  writeLines(
    cron_to_write,
    cron_file
  )
  
  system2(
    "crontab",
    args = cron_file
  )
  
  log_message(
    glue::glue(
      "Cron schedule installed: {schedule}"
    )
  )
  
}

get_next_scraper_run <- function() {
  
  tz <- Sys.timezone()
  
  schedule <- Sys.getenv("SCRAPER_SCHEDULE")
  
  schedule <- str_split_1(schedule, pattern = " ")
  
  mins <- schedule[1]
  
  hours <- str_split_1(schedule[2], ",")
  
  runs <- bind_rows(
    tibble(hours, mins, date = Sys.Date()),
    tibble(hours, mins, date = Sys.Date() + days(1)),
  ) %>% 
    mutate(dt = lubridate::ymd_hm(str_c(date,hours,mins), tz = tz)) %>% 
    filter(dt > now())
  
  next_run <- min(runs$dt)
  
  return(next_run)
  
}