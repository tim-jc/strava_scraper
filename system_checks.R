check_cron_schedule <- function() {

environ_schedule <- Sys.getenv("SCRAPER_SCHEDULE")

# read schedule from cron
cron_schedule <- tryCatch(
    system("crontab -l", intern = TRUE),
    error = function(e) character(0)
  )

# extract scraper schedule from cron

# compare, warn if out of sync

}