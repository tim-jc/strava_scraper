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