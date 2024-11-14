#' Helper function for handling errors
try_catch <- function(x) {
  
  # empty messages
  err <- NULL
  war <- NULL
  
  # attempt & log error
  result <- 
    withCallingHandlers(
      tryCatch(
        x,
        error = function(e) {
          err <<- e
          NULL
        },
        warning = function(w) {
          war <<- w
          NULL
        }
      )
    )
  
  out <-
    list(
      result = result,
      error = err,
      warning = war
    )
  
  return(out)
  
}

#' Helper function for attempting retrying on error
retry_catch <- function(x, 
                        retry = 5, 
                        delay = 1) {
  
  # make multiple attempts if encountering an error
  for (n in 1:retry) {
    
    # attempt expression
    result <- try_catch(eval.parent(substitute(x)))
    
    # exit if something is returned
    if (!is.null(result$result)) {
      break
    }
    
    # try again or accept error
    if (n < retry) {
      Sys.sleep(delay)
    }
    
  }
  
  return(result)
  
}

#' Helper function for appending a log
write_log <- function(x, file) {
  
  if (!file.exists(file)) {
    
    x %>% arrow::write_parquet(file)
    
  } else {
    
    arrow::read_parquet(file) %>%
      bind_rows(x) %>%
      arrow::write_parquet(file)
    
  }
  
}

