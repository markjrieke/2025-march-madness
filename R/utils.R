#' Generate basic sampler dianostics as a tibble
#'
#' @description
#' Generate model diagnostics for `num_divergent` and `num_max_treedepth` as a
#' tibble
#'
#' @param model model fit by the `$sample()` method
diagnostic_summary <- function(model) {
  
  model$diagnostic_summary() %>%
    as_tibble() %>%
    summarise(num_divergent = sum(num_divergent),
              num_max_treedepth = sum(num_max_treedepth))
  
}

#' Append an existing parquet file with new data, or create a new one if it 
#' doesn't already exist.
#' 
#' @param x A tibble or dataframe
#' @param file Path to parquet file. If it exists, the data in `x` must match to
#'        be able to append. If it doesn't exist, `file` indicates the path/
#'        filename of the parquet file to be created.  
append_parquet <- function(data, file) {
  
  if (!file.exists(file)) {
    
    cli::cli_alert_info(glue::glue("{file} doesn't exist. Initializing file."))
    data %>%
      arrow::write_parquet(file)
    
  } else {
    
    # read in current file
    current_file <-
      arrow::read_parquet(file)
    
    # append and write
    current_file %>%
      bind_rows(data) %>%
      arrow::write_parquet(file)
    
  }
  
}

#' Check whether an output file is out-of-date
#'
#' @description
#' Compare the last-modified timestamps of an output file and any trigger files.
#' When any trigger file has a more recent timestamp than the output file or the
#' output file doesn't exist, `out_of_date()` returns `TRUE`. Otherwise, returns
#' `FALSE`.
#'
#' @param output file path of the output file
#' @param triggers file path(s) of the trigger file(s)
out_of_date <- function(output, triggers) {
  
  if (file.exists(output)) {
    
    output_time <- file.info(output)$mtime
    trigger_times <- file.info(triggers)$mtime
    out_of_date <- any(trigger_times > output_time)
    
  } else {
    
    out_of_date <- TRUE
    
  }
  
  return(out_of_date)
  
}

#' Remove all model output files
#'
#' @description
#' Removes all output files under the `out/` directory. Requires user input of
#' "y" to confirm deletion.
reset_outputs <- function() {
  
  question <- "Are you sure you want to reset outputs for all models? (y/n)"
  cli::cli_alert_warning(question)
  
  if (readline() == "y") {
    
    list.files("out", recursive = TRUE) %>%
      walk(~file.remove(paste0("out/", .x)))
    
    cli::cli_alert_info("Outputs reset")
    
  } else {
    
    cli::cli_alert_info("Aborted")
    
  }
  
}

#' Return result of an expression along with errors and warnings as a list
#' 
#' @description
#' `try_catch()` is a small wrapper around `tryCatch()` that returns a list 
#' containing the expression result as well as any warnings or errors thrown. In 
#' the event that an error occurs, `$result` is `NULL`. 
#' 
#' @param x An expression to evaluate.
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

#' Retry an expression in the event of an error.
#' 
#' @description
#' Attempt to evaluate an expression multiple times in the event that an error
#' is thrown. As soon as the expression evaluates successfully, return the
#' result.
#' 
#' @param x An expression to evaluate.
#' @param retry Number of times to re-attempt the expression before returning a
#'        `NULL` result.
#' @param delay Amount of time (in seconds) between attempts at evaluating the 
#'        expression.
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

#' Helper function for sleeping with a progress bar
#' 
#' @param time Amount of time (in seconds) for the system to sleep.
#' @param .progress Whether or not to display a progress bar while sleeping.
sys_sleep <- function(time, .progress = TRUE) {
  
  if (.progress) {
    
    cli::cli_progress_bar("Sleeping...", total = time, clear = FALSE)
    for (t in 1:time) {
      Sys.sleep(1)
      cli::cli_progress_update()
    }
    
  } else {
    
    Sys.sleep(time)
    
  }
  
}

