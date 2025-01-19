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
