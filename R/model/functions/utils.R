diagnostic_summary <- function(model) {
  
  model$diagnostic_summary() %>%
    as_tibble() %>%
    summarise(num_divergent = sum(num_divergent),
              num_max_treedepth = sum(num_max_treedepth))
  
}

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

missing_seasons <- function(file) {
  
  seasons <- 2002:2024
  
  if (file.exists(file)) {
    
    # pull in the seasons that have been run
    existing_seasons <-
      arrow::read_parquet(file) %>%
      distinct(season) %>%
      pull(season)
    
    # remove any previously run seasons
    seasons <- seasons[!seasons %in% existing_seasons]
    
  }
  
  return(seasons)
  
}

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
