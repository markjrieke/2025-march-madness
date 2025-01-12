recover_priors <- function(step,
                           teams,
                           season,
                           league,
                           variable = c("o", "d", "h"),
                           samples = 4000,
                           chains = 8) {
  
  # evaluate processing time
  start_ts <- Sys.time()
  
  # compile/recompile model
  recovery <-
    cmdstan_model(
      "stan/recovery.stan",
      dir = "exe/"
    )
  
  # pass data to stan 
  stan_data <-
    list(
      S = samples,
      T = nrow(step),
      beta_mean = step$mean,
      beta_sd = step$sd
    )
  
  # fit to find hierarchical parameterization from posterior
  recovery_fit <-
    recovery$sample(
      data = stan_data,
      seed = 2025,
      init = 0.01,
      step_size = 0.002,
      chains = chains,
      parallel_chains = chains,
      iter_warmup = round(samples/chains),
      iter_sampling = round(samples/chains),
      refresh = samples
    )
  
  # post processing!
  eta_step <- recovery_fit$summary("eta")
  log_sigma_step <- recovery_fit$summary("log_sigma")
  
  # write results for eta
  eta_step %>%
    mutate(tid = parse_number(variable)) %>%
    left_join(teams) %>%
    select(-c(variable, tid)) %>%
    mutate(season = season,
           league = league,
           variable = glue::glue("eta_{variable}_step")) %>%
    relocate(season, 
             league,
             team_id, 
             team_name, 
             variable) %>%
    append_parquet("out/historical_parameters_team.parquet")
  
  # write results for log_sigma
  log_sigma_step %>%
    select(-variable) %>%
    mutate(season = season,
           league = league,
           variable = glue::glue("log_sigma_{variable}_step")) %>%
    relocate(season,
             league,
             variable) %>%
    append_parquet("out/historical_parameters_global.parquet")
  
  # diagnostics
  diagnostics <-
    recovery_fit %>%
    diagnostic_summary()
  
  # evaluate processing time
  end_ts <- Sys.time()
  
  # generate model log
  model_log <-
    tibble(
      model_name = "recovery",
      model_version = file.info("stan/recovery.stan")$mtime,
      start_ts = start_ts,
      end_ts = end_ts,
      observations = stan_data$T,
      num_divergent = diagnostics$num_divergent,
      num_max_treedepth = diagnostics$num_max_treedepth,
      samples = samples,
      season = season,
      date_min = mdy(paste0("11/1/", season)),
      date_max = mdy(paste0("4/30/", season + 1)),
      target_variable = glue::glue("beta_{variable}")
    )
  
  model_log %>%
    append_parquet("out/model_log.parquet")
  
}