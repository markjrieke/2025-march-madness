#' Run the recovery model
#' 
#' @description
#' The recovery model is a submodel of the historical model post-processing that
#' converts a set of pseudo-random walk parameters (i.e., `beta_*_step`) to 
#' their hierarchical components, `eta` and `sigma`. By making use of the 
#' sufficient formulation of the normal distribution, the model is able to take
#' a summary of team parameters as input, rather than a set of draws as input.
#' 
#' @param step A summary tibble of pseudo-random walk parameters fit by the 
#'        historical model.
#' @param teams A tibble mapping ESPN `team_name` and `team_id` to an internal
#'        mapping id, `tid`.
#' @param season Season to extract results for. Seasons are identified by the
#'        year in which the last game was played.
#' @param league Which league to extract results for. Either "mens" or "womens".
#' @param variable Which team parameter to process (offense, defense, or home-
#'        court advantage).
#' @param samples Number of posterior samples to generate. Used for both warmup
#'        and sampling.
#' @param chains Number of chains used to fit the model. All chains will be run
#'        in parallel, if available. 
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
    append_parquet("out/historical/historical_parameters_team.parquet")
  
  # write results for log_sigma
  log_sigma_step %>%
    select(-variable) %>%
    mutate(season = season,
           league = league,
           variable = glue::glue("log_sigma_{variable}_step")) %>%
    relocate(season,
             league,
             variable) %>%
    append_parquet("out/historical/historical_parameters_global.parquet")
  
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
      league = league,
      date_min = mdy(paste0("11/1/", season)),
      date_max = mdy(paste0("4/30/", season + 1)),
      target_variable = glue::glue("beta_{variable}")
    )
  
  model_log %>%
    append_parquet("out/model_log.parquet")
  
}