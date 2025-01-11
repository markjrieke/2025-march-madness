# setup ------------------------------------------------------------------------

# libraries
library(tidyverse)
library(cmdstanr)
library(riekelib)

# functions
function_path <- "R/model/functions/"
walk(list.files(function_path), ~source(paste0(function_path, .x)))

# create stan model exe directory
if (!dir.exists("exe")) {
  dir.create("exe")
}

# run historical model ---------------------------------------------------------

run_historical_model <- function(season,
                                 league,
                                 ...,
                                 samples = 1e4,
                                 chains = 8,
                                 beta_o_step_sigma = 0.03,
                                 beta_d_step_sigma = 0.03,
                                 beta_h_step_sigma = 0.03,
                                 log_sigma_i_step_sigma = 0.03,
                                 gamma_0_step_sigma = 0.2,
                                 delta_0_step_sigma = 0.05,
                                 gamma_ot_step_sigma = 0.2,
                                 delta_ot_step_sigma = 0.05) {
  
  # evaluate processing time
  start_ts <- Sys.time()
  
  # check whether or not functions.stan has been updated since last run
  if (out_of_date("exe/historical", "stan/functions.stan")) {
    force_recompile <- TRUE
  } else {
    force_recompile <- FALSE
  }
  
  # compile model!
  historical <- 
    cmdstan_model(
      "stan/historical.stan",
      dir = "exe/",
      force_recompile = force_recompile
    )
  
  # rename variables for internal use
  season_int <- season
  league_int <- league
  
  # import current set of games
  games <-
    arrow::read_parquet("data/games/games.parquet") %>%
    
    # mid-continent university has both a NA id and an actual id (564)
    # adjust to NA for all observations
    mutate(across(ends_with("_id"), ~if_else(.x == "564", NA, .x)),
           across(ends_with("_id"), ~if_else(is.na(.x), "missing", .x)),
           home_name = if_else(home_id == "missing", "missing", home_name),
           away_name = if_else(away_id == "missing", "missing", away_name)) %>%
    
    # filter to only div 1 teams in current league/season
    filter(league == league_int,
           season == season_int,
           home_name != "missing",
           away_name != "missing")
  
  # assign tids for the current season 
  teams <- 
    bind_rows(games %>% distinct(team_name = home_name, team_id = home_id),
              games %>% distinct(team_name = away_name, team_id = away_id)) %>%
    distinct(team_name, team_id) %>%
    arrange(team_name) %>%
    rowid_to_column("tid")
  
  # set data based on current season information
  data_args <- set_data_args(games, teams)
  
  # set priors based on last season's posterior
  priors <- set_historical_priors(season, data_args$T)
  
  # set random walk parameters
  random_walk_priors <-
    list(
      beta_o_step_sigma = beta_o_step_sigma,
      beta_d_step_sigma = beta_d_step_sigma,
      beta_h_step_sigma = beta_h_step_sigma
    )
  
  # combine data/priors
  stan_data <- c(data_args, priors, random_walk_priors)
  
  # fit stan model!
  historical_fit <-
    historical$sample(
      data = stan_data,
      seed = 2025,
      init = 0.01,
      step_size = 0.002,
      chains = chains,
      parallel_chains = chains,
      iter_warmup = round(samples/chains),
      iter_sampling = round(samples/chains),
      refresh = round(2/100 * samples/chains)
    )
  
  # extract information needed to set next priors
  beta_o_step <- historical_fit$summary("beta_o_step")
  beta_d_step <- historical_fit$summary("beta_d_step")
  beta_h_step <- historical_fit$summary("beta_h_step")
  
  # recover priors for next season
  recover_priors(beta_o_step, teams, season, league, "o")
  recover_priors(beta_d_step, teams, season, league, "d")
  recover_priors(beta_h_step, teams, season, league, "h")
  
  # append beta_* posteriors
  extract_team_parameter(historical_fit, "beta_o", teams, season, league)
  extract_team_parameter(historical_fit, "beta_d", teams, season, league)
  extract_team_parameter(historical_fit, "beta_h", teams, season, league)
  
  # append log_sigma_* posteriors
  extract_global_parameter(historical_fit, "log_sigma_o", season, league)
  extract_global_parameter(historical_fit, "log_sigma_d", season, league)
  extract_global_parameter(historical_fit, "log_sigma_h", season, league)
  extract_global_parameter(historical_fit, "log_sigma_i", season, league)

  # extract hurdle posteriors
  extract_global_parameter(historical_fit, "gamma_0", season, league)
  extract_global_parameter(historical_fit, "delta_0", season, league)
  extract_global_parameter(historical_fit, "gamma_ot", season, league)
  extract_global_parameter(historical_fit, "delta_ot", season, league)
  
  # diagnostics
  diagnostics <-
    historical_fit %>%
    diagnostic_summary()
  
  # evaluate processing time
  end_ts <- Sys.time()
  
  # generate model log
  model_log <-
    tibble(
      model_name = "historical",
      model_version = file.info("stan/historical.stan")$mtime,
      start_ts = start_ts,
      end_ts = end_ts,
      observations = stan_data$N,
      num_divergent = diagnostics$num_divergent,
      num_max_treedepth = diagnostics$num_max_treedepth,
      samples = samples,
      season = season,
      date_min = mdy(paste0("11/1/", season)),
      date_max = mdy(paste0("4/30/", season + 1)),
      target_variable = glue::glue("")
    )
  
  # append log
  model_log %>%
    append_parquet("out/model_log.parquet")
  
}

# utils ------------------------------------------------------------------------

extract_global_parameter <- function(fit, 
                                     parameter, 
                                     season, 
                                     league) {
  
  fit$summary(parameter) %>%
    select(-variable) %>%
    mutate(season = season,
           league = league,
           variable = parameter) %>%
    relocate(season,
             league,
             variable) %>%
    append_parquet("out/historical_parameters_global.parquet")
  
}

extract_team_parameter <- function(fit,
                                   parameter,
                                   teams,
                                   season,
                                   league) {
  
  fit$summary(parameter) %>%
    mutate(tid = parse_number(variable)) %>%
    left_join(teams) %>%
    select(-c(variable, tid)) %>%
    mutate(season = season,
           league = league,
           variable = parameter) %>%
    relocate(season, 
             league,
             team_id, 
             team_name, 
             variable) %>%
    append_parquet("out/historical_parameters_team.parquet")
  
}

recover_priors <- function(step,
                           teams,
                           season,
                           league,
                           variable = c("o", "d", "h"),
                           ...,
                           samples = 4000) {
  
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
      chains = 8,
      parallel_chains = 8,
      iter_warmup = 500,
      iter_sampling = 500,
      refresh = 1000
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

set_data_args <- function(games, teams) {
  
  # map tid to home/away for each game in the season
  tid <- 
    games %>%
    left_join(teams, by = c("home_name" = "team_name")) %>%
    rename(tid1 = tid) %>%
    left_join(teams, by = c("away_name" = "team_name")) %>%
    rename(tid2 = tid) %>%
    select(starts_with("tid")) %>%
    as.matrix() %>%
    t()
  
  # map home/away scores for each game in the season
  Y <- 
    games %>%
    select(home_score, away_score) %>%
    as.matrix() %>%
    t()
  
  # total number of games
  N <- nrow(games)
  
  # total number of teams
  T <- max(tid)
  
  # number of overtimes per game
  O <- games$n_ot
  
  # whether (1) or not (0) to apply the home-court advantage
  V <- 1 - games$neutral
  
  # log-mean score per minute
  alpha <- log(70/40)
  
  # coalesce data arguments for stan model
  out <-
    list(
      N = N,
      T = T,
      tid = tid,
      Y = Y,
      O = O,
      V = V,
      alpha = alpha
    )
  
  return(out)
  
}

set_historical_priors <- function(season,
                                  T) {
  
  # file to check for historical posteriors/priors
  path <- "out/historical/team_parameters.parquet"
  
  # set priors manually for the first season
  # otherwise infer priors from previous season's posterior
  if (!file.exists(path)) {
    
    out <-
      list(
        eta_o_mu = rep(0, T),
        eta_o_sigma = rep(1, T),
        eta_d_mu = rep(0, T),
        eta_d_sigma = rep(1, T),
        eta_h_mu = rep(0, T),
        eta_h_sigma = rep(1, T),
        
        log_sigma_o_mu = -3,
        log_sigma_o_sigma = 0.5,
        log_sigma_d_mu = -3,
        log_sigma_d_sigma = 0.5,
        log_sigma_h_mu = -3,
        log_sigma_h_sigma = 0.5,
        
        log_sigma_i_mu = -3,
        log_sigma_i_sigma = 0.5,
        
        gamma_0_mu = 0,
        gamma_0_sigma = 1,
        delta_0_mu = 2,
        delta_0_sigma = 1,
        gamma_ot_mu = 0,
        gamma_ot_sigma = 1,
        delta_ot_mu = -1.5,
        delta_ot_sigma = 1
      )
    
  } else {
    
    # fix later dawg
    
    # eta_o_mu <- set_eta_mu(eta_o_step)
    # eta_o_sigma <- set_eta_sd(eta_o_step)
    # eta_d_mu <- set_eta_mu(eta_d_step)
    # eta_d_sigma <- set_eta_sd(eta_d_step)
    # eta_h_mu <- set_eta_mu(eta_h_step)
    # eta_h_sigma <- set_eta_sd(eta_h_step)
    # 
    # log_sigma_o_mu <- set_parameter_mu(log_sigma_o_step)
    # log_sigma_o_sigma <- set_parameter_sd(log_sigma_o_step)
    # log_sigma_d_mu <- set_parameter_mu(log_sigma_d_step)
    # log_sigma_d_sigma <- set_parameter_sd(log_sigma_d_step)
    # log_sigma_h_mu <- set_parameter_mu(log_sigma_h_step)
    # log_sigma_h_sigma <- set_parameter_sd(log_sigma_h_step)
    # 
    # log_sigma_i_mu <- set_parameter_mu(log_sigma_i)
    # log_sigma_i_sigma <- set_parameter_sd(log_sigma_i) + log_sigma_i_step_sigma
    # 
    # gamma_0_mu <- set_parameter_mu(gamma_0)
    # gamma_0_sd <- set_parameter_sd(gamma_0) + gamma_0_step_sigma
    # delta_0_mu <- set_parameter_mu(delta_0)
    # delta_0_sd <- set_parameter_sd(delta_0) + delta_0_step_sigma
    # gamma_ot_mu <- set_parameter_mu(gamma_ot)
    # gamma_ot_sd <- set_parameter_sd(gamma_ot) + gamma_ot_step_sigma
    # delta_ot_mu <- set_parameter_mu(delta_ot)
    # delta_ot_sd <- set_parameter_sd(delta_ot) + delta_ot_step_sigma
    
  }
  
  return(out)
  
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
