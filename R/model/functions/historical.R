#' Run the historical model
#' 
#' @description
#' The historical model is a comparative skill model that estimates the outcome
#' of each game based on the latent expected rate of scoring for each team.
#' Further, the historical model includes a hurdle component that estimates the 
#' probability of a matchup going to overtime and the number of overtimes 
#' played if so. 
#' 
#' The latent rate of scoring for each team is a combination of the team's 
#' offensive skill, the opponent's defensive skill, and the team's boost in 
#' scoring potential due to home-court advantages, if applicable. For games that
#' are played at neutral locations, neither team benefits from their home-court
#' advantage. 
#' 
#' The probability of a game going to overtime is estimated with a simple 
#' single-variable linear model using the absolute difference in each team's 
#' rate of scoring (i.e., it's expected that a game with two equally-matched 
#' teams is likelier to go to overtime than an imbalanced game). Similarly, the 
#' number of overtimes played is estimated with a simple linear model based on 
#' the absolute difference in each team's rate of scoring. 
#' 
#' Offensive, defensive, and home-court advantage parameters are modeled as
#' hierarchically distributed independent parameters. Parameters for both 
#' components of the hurdle model are estimated as fixed parameters. 
#' 
#' Priors are set using a psuedo-random walk process. For the first season, 
#' weakly informative priors are set for all parameters by the function 
#' internally. For subsequent seasons, priors are set using the posterior of 
#' each parameter plus a fixed amount of drift passed as arguments to this 
#' function. A `recovery.stan` submodel is used to convert the output of the 
#' pseudo-random walk (`beta`) to the hierarchical components (`eta` and 
#' `sigma`) that are passed into the model as priors. More information on the 
#' recovery model can be found in `recovery.R`.
#' 
#' For each season, the historical model outputs the log of scale parameters and
#' the hurdle model components to `historical_parameters_global.parquet`. Team-
#' level offensive, defensive, and home-court advantage parameters for each 
#' season are stored in `historical_parameters_team.parquet`. 
#' 
#' @param season Season to extract results for. Seasons are identified by the
#'        year in which the last game was played.
#' @param league Which league to extract results for. Either "mens" or "womens".
#' @param ... Unused
#' @param samples Number of posterior samples to generate. Used for both warmup
#'        and sampling.
#' @param chains Number of chains used to fit the model. All chains will be run
#'        in parallel, if available. 
#' @param beta_o_step_sigma,beta_d_step_sigma,beta_h_step_sigma Pseudo-random 
#'        walk scale for team skill parameters.
#' @param log_sigma_i_step_sigma Pseudo-random walk scale for overdispersion 
#'        scale.
#' @param gamma_0_step_sigma,gamma_ot_step_sigma Pseudo-random walk scale for 
#'        slope parameters in the hurdle model.
#' @param delta_0_step_sigma,delta_ot_step_sigma Pseudo-random walk scale for 
#'        intercept parameters in the hurdle model.
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
  
  cli::cli_h1(glue::glue("{str_to_title(league)} {season-1}-{str_sub(as.character(season), -2)}"))
  
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
  priors <- 
    set_historical_priors(
      teams, 
      season, 
      league, 
      data_args$T,
      log_sigma_i_step_sigma,
      gamma_0_step_sigma,
      delta_0_step_sigma,
      gamma_ot_step_sigma,
      delta_ot_step_sigma
    )
  
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
      refresh = round(2/10 * samples/chains)
    )
  
  # extract information needed to set next priors
  beta_o_step <- historical_fit$summary("beta_o_step")
  beta_d_step <- historical_fit$summary("beta_d_step")
  beta_h_step <- historical_fit$summary("beta_h_step")
  
  # recover priors for next season
  recover_priors(beta_o_step, teams, season, league, "o", samples, chains)
  recover_priors(beta_d_step, teams, season, league, "d", samples, chains)
  recover_priors(beta_h_step, teams, season, league, "h", samples, chains)
  
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
      league = league,
      date_min = mdy(paste0("11/1/", season)),
      date_max = mdy(paste0("4/30/", season + 1)),
      target_variable = glue::glue("")
    )
  
  # append log
  model_log %>%
    append_parquet("out/model_log.parquet")
  
}

#' Extract the summary of a global parameter from a model fit
#' 
#' @param fit A model fit by `historical.stan`
#' @param parameter The parameter name to be extracted.
#' @param season Season to extract results for. Seasons are identified by the
#'        year in which the last game was played.
#' @param league Which league to extract results for. Either "mens" or "womens".
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
    append_parquet("out/historical/historical_parameters_global.parquet")
  
}

#' Extract summaries of team parameters from a model fit
#' 
#' @param fit A model fit by `historical.stan`
#' @param parameter The parameter name to be extracted.
#' @param teams A tibble mapping ESPN `team_name` and `team_id` to an internal
#'        mapping id, `tid`.
#' @param season Season to extract results for. Seasons are identified by the
#'        year in which the last game was played.
#' @param league Which league to extract results for. Either "mens" or "womens".
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
    append_parquet("out/historical/historical_parameters_team.parquet")
  
}

#' Check whether or not all prior seasons have been run for the historical model
#' 
#' @description
#' Checks whether or not all prior seasons (i.e., 2002-2024) have been run and 
#' for appear in `model_log.parquet` the specified league.
#' 
#' @param league Which league to extract results for. Either "mens" or "womens".
historical_completed <- function(league) {
  
  # rename variables for internal use
  league_int <- league
  
  # return seasons that have been evaluated
  completed_seasons <- 
    arrow::read_parquet("out/model_log.parquet") %>%
    filter(model_name == "historical",
           league == league_int) %>%
    pull(season)
  
  # check if all historical seasons have been run
  out <- all(2002:2024 %in% completed_seasons)
  
  return(out)
  
}

