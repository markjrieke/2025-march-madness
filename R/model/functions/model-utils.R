#' Prep data to be passed to the historical model
#' 
#' @param games A tibble of game data for a given season.
#' @param teams A tibble mapping ESPN `team_name` and `team_id` to an internal
#'        mapping id, `tid`.
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

#' Prep priors to be passed to the historical model
#' 
#' @param teams A tibble mapping ESPN `team_name` and `team_id` to an internal
#'        mapping id, `tid`.
#' @param season Season to extract results for. Seasons are identified by the
#'        year in which the last game was played.
#' @param league Which league to extract results for. Either "mens" or "womens".
#' @param T The number of Division I teams that played this season.
#' @param log_sigma_i_step_sigma Pseudo-random walk scale for overdispersion 
#'        scale.
#' @param gamma_0_step_sigma,gamma_ot_step_sigma Pseudo-random walk scale for 
#'        slope parameters in the hurdle model.
#' @param delta_0_step_sigma,delta_ot_step_sigma Pseudo-random walk scale for 
#'        intercept parameters in the hurdle model.
set_historical_priors <- function(teams,
                                  season,
                                  league,
                                  T,
                                  log_sigma_i_step_sigma,
                                  gamma_0_step_sigma,
                                  delta_0_step_sigma,
                                  gamma_ot_step_sigma,
                                  delta_ot_step_sigma) {
  
  # rename variables for internal use
  league_int <- league
  
  # check whether or not the first season (2002) has been run
  first_run <-
    arrow::read_parquet("out/model_log.parquet") %>%
    filter(season == 2002,
           league == league_int) %>%
    nrow()
  
  # set priors manually for the first season
  # otherwise infer priors from previous season's posterior
  if (first_run == 0) {
    
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
    
    out <-
      list(
        eta_o_mu = set_team_mu("eta_o_step", teams, season, league),
        eta_o_sigma = set_team_sigma("eta_o_step", teams, season, league),
        eta_d_mu = set_team_mu("eta_d_step", teams, season, league),
        eta_d_sigma = set_team_sigma("eta_d_step", teams, season, league),
        eta_h_mu = set_team_mu("eta_h_step", teams, season, league),
        eta_h_sigma = set_team_sigma("eta_h_step", teams, season, league),
        
        log_sigma_o_mu = set_global_mu("log_sigma_o_step", season, league),
        log_sigma_o_sigma = set_global_sigma("log_sigma_o_step", season, league),
        log_sigma_d_mu = set_global_mu("log_sigma_d_step", season, league),
        log_sigma_d_sigma = set_global_sigma("log_sigma_d_step", season, league),
        log_sigma_h_mu = set_global_mu("log_sigma_h_step", season, league),
        log_sigma_h_sigma = set_global_sigma("log_sigma_h_step", season, league),
        
        log_sigma_i_mu = set_global_mu("log_sigma_i", season, league),
        log_sigma_i_sigma = set_global_sigma("log_sigma_i", season, league) + log_sigma_i_step_sigma,
        
        gamma_0_mu = set_global_mu("gamma_0", season, league),
        gamma_0_sigma = set_global_sigma("gamma_0", season, league) + gamma_0_step_sigma,
        delta_0_mu = set_global_mu("delta_0", season, league),
        delta_0_sigma = set_global_sigma("delta_0", season, league) + delta_0_step_sigma,
        gamma_ot_mu = set_global_mu("gamma_ot", season, league),
        gamma_ot_sigma = set_global_sigma("gamma_ot", season, league) + gamma_ot_step_sigma,
        delta_ot_mu = set_global_mu("delta_ot", season, league),
        delta_ot_sigma = set_global_sigma("delta_ot", season, league) + delta_ot_step_sigma
      )
    
  }
  
  return(out)
  
}

#' Set the mean for the prior of a team parameter
#' 
#' @param parameter The parameter to set a prior for.
#' @param teams A tibble mapping ESPN `team_name` and `team_id` to an internal
#'        mapping id, `tid`.
#' @param season Season to extract results for. Seasons are identified by the
#'        year in which the last game was played.
#' @param league Which league to extract results for. Either "mens" or "womens".
set_team_mu <- function(parameter,
                        teams,
                        season,
                        league) {
  
  # rename variables for internal use
  season_int <- season
  league_int <- league
  
  out <- 
    arrow::read_parquet("out/historical/historical_parameters_team.parquet") %>%
    filter(season == season_int - 1,
           league == league_int,
           variable == parameter) %>%
    right_join(teams) %>%
    mutate(mean = replace_na(mean, 0)) %>%
    arrange(tid) %>%
    pull(mean)
  
  return(out)
  
}

#' Set the standard deviation for the prior of a team parameter
#' 
#' @param parameter The parameter to set a prior for.
#' @param teams A tibble mapping ESPN `team_name` and `team_id` to an internal
#'        mapping id, `tid`.
#' @param season Season to extract results for. Seasons are identified by the
#'        year in which the last game was played.
#' @param league Which league to extract results for. Either "mens" or "womens".
set_team_sigma <- function(parameter,
                           teams,
                           season,
                           league) {
  
  # rename variables for internal use
  season_int <- season
  league_int <- league
  
  out <- 
    arrow::read_parquet("out/historical/historical_parameters_team.parquet") %>%
    filter(season == season_int - 1,
           league == league_int,
           variable == parameter) %>%
    right_join(teams) %>%
    mutate(sd = replace_na(sd, 1)) %>%
    arrange(tid) %>%
    pull(sd)
  
  return(out)
  
}

#' Set the mean for the prior of a global parameter
#' 
#' @param parameter The parameter to set a prior for.
#' @param season Season to extract results for. Seasons are identified by the
#'        year in which the last game was played.
#' @param league Which league to extract results for. Either "mens" or "womens".
set_global_mu <- function(parameter,
                          season,
                          league) {
  
  # rename variables for internal use
  season_int <- season
  league_int <- league
  
  out <- 
    arrow::read_parquet("out/historical/historical_parameters_global.parquet") %>%
    filter(season == season_int - 1,
           league == league_int,
           variable == parameter) %>%
    pull(mean)
  
  return(out)
  
}

#' Set the mean for the prior of a global parameter
#' 
#' @param parameter The parameter to set a prior for.
#' @param season Season to extract results for. Seasons are identified by the
#'        year in which the last game was played.
#' @param league Which league to extract results for. Either "mens" or "womens".
set_global_sigma <- function(parameter,
                             season,
                             league) {
  
  # rename variables for internal use
  season_int <- season
  league_int <- league
  
  out <-
    arrow::read_parquet("out/historical/historical_parameters_global.parquet") %>%
    filter(season == season_int - 1,
           league == league_int,
           variable == parameter) %>%
    pull(sd)
  
  return(out)
  
}

#' Prep a games dataframe for modeling
#' 
#' @param games A tibble of game results as extracted by `scrape_games()`
prep_games <- function(games) {
  
  games %>%
    
    # mid-continent university has both a NA id and an actual id (564)
    # adjust to NA for all observations
    mutate(across(ends_with("_id"), ~if_else(.x == "564", NA, .x)),
           across(ends_with("_id"), ~if_else(is.na(.x), "missing", .x)),
           home_name = if_else(home_id == "missing", "missing", home_name),
           away_name = if_else(away_id == "missing", "missing", away_name)) %>%
    
    # filter to only div 1 teams in current league/season
    filter(home_name != "missing",
           away_name != "missing")
  
}

#' Map team names and ids to a tid for modeling
#' 
#' @param games A tibble of game results as extracted by `scrape_games()` and 
#'        cleaned by `prep_games()`.
assign_tids <- function(games) {
  
  bind_rows(games %>% distinct(team_name = home_name, team_id = home_id),
            games %>% distinct(team_name = away_name, team_id = away_id)) %>%
    distinct(team_name, team_id) %>%
    arrange(team_name) %>%
    rowid_to_column("tid")
  
}

#' Determine which days have not been run for the update model
#' 
#' @param league Which league to extract results for. Either "mens" or "womens".
#' @param model_name Basename of the model file
missing_days <- function(league, model_name) {
  
  # rename variables for internal use
  league_int <- league
  model_name_int <- model_name
  
  # all potential days to have run the model
  eligible_days <- 
    seq.Date(
      from = mdy("11/4/24"), 
      to = Sys.Date(), 
      by = "day"
    )
  
  # days that the model was actually run
  completed_days <- 
    arrow::read_parquet("out/model_log.parquet") %>%
    filter(model_name == model_name_int,
           league == league_int) %>%
    pull(date_max)
  
  # remove days that were run from eligible days
  out <- 
    eligible_days[which(!eligible_days %in% completed_days)]
  
  return(out)
  
}

#' Append logs in the event of an early exit from a model run
#' 
#' @param model_name Basename of the model file
#' @param message A message to display in the console in the event of early exit.
#' @param start_ts The timestame that the model run was started.
#' @param season Season to extract results for. Seasons are identified by the
#'        year in which the last game was played.
#' @param league Which league to extract results for. Either "mens" or "womens".
#' @param date The date in the current season to run the update for.
log_early_exit <- function(model_name, 
                           message,
                           start_ts,
                           season,
                           league,
                           date) {
  
  cli::cli_alert_warning(message)
  
  # evaluate processing time
  end_ts <- Sys.time()
  
  # generate model log
  model_log <-
    tibble(
      model_name = model_name,
      model_version = file.info(glue::glue("stan/{model_name}.stan"))$mtime,
      start_ts = start_ts,
      end_ts = end_ts,
      observations = 0,
      num_divergent = 0,
      num_max_treedepth = 0,
      samples = 0,
      season = season,
      league = league,
      date_min = date,
      date_max = date,
      target_variable = glue::glue("")
    )
  
  # append log
  model_log %>%
    append_parquet("out/model_log.parquet")
  
}

