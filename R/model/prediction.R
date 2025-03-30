#' Run the prediction model
#' 
#' @description
#' The prediction model uses the output of the update model to generate pre-game
#' predictions of each team's score and probability of winning. Unlike other
#' models, the prediction model does not conduct inference or update parameters,
#' but rather runs as a `fixed parameter` model. 
#' 
#' @param league Which league to extract results for. Either "mens" or "womens".
#' @param date The date in the current season to run the update for. Filters to
#'        only games that occur on the specified date.
#' @param ... Unused
#' @param samples Number of posterior samples to generate.
#' @param chains Number of chains used to fit the model. All chains will be run
#'        in parallel, if available. 
#' @param log_sigma_i_step_sigma Pseudo-random walk scale for overdispersion 
#'        scale.
#' @param gamma_0_step_sigma,gamma_ot_step_sigma Pseudo-random walk scale for 
#'        slope parameters in the hurdle model.
#' @param delta_0_step_sigma,delta_ot_step_sigma Pseudo-random walk scale for 
#'        intercept parameters in the hurdle model.
run_prediction_model <- function(league,
                                 date,
                                 ...,
                                 samples = 1e4,
                                 chains = 8,
                                 log_sigma_i_step_sigma = 0.03,
                                 gamma_0_step_sigma = 0.2,
                                 delta_0_step_sigma = 0.05,
                                 gamma_ot_step_sigma = 0.2,
                                 delta_ot_step_sigma = 0.05) {
  
  cli::cli_h1(glue::glue("{str_to_title(league)} {scales::label_date('%b %d, %Y')(date)} Predictions"))
  
  # evaluate processing time
  start_ts <- Sys.time()
  
  # check whether or not functions.stan has been updated since last run
  if (out_of_date("exe/prediction", "stan/functions.stan")) {
    force_recompile <- TRUE
  } else {
    force_recompile <- FALSE
  }
  
  # compile model!
  prediction <- 
    cmdstan_model(
      "stan/prediction.stan",
      dir = "exe/",
      force_recompile = force_recompile
    )
  
  # rename variables for internal use
  date_int <- date
  league_int <- league
  season <- 2025
  
  # import games to be played
  games <- 
    arrow::read_parquet(glue::glue("out/update/{league}-games.parquet")) %>%
    filter(date == date_int)
  
  # clean and format 
  games <- 
    games %>%
    prep_games(remove_upcoming = FALSE)
  
  # early exit if no games were played on the specified date
  if (nrow(games) == 0) {
    
    message <- glue::glue("No {league} games played on {scales::label_date('%b %d, %Y')(date)}! Exiting...")
    log_early_exit("prediction", message, start_ts, season, league, date)
    return(invisible())
    
  }
  
  # assign tids for the set of games to predict
  teams <-
    games %>%
    assign_tids()
  
  # generate dataset to pass to stan
  stan_data <- 
    set_prediction_data(
      games, 
      teams, 
      league, 
      date,
      log_sigma_i_step_sigma,
      gamma_0_step_sigma,
      delta_0_step_sigma,
      gamma_ot_step_sigma,
      delta_ot_step_sigma
    )
  
  # predict!
  predictions <- 
    prediction$sample(
      data = stan_data,
      seed = 2025,
      chains = chains,
      parallel_chains = chains,
      iter_warmup = 100,
      iter_sampling = round(samples/chains),
      fixed_param = TRUE
    )
  
  # extract summary output cols
  Ot <- predictions$summary("Ot")
  p_home_win <- predictions$summary("p_home_win")
  Y_home <- predictions$summary(paste0("Y_rep[1,", 1:nrow(games), "]"))
  Y_away <- predictions$summary(paste0("Y_rep[2,", 1:nrow(games), "]"))
  
  # generate summary dataframe & append to output
  predictions <- 
    games %>%
    transmute(league,
              season,
              date,
              game_id,
              game_type,
              neutral,
              home_id,
              home_name,
              home_prob = p_home_win$mean,
              home_median = Y_home$median,
              home_lower = Y_home$q5,
              home_upper = Y_home$q95,
              away_id,
              away_name,
              away_prob = 1 - p_home_win$mean,
              away_median = Y_away$median,
              away_lower = Y_away$q5,
              away_upper = Y_away$q95)
  
  arrow::read_parquet("out/prediction/predictions.parquet") %>%
    anti_join(predictions, by = c("league", "season", "date", "game_id")) %>%
    bind_rows(predictions) %>%
    arrow::write_parquet("out/prediction/predictions.parquet")

  # evaluate processing time
  end_ts <- Sys.time()
  
  # generate model log
  model_log <-
    tibble(
      model_name = "prediction",
      model_version = file.info("stan/prediction.stan")$mtime,
      start_ts = start_ts,
      end_ts = end_ts,
      observations = stan_data$N,
      num_divergent = 0,
      num_max_treedepth = 0,
      samples = samples,
      season = 2025,
      league = league,
      date_min = mdy("11/1/2024"),
      date_max = date,
      target_variable = glue::glue("")
    )
  
  # append log
  model_log %>%
    append_parquet("out/model_log.parquet")
  
}

#' Prep prediction data for passing to Stan
#'
#' @param games The set of of games to generate predictions for, cleaned by
#'        `prep_games()`.
#' @param teams A tibble mapping ESPN `team_name` and `team_id` to an internal
#'        mapping id, `tid`.
#' @param league Which league to extract results for. Either "mens" or "womens".
#' @param date The date in the current season to run the update for. Filters to
#'        only games that occur on the specified date.
#' @param log_sigma_i_step_sigma Pseudo-random walk scale for overdispersion
#'        scale.
#' @param gamma_0_step_sigma,gamma_ot_step_sigma Pseudo-random walk scale for
#'        slope parameters in the hurdle model.
#' @param delta_0_step_sigma,delta_ot_step_sigma Pseudo-random walk scale for
#'        intercept parameters in the hurdle model.
set_prediction_data <- function(games,
                                teams,
                                league,
                                date,
                                log_sigma_i_step_sigma,
                                gamma_0_step_sigma,
                                delta_0_step_sigma,
                                gamma_ot_step_sigma,
                                delta_ot_step_sigma) {
  
  # total number of games
  N <- nrow(games)
  
  # total number of teams
  T <- nrow(teams)
  
  # whether (1) or not (0) to apply the home-court advantage
  V <- 1 - games$neutral
  
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
  
  # log-mean score per minute
  alpha <- log(70/40)
  
  # set mean and covariance of beta params
  beta <- set_beta(teams, league, date)
  beta_Mu <- beta$beta_Mu
  beta_Sigma <- beta$beta_Sigma
  
  # set overdispersion scale
  log_sigma_i <- set_log_sigma_i(league, date, log_sigma_i_step_sigma)
  
  # set overtime hurdle/poisson params
  hurdle_params <- set_overtime_params("0", league, date, gamma_0_step_sigma, delta_0_step_sigma)
  poisson_params <- set_overtime_params("ot", league, date, gamma_ot_step_sigma, delta_ot_step_sigma)
  
  hurdle_Mu <- hurdle_params$Mu
  hurdle_Sigma <- hurdle_params$Sigma
  
  poisson_Mu <- poisson_params$Mu
  poisson_Sigma <- poisson_params$Sigma
  
  stan_data <-
    list(
      N = N,
      T = T,
      V = V,
      tid = tid,
      alpha = alpha,
      beta_Mu = beta_Mu,
      beta_Sigma = beta_Sigma,
      log_sigma_i_mu = log_sigma_i$mean,
      log_sigma_i_sigma = log_sigma_i$sd,
      hurdle_Mu = hurdle_Mu,
      hurdle_Sigma = hurdle_Sigma,
      poisson_Mu = poisson_Mu,
      poisson_Sigma = poisson_Sigma
    )
  
  return(stan_data)
  
}

#' Set correlated team parameter arguments
#'
#' @description
#' Generates a list containing `beta_Mu`, a list of vectors representing the
#' mean values of `beta_o`, `beta_d`, and `beta_h`, and `beta_Sigma`, a list of
#' covariance matrices. Each element of each list maps to a team indexed by a
#' `tid`.
#'
#' By default, `set_beta()` will use the most recent mean vector and covariance
#' matrix for each team as output by the update model. If a team has not yet
#' played a game this season, `set_beta()` will set the team's mean vector and
#' covariance matrix by taking a random walk step from the previous season's
#' values. If the team did not play in the previous season, a standard normal
#' distribution will be used to generate skill parameter draws.
#'
#' @param teams A tibble mapping ESPN `team_name` and `team_id` to an internal
#'        mapping id, `tid`.
#' @param league Which league to extract results for. Either "mens" or "womens".
#' @param date The date in the current season to run the update for. Filters to
#'        only games that occur on the specified date.
set_beta <- function(teams,
                     league,
                     date) {
  
  # rename for internal use
  league_int <- league
  date_int <- date
  
  # import existing team parameters by the specified date
  team_params <-
    read_rds("out/update/team_parameters.rds") %>%
    filter(date == date_int,
           league == league_int,
           team_name %in% teams$team_name) %>%
    right_join(teams) %>%
    arrange(tid)
  
  # assign to a local temporary variable
  Mu <- team_params$Mu
  Sigma <- team_params$Sigma
  
  # create the output variable containers
  beta_Mu <- array(dim = c(nrow(teams), 3))
  beta_Sigma <- array(dim = c(nrow(teams), 3, 3))
  
  # pre-load dataframes with prior year's output to account for missing teams
  missing_teams <- 
    arrow::read_parquet("out/historical/historical_parameters_team.parquet") %>% 
    filter(season == 2024, 
           league == league_int, 
           str_detect(variable, "step"))
  
  log_sigma_step <- 
    arrow::read_parquet("out/historical/historical_parameters_global.parquet") %>%
    filter(season == 2024,
           league == league_int,
           str_detect(variable, "step"))
  
  # map prior mean and covariance vectors
  for (t in 1:nrow(teams)) {
    
    # assign most recent current-season priors if available
    if (!is.null(Mu[[t]])) {
      
      beta_Mu[t,] <- Mu[[t]]
      beta_Sigma[t,,] <- Sigma[[t]]
      
      # otherwise, infer priors from last season
    } else {
      
      team_params <-
        missing_teams %>%
        filter(team_name == teams$team_name[t])
      
      if (nrow(team_params) != 3) {
        
        # if priors from last season aren't available, use std normal samples
        beta_draws <-
          tibble(eta_o = rnorm(1e4, 0, 1),
                 eta_d = rnorm(1e4, 0, 1),
                 eta_h = rnorm(1e4, 0, 1))
        
      } else {
        
        # if priors from last season are available, generate samples according to the prior
        beta_draws <- 
          tibble(eta_o = rnorm_param(team_params, "eta_o_step"),
                 eta_d = rnorm_param(team_params, "eta_d_step"),
                 eta_h = rnorm_param(team_params, "eta_h_step"))
        
      }
      
      # convert from eta_* -> beta_*
      beta_draws <- 
        beta_draws %>%
        bind_cols(sigma_o = rnorm_param(log_sigma_step, "log_sigma_o_step"),
                  sigma_d = rnorm_param(log_sigma_step, "log_sigma_d_step"),
                  sigma_h = rnorm_param(log_sigma_step, "log_sigma_h_step")) %>%
        mutate(across(starts_with("sigma"), exp),
               beta_o = eta_o * sigma_o,
               beta_d = eta_d * sigma_d,
               beta_h = eta_h * sigma_h) %>%
        select(starts_with("beta")) %>%
        as.matrix()
      
      # assign simulate mean/covariance priors
      beta_Mu[t,] <- colMeans(beta_draws)
      beta_Sigma[t,,] <- cov(beta_draws)
      
    }
    
  }
  
  # return a list with the mean vectors / covariance matrices
  out <-
    list(
      beta_Mu = beta_Mu,
      beta_Sigma = beta_Sigma
    )
  
  return(out)
  
}

#' Util function for generating random normal draws from a summary dataframe
#'
#' @param data A dataframe summarizing parameter results ouptut by 
#'        `CmdStanModel$summary()`.
#' @param parameter The model parameter to generate random normal draws for.
#' @param n The number of random samples to generate.
rnorm_param <- function(data, parameter, n = 1e4) {
  
  summary <-
    data %>%
    filter(variable == parameter)
  
  out <- rnorm(n, summary$mean, summary$sd)
  
  return(out)
  
}

#' Set the observation-level overdispersion scale
#' 
#' @description 
#' Sets the observation-level overdispersion scale parameter. If necessary, uses
#' a random walk from the previous season's value.
#' 
#' @param league Which league to extract results for. Either "mens" or "womens".
#' @param date The date in the current season to run the update for. Filters to
#'        only games that occur on the specified date.
#' @param log_sigma_i_step_sigma Pseudo-random walk scale for overdispersion 
#'        scale.
set_log_sigma_i <- function(league,
                            date,
                            log_sigma_i_step_sigma) {
  
  # internal renaming for filtering
  league_int <- league
  date_int <- date
  
  # import most recent log_sigma_i if available
  log_sigma_i <- 
    arrow::read_parquet("out/update/log_sigma_i.parquet") %>%
    filter(date == date_int,
           league == league_int)
  
  # use random walk from last season if necessary
  if (nrow(log_sigma_i) == 0) {
    
    log_sigma_i <- 
      arrow::read_parquet("out/historical/historical_parameters_global.parquet") %>%
      filter(season == 2024,
             league == league_int,
             variable == "log_sigma_i") %>%
      mutate(sd = sd + log_sigma_i_step_sigma)
    
  }
  
  return(log_sigma_i)
  
}

#' Set overtime hurdle or poisson parameters
#' 
#' @description
#' Sets the hurdle or poisson parameters for the overtime hurdle calculation. If
#' necessary, uses a random walk from the previous season's value.
#' 
#' @param parameter The set of parameters to be extracted. Use "0" for the
#'        hurdle component and "ot" for the poisson component.
#' @param league Which league to extract results for. Either "mens" or "womens".
#' @param date The date in the current season to run the update for. Filters to
#'        only games that occur on the specified date.
#' @param gamma_step Pseudo-random walk scale for slope parameters in the hurdle 
#'        model.
#' @param delta_step Pseudo-random walk scale for intercept parameters in the 
#'        hurdle model.
set_overtime_params <- function(parameter,
                                league,
                                date,
                                gamma_step,
                                delta_step) {
  
  # rename for internal use
  parameter_int <- parameter
  league_int <- league
  date_int <- date
  
  # import the most recent set of parameters output by the update model
  params <-
    read_rds("out/update/global_parameters.rds") %>%
    filter(date == date_int,
           league == league_int,
           parameter == parameter_int)
  
  # use a random walk from last season's value if necessary
  if (nrow(params) == 0) {
    
    params <- 
      arrow::read_parquet("out/historical/historical_parameters_global.parquet") %>%
      filter(season == 2024,
             league == league_int,
             str_detect(variable, parameter_int)) %>%
      mutate(sd = if_else(str_detect(variable, "gamma"),
                          sd + gamma_step,
                          sd + delta_step),
             var = sd^2) 
    
    # convert tibble output to list of mean vector and covariance matrix
    Mu <- params$mean
    Sigma <- matrix(c(params$var[1], 0, 0, params$var[2]), nrow = 2)
    
    params <-
      tibble(
        Mu = list(Mu),
        Sigma = list(Sigma)
      )
    
  }
  
  # return output as a list
  out <-
    list(
      Mu = params$Mu[[1]],
      Sigma = params$Sigma[[1]] 
    )
  
  return(out)
  
}
