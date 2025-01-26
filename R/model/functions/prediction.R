run_prediction_model <- function(league,
                                 date) {
  
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
  
  if (date == Sys.Date()) {
    
    # write a scraper for the current date
    
  } else {
    
    # import games to be played
    games <- 
      arrow::read_parquet(glue::glue("out/update/{league}-games.parquet")) %>%
      filter(date == date_int)
    
  }
  
  # early exit if no games were played on the specified date
  if (nrow(games) == 0) {
    
    cli::cli_alert_warning(glue::glue("No {league} games played on {scales::label_date('%b %d, %Y')(date)}! Exiting..."))
    
    # evaluate processing time
    end_ts <- Sys.time()
    
    # generate model log
    model_log <-
      tibble(
        model_name = "prediction",
        model_version = file.info("stan/prediction.stan")$mtime,
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
  
  # clean and format 
  games <- 
    games %>%
    prep_games()
  
  # assign tids for the set of games to predict
  teams <-
    games %>%
    assign_tids()
  
  Mu <- 
    read_rds("out/update/team_parameters.rds") %>%
    filter(date == date_int,
           league == league_int,
           team_name %in% teams$team_name) %>%
    left_join(teams) %>%
    arrange(tid) %>%
    pull(Mu)
  
  Sigma <- 
    read_rds("out/update/team_parameters.rds") %>%
    filter(date == date_int,
           league == league_int,
           team_name %in% teams$team_name) %>%
    left_join(teams) %>%
    pull(Sigma)
  
  beta_Mu <- array(dim = c(nrow(teams), 3))
  beta_Sigma <- array(dim = c(nrow(teams), 3, 3))
  
  for (t in 1:nrow(teams)) {
    
    beta_Mu[t,] <- Mu[[t]]
    beta_Sigma[t,,] <- Sigma[[t]]
    
  }
  
  stan_data <- 
    list(
      N = 8,
      T = 16,
      V = rep(0, 8),
      tid = array(1:16, dim = c(2,8)),
      alpha = log(70/40),
      beta_Mu = beta_Mu[bracket_ids,],
      beta_Sigma = beta_Sigma[bracket_ids,,],
      log_sigma_i = log_sigma_i[2,]$mean,
      hurdle_Mu = hurdle_Mu,
      hurdle_Sigma = hurdle_Sigma,
      poisson_Mu = poisson_Mu,
      poisson_Sigma = poisson_Sigma
    )
  
}