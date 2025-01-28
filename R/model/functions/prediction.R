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
  
  # import games to be played
  games <- 
    arrow::read_parquet(glue::glue("out/update/{league}-games.parquet")) %>%
    filter(date == date_int)
  
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
    
    return(invisible())
    
  }
  
  # clean and format 
  games <- 
    games %>%
    prep_games()
  
  # assign tids for the set of games to predict
  teams <-
    games %>%
    assign_tids()
  
  N <- nrow(games)
  
  T <- nrow(teams)
  
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
  
  alpha <- log(70/40)
  
  team_params <-
    read_rds("out/update/team_parameters.rds") %>%
    filter(date == date_int,
           league == league_int,
           team_name %in% teams$team_name) %>%
    right_join(teams) %>%
    arrange(tid)
  
  Mu <- team_params$Mu
  Sigma <- team_params$Sigma
  
  beta_Mu <- array(dim = c(nrow(teams), 3))
  beta_Sigma <- array(dim = c(nrow(teams), 3, 3))
  
  # code for teams that haven't yet played and don't have a parameter in the
  # previous season
  log_sigma_step <- 
    arrow::read_parquet("out/historical/historical_parameters_global.parquet") %>%
    filter(season == 2024,
           league == league_int,
           str_detect(variable, "step"))
  
  log_sigma_rnorm <- function(log_sigma_step, parameter, n = 1e4) {
    
    summary <- 
      log_sigma_step %>%
      filter(variable == parameter)
    
    out <- rnorm(n, summary$mean, summary$sd)
    
    return(out)
    
  }
  
  beta_fresh <- 
    tibble(sigma_o = log_sigma_rnorm(log_sigma_step, "log_sigma_o_step"),
           sigma_d = log_sigma_rnorm(log_sigma_step, "log_sigma_d_step"),
           sigma_h = log_sigma_rnorm(log_sigma_step, "log_sigma_h_step")) %>%
    mutate(across(everything(), exp)) %>%
    bind_cols(beta_o = rnorm(nrow(.), 0, .$sigma_o),
              beta_d = rnorm(nrow(.), 0, .$sigma_d),
              beta_h = rnorm(nrow(.), 0, .$sigma_h)) %>%
    select(starts_with("beta")) %>%
    as.matrix()
  
  colMeans(beta_fresh)
  cov(beta_fresh)
  
  # code for teams that haven't played yet but _do_ have parameters in the 
  # previous season
  missing_teams <- 
    arrow::read_parquet("out/historical/historical_parameters_team.parquet") %>% 
    filter(season == 2024, 
           league == "mens", 
           team_name == "Tulsa", 
           str_detect(variable, "step"))
  
  log_sigma_step <- 
    arrow::read_parquet("out/historical/historical_parameters_global.parquet") %>%
    filter(season == 2024,
           league == "mens",
           str_detect(variable, "step"))
  
  rnorm_param <- function(data, parameter, n = 1e4) {
    
    summary <-
      data %>%
      filter(variable == parameter)
    
    out <- rnorm(n, summary$mean, summary$sd)
    
    return(out)
    
  }
  
  beta_draws <- 
    tibble(eta_o = rnorm_param(missing_teams, "eta_o_step"),
           eta_d = rnorm_param(missing_teams, "eta_d_step"),
           eta_h = rnorm_param(missing_teams, "eta_h_step"),
           sigma_o = rnorm_param(log_sigma_step, "log_sigma_o_step"),
           sigma_d = rnorm_param(log_sigma_step, "log_sigma_d_step"),
           sigma_h = rnorm_param(log_sigma_step, "log_sigma_h_step")) %>%
    mutate(across(starts_with("sigma"), exp),
           beta_o = eta_o * sigma_o,
           beta_d = eta_d * sigma_d,
           beta_h = eta_h * sigma_h) %>%
    select(starts_with("beta")) %>%
    as.matrix()
  
  colMeans(beta_draws)
  cov(beta_draws)
  
  missing_teams <- 
    arrow::read_parquet("out/historical/historical_parameters_team.parquet") %>% 
    filter(season == 2024, 
           league == "mens", 
           str_detect(variable, "step"))
  
  log_sigma_step <- 
    arrow::read_parquet("out/historical/historical_parameters_global.parquet") %>%
    filter(season == 2024,
           league == "mens",
           str_detect(variable, "step"))
  
  for (t in 1:nrow(teams)) {
    
    if (!is.null(Mu[[t]])) {
      
      beta_Mu[t,] <- Mu[[t]]
      beta_Sigma[t,,] <- Sigma[[t]]
      
    } else {
      
      team_params <-
        missing_teams %>%
        filter(team_name == teams$team_name[t])
      
      if (nrow(team_params) != 3) {
        
        beta_draws <-
          tibble(eta_o = rnorm(1e4, 0, 1),
                 eta_d = rnorm(1e4, 0, 1),
                 eta_h = rnorm(1e4, 0, 1))
        
      } else {
        
        beta_draws <- 
          tibble(eta_o = rnorm_param(team_params, "eta_o_step"),
                 eta_d = rnorm_param(team_params, "eta_d_step"),
                 eta_h = rnorm_param(team_params, "eta_h_step"))
        
      }
      
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
      
      beta_Mu[t,] <- colMeans(beta_draws)
      beta_Sigma[t,,] <- cov(beta_draws)
      
      if (any(is.na(beta_Mu[t,]))) {
        print(teams$team_name[t])
      }
      
    }
    
  }
  
  log_sigma_i <- 
    arrow::read_parquet("out/update/log_sigma_i.parquet") %>%
    filter(date == date_int,
           league == league_int)
  
  hurdle_params <- 
    read_rds("out/update/global_parameters.rds") %>%
    filter(date == date_int,
           league == league_int,
           parameter == "0")
  
  hurdle_Mu <- hurdle_params$Mu[[1]]
  hurdle_Sigma <- hurdle_params$Sigma[[1]]
  
  poisson_params <-
    read_rds("out/update/global_parameters.rds") %>%
    filter(date == date_int,
           league == league_int,
           parameter == "ot")
  
  poisson_Mu <- poisson_params$Mu[[1]]
  poisson_Sigma <- poisson_params$Sigma[[1]]
  
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
  
  predictions <- 
    prediction$sample(
      data = stan_data,
      seed = 2025,
      iter_warmup = 100,
      iter_sampling = 1250,
      chains = 8,
      parallel_chains = 8,
      fixed_param = TRUE
    )
  
  Ot <- predictions$summary("Ot")
  p_home_win <- predictions$summary("p_home_win")
  Y_home <- predictions$summary(paste0("Y_rep[1,", 1:nrow(games), "]"))
  Y_away <- predictions$summary(paste0("Y_rep[2,", 1:nrow(games), "]"))
  
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
              away_upper = Y_away$q95) %>%
    bind_cols(games %>%
                select(home_score, away_score)) %>%
    select(game_id,
           starts_with("home"), 
           starts_with("away"),
           -ends_with("name"),
           -ends_with("prob"),
           -home_id,
           -away_id) %>%
    pivot_longer(c(ends_with("median"),
                   ends_with("lower"),
                   ends_with("upper")),
                 names_to = "parameter",
                 values_to = "estimate") %>%
    separate(parameter, c("location", "parameter"), "_") %>%
    pivot_longer(ends_with("score"),
                 names_to = "location2",
                 values_to = "score") %>%
    filter(str_detect(location2, location)) %>%
    select(-location2) %>%
    pivot_wider(names_from = parameter,
                values_from = estimate) %>%
    ggplot(aes(x = score,
               y = median,
               ymin = lower,
               ymax = upper)) + 
    geom_pointrange() +
    geom_abline() +
    facet_wrap(~location)
  
}