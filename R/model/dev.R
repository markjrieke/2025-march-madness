library(tidyverse)
library(riekelib)
library(cmdstanr)

recover_priors <- function(step) {
  
  recovery_data <-
    list(
      S = 8 * 500,
      T = nrow(step),
      beta_mean = step$mean,
      beta_sd = step$sd
    )
  
  recovery_fit <-
    recovery$sample(
      data = recovery_data,
      seed = 2025,
      init = 0.01,
      step_size = 0.002,
      chains = 8,
      parallel_chains = 8,
      iter_warmup = 500,
      iter_sampling = 500,
      refresh = 1000
    )
  
  out <-
    list(
      eta = recovery_fit$summary("eta"),
      log_sigma = recovery_fit$summary("log_sigma")
    )
  
  return(out)
  
}

extract_eta_step <- function(beta_recovery) {
  
  beta_recovery$eta %>%
    mutate(tid = parse_number(variable)) %>%
    left_join(teams) %>%
    mutate(season = s) %>%
    select(-c(variable, tid)) %>%
    relocate(season, team_name)
  
}

extract_log_sigma_step <- function(beta_recovery) {
  
  beta_recovery$log_sigma %>%
    mutate(season = s) %>%
    relocate(season)
  
}

extract_beta <- function(beta) {
  
  fit$summary(beta) %>%
    mutate(tid = parse_number(variable)) %>%
    left_join(teams) %>%
    mutate(season = s) %>%
    select(-c(variable, tid)) %>%
    relocate(season, team_name)
  
}

extract_log_sigma <- function(log_sigma) {
  
  fit$summary(log_sigma) %>%
    mutate(season = s) %>%
    relocate(season)
  
}

set_eta_mu <- function(eta) {
  
  eta %>%
    filter(season == s - 1) %>%
    right_join(teams) %>%
    mutate(mean = replace_na(mean, 0)) %>%
    arrange(tid) %>%
    pull(mean)
  
}

set_eta_sd <- function(eta) {
  
  eta %>%
    filter(season == s - 1) %>%
    right_join(teams) %>%
    mutate(sd = replace_na(sd, 1)) %>%
    arrange(tid) %>%
    pull(sd)
  
}

set_log_sigma_mu <- function(log_sigma) {
  
  log_sigma %>%
    filter(season == s - 1) %>%
    pull(mean)
  
}

set_log_sigma_sd <- function(log_sigma) {
  
  log_sigma %>%
    filter(season == s - 1) %>%
    pull(sd)
  
}

model <- 
  cmdstan_model(
    "stan/dev_61.stan",
    dir = "exe/"
  )

recovery <-
  cmdstan_model(
    "stan/dev_60.stan",
    dir = "exe/"
  )

beta_o_step_sigma <- 0.03
beta_d_step_sigma <- 0.03
beta_h_step_sigma <- 0.03

games <-
  arrow::read_parquet("data/games/games.parquet") %>%
  mutate(across(ends_with("_id"), ~if_else(.x == "564", NA, .x)),
         across(ends_with("_id"), ~if_else(is.na(.x), "missing", .x)),
         home_name = if_else(home_id == "missing", "missing", home_name),
         away_name = if_else(away_id == "missing", "missing", away_name)) %>%
  filter(league == "mens",
         home_name != "missing",
         away_name != "missing")

smin <- 2018

for (s in smin:2024) {
  
  cli::cli_h1(glue::glue("Season {s-1}-{str_sub(as.character(s), -2)}"))
  cli::cli_h2("Pre-processing...")
  
  season_games <-
    games %>%
    filter(season == s)
  
  teams <- 
    season_games %>%
    select(ends_with("name")) %>%
    pivot_longer(everything(),
                 names_to = "home_away",
                 values_to = "team_name") %>%
    distinct(team_name) %>%
    arrange(team_name) %>%
    rowid_to_column("tid")
  
  tid <- 
    season_games %>%
    left_join(teams, by = c("home_name" = "team_name")) %>%
    rename(tid1 = tid) %>%
    left_join(teams, by = c("away_name" = "team_name")) %>%
    rename(tid2 = tid) %>%
    select(starts_with("tid")) %>%
    as.matrix() %>%
    t()
  
  Y <- 
    season_games %>%
    select(home_score, away_score) %>%
    as.matrix() %>%
    t()
  
  T <- max(tid)
  
  if (s == smin) {
    
    eta_o_mu <- rep(0, T)
    eta_o_sigma <- rep(1, T)
    eta_d_mu <- rep(0, T)
    eta_d_sigma <- rep(1, T)
    eta_h_mu <- rep(0, T)
    eta_h_sigma <- rep(1, T)
    
    log_sigma_o_mu <- -3
    log_sigma_o_sigma <- 0.5
    log_sigma_d_mu <- -3
    log_sigma_d_sigma <- 0.5
    log_sigma_h_mu <- -3
    log_sigma_h_sigma <- 0.5
    
    log_sigma_i_mu <- -3
    log_sigma_i_sigma <- 0.5
    
  } else {
    
    eta_o_mu <- set_eta_mu(eta_o_step)
    eta_o_sigma <- set_eta_sd(eta_o_step)
    eta_d_mu <- set_eta_mu(eta_d_step)
    eta_d_sigma <- set_eta_sd(eta_d_step)
    eta_h_mu <- set_eta_mu(eta_h_step)
    eta_h_sigma <- set_eta_sd(eta_h_step)
    
    log_sigma_o_mu <- set_log_sigma_mu(log_sigma_o_step)
    log_sigma_o_sigma <- set_log_sigma_sd(log_sigma_o_step)
    log_sigma_d_mu <- set_log_sigma_mu(log_sigma_d_step)
    log_sigma_d_sigma <- set_log_sigma_sd(log_sigma_d_step)
    log_sigma_h_mu <- set_log_sigma_mu(log_sigma_h_step)
    log_sigma_h_sigma <- set_log_sigma_sd(log_sigma_h_step)
    
    log_sigma_i_mu <- set_log_sigma_mu(log_sigma_i)
    log_sigma_i_sigma <- set_log_sigma_sd(log_sigma_i) + 0.03
    
  }
  
  # log-mean
  alpha <- log(70/40)
  
  stan_data <-
    list(
      N = nrow(season_games),
      T = T,
      tid = tid,
      Y = Y,
      O = season_games$n_ot,
      V = 1 - season_games$neutral,
      alpha = alpha,
      eta_o_mu = eta_o_mu,
      eta_o_sigma = eta_o_sigma,
      eta_d_mu = eta_d_mu,
      eta_d_sigma = eta_d_sigma,
      eta_h_mu = eta_h_mu,
      eta_h_sigma = eta_h_sigma,
      log_sigma_o_mu = log_sigma_o_mu,
      log_sigma_o_sigma = log_sigma_o_sigma,
      log_sigma_d_mu = log_sigma_d_mu,
      log_sigma_d_sigma = log_sigma_d_sigma,
      log_sigma_h_mu = log_sigma_h_mu,
      log_sigma_h_sigma = log_sigma_h_sigma,
      log_sigma_i_mu = log_sigma_i_mu,
      log_sigma_i_sigma = log_sigma_i_sigma,
      beta_o_step_sigma = beta_o_step_sigma,
      beta_d_step_sigma = beta_d_step_sigma,
      beta_h_step_sigma = beta_h_step_sigma
    )
  
  fit <-
    model$sample(
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
  
  cli::cli_h2("Extracting beta_*_step")
  
  # extract information needed to set next priors
  beta_o_step_s <- fit$summary("beta_o_step")
  beta_d_step_s <- fit$summary("beta_d_step")
  beta_h_step_s <- fit$summary("beta_h_step")
  
  beta_o_recovery_s <- recover_priors(beta_o_step_s)
  beta_d_recovery_s <- recover_priors(beta_d_step_s)
  beta_h_recovery_s <- recover_priors(beta_h_step_s)
  
  eta_o_step_s <- extract_eta_step(beta_o_recovery_s)
  eta_d_step_s <- extract_eta_step(beta_d_recovery_s)
  eta_h_step_s <- extract_eta_step(beta_h_recovery_s)
  
  log_sigma_o_step_s <- extract_log_sigma_step(beta_o_recovery_s)
  log_sigma_d_step_s <- extract_log_sigma_step(beta_d_recovery_s)
  log_sigma_h_step_s <- extract_log_sigma_step(beta_h_recovery_s)
  
  cli::cli_h2("Extracting beta_*")
  
  # extract posteriors
  beta_o_s <- extract_beta("beta_o")
  beta_d_s <- extract_beta("beta_d")
  beta_h_s <- extract_beta("beta_h")
  
  cli::cli_h2("Extracting log_sigma_*")
  
  log_sigma_o_s <- extract_log_sigma("log_sigma_o")
  log_sigma_d_s <- extract_log_sigma("log_sigma_d")
  log_sigma_h_s <- extract_log_sigma("log_sigma_h")
  log_sigma_i_s <- extract_log_sigma("log_sigma_i")
  
  if (s == smin) {
    
    # initialize prior setting tracking
    eta_o_step <- eta_o_step_s
    eta_d_step <- eta_d_step_s
    eta_h_step <- eta_h_step_s
    
    log_sigma_o_step <- log_sigma_o_step_s
    log_sigma_d_step <- log_sigma_d_step_s
    log_sigma_h_step <- log_sigma_h_step_s
    
    # initialize posterior tracking
    beta_o <- beta_o_s
    beta_d <- beta_d_s
    beta_h <- beta_h_s
    
    log_sigma_o <- log_sigma_o_s
    log_sigma_d <- log_sigma_d_s
    log_sigma_h <- log_sigma_h_s
    log_sigma_i <- log_sigma_i_s
    
  } else {
    
    # append prior setting tracking
    eta_o_step <- bind_rows(eta_o_step, eta_o_step_s)
    eta_d_step <- bind_rows(eta_d_step, eta_d_step_s)
    eta_h_step <- bind_rows(eta_h_step, eta_h_step_s)
    
    log_sigma_o_step <- bind_rows(log_sigma_o_step, log_sigma_o_step_s)
    log_sigma_d_step <- bind_rows(log_sigma_d_step, log_sigma_d_step_s)
    log_sigma_h_step <- bind_rows(log_sigma_h_step, log_sigma_h_step_s)
    
    # append posterior tracking
    beta_o <- bind_rows(beta_o, beta_o_s)
    beta_d <- bind_rows(beta_d, beta_d_s)
    beta_h <- bind_rows(beta_h, beta_h_s)
    
    log_sigma_o <- bind_rows(log_sigma_o, log_sigma_o_s)
    log_sigma_d <- bind_rows(log_sigma_d, log_sigma_d_s)
    log_sigma_h <- bind_rows(log_sigma_h, log_sigma_h_s)
    log_sigma_i <- bind_rows(log_sigma_i, log_sigma_i_s)
    
  }
  
}

preds <- fit$summary("log_lambda")

corrected_data <- 
  season_games %>%
  rowid_to_column() %>%
  select(rowid,
         n_ot,
         home_score,
         away_score) %>%
  pivot_longer(ends_with("score"),
               names_to = "location",
               values_to = "score") %>%
  mutate(location = str_remove(location, "_score")) %>%
  left_join(preds %>%
              mutate(variable = str_remove_all(variable, "log_lambda\\[|\\]")) %>%
              separate(variable, c("location", "rowid")) %>%
              mutate(rowid = as.integer(rowid),
                     location = if_else(location == "1", "home", "away"))) %>%
  mutate(truth = log(score/(40 + 5 * n_ot)),
         across(c(mean, median, q5, q95), ~.x - log(40 + 5 * n_ot)))

corrected_data %>%
  # slice_sample(n = 1000) %>%
  ggplot(aes(x = median,
             xmin = q5,
             xmax = q95,
             y = truth)) + 
  geom_point(alpha = 0.01) + 
  geom_abline(color = "red") + 
  geom_smooth(method = "lm") + 
  facet_wrap(~location) + 
  theme_rieke() + 
  expand_limits(x = c(-0.5, 1.5),
                y = c(-0.5, 1.5))

# ~62%!
corrected_data %>%
  mutate(within = truth > q5 & truth < q95) %>%
  percent(within)

correct <-
  cmdstan_model(
    "stan/dev_65.stan",
    dir = "exe/"
  )

stan_data <-
  list(
    N = nrow(corrected_data),
    X_mu = corrected_data$mean,
    X_sigma = corrected_data$sd,
    Y = corrected_data$truth,
    lid = if_else(corrected_data$location == "home", 1, 2),
    alpha_mu = 0,
    alpha_sigma = 1,
    beta_mu = 0,
    beta_sigma = 1,
    log_sigma_mu = -2,
    log_sigma_sigma = 1
  )

correct_fit <-
  correct$sample(
    data = stan_data,
    seed = 2025,
    init = 0.01,
    step_size = 0.002,
    chains = 8,
    parallel_chains = 8,
    iter_warmup = 500,
    iter_sampling = 500,
    refresh = 100
  )

correct_preds <- correct_fit$summary("Y_rep")

correct_preds %>%
  bind_cols(corrected_data %>% select(truth, location)) %>%
  slice_sample(n = 1000) %>%
  ggplot(aes(y = truth,
             x = median,
             xmin = q5,
             xmax = q95)) + 
  geom_pointrange(alpha = 0.05) +
  geom_abline(color = "red") + 
  geom_smooth(method = "lm") + 
  facet_wrap(~location) + 
  theme_rieke() +
  expand_limits(x = c(-0.5, 1.5),
                y = c(-0.5, 1.5))

# ~90% (yay!)
correct_preds %>%
  bind_cols(corrected_data %>% select(truth, location)) %>%
  mutate(within = truth > q5 & truth < q95) %>%
  percent(within)

X <- correct_fit$summary("X")

X %>%
  select(median, q5, q95) %>%
  rename_with(~paste0(.x, "_X")) %>%
  bind_cols(corrected_data) %>%
  slice_sample(n = 1000) %>%
  ggplot(aes(x = median,
             y = median_X)) + 
  geom_pointrange(aes(ymin = q5_X,
                      ymax = q95_X),
                  alpha = 0.05) + 
  geom_pointrange(aes(xmin = q5,
                      xmax = q95),
                  alpha = 0.05) + 
  geom_abline(color = "red") + 
  theme_rieke()
