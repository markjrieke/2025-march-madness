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

extract_parameter <- function(parameter) {
  
  fit$summary(parameter) %>%
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

set_parameter_mu <- function(parameter) {
  
  parameter %>%
    filter(season == s - 1) %>%
    pull(mean)
  
}

set_parameter_sd <- function(parameter) {
  
  parameter %>%
    filter(season == s - 1) %>%
    pull(sd)
  
}

model <- 
  cmdstan_model(
    "stan/historical.stan",
    dir = "exe/"
  )

recovery <-
  cmdstan_model(
    "stan/recovery.stan",
    dir = "exe/"
  )

beta_o_step_sigma <- 0.03
beta_d_step_sigma <- 0.03
beta_h_step_sigma <- 0.03
log_sigma_i_step_sigma <- 0.03
gamma_0_step_sigma <- 0.2
delta_0_step_sigma <- 0.05
gamma_ot_step_sigma <- 0.2
delta_ot_step_sigma <- 0.05

games <-
  arrow::read_parquet("data/games/games.parquet") %>%
  mutate(across(ends_with("_id"), ~if_else(.x == "564", NA, .x)),
         across(ends_with("_id"), ~if_else(is.na(.x), "missing", .x)),
         home_name = if_else(home_id == "missing", "missing", home_name),
         away_name = if_else(away_id == "missing", "missing", away_name)) %>%
  filter(league == "mens",
         home_name != "missing",
         away_name != "missing")

smin <- 2023

for (s in smin:2024) {
  
  tictoc::tic()
  
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
    
    gamma_0_mu <- 0
    gamma_0_sigma <- 1
    delta_0_mu <- 2
    delta_0_sigma <- 1
    gamma_ot_mu <- 0
    gamma_ot_sigma <- 1
    delta_ot_mu <- -1.5
    delta_ot_sigma <- 1
    
  } else {
    
    eta_o_mu <- set_eta_mu(eta_o_step)
    eta_o_sigma <- set_eta_sd(eta_o_step)
    eta_d_mu <- set_eta_mu(eta_d_step)
    eta_d_sigma <- set_eta_sd(eta_d_step)
    eta_h_mu <- set_eta_mu(eta_h_step)
    eta_h_sigma <- set_eta_sd(eta_h_step)
    
    log_sigma_o_mu <- set_parameter_mu(log_sigma_o_step)
    log_sigma_o_sigma <- set_parameter_sd(log_sigma_o_step)
    log_sigma_d_mu <- set_parameter_mu(log_sigma_d_step)
    log_sigma_d_sigma <- set_parameter_sd(log_sigma_d_step)
    log_sigma_h_mu <- set_parameter_mu(log_sigma_h_step)
    log_sigma_h_sigma <- set_parameter_sd(log_sigma_h_step)
    
    log_sigma_i_mu <- set_parameter_mu(log_sigma_i)
    log_sigma_i_sigma <- set_parameter_sd(log_sigma_i) + log_sigma_i_step_sigma
    
    gamma_0_mu <- set_parameter_mu(gamma_0)
    gamma_0_sd <- set_parameter_sd(gamma_0) + gamma_0_step_sigma
    delta_0_mu <- set_parameter_mu(delta_0)
    delta_0_sd <- set_parameter_sd(delta_0) + delta_0_step_sigma
    gamma_ot_mu <- set_parameter_mu(gamma_ot)
    gamma_ot_sd <- set_parameter_sd(gamma_ot) + gamma_ot_step_sigma
    delta_ot_mu <- set_parameter_mu(delta_ot)
    delta_ot_sd <- set_parameter_sd(delta_ot) + delta_ot_step_sigma
    
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
      gamma_0_mu = gamma_0_mu,
      gamma_0_sigma = gamma_0_sigma,
      delta_0_mu = delta_0_mu,
      delta_0_sigma = delta_0_sigma,
      gamma_ot_mu = gamma_ot_mu,
      gamma_ot_sigma = gamma_ot_sigma,
      delta_ot_mu = delta_ot_mu,
      delta_ot_sigma = delta_ot_sigma,
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
  
  log_sigma_o_s <- extract_parameter("log_sigma_o")
  log_sigma_d_s <- extract_parameter("log_sigma_d")
  log_sigma_h_s <- extract_parameter("log_sigma_h")
  log_sigma_i_s <- extract_parameter("log_sigma_i")
  
  cli::cli_h2("Extracting hurdle parameters")
  
  gamma_0_s <- extract_parameter("gamma_0")
  delta_0_s <- extract_parameter("delta_0")
  gamma_ot_s <- extract_parameter("gamma_ot")
  delta_ot_s <- extract_parameter("delta_ot")
  
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
    
    gamma_0 <- gamma_0_s
    delta_0 <- delta_0_s
    gamma_ot <- gamma_ot_s
    delta_ot <- delta_ot_s
    
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
    
    gamma_0 <- bind_rows(gamma_0, gamma_0_s)
    delta_0 <- bind_rows(delta_0, delta_0_s)
    gamma_ot <- bind_rows(gamma_ot, gamma_ot_s)
    delta_ot <- bind_rows(delta_ot, delta_ot_s)
    
    
  }
  
  tictoc::toc()
  
}

beta_o_draws <- fit$draws("beta_o", format = "matrix")
beta_d_draws <- fit$draws("beta_d", format = "matrix")
beta_h_draws <- fit$draws("beta_h", format = "matrix")

hurdle_draws <- fit$draws(c("gamma_0", "delta_0"), format = "matrix")
poisson_draws <- fit$draws(c("gamma_ot", "delta_ot"), format = "matrix")

beta_Mu <- array(dim = c(T, 3))
beta_Sigma <- array(dim = c(T, 3, 3))

hurdle_Mu <- colMeans(hurdle_draws)
hurdle_Sigma <- cov(hurdle_draws)
poisson_Mu <- colMeans(poisson_draws)
poisson_Sigma <- cov(poisson_draws)

for (t in 1:T) {
  
  beta_draws <- 
    cbind(
      beta_o_draws[,t], 
      beta_d_draws[,t], 
      beta_h_draws[,t]
    )
  
  beta_Mu[t,] <- colMeans(beta_draws)
  beta_Sigma[t,,] <- cov(beta_draws)
  
}

prediction <-
  cmdstan_model(
    "stan/prediction.stan",
    dir = "exe/"
  )

stan_data <- 
  list(
    N = 2,
    T = 4,
    V = c(0, 0),
    tid = array(1:4, dim = c(2,2)),
    alpha = log(70/40),
    beta_Mu = beta_Mu[c(326, 4, 192, 244),],
    beta_Sigma = beta_Sigma[c(326, 4, 192, 244),,],
    log_sigma_i = log_sigma_i[2,]$mean,
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

predictions$draws(c("Y_rep", "Ot"), format = "df") %>%
  as_tibble() %>%
  pivot_longer(starts_with("Y_rep"),
               names_to = "variable",
               values_to = "score") %>%
  mutate(variable = str_remove_all(variable, "Y_rep\\[|\\]")) %>%
  separate(variable, c("tid", "game")) %>%
  mutate(across(c(tid, game), as.integer),
         location = if_else(tid %in% c(1, 3), "home", "away")) %>%
  select(-tid) %>%
  pivot_wider(names_from = location,
              values_from = score) %>%
  mutate(n_ot = if_else(game == 1, `Ot[1]`, `Ot[2]`),
         game = if_else(game == 1, 
                        "Game 1: UConn vs. Alabama",
                        "Game 2: NC State vs. Purdue")) %>%
  # group_by(game) %>% percent(home > away)
  count(game, home, away, n_ot) %>%
  ggplot(aes(x = home,
             y = away,
             color = as.factor(n_ot),
             alpha = n)) + 
  geom_point() + 
  facet_wrap(~game) + 
  theme_rieke()

# save intermittant results to avoid refitting the whole thing each time
eta_o_step %>% arrow::write_parquet("out/dev/eta_o_step.parquet")
eta_d_step %>% arrow::write_parquet("out/dev/eta_d_step.parquet")
eta_h_step %>% arrow::write_parquet("out/dev/eta_h_step.parquet")

log_sigma_o_step %>% arrow::write_parquet("out/dev/log_sigma_o_step.parquet")
log_sigma_d_step %>% arrow::write_parquet("out/dev/log_sigma_d_step.parquet")
log_sigma_h_step %>% arrow::write_parquet("out/dev/log_sigma_h_step.parquet")

beta_o %>% arrow::write_parquet("out/dev/beta_o.parquet")
beta_d %>% arrow::write_parquet("out/dev/beta_d.parquet")
beta_h %>% arrow::write_parquet("out/dev/beta_h.parquet")

log_sigma_o %>% arrow::write_parquet("out/dev/log_sigma_o.parquet")
log_sigma_d %>% arrow::write_parquet("out/dev/log_sigma_d.parquet")
log_sigma_h %>% arrow::write_parquet("out/dev/log_sigma_h.parquet")
log_sigma_i %>% arrow::write_parquet("out/dev/log_sigma_i.parquet")

gamma_0 %>% arrow::write_parquet("out/dev/gamma_0.parquet")
delta_0 %>% arrow::write_parquet("out/dev/delta_0.parquet")
gamma_ot %>% arrow::write_parquet("out/dev/gamma_ot.parquet")
delta_ot %>% arrow::write_parquet("out/dev/delta_ot.parquet")

# some posterior explorations
bind_rows(beta_o %>% mutate(variable = "beta_o"),
          beta_d %>% mutate(variable = "beta_d"),
          beta_h %>% mutate(variable = "beta_h")) %>%
  nest(data = -team_name) %>%
  mutate(obs = map_int(data, nrow)) %>%
  # filter(obs == 69) %>%
  slice_sample(n = 12) %>%
  unnest(data) %>%
  ggplot(aes(x = season,
             y = median,
             ymin = q5,
             ymax = q95)) + 
  geom_ribbon(aes(fill = variable),
              alpha = 0.25) + 
  geom_line(aes(color = variable)) +
  scale_color_brewer(palette = "Dark2") + 
  scale_fill_brewer(palette = "Dark2") + 
  facet_wrap(~team_name) + 
  theme_rieke()

bind_rows(log_sigma_o,
          log_sigma_d,
          log_sigma_h,
          log_sigma_i) %>%
  ggplot(aes(x = season,
             y = median,
             ymin = q5,
             ymax = q95)) + 
  geom_ribbon(alpha = 0.25) + 
  geom_line() +
  facet_wrap(~variable) + 
  theme_rieke()

bind_rows(gamma_0,
          delta_0,
          gamma_ot,
          delta_ot) %>%
  ggplot(aes(x = season,
             y = median,
             ymin = q5,
             ymax = q95)) + 
  geom_ribbon(alpha = 0.25) + 
  geom_line() + 
  facet_wrap(~variable) +
  theme_rieke()

