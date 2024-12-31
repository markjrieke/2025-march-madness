library(tidyverse)
library(riekelib)
library(cmdstanr)

extract_eta <- function(eta) {
  
  fit$summary(eta) %>%
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

set_eta_sd <- function(eta, scale = 0.2, min = 0.1) {
  
  eta %>%
    filter(season == s - 1) %>%
    mutate(sd_prior = scale * sd,
           sd_prior = if_else(sd_prior < min, min, sd_prior),
           sd = sd + sd_prior) %>%
    right_join(teams) %>%
    mutate(sd = replace_na(sd, 1)) %>%
    arrange(tid) %>%
    pull(sd)
  
}

set_log_sigma_mu <- function(log_sigma) {
  
  log_sigma_o %>%
    filter(season == s - 1) %>%
    pull(mean)
  
}

set_log_sigma_sd <- function(log_sigma, scale = 0.2, min = 0.02) {
  
  log_sigma %>%
    filter(season == s - 1) %>%
    mutate(sd_prior = scale * sd,
           sd_prior = if_else(sd_prior < min, min, sd_prior),
           sd = sd + sd_prior) %>%
    pull(sd)
  
}

model <- 
  cmdstan_model(
    "stan/dev_58.stan",
    dir = "exe/"
  )

games <-
  arrow::read_parquet("data/games/games.parquet") %>%
  mutate(across(ends_with("_id"), ~if_else(.x == "564", NA, .x)),
         across(ends_with("_id"), ~if_else(is.na(.x), "missing", .x)),
         home_name = if_else(home_id == "missing", "missing", home_name),
         away_name = if_else(away_id == "missing", "missing", away_name)) %>%
  filter(league == "mens",
         home_name != "missing",
         away_name != "missing")

for (s in 2002:2024) {
  
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
  
  if (s == 2002) {
    
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
    
    eta_o_mu <- set_eta_mu(eta_o)
    eta_o_sigma <- set_eta_sd(eta_o)
    eta_d_mu <- set_eta_mu(eta_d)
    eta_d_sigma <- set_eta_sd(eta_d)
    eta_h_mu <- set_eta_mu(eta_h)
    eta_h_sigma <- set_eta_sd(eta_h)
    
    log_sigma_o_mu <- set_log_sigma_mu(log_sigma_o)
    log_sigma_o_sigma <- set_log_sigma_sd(log_sigma_o)
    log_sigma_d_mu <- set_log_sigma_mu(log_sigma_d)
    log_sigma_d_sigma <- set_log_sigma_sd(log_sigma_d)
    log_sigma_h_mu <- set_log_sigma_mu(log_sigma_h)
    log_sigma_h_sigma <- set_log_sigma_sd(log_sigma_h)
    
    log_sigma_i_mu <- set_log_sigma_mu(log_sigma_i)
    log_sigma_i_sigma <- set_log_sigma_sd(log_sigma_i)
    
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
      log_sigma_i_sigma = log_sigma_i_sigma
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
  
  cli::cli_h2("Post-processing...")
  
  if (s == 2002) {
    
    eta_o <- extract_eta("eta_o")
    eta_d <- extract_eta("eta_d")
    eta_h <- extract_eta("eta_h")
    
    log_sigma_o <- extract_log_sigma("log_sigma_o")
    log_sigma_d <- extract_log_sigma("log_sigma_d")
    log_sigma_h <- extract_log_sigma("log_sigma_h")
    log_sigma_i <- extract_log_sigma("log_sigma_i")
    
  } else {
    
    eta_o <- bind_rows(eta_o, extract_eta("eta_o"))
    eta_d <- bind_rows(eta_d, extract_eta("eta_d"))
    eta_h <- bind_rows(eta_h, extract_eta("eta_h"))
    
    log_sigma_o <- bind_rows(log_sigma_o, extract_log_sigma("log_sigma_o"))
    log_sigma_d <- bind_rows(log_sigma_d, extract_log_sigma("log_sigma_d"))
    log_sigma_h <- bind_rows(log_sigma_h, extract_log_sigma("log_sigma_h"))
    log_sigma_i <- bind_rows(log_sigma_i, extract_log_sigma("log_sigma_i"))
    
  }
  
}

preds <-
  fit$summary(c("Y_rep"))

preds %>%
  mutate(idx = str_remove_all(variable, "Y_rep|\\[|\\]")) %>%
  separate(idx, c("location", "rowid")) %>%
  mutate(location = if_else(location == "1", "home", "away"),
         rowid = as.integer(rowid)) %>%
  left_join(truth) %>%
  ggplot(aes(x = score,
             y = median,
             ymin = q5,
             ymax = q95)) + 
  # geom_point(alpha = 0.01) +
  geom_pointrange(alpha = 0.0625) +
  geom_abline(color = "white") + 
  geom_smooth(method = "lm") + 
  coord_flip() + 
  facet_wrap(~location) +
  theme_rieke()

fit$profiles()[[1]] %>%
  as_tibble() %>%
  arrange(desc(total_time))

fit$summary(paste0("log_sigma_", c("o", "d", "h", "a", "i")))
fit$draws(paste0("log_sigma_", c("o", "d", "h", "a", "i"))) %>% bayesplot::mcmc_pairs()
fit$summary(c(paste0("eta_", c("o", "d", "h", "a"), "[315]")))
fit$draws(paste0("eta_", c("o", "d", "h", "a"), "[315]")) %>% bayesplot::mcmc_pairs()
fit$summary(c(paste0("beta_", c("o", "d", "h", "a"), "[315]")))
fit$draws(paste0("beta_", c("o", "d", "h", "a"), "[315]")) %>% bayesplot::mcmc_pairs()
fit$cmdstan_diagnose()
