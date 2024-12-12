library(tidyverse)
library(cmdstanr)
library(riekelib)

mu0 <- log(c(65, 70)/40)
sigma <- 0.03

# set.seed(1234)
sims <- 
  crossing(season = 2000:2020,
           team_id = 1:2) %>%
  mutate(mu = map_dbl(team_id, ~mu0[.x])) %>%
  bind_cols(eta = rnorm(nrow(.))) %>%
  mutate(delta = if_else(season == 2000, 0, eta * sigma)) %>%
  group_by(team_id) %>%
  mutate(delta = cumsum(delta),
         mu = mu + delta) %>%
  ungroup() %>%
  mutate(lambda = exp(mu) * 40) %>%
  crossing(game = 1:30) %>%
  bind_cols(points = rpois(nrow(.), .$lambda))
  

sims %>% 
  ggplot(aes(x = season,
             y = points,
             color = as.factor(team_id),
             fill = as.factor(team_id))) + 
  geom_point(alpha = 0.25) +
  geom_smooth() +
  scale_color_brewer(palette = "Dark2") + 
  scale_fill_brewer(palette = "Dark2") +
  facet_wrap(~team_id, ncol = 1) +
  theme_rieke()


model <-
  cmdstan_model(
    "stan/dev_26.stan",
    dir = "exe/"
  )

rm(log_sigma_logs)
rm(beta_logs)

extract_beta_log <- function(fit) {
  
  fit$summary("beta") %>%
    mutate(index = str_sub(variable, str_locate(variable, "beta")[,2] + 1),
           index = str_remove_all(index, "\\[|\\]"),
           variable = "beta") %>%
    separate(index, c("tid", "sid")) %>%
    mutate(across(c(tid, sid), as.integer)) %>%
    left_join(game_data %>% distinct(season, tid, sid)) %>%
    select(-sid) %>%
    mutate(season = if_else(is.na(season), s - 1, season),
           s = s,
           g = g,
           updated_ts = Sys.time())
  
}

extract_log_sigma_log <- function(fit) {
  
  fit$summary("log_sigma") %>%
    mutate(s = s,
           g = g,
           updated_ts = Sys.time())
  
}

extract_eta_log <- function(fit) {
  
  fit$summary("eta") %>%
    mutate(index = str_sub(variable, str_locate(variable, "eta")[,2] + 1),
           index = str_remove_all(index, "\\[|\\]"),
           variable = "eta") %>%
    separate(index, c("tid", "sid")) %>%
    mutate(across(c(tid, sid), as.integer),
           sid = sid + 1) %>%
    left_join(game_data %>% distinct(season, tid, sid)) %>%
    select(-sid) %>%
    mutate(s = s,
           g = g,
           updated_ts = Sys.time())
  
}

s <- 2000

for (s in unique(sims$season)) {
  
  season_data <- 
    sims %>%
    filter(season == s) %>%
    mutate(sid = rank(season, ties.method = "min") + 1,
           tid = team_id,
           Y = points)
  
  g_max <- max(season_data$game)
  
  for (g in 1:g_max) {
    
    g1m <- g - 1
    
    game_data <-
      season_data %>%
      filter(game == g)
    
    T <- max(game_data$tid)
    S <- max(game_data$sid)
    
    observations <- 
      list(
        N = nrow(game_data),
        T = T,
        S = S,
        tid = game_data$tid,
        sid = game_data$sid,
        Y = game_data$Y
      )
    
    eta_mu <- matrix(nrow = T, ncol = S - 1)
    eta_sigma <- matrix(nrow = T, ncol = S - 1)
    
    if (!exists("beta_logs")) {
      
      beta0_mu <- rep(log(70/40), T)
      beta0_sigma <- rep(0.25, T)
      
    } else {
      
      beta0 <- 
        beta_logs %>%
        filter(season == s - 1,
               g == g1m) %>%
        arrange(tid)
      
      beta0_mu <- beta0$mean
      beta0_sigma <- beta0$sd
      
    }
    
    if (!exists("eta_logs")) {
      
      eta_mu <- matrix(0, nrow = T, ncol = S - 1)
      eta_sigma <- matrix(1, nrow = T, ncol = S - 1)
      
    } else {
      
      eta <- 
        eta_logs %>%
        filter(season == s,
               g == g1m) %>%
        arrange(tid)
      
      eta_mu[,1] <- eta$mean
      eta_sigma[,1] <- eta$sd
      
    }
    
    if (!exists("log_sigma_logs")) {
      
      log_sigma_mu <- log(0.025)
      log_sigma_sigma <- 0.5
      
    } else {
      
      log_sigma <- 
        log_sigma_logs %>%
        filter(updated_ts == max(updated_ts))
      
      log_sigma_mu <- log_sigma$mean
      log_sigma_sigma <- log_sigma$sd
      
    }
    
    priors <-
      list(
        beta0_mu = beta0_mu,
        beta0_sigma = beta0_sigma,
        eta_mu = eta_mu,
        eta_sigma = eta_sigma,
        log_sigma_mu = log_sigma_mu,
        log_sigma_sigma = log_sigma_sigma
      )
    
    stan_data <-
      c(
        observations,
        priors
      )
    
    iterative_fit <-
      model$sample(
        data = stan_data,
        seed = 1234,
        init = 0.01,
        step_size = 0.002,
        chains = 4,
        parallel_chains = 4,
        iter_warmup = 1000,
        iter_sampling = 1000
      )
    
    if (!exists("log_sigma_logs")) {
      
      log_sigma_logs <- 
        extract_log_sigma_log(iterative_fit)
      
    } else {
      
      log_sigma_logs <- 
        log_sigma_logs %>%
        bind_rows(extract_log_sigma_log(iterative_fit))
      
    }
    
    if (!exists("beta_logs")) {
      
      beta_logs <- 
        extract_beta_log(iterative_fit)
      
    } else {
      
      beta_logs <- 
        beta_logs %>%
        bind_rows(extract_beta_log(iterative_fit))
      
    }
    
    if (!exists("eta_logs")) {
      
      eta_logs <- 
        extract_eta_log(iterative_fit)
      
    } else {
      
      eta_logs <- 
        eta_logs %>%
        bind_rows(extract_eta_log(iterative_fit))
      
    }
    
  }
  
}

beta_logs %>%
  left_join(sims %>% distinct(season, tid = team_id, mu)) %>%
  ggplot(aes(x = season,
             y = median,
             ymin = q5,
             ymax = q95,
             color = as.factor(tid),
             fill = as.factor(tid))) +
  geom_ribbon(aes(color = NULL),
              alpha = 0.25) + 
  geom_line() + 
  geom_point(aes(y = mu)) + 
  scale_color_brewer(palette = "Dark2") + 
  scale_fill_brewer(palette = "Dark2") + 
  theme_rieke()


