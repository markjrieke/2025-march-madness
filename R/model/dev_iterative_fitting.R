library(tidyverse)
library(cmdstanr)
library(riekelib)

mu0 <- log(c(65, 70)/40)
sigma <- 0.05

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
  bind_cols(points = rpois(nrow(.), .$lambda)) %>%
  mutate(tid = team_id,
         Y = points)
  
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
    "stan/dev_28.stan",
    dir = "exe/"
  )

s <- 2000

for (s in unique(sims$season)) {
  
  season_data <- 
    sims %>%
    filter(season == s) %>%
    mutate(sid = 2)
  
  T <- max(season_data$tid)
  S <- max(season_data$sid)
  P <- T + (T * (S-1)) + 1
  
  # set priors
  if (s == 2000) {
    
    beta0_mu <- rep(log(70/40), T)
    beta0_sigma <- rep(0.25, T)
    eta_mu <- rep(0, T * (S-1))
    eta_sigma <- rep(1, T * (S-1))
    log_sigma_mu <- log(0.025)
    log_sigma_sigma <- 0.5
    
    mu <- c(beta0_mu, eta_mu, log_sigma_mu)
    Sigma <- diag(c(beta0_sigma, eta_sigma, log_sigma_sigma)^2)
    
  } else {
    
    post <- 
      season_fit$draws(c(paste0("beta[", 1:T, ",2]"), "log_sigma"), format = "matrix")
    
    priors <- 
      matrix(
        c(post[,1:2],
          rnorm(1e4),
          rnorm(1e4),
          post[,3]),
        nrow = 1e4
      )
    
    mu <- colMeans(priors)
    Sigma <- cov(priors)
    
  }
  
  # pass data to stan
  stan_data <-
    list(
      N = nrow(season_data),
      T = T,
      S = S,
      tid = season_data$tid,
      sid = season_data$sid,
      Y = season_data$Y,
      P = P,
      mu = mu, 
      Sigma = Sigma
    )
  
  # fit season level data
  season_fit <-
    model$sample(
      data = stan_data,
      seed = 2025,
      iter_warmup = 1250,
      iter_sampling = 1250,
      chains = 8,
      parallel_chains = 8,
      init = 0.01,
      step_size = 0.002
    )
  
  if (s == 2000) {
    
    beta_log <- 
      season_fit$summary(paste0("beta[", 1:T, ",2]")) %>%
      mutate(season = s)
    
    log_sigma_log <-
      season_fit$summary("log_sigma") %>%
      mutate(season = s)
    
  } else {
    
    beta_log <- 
      beta_log %>%
      bind_rows(season_fit$summary(paste0("beta[", 1:T, ",2]")) %>%
                  mutate(season = s))
    
    log_sigma_log <- 
      log_sigma_log %>%
      bind_rows(season_fit$summary("log_sigma") %>%
                  mutate(season = s)) 
    
  }
  
}

log_sigma_log %>%
  ggplot(aes(x = season,
             y = median,
             ymin = q5,
             ymax = q95)) + 
  geom_ribbon(alpha = 0.25) + 
  geom_line() +
  geom_hline(yintercept = log(sigma),
             linetype = "dotted") +
  theme_rieke()

beta_log %>%
  mutate(tid = str_remove_all(variable, "beta\\[|,2\\]"),
         tid = as.integer(tid)) %>%
  left_join(sims %>% distinct(season, tid, mu)) %>%
  ggplot(aes(x = season,
             y = median,
             ymin = q5,
             ymax = q95,
             color = as.factor(tid),
             fill = as.factor(tid))) + 
  geom_ribbon(aes(color = NULL),
              alpha = 0.25) +
  geom_line() + 
  geom_point(aes(y = mu,
                 fill = NULL),
             shape = 21,
             size = 3) +
  scale_color_brewer(palette = "Dark2") + 
  scale_fill_brewer(palette = "Dark2") + 
  facet_wrap(~tid, ncol = 1) + 
  theme_rieke()
