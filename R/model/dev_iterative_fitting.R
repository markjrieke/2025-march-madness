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
    "stan/dev_29.stan",
    dir = "exe/"
  )

# split into old/new (midseason) data
old_data <- 
  sims %>%
  filter(season < 2020 | game <= 15)

s <- 2001

# iterate over training data
for (s in unique(sims$season)) {
  
  season_data <-
    sims %>%
    filter(season == s) %>%
    mutate(sid = 2)
  
  g_max <- max(season_data$game)
  
  T <- max(season_data$tid)
  S <- max(season_data$sid)
  P <- T + (T * (S-1)) + 1
  
  # make generic? (i.e., if _thing_ doesn't exist -> instantiate priors)
  if (s == 2000) {
    
    log_sigma_mu <- log(0.025)
    log_sigma_sigma <- 0.5
    beta0_mu <- rep(log(70/40), T)
    beta0_sigma <- rep(0.25, T)
    eta_mu <- rep(0, T * (S-1))
    eta_sigma <- rep(1, T * (S-1))
    
    mu <- c(log_sigma_mu, beta0_mu, eta_mu)
    Sigma <- diag(c(log_sigma_sigma, beta0_sigma, eta_sigma)^2)
    
  } else {
    
    post <-
      season_fit$draws(
        c("log_sigma", paste0("beta[", 1:T, ",2]")),
        format = "matrix"
      )
    
    mu <- c(colMeans(post), rep(0, T))
    Sigma <- diag(1, P)
    Sigma[1:(T+1),1:(T+1)] <- cov(post)
    
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
  
  # fit game level data
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
  
  # write logs
  if (s == 2000) {
    
    beta_log <-
      season_fit$summary(paste0("beta[", 1:T, ",2]")) %>%
      mutate(season = s,
             game = g_max)
    
    log_sigma_log <-
      season_fit$summary("log_sigma") %>%
      mutate(season = s,
             game = g_max)
    
  } else {
    
    beta_log <-
      beta_log %>%
      bind_rows(season_fit$summary(paste0("beta[", 1:T, ",2]")) %>%
                  mutate(season = s,
                         game = g_max))
    
    log_sigma_log <-
      log_sigma_log %>%
      bind_rows(season_fit$summary("log_sigma") %>%
                  mutate(season = s,
                         game = g_max))
    
  }
  
}

log_sigma_log_season %>%
  # mutate(x = season + (game - 1)/30) %>%
  filter(game == 30) %>%
  mutate(x = season) %>%
  ggplot(aes(x = x,
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
  mutate(x = season + (game - 1)/30,
         mu = if_else(game != 30, NA, mu)) %>%
  ggplot(aes(x = x,
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



# iterate over each game
for (s in unique(sims$season)) {
  
  season_data <- 
    sims %>%
    filter(season == s) %>%
    mutate(sid = 2)
  
  g_min <- min(season_data$game)
  
  for (g in unique(season_data$game)) {
    
    game_data <-
      season_data %>%
      filter(game == g)
    
    T <- max(game_data$tid)
    S <- max(game_data$sid)
    P <- T + (T * (S-1)) + 1
    
    if (s == 2000 & g == 1) {
      
      beta0_mu <- rep(log(70/40), T)
      beta0_sigma <- rep(0.25, T)
      eta_mu <- rep(0, T * (S-1))
      eta_sigma <- rep(1, T * (S-1))
      log_sigma_mu <- log(0.025)
      log_sigma_sigma <- 0.5
      
      mu <- c(beta0_mu, eta_mu, log_sigma_mu)
      Sigma <- diag(c(beta0_sigma, eta_sigma, log_sigma_sigma)^2)
      
    } else {
      
      if (g != g_min) {
        
        priors <- 
          game_fit$draws(c("beta0", "eta", "log_sigma"), format = "matrix")
        
      } else {
        
        post <- 
          game_fit$draws(c(paste0("beta[", 1:T, ",2]"), "log_sigma"), format = "matrix")
        
        priors <-
          matrix(
            c(post[,1:2],
              rnorm(1e4),
              rnorm(1e4),
              post[,3]),
            nrow = 1e4
          )
        
      }
      
      mu <- colMeans(priors)
      Sigma <- cov(priors)
      
    }
    
    # pass data to stan
    stan_data <-
      list(
        N = nrow(game_data),
        T = T,
        S = S,
        tid = game_data$tid,
        sid = game_data$sid,
        Y = game_data$Y,
        P = P,
        mu = mu, 
        Sigma = Sigma
      )
    
    # fit game level data
    game_fit <-
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
    
    # post-processing
    if (s == 2000 & g == 1) {
      
      beta_log <- 
        game_fit$summary(paste0("beta[", 1:T, ",2]")) %>%
        mutate(season = s,
               game = g)
      
      log_sigma_log <-
        game_fit$summary("log_sigma") %>%
        mutate(season = s,
               game = g)
      
    } else {
      
      beta_log <- 
        beta_log %>%
        bind_rows(game_fit$summary(paste0("beta[", 1:T, ",2]")) %>%
                    mutate(season = s,
                           game = g))
      
      log_sigma_log <-
        log_sigma_log %>%
        bind_rows(game_fit$summary("log_sigma") %>%
                    mutate(season = s,
                           game = g))
      
    }
    
  }
  
}

season_fit$summary("beta") %>%
  mutate(index = str_remove_all(variable, "beta\\[|\\]")) %>%
  separate(index, c("tid", "sid"), ",") %>%
  mutate(across(c(tid, sid), as.integer)) %>%
  left_join(season_data %>% filter(game == 30) %>% distinct(tid, sid, season, game)) %>%
  drop_na() %>%
  select(-sid) %>%
  mutate(fit = "full") %>%
  bind_rows(beta_log_game %>%
              filter(game == 30) %>%
              mutate(index = str_remove_all(variable, "beta\\[|\\]")) %>%
              separate(index, c("tid", "sid"), ",") %>%
              mutate(across(c(tid, sid), as.integer)) %>%
              select(-sid) %>%
              mutate(fit = "game")) %>%
  bind_rows(beta_log_season %>%
              mutate(index = str_remove_all(variable, "beta\\[|\\]")) %>%
              separate(index, c("tid", "sid"), ",") %>%
              mutate(across(c(tid, sid), as.integer)) %>%
              select(-sid) %>%
              mutate(fit = "season")) %>%
  ggplot(aes(x = season,
             y = median,
             color = fit,
             # linetype = fit,
             ymin = q5,
             ymax = q95)) + 
  geom_ribbon(fill = NA) + 
  geom_line() + 
  facet_wrap(~tid, ncol = 1) +
  theme_rieke()

beta_log_game %>%
  mutate(index = str_remove_all(variable, "beta\\[|\\]")) %>%
  separate(index, c("tid", "sid"), ",") %>%
  mutate(across(c(tid, sid), as.integer)) %>%
  select(-sid) %>%
  mutate(fit = "game") %>%
  bind_rows(beta_log_season %>%
              mutate(index = str_remove_all(variable, "beta\\[|\\]")) %>%
              separate(index, c("tid", "sid"), ",") %>%
              mutate(across(c(tid, sid), as.integer)) %>%
              select(-sid) %>%
              mutate(fit = "season")) %>%
  bind_rows(season_fit$summary("beta") %>%
              mutate(index = str_remove_all(variable, "beta\\[|\\]")) %>%
              separate(index, c("tid", "sid"), ",") %>%
              mutate(across(c(tid, sid), as.integer)) %>%
              left_join(season_data %>% filter(game == 30) %>% distinct(tid, sid, season, game)) %>%
              drop_na() %>%
              select(-sid) %>%
              mutate(fit = "full")) %>%
  mutate(x = season + (game - 1)/30) %>%
  ggplot(aes(x = x,
             y = median,
             color = as.factor(tid),
             linetype = fit,
             ymin = q5,
             ymax = q95)) + 
  geom_ribbon(fill = NA,
              linewidth = 0.25) +
  geom_line() + 
  facet_wrap(~tid, ncol = 1) + 
  theme_rieke()

log_sigma_log_game %>%
  filter(game == 30) %>%
  mutate(fit = "game") %>%
  bind_rows(log_sigma_log_season %>%
              mutate(fit = "season")) %>%
  mutate(x = season + (game - 1)/30) %>%
  ggplot(aes(x = season,
             y = exp(median),
             ymin = exp(q5),
             ymax = exp(q95),
             linetype = fit)) + 
  geom_ribbon(fill = NA,
              color = "black") + 
  geom_line() +
  geom_pointrange(data = log_sigma_log_full,
                  mapping = aes(linetype = NULL),
                  x = 2020) + 
  theme_rieke()

# blegh ------------------------------------------------------------------------

model <-
  cmdstan_model(
    "stan/dev_30.stan",
    dir = "exe/"
  )





