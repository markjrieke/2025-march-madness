library(tidyverse)

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
    "stan/dev_25.stan",
    dir = "exe/"
  )

rm(log_sigma_logs)
rm(beta_logs)

extract_beta_log <- function(fit) {
  
  fit$summary("beta") %>%
    mutate(index = str_sub(variable, str_locate(variable, "beta")[,2] + 1),
           index = str_remove_all(index, "\\[|\\]"),
           variable = "beta") %>%
    separate(index, c("tid", "sid"), ",") %>%
    mutate(across(c(tid, sid), as.integer)) %>%
    inner_join(season_data %>% distinct(season, tid, sid)) %>%
    select(-sid)
  
}

for (s in unique(sims$season)) {
  
  season_data <- 
    sims %>%
    filter(season == s) %>%
    mutate(sid = rank(season, ties.method = "min") + 1,
           tid = team_id,
           Y = points)
  
  observations <- 
    list(
      N = nrow(season_data),
      T = max(season_data$tid),
      S = max(season_data$sid),
      tid = season_data$tid,
      sid = season_data$sid,
      Y = season_data$Y
    )
  
  if (s == 2000) {
    
    priors <- 
      list(
        beta0_mu = rep(log(70/40), observations$T),
        beta0_sigma = rep(0.25, observations$T),
        log_sigma_mu = log(0.025),
        log_sigma_sigma = 0.5
      )
    
  } else {
    
    prev_beta <-
      beta_logs %>%
      filter(season == s - 1) %>%
      arrange(tid)
    
    prev_log_sigma <-
      log_sigma_logs %>%
      filter(season == s - 1)
    
    priors <- 
      list(
        beta0_mu = prev_beta$mean,
        beta0_sigma = prev_beta$sd,
        log_sigma_mu = prev_log_sigma$mean,
        log_sigma_sigma = prev_log_sigma$sd
      )
    
  }
  
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
  
  if (s == 2000) {
    
    log_sigma_logs <- 
      iterative_fit$summary("log_sigma") %>%
      mutate(season = s)
    
    beta_logs <- 
      extract_beta_log(iterative_fit)
    
  } else {
    
    log_sigma_logs <- 
      log_sigma_logs %>%
      bind_rows(iterative_fit$summary("log_sigma") %>%
                  mutate(season = s))
    
    beta_logs <-
      beta_logs %>%
      bind_rows(extract_beta_log(iterative_fit))
    
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


