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
  nest(data = -season) %>%
  mutate(sid = rank(season, ties.method = "first")) %>%
  unnest(data) %>%
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

sims1 <-
  sims %>%
  slice_head(n = nrow(sims)/2)

sims2 <-
  sims %>%
  anti_join(sims1)

model <-
  cmdstan_model(
    "stan/dev_28.stan",
    dir = "exe/"
  )

# priors
T <- max(sims$tid)
S <- max(sims$sid)
P <- T + (T * (S-1)) + 1

beta0_mu <- rep(log(70/40), T)
beta0_sigma <- rep(0.25, T)
eta_mu <- rep(0, T * (S-1))
eta_sigma <- rep(1, T * (S-1))
log_sigma_mu <- log(0.025)
log_sigma_sigma <- 0.5

mu <- c(beta0_mu, eta_mu, log_sigma_mu)
Sigma <- diag(c(beta0_sigma, eta_sigma, log_sigma_sigma)^2)

# fit full model for reference
stan_data <-
  list(
    N = nrow(sims),
    T = T,
    S = S,
    tid = sims$tid,
    sid = sims$sid,
    Y = sims$Y,
    P = P,
    mu = mu,
    Sigma = Sigma
  )

full_fit <-
  model$sample(
    data = stan_data,
    seed = 2025,
    iter_warmup = 1250,
    iter_sampling = 1250,
    chains = 8,
    parallel_chains = 8
  )

# fit to the first half of the dataset
stan_data <-
  list(
    N = nrow(sims1),
    T = T,
    S = S,
    tid = sims1$tid,
    sid = sims1$sid,
    Y = sims1$Y,
    P = P,
    mu = mu,
    Sigma = Sigma
  )

first_fit <-
  model$sample(
    data = stan_data,
    seed = 2025,
    iter_warmup = 1250,
    iter_sampling = 1250,
    chains = 8,
    parallel_chains = 8
  )

# update priors
params <- first_fit$draws("params", format = "matrix") 
mu <- colMeans(params)
Sigma <- cov(params)

# second fit
stan_data <-
  list(
    N = nrow(sims2),
    T = T,
    S = S,
    tid = sims2$tid,
    sid = sims2$sid,
    Y = sims2$Y,
    P = P,
    mu = mu,
    Sigma = Sigma
  )

second_fit <-
  model$sample(
    data = stan_data,
    seed = 2025,
    iter_warmup = 1250,
    iter_sampling = 1250,
    chains = 8,
    parallel_chains = 8
  )

# util plotting function
plot_beta <- function(fit) {
  
  fit$summary("beta") %>%
    mutate(index = str_remove_all(variable, "beta\\[|\\]")) %>%
    separate(index, c("tid", "sid"), ",") %>%
    mutate(across(c(tid, sid), as.integer)) %>%
    ggplot(aes(x = sid,
               y = median,
               ymin = q5,
               ymax = q95,
               color = as.factor(tid),
               fill = as.factor(tid))) + 
    geom_ribbon(aes(color = NULL),
                alpha = 0.25) +
    geom_line() + 
    scale_color_brewer(palette = "Dark2") + 
    scale_fill_brewer(palette = "Dark2") +
    facet_wrap(~tid) + 
    theme_rieke()
  
}

plot_beta(first_fit)
plot_beta(second_fit)
plot_beta(full_fit)

