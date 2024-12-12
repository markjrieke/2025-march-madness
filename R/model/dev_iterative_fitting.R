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


model <-
  cmdstan_model(
    "stan/dev_28.stan",
    dir = "exe/"
  )

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

matrix_fit <-
  model$sample(
    data = stan_data,
    seed = 2025,
    iter_warmup = 1000,
    iter_sampling = 1000,
    chains = 4,
    parallel_chains = 4
  )

matrix_fit$draws("params", format = "matrix") %>%
  cor() %>%
  heatmap(Rowv = NA, Colv = NA)

matrix_fit$draws(c("beta0", "eta", "log_sigma"), format = "matrix") %>%
  cor() %>%
  heatmap(Rowv = NA, Colv = NA)

tmp <- 
  matrix_fit$summary(c("beta0", "eta", "log_sigma"))


