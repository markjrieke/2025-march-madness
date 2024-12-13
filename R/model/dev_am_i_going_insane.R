library(tidyverse)
library(cmdstanr)
library(riekelib)

beta0 <- 0
log_sigma_s <- log(1)
log_sigma_o <- log(0.5)

set.seed(123)
data <-
  tibble(t = 1:100) %>%
  bind_cols(eta = rnorm(nrow(.))) %>%
  mutate(beta = cumsum(eta * exp(log_sigma_s)) + beta0) %>%
  bind_cols(Y = rnorm(nrow(.), .$beta, exp(log_sigma_o)))

data %>%
  ggplot(aes(x = t,
             y = Y)) + 
  geom_point()

model <-
  cmdstan_model(
    "stan/dev_31.stan",
    dir = "exe/"
  )

mu <- rep(0, nrow(data))
Sigma <- diag(1, nrow(data))

stan_data <-
  list(
    N = nrow(data),
    Y = data$Y,
    log_sigma_s = log_sigma_s,
    log_sigma_o = log_sigma_o,
    mu = mu,
    Sigma = Sigma
  )

full_fit <-
  model$sample(
    data = stan_data,
    seed = 1234,
    iter_warmup = 1250,
    iter_sampling = 1250,
    chains = 8,
    parallel_chains = 8
  )

full_fit$summary("beta") %>%
  bind_cols(data) %>%
  ggplot(aes(x = t,
             y = median,
             ymin = q5,
             ymax = q95)) + 
  geom_ribbon(alpha = 0.25) +
  geom_line() +
  geom_point(aes(y = beta)) +
  geom_point(aes(y = Y),
             shape = 21)

