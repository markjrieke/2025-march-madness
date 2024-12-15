library(tidyverse)
library(cmdstanr)
library(riekelib)

beta0 <- 0
log_sigma_s <- log(0.25)
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
  geom_point(shape = 21) +
  expand_limits(y = c(-1.75, 3.25))

model <-
  cmdstan_model(
    "stan/dev_33.stan",
    dir = "exe/"
  )

partial <-
  data %>%
  filter(t <= 75)

mu <- rep(0, nrow(partial))
Sigma <- diag(1, nrow(partial))

initial_data <-
  list(
    N = nrow(partial),
    Y = partial$Y,
    log_sigma_s = log_sigma_s,
    log_sigma_o = log_sigma_o,
    mu = mu,
    Sigma = Sigma
  )

initial_fit <-
  model$sample(
    data = initial_data,
    seed = 2025,
    iter_warmup = 1250,
    iter_sampling = 1250,
    chains = 8,
    parallel_chains = 8
  )

log_lik <- 
  initial_fit$draws("beta[75]", format = "df") %>%
  as_tibble() %>%
  mutate(x = list(1:10),
         eta = map(x, ~rnorm(length(.x)))) %>%
  unnest(c(x, eta)) %>%
  mutate(beta_pred = `beta[75]` + eta * exp(log_sigma_s)) %>%
  bind_cols(Y = rnorm(nrow(.), 0.5, exp(log_sigma_o))) %>%
  mutate(lp = dnorm(Y, beta_pred, exp(log_sigma_o), log = TRUE)) %>%
  group_by(.draw) %>%
  summarise(lp = sum(lp))

psis_out <-
  loo::psis(log_lik$lp)

psis_out$diagnostics

draw_weights <- 
  loo::weights.importance_sampling(psis_out, log = FALSE)

plot(draw_weights)

posterior::resample_draws(
  initial_fit$draws("beta[75]"),
  weights = draw_weights
) %>%
  as_tibble() %>%
  rename(beta = 1) %>%
  summarise(mean = mean(beta),
            sd = sd(beta))

initial_fit$summary("beta[75]")
