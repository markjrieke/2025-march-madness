library(tidyverse)
library(cmdstanr)
library(riekelib)

n_groups <- 100

data <- 
  tibble(group = 1:n_groups,
         eta = rnorm(n_groups)) %>%
  mutate(mu = 0.25 + eta*0.25,
         theta = expit(mu)) %>%
  bind_cols(K = rpois(nrow(.), .$group * 10)) %>%
  mutate(K = if_else(K == 0, 1, K)) %>%
  bind_cols(Y = rbinom(nrow(.), .$K, .$theta))

model <-
  cmdstan_model(
    "stan/dev_34.stan",
    dir = "exe/"
  )

recovery <-
  cmdstan_model(
    "stan/dev_35.stan",
    dir = "exe/"
  )

stan_data <-
  list(
    N = nrow(data),
    G = max(data$group),
    Y = data$Y,
    K = data$K,
    gid = data$group
  )

fit <-
  model$sample(
    data = stan_data,
    seed = 4321,
    iter_warmup = 1000,
    iter_sampling = 1000,
    chains = 4,
    parallel_chains = 4
  )

group_data <- 
  fit$summary("beta")

idxs <- sample(1:100, 4)
params <- paste0("beta[", idxs, "]")

beta_data <- 
  fit$draws(params, format = "matrix") 

recovery_data <-
  list(
    S = nrow(beta_data),
    G = ncol(beta_data),
    y = beta_data
  )

recovery_fit <-
  recovery$sample(
    recovery_data,
    seed = 1234,
    iter_warmup = 1000,
    iter_sampling = 1000,
    chains = 4,
    parallel_chains = 4
  )

recovery_fit$summary()
fit$summary(params)
