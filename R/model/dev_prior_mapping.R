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
    "stan/dev_37.stan",
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
idxs <- 1:100
params <- paste0("beta[", idxs, "]")

beta_data <- 
  fit$summary(params) 

recovery_data <-
  list(
    S = 4000,
    G = nrow(beta_data),
    beta_mu = beta_data$mean,
    beta_sigma = beta_data$sd
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

recovery_fit$summary("beta[2]")
fit$summary(params)

recovery_fit$summary("eta") %>%
  mutate(fit = "recovrey") %>%
  bind_rows(fit$summary("eta") %>%
              mutate(fit = "fit")) %>%
  nest(data = -variable) %>%
  slice_sample(n = 20) %>%
  unnest(data) %>%
  ggplot(aes(x = variable,
             y = median,
             ymin = q5,
             ymax = q95,
             color = fit)) + 
  geom_pointrange(alpha = 0.5) +
  coord_flip() +
  theme_rieke()

# blegh ------------------------------------------------------------------------

centered <- 
  cmdstan_model(
    "stan/dev_38.stan",
    dir = "exe/"
  )

centered_draws <- 
  fit$draws(params, format = "df") %>%
  as_tibble() %>%
  select(starts_with("beta")) %>%
  pivot_longer(starts_with("beta"),
               names_to = "parameter",
               values_to = "y") %>%
  nest(data = -parameter) %>%
  rowid_to_column("g") %>%
  unnest(data)

centered_data <-
  list(
    N = nrow(centered_draws),
    K = max(centered_draws$g),
    y = centered_draws$y,
    g = centered_draws$g
  )

centered_fit <-
  centered$sample(
    centered_data,
    seed = 1234,
    iter_warmup = 1000,
    iter_sampling = 1000,
    chains = 4,
    parallel_chains = 4
  )

centered_fit$summary("tau")
