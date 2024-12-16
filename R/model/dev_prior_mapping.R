library(tidyverse)
library(cmdstanr)
library(riekelib)

n_groups <- 100

data <- 
  tibble(group = 1:n_groups,
         eta = rnorm(n_groups)) %>%
  mutate(mu = 0.25 + eta*0.25,
         theta = expit(mu)) %>%
  bind_cols(K = rpois(nrow(.), 75)) %>%
  bind_cols(Y = rbinom(nrow(.), .$K, .$theta))

model <-
  cmdstan_model(
    "stan/dev_34.stan",
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

fit$draws("beta", format = "df") %>%
  as_tibble() %>%
  select(starts_with("beta"), .draw) %>%
  pivot_longer(starts_with("beta"),
               names_to = "group",
               values_to = "beta") %>%
  mutate(group = parse_number(group)) %>%
  group_by(.draw) %>%
  mutate(sd = sd(beta)) %>%
  ungroup() %>%
  mutate(eta = beta/sd) %>%
  group_by(group) %>%
  summarise(eta_mu = mean(eta),
            eta_sd = sd(eta))
  
  
  distinct(.draw, .keep_all = TRUE) %>%
  mutate(log_sigma = log(sd)) %>%
  summarise(log_sigma_mu = mean(log_sigma),
            log_sigma_sigma = sd(log_sigma))

fit$draws("log_sigma", format = "df") %>%
  as_tibble() %>%
  mutate(sigma = exp(log_sigma)) %>% 
  summarise(log_sigma_mu = mean(log_sigma),
            log_sigma_sigma = sd(log_sigma))


