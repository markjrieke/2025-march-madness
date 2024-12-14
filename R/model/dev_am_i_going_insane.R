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

mu <- rep(0, nrow(data))
Sigma <- diag(1, nrow(data))

for (i in 97:100) {
  
  partial <-
    data %>%
    filter(t <= i)
  
  mu <- rep(0, nrow(partial))
  Sigma <- diag(1, nrow(partial))
  
  stan_data <-
    list(
      N = nrow(partial),
      Y = partial$Y,
      log_sigma_s = log_sigma_s,
      log_sigma_o = log_sigma_o,
      mu = mu,
      Sigma = Sigma
    )
  
  fit <-
    model$sample(
      data = stan_data,
      seed = 1234,
      iter_warmup = 1250,
      iter_sampling = 1250,
      chains = 8,
      parallel_chains = 8
    )
  
  if (i == 1) {
    
    results <- 
      fit$summary("beta") %>%
      mutate(t = i)
    
  } else {
    
    results <- 
      results %>%
      bind_rows(fit$summary(paste0("beta[", i, "]")) %>%
                  mutate(t = i))
    
  }
  
}

results %>%
  mutate(fit = "full_iterative") %>%
  left_join(data) %>%
  bind_rows(full_fit$summary("beta") %>%
              rowid_to_column("t") %>%
              mutate(fit = "full")) %>%
  # bind_rows(partial_summary %>%
  #             mutate(fit = "partial")) %>%
  ggplot(aes(x = t,
             y = median,
             ymin = q5,
             ymax = q95)) + 
  geom_ribbon(aes(linetype = fit),
              color = "black",
              fill = NA,
              alpha = 0.25) +
  # geom_line(aes(linetype = fit)) +
  # geom_point(aes(y = beta)) +
  # geom_point(aes(y = Y),
  #            shape = 21) +
  expand_limits(y = c(-1.75, 3.25)) +
  theme_rieke()


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

model <-
  cmdstan_model(
    "stan/dev_32.stan",
    dir = "exe"
  )

mu <- c(0, 0)
Sigma <- diag(1, 2)

for (i in 1:100) {
  
  partial <-
    data %>%
    filter(t == i)
  
  if (i > 1) {
    
    priors <- partial_summary %>% filter(t == i - 1)
    mu <- c(priors$mean, 0)
    Sigma <- diag(c(priors$sd, 1))
    
  }
  
  stan_data <-
    list(
      N = 1,
      Y = partial$Y,
      t = i,
      log_sigma_s = log_sigma_s,
      log_sigma_o = log_sigma_o,
      mu = mu,
      Sigma = Sigma
    )
  
  partial_fit <-
    model$sample(
      data = stan_data,
      seed = 1234,
      iter_warmup = 1250,
      iter_sampling = 1250,
      chains = 8,
      parallel_chains = 8
    )
  
  if (i == 1) {
    
    partial_summary <- 
      partial_fit$summary("beta") %>%
      mutate(t = i)

  } else {
    
    partial_summary <-
      partial_summary %>%
      bind_rows(partial_fit$summary("beta") %>%
                  mutate(t = i))
    
  }
  
}

partial_summary %>%
  left_join(data) %>%
  ggplot(aes(x = t,
             y = median,
             ymin = q5,
             ymax = q95)) + 
  geom_ribbon(alpha = 0.25) +
  geom_line() +
  geom_point(aes(y = beta)) +
  geom_point(aes(y = Y),
             shape = 21) +
  expand_limits(y = c(-1.75, 3.25))

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
             shape = 21) +
  expand_limits(y = c(-1.75, 3.25))

