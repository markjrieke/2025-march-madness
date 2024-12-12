library(tidyverse)
library(riekelib)
library(cmdstanr)

alpha <- 3
beta <- 2
log_sigma <- log(1)

set.seed(1234)
data <- 
  tibble(X = rnorm(100)) %>%
  mutate(mu = alpha + X * beta) %>%
  bind_cols(Y = rnorm(nrow(.), .$mu, exp(log_sigma)))

data1 <- 
  data %>%
  slice_head(n = 50)

data2 <- 
  data %>%
  anti_join(data1)

model <-
  cmdstan_model(
    "stan/dev_27.stan",
    dir = "exe/"
  )

# full fit
alpha_mu <- 0
alpha_sigma <- 5
beta_mu <- 0
beta_sigma <- 5
log_sigma_mu <- 0
log_sigma_sigma <- 5

mu <- c(alpha_mu, beta_mu, log_sigma_mu)
Sigma <- 
  matrix(
    c(c(alpha_sigma^2, 0, 0),
      c(0, beta_sigma^2, 0),
      c(0, 0, log_sigma_sigma^2)),
    nrow = 3,
    ncol = 3
  )

stan_data <-
  list(
    N = nrow(data),
    X = data$X,
    Y = data$Y,
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

# iterative fit
stan_data <-
  list(
    N = nrow(data1),
    X = data1$X,
    Y = data1$Y,
    mu = mu,
    Sigma = Sigma
  )

data1_fit <- 
  model$sample(
    data = stan_data,
    seed = 1234,
    iter_warmup = 1250,
    iter_sampling = 1250,
    chains = 8,
    parallel_chains = 8
  )

# update priors
samples <- data1_fit$draws(c("alpha", "beta", "log_sigma"), format = "matrix")

mu <- colMeans(samples)
Sigma <- cov(samples)

stan_data <-
  list(
    N = nrow(data2),
    X = data2$X,
    Y = data2$Y,
    mu = mu,
    Sigma = Sigma
  )

# fit la segunda vez
data2_fit <-
  model$sample(
    data = stan_data,
    seed = 1234,
    iter_warmup = 1250,
    iter_sampling = 1250,
    chains = 8,
    parallel_chains = 8
  )

