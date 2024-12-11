library(tidyverse)

mu0 <- log(c(65, 70)/40)
sigma <- 0.02

# set.seed(1234)
sims <- 
  crossing(t = 0:20,
           tid = 1:2) %>%
  mutate(mu = map_dbl(tid, ~mu0[.x])) %>%
  bind_cols(eta = rnorm(nrow(.))) %>%
  mutate(delta = if_else(t == 0, 0, eta * sigma)) %>%
  group_by(tid) %>%
  mutate(delta = cumsum(delta),
         mu = mu + delta) %>%
  ungroup() %>%
  mutate(lambda = exp(mu) * 40) %>%
  crossing(g = 1:30) %>%
  bind_cols(points = rpois(nrow(.), .$lambda))
  

sims %>% 
  ggplot(aes(x = t,
             y = points,
             color = as.factor(tid),
             fill = as.factor(tid))) + 
  geom_point(alpha = 0.25) +
  geom_smooth() +
  scale_color_brewer(palette = "Dark2") + 
  scale_fill_brewer(palette = "Dark2") +
  facet_wrap(~tid, ncol = 1) +
  theme_rieke()


model <-
  cmdstan_model(
    "stan/dev_23.stan",
    dir = "exe/"
  )

stan_data <-
  list(
    N = nrow(sims),
    T = max(sims$tid),
    J = max(sims$t) + 1,
    M = 5,
    tid = sims$tid,
    jid = sims$t + 1,
    S = sims$points,
    beta0_mu = log(70/40),
    beta0_sigma = 0.25,
    log_sigma_mu = log(0.025),
    log_sigma_sigma = 0.5
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

iterative_fit$summary("beta_plus") %>%
  mutate(across(c(median, q5, q95), ~.x - log(40)),
         variable = str_remove_all(variable, "beta_plus\\[|\\]"),
         tid = str_sub(variable, 1, str_locate(variable, ",")[,1] - 1),
         tid = as.integer(tid),
         t = str_sub(variable, str_locate(variable, ",")[,1] + 1),
         t = as.integer(t) - 1) %>%
  select(t, tid, median, q5, q95) %>%
  left_join(sims %>% distinct(t, tid, mu)) %>%
  ggplot(aes(x = t,
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

iterative_fit$summary(c("beta0", "sigma"))
