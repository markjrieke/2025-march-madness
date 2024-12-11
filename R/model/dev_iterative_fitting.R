library(tidyverse)

mu0 <- log(c(65, 70)/40)
sigma <- 0.02

set.seed(1234)
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
    "stan/dev_20.stan",
    dir = "exe/"
  )

single_team <- 
  sims %>%
  filter(tid == 2)

stan_data <-
  list(
    N = nrow(single_team),
    T = max(single_team$t) + 1,
    tid = single_team$t + 1,
    S = single_team$points,
    beta0_mu = log(70/40),
    beta0_sigma = 0.25,
    sigma_mu = 0,
    sigma_sigma = 0.25
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

iterative_fit$summary("beta") %>%
  mutate(t = parse_number(variable) - 1,
         across(c(median, q5, q95), ~.x - log(40))) %>%
  select(t, median, q5, q95) %>%
  left_join(single_team %>% distinct(t, mu)) %>%
  ggplot(aes(x = t,
             y = median,
             ymin = q5,
             ymax = q95)) + 
  geom_ribbon(alpha = 0.25) +
  geom_line() +
  geom_point(aes(y = mu))
