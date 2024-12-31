library(tidyverse)
library(riekelib)
library(cmdstanr)

used_teams <- c("Tulsa", "Stanford", "Missouri", "Loyola Chicago", "Marshall", "Vermont", "Saint Louis", "Yale")
  
tulsa <- 
  arrow::read_parquet("data/games/games.parquet") %>%
  mutate(across(ends_with("_id"), ~if_else(.x == "564", NA, .x)),
         across(ends_with("_id"), ~if_else(is.na(.x), "missing", .x)),
         home_name = if_else(home_id == "missing", "missing", home_name),
         away_name = if_else(away_id == "missing", "missing", away_name)) %>%
  filter(league == "mens") %>%
  # filter(home_name %in% used_teams | away_name %in% used_teams) %>%
  # slice_sample(prop = 0.5) %>%
  # mutate(across(ends_with("name"), ~if_else(.x %in% used_teams, .x, "Other")))
  filter(season == 2018) %>%
  filter(home_name != "missing" & away_name != "missing")

model <- 
  cmdstan_model(
    "stan/dev_57.stan",
    dir = "exe/"
  )

teams <- 
  tulsa %>%
  select(ends_with("name")) %>%
  pivot_longer(everything(),
               names_to = "home_away",
               values_to = "team_name") %>%
  distinct(team_name) %>%
  arrange(team_name) %>%
  rowid_to_column("tid")

tid <- 
  tulsa %>%
  left_join(teams, by = c("home_name" = "team_name")) %>%
  rename(tid1 = tid) %>%
  left_join(teams, by = c("away_name" = "team_name")) %>%
  rename(tid2 = tid) %>%
  select(starts_with("tid")) %>%
  as.matrix() %>%
  t()

Y <- 
  tulsa %>%
  select(home_score, away_score) %>%
  as.matrix() %>%
  t()

T <- max(tid)

log_sigma_o_mu <- -3
log_sigma_o_sigma <- 0.5
log_sigma_d_mu <- -3
log_sigma_d_sigma <- 0.5
log_sigma_h_mu <- -3
log_sigma_h_sigma <- 0.5
log_sigma_a_mu <- -3
log_sigma_a_sigma <- 0.5
log_sigma_i_mu <- -3
log_sigma_i_sigma <- 0.5

# log-mean
alpha <- log(70/40)

stan_data <-
  list(
    N = nrow(tulsa),
    T = T,
    tid = tid,
    Y = Y,
    O = tulsa$n_ot,
    V = 1 - tulsa$neutral,
    alpha = alpha,
    log_sigma_o_mu = log_sigma_o_mu,
    log_sigma_o_sigma = log_sigma_o_sigma,
    log_sigma_d_mu = log_sigma_d_mu,
    log_sigma_d_sigma = log_sigma_d_sigma,
    log_sigma_h_mu = log_sigma_h_mu,
    log_sigma_h_sigma = log_sigma_h_sigma,
    log_sigma_a_mu = log_sigma_a_mu,
    log_sigma_a_sigma = log_sigma_a_sigma,
    log_sigma_i_mu = log_sigma_i_mu,
    log_sigma_i_sigma = log_sigma_i_sigma
  )

fit <-
  model$sample(
    data = stan_data,
    seed = 2025,
    init = 0.01,
    step_size = 0.002,
    chains = 8,
    parallel_chains = 8,
    iter_warmup = 1250,
    iter_sampling = 1250
  )

truth <- 
  tulsa %>%
  rowid_to_column() %>%
  pivot_longer(ends_with("name"),
               names_to = "location",
               values_to = "team_name") %>%
  mutate(location = str_remove(location, "_name")) %>%
  pivot_longer(ends_with("score"),
               names_to = "check",
               values_to = "score") %>%
  mutate(check = str_remove(check, "_score")) %>%
  filter(location == check) %>%
  select(-check)

preds <-
  fit$summary(c("Y_rep"))

preds %>%
  mutate(idx = str_remove_all(variable, "Y_rep|\\[|\\]")) %>%
  separate(idx, c("location", "rowid")) %>%
  mutate(location = if_else(location == "1", "home", "away"),
         rowid = as.integer(rowid)) %>%
  left_join(truth) %>%
  nest(data = -team_name) %>%
  slice_sample(n = 12) %>%
  unnest(data) %>%
  ggplot(aes(x = date,
             y = median,
             ymin = q5,
             ymax = q95)) + 
  geom_pointrange(color = "royalblue") + 
  geom_point(aes(y = score),
             color = "orange") + 
  theme_rieke() +
  facet_wrap(~team_name)
  # filter(season == 2024) 
  # filter(team_name %in% used_teams) %>%
  ggplot(aes(x = score,
             y = median,
             ymin = q5,
             ymax = q95)) + 
  # geom_point(alpha = 0.01) +
  geom_pointrange(alpha = 0.0625) +
  geom_abline(color = "white") + 
  theme_rieke() +
  facet_wrap(~location)

fit$profiles()[[1]] %>%
  as_tibble() %>%
  arrange(desc(total_time))

fit$summary(paste0("log_sigma_", c("o", "d", "h", "i")))
fit$draws(paste0("log_sigma_", c("o", "d", "h", "i"))) %>% bayesplot::mcmc_pairs()
fit$summary(c(paste0("eta_", c("o", "d", "h"), "[315]")))
fit$draws(paste0("eta_", c("o", "d", "h"), "[315]")) %>% bayesplot::mcmc_pairs()
fit$summary(c(paste0("beta_", c("o", "d", "h"), "[315]")))
fit$draws(paste0("beta_", c("o", "d", "h"), "[315]")) %>% bayesplot::mcmc_pairs()
fit$cmdstan_diagnose()
