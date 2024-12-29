library(tidyverse)
library(riekelib)
library(cmdstanr)

used_teams <- c("Tulsa", "Stanford", "Missouri", "Loyola Chicago")#, "Marshall", "Vermont", "Saint Louis", "Yale")
  
tulsa <- 
  arrow::read_parquet("data/games/games.parquet") %>%
  mutate(across(ends_with("_id"), ~if_else(.x == "564", NA, .x)),
         across(ends_with("_id"), ~if_else(is.na(.x), "missing", .x)),
         home_name = if_else(home_id == "missing", "missing", home_name),
         away_name = if_else(away_id == "missing", "missing", away_name)) %>%
  filter(league == "mens") %>%
  filter(home_name %in% used_teams | away_name %in% used_teams) %>%
  # slice_sample(prop = 0.5) %>%
  mutate(across(ends_with("name"), ~if_else(.x %in% used_teams, .x, "Other")))
  filter(season == 2018)

model <- 
  cmdstan_model(
    "stan/dev_51.stan",
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

seasons <-
  tulsa %>%
  distinct(season) %>%
  arrange(season) %>%
  rowid_to_column("sid")

sid <- 
  tulsa %>%
  left_join(seasons) %>%
  pull(sid)

Y <- 
  tulsa %>%
  select(home_score, away_score) %>%
  as.matrix() %>%
  t()

T <- max(tid)
S <- max(sid)

logit_rho_mu <- 0
logit_rho_sigma <- 1.5

log_sigma_o_mu <- 0
log_sigma_o_sigma <- 0.5
log_sigma_d_mu <- 0
log_sigma_d_sigma <- 0.5
log_sigma_h_mu <- 0
log_sigma_h_sigma <- 0.5

beta0_h_mu <- 0
beta0_h_sigma <- 0.25

beta0_od_mu <- 0
beta0_od_sigma <- 0.25

# log-mean
alpha <- log(70/40)

stan_data <-
  list(
    N = nrow(tulsa),
    T = T,
    S = S,
    P = P,
    sid = sid,
    tid = tid,
    Y = Y,
    O = tulsa$n_ot,
    V = tulsa$neutral,
    alpha = alpha,
    logit_rho_mu = logit_rho_mu,
    logit_rho_sigma = logit_rho_sigma,
    log_sigma_o_mu = log_sigma_o_mu,
    log_sigma_o_sigma = log_sigma_o_sigma,
    log_sigma_d_mu = log_sigma_d_mu,
    log_sigma_d_sigma = log_sigma_d_sigma,
    log_sigma_h_mu = log_sigma_h_mu,
    log_sigma_h_sigma = log_sigma_h_sigma,
    beta0_h_mu = beta0_h_mu,
    beta0_h_sigma = beta0_h_sigma,
    beta0_od_mu = beta0_od_mu,
    beta0_od_sigma = beta0_od_sigma
  )

fit <-
  model$sample(
    data = stan_data,
    seed = 2025,
    init = 0.01,
    step_size = 0.002,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 1000,
    iter_sampling = 1000,
    refresh = 20
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
  ggplot(aes(x = score,
             y = median,
             ymin = q5,
             ymax = q95)) + 
  geom_pointrange(alpha = 0.125) + 
  geom_abline(color = "white") + 
  theme_rieke() +
  facet_wrap(~team_name)

fit$profiles()[[1]] %>%
  as_tibble() %>%
  arrange(desc(total_time))

fit$draws(c("beta_o[1,12]", "beta_d[1,12]", "beta_h[1,12]")) %>% bayesplot::mcmc_pairs()
fit$summary(paste0("beta_", c("o", "d", "h"), "[1,12]"))
