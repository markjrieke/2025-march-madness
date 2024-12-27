library(tidyverse)
library(riekelib)
library(cmdstanr)

used_teams <- c("Tulsa", "Stanford", "Missouri", "Loyola Chicago", "Marshall", "Vermont", "Saint Louis")
  
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

model <- 
  cmdstan_model(
    "stan/dev_42.stan",
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
P <- 2 * (T + (T * (S - 1))) + 1

# log_sigma
prior_mu <- rep(-3, 1)
prior_Sigma <- rep(0.5, 1)

# beta_o0
for (t in 1:T) {
  prior_mu <- c(prior_mu, 0)
  prior_Sigma <- c(prior_Sigma, 0.25)
}

# beta_d0
for (t in 1:T) {
  prior_mu <- c(prior_mu, 0)
  prior_Sigma <- c(prior_Sigma, 0.25)
}

# eta_o
for (t in 1:T) {
  for (s in 1:(S-1)) {
    prior_mu <- c(prior_mu, 0)
    prior_Sigma <- c(prior_Sigma, 1)
  }
}

# eta_d
for (t in 1:T) {
  for (s in 1:(S-1)) {
    prior_mu <- c(prior_mu, 0)
    prior_Sigma <- c(prior_Sigma, 1)
  }
}

length(prior_mu) == P
length(prior_Sigma) == P

# convert to matrix
prior_Sigma <- diag(prior_Sigma, nrow = P, ncol = P)

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
    alpha = alpha,
    prior_mu = prior_mu,
    prior_Sigma = prior_Sigma
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
    iter_sampling = 1000
  )

preds <-
  fit$summary(c("beta_o", "beta_d"))

preds %>%
  mutate(idx = str_remove_all(variable, "beta_|o|d|\\[|\\]")) %>%
  separate(idx, c("tid", "sid"), ",") %>%
  mutate(across(ends_with("id"), as.integer)) %>%
  left_join(teams) %>%
  left_join(seasons) %>%
  mutate(variable = if_else(str_detect(variable, "o"), "offense", "defense")) %>%
  ggplot(aes(x = season,
             y = median,
             ymin = q5,
             ymax = q95)) + 
  geom_ribbon(aes(fill = team_name),
              alpha = 0.25) +
  geom_line(aes(color = team_name)) + 
  scale_color_brewer(palette = "Dark2") + 
  scale_fill_brewer(palette = "Dark2") +
  facet_grid(vars(team_name), vars(variable)) +
  theme_rieke()

fit$summary("log_sigma")

beepr::beep(5)

preds %>%
  mutate(idx = str_remove_all(variable, "beta_|o|d|\\[|\\]")) %>%
  separate(idx, c("tid", "sid"), ",") %>%
  mutate(across(ends_with("id"), as.integer)) %>%
  left_join(teams) %>%
  left_join(seasons) %>%
  mutate(variable = if_else(str_detect(variable, "o"), "offense", "defense")) %>%
  filter(team_name %in% sample(used_teams, 2)) %>%
  filter(season == sample(seasons$season, 1))

