library(tidyverse)
library(riekelib)
library(cmdstanr)

tulsa <- 
  arrow::read_parquet("data/games/games.parquet") %>%
  mutate(across(ends_with("_id"), ~if_else(.x == "564", NA, .x)),
         across(ends_with("_id"), ~if_else(is.na(.x), "missing", .x)),
         home_name = if_else(home_id == "missing", "missing", home_name),
         away_name = if_else(away_id == "missing", "missing", away_name)) %>%
  filter(league == "mens",
         season == 2018) #%>%
  filter(home_name == "Tulsa" | away_name == "Tulsa")

gamma <- log(40)
alpha_h <- log(30)
alpha_a <- log(30)
n_games <- 500

sims <- 
  tibble(Y0 = rpois(n_games, exp(gamma)),
         Y1 = rpois(n_games, exp(alpha_h)),
         Y2 = rpois(n_games, exp(alpha_a))) %>%
  mutate(H = Y0 + Y1,
         A = Y0 + Y2) %>%
  select(H, A)

model <- 
  cmdstan_model(
    "stan/dev_06.stan",
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
  as.matrix()

stan_data <-
  list(
    N = nrow(tulsa),
    T = max(teams$tid),
    tid = tid,
    H = tulsa$home_score,
    A = tulsa$away_score,
    O = tulsa$n_ot,
    alpha_mu = log(70/40),
    alpha_sigma = 0.5,
    sigma_t_mu = 0,
    sigma_t_sigma = 0.5
    # N = nrow(sims),
    # H = sims$H,
    # A = sims$A
  )

tulsa_fit <-
  model$sample(
    data = stan_data,
    seed = 2025,
    init = 0.01,
    step_size = 0.002,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 2000,
    iter_sampling = 2000
  )

tulsa_fit$draws(c("Yh", "Ya", "Ot"), format = "df") %>%
  as_tibble() %>%
  count(Yh, Ya, Ot) %>%
  ggplot(aes(x = Yh,
             y = Ya,
             alpha = n)) + 
  geom_point(data = tulsa %>%
               count(home_score, 
                     away_score,
                     Ot = n_ot),
             mapping = aes(x = home_score,
                           y = away_score,
                           alpha = n),
             color = "royalblue") +
  geom_point(color = "red") + 
  # geom_density2d(color = "red") +
  # facet_wrap(~Ot) + 
  theme_rieke()

