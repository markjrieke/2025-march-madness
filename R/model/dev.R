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

tid <- array(tid, dim = c(nrow(tid), ncol(tid)))

stan_data <-
  list(
    N = nrow(tulsa),
    T = max(teams$tid),
    tid = tid,
    H = tulsa$home_score,
    A = tulsa$away_score,
    O = tulsa$n_ot,
    alpha = log(70/40),
    alpha_mu = log(70/40),
    alpha_sigma = 0.75,
    sigma_t_mu = 0,
    sigma_t_sigma = 0.75,
    sigma_o_mu = 0,
    sigma_o_sigma = 0.75,
    sigma_d_mu = 0,
    sigma_d_sigma = 0.75
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

preds <- 
  tulsa_fit$summary(c("Yh", "Ya"))

preds %>%
  mutate(rowid = parse_number(variable),
         location = if_else(str_detect(variable, "h"), "home", "away")) %>%
  select(rowid,
         location,
         median,
         q5,
         q95) %>%
  rename(score = median) %>%
  pivot_wider(names_from = location,
              values_from = c(score, q5, q95)) %>%
  bind_cols(tulsa) %>% #filter(away_name == "Tulsa", home_name == "Oklahoma State")
  select(rowid,
         home_score,
         away_score,
         score_home,
         score_away,
         q5_home,
         q5_away,
         q95_home,
         q95_away) %>%
  ggplot(aes(x = home_score,
             y = score_home,
             ymin = q5_home,
             ymax = q95_home)) + 
  geom_pointrange(alpha = 0.25,
                  color = "royalblue") +
  geom_abline(linetype = "dashed", 
              color = "orange",
              linewidth = 1) +
  theme_rieke()

od <- 
  tulsa_fit$summary(c("eta_o", "eta_d"))

od %>%
  mutate(tid = parse_number(variable),
         variable = if_else(str_detect(variable, "o"), "offense", "defense")) %>%
  select(tid, variable, median, q5, q95) %>%
  pivot_wider(names_from = variable,
              values_from = c(median, q5, q95)) %>%
  ggplot(aes(x = median_offense,
             y = median_defense)) +
  geom_point(size = 2.5,
             alpha = 0.25) +
  geom_segment(aes(x = q5_offense,
                   xend = q95_offense),
               alpha = 0.25) +
  geom_segment(aes(y = q5_defense,
                   yend = q95_defense),
               alpha = 0.25) + 
  theme_rieke()

tuod <- 
  tulsa_fit$draws(c("eta_o[315]", "eta_d[315]"), format = "df")

tuod %>%
  as_tibble() %>%
  rename(offense = 1,
         defense = 2) %>%
  slice_sample(n = 2000) %>%
  ggplot(aes(x = offense,
             y = defense)) + 
  geom_point()

1.8/0.8; 0.5/0.4; 0.095/0.095

zo <- 1.8*0.095
zd <- 0.8*0.095
to <- 0.5*0.095
td <- 0.4*0.095
a <- log(70/40)

tibble(z = rpois(1e4, exp(a + zo - td) * 40),
       t = rpois(1e4, exp(a + to - zd) * 40)) %>%
  summarise(pz = sum(z>t)/n())

sigmas <-
  tulsa_fit$draws(c("sigma_o", "sigma_d"), format = "df")

sigmas %>%
  as_tibble() %>%
  ggplot(aes(x = sigma_o,
             y = sigma_d)) + 
  geom_point()

# sbc --------------------------------------------------------------------------

alpha_mu <- log(70/40)
alpha_sigma <- 0.3
sigma_o_mu <- 0
sigma_o_sigma <- 0.3
sigma_d_mu <- 0
sigma_d_sigma <- 0.3

n_sims <- 5000

tibble(alpha = rnorm(n_sims, alpha_mu, alpha_sigma),
       eta_oh = rnorm(n_sims),
       eta_dh = rnorm(n_sims),
       eta_oa = rnorm(n_sims),
       eta_da = rnorm(n_sims),
       sigma_o = abs(rnorm(n_sims, sigma_o_mu, sigma_o_sigma)),
       sigma_d = abs(rnorm(n_sims, sigma_d_mu, sigma_d_sigma))) %>%
  mutate(beta_h = alpha + eta_oh * sigma_o - eta_da * sigma_d,
         beta_a = alpha + eta_oa * sigma_o - eta_dh * sigma_d,
         lambda_h = exp(beta_h) * 40,
         lambda_a = exp(beta_a) * 40) %>%
  bind_cols(Yh = rpois(nrow(.), .$lambda_h),
            Ya = rpois(nrow(.), .$lambda_a)) %>%
  ggplot(aes(x = Yh,
             y = Ya)) + 
  geom_density2d()


