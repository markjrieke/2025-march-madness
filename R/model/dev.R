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
  slice_sample(n = 1000)
  filter(home_name == "Tulsa" | away_name == "Tulsa")

model <- 
  cmdstan_model(
    "stan/dev_17.stan",
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

S <- 
  tulsa %>%
  select(home_score, away_score) %>%
  as.matrix() %>%
  t()

stan_data <-
  list(
    N = nrow(tulsa),
    T = max(teams$tid),
    O = tulsa$n_ot,
    V = 1 - tulsa$neutral,
    tid = tid,
    S = S,
    alpha = log(70/40),
    sigma_o_mu = 0,
    sigma_o_sigma = 0.75,
    sigma_d_mu = 0,
    sigma_d_sigma = 0.75,
    sigma_h_mu = 0,
    sigma_h_sigma = 0.25,
    sigma_i_mu = 0,
    sigma_i_sigma = 0.75,
    gamma_mu = 0,
    gamma_sigma = 0.25,
    delta_mu = log(0.1),
    delta_sigma = 0.25
  )

tulsa_fit <-
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
  tulsa_fit$summary(c("Y"))

preds %>%
  mutate(location = if_else(str_sub(variable, 3, 3) == "1", "home", "away"),
         variable = str_remove_all(variable, "Y|\\[1|\\[2|,|\\]"),
         variable = as.numeric(variable)) %>%
  rename(rowid = variable,
         score = median) %>%
  left_join(tulsa %>% rowid_to_column()) %>%
  mutate(truth = if_else(location == "home", home_score, away_score)) %>%
  select(location,
         truth,
         score,
         q5,
         q95,
         neutral) %>%
  ggplot(aes(x = truth,
             y = score,
             ymin = q5,
             ymax = q95)) + 
  geom_pointrange(alpha = 0.125,
                  color = "royalblue") +
  geom_abline(linetype = "dashed",
              color = "orange",
              linewidth = 1) + 
  facet_wrap(~location) +
  theme_rieke()

beepr::beep(1)

tulsa_fit$summary(c(paste0("eta_", c("o", "d", "g"), "[315]"), paste0("sigma_", c("o", "d", "g"))))
bayesplot::mcmc_pairs(tulsa_fit$draws(c(paste0("eta_", c("o", "d", "g"), "[315]"), paste0("sigma_", c("o", "d", "g")))))

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


