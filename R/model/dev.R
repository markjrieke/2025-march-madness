library(tidyverse)
library(riekelib)
library(cmdstanr)

bracket <-
  cmdstan_model(
    "stan/bracket.stan",
    dir = "exe/"
  )

stan_data <-
  list(
    T = 64,
    wid0 = array(c(1:64, 1, 3, 5, rep(0, 6*64 - 3)), dim = c(64, 7)),
    alpha = log(70/40),
    beta_Mu = beta_Mu[1:64,],
    beta_Sigma = beta_Sigma[1:64,,],
    log_sigma_i_mu = log_sigma_i[2,]$mean,
    log_sigma_i_sigma = log_sigma_i[2,]$sd,
    hurdle_Mu = hurdle_Mu,
    hurdle_Sigma = hurdle_Sigma,
    poisson_Mu = poisson_Mu,
    poisson_Sigma = poisson_Sigma
  )

bracket_fit <-
  bracket$sample(
    data = stan_data,
    seed = 2025,
    iter_warmup = 100,
    iter_sampling = 1250,
    chains = 8,
    parallel_chains = 8,
    fixed_param = TRUE
  )

p_advance <- 
  bracket_fit$summary("p_advance")

# woohoo! works with fixed teams (i.e., teams that have already won)
p_advance %>%
  mutate(variable = str_remove_all(variable, "p_advance\\[|\\]")) %>%
  separate(variable, c("tid", "round")) %>%
  mutate(tid = as.integer(tid),
         round = paste0("round_", round)) %>%
  left_join(teams) %>%
  select(team_name,
         round,
         p_advance = mean) %>%
  pivot_wider(names_from = round,
              values_from = p_advance)





