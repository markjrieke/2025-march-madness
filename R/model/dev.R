library(tidyverse)
library(riekelib)
library(cmdstanr)

prediction <-
  cmdstan_model(
    "stan/prediction.stan",
    dir = "exe/"
  )

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

# blegh ------------------------------------------------------------------------

bracket_ids <- c(326, 267, 124, 131, 205, 4, 55, 10, 118, 76, 192, 164, 244, 107, 64, 301)

stan_data <- 
  list(
    N = 8,
    T = 16,
    V = rep(0, 8),
    tid = array(1:16, dim = c(2,8)),
    alpha = log(70/40),
    beta_Mu = beta_Mu[bracket_ids,],
    beta_Sigma = beta_Sigma[bracket_ids,,],
    log_sigma_i = log_sigma_i[2,]$mean,
    hurdle_Mu = hurdle_Mu,
    hurdle_Sigma = hurdle_Sigma,
    poisson_Mu = poisson_Mu,
    poisson_Sigma = poisson_Sigma
  )

predictions <- 
  prediction$sample(
    data = stan_data,
    seed = 2025,
    iter_warmup = 100,
    iter_sampling = 1250,
    chains = 8,
    parallel_chains = 8,
    fixed_param = TRUE
  )

predictions$draws(c("Y_rep"), format = "df") %>%
  as_tibble() %>%
  pivot_longer(starts_with("Y_rep"),
               names_to = "variable",
               values_to = "score") %>%
  mutate(variable = str_remove_all(variable, "Y_rep\\[|\\]")) %>%
  separate(variable, c("tid", "game")) %>%
  nest(data = -c(tid, game)) %>%
  mutate(across(c(tid, game), as.integer),
         location = if_else(tid == 1, "home", "away"),
         tid = 2 * game - 2 + tid) %>%
  mutate(name = teams$team_name[bracket_ids[tid]]) %>%
  select(-tid) %>%
  nplyr::nest_select(data, score, .draw) %>%
  unnest(data) %>%
  pivot_wider(names_from = location,
              values_from = c(score, name)) %>%
  left_join(predictions$draws("Ot", format = "df") %>%
              as_tibble() %>%
              select(starts_with("Ot"), .draw) %>% 
              pivot_longer(starts_with("Ot"),
                           names_to = "game",
                           values_to = "n_ot") %>%
              nest(data = -game) %>%
              mutate(game = parse_number(game)) %>%
              unnest(data)) %>%
  nest(data = -c(game, name_home, name_away)) %>%
  mutate(p_home = map_dbl(data, ~sum(.x$score_home > .x$score_away)/nrow(.x)),
         p_away = 1 - p_home,
         game_title = glue::glue("Game {game}: {name_home} ({scales::label_percent(accuracy = 1)(p_home)}) ",
                                 "vs. {name_away} ({scales::label_percent(accuracy = 1)(p_away)})")) %>%
  unnest(data) %>%
  count(game_title,
        score_home,
        score_away,
        n_ot) %>%
  ggplot(aes(x = score_home,
             y = score_away,
             color = as.factor(n_ot),
             alpha = n)) + 
  geom_point() +
  facet_wrap(~game_title, ncol = 2) + 
  theme_rieke() + 
  theme(legend.position = "none") 

# some posterior explorations
bind_rows(beta_o %>% mutate(variable = "beta_o"),
          beta_d %>% mutate(variable = "beta_d"),
          beta_h %>% mutate(variable = "beta_h")) %>%
  nest(data = -team_name) %>%
  mutate(obs = map_int(data, nrow)) %>%
  # filter(obs == 69) %>%
  slice_sample(n = 12) %>%
  unnest(data) %>%
  ggplot(aes(x = season,
             y = median,
             ymin = q5,
             ymax = q95)) + 
  geom_ribbon(aes(fill = variable),
              alpha = 0.25) + 
  geom_line(aes(color = variable)) +
  scale_color_brewer(palette = "Dark2") + 
  scale_fill_brewer(palette = "Dark2") + 
  facet_wrap(~team_name) + 
  theme_rieke()

bind_rows(log_sigma_o,
          log_sigma_d,
          log_sigma_h,
          log_sigma_i) %>%
  ggplot(aes(x = season,
             y = median,
             ymin = q5,
             ymax = q95)) + 
  geom_ribbon(alpha = 0.25) + 
  geom_line() +
  facet_wrap(~variable) + 
  theme_rieke()

bind_rows(gamma_0,
          delta_0,
          gamma_ot,
          delta_ot) %>%
  ggplot(aes(x = season,
             y = median,
             ymin = q5,
             ymax = q95)) + 
  geom_ribbon(alpha = 0.25) + 
  geom_line() + 
  facet_wrap(~variable) +
  theme_rieke()

