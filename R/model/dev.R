library(tidyverse)
library(riekelib)
library(cmdstanr)

tulsa <- 
  arrow::read_parquet("data/games/games.parquet") %>%
  mutate(across(ends_with("_id"), ~if_else(.x == "564", NA, .x)),
         across(ends_with("_id"), ~if_else(is.na(.x), "missing", .x))) %>%
  filter(league == "mens",
         season == 2018) #%>%
  filter(home_name == "Tulsa" | away_name == "Tulsa")

model <- 
  cmdstan_model(
    "stan/dev_01.stan",
    dir = "exe/"
  )

stan_data <-
  list(
    N = nrow(tulsa),
    H = tulsa$home_score,
    A = tulsa$away_score
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

tulsa_fit$draws(c("Sh", "Sa"), format = "df") %>%
  as_tibble() %>%
  slice_sample(n = 1000) %>%
  ggplot(aes(x = Sh,
             y = Sa)) + 
  geom_point()
