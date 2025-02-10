library(rvest)

bracket <- function() {
  
  url <- "https://www.espn.com/mens-college-basketball/bracket/_/season/2023"
  
  bracket <- read_html(url)
  
  # region setup -----------------------------------------------------------------
  
  # four region overlay
  four_region <- 
    bracket %>%
    html_element(".BracketWrapper") %>%
    html_children() %>%
    html_children() %>%
    html_children() %>%
    html_children()
  
  # left-to-right in ascending tournament order
  south <- 
    four_region %>%
    pluck(1) %>%
    html_children() %>%
    pluck(1) %>%
    html_children() %>%
    pluck(2) %>%
    html_children()
  
  east <- 
    four_region %>%
    pluck(3) %>%
    html_children() %>%
    pluck(1) %>%
    html_children() %>%
    pluck(2) %>%
    html_children()
  
  # right-to-left in ascending tournament order
  midwest <- 
    four_region %>%
    pluck(1) %>%
    html_children() %>%
    pluck(2) %>%
    html_children() %>%
    pluck(2) %>%
    html_children()
  
  west <- 
    four_region %>%
    pluck(3) %>%
    html_children() %>%
    pluck(2) %>%
    html_children() %>%
    pluck(2) %>%
    html_children()
  
  # first round games ------------------------------------------------------------
  
  # later, I'll need to deal with non-complete rounds
  extract_game_ids <- function(x) {
    
    x %>%
      html_children() %>%
      html_elements(".BracketMatchup") %>%
      html_elements("a") %>%
      html_attr("href") %>%
      str_remove_all("/mens-college-basketball/game/_/gameId/") %>%
      map_chr(~str_sub(.x, 1, str_locate(.x, "/")[,1] - 1))
    
  }
  
  south_1r <- 
    south %>%
    pluck(1) %>%
    extract_game_ids()
  
  east_1r <- 
    east %>%
    pluck(1) %>% 
    extract_game_ids()
  
  midwest_1r <- 
    midwest %>%
    pluck(4) %>%
    extract_game_ids() 
  
  west_1r <- 
    west %>%
    pluck(4) %>%
    extract_game_ids()
  
  first_round <- 
    tibble(game_id = c(south_1r, east_1r, midwest_1r, west_1r)) %>%
    rowid_to_column("rank")
  
  teams <-  
    arrow::read_parquet("data/games/games.parquet") %>%
    filter(game_id %in% first_round$game_id) %>%
    select(ends_with("id")) %>%
    left_join(first_round) %>%
    arrange(rank) %>%
    select(rank, home_id, away_id) %>%
    pivot_longer(ends_with("id"),
                 names_to = "location",
                 values_to = "team_id") %>%
    rowid_to_column("tid") %>%
    select(tid, team_id) %>%
    left_join(arrow::read_parquet("data/teams/teams.parquet") %>%
                filter(league == "mens") %>%
                select(team_id, team_name))
  
  beta <- 
    read_rds("out/update/team_parameters.rds") %>%
    filter(league == "mens",
           date == max(date)) %>%
    filter(team_id %in% teams$team_id) %>%
    left_join(teams) %>%
    arrange(tid)
  
  beta_Mu <- array(dim = c(64, 3))
  beta_Sigma <- array(dim = c(64, 3, 3))
  
  for (t in 1:64) {
    
    beta_Mu[t,] <- beta$Mu[[t]]
    beta_Sigma[t,,] <- beta$Sigma[[t]]
    
  }
  
  wid0 <- array(0, dim = c(64, 7))
  wid0[,1] <- teams$tid
  
  alpha <- log(70/40)
  
  log_sigma_i <- 
    arrow::read_parquet("out/update/log_sigma_i.parquet") %>%
    filter(date == max(date),
           league == "mens")
  
  log_sigma_i_mu <- log_sigma_i$mean
  log_sigma_i_sigma <- log_sigma_i$sd
  
  overtime_params <- 
    read_rds("out/update/global_parameters.rds") %>%
    filter(date == max(date),
           league == "mens")
  
  hurdle_params <- 
    overtime_params %>%
    filter(parameter == "0")
  
  poisson_params <-
    overtime_params %>%
    filter(parameter == "ot")
  
  hurdle_Mu <- hurdle_params$Mu[[1]]
  hurdle_Sigma <- hurdle_params$Sigma[[1]]
  
  poisson_Mu <- poisson_params$Mu[[1]]
  poisson_Sigma <- poisson_params$Sigma[[1]]
  
  bracket <-
    cmdstan_model(
      "stan/bracket.stan",
      dir = "exe/"
    )
  
  stan_data <-
    list(
      T = 64,
      wid0 = wid,
      alpha = log(70/40),
      beta_Mu = beta_Mu,
      beta_Sigma = beta_Sigma,
      log_sigma_i_mu = log_sigma_i_mu,
      log_sigma_i_sigma = log_sigma_i_sigma,
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
  
  p_advance %>%
    mutate(variable = str_remove_all(variable, "p_advance\\[|\\]")) %>%
    separate(variable, c("tid", "round"), ",") %>%
    mutate(across(c(tid, round), as.integer)) %>%
    left_join(teams) %>%
    select(team_name,
           round,
           p_advance = mean) %>%
    mutate(winner = if_else(team_name == "UConn", "winner", "not")) %>%
    ggplot(aes(x = round,
               y = p_advance,
               group = team_name,
               color = winner,
               linewidth = winner,
               alpha = winner)) +
    geom_line() +
    scale_color_manual(values = c("gray40", "royalblue")) +
    scale_linewidth_manual(values = c(0.5, 2)) +
    scale_alpha_manual(values = c(0.25, 1)) +
    theme_rieke()
  
}


