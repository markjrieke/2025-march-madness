bracket <- function() {
  
  url <- "https://www.espn.com/mens-college-basketball/bracket/_/season/2023"
  url <- "https://web.archive.org/web/20230324013710/https://www.espn.com/mens-college-basketball/bracket/_/season/2023"
  bracket <- read_html(url)
  
  bracket <- read_html("tmp.html")
  
  # region setup -----------------------------------------------------------------
  
  # four region overlay
  four_region <- 
    bracket %>%
    html_element(".BracketWrapper") %>%
    html_children() %>%
    html_children() %>%
    html_children() %>%

    # for reading from wayback machine only
    pluck(3) %>%
    
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
    
    # each round contains a vector of ids
    game_ids <- list()
    
    # iterate over rounds
    for (r in 1:4) {
      
      # extract all the BracketMatchup classes
      # (these exist whether or not the game_id has been created)
      matchups <- 
        x %>%
        pluck(r) %>%
        html_children() %>%
        html_elements(".BracketMatchup")
      
      # container for the current round game_ids
      round_game_ids <- vector("character", length = length(matchups))
      
      # iterate over matchups --- empty games are encoded as NA
      for (m in 1:length(matchups)) {
        
        round_game_ids[m] <- 
          matchups %>%
          pluck(m) %>%
          html_children() %>%
          html_attr("href") %>%
          str_remove_all("/web/20230324013710mp_/https://www.espn.com|/mens-college-basketball/game/_/gameId/") %>%
          map_chr(~if_else(str_detect(.x, "/"), 
                           str_sub(.x, 1, str_locate(.x, "/")[,1] - 1),
                           .x))
        
      }
      
      # assign current round's ids to the top-level list
      game_ids[[r]] <- round_game_ids
      
    }
    
    return(game_ids)
    
  }

  # extract list of vectors of game ids
  south <- extract_game_ids(south)
  east <- extract_game_ids(east)
  midwest <- extract_game_ids(midwest)
  west <- extract_game_ids(west)

  # reverse right side of bracket
  midwest <- midwest[4:1]
  west <- west[4:1]
  
  # get the first round in 1:64 format
  first_round <-
    tibble(game_id = c(south[[1]], east[[1]], midwest[[1]], west[[1]])) %>%
    rowid_to_column("rank")
  
  # assign team ids
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
  
  games <- 
    arrow::read_parquet("data/games/games.parquet") %>%
    filter(league == "mens",
           season == 2023,
           home_id %in% teams$team_id | away_id %in% teams$team_id,
           game_type == "Postseason")
  
  extract_teams <- function(game_id) {
    
    game_id_int <- game_id
    
    ids <-
      games %>%
      filter(game_id == game_id_int) %>%
      select(home_id, away_id) %>%
      pivot_longer(everything(),
                   names_to = "location",
                   values_to = "id") %>%
      pull(id)
    
    if (length(ids) > 0) {
      out <- match(ids, teams$team_id)
    } else {
      out <- c(0, 0)
    }
    
    return(out)
    
  }
  
  flatten_teams <- function(game_ids) {
    
    game_ids %>%
      map(extract_teams) %>%
      list_c()
    
  }
  
  wid0 <- array(0, dim = c(64, 7))
  
  wid0[1:64,1] <-
    c(
      flatten_teams(south[[1]]),
      flatten_teams(east[[1]]),
      flatten_teams(midwest[[1]]),
      flatten_teams(west[[1]])
    )
  
  wid0[1:32,2] <-
    c(
      flatten_teams(south[[2]]),
      flatten_teams(east[[2]]),
      flatten_teams(midwest[[2]]),
      flatten_teams(west[[2]])
    )
  
  wid0[1:16,3] <-
    c(
      flatten_teams(south[[3]]),
      flatten_teams(east[[3]]),
      flatten_teams(midwest[[3]]),
      flatten_teams(west[[3]])
    )
  
  wid0[1:8,4] <-
    c(
      flatten_teams(south[[4]]),
      flatten_teams(east[[4]]),
      flatten_teams(midwest[[4]]),
      flatten_teams(west[[4]])
    )
  
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
      wid0 = wid0,
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
    # filter(round == 6) %>%
    # arrange(desc(p_advance))
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


