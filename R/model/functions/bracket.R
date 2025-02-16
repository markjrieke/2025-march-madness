run_bracket_model <- function(league,
                              ...,
                              season = 2025,
                              date = Sys.Date(),
                              samples = 1e4,
                              chains = 8) {
  
  cli::cli_h1(glue::glue("{str_to_title(league)} {scales::label_date('%b %d, %Y')(Sys.Date())} Bracket Update"))
  
  # evaluate processing time
  start_ts <- Sys.time()
  
  # check whether or not functions.stan has been updated since last run
  if (out_of_date("exe/bracket", "stan/functions.stan")) {
    force_recompile <- TRUE
  } else {
    force_recompile <- FALSE
  }
  
  # compile model!
  bracket <-
    cmdstan_model(
      "stan/bracket.stan",
      dir = "exe/",
      force_recompile = force_recompile
    )
  
  # rename variables for internal use
  league_int <- league
  season_int <- season
  date_int <- date
  
  # setup teams and bracket structure
  tournament <- tournament_structure(league, season)
  
  # separate tournament objects
  teams <- tournament$teams
  wid0 <- tournament$wid0
  
  # set data for stan
  stan_data <- set_bracket_params(teams, wid0, league, date)
  
  # make predictions!
  bracket_fit <-
    bracket$sample(
      data = stan_data,
      seed = 2025,
      iter_warmup = 100,
      iter_sampling = round(samples/chains),
      chains = chains,
      parallel_chains = chains,
      fixed_param = TRUE
    )
  
  # probability of each team advancing to each round
  p_advance <- 
    bracket_fit$summary("p_advance") %>%
    mutate(variable = str_remove_all(variable, "p_advance\\[|\\]")) %>%
    separate(variable, c("tid", "round"), ",") %>%
    mutate(across(c(tid, round), as.integer)) %>%
    left_join(teams) %>%
    transmute(league = league,
              date = date,
              team_name = team_name,
              round = round,
              p_advance = mean)
  
  # write results out
  p_advance %>%
    append_parquet("out/bracket/p_advance.parquet")
  
  # evaluate processing time
  end_ts <- Sys.time()
  
  # generate model log
  model_log <-
    tibble(
      model_name = "bracket",
      model_version = file.info("stan/bracket.stan")$mtime,
      start_ts = start_ts,
      end_ts = end_ts,
      observations = 63,
      num_divergent = 0,
      num_max_treedepth = 0,
      samples = samples,
      season = 2025,
      league = league,
      date_min = mdy("11/1/2024"),
      date_max = date,
      target_variable = glue::glue("")
    )
  
  # append log
  model_log %>%
    append_parquet("out/model_log.parquet")
  
}

set_bracket_params <- function(teams, wid0, league, date) {
  
  # log mean score
  alpha <- log(70/40)
  
  # team skill parameters
  beta <- 
    set_beta(
      teams = teams,
      league = league,
      date = date
    )
  
  beta_Mu <- beta$beta_Mu
  beta_Sigma <- beta$beta_Sigma
  
  # overdispersion parameter
  log_sigma_i <- 
    set_log_sigma_i(
      league = league, 
      date = date, 
      log_sigma_i_step_sigma = 0
    )
  
  log_sigma_i_mu <- log_sigma_i$mean
  log_sigma_i_sigma <- log_sigma_i$sd
  
  # probability of overtime parameters
  hurdle_params <- set_overtime_params("0", league, date, 0, 0)
  poisson_params <- set_overtime_params("ot", league, date, 0, 0)
  
  hurdle_Mu <- hurdle_params$Mu
  hurdle_Sigma <- hurdle_params$Sigma
  
  poisson_Mu <- poisson_params$Mu
  poisson_Sigma <- poisson_params$Sigma
  
  # coalesce to list for stan
  stan_data <-
    list(
      T = 64,
      wid0 = wid0,
      alpha = alpha,
      beta_Mu = beta_Mu,
      beta_Sigma = beta_Sigma,
      log_sigma_i_mu = log_sigma_i_mu,
      log_sigma_i_sigma = log_sigma_i_sigma,
      hurdle_Mu = hurdle_Mu,
      hurdle_Sigma = hurdle_Sigma,
      poisson_Mu = poisson_Mu,
      poisson_Sigma = poisson_Sigma
    )
  
  return(stan_data)
  
}

tournament_structure <- function(league, season) {
  
  # internal renaming for filtering
  league_int <- league
  season_int <- season
  
  # internal redirect for testing pre-tournament
  if (season == 2025) {
    game_file <- glue::glue("out/update/{league}-games.parquet")
  } else {
    game_file <- "data/games/games.parquet"
  }
  
  # import set of games to be played
  games <- 
    arrow::read_parquet(game_file) %>%
    filter(league == league_int,
           season == season_int,
           game_type == "Postseason")
  
  # scrape current bracket
  url <- glue::glue("https://www.espn.com/{league}-college-basketball/bracket/_/season/{season}")
  html <- read_html(url)
  
  # overall bracket html - regions as nodes
  four_region <- 
    html %>%
    html_element(".BracketWrapper") %>%
    html_children() %>%
    html_children() %>%
    html_children() %>%
    html_children()
  
  # south/east ascend from left-to-right - rounds as nodes
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
  
  # midwest/west ascend from right-to-left - rounds as nodes
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
  
  # final four and championship game nodes
  final_four <- 
    four_region %>%
    pluck(2) %>%
    html_children() %>%
    html_children() %>%
    html_children() %>%
    html_children()
  
  # extract game ids for final four and championship
  south_east <- 
    final_four %>%
    pluck(1) %>%
    html_element(".BracketMatchup") %>%
    html_elements("a") %>%
    html_attr("href") %>%
    parse_game_id()
  
  midwest_west <-
    final_four %>%
    pluck(3) %>%
    html_element(".BracketMatchup") %>%
    html_elements("a") %>%
    html_attr("href") %>%
    parse_game_id()
  
  finals <- 
    final_four %>%
    pluck(2) %>%
    html_children() %>%
    html_element("a") %>%
    html_attr("href") %>%
    parse_game_id()
  
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
    arrow::read_parquet(game_file) %>%
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
                filter(league == league_int) %>%
                select(team_id, team_name))
  
  # initialize bracket structure
  wid0 <- array(0, dim = c(64, 7))
  
  # set first round
  wid0[1:64,1] <-
    c(
      flatten_teams(south[[1]], games, teams),
      flatten_teams(east[[1]], games, teams),
      flatten_teams(midwest[[1]], games, teams),
      flatten_teams(west[[1]], games, teams)
    )
  
  # set second round
  wid0[1:32,2] <-
    c(
      flatten_teams(south[[2]], games, teams),
      flatten_teams(east[[2]], games, teams),
      flatten_teams(midwest[[2]], games, teams),
      flatten_teams(west[[2]], games, teams)
    )
  
  # set third round (sweet sixteen)
  wid0[1:16,3] <-
    c(
      flatten_teams(south[[3]], games, teams),
      flatten_teams(east[[3]], games, teams),
      flatten_teams(midwest[[3]], games, teams),
      flatten_teams(west[[3]], games, teams)
    )
  
  # set fourth round (elite eight)
  wid0[1:8,4] <-
    c(
      flatten_teams(south[[4]], games, teams),
      flatten_teams(east[[4]], games, teams),
      flatten_teams(midwest[[4]], games, teams),
      flatten_teams(west[[4]], games, teams)
    )
  
  # set fifth round (final four)
  wid0[1:4,5] <-
    c(
      extract_teams(south_east, games, teams), 
      extract_teams(midwest_west, games, teams)
    )
  
  # set championship
  wid0[1:2,6] <- extract_teams(finals, games, teams)
  
  # return both teams and wid0
  out <-
    list(
      teams = teams,
      wid0 = wid0
    )
  
  return(out)
  
}

parse_game_id <- function(x) {
  
  x %>%
    str_remove_all("/mens|/womens|-college-basketball/game/_/gameId/") %>%
    map_chr(~if_else(str_detect(.x, "/"), 
                     str_sub(.x, 1, str_locate(.x, "/")[,1] - 1),
                     .x))
  
}

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
        html_element("a") %>%
        html_attr("href") %>%
        parse_game_id()
      
    }
    
    # assign current round's ids to the top-level list
    game_ids[[r]] <- round_game_ids
    
  }
  
  return(game_ids)
  
}

extract_teams <- function(game_id, games, teams) {
  
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

flatten_teams <- function(game_ids, games, teams) {
  
  game_ids %>%
    map(~extract_teams(.x, games, teams)) %>%
    list_c()
  
}


