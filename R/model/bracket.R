#' Run the bracket model
#' 
#' @description
#' The bracket model simulates the outcome of the tournament. Midway through the
#' tournament, the model treats realized outcomes as occurring with 100% 
#' probability such that the probability of each team winning the tournament 
#' updates as they advance through the tournament. Like the prediction model, 
#' the bracket model does not conduct inference or update parameters, but 
#' instead runs as a `fixed parameter` model.
#'  
#' @param league Which league to extract results for. Either "mens" or "womens".
#' @param ... Unused
#' @param season Which season to simulate tournament results for. Used for 
#'        testing only.
#' @param date The date in the current season to run. Uses team skill and 
#'        overtime parameters from the specified date.
#' @param samples Number of posterior samples to generate.
#' @param chains Number of chains used to fit the model. All chains will be run
#'        in parallel, if available. 
run_bracket_model <- function(league,
                              ...,
                              season = 2025,
                              date = Sys.Date(),
                              samples = 1e4,
                              chains = 8) {
  
  cli::cli_h1(glue::glue("{str_to_title(league)} {scales::label_date('%b %d, %Y')(date)} Bracket Update"))
  
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
    mutate(across(c(tid, round), as.integer),
           advanced = pmap_lgl(list(tid, round), ~..1 %in% wid0[,..2 + 1])) %>%
    group_by(round) %>%
    mutate(n_advanced = sum(advanced),
           round_complete = n_advanced == 2^(6 - round)) %>%
    ungroup() %>%
    mutate(status = case_when(advanced ~ "Won",
                              !advanced & round_complete ~ "Eliminated")) %>%
    left_join(teams) %>%
    transmute(league = league,
              date = date,
              tid = tid,
              team_name = team_name,
              round = round,
              status = status,
              p_advance = mean)
  
  # write results out
  p_advance %>%
    append_parquet("out/bracket/p_advance.parquet")
  
  # save wid0 output
  tibble(league = league,
         date = date,
         wid0 = list(wid0)) %>%
    append_rds("out/bracket/wid0.rds")
  
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

#' Prep data for the bracket Stan model
#' 
#' @param teams A tibble mapping ESPN `team_name` and `team_id` to an internal
#'        mapping id, `tid`.
#' @param wid0 A 64 x 7 matrix of integers that map `tid` to matchups. Each 
#'        column maps to a round in the tournament, with `wid[1,7]` indicating
#'        the overall winner. `0` is used as a filler value for the matrix 
#'        (i.e., `wid[2:64,7]` contains all `0`) or as an indication that the 
#'        outcome of a game has not yet been determined.
#' @param league Which league to extract results for. Either "mens" or "womens".
#' @param date The date in the current season to run. Uses team skill and 
#'        overtime parameters from the specified date.
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

#' Generate the data structure necessary for the model
#' 
#' @description
#' This function scrapes the March Madness bracket page from ESPN and returns a 
#' list containing the teams in the tournament (`teams`) and the current state
#' of the tournament in a format that the bracket model expects (`wid0`).
#' 
#' @param league Which league to extract results for. Either "mens" or "womens".
#' @param season Which season to simulate tournament results for. Used for 
#'        testing only.
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
           season == season_int)
  
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
  
  # correct wid0 bracket paths
  wid0 <- adjust_wid(wid0)
  
  # return both teams and wid0
  out <-
    list(
      teams = teams,
      wid0 = wid0
    )
  
  return(out)
  
}

#' Return a set of ESPN game_ids from a vector of url slugs
#' 
#' @param x A vector of url slugs that link to games
parse_game_id <- function(x) {
  
  x %>%
    str_remove_all("/mens|/womens|-college-basketball/game/_/gameId/") %>%
    map_chr(~if_else(str_detect(.x, "/"), 
                     str_sub(.x, 1, str_locate(.x, "/")[,1] - 1),
                     .x))
  
}

#' Convert region bracket HTML into a list of vectors containing ESPN game_ids
#' for each round of the tournament
#' 
#' @param x Regional bracket HTML (i.e., south, east, midwest, or west)
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

#' Extract a set of tids given a game_id
#' 
#' @param game_id ESPN game_id
#' @param games A tibble containing schedule postseason games
#' @param teams A tibble mapping ESPN `team_name` and `team_id` to an internal
#'        mapping id, `tid`.
extract_teams <- function(game_id, games, teams) {
  
  if (length(game_id) == 0) {
    return(c(0, 0))
  }
  
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
    out <- replace_na(out, 0)
  } else {
    out <- c(0, 0)
  }
  
  return(out)
  
}

#' Convert a vector of game ids into a vector of tids
#' 
#' @param game_id A vector ESPN game_ids
#' @param games A tibble containing schedule postseason games
#' @param teams A tibble mapping ESPN `team_name` and `team_id` to an internal
#'        mapping id, `tid`.
flatten_teams <- function(game_ids, games, teams) {
  
  game_ids %>%
    map(~extract_teams(.x, games, teams)) %>%
    list_c()
  
}

#' Adjust tid positions in wid0 to correctly map bracket path
#' 
#' @param wid0 A 64 x 7 matrix of integers that map `tid` to matchups. Each 
#'        column maps to a round in the tournament, with `wid[1,7]` indicating
#'        the overall winner. `0` is used as a filler value for the matrix 
#'        (i.e., `wid[2:64,7]` contains all `0`) or as an indication that the 
#'        outcome of a game has not yet been determined.
adjust_wid <- function(wid0) {
  
  # return the adjusted bracket
  wid_adj <- wid0
  
  # adjust wid0 so that bracket advancements proceed along the correct path
  for (r in 6:2) {
    
    # vector for noting if teams are in the right place (TRUE) or not (FALSE)
    adjust <- vector("logical", 2^(7-r))
    
    for (g in 1:(2^(7-r))) {
      
      # check if teams are in the right position based on upstream bracket
      adjust[g] <- wid0[g,r] %in% wid0[(2 * g - 1):(2 * g), r - 1]
      
    }
    
    # flip teams around if necessary
    for (g in 1:max(1, 2^(7-r-1))) {
      
      # range of positions for current swap to eval 
      range <- (2 * g - 1):(2 * g)
      
      if (all(!adjust[range])) {
        
        swap <- wid0[range, r]
        wid_adj[range, r] <- rev(swap)
        
      }
      
    }
    
  }
  
  return(wid_adj)
  
}










