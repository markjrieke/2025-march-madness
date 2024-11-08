# setup ------------------------------------------------------------------------

library(tidyverse)
library(rvest)
library(furrr)

# scrape results ---------------------------------------------------------------

#' Main function for scraping game-level results
scrape_game <- function(league, game_id, sleep_time = 1) {
  
  # scrape the html for the specified game
  url <- glue::glue("https://www.espn.com/{league}-college-basketball/game/_/gameId/{game_id}")
  html <- read_html(url)
  
  # topline summary
  competitors <- 
    html %>%
    html_elements(".Gamestrip__Competitors")
  
  # elements for each team
  left_elements <- extract_elements(competitors, league, "left")
  right_elements <- extract_elements(competitors, league, "right")
  
  # if missing, set home to false
  # (this is the case for post-season/neutral games)
  left_elements$home <- if (length(left_elements$home) == 0) FALSE else left_elements$home
  right_elements$home <- if (length(right_elements$home) == 0) FALSE else right_elements$home
  
  # assign to winner/loser elements based on score
  if (left_elements$score > right_elements$score | (is.na(left_elements$score) & is.na(right_elements$score))) {
    
    winner_elements <- left_elements
    loser_elements <- right_elements
    
  } else {
    
    winner_elements <- right_elements
    loser_elements <- left_elements
    
  }
  
  # extract periods from summary table
  periods <- 
    competitors %>%
    html_table() %>%
    pluck(1) %>%
    ncol()
  
  # remove team name/total col
  periods <- periods - 2
  
  # some games are canceled each season
  if (periods <= 0) {
    
    out <-
      tibble(
        winner_score = 0,
        winner_home = FALSE,
        winner_id = "missing",
        loser_score = 0,
        loser_home = FALSE,
        loser_id = "missing",
        periods = periods
      )
    
  } else {
    
    out <- 
      tibble(
        winner_score = winner_elements$score,
        winner_home = winner_elements$home,
        winner_id = winner_elements$id,
        loser_score = loser_elements$score,
        loser_home = loser_elements$home,
        loser_id = loser_elements$id,
        periods = periods
      )
    
  }
  
  Sys.sleep(sleep_time)
  
  return(out)
  
}

# helper functions -------------------------------------------------------------

#' Helper function: extract team-level results for the winning/losing team
extract_elements <- function(html,
                             league,
                             team) {
  
  team_elements <-
    html %>%
    html_elements(glue::glue(".Gamestrip__Team--{team}"))
  
  out <-
    list(
      score = extract_score(team_elements),
      home = extract_home(team_elements),
      id = extract_id(team_elements, league)
    )
  
  return(out)
  
}

#' Helper function: extract a team's score from the html summary
extract_score <- function(html) {
  
  html %>%
    html_element(".Gamestrip__Score") %>%
    html_text2() %>%
    parse_number()
  
}

#' Helper function: extract whether (or not) a team is the home team
extract_home <- function(html) {
  
  html %>%
    html_elements(".Gamestrip__InnerRecord") %>%
    html_text2() %>%
    str_detect("home")

}

#' Helper function: extract a team's id from the html summary
extract_id <- function(html, league) {
  
  slug <- 
    html %>%
    html_elements(".ScoreCell__Truncate") %>%
    html_elements("a") %>%
    html_attr("href")
  
  if (length(slug) == 0) {
    
    out <- "missing"
    
  } else {
    
    out <-
      slug %>%
      str_remove_all(glue::glue("/{league}-college-basketball/team/_/id/")) %>%
      str_split_1("/") %>%
      pluck(1)
    
  }
  
}

# scrape ! ---------------------------------------------------------------------

# pick up data that has already been saved
written <- 
  tibble(file = list.files("data/games/")) %>%
  mutate(games = map(file, ~arrow::read_parquet(paste0("data/games/", .x)))) %>%
  select(-file) %>%
  unnest(games) %>%
  pull(game_id)

seasons <- 
  arrow::read_parquet("data/schedule/schedule.parquet") %>%
  mutate(month = month(date),
         year = year(date),
         season = if_else(month >= 11, year + 1, year)) %>%
  select(-c(month, year)) %>%
  filter(!game_id %in% written) %>%
  nest(data = -c(league, season))

# scrape results
plan(multisession, workers = 8)

for (league in c("mens", "womens")) {
  
  for (season in 2002:2024) {
    
    # # skip over anything that already exists
    # if (file.exists(glue::glue("data/games/{league}-{season}.parquet"))) {
    #   cli::cli_alert_info("{league}-{season}.parquet already exists, skipping")
    #   next
    # }
    
    # rename variables for filtering
    league_int <- league
    season_int <- season
    
    # skip if season is completed
    chunks <- 
      seasons %>%
      filter(league == league_int,
             season == season_int)
    
    if (nrow(chunks) == 0) {
      cli::cli_alert_info("{league}-{season}.parquet already complete, skipping")
      next
    }
    
    #  break up each season into manageable "chunks"
    chunks <- 
      chunks %>%
      unnest(data) %>%
      rowid_to_column("chunk") %>%
      mutate(chunk = ceiling(chunk/200))
    
    n_chunks <- max(chunks$chunk)
    
    for (chunk in 1:n_chunks) {
      
      cli::cli_h1(
        glue::glue(
          "{str_to_title(league)} {season - 1}-{str_sub(as.character(season), -2)}",
          "Chunk {chunk}/{n_chunks}",
          .sep = ": "
        )
      )
      
      # rename variables for filtering
      chunk_int <- chunk
      
      # scrape game results for this chunk of games
      game_chunk <- 
        chunks %>%
        filter(chunk == chunk_int) %>%
        mutate(result = future_pmap(list(league, game_id),
                                    ~scrape_game(..1, ..2),
                                    .progress = TRUE)) %>%
        unnest(result) %>%
        select(-chunk)
      
      if (file.exists(glue::glue("data/games/{league}-{season}.parquet"))) {
        
        arrow::read_parquet(glue::glue("data/games/{league}-{season}.parquet")) %>%
          bind_rows(game_chunk) %>%
          arrow::write_parquet(glue::glue("data/games/{league}-{season}.parquet"))
        
      } else {
        
        game_chunk %>%
          arrow::write_parquet(glue::glue("data/games/{league}-{season}.parquet"))
        
      }
      
      # give 30-s break between scraping chunks
      cli::cli_progress_bar("Sleeping...", total = 30, clear = FALSE)
      for (t in 1:30) {
        Sys.sleep(1)
        cli::cli_progress_update()
      }
      
    }
    
    # give 5-min break between scraping seasons
    cli::cli_progress_bar("Sleeping...", total = 300, clear = FALSE)
    for (t in 1:300) {
      Sys.sleep(1)
      cli::cli_progress_update()
    }
    
  }
  
  # give a 10-min break between scraping leagues
  cli::cli_progress_bar("Sleeping...", total = 600, clear = FALSE)
  for (t in 1:600) {
    Sys.sleep(1)
    cli::cli_progress_update()
  }
  
}

plan(sequential)




