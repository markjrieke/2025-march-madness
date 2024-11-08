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
  loser_elements <- extract_elements(competitors, league, "loser")
  winner_elements <- extract_elements(competitors, league, "winner")
  
  # if missing, set home to false
  # (this is the case for post-season/neutral games)
  loser_elements$home <- if (length(loser_elements$home) == 0) FALSE else loser_elements$home
  winner_elements$home <- if (length(winner_elements$home) == 0) FALSE else winner_elements$home
  
  # extract periods from summary table
  periods <- 
    competitors %>%
    html_table() %>%
    pluck(1) %>%
    ncol()
  
  # remove team name/total col
  periods <- periods - 2
  
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

seasons <- 
  arrow::read_parquet("data/schedule/schedule.parquet") %>%
  mutate(month = month(date),
         year = year(date),
         season = if_else(month >= 11, year + 1, year)) %>%
  select(-c(month, year)) %>%
  nest(data = -c(league, season))

# scrape results
plan(multisession, workers = 8)

for (league in c("mens", "womens")) {
  
  for (season in 2002:2024) {
    
    # skip over anything that already exists
    if (file.exists(glue::glue("data/games/{league}-{season}.parquet"))) {
      cli::cli_alert_info("{league}-{season}.parquet already exists, skipping")
      next
    }
    
    # rename variables for filtering
    league_int <- league
    season_int <- season
    
    # break up each season into manageable "chunks"
    chunks <- 
      seasons %>%
      filter(league == league_int,
             season == season_int) %>%
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
        unnest(result)
      
      # create a df for this season
      if (chunk == 1) {
        
        games <- game_chunk
        
      } else {
        
        games <-
          games %>%
          bind_rows(game_chunk)
        
      }
      
      # give 30-s break between scraping chunks
      cli::cli_progress_bar("Sleeping...", total = 30, clear = FALSE)
      for (t in 1:30) {
        Sys.sleep(1)
        cli::cli_progress_update()
      }
      
    }
    
    # write season-level results out
    games %>%
      select(-chunk) %>%
      arrow::write_parquet(glue::glue("data/games/{league}-{season}.parquet"))
    
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




