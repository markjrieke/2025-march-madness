# setup ------------------------------------------------------------------------

library(tidyverse)
library(rvest)

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
  
  html %>%
    html_elements(".ScoreCell__Truncate") %>%
    html_elements("a") %>%
    html_attr("href") %>%
    str_remove_all(glue::glue("/{league}-college-basketball/team/_/id/")) %>%
    str_split_1("/") %>%
    pluck(1)
  
}
