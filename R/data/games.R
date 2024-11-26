# setup ------------------------------------------------------------------------

library(tidyverse)
library(rvest)
library(furrr)

source("R/data/utils.R")

# functions --------------------------------------------------------------------

scrape_games <- function(league, team_id, season) {
  
  # read html
  url <- glue::glue("https://www.espn.com/{league}-college-basketball/team/schedule/_/id/{team_id}/season/{season}")
  
  # get schedule as html table
  result <- retry_catch(read_html(url))
  
  # log a warning if need be
  if (!is.null(result$warning)) {
    
    tibble(league = league,
           team_id = team_id,
           season = season,
           warning = as.character(result$warning)) %>%
      write_log("data/games/games-warnings.parquet")
    
  }
  
  # log an error if need be
  if (!is.null(result$error)) {
    
    tibble(league = league,
           team_id = team_id,
           season = season,
           error = as.character(result$error)) %>%
      write_log("data/games/games-errors.parquet")
    
    # exit the function
    return()
    
  }
  
  # extract the schedule as a table
  elements <-
    result$result %>%
    html_elements(".Table__TR")
  
  # infer which elements are table rows & which are headers
  games <-
    tibble(text = html_text2(elements)) %>%
    rowid_to_column("eid") %>%
    mutate(game_type = if_else(str_sub(text, -5) == "eason", text, NA_character_)) %>%
    fill(game_type) %>%
    mutate(date = str_sub(text, 1, str_locate(text, "\t")[,1] - 1),
           date = paste(str_sub(date, 6), season, sep = ", "),
           date = mdy(date),
           month = month(date),
           date = if_else(month >= 11, date - years(1), date)) %>%
    drop_na() %>%
    select(eid, date, game_type)
  
  # extract table items from each game
  game_results <- 
    games %>%
    mutate(results = map(eid, ~extract_game_result(.x, elements))) %>%
    unnest(results) %>%
    select(-eid)
  
  return(game_results)
  
}

extract_game_result <- function(eid, elements) {
  
  row_elements <- html_elements(elements[eid], ".Table__TD")[2:3]
  
  # extract links
  opponent_link <-
    row_elements[1] %>%
    html_elements("a") %>%
    html_attr("href") %>%
    unique()
  
  # (this will be converted to game_id later)
  game_link <-
    row_elements[2] %>%
    html_elements("a") %>%
    html_attr("href")
  
  # extract text
  opponent_text <- html_text2(row_elements[1])
  result_text <- html_text2(row_elements[2])
  
  # convert in the event that anything is missing
  opponent_link <- if (length(opponent_link) == 0) "missing" else opponent_link
  game_link <- if (length(game_link) == 0) "missing" else game_link
  opponent_text <- if (length(opponent_text) == 0) "missing" else opponent_text
  result_text <- if (length(result_text) == 0) "missing" else result_text
  
  out <-
    tibble(
      opponent_link = opponent_link,
      game_link = game_link,
      opponent_text = html_text2(row_elements[1]),
      result_text = html_text2(row_elements[2])
    )
  
  return(out)
  
}

# scrape ! ---------------------------------------------------------------------

# full list of schedules to scrape
items <- 
  arrow::read_parquet("data/teams/teams.parquet") %>%
  distinct(league, team_id) %>%
  crossing(season = 2002:2024)

# process results in parallel
plan(multisession, workers = 8)

# scrape!
for (league in c("mens", "womens")) {
  
  for (season in 2002:2024) {
    
    # rename variables for filtering
    league_int <- league
    season_int <- season
    
    # anti-join to remove any pre-collected items
    
    # break up season scrapes into 200-unit "chunks"
    chunks <- 
      items %>%
      filter(league == league_int,
             season == season_int) %>%
      rowid_to_column("chunk") %>%
      mutate(chunk = ceiling(chunk/200))
    
    # total number of chunks to evaluate
    n_chunks <- max(chunks$chunk)
    
    # evaluate each chunk separately
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
      
      # scrape the schedules for the specified chunk
      game_chunks <- 
        chunks %>%
        filter(chunk == chunk_int) %>%
        mutate(results = future_pmap(list(league, team_id, season),
                                     scrape_games,
                                     .progress = TRUE)) %>%
        select(-chunk) %>%
        unnest(results)
      
      # save intermittent results
      game_chunks %>%
        append_parquet(glue::glue("data/games/{league}-{season}.parquet"))
      
      # break between chunk scrapes
      sys_sleep(5)
      
    }
    
    # break between season scrapes
    sys_sleep(10)
    
  }
  
  # break between league scrapes
  sys_sleep(30)
  
}

plan(sequential)

# wrangle ----------------------------------------------------------------------

# open the dataset
tibble(file = list.files("data/games", full.names = TRUE)) %>%
  filter(file != "data/games/games.parquet") %>%
  mutate(games = map(file, arrow::read_parquet)) %>%
  unnest(games) %>%

  # extract the game_id from the game link
  mutate(game_id = str_remove_all(game_link, "https://www.espn.com/|womens-|mens-|college-basketball/game/_/gameId/"),
         game_id = str_sub(game_id, 1, str_locate(game_id, "/")[,1] - 1)) %>%
  
  # de-duplicate games
  distinct(league,
           date,
           game_id, 
           .keep_all = TRUE) %>%

  # join in source team name
  left_join(arrow::read_parquet("data/teams/teams.parquet") %>% 
              distinct(league, team_id, team_name),
            by = c("league", "team_id")) %>%
  
  # determine whether (or not) the source team is home or away
  # "vs" indicates home, "@" indicates away (i.e., Tulsa vsOSU or Tulsa @OSU)
  mutate(home = str_sub(opponent_text, 1, 2) == "vs") %>%
  
  # extract opponent team's name
  # removes rank from opponent team, if applicable
  # removes neutral site indicator, if applicable
  mutate(opponent_name = if_else(home,
                                 str_sub(opponent_text, 3),
                                 str_sub(opponent_text, 2)),
         opponent_name = if_else(is.na(parse_number(str_sub(opponent_name, 1, 1))),
                                 opponent_name,
                                 str_sub(opponent_name, str_locate(opponent_name, " ")[,1] + 1)),
         opponent_name = str_remove(opponent_name, " \\*|\\*")) %>%
  
  # determine whether (or not) the game was played on neutral territory
  mutate(neutral = str_detect(opponent_text, " \\*|\\*")) %>%
  
  # determine whether (or not) the game went to overtime & the number of overtimes
  mutate(ot = str_detect(result_text, "OT"),
         n_ot = str_sub(result_text, -3, -3),
         n_ot = case_when(!ot ~ 0,
                          n_ot == " " ~ 1,
                          .default = as.numeric(n_ot))) %>%
  
  # determine whether (or not) the source team won/lost
  mutate(win = str_sub(result_text, 1, 1) == "W") %>%
  
  # extract the winning/losing score from the results text
  mutate(win_score = as.numeric(str_sub(result_text, 2, str_locate(result_text, "-")[,1] - 1)),
         lose_score = str_sub(result_text, str_locate(result_text, "-")[,1] + 1),
         lose_score = if_else(ot,
                              str_sub(lose_score, 1, str_locate(lose_score, " ")[,1] - 1),
                              lose_score),
         lose_score = as.numeric(lose_score)) %>%
  
  # assign winner/loser scores to source/opponent team based on win flag
  mutate(team_score = if_else(win, win_score, lose_score),
         opponent_score = if_else(win, lose_score, win_score)) %>%
  
  # extract the opponent's team_id from the opponent link
  mutate(opponent_id = str_remove_all(opponent_link, "/mens-|/womens-|college-basketball/team/_/id/"),
         opponent_id = str_sub(opponent_id, 1, str_locate(opponent_id, "/")[,1] - 1)) %>%
  
  # assign home/away parameters based on whether (or not) the source team is the home team
  mutate(home_id = if_else(home, team_id, opponent_id),
         home_name = if_else(home, team_name, opponent_name),
         home_score = if_else(home, team_score, opponent_score),
         away_id = if_else(home, opponent_id, team_id),
         away_name = if_else(home, opponent_name, team_name),
         away_score = if_else(home, opponent_score, team_score)) %>%
  
  # remove canceled or rescheduled games
  filter(!is.na(game_id),
         !is.na(home_score),
         !is.na(away_score),
         home_score > 0,
         away_score > 0) %>%
  
  # join teams with n/a ids but that exist in the db
  left_join(arrow::read_parquet("data/teams/teams.parquet") %>%
              distinct(team_name, league, missing_id = team_id),
            by = c("home_name" = "team_name", "league")) %>%
  mutate(home_id = if_else(is.na(home_id), missing_id, home_id)) %>%
  select(-missing_id) %>%
  left_join(arrow::read_parquet("data/teams/teams.parquet") %>%
              distinct(team_name, league, missing_id = team_id),
            by = c("away_name" = "team_name", "league")) %>%
  mutate(away_id = if_else(is.na(away_id), missing_id, away_id)) %>%
  
  # write relevant cols to disk
  select(league,
         season,
         date,
         game_id,
         game_type,
         neutral,
         n_ot,
         home_id,
         home_name,
         home_score,
         away_id,
         away_name,
         away_score) %>%
  arrow::write_parquet("data/games/games.parquet")



