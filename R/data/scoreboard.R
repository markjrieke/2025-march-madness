# setup ------------------------------------------------------------------------

# libraries
library(tidyverse)
library(httr2)
library(furrr)

# overwrite api call from hoopR/wehoop
espn_api_scoreboard <- function(date_str,
                                league = "mens") {
  
  # build api call url
  url <-
    glue::glue(
      "http://site.api.espn.com/apis/site/v2/sports/basketball/{league}-college-basketball/scoreboard?dates={date_str}"
    )
  
  # ping ping!
  resp <-
    request(url) %>%
    req_perform()
  
  # json-ify (yuck)
  resp_json <- 
    resp %>%
    resp_body_json() %>%
    jsonlite::toJSON() %>%
    jsonlite::fromJSON(simplifyDataFrame = FALSE,
                       simplifyVector = FALSE,
                       simplifyMatrix = FALSE)
  
  # get correct date (not in events)
  game_date <-
    ymd(resp_json[["day"]]$date[[1]][1])
  
  # convert to tibble
  resp_tibble <-
    resp_json %>%
    pluck("events") %>%
    tibble(data = .) %>%
    unnest_wider(data)
    
  # verify that games were played that day
  if (!("competitions" %in% colnames(resp_tibble))) {
    return(NULL)
  }
  
  # parse game data
  out <- 
    resp_tibble %>%
    unchop(competitions) %>%
    select(competitions) %>%
    unnest_wider(competitions) %>%
    select(id,
           status,
           neutral_site = neutralSite,
           competitors) %>%
    unnest_wider(status) %>%
      
    # extract nested cols from competitions
    unnest(c(id, period, neutral_site)) %>%
    mutate(id = map_chr(id, ~.x[1]),
           neutral_site = map_lgl(neutral_site, ~.x[1]),
           periods = map_int(period, ~.x[1])) %>%
    rename(game_id = id) %>%
    
    # get competitors into a tibble format
    unnest(competitors) %>%
    select(game_id,
           periods,
           neutral_site,
           competitors) %>%
    unnest_wider(competitors) %>%
    select(game_id,
           team_id = id,
           home_away = homeAway,
           neutral_site,
           periods,
           team,
           score) %>%
    
    # extract nested cols from competitors
    unnest(c(team_id, home_away, score)) %>%
    mutate(across(c(team_id, home_away, score), ~map_chr(.x, ~.x[1])),
           across(c(team_id, score), as.integer)) %>%
    unnest_wider(team) %>%
      
    # add in game date, get table into final format
    mutate(date = game_date) %>%
    select(game_id,
           date,
           periods,
           team_id,
           home_away,
           neutral_site,
           location,
           score,
           name,
           display_name = displayName,
           short_display_name = shortDisplayName) %>%
    
    # extract nested cols from team
    unnest(c(location, name, display_name, short_display_name)) %>%
    mutate(across(c(location, name, display_name, short_display_name),
                  ~map_chr(.x, ~.x[1])))
  
  # write to disk
  season <- str_sub(date_str, 1, 4)
  
  if (!dir.exists(paste0("data/scoreboard/", league))) {
    dir.create(paste0("data/scoreboard/", league))
  }
  
  if (!dir.exists(paste0("data/socreboard/", league, "/", season))) {
    dir.create(paste0("data/scoreboard/", league, "/", season))
  }
  
  out %>%
    write_csv(paste0("data/scoreboard/", league, "/", season, "/", league, "-", date_str, ".csv"))
  
}

# pull scores ------------------------------------------------------------------

# import scores from the api
plan(multisession, workers = 8)

crossing(league = c("mens", "womens"),
         game_date = seq.Date(from = mdy("10/1/2001"),
                              to = mdy("10/1/2024"),
                              by = "day")) %>%
  mutate(dateid = as.character(game_date),
         dateid = str_remove_all(dateid, "-")) %>%
  select(dateid, league) %>%
  as.list() %>%
  future_pwalk(~espn_api_scoreboard(..1, ..2),
               .progress = TRUE)

plan(sequential)





