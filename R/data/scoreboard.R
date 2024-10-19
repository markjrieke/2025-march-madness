library(tidyverse)
library(httr2)

base_url <- "http://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/scoreboard"
date_str <- "20240321"
url <- paste(base_url, date_str, sep = "?dates=")

resp <- 
  request(url) %>%
  req_perform()

resp_json <- 
  resp %>%
  resp_body_json() %>%
  jsonlite::toJSON() %>%
  jsonlite::fromJSON(simplifyDataFrame = FALSE,
                     simplifyVector = FALSE,
                     simplifyMatrix = FALSE)

game_date <-
  ymd(resp_json[["day"]]$date[[1]][1])

# includes teams, scores, periods, and location for each game
resp_json %>%
  pluck("events") %>%
  tibble(data = .) %>%
  unnest_wider(data) %>%
  unchop(competitions) %>%
  select(competitions) %>%
  unnest_wider(competitions) %>%
  select(id,
         status,
         neutral_site = neutralSite,
         competitors) %>%
  unnest_wider(status) %>%
  unnest(c(id, period, neutral_site)) %>%
  mutate(id = map_chr(id, ~.x[1]),
         neutral_site = map_lgl(neutral_site, ~.x[1]),
         periods = map_int(period, ~.x[1])) %>%
  rename(game_id = id) %>%
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
  unnest(c(team_id, home_away, score)) %>%
  mutate(across(c(team_id, home_away, score), ~map_chr(.x, ~.x[1])),
         across(c(team_id, score), as.integer)) %>%
  unnest_wider(team) %>%
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
         abbreviation,
         display_name = displayName,
         short_display_name = shortDisplayName) %>%
  unnest(c(location, name, abbreviation, display_name, short_display_name)) %>%
  mutate(across(c(location, name, abbreviation, display_name, short_display_name),
                ~map_chr(.x, ~.x[1])))


