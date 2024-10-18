library(tidyverse)
library(httr2)

base_url <- "http://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/scoreboard"
date_str <- "20231111"
url <- paste(base_url, date_str, sep = "?dates=")

resp <- 
  request(url) %>%
  req_perform()

# this gets everything _except_ for the number of periods played
resp %>%
  resp_body_json() %>%
  jsonlite::toJSON() %>%
  jsonlite::fromJSON(simplifyDataFrame = FALSE,
                     simplifyVector = FALSE,
                     simplifyMatrix = FALSE) %>%
  pluck("events") %>%
  tibble(data = .) %>%
  unnest_wider(data) %>%
  unchop(competitions) %>%
  select(competitions) %>%
  unnest_wider(competitions) %>%
  select(id,
         date,
         status,
         neutral_site = neutralSite,
         competitors) %>%
  unnest(c(id, date, neutral_site)) %>%
  mutate(across(c(id, date), ~map_chr(.x, ~.x[1])),
         neutral_site = map_lgl(neutral_site, ~.x[1]),
         date = ymd_hm(date),
         date = as_date(date)) %>%
  rename(game_id = id) %>%
  unnest(competitors) %>%
  unnest_wider(competitors) %>%
  select(game_id,
         date,
         status,
         team_id = id,
         home_away = homeAway,
         neutral_site,
         team,
         score) %>%
  unnest(c(team_id, home_away, score)) %>%
  mutate(across(c(team_id, home_away, score), ~map_chr(.x, ~.x[1])),
         across(c(team_id, score), as.integer)) %>%
  unnest_wider(team) %>%
  select(game_id,
         date,
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


