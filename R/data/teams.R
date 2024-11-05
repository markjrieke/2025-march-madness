# setup ------------------------------------------------------------------------

library(tidyverse)
library(rvest)

# scrape teams -----------------------------------------------------------------

scrape_teams <- function(league = "mens") {
  
  # build league params
  url <- glue::glue("https://www.espn.com/{league}-college-basketball/teams")
  path <- glue::glue("/{league}-college-basketball/team/_")
  
  # extract baseline elements
  team_elements <-
    read_html(url) %>%
    html_element("body") %>%
    html_elements(".pl3") %>%
    html_elements("a")
  
  # extract team ids and slugs
  teams <-
    team_elements %>%
    html_attr("href") %>%
    as_tibble() %>%
    rename(id = value) %>%
    filter(str_detect(id, path)) %>%
    mutate(id = str_remove(id, paste0(path, "/id/"))) %>%
    separate(id, c("team_id", "team_slug"), "/")

  # extract team names
  team_names <- 
    team_elements %>%
    html_elements("h2") %>%
    html_text2() %>%
    as_tibble() %>%
    rename(team_name = value)
  
  # join together
  teams <- 
    teams %>%
    bind_cols(team_names)
  
  return(teams)
    
}

# scrape & write ---------------------------------------------------------------

tibble(league = c("mens", "womens")) %>%
  mutate(data = map(league, scrape_teams)) %>%
  unnest(data) %>%
  write_csv("data/teams/teams.csv")



