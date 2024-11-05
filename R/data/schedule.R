# setup ------------------------------------------------------------------------

library(tidyverse)
library(rvest)
library(furrr)

# scrape schedules -------------------------------------------------------------

scrape_schedule <- function(league, date) {
  
  # build url
  url <- glue::glue("https://www.espn.com/{league}-college-basketball/schedule/_/date/{date}")
  
  # extract game_ids
  games <- 
    read_html(url) %>%
    html_elements(".teams__col") %>%
    html_elements("a") %>%
    html_attr("href")
  
  Sys.sleep(1)
  
  return(games)
  
}

# scrape & write ---------------------------------------------------------------

# process results in parallel
plan(multisession, workers = 8)

schedule <- 
  crossing(league = c("mens", "womens"),
           date = seq.Date(from = mdy("11/1/2001"), 
                           to = mdy("4/30/2024"),
                           by = "day")) %>%
  mutate(month = month(date)) %>%
  filter(!month %in% 5:10) %>%
  select(-month) %>%
  mutate(date = str_remove_all(as.character(date), "-")) %>%
  mutate(game_id = future_pmap(list(league, date), 
                               ~scrape_schedule(..1, ..2),
                               .progress = TRUE))

plan(sequential)


