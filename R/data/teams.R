# setup ------------------------------------------------------------------------

# libraries
library(tidyverse)
library(rvest)
library(furrr)

# util webscraping functions
source("R/data/utils.R")

# scrape teams -----------------------------------------------------------------

scrape_teams <- function(league, date) {
  
  # build url
  url <- glue::glue("https://www.espn.com/{league}-college-basketball/schedule/_/date/{date}")
  
  # pink the website
  result <- retry_catch(read_html(url))
  
  # log a warning if need be
  if (!is.null(result$warning)) {
    
    tibble(league = league,
           date = ymd(date),
           warning = as.character(result$warning)) %>%
      append_parquet("data/teams/teams-warnings.parquet")
    
  }
  
  # log an error if need be
  if (!is.null(result$error)) {
    
    tibble(league = league,
           date = ymd(date),
           error = as.character(result$error)) %>%
      append_parquet("data/teams/teams-errors.parquet")
    
    # exit the function
    return()
    
  }
  
  # extract team links
  # (can only look up games for D1 teams)
  links <- 
    result$result %>%
    html_elements(".Table__Team") %>%
    html_elements("a")
  
  # construct output from links
  out <- 
    tibble(link = html_attr(links, "href"),
           team_name = html_text2(links)) %>%
    filter(team_name != "")
  
  return(out)
  
}

# scrape -----------------------------------------------------------------------

# process results in parallel
plan(multisession, workers = 8)

teams <- 
  crossing(league = c("mens", "womens"),
           date = seq.Date(from = mdy("11/1/2001"),
                           to = mdy("4/30/2024"),
                           by = "day")) %>%
  mutate(month = month(date)) %>%
  filter(!month %in% 5:10) %>%
  select(-month) %>%
  mutate(date = str_remove_all(as.character(date), "-"),
         teams = future_pmap(list(league, date),
                             ~scrape_teams(..1, ..2),
                             .progress = TRUE))

plan(sequential)

# wrangle & write --------------------------------------------------------------

teams %>%
  unnest(teams) %>%
  distinct(league, link, team_name) %>%
  arrange(link) %>% 
  mutate(link = str_remove_all(link, "/mens|/womens|-college-basketball/team/_/id/")) %>%
  separate(link, c("team_id", "team_slug"), "/") %>%
  arrow::write_parquet("data/teams/teams.parquet")



