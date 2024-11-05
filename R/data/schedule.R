# setup ------------------------------------------------------------------------

library(tidyverse)
library(rvest)
library(furrr)

# scrape schedules -------------------------------------------------------------

scrape_schedule <- function(league, date, sleep_time = 1) {
  
  # build url
  url <- glue::glue("https://www.espn.com/{league}-college-basketball/schedule/_/date/{date}")
  
  # extract game_ids
  games <- 
    read_html(url) %>%
    html_elements(".teams__col") %>%
    html_elements("a") %>%
    html_attr("href")
  
  Sys.sleep(sleep_time)
  
  return(games)
  
}

# scrape -----------------------------------------------------------------------

# process results in parallel
plan(multisession, workers = 8)

# breaking this up in chunks to be polite to the robots
for (league in c("mens", "womens")) {
  
  cli::cli_h1(glue::glue("{str_to_title(league)} Basketball"))
  
  for (season in 2002:2024) {
    
    cli::cli_h2(glue::glue("{season - 1}-{str_sub(as.character(season), -2)}"))
    
    # pull games between November & April
    season_start <- mdy(paste0("11/1/", season - 1))
    season_end <- mdy(paste0("4/30/", season))
    
    # scrape espn's site
    scraped <-
      tibble(league = league,
             date = seq.Date(from = season_start,
                             to = season_end,
                             by = "day")) %>%
      mutate(date = str_remove_all(as.character(date), "-")) %>%
      mutate(game_id = future_pmap(list(league, date),
                                   ~scrape_schedule(..1, ..2),
                                   .progress = TRUE))
    
    # create schedule if it doesn't exist already
    if (!exists("schedule")) {
      
      schedule <- scraped
      
    } else {
      
      schedule <-
        schedule %>%
        bind_rows(scraped)
      
    }
    
    # give 1-min break between scraping seasons
    cli::cli_progress_bar("Sleeping...", total = 60, clear = FALSE)
    for (t in 1:60) {
      Sys.sleep(1)
      cli::cli_progress_update()
    }
    
  }
  
}

plan(sequential)

# wrangle & write --------------------------------------------------------------

schedule %>% 
  mutate(n = map_int(game_id, length)) %>%
  filter(n > 0) %>%
  select(-n) %>%
  unnest(game_id) %>%
  mutate(game_id = str_remove_all(game_id, "/mens-|/womens-|college-basketball/game/_/gameId/"),
         game_id = map_chr(game_id, ~str_split_1(.x, "/")[1]),
         date = ymd(date)) %>%
  arrow::write_parquet("data/schedule/schedule.parquet")

