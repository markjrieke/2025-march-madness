library(rvest)

url <- "https://www.espn.com/mens-college-basketball/bracket/_/season/2023"

bracket <- read_html(url)

# region setup -----------------------------------------------------------------

# four region overlay
four_region <- 
  bracket %>%
  html_element(".BracketWrapper") %>%
  html_children() %>%
  html_children() %>%
  html_children() %>%
  html_children()

# left-to-right in ascending tournament order
south <- 
  four_region %>%
  pluck(1) %>%
  html_children() %>%
  pluck(1) %>%
  html_children() %>%
  pluck(2) %>%
  html_children()

east <- 
  four_region %>%
  pluck(3) %>%
  html_children() %>%
  pluck(1) %>%
  html_children() %>%
  pluck(2) %>%
  html_children()

# right-to-left in ascending tournament order
midwest <- 
  four_region %>%
  pluck(1) %>%
  html_children() %>%
  pluck(2) %>%
  html_children() %>%
  pluck(2) %>%
  html_children()

west <- 
  four_region %>%
  pluck(3) %>%
  html_children() %>%
  pluck(2) %>%
  html_children() %>%
  pluck(2) %>%
  html_children()

# first round games ------------------------------------------------------------

# later, I'll need to deal with non-complete rounds
extract_game_ids <- function(x) {
  
  x %>%
    html_children() %>%
    html_elements(".BracketMatchup") %>%
    html_elements("a") %>%
    html_attr("href") %>%
    str_remove_all("/mens-college-basketball/game/_/gameId/") %>%
    map_chr(~str_sub(.x, 1, str_locate(.x, "/")[,1] - 1))
  
}

south_1r <- 
  south %>%
  pluck(1) %>%
  extract_game_ids()

east_1r <- 
  east %>%
  pluck(1) %>% 
  extract_game_ids()

midwest_1r <- 
  midwest %>%
  pluck(4) %>%
  extract_game_ids() 

west_1r <- 
  west %>%
  pluck(4) %>%
  extract_game_ids()

first_round <- 
  tibble(game_id = c(south_1r, east_1r, midwest_1r, west_1r)) %>%
  rowid_to_column("rank")

teams <-  
  arrow::read_parquet("data/games/games.parquet") %>%
  filter(game_id %in% first_round$game_id) %>%
  select(ends_with("id")) %>%
  left_join(first_round) %>%
  arrange(rank) %>%
  select(rank, home_id, away_id) %>%
  pivot_longer(ends_with("id"),
               names_to = "location",
               values_to = "team_id") %>%
  rowid_to_column("tid") %>%
  select(tid, team_id)

beta <- 
  read_rds("out/update/team_parameters.rds") %>%
  filter(league == "mens",
         date == max(date)) %>%
  filter(team_id %in% teams$team_id) %>%
  left_join(teams) %>%
  arrange(tid)

beta_Mu <- array(dim = c(64, 3))
beta_Sigma <- array(dim = c(64, 3, 3))

for (t in 1:64) {
  
  beta_Mu[t,] <- beta$Mu[[t]]
  beta_Sigma[t,,] <- beta$Sigma[[t]]
  
}

wid <- array(0, dim = c(64, 7))
wid[,1] <- teams$tid
