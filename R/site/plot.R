library(tidyverse)
library(ggiraph)

bracket_linewidth <- 0.35
min_linewidth <- 0.5
max_linewidth <- 4

read_file <- function(path, ..., encoding = "UTF-8", warn = FALSE) {
  paste0(readLines(con = path, encoding = encoding, warn = warn, ...), collapse = "\n")
}

round_structure <- function(round) {
  
  tibble(
    round = round,
    pid = 1:(2^(7-round))
  )
  
}

eval_y <- function(round, pid, lhs) {
  
  case_when(
    round == 7 ~ 20,
    lhs ~ 33 - (2^(round - 2) + 0.5 + (pid - 1) * (2^(round - 1))),
    !lhs ~ 33 - (2^(round - 2) + 0.5 + (pid - 2^(6 - round) - 1) * (2^(round - 1)))
  )
  
}

increment_pid <- function(x) {
  
  (x + (x %% 2)) / 2
  
}

route_pid <- function(x) {
  
  if (length(x) == 1) {
    return(x)
  }
  
  for (i in 2:length(x)) {
    x[i] <- increment_pid(x[i-1])
  }
  
  return(x)
  
}

p_advance <- 
  p_advance %>%
  mutate(team_data_id = str_remove(team_name, "'"))

teams <-
  p_advance %>%
  distinct(tid, team_name, team_data_id) %>%
  arrange(tid)

# skeleton for building bracket
structure <- 
  bind_rows(
    round_structure(1),
    round_structure(2),
    round_structure(3),
    round_structure(4),
    round_structure(5),
    round_structure(6),
    round_structure(7)
  )

# join in teams that have already advanced
structure <- 
  wid0 %>%
  as_tibble() %>%
  rename_with(~str_replace(.x, "V", "round_")) %>%
  mutate(across(everything(), as.integer),
         across(everything(), ~map_chr(.x, ~if (length(teams$team_data_id[.x]) > 0) teams$team_data_id[.x] else NA_character_))) %>%
  rowid_to_column("pid") %>%
  pivot_longer(-pid,
               names_to = "round",
               values_to = "team_data_id") %>%
  drop_na() %>%
  mutate(round = parse_number(round)) %>%
  left_join(teams %>% select(c(team_name, team_data_id))) %>%
  right_join(structure)

# append with plotting coordinates
structure <- 
  structure %>%
  
  # common structures
  mutate(lhs = pid <= 2^(6 - round),
         rhs = !lhs) %>%
  
  # horizontal bracket components
  mutate(hx = case_when(lhs ~ round,
                        rhs ~ 13 - round),
         hxend = case_when(lhs ~ hx - 1,
                           rhs ~ hx + 1),
         hy = eval_y(round, pid, lhs),
         hyend = hy) %>%
  
  # vertical bracket components
  mutate(vx = case_when(round == 1 | round == 7 ~ NA,
                        lhs ~ round - 1,
                        rhs ~ 13 - round + 1),
         vxend = vx,
         vy = if_else(round == 1 | round == 7,
                      NA,
                      eval_y(round - 1, 2 * pid - 1, lhs)),
         vyend = if_else(round == 1 | round == 7,
                         NA,
                         eval_y(round - 1, 2 * pid, lhs))) %>%
  
  # text components
  mutate(tx = case_when(lhs ~ round - 0.95,
                        rhs ~ 13 - round + 0.95),
         ty = eval_y(round, pid, lhs) + 0.35,
         hjust = case_when(lhs ~ 0,
                           rhs ~ 1))

# team-level structure for advancement lines
advance_structure <-
  p_advance %>%
  
  # only display lead-lines for future games
  group_by(team_name) %>%
  filter(all(status == "Won" | is.na(status))) %>%
  ungroup() %>%
  filter(is.na(status)) %>%
  
  # common transforms and route pids through the tournament
  rowid_to_column() %>%
  mutate(init = if_else(round == min(round), rowid, 0),
         lhs = tid <= 32,
         rhs = !lhs) %>%
  nest(data = -c(team_name, team_data_id)) %>%
  mutate(pid = map(data, ~.x$init),
         pid = map(pid, route_pid)) %>%
  unnest(c(data, pid)) %>%
  select(-init) %>%
  
  # linewidth is based on probability of advancement
  mutate(linewidth = (max_linewidth - min_linewidth) * p_advance + min_linewidth) %>%
  
  # horizontal segment structure
  mutate(hx = case_when(lhs ~ round,
                        rhs ~ 13 - round),
         hxend = case_when(lhs ~ hx + 1,
                           rhs ~ hx - 1),
         hy = eval_y(round + 1, increment_pid(pid), lhs),
         hyend = hy) %>%
  
  # vertical segment structure
  mutate(vx = hx,
         vxend = vx,
         vy = eval_y(round + 1, increment_pid(pid), lhs),
         vyend = eval_y(round, pid, lhs))

# winbox items
winbox <- 
  advance_structure %>%
  filter(round == 6) %>%
  bind_cols(img = sample(imgs, nrow(.), replace = TRUE)) %>%
  select(team_name, team_data_id) %>%
  left_join(arrow::read_parquet("data/teams/teams.parquet") %>%
              filter(league == "mens")) %>%
  left_join(arrow::read_parquet("data/images/womens-images.parquet"))

ggobj <- 
  ggplot() + 
  geom_segment(data = structure,
               mapping = aes(x = hx,
                             xend = hxend,
                             y = hy,
                             yend = hyend),
               linewidth = bracket_linewidth) +
  geom_segment(data = structure ,
               mapping = aes(x = vx,
                             xend = vxend,
                             y = vy,
                             yend = vyend),
               linewidth = bracket_linewidth) +
  geom_segment_interactive(data = advance_structure,
                           mapping = aes(x = hx,
                                         xend = hxend,
                                         y = hy,
                                         yend = hyend,
                                         color = team_data_id,
                                         linewidth = linewidth,
                                         data_id = team_data_id),
                           lineend = "square") +
  geom_segment_interactive(data = advance_structure,
                           mapping = aes(x = vx,
                                         xend = vxend,
                                         y = vy,
                                         yend = vyend,
                                         color = team_data_id,
                                         linewidth = linewidth,
                                         data_id = team_data_id),
                           lineend = "square") +
  geom_text_interactive(data = structure,
                        mapping = aes(x = tx,
                                      y = ty,
                                      label = team_name,
                                      hjust = hjust,
                                      data_id = team_data_id),
                        family = "IBM Plex Sans",
                        size = 2.7) + 
  geom_text_interactive(data = advance_structure,
                        mapping = aes(x = if_else(lhs, hx + 0.5, hx - 0.5),
                                      y = hy + 0.5,
                                      label = case_when(p_advance > 0.99 ~ ">99%",
                                                        p_advance < 0.01 ~ "<1%",
                                                        .default = scales::label_percent(accuracy = 1)(p_advance)),
                                      data_id = team_data_id,
                                      color = team_data_id),
                        family = "IBM Plex Sans",
                        fontface = "bold",
                        size = 4) +
  geom_text_interactive(data = winbox,
                        mapping = aes(label = team_name,
                                      data_id = team_data_id,
                                      color = team_data_id),
                        x = 6.5,
                        y = 16.5,
                        family = "IBM Plex Sans",
                        fontface = "bold",
                        size = 6) + 
  scale_linewidth_identity() + 
  scale_size_identity() + 
  expand_limits(y = c(0, 13)) +
  guides(color = "none") + 
  theme_void()

# dev path for img
path <- tempfile()

args <- list(
  canvas_id = paste("svg", gsub("-", "_", uuid::UUIDgenerate()), sep = "_"),
  file = path,
  width = 9,
  height = 9,
  pointsize = 12,
  standalone = TRUE,
  setdims = FALSE
)

devlenth <- length(dev.list())
do.call(dsvg, args)
tryCatch({
  print(ggobj)
}, finally = {
  if (length(dev.list()) > devlenth) {
    dev.off()
  }
})

html_str <- 
  read_file(path) %>%
  str_split("\n") %>%
  pluck(1)

html_str[2] <- 
  if_else(
    str_detect(html_str[2], "width="),
    paste0(str_sub(html_str[2], 1, str_locate(html_str[2], "width=")[1] - 2), ">"),
    html_str[2]
  )

html_intro <- html_str[1:(length(html_str) - 1)]
html_outro <- "</svg>"

js <- 
  read_lines("R/site/plot.js") %>%
  str_c(collapse = "\n")

pngs <- 
  paste0('<image x="291.6" y="167.32" width="64.8" height="64.8" xlink:href="', winbox$image, '" data-id="', winbox$team_data_id, '" />') %>%
  str_c(collapse = "\n")

styled_teams <- paste0("changeStyle(\"", teams$team_data_id, "\");\n")
styled_teams <- str_c(styled_teams, collapse = "")

script <- paste("<script>", js, styled_teams, "</script>", sep = "\n")

html_aug <- c(html_intro, pngs, html_outro, script)
html_aug <- str_c(html_aug, collapse = "\n")

html_aug %>%
  write_lines("tmp.html")

unlink(path)


