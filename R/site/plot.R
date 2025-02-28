library(tidyverse)
library(ggiraph)
library(patchwork)

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
  arrange(tid) %>%
  left_join(read_csv("data/manual/team-manual.csv") %>%
              filter(league == "mens"))

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
  left_join(teams %>% select(c(team_name, team_data_id, team_display))) %>%
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
  bind_rows(tibble(league = "mens",
                   date = Sys.Date(),
                   tid = teams$tid,
                   team_name = teams$team_name,
                   round = 0,
                   status = "Won",
                   p_advance = 1,
                   team_data_id = teams$team_data_id),
            .) %>%
  
  # only display probabilities for future games
  group_by(team_name) %>%
  mutate(status_proj = status) %>%
  fill(status_proj) %>%
  filter(status_proj == "Won" | round == 6) %>%
  ungroup() %>%
  mutate(p_advance_text = case_when(is.na(status) & status_proj == "Won" ~ p_advance, 
                                    round == 6 & status_proj == "Eliminated" ~ p_advance,
                                    .default = NA),
         p_advance_text = case_when(round == 6 & status_proj == "Eliminated" ~ "0%",
                                    p_advance_text > 0.99 ~ ">99%",
                                    p_advance_text < 0.01 ~ "<1%",
                                    .default = scales::label_percent(accuracy = 1)(p_advance_text))) %>%

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
         hy = eval_y(round + 1, pid, lhs),
         hyend = hy) %>%
  
  # vertical segment structure
  group_by(team_data_id) %>%
  mutate(vx = hxend,
         vxend = vx,
         vy = case_when(round >= 5 ~ NA,
                        lead(status_proj, default = "Eliminated") == "Eliminated" ~ NA,
                        .default = eval_y(round + 1, pid, lhs)),
         vyend = case_when(round >= 5 ~ NA,
                           n() == 2 ~ NA,
                           lead(status_proj, default = "Eliminated") == "Eliminated" ~ NA,
                           .default = eval_y(round + 2, increment_pid(pid), lhs)),
         linewidth_lead = (max_linewidth - min_linewidth) * lead(p_advance) + min_linewidth) %>%
  ungroup()

# winbox items
winbox <- 
  advance_structure %>%
  filter(round == 6) %>%
  select(team_name, team_data_id) %>%
  left_join(arrow::read_parquet("data/teams/teams.parquet") %>%
              filter(league == "mens")) %>%
  left_join(arrow::read_parquet("data/images/mens-images.parquet"))

# regional text (ALRIGHT YOU'LL HAVE TO MANUALLY REARRANGE THIS ONCE THE BRACKET LAUNCHES)
regional_text <- 
  tibble(
    mens = c("SOUTH", "WEST", "EAST", "MIDWEST"),
    womens = c("BRIDGEPORT", "LEXINGTON", "SIOUX FALLS", "KANSAS CITY"),
    x = c(3, 3, 10, 10),
    y = c(24.5, 8.5, 24.5, 8.5)
  )

bracket_header <- 
  tibble(x = 1:13 - 0.5,
         y = 0.5,
         label = c("Round of 64", "Round of 32", "Sweet 16", "Elite 8", "Final 4", "",
                   "Championship",
                   "", "Final 4", "Elite 8", "Sweet 16", "Round of 32", "Round of 64")) %>%
  ggplot() + 
  geom_text(aes(x = x,
                y = y,
                label = label),
            family = "IBM Plex Sans",
            size = 2.4,
            color = "gray60") +
  annotate(geom = "segment",
           x = 0,
           xend = 13,
           y = 0.3,
           yend = 0.3,
           color = "gray60") + 
  annotate(geom = "segment",
           x = 0,
           xend = 13,
           y = 0.7,
           yend = 0.7,
           color = "gray60") + 
  theme_void()

bracket_plot <- 
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
  geom_text(data = regional_text,
            mapping = aes(x = x,
                          y = y,
                          label = mens),
            color = "gray80",
            family = "IBM Plex Sans",
            fontface = "bold",
            size = 3.5) + 
  annotate(geom = "text",
           x = 6.5,
           y = 30,
           label = "Hover over each\nteam to see their \nchances of advancing.",
           family = "IBM Plex Sans",
           size = 3,
           color = "gray80") +
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
                                         linewidth = linewidth_lead,
                                         data_id = team_data_id),
                           lineend = "square") +
  geom_text_interactive(data = structure,
                        mapping = aes(x = tx,
                                      y = ty,
                                      label = team_display,
                                      hjust = hjust,
                                      data_id = team_data_id),
                        family = "IBM Plex Sans",
                        size = 2.7) + 
  geom_text_interactive(data = advance_structure,
                        mapping = aes(x = if_else(lhs, hx + 0.5, hx - 0.5),
                                      y = hy + 0.5,
                                      label = p_advance_text,
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
                        y = 26,
                        family = "IBM Plex Sans",
                        fontface = "bold",
                        size = 6) + 
  scale_linewidth_identity() + 
  scale_size_identity() + 
  expand_limits(y = c(0, 13)) +
  guides(color = "none") + 
  theme_void()

ggobj <- 
  (bracket_header / bracket_plot) +
  plot_layout(heights = c(1, 34))

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
  read_lines("js/plot.js") %>%
  str_c(collapse = "\n")

pngs <- 
  paste0('<image x="286.87" y="174.8" width="69.53" height="69.53" xlink:href="', winbox$image, '" data-id="', winbox$team_data_id, '" />') %>%
  str_c(collapse = "\n")

styled_teams <- paste0("changeStyle(\"", teams$team_data_id, "\");\n")
styled_teams <- str_c(styled_teams, collapse = "")

script <- paste("<script>", js, styled_teams, "</script>", sep = "\n")

html_aug <- c(html_intro, pngs, html_outro, script)
html_aug <- str_c(html_aug, collapse = "\n")

html_aug %>%
  write_lines("tmp.html")

unlink(path)


