library(tidyverse)
library(ggiraph)

round_structure <- function(round) {
  
  tibble(
    round = round,
    pid = 1:(2^(7-round))
  )
  
}

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

structure <- 
  wid0 %>%
  as_tibble() %>%
  rename_with(~str_replace(.x, "V", "round_")) %>%
  mutate(across(everything(), as.integer),
         across(everything(), ~map_chr(.x, ~if (length(teams$team_name[.x]) > 0) teams$team_name[.x] else NA_character_))) %>%
  rowid_to_column("pid") %>%
  pivot_longer(-pid,
               names_to = "round",
               values_to = "team") %>%
  drop_na() %>%
  mutate(round = parse_number(round)) %>%
  right_join(structure)

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
  
  for (i in 2:length(x)) {
    x[i] <- increment_pid(x[i-1])
  }
  
  return(x)
  
}

p_advance %>%
  group_by(team_name) %>%
  filter(all(status == "Won" | is.na(status))) %>%
  ungroup() %>%
  filter(is.na(status)) %>%
  rowid_to_column() %>%
  mutate(init = if_else(round == min(round), rowid, 0),
         lhs = tid <= 32,
         rhs = !lhs,
         x = case_when(lhs ~ round,
                       rhs ~ 13 - round)) %>%
  nest(data = -team_name) %>%
  mutate(pid = map(data, ~.x$init),
         pid = map(pid, route_pid)) %>%
  unnest(c(data, pid)) %>%
  select(-init) %>%
  mutate(y = eval_y(round + 1, increment_pid(pid), lhs)) 


ggobj <- 
  ggplot() + 
  geom_segment(data = structure %>%
                 mutate(lhs = pid <= 2^(6 - round),
                        rhs = !lhs,
                        x = case_when(lhs ~ round,
                                      rhs ~ 13 - round),
                        xend = case_when(lhs ~ x - 1,
                                         rhs ~ x + 1),
                        y = eval_y(round, pid, lhs),
                        yend = y),
               mapping = aes(x = x,
                             xend = xend,
                             y = y,
                             yend = yend)) +
  geom_segment(data = structure %>%
                 filter(round > 1,
                        round < 7) %>%
                 mutate(lhs = pid <= 2^(6 - round),
                        rhs = !lhs,
                        x = case_when(lhs ~ round - 1,
                                      rhs ~ 13 - round + 1),
                        xend = x,
                        y = eval_y(round - 1, 2 * pid - 1, lhs),
                        yend = eval_y(round - 1, 2 * pid, lhs)),
               mapping = aes(x = x,
                             xend = xend,
                             y = y,
                             yend = yend)) +
  geom_text(data = structure %>%
              mutate(lhs = pid <= 2^(6 - round),
                     rhs = !lhs,
                     x = case_when(lhs ~ round - 0.95,
                                   rhs ~ 13 - round + 0.95),
                     y = eval_y(round, pid, lhs) + 0.35,
                     hjust = case_when(lhs ~ 0, rhs ~ 1)),
            mapping = aes(x = x,
                          y = y,
                          label = team,
                          hjust = hjust),
            family = "IBM Plex Sans",
            size = 2.7) + 
  geom_segment_interactive(data = p_advance %>%
                 group_by(team_name) %>%
                 filter(all(status == "Won" | is.na(status))) %>%
                 ungroup() %>%
                 filter(is.na(status)) %>%
                 rowid_to_column() %>%
                 mutate(init = if_else(round == min(round), rowid, 0),
                        lhs = tid <= 32,
                        rhs = !lhs,
                        x = case_when(lhs ~ round,
                                      rhs ~ 13 - round)) %>%
                 nest(data = -team_name) %>%
                 mutate(pid = map(data, ~.x$init),
                        pid = map(pid, route_pid)) %>%
                 unnest(c(data, pid)) %>%
                 select(-init) %>%
                 mutate(y = eval_y(round + 1, increment_pid(pid), lhs),
                        xend = case_when(lhs ~ x + 1,
                                         rhs ~ x - 1),
                        yend = y,
                        linewidth = 6 * p_advance),
               mapping = aes(x = x,
                             xend = xend,
                             y = y,
                             yend = yend,
                             color = team_name,
                             linewidth = linewidth,
                             data_id = team_name)) +
  geom_segment_interactive(data = p_advance %>%
                 group_by(team_name) %>%
                 filter(all(status == "Won" | is.na(status))) %>%
                 ungroup() %>%
                 filter(is.na(status)) %>%
                 rowid_to_column() %>%
                 mutate(init = if_else(round == min(round), rowid, 0),
                        lhs = tid <= 32,
                        rhs = !lhs,
                        x = case_when(lhs ~ round,
                                      rhs ~ 13 - round)) %>%
                 nest(data = -team_name) %>%
                 mutate(pid = map(data, ~.x$init),
                        pid = map(pid, route_pid)) %>%
                 unnest(c(data, pid)) %>%
                 select(-init) %>%
                 mutate(y = eval_y(round + 1, increment_pid(pid), lhs),
                        xend = x,
                        yend = eval_y(round, pid, lhs),
                        linewidth = 6 * p_advance),
               mapping = aes(x = x,
                             xend = xend,
                             y = y,
                             yend = yend,
                             color = team_name,
                             linewidth = linewidth,
                             data_id = team_name),
               alpha = 0.001) + 
  geom_point_interactive(data = p_advance %>%
               group_by(team_name) %>%
               filter(all(status == "Won" | is.na(status))) %>%
               ungroup() %>%
               filter(is.na(status)) %>%
               rowid_to_column() %>%
               mutate(init = if_else(round == min(round), rowid, 0),
                      lhs = tid <= 32,
                      rhs = !lhs,
                      x = case_when(lhs ~ round,
                                    rhs ~ 13 - round)) %>%
               nest(data = -team_name) %>%
               mutate(pid = map(data, ~.x$init),
                      pid = map(pid, route_pid)) %>%
               unnest(c(data, pid)) %>%
               select(-init) %>%
               mutate(y = eval_y(round + 1, increment_pid(pid), lhs),
                      size = 5 * p_advance),
             mapping = aes(x = x,
                           y = y,
                           size = size,
                           color = team_name,
                           data_id = team_name)) + 
  geom_point_interactive(data = p_advance %>%
               group_by(team_name) %>%
               filter(all(status == "Won" | is.na(status))) %>%
               ungroup() %>%
               filter(is.na(status)) %>%
               rowid_to_column() %>%
               mutate(init = if_else(round == min(round), rowid, 0),
                      lhs = tid <= 32,
                      rhs = !lhs,
                      x = case_when(lhs ~ round,
                                    rhs ~ 13 - round)) %>%
               nest(data = -team_name) %>%
               mutate(pid = map(data, ~.x$init),
                      pid = map(pid, route_pid)) %>%
               unnest(c(data, pid)) %>%
               select(-init) %>%
               mutate(y = eval_y(round, pid, lhs),
                      size = 5 * p_advance),
             mapping = aes(x = x,
                           y = y,
                           size = size,
                           color = team_name,
                           data_id = team_name)) +
  geom_point_interactive(data = p_advance %>%
               group_by(team_name) %>%
               filter(all(status == "Won" | is.na(status))) %>%
               ungroup() %>%
               filter(is.na(status)) %>%
               rowid_to_column() %>%
               mutate(init = if_else(round == min(round), rowid, 0),
                      lhs = tid <= 32,
                      rhs = !lhs,
                      x = case_when(lhs ~ round + 1,
                                    rhs ~ 13 - round - 1)) %>%
               nest(data = -team_name) %>%
               mutate(pid = map(data, ~.x$init),
                      pid = map(pid, route_pid)) %>%
               unnest(c(data, pid)) %>%
               select(-init) %>%
               mutate(y = eval_y(round + 1, increment_pid(pid), lhs),
                      size = 5 * p_advance),
             mapping = aes(x = x,
                           y = y,
                           size = size,
                           color = team_name,
                           data_id = team_name)) +
  scale_linewidth_identity() + 
  scale_size_identity() + 
  expand_limits(y = c(0, 13)) +
  guides(color = "none") + 
  theme_void()

opts_default <- function() {
  
  base::structure(
    list(
      css = "opacity:0;"
    ),
    class = "opts_default"
  )
  
}

girafe(
  ggobj = ggobj,
  options = list(
    opts_hover(css = "opacity:1;"),
    opts_hover_inv("opacity:0;"),
    opts_hover_theme("opacity:0;")
  )
)
