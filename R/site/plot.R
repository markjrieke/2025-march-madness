library(tidyverse)

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
  expand_limits(y = c(0, 13)) +
  theme_void()
