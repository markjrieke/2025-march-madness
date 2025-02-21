library(gt)
library(gtExtras)

p_advance <- arrow::read_parquet("tmp.parquet")

checkmark_results <- function(x, s) {
  
  case_match(
    s,
    "Won" ~ "✓",
    "Eliminated" ~ "-",
    .default = scales::label_percent(accuracy = 1)(p)
  )
  
}

check_previous_round <- function(data, round, previous) {
  
  data %>%
    mutate("{{round}}" := if_else({{previous}} == "Eliminated", "Eliminated", {{round}}))
  
}

# dev --- needs p_advance from bracket output
p_advance %>%
  select(team_name, 
         round, 
         s = status,
         p = p_advance) %>% 
  pivot_wider(names_from = round, 
              values_from = c(p, s), 
              names_prefix = "round_") %>% 
  arrange(desc(p_round_6), 
          desc(p_round_5), 
          desc(p_round_4), 
          desc(p_round_3), 
          desc(p_round_2), 
          desc(p_round_1)) %>%
  check_previous_round(s_round_2, s_round_1) %>% 
  check_previous_round(s_round_3, s_round_2) %>%
  check_previous_round(s_round_4, s_round_3) %>%
  check_previous_round(s_round_5, s_round_4) %>%
  check_previous_round(s_round_6, s_round_5) %>%
  gt() %>% 
  gt_theme_538() %>%
  cols_hide(starts_with("s_round")) %>%
  cols_label(team_name = "",
             p_round_1 = "Round of 32",
             p_round_2 = "Sweet 16",
             p_round_3 = "Elite 8",
             p_round_4 = "Final 4",
             p_round_5 = "Championship",
             p_round_6 = "Win Tournament") %>%
  cols_align(align = "center",
             columns = starts_with("p_round")) %>%
  fmt_percent(starts_with("p_round"), 
              decimals = 0) %>% 
  # fmt(columns = p_round_1,
  #     fns = checkmark_results(x, s = s_round_1)) %>%
  data_color(starts_with("p_round"), 
             palette = c("white", "#838CF1"), 
             domain = c(0, 1)) %>%
  tab_style(style = list(cell_fill("#5A9282")),
            locations = cells_body(columns = p_round_1,
                                   rows = s_round_1 == "Won")) %>%
  tab_style(style = list(cell_fill("#5A9282")),
            locations = cells_body(columns = p_round_2,
                                   rows = s_round_2 == "Won")) %>%
  tab_style(style = list(cell_fill("#5A9282")),
            locations = cells_body(columns = p_round_3,
                                   rows = s_round_3 == "Won")) %>%
  tab_style(style = list(cell_fill("#5A9282")),
            locations = cells_body(columns = p_round_4,
                                   rows = s_round_4 == "Won")) %>%
  tab_style(style = list(cell_fill("#5A9282")),
            locations = cells_body(columns = p_round_5,
                                   rows = s_round_5 == "Won")) %>%
  tab_style(style = list(cell_fill("#5A9282")),
            locations = cells_body(columns = p_round_6,
                                   rows = s_round_6 == "Won")) %>%
  tab_style(style = list(cell_fill()),
            locations = cells_body(columns = p_round_1,
                                   rows = s_round_1 == "Eliminated")) %>%
  tab_style(style = list(cell_fill()),
            locations = cells_body(columns = p_round_2,
                                   rows = s_round_2 == "Eliminated")) %>%
  tab_style(style = list(cell_fill()),
            locations = cells_body(columns = p_round_3,
                                   rows = s_round_3 == "Eliminated")) %>%
  tab_style(style = list(cell_fill()),
            locations = cells_body(columns = p_round_4,
                                   rows = s_round_4 == "Eliminated")) %>%
  tab_style(style = list(cell_fill()),
            locations = cells_body(columns = p_round_5,
                                   rows = s_round_5 == "Eliminated")) %>%
  tab_style(style = list(cell_fill()),
            locations = cells_body(columns = p_round_6,
                                   rows = s_round_6 == "Eliminated")) %>%
  opt_interactive(use_sorting = TRUE,
                  use_search = TRUE,
                  use_highlight = TRUE)
