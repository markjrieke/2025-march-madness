# dev --- needs p_advance from bracket output
p_advance %>% 
  select(team_name, 
         round, 
         p_advance) %>% 
  pivot_wider(names_from = round, 
              values_from = p_advance, 
              names_prefix = "round_") %>% 
  arrange(desc(round_6), 
          desc(round_5), 
          desc(round_4), 
          desc(round_3), 
          desc(round_2), 
          desc(round_1)) %>% 
  gt() %>% 
  gt_theme_538() %>%
  cols_label(team_name = "",
             round_1 = "Round of 32",
             round_2 = "Sweet 16",
             round_3 = "Elite 8",
             round_4 = "Final 4",
             round_5 = "Championship",
             round_6 = "Win Tournament") %>%
  cols_align(align = "center",
             columns = starts_with("round")) %>%
  fmt_percent(starts_with("round"), 
              decimals = 0) %>% 
  data_color(starts_with("round"), 
             palette = "PuBu", 
             domain = c(0, 1)) %>%
  opt_interactive(use_sorting = TRUE,
                  use_search = TRUE,
                  use_highlight = TRUE)
