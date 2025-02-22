library(tidyverse)
library(gt)
library(gtExtras)
library(riekelib)

p_advance <- arrow::read_parquet("tmp.parquet")

col_win <- "#5A9282"
col_lose <- "#D3D3D3"
pal <- c("white", "#838CF1")

check_previous_round <- function(data, round, previous) {
  
  data %>%
    mutate("{{ round }}" := if_else({{ previous }} == "Eliminated", "Eliminated", {{ round }}))
  
}

style_override <- function(table, fill, p, s, status) {
  
  table %>%
    tab_style(style = list(cell_fill(fill)),
              locations = cells_body(columns = {{ p }},
                                     rows = {{ s }} == status))
  
}

text_override <- function(table, p, s, status) {
  
  if (status == "Won") {
    override <- "✓"
  } else {
    override <- "✗"
  }
  
  out <-
    table %>%
    text_transform(
      fn = function(x) {override},
      locations = cells_body(
        columns = {{ p }},
        rows = {{ s }} == status
      )
    )
  
  return(out)
  
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
  data_color(starts_with("p_round"), 
             palette = pal, 
             domain = c(0, 1)) %>%
  style_override(col_win, p_round_1, s_round_1, "Won") %>%
  style_override(col_win, p_round_2, s_round_2, "Won") %>%
  style_override(col_win, p_round_3, s_round_3, "Won") %>%
  style_override(col_win, p_round_4, s_round_4, "Won") %>%
  style_override(col_win, p_round_5, s_round_5, "Won") %>%
  style_override(col_win, p_round_6, s_round_6, "Won") %>%
  style_override(col_lose, p_round_1, s_round_1, "Eliminated") %>%
  style_override(col_lose, p_round_2, s_round_2, "Eliminated") %>%
  style_override(col_lose, p_round_3, s_round_3, "Eliminated") %>%
  style_override(col_lose, p_round_4, s_round_4, "Eliminated") %>%
  style_override(col_lose, p_round_5, s_round_5, "Eliminated") %>%
  style_override(col_lose, p_round_6, s_round_6, "Eliminated") %>%
  text_override(p_round_1, s_round_1, "Won") %>%
  text_override(p_round_2, s_round_2, "Won") %>%
  text_override(p_round_3, s_round_3, "Won") %>%
  text_override(p_round_4, s_round_4, "Won") %>%
  text_override(p_round_5, s_round_5, "Won") %>%
  text_override(p_round_6, s_round_6, "Won") %>%
  text_override(p_round_1, s_round_1, "Eliminated") %>%
  text_override(p_round_2, s_round_2, "Eliminated") %>%
  text_override(p_round_3, s_round_3, "Eliminated") %>%
  text_override(p_round_4, s_round_4, "Eliminated") %>%
  text_override(p_round_5, s_round_5, "Eliminated") %>%
  text_override(p_round_6, s_round_6, "Eliminated") %>%
  opt_interactive(use_sorting = TRUE,
                  use_search = TRUE,
                  use_highlight = TRUE) %>%
  tab_footnote(md(glue::glue("Chances of each team {color_text('**advancing to the next round**', pal[2])}", 
                             "Team has {color_text('**advanced to this round**', col_win)}",
                             "Team was {color_text('**eliminated**', col_lose)}",
                             .sep = "\n\n")))
