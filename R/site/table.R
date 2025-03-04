generate_html_table <- function(league,
                                date,
                                ...,
                                col_win = "#5a9282",
                                col_lose = "#d3d3d3",
                                pal = c("white", "#838cf1")) {
  
  cli::cli_h1(glue::glue("Generating {league} summary table for {scales::label_date('%b %d, %Y')(date)}"))
  
  # rename variables for internal filtering
  league_int <- league
  date_int <- date
  
  # # read in data
  # p_advance <- 
  #   arrow::read_parquet("out/bracket/p_advance.parquet") %>%
  #   filter(league == league_int,
  #          date == date_int)
  
  # temporarily use a tmp.parquet file
  p_advance <- arrow::read_parquet("tmp.parquet")
  
  # build the summary table!
  table <- 
    p_advance %>%
    select(team_name, 
           round, 
           s = status,
           p = p_advance) %>% 
    pivot_wider(names_from = round, 
                values_from = c(p, s), 
                names_prefix = "round_") %>%

    # enforce most likely winner at top
    arrange(desc(p_round_6), 
            desc(p_round_5), 
            desc(p_round_4), 
            desc(p_round_3), 
            desc(p_round_2), 
            desc(p_round_1)) %>%
    
    # propagate "eliminated" status for teams that are eliminated
    check_previous_round(s_round_2, s_round_1) %>% 
    check_previous_round(s_round_3, s_round_2) %>%
    check_previous_round(s_round_4, s_round_3) %>%
    check_previous_round(s_round_5, s_round_4) %>%
    check_previous_round(s_round_6, s_round_5) %>%
    
    # table-ify
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
    
    # override color with col_win for teams that have won
    style_override(col_win, p_round_1, s_round_1, "Won") %>%
    style_override(col_win, p_round_2, s_round_2, "Won") %>%
    style_override(col_win, p_round_3, s_round_3, "Won") %>%
    style_override(col_win, p_round_4, s_round_4, "Won") %>%
    style_override(col_win, p_round_5, s_round_5, "Won") %>%
    style_override(col_win, p_round_6, s_round_6, "Won") %>%
    
    # override color with col_lose for teams that have lost
    style_override(col_lose, p_round_1, s_round_1, "Eliminated") %>%
    style_override(col_lose, p_round_2, s_round_2, "Eliminated") %>%
    style_override(col_lose, p_round_3, s_round_3, "Eliminated") %>%
    style_override(col_lose, p_round_4, s_round_4, "Eliminated") %>%
    style_override(col_lose, p_round_5, s_round_5, "Eliminated") %>%
    style_override(col_lose, p_round_6, s_round_6, "Eliminated") %>%
    
    # override probability text with ✓ for teams that have won
    text_override(p_round_1, s_round_1, "Won") %>%
    text_override(p_round_2, s_round_2, "Won") %>%
    text_override(p_round_3, s_round_3, "Won") %>%
    text_override(p_round_4, s_round_4, "Won") %>%
    text_override(p_round_5, s_round_5, "Won") %>%
    text_override(p_round_6, s_round_6, "Won") %>%
    
    # override probability text with ✗ for teams that have lost
    text_override(p_round_1, s_round_1, "Eliminated") %>%
    text_override(p_round_2, s_round_2, "Eliminated") %>%
    text_override(p_round_3, s_round_3, "Eliminated") %>%
    text_override(p_round_4, s_round_4, "Eliminated") %>%
    text_override(p_round_5, s_round_5, "Eliminated") %>%
    text_override(p_round_6, s_round_6, "Eliminated") %>%
    
    # add interactivity, footer
    opt_interactive(use_sorting = TRUE,
                    use_search = TRUE,
                    use_highlight = TRUE) %>%
    tab_footnote(md(glue::glue("Chances of each team {color_text('**advancing to the next round**', pal[2])}", 
                               "Team has {color_text('**advanced to this round**', col_win)}",
                               "Team was {color_text('**eliminated**', col_lose)}",
                               .sep = "\n\n")))
  
  # save full rds
  # (html output is fundamentally bjorked for some reason?)
  table %>%
    write_rds(glue::glue("site/table/{league}-{date}.rds"))
  
}

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




