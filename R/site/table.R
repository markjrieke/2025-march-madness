#' Generate an interactive March Madness summary table
#' 
#' @description
#' Generates a `gt` table that summarizes the tournament for the specified 
#' league on the specified day. Teams that have advanced are marked with a ✓ and
#' teams that have been eliminated are marked with a ✗. Each team's probability
#' of advancement to each round is also displayed.
#' 
#' @param league Which league to generate a plot for. Either "mens" or "womens".
#' @param date The date in the tournament to generate a table for.
#' @param ... Unused.
#' @param col_win Cell color for teams that have won/advanced to a round.
#' @param col_lose Cell color for teams that have been eliminated.
#' @param pal A length-2 vector containing the lower/upper bounds of a color 
#'        scale that the probability of advancement is interpolated between.
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
  
  # read in data
  p_advance <-
    arrow::read_parquet("out/bracket/p_advance.parquet") %>%
    filter(league == league_int,
           date == date_int)
  
  teams <- 
    p_advance %>%
    distinct(tid, team_name)
  
  # build the summary table!
  table <- 
    p_advance %>%
    bind_rows(tibble(league = league,
                     date = date,
                     tid = teams$tid,
                     team_name = teams$team_name,
                     round = 0,
                     status = "Won",
                     p_advance = 1),
              .) %>%
    
    # route pids through the tournament
    rowid_to_column() %>%
    mutate(init = if_else(round == min(round), rowid, 0)) %>%
    nest(data = -team_name) %>%
    mutate(pid = map(data, ~.x$init),
           pid = map(pid, route_pid)) %>%
    unnest(c(data, pid)) %>%
    select(-init) %>%
    
    # update statuses for eliminated teams mid-round
    group_by(round, pid) %>%
    mutate(status = if_else(!(all(is.na(status)) | !is.na(status)), "Eliminated", status)) %>%
    ungroup() %>%
      
    # only display probabilities for future games
    group_by(team_name) %>%
    mutate(status_proj = status) %>%
    fill(status_proj) %>%
    ungroup() %>%
    mutate(status = if_else(status_proj == "Eliminated", status_proj, status)) %>%
    filter(round > 0) %>%
  
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

#' Override the cell style with a fill color if the cell meets the specified
#' status condition
#' 
#' @param table A `gt` table
#' @param fill The override fill color
#' @param p The probability column that will potentially have its style 
#'        overridden.
#' @param s The status column that is used to determine if p's style is 
#'        overridden.  
#' @param status The status condition (character) that will trigger the 
#'        override.
style_override <- function(table, fill, p, s, status) {
  
  table %>%
    tab_style(style = list(cell_fill(fill)),
              locations = cells_body(columns = {{ p }},
                                     rows = {{ s }} == status))
  
}

#' Override the cell text with new text if the cell meets the specified status
#' condition.
#' 
#' @param table A `gt` table
#' @param p The probability column that will potentially have its text 
#'        overridden.
#' @param s The status column that is used to determine if p's text is 
#'        overridden.  
#' @param status The status condition (character) that will trigger the 
#'        override.
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




