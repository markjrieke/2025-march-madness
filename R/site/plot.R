generate_html_bracket <- function(league,
                                  date,
                                  ...,
                                  bracket_linewidth = 0.35,
                                  min_linewidth = 0.5,
                                  max_linewidth = 4) {
  
  cli::cli_h1(glue::glue("Generating {league} bracket for {scales::label_date('%b %d, %Y')(date)}"))
  
  # rename variables for internal filtering
  league_int <- league
  date_int <- date
  
  # # read in probability of advancement
  # p_advance <- 
  #   arrow::read_parquet("out/bracket/p_advance.parquet") %>%
  #   filter(league == league_int,
  #          date == date_int)
  
  # # read in tournament structure matrix
  # read_rds("out/bracket/wid0.rds") %>% 
  #   filter(league == league_int, 
  #          date == date_int) %>% 
  #   pull(wid0) %>%
  #   pluck(1)
  
  # read in manually set team html parameters
  team_html <- 
    read_csv("data/manual/team-manual.csv") %>%
    filter(league == league_int)
  
  # temporarily use tmp files
  p_advance <- arrow::read_parquet("tmp.parquet")
  wid0 <- 
    read_rds("tmp.rds") %>%
    pull(wid0) %>%
    pluck(1)
  
  # set team_data_id html identifier as team_name w/o apostrophes 
  p_advance <- 
    p_advance %>%
    mutate(team_data_id = str_remove(team_name, "'"))
  
  # set function-level team reference df
  teams <-
    p_advance %>%
    distinct(tid, team_name, team_data_id) %>%
    arrange(tid) %>%
    left_join(team_html)
  
  # data necessary for building the bracket plot
  bracket_data <-
    build_bracket_data(
      league,
      date,
      p_advance,
      teams,
      wid0
    )
  
  # the main kahuna
  ggobj <- build_bracket_ggobj(bracket_data)
  
  # save output has html
  write_bracket_html(
    league = league,
    date = date,
    ggobj = ggobj,
    teams = teams,
    winbox = bracket_data$winbox
  )
  
}

build_bracket_structure <- function(teams,
                                    wid0,
                                    ...,
                                    h_offset = 0.95,
                                    v_offset = 0.35) {
  
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
    mutate(tx = case_when(lhs ~ round - h_offset,
                          rhs ~ 13 - round + h_offset),
           ty = eval_y(round, pid, lhs) + v_offset,
           hjust = case_when(lhs ~ 0,
                             rhs ~ 1))
  
  return(structure)
  
}

build_advance_structure <- function(p_advance,
                                    teams,
                                    league,
                                    date,
                                    ...,
                                    max_linewidth = 4,
                                    min_linewidth = 0.35) {
  
  # team-level structure for advancement lines
  advance_structure <-
    p_advance %>%
    bind_rows(tibble(league = league,
                     date = date,
                     tid = teams$tid,
                     team_name = teams$team_name,
                     round = 0,
                     status = "Won",
                     p_advance = 1,
                     team_data_id = teams$team_data_id),
              .) %>%
    left_join(teams %>% select(team_name, team_color)) %>%
    
    # route pids through the tournament
    rowid_to_column() %>%
    mutate(init = if_else(round == min(round), rowid, 0)) %>%
    nest(data = -c(team_name, team_data_id)) %>%
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
    filter(status_proj == "Won" | round == 6) %>%
    ungroup() %>%
    mutate(p_advance_text = case_when(is.na(status) & status_proj == "Won" ~ p_advance, 
                                      round == 6 & status_proj == "Eliminated" ~ p_advance,
                                      .default = NA),
           p_advance_text = case_when(round == 6 & status_proj == "Eliminated" ~ "0%",
                                      p_advance_text > 0.99 ~ ">99%",
                                      p_advance_text < 0.01 ~ "<1%",
                                      .default = scales::label_percent(accuracy = 1)(p_advance_text))) %>%
    
    # common transforms
    mutate(lhs = tid <= 32,
           rhs = !lhs) %>%
    
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
  
  return(advance_structure)
  
}

build_winbox <- function(advance_structure,
                         league) {
  
  # internal renaming for filtering
  league_int <- league
  
  # get necessary components for mapping images to teams
  slugs <-
    arrow::read_parquet("data/teams/teams.parquet") %>%
    filter(league == league_int) 
  
  images <-
    arrow::read_parquet(glue::glue("data/images/{league}-images.parquet"))
  
  # join for displaying data in the winbox
  winbox <-
    advance_structure %>%
    filter(round == 6) %>%
    select(team_name, 
           team_id, 
           team_color) %>%
    left_join(slugs) %>%
    left_join(images)
  
  return(winbox)
  
}

build_regional_text <- function(league) {
  
  label <-
    if_else(
      league == "mens",
      c("SOUTH", "WEST", "EAST", "MIDWEST"),
      c("BRIDGEPORT", "LEXINGTON", "SIOUX FALLS", "KANSAS CITY")
    )
  
  regional_text <-
    tibble(
      label = label,
      x = c(3, 3, 10, 10),
      y = c(24.5, 8.5, 24.5, 8.5)
    )
  
  return(regional_text)
  
}

build_bracket_data <- function(league,
                               date,
                               p_advance,
                               teams,
                               wid0) {
  
  # lines for the bracket structure
  structure <- 
    build_bracket_structure(
      teams, 
      wid0
    )
  
  # probability of advancement mapping for undetermined games
  advance_structure <- 
    build_advance_structure(
      p_advance,
      teams,
      league,
      date
    )
  
  # items displayed on hover in the "winbox"
  winbox <-
    build_winbox(
      advance_structure,
      league
    )
  
  # text annotations for each of the bracket regions
  regional_text <- build_regional_text(league)
  
  # return result as a list
  out <-
    list(
      structure = structure,
      advance_structure = advance_structure,
      winbox = winbox,
      regional_text = regional_text
    )
  
  return(out)
  
}

build_bracket_ggobj <- function(bracket_data,
                                ...,
                                bracket_linewidth = 0.35) {
  
  # extract components from bracket data
  structure <- bracket_data$structure
  advance_structure <- bracket_data$advance_structure
  winbox <- bracket_data$winbox
  regional_text <- bracket_data$regional_text
  
  # header component listing rounds
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
  
  # main bracket component
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
                                           color = team_color,
                                           linewidth = linewidth,
                                           data_id = team_data_id),
                             lineend = "square") +
    geom_segment_interactive(data = advance_structure,
                             mapping = aes(x = vx,
                                           xend = vxend,
                                           y = vy,
                                           yend = vyend,
                                           color = team_color,
                                           linewidth = linewidth_lead,
                                           data_id = team_data_id),
                             lineend = "square") +
    geom_label_interactive(data = structure,
                           mapping = aes(x = tx,
                                         y = ty,
                                         label = team_display,
                                         hjust = hjust,
                                         data_id = team_data_id),
                           family = "IBM Plex Sans",
                           label.size = 0,
                           size = 2.7) + 
    geom_text_interactive(data = advance_structure,
                          mapping = aes(x = if_else(lhs, hx + 0.5, hx - 0.5),
                                        y = hy + 0.5,
                                        label = p_advance_text,
                                        data_id = team_data_id,
                                        color = team_color),
                          family = "IBM Plex Sans",
                          fontface = "bold",
                          size = 4) +
    geom_text_interactive(data = winbox,
                          mapping = aes(label = team_name,
                                        data_id = team_data_id,
                                        color = team_color),
                          x = 6.5,
                          y = 26,
                          family = "IBM Plex Sans",
                          fontface = "bold",
                          size = 6) + 
    scale_linewidth_identity() + 
    scale_size_identity() + 
    scale_color_identity() + 
    expand_limits(y = c(0, 13)) +
    guides(color = "none") + 
    theme_void()
  
  # arrange with patchwork
  ggobj <- 
    (bracket_header / bracket_plot) +
    plot_layout(heights = c(1, 34))
  
  return(ggobj)
  
}

write_bracket_html <- function(league,
                               date,
                               ggobj,
                               teams,
                               winbox) {
  
  # dev path for img
  path <- tempfile()
  
  # args for rendering the svg
  args <- 
    list(
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
  
  # render the ggobj to the device
  tryCatch({
    print(ggobj)
  }, finally = {
    if (length(dev.list()) > devlenth) {
      dev.off()
    }
  })
  
  # read in the svg html
  html_str <- 
    read_file(path) %>%
    str_split("\n") %>%
    pluck(1)
  
  # remove width/height constraints
  html_str[2] <- 
    if_else(
      str_detect(html_str[2], "width="),
      paste0(str_sub(html_str[2], 1, str_locate(html_str[2], "width=")[1] - 2), ">"),
      html_str[2]
    )
  
  # separate the svg into parts to inject javascript
  html_intro <- html_str[1:(length(html_str) - 1)]
  html_outro <- "</svg>"
  
  # custom javascript function that changes the opacity for 
  # elements with the same data_id on hover
  js <- 
    read_lines("js/plot.js") %>%
    str_c(collapse = "\n")
  
  # apply js function to all teams in the bracket
  styled_teams <- paste0("changeStyle(\"", teams$team_data_id, "\");\n")
  styled_teams <- str_c(styled_teams, collapse = "")
  
  # coalesce into an injestible <script> tag
  script <- paste("<script>", js, styled_teams, "</script>", sep = "\n")
  
  # add image elements so that team logos appear in the winbox on hover
  pngs <- 
    paste0('<image x="286.87" y="174.8" width="69.53" height="69.53" xlink:href="', 
           winbox$image, 
           '" data-id="', 
           winbox$team_data_id, '" />') %>%
    str_c(collapse = "\n")
  
  # augment ggobj html with team logos/javascript
  html_aug <- c(html_intro, pngs, html_outro, script)
  html_aug <- str_c(html_aug, collapse = "\n")
  
  # save!
  html_aug %>%
    write_lines(glue::glue("site/plot/{league}-{date}.html"))
  
  # close the tmp file
  unlink(path)
  
}

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


