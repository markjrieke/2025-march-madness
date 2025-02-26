library(tidyverse)
library(ggiraph)

# rewrite girafe locally
default_width <- function(default = 6) {
  if (isTRUE(getOption("knitr.in.progress"))) {
    knitr::opts_current$get("fig.width")
  } else default
}

default_height <- function(default = 5) {
  if (isTRUE(getOption("knitr.in.progress"))) {
    knitr::opts_current$get("fig.height")
  } else default
}

default_opts <- function(){
  list(
    tooltip = girafe_defaults("opts_tooltip"),
    hover = girafe_defaults("opts_hover"),
    hover_inv = girafe_defaults("opts_hover_inv"),
    hover_key = girafe_defaults("opts_hover_key"),
    hover_theme = girafe_defaults("opts_hover_theme"),
    select = girafe_defaults("opts_selection"),
    select_inv = girafe_defaults("opts_selection_inv"),
    select_key = girafe_defaults("opts_selection_key"),
    select_theme = girafe_defaults("opts_selection_theme"),
    zoom = girafe_defaults("opts_zoom"),
    toolbar = girafe_defaults("opts_toolbar"),
    sizing = girafe_defaults("opts_sizing")
  )
}

merge_options <- function(options, args){
  for (arg in args) {
    if (inherits(arg, "opts_zoom")) {
      options$zoom <- arg
    } else if (inherits(arg, "opts_selection")) {
      options$select <- arg
    } else if (inherits(arg, "opts_selection_inv")) {
      options$select_inv <- arg
    } else if (inherits(arg, "opts_selection_key")) {
      options$select_key <- arg
    } else if (inherits(arg, "opts_selection_theme")) {
      options$select_theme <- arg
    } else if (inherits(arg, "opts_tooltip")) {
      options$tooltip <- arg
    } else if (inherits(arg, "opts_hover")) {
      options$hover <- arg
    } else if (inherits(arg, "opts_hover_key")) {
      options$hover_key <- arg
    } else if (inherits(arg, "opts_hover_theme")) {
      options$hover_theme <- arg
    } else if (inherits(arg, "opts_hover_inv")) {
      options$hover_inv <- arg
    } else if (inherits(arg, "opts_toolbar")) {
      options$toolbar <- arg
    } else if (inherits(arg, "opts_sizing")) {
      options$sizing <- arg
    }
  }
  options
}

merge_sizing_policy <- function(policy, args) {
  for (arg in args) {
    if (is.list(arg) && all(names(arg) %in% names(htmlwidgets::sizingPolicy()))) {
      policy <- arg
    }
  }
  policy
}

default_sizing_policy <- function() {
  htmlwidgets::sizingPolicy(knitr.figure = TRUE, browser.fill = FALSE)
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
  geom_text_interactive(data = structure %>%
                          mutate(lhs = pid <= 2^(6 - round),
                                 rhs = !lhs,
                                 x = case_when(lhs ~ round - 0.95,
                                               rhs ~ 13 - round + 0.95),
                                 y = eval_y(round, pid, lhs) + 0.35,
                                 hjust = case_when(lhs ~ 0, rhs ~ 1)),
                        mapping = aes(x = x,
                                      y = y,
                                      label = team,
                                      hjust = hjust,
                                      data_id = team),
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

# function args
ggobj <- ggobj
pointsize <- 12
width_svg <- NULL
height_svg <- NULL
options <- list(opts_hover(""))
dependencies <- NULL

path <- tempfile()

if (is.null(width_svg)) {
  width_svg <- default_width()
}
if (is.null(height_svg)) {
  height_svg <- default_height()
}

args <- list()
args$canvas_id <- args$canvas_id %||% paste("svg", gsub("-", "_", uuid::UUIDgenerate()), sep = "_")
args$file <- path
args$width <- width_svg
args$height <- height_svg
args$pointsize <- pointsize
args$standalone <- TRUE
arts$setdims <- FALSE

if (identical(args$bg, "transparent")) {
  args$bg <- "#ffffff01"
}

devlenth <- length(dev.list())
do.call(dsvg, args)
tryCatch({
  print(ggobj)
}, finally = {
  if (length(dev.list()) > devlenth) {
    dev.off()
  }
})

settings <- merge_options(default_opts(), options)
sizing_policy <- merge_sizing_policy(default_sizing_policy(), options)

html_str <- 
  read_file(path) %>%
  str_split("\n") %>%
  pluck(1)

js <- 
'function changeStyle(dataId) {
  const elements = document.querySelectorAll(`[data-id="${dataId}"]`);
  for (var i = 0; i < elements.length; i++) {
    if (elements[i].tagName != "text") {
      elements[i].setAttribute("fill-opacity", 0);
      elements[i].setAttribute("stroke-opacity", 0);
    };
    elements[i].onmouseover = function() {
    	for (var k = 0; k < elements.length; k++) {
    	  if (elements[k].tagName != "text") {
    	    elements[k].setAttribute("fill-opacity", 1);
        	elements[k].setAttribute("stroke-opacity", 1);
    	  }
      }
    };
    elements[i].onmouseout = function() {
    	for (var k = 0; k < elements.length; k++) {
    	  if (elements[k].tagName != "text") {
    	    elements[k].setAttribute("fill-opacity", 0);
        	elements[k].setAttribute("stroke-opacity", 0);
    	  }
      }
    };
  }
};'

styled_teams <- paste0("changeStyle(\"", teams$team_name, "\");\n")
styled_teams <- str_c(styled_teams, collapse = "")

script <- paste("<script>", js, styled_teams, "</script>", sep = "\n")

html_str[2] <- paste0(str_sub(html_str[2], 1, str_locate(html_str[2], "width=")[1] - 2), ">")

html_aug <- c(html_str, script)
html_aug <- str_c(html_aug, collapse = "\n")

html_aug %>%
  write_lines("tmp.html")

unlink(path)

girafe(
  ggobj = ggobj,
  options = list(
    opts_hover(css = "opacity:1;"),
    opts_hover_inv("opacity:0;"),
    opts_hover_theme("opacity:0;")
  )
)
