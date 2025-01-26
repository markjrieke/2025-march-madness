#' Run mid-season updates
#' 
#' @description
#' The update model re-uses the historical model to update the offensive, 
#' defensive, and home-court advantage parameters for each team in the current
#' (2024-2025) season. More details about the historical model are documented in
#' the `run_historical_model()` roxygen tags.
#' 
#' Although it uses the historical stan model, the update model outputs a 
#' different set of variables. Namely, rather than outputting the pseudo-random
#' walk variables, the update model records the actual parameter summaries for 
#' each team (i.e., `beta_o` is recorded instead of `beta_o_step`). Because each
#' team rating component is correlated, the output also contains a covariance 
#' matrix for each team. These values are recorded for each team for each day
#' under `team_parameters.rds` (saving as a .rds is necessary to preserve the 
#' correlation matrix). Global parameters for the hurdle model and their 
#' covariance matrices are stored in a much similar way to 
#' global_parameters.rds`.
#' 
#' @param league Which league to extract results for. Either "mens" or "womens".
#' @param date The date in the current season to run the update for. Filters to
#'        only games that have occurred before the specified date.
#' @param ... Unused
#' @param samples Number of posterior samples to generate. Used for both warmup
#'        and sampling.
#' @param chains Number of chains used to fit the model. All chains will be run
#'        in parallel, if available. 
#' @param beta_o_step_sigma,beta_d_step_sigma,beta_h_step_sigma Pseudo-random 
#'        walk scale for team skill parameters.
#' @param log_sigma_i_step_sigma Pseudo-random walk scale for overdispersion 
#'        scale.
#' @param gamma_0_step_sigma,gamma_ot_step_sigma Pseudo-random walk scale for 
#'        slope parameters in the hurdle model.
#' @param delta_0_step_sigma,delta_ot_step_sigma Pseudo-random walk scale for 
#'        intercept parameters in the hurdle model.
run_update_model <- function(league,
                             date,
                             ...,
                             samples = 1e4,
                             chains = 8,
                             beta_o_step_sigma = 0.03,
                             beta_d_step_sigma = 0.03,
                             beta_h_step_sigma = 0.03,
                             log_sigma_i_step_sigma = 0.03,
                             gamma_0_step_sigma = 0.2,
                             delta_0_step_sigma = 0.05,
                             gamma_ot_step_sigma = 0.2,
                             delta_ot_step_sigma = 0.05) {
  
  cli::cli_h1(glue::glue("{str_to_title(league)} {scales::label_date('%b %d, %Y')(date)} Update"))
  
  # evaluate processing time
  start_ts <- Sys.time()
  
  # check whether or not functions.stan has been updated since last run
  if (out_of_date("exe/historical", "stan/functions.stan")) {
    force_recompile <- TRUE
  } else {
    force_recompile <- FALSE
  }
  
  # compile model!
  historical <- 
    cmdstan_model(
      "stan/historical.stan",
      dir = "exe/",
      force_recompile = force_recompile
    )
  
  # rename variables for internal use
  date_int <- date
  league_int <- league
  season <- 2025
  
  # update game data if not running backtest
  games_file <- glue::glue("out/update/{league}-games.parquet")
  if (date == Sys.Date() | !file.exists(games_file)) {
    update_season_data(league)
  }
    
  # import current set of games
  games <- 
    arrow::read_parquet(glue::glue("out/update/{league}-games.parquet")) %>%
    filter(date < date_int) %>%
    prep_games()
  
  # assign tids for the current season 
  teams <- 
    games %>%
    assign_tids()
  
  # set data based on current season information
  data_args <- set_data_args(games, teams)
  
  # set priors based on last season's posterior
  priors <- 
    set_historical_priors(
      teams, 
      season, 
      league, 
      data_args$T,
      log_sigma_i_step_sigma,
      gamma_0_step_sigma,
      delta_0_step_sigma,
      gamma_ot_step_sigma,
      delta_ot_step_sigma
    )
  
  # set random walk parameters (unused in the output, but passed to the model)
  random_walk_priors <-
    list(
      beta_o_step_sigma = beta_o_step_sigma,
      beta_d_step_sigma = beta_d_step_sigma,
      beta_h_step_sigma = beta_h_step_sigma
    )
  
  # combine data/priors
  stan_data <- c(data_args, priors, random_walk_priors)
  
  # fit stan model!
  historical_fit <-
    historical$sample(
      data = stan_data,
      seed = 2025,
      init = 0.01,
      step_size = 0.002,
      chains = chains,
      parallel_chains = chains,
      iter_warmup = round(samples/chains),
      iter_sampling = round(samples/chains),
      refresh = round(2/10 * samples/chains)
    )
  
  # post processing
  extract_correlated_team_parameters(historical_fit, teams, date, league)
  extract_correlated_global_parameters(historical_fit, "0", date, league)
  extract_correlated_global_parameters(historical_fit, "ot", date, league)
  
  # diagnostics
  diagnostics <-
    historical_fit %>%
    diagnostic_summary()
  
  # evaluate processing time
  end_ts <- Sys.time()
  
  # generate model log
  model_log <-
    tibble(
      model_name = "update",
      model_version = file.info("stan/historical.stan")$mtime,
      start_ts = start_ts,
      end_ts = end_ts,
      observations = stan_data$N,
      num_divergent = diagnostics$num_divergent,
      num_max_treedepth = diagnostics$num_max_treedepth,
      samples = samples,
      season = season,
      league = league,
      date_min = mdy(paste0("11/1/", season)),
      date_max = date,
      target_variable = glue::glue("")
    )
  
  # append log
  model_log %>%
    append_parquet("out/model_log.parquet")
  
}

#' Extract mean and covariance matrix summarizing team parameters
#' 
#' @param fit A model fit by `historical.stan`
#' @param teams A tibble mapping ESPN `team_name` and `team_id` to an internal
#'        mapping id, `tid`.
#' @param date The date in the current season to run the update for.
#' @param league Which league to extract results for. Either "mens" or "womens".
extract_correlated_team_parameters <- function(fit,
                                               teams,
                                               date,
                                               league) {
  
  # total number of teams
  T <- max(teams$tid)
  
  # extract draws in matrix format
  beta_o_draws <- fit$draws("beta_o", format = "matrix")
  beta_d_draws <- fit$draws("beta_d", format = "matrix")
  beta_h_draws <- fit$draws("beta_h", format = "matrix")
  
  # summarize with a mean and covariance matrix per team
  for (t in 1:T) {
    
    # combine team parameters into a single matrix
    beta_draws <- 
      cbind(
        beta_o_draws[,t], 
        beta_d_draws[,t], 
        beta_h_draws[,t]
      )
    
    # summarize as a tibble
    team_params <-
      tibble(
        date = date,
        league = league,
        tid = t,
        variable = list(c("beta_o", "beta_d", "beta_h")),
        Mu = list(colMeans(beta_draws)),
        Sigma = list(cov(beta_draws))
      )
    
    if (t == 1) {
      
      out <- team_params
      
    } else {
      
      out <-
        out %>%
        bind_rows(team_params)
      
    }
    
  }
  
  # join & save results
  out %>%
    left_join(teams) %>%
    select(-tid) %>%
    relocate(team_id, team_name, .after = league) %>%
    append_rds("out/update/team_parameters.rds")
  
}

#' Extract mean and covariance matrix summarizing global parameters
#' 
#' @param fit A model fit by `historical.stan`
#' @param parameter The set of parameters to be extracted. Use "0" for the
#'        hurdle component and "ot" for the poisson component.
#' @param date The date in the current season to run the update for.
#' @param league Which league to extract results for. Either "mens" or "womens".
extract_correlated_global_parameters <- function(fit,
                                                 parameter = c("0", "ot"),
                                                 date,
                                                 league) {
  
  # set parameters
  gamma_param <- paste0("gamma_", parameter)
  delta_param <- paste0("delta_", parameter)
  
  # extract draws in matrix format
  draws <- fit$draws(c(gamma_param, delta_param), format = "matrix")
  
  # summarize as a tibble
  global_params <-
    tibble(
      date = date,
      league = league,
      parameter = parameter,
      variable = list(c(gamma_param, delta_param)),
      Mu = list(colMeans(draws)),
      Sigma = list(cov(draws))
    )
  
  global_params %>%
    append_rds("out/update/global_parameters.rds")
  
}


#' Extract the most recent set of game results for the current season
#' 
#' @param league Which league to extract results for. Either "mens" or "womens".
update_season_data <- function(league) {
  
  cli::cli_alert_info(glue::glue("Updating {str_to_title(league)} data"))
  
  # rename variables for internal use
  league_int <- league
  season <- 2025

  # full list of schedules to scrape
  teams <- 
    arrow::read_parquet("data/teams/teams.parquet") %>%
    filter(league == league_int) %>%
    distinct(team_id)
  
  # break up season scrapes into 200-unit "chunks"
  chunks <- 
    teams %>%
    rowid_to_column("chunk") %>%
    mutate(chunk = ceiling(chunk/200))
  
  # total number of chunks to evaluate
  n_chunks <- max(chunks$chunk)
  
  # process webscraping in parallel
  plan(multisession, workers = 8)
  
  # evaluate each chunk separately
  for (chunk in 1:n_chunks) {
    
    # rename variables for filtering
    chunk_int <- chunk
    
    # scrape the schedules for the specified chunk
    game_chunk <- 
      chunks %>%
      filter(chunk == chunk_int) %>%
      mutate(results = future_map(team_id, 
                                  ~scrape_games(league, .x, season),
                                  .progress = TRUE)) %>%
      select(-chunk) %>%
      unnest(results)
    
    # save intermittant results
    if (chunk == 1) {
      results <- game_chunk
    } else {
      results <-
        results %>%
        bind_rows(game_chunk)
    }
    
    # break between chunk scrapes
    if (chunk != n_chunks) {
      sys_sleep(5)
    }
    
  }
  
  # return to sequential processing
  plan(sequential)
  
  # process results
  out <- 
    results %>%
    mutate(league = league,
           season = season) %>%
    
    # extract the game_id from the game link
    mutate(game_id = str_remove_all(game_link, "https://www.espn.com/|womens-|mens-|college-basketball/game/_/gameId/"),
           game_id = str_sub(game_id, 1, str_locate(game_id, "/")[,1] - 1)) %>%
    
    # de-duplicate games
    distinct(league,
             date,
             game_id, 
             .keep_all = TRUE) %>%
    
    # join in source team name
    left_join(arrow::read_parquet("data/teams/teams.parquet") %>% 
                distinct(league, team_id, team_name),
              by = c("league", "team_id")) %>%
    
    # determine whether (or not) the source team is home or away
    # "vs" indicates home, "@" indicates away (i.e., Tulsa vsOSU or Tulsa @OSU)
    mutate(home = str_sub(opponent_text, 1, 2) == "vs") %>%
    
    # extract opponent team's name
    # removes rank from opponent team, if applicable
    # removes neutral site indicator, if applicable
    mutate(opponent_name = if_else(home,
                                   str_sub(opponent_text, 3),
                                   str_sub(opponent_text, 2)),
           opponent_name = if_else(is.na(parse_number(str_sub(opponent_name, 1, 1))),
                                   opponent_name,
                                   str_sub(opponent_name, str_locate(opponent_name, " ")[,1] + 1)),
           opponent_name = str_remove(opponent_name, " \\*|\\*")) %>% 
    
    # determine whether (or not) the game was played on neutral territory
    mutate(neutral = str_detect(opponent_text, " \\*|\\*")) %>% 
    
    # determine whether (or not) the game went to overtime & the number of overtimes
    mutate(ot = str_detect(result_text, "OT"),
           n_ot = str_sub(result_text, -3, -3),
           n_ot = case_when(!ot ~ 0,
                            n_ot == " " ~ 1,
                            .default = as.numeric(n_ot))) %>%
    
    # determine whether (or not) the source team won/lost
    mutate(win = str_sub(result_text, 1, 1) == "W") %>%
    
    # extract the winning/losing score from the results text
    mutate(win_score = as.numeric(str_sub(result_text, 2, str_locate(result_text, "-")[,1] - 1)),
           lose_score = str_sub(result_text, str_locate(result_text, "-")[,1] + 1),
           lose_score = if_else(ot,
                                str_sub(lose_score, 1, str_locate(lose_score, " ")[,1] - 1),
                                lose_score),
           lose_score = as.numeric(lose_score)) %>%
    
    # assign winner/loser scores to source/opponent team based on win flag
    mutate(team_score = if_else(win, win_score, lose_score),
           opponent_score = if_else(win, lose_score, win_score)) %>%
    
    # extract the opponent's team_id from the opponent link
    mutate(opponent_id = str_remove_all(opponent_link, "/mens-|/womens-|college-basketball/team/_/id/"),
           opponent_id = str_sub(opponent_id, 1, str_locate(opponent_id, "/")[,1] - 1)) %>%
    
    # recode Cincinnati Christian --- the only school with a different id for men/women
    mutate(team_id = if_else(team_name == "Cincinnati Christian", "3093", team_id),
           opponent_id = if_else(opponent_name == "Cincinnati Christian", "3093", opponent_id)) %>%
    
    # assign home/away parameters based on whether (or not) the source team is the home team
    mutate(home_id = if_else(home, team_id, opponent_id),
           home_name = if_else(home, team_name, opponent_name),
           home_score = if_else(home, team_score, opponent_score),
           away_id = if_else(home, opponent_id, team_id),
           away_name = if_else(home, opponent_name, team_name),
           away_score = if_else(home, opponent_score, team_score)) %>%
    
    # remove canceled or rescheduled games
    filter(!is.na(game_id),
           !is.na(home_score),
           !is.na(away_score),
           home_score > 0,
           away_score > 0) %>%
    
    # join teams with n/a ids but that exist in the db
    # also remove bunk Cincinnati Christian recode
    left_join(arrow::read_parquet("data/teams/teams.parquet") %>%
                distinct(team_name, missing_id = team_id) %>%
                filter(missing_id != "108833"),
              by = c("home_name" = "team_name")) %>%
    mutate(home_id = if_else(is.na(home_id), missing_id, home_id)) %>%
    select(-missing_id) %>%
    left_join(arrow::read_parquet("data/teams/teams.parquet") %>%
                distinct(team_name, missing_id = team_id) %>%
                filter(missing_id != "108833"),
              by = c("away_name" = "team_name")) %>%
    mutate(away_id = if_else(is.na(away_id), missing_id, away_id)) %>%
    
    # write relevant cols to disk
    select(league,
           season,
           date,
           game_id,
           game_type,
           neutral,
           n_ot,
           home_id,
           home_name,
           home_score,
           away_id,
           away_name,
           away_score)
  
  # overwrite existing season games
  out %>%
    arrow::write_parquet(glue::glue("out/update/{league}-games.parquet"))
  
}

#' Get game results given a team and season
#' 
#' @param league Which league to extract results for. Either "mens" or "womens".
#' @param team_id ESPN numeric team_id.
#' @param season Season to extract results for. Seasons are identified by the
#'        year in which the last game was played.
scrape_games <- function(league, team_id, season) {
  
  # read html
  url <- glue::glue("https://www.espn.com/{league}-college-basketball/team/schedule/_/id/{team_id}/season/{season}")
  
  # get schedule as html table
  result <- retry_catch(read_html(url))
  
  # log a warning if need be
  if (!is.null(result$warning)) {
    
    tibble(league = league,
           team_id = team_id,
           season = season,
           warning = as.character(result$warning)) %>%
      append_parquet("out/update/games-warnings.parquet")
    
  }
  
  # log an error if need be
  if (!is.null(result$error)) {
    
    tibble(league = league,
           team_id = team_id,
           season = season,
           error = as.character(result$error)) %>%
      append_parquet("out/update/games-errors.parquet")
    
    # exit the function
    return()
    
  }
  
  # extract the schedule as a table
  elements <-
    result$result %>%
    html_elements(".Table__TR")
  
  # separate out by game type
  games <- 
    tibble(text = html_text2(elements)) %>%
    rowid_to_column("eid") %>%
    mutate(game_type = case_when(str_sub(text, -5) == "eason" ~ text,
                                 str_sub(text, 15, 18) == "TIME" ~ "upcoming")) %>%
    fill(game_type) %>%
    mutate(date = str_sub(text, 1, str_locate(text, "\t")[,1] - 1),
           date = paste(str_sub(date, 6), season, sep = ", "),
           date = mdy(date),
           month = month(date),
           date = if_else(month >= 11, date - years(1), date)) %>%
    drop_na() %>%
    filter(game_type != "upcoming")
  
  # extract table items from each game
  game_results <- 
    games %>%
    mutate(results = map(eid, ~extract_game_result(.x, elements))) %>%
    unnest(results) %>%
    select(-eid)
  
  return(game_results)
  
}

#' Extract the results of a game as a one-row tibble
#' 
#' @param eid Element id identifying the index of the game to extract from 
#'        `elements`.
#' @param elements HTML elements containing game results.
extract_game_result <- function(eid, elements) {
  
  row_elements <- html_elements(elements[eid], ".Table__TD")[2:3]
  
  # extract links
  opponent_link <-
    row_elements[1] %>%
    html_elements("a") %>%
    html_attr("href") %>%
    unique()
  
  # (this will be converted to game_id later)
  game_link <-
    row_elements[2] %>%
    html_elements("a") %>%
    html_attr("href")
  
  # extract text
  opponent_text <- html_text2(row_elements[1])
  result_text <- html_text2(row_elements[2])
  
  # convert in the event that anything is missing
  opponent_link <- if (length(opponent_link) == 0) "missing" else opponent_link
  game_link <- if (length(game_link) == 0) "missing" else game_link
  opponent_text <- if (length(opponent_text) == 0) "missing" else opponent_text
  result_text <- if (length(result_text) == 0) "missing" else result_text
  
  out <-
    tibble(
      opponent_link = opponent_link,
      game_link = game_link,
      opponent_text = html_text2(row_elements[1]),
      result_text = html_text2(row_elements[2])
    )
  
  return(out)
  
}

#' Determine which days have not been run for the update model
#' 
#' @param league Which league to extract results for. Either "mens" or "womens".
missing_days <- function(league) {
  
  # rename variables for internal use
  league_int <- league
  
  # all potential days to have run the model
  eligible_days <- 
    seq.Date(
      from = mdy("11/5/24"), 
      to = Sys.Date(), 
      by = "day"
    )
  
  # days that the model was actually run
  completed_days <- 
    arrow::read_parquet("out/model_log.parquet") %>%
    filter(model_name == "update",
           league == league_int) %>%
    pull(date_max)
  
  # remove days that were run from eligible days
  out <- 
    eligible_days[which(!eligible_days %in% completed_days)]
  
  return(out)
  
}


