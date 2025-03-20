# setup ------------------------------------------------------------------------

# libraries
library(tidyverse)
library(cmdstanr)
library(riekelib)
library(furrr)
library(rvest)
library(gt)
library(gtExtras)
library(ggiraph)
library(patchwork)

# util functions
source("R/utils.R")

# functions
function_paths <- c("R/model/", "R/site/")
for (path in function_paths) {
  walk(list.files(path), ~source(paste0(path, .x)))
}

# create stan model exe directory
if (!dir.exists("exe")) {
  dir.create("exe")
}

# run models -------------------------------------------------------------------

# iterate workflow across leagues
for (league in c("mens", "womens")) {
  
  # re-run historical model if need be
  if(!historical_completed(league)) {
    walk(2002:2024, ~run_historical_model(.x, league))
  }
  
  # update parameters for the current season
  walk(missing_days(league, "update"), ~run_update_model(league, .x))
  
  # update game-level predictions in the current season
  walk(missing_days(league, "prediction"), ~run_prediction_model(league, .x))
  
  # update tournament predictions
  tournament_days <- missing_tournament_days(league)
  walk(tournament_days, ~run_bracket_model(league, date = .x))
  
  # update site graphics
  walk(tournament_days, ~generate_html_bracket(league, .x))
  walk(tournament_days, ~generate_html_table(league, .x))

}


