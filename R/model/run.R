# setup ------------------------------------------------------------------------

# libraries
library(tidyverse)
library(cmdstanr)
library(riekelib)
library(furrr)
library(rvest)

# util functions
source("R/utils.R")

# functions
function_path <- "R/model/functions/"
walk(list.files(function_path), ~source(paste0(function_path, .x)))

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
  
  # update bracket predictions in the current season
  march_madness_dates <- seq.Date(from = mdy("3/20/25"), to = mdy("4/7/25"), by = "day")
  if (Sys.Date() %in% march_madness_dates) {
    run_bracket_model(league)
  }
  
}






