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
  walk(missing_days(league), ~run_update_model(league, .x))
  
}






