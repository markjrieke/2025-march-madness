# setup ------------------------------------------------------------------------

# libraries
library(tidyverse)
library(cmdstanr)
library(riekelib)

# functions
function_path <- "R/model/functions/"
walk(list.files(function_path), ~source(paste0(function_path, .x)))

# create stan model exe directory
if (!dir.exists("exe")) {
  dir.create("exe")
}

# run historical model ---------------------------------------------------------

# re-run historical model if needed
for (league in c("mens", "womens")) {
  if(!historical_completed(league)) {
    walk(2002:2024, ~run_historical_model(.x, league))
  }
}

# utils ------------------------------------------------------------------------





