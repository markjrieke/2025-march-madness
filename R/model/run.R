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

# add a check after run
walk(2002:2024, ~run_historical_model(.x, "mens"))
walk(2002:2024, ~run_historical_model(.x, "womens"))

# utils ------------------------------------------------------------------------





