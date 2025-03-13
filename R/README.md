# README

This directory contains all the R scripts and functions necessary to import data, run the modeling pipeline, and generate graphics on the site. Individual functions are documented in detail, but a general overview for each file can be found here. 

## ./

* `run.R`
  * Runs the entire pipeline and generates site graphics. 
* `utils.R` 
  * Contains a set of utility functions that are used across R functions.
  * Includes functions for force-appending files upon write, determining file sync status, and evaluating expressions while retaining errors/warnings in the return.

## data/

Contains scripts for generating data that doesn't need to be recomputed each run but needs to be derived programatically. The outputs of each script live in the `data/` directory under root.

* `games.R`
  * Scrapes the results of all games played in the men's and women's leagues since the 2002 season from ESPN.
  * Games against lower division teams are included in the result. 
* `teams.R` 
  * Scrapes the team information (name, id, and slug) for teams that appear at any point in ESPN's schedule.
  * Teams that have never appeared as D1 teams do not appear in the table.
  
## model/

Contains functions for running each of the pipelines for each model. Models themselves are in the `stan/` directory under root. Model outputs are contained in the `out/` directory under root.

* `bracket.R`
  * Simulates the outcome of the tournament based on the most recent set of team parameters.
  * Writes the probability of each team advancing to each round in the tournament as well as the most recent tournament structure.
* `historical.R`
  * Runs the historical model that estimates team and league parameter values based on all games in the specified season. 
  * Uses the recovery model (see `recovery.R`) as a sub-model to convert pseudo-random walk parameters to their hierarchical components. The recovery model writes the recovered team and league parameters out. 
  * Writes the raw team parameters as well as the pseudo-random walk scales out.
* `prediction.R`
  * Uses the output of the update model to generate pre-game predictions for the league and day of the 2024-25 season specified. 
  * Does not estimate parameters, but is instead run in "fixed parameter" mode.
  * Writes predictions out, including the range of expected scores as well as the probability of each team winning. 
* `recovery.R`
  * A sub-model of the historical model (see `historical.R`) used to convert the pseudo-random walk parameters to their hierarchical components.
  * Writes the recovered team and league parameters out.
* `update.R`
  * Updates the team and league parameters for the league and date of the 2024-25 season specified. 
  * Under the hood, re-uses `historical.stan`.
  * Writes the team and league parameters out as combinations of vectors and correlation matrices to preserve the correlation among the parameters. 
* `model-utils.R`
  * Contains a set of utility functions used across the model functions.
  * Notably, since `update.R` and `historical.R` use the same stan model under the hood, many of the pre-processing functions are stored here.

## site/

Contains functions for generating the site graphics. 

* `plot.R`
  * Generates an interactive March Madness bracket as HTML for the league and date specified. 
  * Injects custom javascript from `js/plot.js` that displays the probability of each team advancing along with their bracket path on hover.
* `table.R`
  * Generates an interactive March Madness summary table, saved as an RDS file, for the league and date specified.
* `utils.R`
  * Contains a few functions that are used across both `plot.R` and `table.R`.
  * Namely, `increment_pid()` and `route_pid()` are used to correctly map the current status of the tournament to the interactive bracket and table elements.
