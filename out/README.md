# README

This directory contains the output of each of the models in the pipeline. Each folder contains the output of the corresponding model script found in the `R/model/` directory. `model_log.parquet` is a running log that is appended after each model run and contains diagnostic information, versioning, and run times.

## bracket

* `p_advance.parquet`: Contains the probability of each team advancing to each round in the tournament for each league on each day. 
* `wid0.rds`: Contains a matrix of `tid` values that inform the structure of the tournament/which teams have advanced for each league on each day.

## historical

* `historical_parameters_global.parquet`: Contains the `CmdStanModel$summary()` output for the following global parameter values for each season in each league:
  * `log_sigma_*`: Log of the group level variance parameter for within-season offensive (`o`), defensive (`d`), and home advantage (`h`) parameters, as well as the overdispersion (`i`) parameter.
  * `log_sigma_*_step`: Log of the recovered group level variance parameters (with the exception of the overdispersion parameter) used as the prior for the next season. 
  * `gamma_*`: Slope parameter in the hurdle (`0`) and poisson (`ot`) components of overtime estimation.
  * `delta_*`: Intercept parameter in the hurdle (`0`) and poisson (`ot`) components of overtime estimation.
* `historical_parameters_team.parquet`: Contains the `CmdStanModel$summary()` output for the following team parameter values for each team over each season in each league:
  * `beta_*`: Within-season offensive, defensive, and home advantage parameters for each team.
  * `eta_*_step`: Recovered hierarchical offensive, defensive, and home advantage parameters for each team to be used as a prior for the next season.

## prediction

* `predictions.parquet`: Contains the probability of each team winning as well as the 90% credible interval for the expected score for each game in each league in the 2024-25 season. Updated daily.

## update

* `global_parameters.rds`: For the overtime hurdle and poisson linear models, contains the following objects for each league on each day in the 2024-25 season:
  * `Mu`: a vector of means mapping to `gamma_*` and `delta_*`.
  * `Sigma`: a covariance matrix.
* `team_parameter.rds`: For each team in each league on each day in the 2024-25 season, contains the following team parameter objects:
  * `Mu`: a vector of means mapping to `beta_*`.
  * `Sigma`: a covariance matrix
* `log_sigma_i.parquet`: Contains the output of `CmdStanModel$summary()` for `log_sigma_i` on each day in the 2024-25 season for each league.
* `{league}-games.parquet`: Contains the most recently scraped game results from ESPN for the specified league.
