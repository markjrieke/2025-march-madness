# README

This directory contains pre-computed data that does not need to be re-evaluated on each model run as well as static data entered by hand. The data generating scripts for results in `games/` and `teams/` can be found in `R/data`. The data generating script for results in `images/` is available under `python/`. 

## games

* `games.parquet`: A parquet file containing the results of all games available on ESPN for both the men's and women's leagues from the 2002-2024 seaons. 
* `{league}-{season}.parquet` A parquet file containing the results of all games available on ESPN for the specified league and season.

## images

* `{league}-images.parquet`: A parquet file mapping each ESPN team_id to a link to the team's logo found on the current team landing page.

## manual

* `team-manual.csv`: A manually edited csv file mapping team id, slug, and name to an appropriately-sized display name and team color for each league.

## teams

* `teams-errors.parquet`: A log of errors encountered when scraping teams.
* `teams.parquet`: A parquet file mapping ESPN team id, slug, and name for each league.