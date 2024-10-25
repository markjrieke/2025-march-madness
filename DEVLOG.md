# DEVLOG

## 2024-10-25

This past week, I spent a good chunk of time rewriting the hoopR/wehoop interface to the ESPN basketball API. *Unfortunately*, the API itself is bjorked in un-fixable ways:

* It sometimes just serves incorrect information
  * For example, the Elon vs. Colgate game on Nov 17th, 2012 returns the halftime score, not the full game results. 
  * I was able to catch this because the game (among others) showed up as only having one period, which was odd.
  * It gives me pause about the ~44k other games that *look* correct.
* It's just *missing* a bunch of games?
  * Tulsa played < 5 games in the 2023-24 season, according to the API.
  * (This applies to basically every team)
  
The API is untrustworthy, but it's not clear if that's reflective of a poor product by ESPN. There's no official documentation on the API from ESPN, and what users have gathered online explicitly refer to it as [ESPN's hidden API endpoints](https://gist.github.com/akeaswaran/b48b02f1c94f873c6655e7129910fc3b), so it's not clear if this is maintained or intended for public use. Regardless, the API is unusable, so that leaves me with a few options:

* Hope that hoopR/wehooop have figured this out already & I can pull in game data from their respective {package}-data repositories
* Re-write a mens/womens variant of the [ncaahoopR](https://github.com/lbenz730/ncaahoopR) functions for scraping results from ESPN's website.

I did a cursory check of the former. It looks like it gets me like ~90% of the way there. The benefit is that the data is preformatted & all I need to do is read in from the repository. The downside is that there are some nice-to-have fields missing (number of periods, neutral site status) and that there still seem to be some data quality issues (Tulsa only has 29 of 32 games for the 2023-24 season and the `game_date` is incorrect more often than not). 

## 2024-10-18

As of this week, I've found the next project that'll consume my every-waking-moment from now until March: a March Madness forecast. I'm starting a devlog for this project so that if/when folks (includeing future me) look at this later down the line, they can see the obstacles/workarounds associated with a full-from-scratch project such as this. The devlogs should be kept to only a few short notes wrapping up the things I've done each Friday.

There are four basic elements of this project:

* Getting the data
* Building the model
* Simulating the tournament
* Building the UI

Weirdly enough, I expect the model to be the least intense. Regardless, I can't get the model up/running without data, so that's where I'm working first.

I need toplines and home/away status for each game. Thankfully, ESPN has a [public API](https://gist.github.com/akeaswaran/b48b02f1c94f873c6655e7129910fc3b) that I can ping to get game-level results. There are two packages, [hoopR](https://hoopr.sportsdataverse.org/) and [wehoop](https://wehoop.sportsdataverse.org/index.html), that serve as wrappers for the API. Unfortunately, the results from the package are a bit buggy and contain more information than I actually need, so I plan on rewriting the API calls myself. So far, it seems to be working fine, but I have _words_ for whoever invented JSON as a data structure. 