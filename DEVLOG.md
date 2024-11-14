# DEVLOG

## 2024-11-15

Other data quality issues
* home/away is missing a _lot_ & it's not always the case that it's because of a neutral site game. See [here](https://www.espn.com/mens-college-basketball/game/_/gameId/330232050) for example --- no home/away indicators, but Ball State University is in Muncie, Indiana, where the game was played, so they _should_ be home. 
  * Ah shit, it looks like home is always the right side of the panel & away is the left side...
* Too many missing game_ids to recode by hand, will have to come up with a programmatic based solution
* Some games exist in the schedule, but don't have a game results page. I'm just ignoring these, but there's probably a better way to have writting the `tryCatch()` expression than what I implemented. 
  * This doesn't always work though... Sometimes
* Games with no results
  * These get encoded as periods < 0
  * Appears that they're mostly canceled games
  * Going to roll with that assumption
* Games with improbably low scores (total score < 20)
  * Only ~600 across both leagues
  * Looks to be that these need manual overrides
  * Fine, I'll do that...
* So many games without home/away status
  * Looks like right side is always home / left side is always away
  * Probably warrants a full rerun
  * Will need to separate this from 'neutral ground' games
* Women switch to a 4-period game in the 2015-16 season
* Periods doesn't count double-overtimes
  * Looks like the OT column in the results table includes *all* overtimes
  * Will need to pull the table header, which lists, for example, Final/2OT
* ID == missing
  * Looks like these are pretty much all low div teams (DII / DIII)
  * Will probably also want to pull other items from the header, like team names
  * Usually ~ 5-7%, but a big spike in the 2020-21 season
* Games that end in a tie?
  * There are two games that end in a tie
  * This is fine from a modeling perspective, but weird


## 2024-11-08

Kamala lost, which sucks. Gonna bury my head into work & personal projects & hope that the US doesn't descend into fascism under Trump's second term.

Anyway, I've decided to just re-write the webscraping functions myself, rather than import results from another package. It's a bit more work, but I get *exactly* what I need and have a bit more agency to solve weird data problems. The team and schedule scraping isn't too bad, but the game scraping takes like a billion years to run. I have to pepper in a bunch of calls to `Sys.sleep()` calls to keep from getting booted from the site. Even still, I get booted sometimes, so I've had to rewrite the scraping functions a few times to make sure I can make incremental progress.

There are still some weird data issues. Notably, some games have a score of 0-0 or 1-0 in ESPN, even though the game was actually played. For example, Iowa State vs. Iowa show weird results on [ESPN](https://www.espn.com/mens-college-basketball/game/_/gameId/230802294), but show the full score as expected [here](https://www.sports-reference.com/cbb/schools/iowa-state/men/2003-schedule.html). I suppose I'll just need to do a level of manual cleaning after-the-fact, but that's fine. 

## 2024-11-01

Literally got nothing done this week. A bit too stressed out about the election. Hopefully, Kamala wins & I can go on with my merry lil project here.

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