# DEVLOG

## 2024-11-15

Alrighty, this past week was somewhat involved, so this is a lengthier update. I was able to run the full game webscrape that I put together manually. It literally took two days to run because I had to pepper in a bunch of calls to `Sys.sleep()` to avoid being rude to the robots. That being said, after getting all the games back, I still ran into a number of data quality issues:

* Missing home/away
  * The indicator text for home/away tams is missing *a lot*, and it's not always the case that it's because of a neutral site game.
  * For example, [this game](https://www.espn.com/mens-college-basketball/game/_/gameId/330232050) between Ball State University and Iowa doesn't indicate which team is home/away. The game was played in Muncie, Indiana, where Ball State is located, so they *should* be the home team.
  * In general, it looks like the home team is always the right side of the panel and away is the left side.
  * There are too many instances to recode by hand, so this in and of itself likely warrants a rerun.
* Games with no results
  * Some games exist in the schedule, but don't have a game results page. I'm just ignoring these.
  * Some games have negative periods (periods were derived from the number of columns in the results table). It appears that these are either canceled or rescheduled events. I'm rolling with that assumption. 
  * Some games have improbably low scores (total score < 20). It looks like there's only ~600 across both leagues, so these'll have to be manually recoded to display the correct score.
* Overtimes
  * The number of periods played is derived from the number of columns in the results table for each game. But! Overtimes just get added to a single column --- even if the game goes through multiple overtimes. 
  * I'll need to import the correct number of overtimes from the header text (i.e., double-overtime would be recorded as "Final/2OT").
* Missing team ids
  * There are a number of games where one team has no page link and therefore has no team id.
  * These appear to be exhibition games against lower division (DII/DIII) schools.
  * To differentiate, I'll need to pull the team name *and* ID.
* Tie games
  * There are two games that end in a tie.
  * This is fine from a modeling perspective, but weird.
* Outstanding items
  * I still need to find a way to indicate whether or not a game is in the preseason, regular season, or postseason.
  * I also need to find a way to consistently indicate whether or not a game was played on neutral territory. 
  
I figured I would find issues after "the big scrape," so the list above isn't super surprising. Since some of the issues warrant a full rerun anyway, I poked around to see if there's a better method for getting the necessary info out. Turns out there is! Each team's schedule page lists for each season lists the results of each game, whether or not the game is in the regular or postseason, and whether or not the game was played on neutral territory. Further, this drastically reduces the number of scraping calls I need to make from one per game (~255,000) to one per team per season (~17,000). I'll end up with duplicate games in the results (since Team A vs. Team B will appear when I run both Team A's schedule *and* Team B's), but that's a worthwhile trade that I can wrangle away later. 

I rewrote a few wrappers around `tryCatch()` that should also require fewer calls to `Sys.sleep()` (and therefore, should run faster). I need to tinker around with it a bit to make sure, but I should be able to get the full set of results next week.

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