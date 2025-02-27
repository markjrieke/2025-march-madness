# DEVLOG

## 2025-02-28

* decided to add a bracket plot
* (if I can figure it out, if not, I can just do the table only)
* added a function for modifying wid0 so that teams appear in the correct spot in the bracket
* making the chart interactive involves breaking ggiraph
* and writing my own set of javascript (fuck)
* HOLY SHIT IT WORKS
* Basically, I rewrote ggiraph, stripped out the `htmlwidgets::createWidget()` component, and injected my own javascript function into a big html string. Sending the raw html string to quarto lets me render the bracket.
* Realized there's a (mildly) annoying bug that isn't immediately solvable --- the first round games can be mixed up. This is because ESPN doesn't store the team ids in the html, just the game ids. So I assign position based on home/away, then correct positions in rounds 2:7 (I can do this because the bracket structure enforces a specific ordering). The only round I *can't* do this for is the first round. So we'll just live with it. 
* I also had to write the images directly to the svg since ggimage took years to render
* To get the links, I had to jump into some python/selenium (there's not really a good post-load option with rvest AFAIK)

## 2025-02-21

Working through the basic table structure didn't take too long. I ran into an issue in that I can't conditionally overwrite display text in `gt()` without overwriting the underlying data type --- it appears [this might not be possible in `gt`](https://stackoverflow.com/questions/79457694/conditionally-overwrite-cell-text-with-gtgt). That's alright, the workaround is just to explain in a legend.

I think all that's left is to productionalize the tabling function and write the qmds to display on the site!

## 2025-02-14

Some more progress on the bracket model this week --- I think it's functionally complete, though I may end up having to do some hotfixes once the bracket actually goes live. In order to test mid-bracket updates, I had to use the wayback machine to pull snapshots mid-tournament from 2023. This is fine, but the current HTML format for the bracket is slightly different than that used in 2023. I tested again on the 2024 bracket, so it should be fine, but as a backup process, I can manually load a csv with the correct `game_id`s. 

Hopefully I can finish out the bracket work this upcoming week --- I really just need to spend some time writing the documentation and cleaning up. After that I can *finally* get started on the UI element. I plan for it to be pretty simple: just a table with tabs for different days showing different snapshots of the tournament. 

## 2025-02-07

Not much forward progress this past week --- have been a bit distracted by getting a landing page for [swishbook](https://www.swishbook.com/) up and running. Really only was able to add some documentation for the prediction functions. I'll likely need to buckle down in the next week to really flesh out the bracket functions. 

## 2025-01-31

I made a good amount of progress this week, though I haven't finished all the "January" tasks (that's fine, there's enough time ahead of the tournament to finish them out). I finished writing the functions to pass data to the prediction model and ran predictions for games through today. I still need to add documentation, but the functions are fully, well, functional. 

I still need to write the functions for scraping the March Madness bracket page and running the bracket model. Given the amount of time the prediction functions took, I imagine I'll take another full week to fully flesh out the bracket update model. I'll need to use the 2024 page to flesh out the functions (obviously, the 2025 bracket isn't set yet!) and then use the output to build out the UI.

Getting close to the home stretch!

## 2025-01-24

Another relatively light week --- I've been working pretty diligently on another tangentially related side project. That being said, I feel like this project is in a pretty good spot in terms of the planned schedule. I've added documentation to all the existing functions and setup the `run.R` script to update the set of games in the current season. For January, I need to write the following:

* Function(s) for generating pre-game predictions in the current season (this isn't strictly necessary for this project, but is a nice-to-have, and I've already written the model to do so).
* Function(s) for scraping the [March Madness page](https://www.espn.com/mens-college-basketball/bracket/_/season/2025). At the moment, the 2025 page redirects to the 2024 bracket, which is fine for development purposes.
* Function(s) for running the bracket model.

After all that is done, I can start worrying about presentation and UI in February, though I plan to keep it somewhat simple. All things considered, this project is moving along at the expected pace.

## 2025-01-17

This week was relatively light in terms of incremental "hands on keys" dev work, but I was able to run the full set of historical models for both the men's and women's leagues. I'll need to write an incremental training script --- I'll probably actually save the output for each team for each day --- and add a bunch of documentation. After all that, I should be in the clear to start writing the March Madness-specific scrape and predict script. 

## 2025-01-10

*Lots* done this week --- the mantra of **don't let perfect be the enemy of the good** has been a good one to follow. I fully deleted all the dev Stan models and committed to an architecture of:

* A historical model
* A parameter recovery model (the yang to the historical model's yin)
* A fixed parameter prediction model for single games
* A fixed parameter prediction model for the entire tournament

As of last night, I was able to get the tournament simulation up and running --- it outputs the probability of each team advancing to each round *and* is able to account for games that have already been completed!

I've been working with sparse output from each set of models --- the next steps are to really ramp this thing up for production (i.e., increase the draws) and build out an update script. 

## 2025-01-02

The theme of this past week was **don't let perfect be the enemy of the good** (I had to write this in all caps in my notebook). I gave myself until December 30th to work on the time-series based historical model. I explored a *lot* of options (including a "correction layer" submodel), but ended up utilizing the `dev_19.stan` variant of the model that I got working a few weeks ago. This is good enough and keeps things moving --- all the time spent on further modeling provided little benefit with lots of extra complications.

The next stage is to "prettify" the model and actually record the historical output/2025 season priors. After that, I can start working on the prediction portion. 

## 2024-12-27

Didn't get too much done this week --- hard to do so over the holidays during perfect times, but I was also *violently* ill. Still, I was able to get started a bit on extending the season-level model to a time-series historical model. The current wrinkle is dealing with a banana posterior. All part of the iterative model building process!

## 2024-12-20

Oh the woes of making the assumption that I know things. Last week, I thought I was getting close to being done with modeling. Turns out I was very wrong! On checking simulated data, a game level iterative fit double counts information somewhere such that the uncertainty around the standard deviation for state change over seasons, $\sigma$, is *way* too low. Iterative fitting is supposed to *hemorrhage* information. When I iteratively fit over seasons, for example, I see the expected slight increase in uncertainty. Using the same model over games doesn't work, for some reason. This made me lose my mind, somewhat. I resigned myself to three possible options:

* Figure out *where* information was getting double-counted in the game-level iterative fit model and fix it.
* Use [PSIS](https://discourse.mc-stan.org/t/updating-model-based-on-new-data-via-psis/37196/1) to update the posterior without re-fitting the full model each time.
* Write two different models --- a historical prior and a season model (a-la [2024-potus](https://github.com/markjrieke/2024-potus)).

After giving the first two options a good ole college try, I settled on using the third --- it's not statistically perfect, but it's a worthwhile compromise to avoid refitting 20 years of basketball data each day. I did need to figure out a way of mapping the historical prior that yields offensive/defensive/home court $\beta$s for each team into hierarchical parameters $\eta$ and $\sigma$. This involves fitting a model to the $\beta$ draws from the historical model. For computational efficiency, I figured out how to use the sufficient formulation of a normal model. What's weird, however, is that the centered parameterization is *way more computationally efficient and stable* than the non-centered parameterization.

Lots of bumping into walls, but will continue to move forward on the historical model.

## 2024-12-13

This week was all about modeling. I wrote like 2,500 lines of code while iterating through dev models. Granted, most of this is because I've been saving each iteration under its own separate file. But still! Lot's done!

On the modeling front, I've been working through two approaches. Firstly, I've been working on a season-level model that uses 2018 data to estimate each team's offensive rating, defensive rating, and home court advantage. It also includes a hurdle component for estimating the number of periods played (i.e., hurdle on whether or not regulation ends in a tie, then poisson over the number of overtimes played). Generating posterior predictions took a bit of figuring out --- games can't end in a tie and the number of overtimes makes things weird. One mess of nested for loops later, though, and I'm able to generate the [expected output](https://bsky.app/profile/markjrieke.bsky.social/post/3lcvwb6zsdk2q).

Teams only play ~30ish games per season, which isn't a lot of information per team. As a result, the season-level model tends to under-estimate good teams and over-estimate bad teams (i.e., the hierarchical component is doing its job and pulling values towards the group average). That's fine/expected --- the other piece of the puzzle is to extend the season-level model to fit over many seasons. Doing so with the full dataset is actually pretty straightforward. However, I don't want to refit the entire model every time a new game comes in. The goal instead is to iteratively fit the model to new data by using the posterior from one fit as the prior for the next fit. This turned out to be somewhat more involved than I originally expected --- you can't just simply pass in each parameter's marginal mean/standard deviation as a prior. You also need to define a correlation structure among the parameters and pass *that* in as the prior. Luckily, I've got mostly gaussian blobs, so I've been able to work with a multivariate normal prior. It's not perfect at retaining information, but it's in the range of variation that you might expect when estimating covariances from 1k-10k samples. 

I've been working with toy data for the problem of iterative fitting. I still have a few things to figure out with the simulated datasets --- namely, making the model flexible enough to handle *either* an entire season *or* a subset of games. Once that's done, the last piece of the modeling puzzle is to apply the iterative-fit extensions to the season-level model.

## 2024-12-06

Last of the data checks completed! `NA` team ids pretty much universally indicate that we're dealing with a div II or div III team, so no need to worry about them too much (with the exception of Mid-Continent University, which is the only team that has both a `NA` id *and* an actual id (564). To keep things simple, we're just going to treat them like a div II / III team). I may need to figure out what to do with teams that migrate up to div I for a few seasons but then drop back down to div II / div III. The quick fix is just to filter based on the current `teams.parquet` list. 

Other than that, I've done some initial toe dipping into super basic model work. There's some identifiability issues with the bivariate poisson --- in hindsight this makes sense because you're determining 3 parameter ($\lambda_0$, $\lambda_h$, and $\lambda_a$) from 2 pieces of data ($S_h$ and $S_a$). The bivariate distribution of scores is pretty gaussian blobby, though, so I can *probably* get away with just modeling as two independent poissons. The alternative is to use a copula to get a better model of the covariance. 

Lots to do, but the path forward is pretty clear!

## 2024-11-29

It's Thanksgiving week & family is in town, so keeping this update short. I really only worked on sorting out a few data quality issues related to how I wrangled the scraped data:

* Fixed an issue causing team ids to get overwritten by the home id
* Added regex for correctly identifying *all* neutral games
* Added specific recode(s) for Cincinnati Christian, which is the *only* team with a *different* team_id for mens (3093) and womens(108833)

Some teams that *do* have an id *don't* have one in the final `games.parquet` file (e.g., BYU-Hawaii). I'll have to do some partial matching (or matching off names) here. That shouldn't be too difficult. On a positive note, it looks like points have the problem of over (rather than under) dispersion given a poisson distribution. This is preferred --- overdispersion of a poisson distribution is a solvable problem with modeling tools on hand. Underdispersion would require that I figure out the generalized poisson.

## 2024-11-22

*BIG* updates this week (as in, I think the data portion of this project can be called mostly done? I need to do some data quality checks, but I've got the data in hand). I deleted all the existing data that I scraped in the previous week and rewrote the scraper(s) to fix the problems noted below. Because the game scrape is now pulling from each team's schedule page, the schedule scraper is no longer needed. The scrapers a written around a new wrapper around `tryCatch()`, `retry_catch()` (which works with arbitrary expressions, not just webscraping). This (& a few other functions) sit in a utils.R script. The net result of all this rewriting is that **scraping the data is *way* faster**. As in, the game scrape took under an hour rather than two days.

The next step is to perform some last data quality checks to iron out edge cases & wrinkles, then I can (finally) get going on the modeling. Realistically, I won't get to modeling until the week after next, since my family is coming to visit for Thanksgiving next week.

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