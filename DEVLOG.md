# DEVLOG

## 2024-10-18

As of this week, I've found the next project that'll consume my every-waking-moment from now until March: a March Madness forecast. I'm starting a devlog for this project so that if/when folks (includeing future me) look at this later down the line, they can see the obstacles/workarounds associated with a full-from-scratch project such as this. The devlogs should be kept to only a few short notes wrapping up the things I've done each Friday.

There are four basic elements of this project:

* Getting the data
* Building the model
* Simulating the tournament
* Building the UI

Weirdly enough, I expect the model to be the least intense. Regardless, I can't get the model up/running without data, so that's where I'm working first.

I need toplines and home/away status for each game. Thankfully, ESPN has a [public API](https://gist.github.com/akeaswaran/b48b02f1c94f873c6655e7129910fc3b) that I can ping to get game-level results. There are two packages, [hoopR](https://hoopr.sportsdataverse.org/) and [wehoop](https://wehoop.sportsdataverse.org/index.html), that serve as wrappers for the API. Unfortunately, the results from the package are a bit buggy and contain more information than I actually need, so I plan on rewriting the API calls myself. So far, it seems to be working fine, but I have _words_ for whoever invented JSON as a data structure. 