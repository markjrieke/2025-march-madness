
# 2025 March Madness Forecast

This repository contains the code for fitting a dynamic Bayesian model
of NCAA team performance and simulating potential March Madness outcomes
based on the results. The model is written in
[Stan](https://mc-stan.org/) and the core pipeline is written in
[R](https://www.r-project.org/), with some modest
[Python](https://www.python.org/) and
[JavaScript](https://www.javascript.com/) as needed.

READMEs in each directory provide details on the directory contents. In
particular, the [stan/](stan/) README walks through the model in detail.
A general overview of the model methodology can be found in the [*How
this
works*](https://www.thedatadiary.net/posts/2025-03-16-march-madness/)
article, and the full output for the
[men’s](https://www.thedatadiary.net/projects/2025-march-madness/mens)
and
[women’s](https://www.thedatadiary.net/projects/2025-march-madness/womens)
brackets can be found at [the data
diary](https://www.thedatadiary.net/).

## Version history

### 1.0

###### 2025-03-20

- Initial release!
- Fixes a few minor pre-release bugs to ensure pipeline runs on 2025
  tournament data

### 0.0.9000

###### 2025-03-16

- Initial pre-release
