# README

This directory contains the Stan models used to generate the forecast. All models are run for 10,000 iterations (1,250 iterations across 8 chains).

## functions.stan

Contains a set of utility functions that are used throughout the pipeline. The functions are individually documented, and those that wrap statistical methods/etc. are described indirectly in the relevant model sections below.

## historical.stan

The historical model estimates team parameters within a league and season based on the final score and number of overtimes played in each game. 

### Score

In each game, $g$, the probability of each team, $t$, scoring the observed number of points, $\text{Y}_{g,t}$, is modeled as Poisson-distributed given a team-game-specific rate parameter, $\lambda_{g,t}$.

$$
\Pr(\text{Y}_{g,t} | \lambda_{g,t}) = \text{Poisson}(\text{Y}_{g,t} | \lambda_{g,t})
$$

$\lambda_{g,t}$ is the product of the team's average expected rate of scoring per minute, $\mu_{g,t}$, the number of minutes played in the game, $\text{M}_g$, and a team-game-specific overdispersion parameter, $\beta_{i,g,t}$.

$$
\log(\lambda_{g,t}) = \mu_{g,t} + \beta_{i,t,g} + \text{M}_{g}
$$

The number of minutes played depends on the number of overtimes played. A regulation game (across both leagues) lasts 40 minutes. Each additional overtime adds 5 minutes of total time to the game. 

$$
\text{M}_g = \log(40 + \text{Ot}_g \times 5)
$$

The expected rate of scoring per minute, $\mu_{g,t}$, is calculated slightly differently for the home team, $H$, and away team, $A$, in each game. Both teams add their offensive strength, $\beta_{o,t}$, and subtract the opponent's defensive strength, $\beta_{d,t}$, from a fixed reference point, $\alpha$. Home teams also add a home advantage parameter, $\beta_{h,t}$, for games that are not played on neutral territory (i.e., when $N_g=0$).

$$
\begin{align*}
\mu_{g,H} &= \alpha + \beta_{o,H} - \beta_{d,A} + \beta_{h,H} \times (1 - N_g) \\
\mu_{g,A} &= \alpha + \beta_{o,A} - \beta_{d,H}
\end{align*}
$$

Offensive, defensive, home advantage, and overdispersion parameters are hierarchically distributed. Priors for $\eta$ and $\sigma$ parameters are given as the output of the recovery model. 

$$
\begin{align*}
\beta_{o,t} &= \eta_{o,t} \sigma_o \\
\beta_{d,t} &= \eta_{d,t} \sigma_d \\
\beta_{h,t} &= \eta_{h,t} \sigma_h \\
\beta_{i,g,t} &= \eta_{i,g,t} \sigma_i
\end{align*}
$$

### Overtime

The probability of each game going to the observed number of overtimes, $\text{Ot}_g$, is modeled as a [hurdled Poisson](https://mc-stan.org/docs/stan-users-guide/finite-mixtures.html#hurdle-models) process where $\theta_g$ is the probability of a game ending in regulation and $\psi_g$ is the expected number of overtimes played among games that go to overtime.

$$
\Pr(\text{Ot}_g | \theta_g, \psi_g) = 
  \begin{cases}
    \theta_g & \text{if}\ \ \text{Ot}_g = 0 \\
    (1 - \theta_g) \frac{\text{Poisson}(\text{Ot}_g | \psi_g)}{1 - \text{PoissonCDF}(0 | \psi_g)} & \text{if}\ \ \text{Ot}_g > 0
  \end{cases}
$$

$\theta_g$ is a simple linear model, with slope $\gamma_0$ and intercept $\delta_0$, based on the absolute difference between the teams' average rate of scoring per minute. Implicitly, this means that teams that are far apart in skill are less likely to go to overtime than evenly-matched teams. 

$$
\text{logit}(\theta_g) = \gamma_0 \times |\mu_{g,H} - \mu_{g,A}| + \delta_0
$$

Similarly, $\psi_g$ is estimated with a simple linear model, with slope $\gamma_{\text{Ot}}$ and intercept $\delta_{\text{Ot}}$. Much like the hurdle component, this parameterizion implies that teams that are far apart in skill are likelier to play fewer overtimes than evenly-matched teams. 

$$
\log(\psi_g) = \gamma_{\text{Ot}} \times |\mu_{g,H} - \mu_{g,A}| + \delta_{\text{Ot}}
$$


## recovery.stan

The recovery model is a sub-model that enables the pseudo-random walk year over year in the historical model. As an output of the historical model, we record the draws for each team parameter, $\beta_t$, as well as the summary statistics of the means, $\mu_{\beta,t}$, and standard deviations, $\sigma_{\beta,t}$. Given these summary statistics, the recovery model estimates the latent parameters needed to recreate $\beta_t$ --- $\mu_{\eta,t}$ and $\sigma_{\eta,t}$ --- as well as the hierarchical standard deviation, $\sigma$. 

The recovery model first estimates the latent mean for each team, $\mu_{\eta,t}$, as normally distributed around $0$ with a hierarchical standard deviation, $\sigma$. 

$$
\Pr(\mu_{\eta,t}) = \text{Normal}(\mu_{\eta,t}\ |\ 0, \sigma) \\
$$

It models the observed summary statistics using the [sufficient formulation of the normal distribution](https://discourse.mc-stan.org/t/fitting-a-simple-normal-model-in-stan-conditional-on-sufficient-statistics-jeffreys-prior/34810/7). Here, $S$ is the number of posterior samples from the historical model.

$$
\begin{align*}
\Pr(\mu_{\beta,t}) &= \text{Normal}\left(\mu_{\beta,t}\ \Bigg |\ \mu_{\eta,t}, \frac{\sigma_{\eta,t}}{\sqrt{S}}\right) \\
\Pr(\sigma_{\beta,t}^2) &= \text{Gamma}\left(\sigma_{\beta,t}^2\ \Bigg |\ \frac{S-1}{2},\ \frac{S-1}{2\sigma_{\eta,t}^2} \right)
\end{align*}
$$

This lets the recovery model regenerate $\beta_t$ per draw using the estimated $\mu_{\eta,t}$ and $\sigma_{\eta,t}$, then recover $\eta_t$ from $\beta_t$ and $\sigma$.

$$
\begin{align*}
\beta_{t,\text{recovered}} &\sim \text{Normal}(\mu_{\eta,t}, \sigma_{\eta,t}) \\
\eta_t &= \frac{\beta_{t, \text{recovered}}}{\sigma}
\end{align*}
$$

## prediction.stan

The prediction model is a [fixed parameter](https://mc-stan.org/docs/reference-manual/mcmc.html#sampling-without-parameters) that does not estimate any parameters, but rather uses the posterior to generate predictions of individual game outcomes. Essentially, the prediction model is the historical model in reverse.

The first step is to extract parameters. For each team, the posterior mean of the offensive, defensive, and home advantage parameters are saved alongside a team-specific covariance matrix. In each simulation, the parameters are drawn from a multivariate normal distribution. 

$$
\begin{bmatrix}
  \beta_{o,t} \\
  \beta_{d,t} \\
  \beta_{h,t} \\
\end{bmatrix}
\sim 
\text{MultiNormal}\left(
  \begin{bmatrix}
    \mu_{o,t} \\
    \mu_{d,t} \\
    \mu_{h,t}
  \end{bmatrix}
  ,
  \Sigma_{\beta,t}
\right)
$$

The game-team overdisperson parameters are independent of the other team parameters, and are are drawn from a normal distribution with standard deviation $\sigma_i$. 

$$
\beta_{i,g,t} \sim \text{Normal}(0, \sigma_i)
$$

Like the team skill parameters, the slope and intercept paramters of the hurdle and poisson components of the overtime model are taken as draws from multivariate normal distributions with mean vectors $\mu$ and covariance matrices $\Sigma$.

$$
\begin{align*}
\begin{bmatrix}
  \gamma_0 \\
  \delta_0
\end{bmatrix}
&\sim 
\text{MultiNormal}\left(
  \begin{bmatrix}
    \mu_{\gamma_0} \\
    \mu_{\delta_0}
  \end{bmatrix}
  ,
  \Sigma_0
\right)
\\
\begin{bmatrix}
  \gamma_{\text{Ot}} \\
  \delta_{\text{Ot}}
\end{bmatrix}
&\sim
\text{MultiNormal}\left(
  \begin{bmatrix}
    \mu_{\gamma_{\text{Ot}}} \\
    \mu_{\delta_{\text{Ot}}}
  \end{bmatrix}
  ,
  \Sigma_{\text{Ot}}
\right)
\end{align*}
$$

With the team, game, and overtime parameters, the prediction model can reconstruct the log-mean expected points per minute, $\mu$.

$$
\begin{align*}
\mu_{g,H} &= \alpha + \beta_{o,H} - \beta_{d,A} + \beta_{h,H} \times (1 - N_g) \\
\mu_{g,A} &= \alpha + \beta_{o,A} - \beta_{d,H}
\end{align*}
$$

The absolute difference between home and away team skill is used to estimate the probabitlity of a game going to overtime, $\theta_g$, as well as the mean number of overtimes in games that do go to overtime, $\psi_g$.

$$
\begin{align*}
\text{logit}(\theta_g) &= \gamma_0 \times |\mu_{g,H} - \mu_{g,A}| + \delta_0 \\
\log(\psi_g) &= \gamma_{\text{Ot}} \times |\mu_{g,H} - \mu_{g,A}| + \delta_{\text{Ot}}
\end{align*}
$$

Next, the prediction model simulates the number of overtimes played. In simulations where a draw from $\text{Bernoulli}(\theta_g)=1$, the number of overtimes, $\text{Ot}_g$ is set to 0. In simulations where a draw from $\text{Bernoulli}(\theta_g)=0$, the number of overtimes is simulated as a draw from $\text{Poisson}(\psi_g)$, subject to the constraint that $\text{Ot}_g \neq 0$.

$$
\text{Ot}_g \sim 
\begin{cases}
0 & \text{if}\ \ \text{Bernoulli}(\theta_g) = 1 \\
\text{Poisson}_{x \neq 0}(\psi_g) & \text{if}\ \ \text{Bernoulli}(\theta_g) = 0
\end{cases}
$$

In simulations where $\text{Ot}_g = 0$, the game lasts 40 minutes, and $\lambda_{g,t}$ is set based on this length.

$$
\begin{align*}
\text{M}_g &= \log(40) \\
\log(\lambda_{g,t}) &= \mu_{g,t} + \beta_{i,g,t} + \text{M}_g
\end{align*}
$$

In these regulation-time simulations, each team's score is simply simulated as a draw from a poisson distribution subject to the constraint that $\text{Y}_{g,H} \neq \text{Y}_{g,A}$.

$$
\text{Y}_{g,t} \sim \text{Poisson}(\lambda_{g,t})
$$

In simulations where $\text{Ot}_g > 0$, however, things get a bit more complicated. The prediction model first sets $\text{M}_g$ as the number of minuts played in all periods excluding the last overtime. It then estimates two parameters: $\Lambda_{g,t}$, the expected number of points scored in all periods excluding the last overtime; and $\lambda_{g,t}$, the expected number of points scored in the last overtime.

$$
\begin{align*}
\text{M}_g &= \log(40 + 5 \times (\text{Ot}_g - 1))\\
\log(\Lambda{g,t}) &= \mu_{g,t} + \beta_{i,g,t} + \text{M}_g \\
\log(\lambda{g,t}) &= \mu_{g,t} + \beta_{i,g,t} + \log(5)
\end{align*}
$$

The prediction model then needs to simulate what the tied score was leading into the final overtime, $\text{T}_g$. To do this, it first determines the probability of the tied score between 0 and 200 based on $\Lambda_{g,t}$.

$$
\phi_s = \frac{\sum_{t \in [H,A]} \text{Poisson}(s | \Lambda_{g,t})}{\sum\phi}
$$

The simulated tied score leading into the final period is then simply a draw from a categorical distribution where each category has probability $\phi$ of being selected. 

$$
\text{T}_g \sim \text{Categorical}(\phi)
$$

Finally, the final score is simulated as the a draw from a poisson distribution given $\lambda_{g,t}$ plus the tied score leading into the final overtime. Again, this final score is subject to the constraint that $\text{Y}_{g,H} \neq \text{Y}_{g,A}$.

$$
\text{Y}_{g,t} \sim \text{Poisson}(\lambda_{g,t}) + \text{T}_g
$$

## bracket.stan

The bracket model is a superset of the prediction model. Like the prediction model, it is a fixed-parameter model takes takes in the multivariate posterior of the historical model and generates predictions according to the method described in the prediction model. Further, the bracket model generates a bracket dataset. In each simulation, the winners of each game advance to the next stage in the bracket. These simulated winners play a simulated game against one another, and the winners keep advancing through the tournament until there is only one remaining. 

There is no new statistical methodology used by the bracket model. The code to generate one simulated tournament outcome can be found in the function [`simulate_tournament_rng()`](https://github.com/markjrieke/2025-march-madness/blob/main/stan/functions.stan#L272-L351).


