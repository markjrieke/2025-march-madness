/*  Calculate the total log probability of data given a poisson distribution

    @param Y: a multidimensional array of integers. Y[1] corresponds to the home
              team's score in each game, Y[2] corresponds to the away team's 
              score. 
    @param log_lambda: an array of vectors containing the log of poisson mean 
              for each team in each game. log_lambda[1] returns a vector of home
              team values, log_lambda[2] returns a vector of away team lambda
              values.
*/
real poisson_log_lpmf(array[,] int Y,
                      array[] vector log_lambda) {
  int T = num_elements(log_lambda[:,1]);
  real lp = 0.0;
  for (t in 1:T) {
    lp += poisson_log_lpmf(Y[t,:] | log_lambda[t,:]);
  }
  
  return lp;
}

/* Map parameters to log-mean rate of scoring per minute of play

    @param alpha: a fixed value for the hierarchical mean score expected for
              two evenly matched teams.
    @param beta_o: a vector of offensive ratings for each team.
    @param beta_d: a vector of defensive ratings for each team.
    @param beta_h: a vector of home advantages for each team.
    @param tid: a multidimensional array of integers mapping each team in each 
              game to either home or away.
    @param H: a matrix that indicates whether (1) or not (0) to apply the home
              court advantage for each team in each game.
*/
array[] vector map_mu(real alpha,
                      vector beta_o,
                      vector beta_d,
                      vector beta_h,
                      array[,] int tid,
                      matrix H) {
  // convert per-minute to per-game
  int N = num_elements(tid[1,:]);
  int T = num_elements(beta_h);
  
  // map team-per-game to observations
  array[2] vector[N] log_mu;
  for (t in 1:2) {
    for (n in 1:N) {
      log_mu[t,n] = alpha
      + beta_o[tid[t,n]]
      - beta_d[tid[2-t+1,n]]
      + beta_h[tid[t,n]] * H[t,n];
    }
    log_mu[t,:] += M;
  }
  
  return log_mu;
}

/* Map log-mean rate of scoring per minute of play to log-mean predicted score

    @param log_mu: an array of vectors containing the home team's (log_mu[1]) 
              and away team's (log_mu[2]) rate of scoring per minute of play per
              game.
    @param beta_i: A matrix of overdispersion parameters for the expected score.
    @param M: A vector of the log of the total minutes played per game.
*/
array[] vector map_lambda(array[] vector log_mu,
                          matrix beta_i,
                          vector M) {
  int N = num_elements(log_mu[1]);
  array[2] vector[N] log_lambda;
  for (t in 1:2) {
    log_lambda[t] = log_mu[t] + beta_i[t,:]' + M;
  }
  
  return log_lambda;
}

/* Convert a difference in team skill to a probability of the game ending
   in regulation
   
    @param log_mu: an array of vectors containing the home team's (log_mu[1])
              and away team's (log_mu[2]) rate of scoring per minute of play per
              game.
    @param gamma_0: slope parameter in the linear model converting differences 
              in team skill to probability of game ending in regulation.
    @param delta_0: intercept parameter in the linear model converting 
              differences in team skill to probability of game ending in 
              regulation.
*/
vector hurdle_probability(array[] vector log_mu,
                          real gamma_0,
                          real delta_0) {
  int N = num_elements(log_mu[1]);
  vector[N] logit_theta = gamma_0 * abs(log_mu[1,:] - log_mu[2,:]) + delta_0;
  
  return inv_logit(logit_theta);
}

/* Convert a difference in team skill to the log mean number of overtimes (in 
   the event the game goes to overtime).
   
    @param log_mu: an array of vectors containing the home team's (log_mu[1])
              and away team's (log_mu[2]) rate of scoring per minute of play per
              game.
    @param gamma_ot: slope parameter in the linear model converting differences 
              in team skill to log-mean number of overtimes.
    @param delta_ot: intercept parameter in the linear model converting 
              differences in team skill to log-mean number of overtimes.
*/ 
vector overtime_poisson(array[] vector log_mu,
                        real gamma_ot,
                        real delta_ot) {
  int N = num_elements(log_mu[1]);
  vector[N] log_lambda_t = gamma_ot * abs(log_mu[1,:] - log_mu[2,:]) + delta_ot;
  
  return log_lambda_t;
}

/* Calculate the total log probability of data given a hurdled poisson

    @param O: an array of integers containing the number of overtimes per game.
    @param theta: a vector containing the probability of each game ending in 
              regulation.
    @param log_lambda_t: a vector containing the log-mean number of overtimes 
              (in the event each game goes to overtime).

*/
real poisson_hurdle_lpmf(array[] int O,
                         vector theta,
                         vector log_lambda_t) {
  int N = num_elements(O);
  real lp = 0.0;
  for (n in 1:N) {
    if (O[n] == 0) {
      lp += log(theta[n]);
    } else {
      lp += log1m(theta[n]) + poisson_log_lpmf(O[n] | log_lambda_t[n]) - poisson_lccdf(0 | exp(log_lambda_t[n]));
    }
  }
  
  return lp;
}