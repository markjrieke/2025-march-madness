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
    };
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

/* Generate estimates of the number of overtimes per game given a hurdled poisson

    @param theta: a vector containing the probability of each game ending in 
              regulation.
    @param log_lambda_t: a vector containing the log-mean number of overtimes 
              (in the event each game goes to overtime).
*/
array[] int poisson_hurdle_rng(vector theta,
                               vector log_lambda_t) {
  int N = num_elements(theta);
  array[N] int Ot;
  for (n in 1:N) {
    if (bernoulli_rng(theta[n])) {
      Ot[n] = 0;
    } else {
      for (i in 1:2000) {
        Ot[n] = poisson_log_rng(log_lambda_t[n]);
        if (Ot[n] > 0) {
          break;
        }
        Ot[n] = 0;
      }
    }
  }
  
  return Ot;
}

/* Simulate the outcome of each game

    @param log_mu: an array of vectors containing the home team's (log_mu[1])
              and away team's (log_mu[2]) rate of scoring per minute of play per
              game.
    @param beta_i: A matrix of overdispersion parameters for the expected score.
    @param Ot: an array of integers containinig the number of overtimes played
              in each game.
*/
array[,] int simulate_scores_rng(array[] vector log_mu,
                                 matrix beta_i,
                                 array[] int Ot) {
  int N = num_elements(Ot);
  array[2,N] int Y_rep;
  for (n in 1:N) {
    
    if (Ot[n] == 0) {
      
      // if the game ends in regulation, just simulate w/o ties
      for (i in 1:100) {
        for (t in 1:2) {
          Y_rep[t,n] = poisson_log_rng(log_mu[t,n] + beta_i[t,n] + log(40));
        }
        if (Y_rep[1,n] != Y_rep[2,n]) {
          break;
        }
      }
      
    } else {
      
      // if the game goes to overtime, first find the tie score at Ot[n] - 1
      int Ot1m = Ot[n] - 1;
      
      // probability of a tie score between 1 and 200
      vector[200] p = rep_vector(0, 200);
      for (i in 1:200) {
        for (t in 1:2) {
          p[i] += poisson_log_lpmf(i | log_mu[t,n] + beta_i[t,n] + log(40 + Ot1m * 5));
        }
      }
      
      // normalize and sample which tie score was realized
      p = exp(p);
      p /= sum(p);
      int tied = categorical_rng(p);
      
      // simulate final overtime w/o ties
      for (i in 1:100) {
        for (t in 1:2) {
          Y_rep[t,n] = tied + poisson_log_rng(log_mu[t,n] + beta_i[t,n] + log(5));
        }
        if (Y_rep[1,n] != Y_rep[2,n]) {
          break;
        }
      }
      
    }
    
  }
  
  return Y_rep;
}

/* Estimate the probability that the home team wins

    @param Y_rep: a multidimensional array of integers containing the score for
              each team in each game.
*/
vector probability_home_win(array[,] int Y_rep) {
  int N = num_elements(Y_rep[1,:]);
  vector[N] p_win;
  for (n in 1:N) {
    p_win[n] = Y_rep[1,n] > Y_rep[2,n] ? 1.0 : 0.0;
  }
  
  return p_win;
}

/* Simulate the results of the tournament and estimate the probability of each
   team making it to each round
  
    @param wid0: a multidimensional array of integers mapping the ids of teams 
              that have won to rounds in the tournament. 0s indicate that the 
              game has not yet been played.
    @param alpha: a fixed value for the hierarchical mean score expected for
              two evenly matched teams.
    @param beta_o: a vector of offensive ratings for each team.
    @param beta_d: a vector of defensive ratings for each team.
    @param beta_h: a vector of home advantages for each team.
    @param sigma_i: scale of overdispersion in team scores.
    @param gamma_0: slope parameter in the linear model converting differences 
              in team skill to probability of game ending in regulation.
    @param delta_0: intercept parameter in the linear model converting 
              differences in team skill to probability of game ending in 
              regulation.
    @param gamma_ot: slope parameter in the linear model converting differences 
              in team skill to log-mean number of overtimes.
    @param delta_ot: intercept parameter in the linear model converting 
              differences in team skill to log-mean number of overtimes.
*/
matrix simulate_tournament_rng(array[,] int wid0,
                               real alpha,
                               vector beta_o,
                               vector beta_d,
                               vector beta_h,
                               real sigma_i,
                               real gamma_0,
                               real delta_0,
                               real gamma_ot,
                               real delta_ot) {
  int T = num_elements(beta_o);
  array[64,7] int wid = wid0;
  
  // simulate the winner matrix
  for (r in 2:7) {
    
    // number of games in this round of the tournament
    int G = to_int(2^(7 - r));
    
    // extract game-level overdispersion
    matrix[2,G] beta_i;
    for (t in 1:2) {
      beta_i[t,:] = to_row_vector(normal_rng(rep_vector(0, G), sigma_i));
    }
    
    // enforce no home game advantages
    matrix[2,G] H = rep_matrix(0, 2, G);
    
    // team ids for winners of the previous round
    array[2*G] int winners = wid[1:(2*G),r-1];
    
    // map the previous round's winners to a current round matrix
    array[2,G] int gid;
    for (g in 1:(2*G)) {
      int i = ((g + 1) % 2) + 1;
      int j = to_int(ceil(g/2.0));
      gid[i,j] = winners[g];
    }
    
    // log mean expected points for matchups in the current round
    array[2] vector[G] log_mu = map_mu(alpha, beta_o, beta_d, beta_h, gid, H);
    
    // hurdle over number of overtimes
    vector[G] theta = hurdle_probability(log_mu, gamma_0, delta_0);
    vector[G] log_lambda_t = overtime_poisson(log_mu, gamma_ot, delta_ot);
    
    // estimate the results of each game
    array[G] int Ot = poisson_hurdle_rng(theta, log_lambda_t);
    array[2,G] int Y_rep = simulate_scores_rng(log_mu, beta_i, Ot);
    
    // assign winners of each game to current round's update matrix
    for (g in 1:G) {
      if (wid[g,r] == 0) {
        if (Y_rep[1,g] > Y_rep[2,g]) {
          wid[g,r] = gid[1,g];
        } else {
          wid[g,r] = gid[2,g];
        }
      }
    }
  
  }
  
  // estimate the probability of each team advancing to each round in the tournament
  matrix[T,6] p_advance = rep_matrix(0, T, 6);
  for (t in 1:T) {
    for (r in 2:7) {
      int G = to_int(2^(7 - r));
      for (g in 1:G) {
        if (wid[g,r] == t) {
          p_advance[t,r-1] += 1;
          break;
        }
      }
    }
  }
  
  return p_advance;
  
}

