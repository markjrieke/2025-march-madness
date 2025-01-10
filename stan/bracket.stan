functions {
  #include /functions.stan
}

data {
  // Dimensions of the dataset
  int<lower=0> T;                        // Number of teams
  
  // Map teams to round of tournament
  array[64,7] int wid0;                  // Tournament advancement array
  
  // Fixed parameters
  real alpha;                            // Fixed intercept value for score (log scale)
  
  // Team data
  array[T] vector[3] beta_Mu;            // Team-level mean of beta_o, beta_d, and beta_h
  array[T] matrix[3,3] beta_Sigma;       // Team-level covariance matrix of beta_o, beta_d, and beta_h
  
  // Overdispersion data
  real log_sigma_i_mu;                   // Log-scale overdispersion scale mean
  real<lower=0> log_sigma_i_sigma;       // Log-scale overdispersion scale scale
  
  // Overtime hurdle data
  vector[2] hurdle_Mu;                   // Hurdle model means of gamma_0 and delta_0
  matrix[2,2] hurdle_Sigma;              // Hurdle model covariance between gamma_0 and delta_0
  vector[2] poisson_Mu;                  // Poisson OT model means of gamma_ot and delta_ot
  matrix[2,2] poisson_Sigma;             // Poisson OT model covariance between gamma_ot and delta_ot
}

generated quantities {
  // overdispersion scale
  real<lower=0> sigma_i = exp(normal_rng(log_sigma_i_mu, log_sigma_i_sigma));
  
  // team parameters derived from posterior
  vector[T] beta_o;
  vector[T] beta_d;
  vector[T] beta_h;
  
  // overtime hurdle parameters
  real gamma_0;
  real delta_0;
  real gamma_ot;
  real delta_ot;
  
  // extract skill parameters
  for (t in 1:T) {
    vector[3] beta;
    beta = multi_normal_rng(beta_Mu[t], beta_Sigma[t]);
    beta_o[t] = beta[1];
    beta_d[t] = beta[2];
    beta_h[t] = beta[3];
  }
  
  // extract hurdle parameters
  {
    vector[2] hurdle_params = multi_normal_rng(hurdle_Mu, hurdle_Sigma);
    vector[2] poisson_params = multi_normal_rng(poisson_Mu, poisson_Sigma);
    gamma_0 = hurdle_params[1];
    delta_0 = hurdle_params[2];
    gamma_ot = poisson_params[1];
    delta_ot = poisson_params[2];
  }
  
  // simulate tournament outcomes
  array[64,7] int wid = wid0;
  {
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
      
      // sample winners in the current round
      array[2] vector[G] log_mu = map_mu(alpha, beta_o, beta_d, beta_h, gid, H);
      
      // hurdle over number of overtimes
      vector[G] theta = hurdle_probability(log_mu, gamma_0, delta_0);
      vector[G] log_lambda_t = overtime_poisson(log_mu, gamma_ot, delta_ot);
      
      // estimate results of each game
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
  
}


