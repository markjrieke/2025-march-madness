data {
  // Dimensions of the dataset
  int<lower=0> N;                        // Number of observations (games)
  int<lower=0> T;                        // Number of teams
  
  // Game-level data
  array[N] int<lower=0> O;               // Number of overtimes per game
  vector<lower=0, upper=1>[N] V;         // Whether (1) or not (0) to apply a home-court advantage
  array[2,N] int<lower=0, upper=T> tid;  // Map team id to game [home, away]
  array[2,N] int<lower=0> S;             // Final score [home, away]
  
  // Fixed parameters
  real alpha;                            // Fixed intercept value for score (log scale)
  
  // Hierarchical priors
  real<lower=0> sigma_o_mu;              // Mean for offensive rating half-normal prior
  real<lower=0> sigma_o_sigma;           // Scale for offensive rating half-normal prior
  real<lower=0> sigma_d_mu;              // Mean for defensive rating half-normal prior
  real<lower=0> sigma_d_sigma;           // Scale for defensive rating half-normal prior
  real<lower=0> sigma_h_mu;              // Mean for home-court advantage half-normal prior
  real<lower=0> sigma_h_sigma;           // Scale for home-court advantage half-normal prior
  real<lower=0> sigma_i_mu;              // Mean for observation-level offset half-normal prior
  real<lower=0> sigma_i_sigma;           // Scale for observation-level offset half-normal prior
  
  // Hurdle model priors (p(Ot=0) = p(regulation))
  real gamma_0_mu;                       // Prior mean for effect of team ratings on p(regulation)
  real<lower=0> gamma_0_sigma;           // Prior scale for effect of team ratings on p(regulation)
  real delta_0_mu;                       // Prior mean for p(regulation) intercept
  real<lower=0> delta_0_sigma;           // Prior scale for p(regulation) intercept
  
  // Hurdle model priors (p(Ot>0))
  real gamma_t_mu;                       // Prior mean for effect of team ratings on overtimes
  real<lower=0> gamma_t_sigma;           // Prior scale for effect of team ratings on overtimes
  real delta_t_mu;                       // Prior mean for overtime intercept
  real<lower=0> delta_t_sigma;           // Prior scale for overtime intercept
}

transformed data {
  vector[N] M = log(40 + to_vector(O) * 5);
  matrix[2,N] H = rep_matrix(0, 2, N);
  H[1,:] = to_row_vector(V);
}

generated quantities {
  array[2,N] int<lower=0> Y;
  array[N] int<lower=0> Ot;
  for(n in 1:N) {
    
    // Hurdle for number of overtimes played
    if (bernoulli_rng(theta[n])) {
      Ot[n] = 0;
    } else {
      for (i in 1:2000) {
        Ot[n] = poisson_log_rng(lambda_t[n]);
        if (Ot[n] > 0) {
          break;
        }
        Ot[n] = 0;
      }
    }
    
    // Simulate team scores
    array[2] real beta_i_rep = normal_rng(rep_vector(0, 2), sigma_i);
    if (Ot[n] == 0) {
      
      // If game ends in regulation, just simulate w/o ties
      for (i in 1:100) {
        for (t in 1:2) {
          Y[t,n] = poisson_log_rng(beta_t[t,n] + beta_i_rep[t] + log(40 + Ot[n] * 5));
        }
        if (Y[1,n] != Y[2,n]) {
          break;
        } 
      }
      
    } else {
      
      // If game goes to overtime, first find the tie score at Ot[n] - 1
      int Ot1m = Ot[n] - 1;
      vector[200] p = rep_vector(0, 200);
      for (i in 1:200) {
        for (t in 1:2) {
          p[i] += poisson_log_lpmf(i | beta_t[t,n] + beta_i_rep[t] + log(40 + Ot1m * 5));
        }
      }
      p = exp(p);
      p /= sum(p);
      int tied = categorical_rng(p);
      
      // Simulate final overtime w/o ties
      for (i in 1:100) {
        for (t in 1:2) {
          Y[t,n] = tied + poisson_log_rng(beta_t[t,n] + beta_i_rep[t] + log(5));
        }
        if (Y[1,n] != Y[2,n]) {
          break;
        }
      }
    }
  }
}


