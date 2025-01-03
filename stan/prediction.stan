data {
  // Dimensions of the dataset
  int<lower=0> N;                        // Number of observations (games)
  int<lower=0> T;                        // Number of teams
  
  // Game-level data
  vector<lower=0, upper=1>[N] V;         // Whether (1) or not (0) to apply a home-court advantage
  array[2,N] int<lower=0, upper=T> tid;  // Map team id to game [home, away]
  
  // Fixed parameters
  real alpha;                            // Fixed intercept value for score (log scale)
  
  // Team data
  array[T] vector[3] beta_Mu;            // Team-level mean of beta_o, beta_d, and beta_h
  array[T] matrix[3,3] beta_Sigma;       // Team-level covariance matrix of beta_o, beta_d, and beta_h
  
  // Overdispersion data
  real log_sigma_i;                      // Log-scale overdispersion scale
  
  // Overtime hurdle data
  vector[2] hurdle_Mu;                   // Hurdle model means of gamma_0 and delta_0
  matrix[2,2] hurdle_Sigma;              // Hurdle model covariance between gamma_0 and delta_0
  vector[2] poisson_Mu;                  // Poisson OT model means of gamma_ot and delta_ot
  matrix[2,2] poisson_Sigma;             // Poisson OT model covariance between gamma_ot and delta_ot
}

transformed data {
  matrix[2,N] H = rep_matrix(0, 2, N);
  H[1,:] = to_row_vector(V);
  real<lower=0> sigma_i = exp(log_sigma_i);
}

generated quantities {
  /*
      BLEGH --- pick up here tomorrow
  */
  vector[T] beta_o;
  vector[T] beta_d;
  vector[T] beta_h;
  
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


