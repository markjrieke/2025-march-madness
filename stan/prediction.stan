functions {
  #include /functions.stan
}

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
  real log_sigma_i_mu;                   // Log-scale overdispersion scale mean
  real<lower=0> log_sigma_i_sigma;       // Log-scale overdispersion scale scale
  
  // Overtime hurdle data
  vector[2] hurdle_Mu;                   // Hurdle model means of gamma_0 and delta_0
  matrix[2,2] hurdle_Sigma;              // Hurdle model covariance between gamma_0 and delta_0
  vector[2] poisson_Mu;                  // Poisson OT model means of gamma_ot and delta_ot
  matrix[2,2] poisson_Sigma;             // Poisson OT model covariance between gamma_ot and delta_ot
}

transformed data {
  matrix[2,N] H = rep_matrix(0, 2, N);
  H[1,:] = to_row_vector(V);
}

generated quantities {
  // overdispersion scale
  real<lower=0> sigma_i = exp(normal_rng(log_sigma_i_mu, log_sigma_i_sigma));
  
  // team parameters derived from posterior
  vector[T] beta_o;
  vector[T] beta_d;
  vector[T] beta_h;
  matrix[2,N] beta_i;
  
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
  
  // extract game-level overdispersion
  for (t in 1:2) {
    beta_i[t,:] = to_row_vector(normal_rng(rep_vector(0, N), sigma_i));
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
  
  // map parameters to observations
  array[2] vector[N] log_mu = map_mu(alpha, beta_o, beta_d, beta_h, tid, H);

  // hurdle over number of overtimes
  vector[N] theta = hurdle_probability(log_mu, gamma_0, delta_0);
  vector[N] log_lambda_t = overtime_poisson(log_mu, gamma_ot, delta_ot);
  
  // estimate results of each game
  array[N] int<lower=0> Ot = poisson_hurdle_rng(theta, log_lambda_t);
  array[2,N] int Y_rep = simulate_scores_rng(log_mu, beta_i, Ot);
  vector[N] p_home_win = probability_home_win(Y_rep);
}


