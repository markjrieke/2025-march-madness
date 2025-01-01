functions {
  real poisson_log_lpmf(array[,] int Y,
                        array[] vector log_lambda) {
    int T = num_elements(log_lambda[:,1]);
    real lp = 0.0;
    for (t in 1:T) {
      lp += poisson_log_lpmf(Y[t,:] | log_lambda[t,:]);
    }
    
    return lp;
  }
  
  array[] vector map_mu(real alpha,
                        vector beta_o,
                        vector beta_d,
                        vector beta_h,
                        array[,] int tid,
                        vector M,
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
  
  array[] vector map_lambda(array[] vector log_mu,
                            matrix beta_i) {
    int N = num_elements(log_mu[1]);
    array[2] vector[N] log_lambda;
    for (t in 1:2) {
      log_lambda[t] = log_mu[t] + beta_i[t,:]';
    }
    
    return log_lambda;
  }
  
}

data {
  int<lower=0> N;                  // Number of observations
  int<lower=0> T;                  // Number of teams

  array[2,N] int tid;              // Map observations to teams [home, away]
  array[2,N] int Y;                // Final score [home, away]
  vector[N] O;                     // Number of overtimes per game
  vector[N] V;                     // Whether (1) or not (0) to apply home/away effects
  
  real alpha;                      // Log mean full time score
  
  vector[T] eta_o_mu;              // Offensive rating mean
  vector<lower=0>[T] eta_o_sigma;  // Offensive rating scale
  vector[T] eta_d_mu;              // Defensive rating mean
  vector<lower=0>[T] eta_d_sigma;  // Defensive rating scale
  vector[T] eta_h_mu;              // Home advantage mean
  vector<lower=0>[T] eta_h_sigma;  // Home advantage scale
  
  real log_sigma_o_mu;             // Log offensive scale mean
  real<lower=0> log_sigma_o_sigma; // Log offensive scale scale
  real log_sigma_d_mu;             // Log defensive scale mean
  real<lower=0> log_sigma_d_sigma; // Log defensive scale scale
  real log_sigma_h_mu;             // Log home advantage scale mean
  real<lower=0> log_sigma_h_sigma; // Log home advantage scale scale
  
  real log_sigma_i_mu;             // Log overdispersion scale mean
  real<lower=0> log_sigma_i_sigma; // Log overdispersion scale scale
  
  real<lower=0> beta_o_step_sigma; // Random walk scale for offensive rating
  real<lower=0> beta_d_step_sigma; // Random walk scale for defensive rating
  real<lower=0> beta_h_step_sigma; // Random walk scale for home advantage
}

transformed data {
  vector[N] M = log(40 + O * 5);
  matrix[2,N] H = rep_matrix(0, 2, N);
  H[1,:] = to_row_vector(V);
}

parameters {
  real log_sigma_o;
  real log_sigma_d;
  real log_sigma_h;
  real log_sigma_i;
  vector[T] eta_o;
  vector[T] eta_d;
  vector[T] eta_h;
  matrix[2,N] eta_i;
}

transformed parameters {
  // convert scale parameters to natural scale
  real<lower=0> sigma_o = exp(log_sigma_o);
  real<lower=0> sigma_d = exp(log_sigma_d);
  real<lower=0> sigma_h = exp(log_sigma_h);
  real<lower=0> sigma_i = exp(log_sigma_i);
  
  // evaluate hierarchical parameters
  vector[T] beta_o = eta_o * sigma_o;
  vector[T] beta_d = eta_d * sigma_d;
  vector[T] beta_h = eta_h * sigma_h;
  matrix[2,N] beta_i = eta_i * sigma_i;
  
  // map parameters to observations
  array[2] vector[N] log_mu = map_mu(alpha, beta_o, beta_d, beta_h, tid, M, H);
  array[2] vector[N] log_lambda = map_lambda(log_mu, beta_i);
}

model {
  // priors
  target += normal_lpdf(log_sigma_o | log_sigma_o_mu, log_sigma_o_sigma);
  target += normal_lpdf(log_sigma_d | log_sigma_d_mu, log_sigma_d_sigma);
  target += normal_lpdf(log_sigma_h | log_sigma_h_mu, log_sigma_h_sigma);
  target += normal_lpdf(log_sigma_i | log_sigma_i_mu, log_sigma_i_sigma);
  target += normal_lpdf(eta_o | eta_o_mu, eta_o_sigma);
  target += normal_lpdf(eta_d | eta_d_mu, eta_d_sigma);
  target += normal_lpdf(eta_h | eta_h_mu, eta_h_sigma);
  target += std_normal_lpdf(to_vector(eta_i));

  // likelihood
  target += poisson_log_lpmf(Y | log_lambda);
}

generated quantities {
  // Estimate one-step-ahead parameters
  array[T] real beta_o_step = normal_rng(beta_o, beta_o_step_sigma);
  array[T] real beta_d_step = normal_rng(beta_d, beta_d_step_sigma);
  array[T] real beta_h_step = normal_rng(beta_h, beta_h_step_sigma);
  
  array[2,N] int Y_rep;
  for (t in 1:2) {
    Y_rep[t,:] = poisson_log_rng(log_lambda[t,:]);
  }
}
