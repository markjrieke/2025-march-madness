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
                        vector beta_a,
                        array[,] int tid,
                        vector M,
                        matrix H,
                        matrix A) {
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
                    + beta_h[tid[t,n]] * H[t,n]
                    - beta_a[tid[t,n]] * A[t,n];
      }
      log_mu[t,:] += M;
    }
    
    return log_mu;
  }
  
  array[] vector map_lambda(array[] vector log_mu,
                            array[] vector beta_i) {
    int N = num_elements(log_mu[1]);
    array[2] vector[N] log_lambda;
    for (t in 1:2) {
      log_lambda[t] = log_mu[t] + beta_i[t];
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
  
  real log_sigma_o_mu;             // Log offensive random walk scale mean
  real<lower=0> log_sigma_o_sigma; // Log offensive random walk scale scale
  real log_sigma_d_mu;             // Log defensive random walk scale mean
  real<lower=0> log_sigma_d_sigma; // Log defensive random walk scale scale
  real log_sigma_h_mu;             // Log home advantage random walk scale mean
  real<lower=0> log_sigma_h_sigma; // Log home advantage random walk scale scale
  real log_sigma_a_mu;             // Log away disadvantage random walk scale mean
  real<lower=0> log_sigma_a_sigma; // Log away disadvantage random walk scale scale
  real log_sigma_i_mu;             // Log overdispersion scale mean
  real<lower=0> log_sigma_i_sigma; // Log overdispersion scale scale
}

transformed data {
  vector[N] M = log(40 + O * 5);
  matrix[2,N] H = rep_matrix(0, 2, N);
  matrix[2,N] A = rep_matrix(0, 2, N);
  H[1,:] = to_row_vector(V);
  A[2,:] = to_row_vector(V);
}

parameters {
  real log_sigma_o;
  real log_sigma_d;
  real log_sigma_h;
  real log_sigma_a;
  real log_sigma_i;
  vector[T] eta_o;
  vector[T] eta_d;
  vector[T] eta_h;
  vector[T] eta_a;
  array[2] vector[N] eta_i;
}

transformed parameters {
  // separate type declarations so I can profile stan
  real<lower=0> sigma_o;
  real<lower=0> sigma_d;
  real<lower=0> sigma_h;
  real<lower=0> sigma_a;
  real<lower=0> sigma_i;
  vector[T] beta_o;
  vector[T] beta_d;
  vector[T] beta_h;
  vector[T] beta_a;
  array[2] vector[N] beta_i;
  array[2] vector[N] log_mu;
  array[2] vector[N] log_lambda;
  
  // convert scale parameters to natural scale
  profile("exp") {
    sigma_o = exp(log_sigma_o);
    sigma_d = exp(log_sigma_d);
    sigma_h = exp(log_sigma_h);
    sigma_a = exp(log_sigma_a);
    sigma_i = exp(log_sigma_i);
  }
  
  // evaluate random walk
  profile("random_walk (matrix)") {
    beta_o = eta_o * sigma_o;
    beta_d = eta_d * sigma_d;
    beta_h = eta_h * sigma_h;
    beta_a = eta_a * sigma_a;
  }
  
  profile("overdispersion") {
    for (t in 1:2) {
      beta_i[t] = eta_i[t] * sigma_i;
    }
  }
  
  // map parameters to observations
  profile("map_mu") {
    log_mu = map_mu(
      alpha, 
      beta_o, 
      beta_d, 
      beta_h, 
      beta_a, 
      tid,
      M, 
      H, 
      A
    );
  }
  
  profile("map lambda") {
    log_lambda = map_lambda(log_mu, beta_i);
  }
}

model {
  // priors
  profile("priors") {
    target += normal_lpdf(log_sigma_o | log_sigma_o_mu, log_sigma_o_sigma);
    target += normal_lpdf(log_sigma_d | log_sigma_d_mu, log_sigma_d_sigma);
    target += normal_lpdf(log_sigma_h | log_sigma_h_mu, log_sigma_h_sigma);
    target += normal_lpdf(log_sigma_a | log_sigma_a_mu, log_sigma_a_sigma);
    target += normal_lpdf(log_sigma_i | log_sigma_i_mu, log_sigma_i_sigma);
    target += std_normal_lpdf(eta_o);
    target += std_normal_lpdf(eta_d);
    target += std_normal_lpdf(eta_h);
    target += std_normal_lpdf(eta_a);
    for (t in 1:2) {
      target += std_normal_lpdf(eta_i[t]);
    }
  }
  
  // likelihood
  profile("likelihood") {
    target += poisson_log_lpmf(Y | log_lambda);
  }
}

generated quantities {
  // add from dev 19 when ready
  array[2,N] int Y_rep;
  profile("generated quantities") {
    for (t in 1:2) {
      Y_rep[t,:] = poisson_log_rng(log_mu[t,:] + normal_rng(0, sigma_i));
    }
  }
  
}
