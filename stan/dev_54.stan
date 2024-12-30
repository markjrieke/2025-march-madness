functions {
  matrix random_walk(vector beta0,
                     matrix eta,
                     real sigma) {
    // instantiate random walk matrix
    int T = num_elements(beta0);
    int S = num_elements(eta[1,:]) + 1;
    matrix[T,S] beta;
    
    // build random walk
    beta[:,1] = beta0;
    beta[:,2:S] = eta * sigma;
    for (s in 2:S) {
      beta[:,s] += beta[:,s-1];
    }
    
    return beta;
  }
  
  real poisson_log_lpmf(array[,] int Y,
                        array[] vector log_lambda) {
    int T = num_elements(log_lambda[:,1]);
    real lp = 0.0;
    for (t in 1:T) {
      lp += poisson_log_lpmf(Y[t,:] | log_lambda[t,:]);
    }
    
    return lp;
  }
  
  array[] vector map_lambda(real alpha,
                            matrix beta_o,
                            matrix beta_d,
                            matrix beta_h,
                            matrix beta_a,
                            array[,] int tid,
                            array[] int sid,
                            vector M,
                            matrix H,
                            matrix A) {
    // convert per-minute to per-game
    int N = num_elements(sid);
    int T = num_elements(beta_h[:,1]);
    int S = num_elements(beta_h[1,:]);

    // map team-per-game to observations
    array[2] vector[N] log_lambda;
    for (t in 1:2) {
      for (n in 1:N) {
        log_lambda[t,n] = alpha
                        + beta_o[tid[t,n], sid[n]]
                        - beta_d[tid[2-t+1,n], sid[n]]
                        + beta_h[tid[t,n], sid[n]] * H[t,n]
                        - beta_a[tid[t,n], sid[n]] * A[t,n];
      }
      log_lambda[t,:] += M;
    }
    
    return log_lambda;
  }
  
}

data {
  int<lower=0> N;                  // Number of observations
  int<lower=0> T;                  // Number of teams
  int<lower=1> S;                  // Number of seasons

  array[N] int sid;                // Map observations to seasons
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
  real beta0_o_mu;                 // Initial offense mean
  real<lower=0> beta0_o_sigma;     // Initial offense scale
  real beta0_d_mu;                 // Initial defense mean
  real<lower=0> beta0_d_sigma;     // Initial defense scale
  real beta0_h_mu;                 // Initial home advantage mean
  real<lower=0> beta0_h_sigma;     // Initial home advantage scale
  real beta0_a_mu;                 // Initial away disadvantage mean
  real<lower=0> beta0_a_sigma;     // Initial away disadvantage scale
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
  vector[T] beta0_o;
  vector[T] beta0_d;
  vector[T] beta0_h;
  vector[T] beta0_a;
  matrix[T,S-1] eta_o;
  matrix[T,S-1] eta_d;
  matrix[T,S-1] eta_h;
  matrix[T,S-1] eta_a;
}

transformed parameters {
  // separate type declarations so I can profile stan
  real<lower=0> sigma_o;
  real<lower=0> sigma_d;
  real<lower=0> sigma_h;
  real<lower=0> sigma_a;
  matrix[T,S] beta_o;
  matrix[T,S] beta_d;
  matrix[T,S] beta_h;
  matrix[T,S] beta_a;
  array[2] vector[N] log_lambda;
  
  // convert random walk scale parameters to natural scale
  profile("exp") {
    sigma_o = exp(log_sigma_o);
    sigma_d = exp(log_sigma_d);
    sigma_h = exp(log_sigma_h);
    sigma_a = exp(log_sigma_a);
  }
  
  // evaluate random walk
  profile("random_walk (matrix)") {
    beta_o = random_walk(beta0_o, eta_o, sigma_o);
    beta_d = random_walk(beta0_d, eta_d, sigma_d);
    beta_h = random_walk(beta0_h, eta_h, sigma_h);
    beta_a = random_walk(beta0_a, eta_a, sigma_a);
  }
  
  // map parameters to observations
  profile("map_lambda") {
    log_lambda = map_lambda(alpha, beta_o, beta_d, beta_h, beta_a, tid, sid, M, H, A);
  }
}

model {
  // priors
  profile("priors") {
    target += normal_lpdf(log_sigma_o | log_sigma_o_mu, log_sigma_o_sigma);
    target += normal_lpdf(log_sigma_d | log_sigma_d_mu, log_sigma_d_sigma);
    target += normal_lpdf(log_sigma_h | log_sigma_h_mu, log_sigma_h_sigma);
    target += normal_lpdf(log_sigma_a | log_sigma_a_mu, log_sigma_a_sigma);
    target += normal_lpdf(beta0_o | beta0_o_mu, beta0_o_sigma);
    target += normal_lpdf(beta0_d | beta0_d_mu, beta0_d_sigma);
    target += normal_lpdf(beta0_h | beta0_h_mu, beta0_h_sigma);
    target += normal_lpdf(beta0_a | beta0_a_mu, beta0_a_sigma);
    target += std_normal_lpdf(to_vector(eta_o));
    target += std_normal_lpdf(to_vector(eta_d));
    target += std_normal_lpdf(to_vector(eta_h));
    target += std_normal_lpdf(to_vector(eta_a));
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
      Y_rep[t,:] = poisson_log_rng(log_lambda[t,:]);
    }
  }
  
}
