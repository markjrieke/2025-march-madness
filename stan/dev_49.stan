functions {
  vector vectorize_parameters(real log_sigma_o,
                              real log_sigma_d,
                              real log_sigma_h,
                              vector beta0_o,
                              vector beta0_d,
                              vector beta0_h,
                              matrix eta_o,
                              matrix eta_d,
                              matrix eta_h) {
    // instantiate parameter vector
    int T = num_elements(beta0_o);
    int S = num_elements(eta_o[1,:]) + 1;
    int P = 3 + (3 * T) + 3 * (T * (S - 1));
    vector[P] params;

    // place parameters into the vector
    params[1] = log_sigma_o;
    params[2] = log_sigma_d;
    params[3] = log_sigma_h;
    params[4:(T+3)] = beta0_o;
    params[(T+4):(2*T+3)] = beta0_d;
    params[(2*T+4):(3*T+3)] = beta0_h;

    // flatten eta_o
    int E = 3 * T + 3;
    for (t in 1:T) {
      for (s in 1:(S-1)) {
        params[E+1] = eta_o[t,s];
        E += 1;
      }
    }
    
    // flatten eta_d
    for (t in 1:T) {
      for (s in 1:(S-1)) {
        params[E+1] = eta_d[t,s];
        E += 1;
      }
    }
    
    // flatten eta_h
    for (t in 1:T) {
      for (s in 1:(S-1)) {
        params[E+1] = eta_h[t,s];
        E += 1;
      }
    }
    
    return params;
  }
  
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
                            array[,] int tid,
                            array[] int sid,
                            vector M,
                            matrix H) {
    // convert per-minute to per-game
    int N = num_elements(sid);
    int T = num_elements(beta_o[:,1]);
    int S = num_elements(beta_o[1,:]);

    // map team-per-game to observations
    array[2] vector[N] log_lambda;
    for (t in 1:2) {
      for (n in 1:N) {
        log_lambda[t,n] = alpha 
                        + beta_o[tid[t,n], sid[n]] 
                        - beta_d[tid[2-t+1,n], sid[n]] 
                        + beta_h[tid[t,n], sid[n]] * H[t,n];
      }
      log_lambda[t,:] += M;
    }
    
    return log_lambda;
  }
  
}

data {
  int<lower=0> N;              // Number of observations
  int<lower=0> T;              // Number of teams
  int<lower=1> S;              // Number of seasons
  int<lower=1> P;              // Number of model parameters
  
  array[N] int sid;            // Map observations to seasons
  array[2,N] int tid;          // Map observations to teams [home, away]
  array[2,N] int Y;            // Final score [home, away]
  vector[N] O;                 // Number of overtimes per game
  vector[N] V;                 // Whether (1) or not (0) to apply the home advantage
  
  real alpha;                  // Log mean full time score
  
  vector[P] prior_mu;          // Prior mean for each parameter
  matrix[P,P] prior_Sigma;     // Prior covariance among the parameters
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
  vector[T] beta0_o;
  vector[T] beta0_d;
  vector[T] beta0_h;
  matrix[T,S-1] eta_o;
  matrix[T,S-1] eta_d;
  matrix[T,S-1] eta_h;
}

transformed parameters {
  // convert random walk scale parameters to natural scale
  real<lower=0> sigma_o = exp(log_sigma_o);
  real<lower=0> sigma_d = exp(log_sigma_d);
  real<lower=0> sigma_h = exp(log_sigma_h);
  
  // evaluate random walk
  matrix[T,S] beta_o = random_walk(beta0_o, eta_o, sigma_o);
  matrix[T,S] beta_d = random_walk(beta0_d, eta_d, sigma_d);
  matrix[T,S] beta_h = random_walk(beta0_h, eta_h, sigma_h);
  
  // map parameters to observations
  array[2] vector[N] log_lambda = map_lambda(alpha, beta_o, beta_d, beta_h, tid, sid, M, H);
}

model {
  // create parameter vector for assessing priors
  vector[P] params = vectorize_parameters(
    log_sigma_o,
    log_sigma_d,
    log_sigma_h,
    beta0_o,
    beta0_d,
    beta0_h,
    eta_o,
    eta_d,
    eta_h
  );
  
  // priors
  target += multi_normal_lpdf(params | prior_mu, prior_Sigma);
  
  // likelihood
  target += poisson_log_lpmf(Y | log_lambda);
}

generated quantities {
  // add from dev 19 when ready
  array[2,N] int Y_rep;
  for (t in 1:2) {
    Y_rep[t,:] = poisson_log_rng(log_lambda[t,:]);
  }
}
