functions {
  vector vectorize_parameters(real log_sigma_o,
                              real log_sigma_d,
                              vector beta_o0,
                              vector beta_d0,
                              matrix eta_o,
                              matrix eta_d) {
    // instantiate parameter vector
    int T = num_elements(beta_o0);
    int S = num_elements(eta_o[1,:]) + 1;
    int P = (T + (T * (S - 1)) + 1) * 2;
    vector[P] params;

    // place parameters into the vector
    params[1] = log_sigma_o;
    params[2] = log_sigma_d;
    params[3:(T+2)] = beta_o0;
    params[(T+3):(T+2+T)] = beta_d0;
    
    // flatten matrix
    int E = 2 * T + 2;
    for (t in 1:T) {
      for (s in 1:(S-1)) {
        params[E+1] = eta_o[t,s];
        E += 1;
      }
    }
    
    for (t in 1:T) {
      for (s in 1:(S-1)) {
        params[E+1] = eta_d[t,s];
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
                            array[,] int tid,
                            array[] int sid) {
    // convert per-minute to per-game
    int N = num_elements(sid);
    int T = num_elements(beta_o[:,1]);
    int S = num_elements(beta_o[1,:]);
    
    // convert relative skill to per-minute score
    array[2] vector[N] log_lambda;
    for (n in 1:N) {
      for (t in 1:2) {
        log_lambda[t,n] = alpha + beta_o[tid[t,n], sid[n]] - beta_d[tid[2-t+1,n], sid[n]] + log(40);
      }
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
  
  real alpha;                  // Log mean score per minute of play
  
  vector[P] prior_mu;          // Prior mean for each parameter
  matrix[P,P] prior_Sigma;     // Prior covariance among the parameters
}

transformed data {
  // add from dev 19 when ready
}

parameters {
  real log_sigma_o;
  real log_sigma_d;
  vector[T] beta_o0;
  vector[T] beta_d0;
  matrix[T,S-1] eta_o;
  matrix[T,S-1] eta_d;
}

transformed parameters {
  real<lower=0> sigma_o = exp(log_sigma_o);
  real<lower=0> sigma_d = exp(log_sigma_d);
  matrix[T,S] beta_o = random_walk(beta_o0, eta_o, sigma_o);
  matrix[T,S] beta_d = random_walk(beta_d0, eta_d, sigma_d);
  
  vector[P] params = vectorize_parameters(
    log_sigma_o, 
    log_sigma_d,
    beta_o0, 
    beta_d0,
    eta_o, 
    eta_d
  );
}

model {
  // unsaved parameters
  array[2] vector[N] log_lambda = map_lambda(
    alpha,
    beta_o,
    beta_d,
    tid,
    sid
  );
  
  // priors
  target += multi_normal_lpdf(params | prior_mu, prior_Sigma);
  
  // likelihood
  target += poisson_log_lpmf(Y | log_lambda);

}

generated quantities {
  // add from dev 19 when ready
}
