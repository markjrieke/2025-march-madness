functions {
  vector vectorize_parameters(real log_sigma,
                              vector beta_h,
                              vector beta0,
                              matrix eta) {
    // instantiate parameter vector
    int T = num_elements(beta0);
    int S = num_elements(eta[1,:]) + 1;
    int P = 1 + (2 * T) + (T * (S - 1));
    vector[P] params;

    // place parameters into the vector
    params[1] = log_sigma;
    params[2:(T+1)] = beta_h;
    params[(T+2):(T+1+T)] = beta0;
    
    // flatten matrix
    int E = 2 * T + 1;
    for (t in 1:T) {
      for (s in 1:(S-1)) {
        params[E+1] = eta[t,s];
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
  
  array[] vector map_lambda(matrix beta,
                            vector beta_h,
                            array[,] int tid,
                            array[] int sid,
                            vector M) {
    // convert per-minute to per-game
    int N = num_elements(sid);
    int T = num_elements(beta[:,1]);
    int S = num_elements(beta[1,:]);
    matrix[T,S] mu = beta;
    
    // map team-per-game to observations
    array[2] vector[N] log_lambda;
    for (t in 1:2) {
      for (n in 1:N) {
        log_lambda[t,n] = mu[tid[t,n], sid[n]];
      }
      log_lambda[t,:] += M;
    }
    
    // add home-court advantage
    for (n in 1:N) {
      log_lambda[1,n] += beta_h[tid[1,n]];
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
  
  real alpha;                  // Log mean full time score
  
  vector[P] prior_mu;          // Prior mean for each parameter
  matrix[P,P] prior_Sigma;     // Prior covariance among the parameters
}

transformed data {
  // add from dev 19 when ready
  vector[N] M = log(40 + O * 5);
}

parameters {
  real log_sigma;
  vector[T] beta_h;
  vector[T] beta0;
  matrix[T,S-1] eta;
}

transformed parameters {
  real<lower=0> sigma = exp(log_sigma);
  matrix[T,S] beta = random_walk(beta0, eta, sigma);
  vector[P] params = vectorize_parameters(log_sigma, beta_h, beta0, eta);
  array[2] vector[N] log_lambda = map_lambda(beta, beta_h, tid, sid, M);
}

model {
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
