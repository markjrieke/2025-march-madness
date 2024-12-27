functions {
  vector vectorize_parameters(real log_sigma,
                              vector beta0,
                              matrix eta) {
    // instantiate parameter vector
    int T = num_elements(beta0);
    int S = num_elements(eta[1,:]) + 1;
    int P = T + (T * (S - 1)) + 1;
    vector[P] params;

    // place parameters into the vector
    params[1] = log_sigma;
    params[2:(T+1)] = beta0;
    
    // flatten matrix
    int E = T + 1;
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
                            array[,] int tid,
                            array[] int sid) {
    // convert per-minute to per-game
    int N = num_elements(sid);
    int T = num_elements(beta[:,1]);
    int S = num_elements(beta[1,:]);
    matrix[T,S] mu = beta + log(40);
    
    // map team-per-game to observations
    array[2] vector[N] log_lambda;
    for (n in 1:N) {
      for (t in 1:2) {
        log_lambda[t,n] = mu[tid[t,n], sid[n]];
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
  
  vector[P] prior_mu;          // Prior mean for each parameter
  matrix[P,P] prior_Sigma;     // Prior covariance among the parameters
}

transformed data {
  // add from dev 19 when ready
}

parameters {
  real log_sigma;
  vector[T] beta0;
  matrix[T,S-1] eta;
}

transformed parameters {
  real<lower=0> sigma = exp(log_sigma);
  matrix[T,S] beta = random_walk(beta0, eta, sigma);
  vector[P] params = vectorize_parameters(log_sigma, beta0, eta);
}

model {
  // unsaved parameters
  array[2] vector[N] log_lambda = map_lambda(beta, tid, sid);
  
  // priors
  target += multi_normal_lpdf(params | prior_mu, prior_Sigma);
  
  // likelihood
  target += poisson_log_lpmf(Y | log_lambda);

}

generated quantities {
  // add from dev 19 when ready
}
