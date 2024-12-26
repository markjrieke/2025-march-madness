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
  matrix[T,S] beta;
  vector[P] params;

  // convert raw parameters into a parameter vector
  // (turn this into a function at some point)
  params[1] = log_sigma;
  params[2:(T+1)] = beta0;
  {
    int E = T + 1;
    for (t in 1:T) {
      for (s in 1:(S-1)) {
        params[E+1] = eta[t,s];
        E += 1;
      }
    }
  }
  
  // find team-level skill parameters
  // (turn this into a function at some point)
  {
    real sigma = exp(log_sigma);
    beta[:,1] = beta0;
    for (s in 2:S) {
      beta[:,s] = beta[:,s-1] + eta[:,s-1] * sigma;
    }
  }
  
}

model {
  // unsaved outputs
  // (turn this into a function at some point)
  matrix[T,S] mu = beta + log(40);
  array[T] vector[N] log_lambda;
  for (n in 1:N) {
    log_lambda[1,n] = mu[tid[1,n], sid[n]];
    log_lambda[2,n] = mu[tid[2,n], sid[n]];
  }
  
  // priors
  target += multi_normal_lpdf(params | prior_mu, prior_Sigma);
  
  // likelihood
  // (overload poisson_log_lpmf so it looks nicer)
  for (t in 1:2) {
    target += poisson_log_lpmf(Y[t,:] | log_lambda[t,:]);
  }
}

generated quantities {
  // add from dev 19 when ready
}
