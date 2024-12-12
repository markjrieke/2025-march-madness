data {
  int N;
  int T;
  int S;
  array[N] int tid;
  array[N] int sid;
  array[N] int Y;
  
  vector[T] beta0_mu;
  vector<lower=0>[T] beta0_sigma;
  matrix[T,S-1] eta_mu;
  matrix<lower=0>[T,S-1] eta_sigma;
  real log_sigma_mu;
  real<lower=0> log_sigma_sigma;
}

parameters {
  vector[T] beta0;
  matrix[T,S-1] eta;
  real log_sigma;
}

transformed parameters {
  real<lower=0> sigma = exp(log_sigma);
  matrix[T,S] beta;
  beta[:,1] = beta0;
  for (s in 2:S) {
    beta[:,s] = beta[:,s-1] + eta[:,s-1] * sigma;
  }
  matrix[T,S] log_lambda = beta + log(40);
}

model {
  // Priors
  target += normal_lpdf(beta0 | beta0_mu, beta0_sigma);
  for (s in 1:(S-1)) {
    target += normal_lpdf(eta[:,s] | eta_mu[:,s], eta_sigma[:,s]);
  }
  target += normal_lpdf(log_sigma | log_sigma_mu, log_sigma_sigma);
  
  // Likelihood
  for (n in 1:N) {
    target += poisson_log_lpmf(Y[n] | log_lambda[tid[n], sid[n]]);
  }
}

generated quantities {

}
