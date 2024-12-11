data {
  int N;
  int T;
  array[N] int tid;
  array[N] int S;
  
  real beta0_mu;
  real<lower=0> beta0_sigma;
  real<lower=0> sigma_mu;
  real<lower=0> sigma_sigma;
}

parameters {
  real beta0;
  vector[T-1] eta;
  real<lower=0> sigma;
}

transformed parameters {
  vector[T] beta;
  beta[1] = beta0;
  for (t in 2:T) {
    beta[t] = beta[t-1] + eta[t-1] * sigma;
  }
  beta += log(40);
}

model {
  // Priors
  target += normal_lpdf(beta0 | beta0_mu, beta0_sigma);
  target += std_normal_lpdf(eta);
  target += normal_lpdf(sigma | sigma_mu, sigma_sigma) - normal_lccdf(0 | sigma_mu, sigma_sigma);
  
  // Likelihood
  for (n in 1:N) {
    target += poisson_log_lpmf(S[n] | beta[tid[n]]);
  }
}

