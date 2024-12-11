data {
  int N;
  int T;
  int J;
  int M;
  array[N] int tid;
  array[N] int jid;
  array[N] int S;
  
  real beta0_mu;
  real<lower=0> beta0_sigma;
  real log_sigma_mu;
  real<lower=0> log_sigma_sigma;
}

parameters {
  vector[T] beta0;
  matrix[T,J-1] eta;
  real log_sigma;
}

transformed parameters {
  real<lower=0> sigma = exp(log_sigma);
  matrix[T,J] beta;
  beta[:,1] = beta0;
  for (j in 2:J) {
    beta[:,j] = beta[:,j-1] + eta[:,j-1] * sigma;
  }
  beta += log(40);
}

model {
  // Priors
  target += normal_lpdf(beta0 | beta0_mu, beta0_sigma);
  target += std_normal_lpdf(to_vector(eta));
  target += normal_lpdf(log_sigma | log_sigma_mu, log_sigma_sigma);
  
  // Likelihood
  for (n in 1:N) {
    target += poisson_log_lpmf(S[n] | beta[tid[n], jid[n]]);
  }
}

generated quantities {
  matrix[T,J+M] beta_plus;
  beta_plus[:,1:J] = beta;
  for (m in 1:M) {
    for (t in 1:T) {
      beta_plus[t,J+m] = beta_plus[t,J+m-1] + std_normal_rng() * sigma;
    }
  }
}
