data {
  int S;                          // Number of samples
  int T;                          // Number of teams
  vector[T] beta_mean;            // Posterior parameter mean
  vector<lower=0>[T] beta_sd;     // Posterior parameter sd
}

transformed data {
  vector<lower=0>[T] beta_var = beta_sd^2;
  real nm1_over2 = 0.5 * (S - 1);
  real sqrt_N = sqrt(S);
}

parameters {
  vector[T] beta_mu;
  vector[T] log_beta_sigma;
  real log_sigma;
}

transformed parameters {
  real<lower=0> sigma = exp(log_sigma);
  vector<lower=0>[T] beta_sigma = exp(log_beta_sigma);
}

model {
  target += normal_lpdf(beta_mu | 0, sigma);
  target += normal_lpdf(beta_mean | beta_mu, beta_sigma / sqrt_N);
  target += gamma_lpdf(beta_var | nm1_over2, nm1_over2 / beta_sigma^2);
}

generated quantities {
  vector[T] beta = to_vector(normal_rng(beta_mu, beta_sigma));
  vector[T] eta = beta / sigma;
}


