data {
  int S;
  int G;
  vector[G] beta_mu;
  vector<lower=0>[G] beta_sigma;
}

transformed data {
  vector<lower=0>[G] beta_var = beta_sigma^2;
  real nm1_over2 = 0.5 * (S - 1);
  real sqrt_N = sqrt(S);
}

parameters {
  vector[G] mu;
  real log_tau;
  vector[G] log_sigma;
}

transformed parameters {
  real<lower=0> tau = exp(log_tau);
  vector<lower=0>[G] sigma = exp(log_sigma);
}

model {
  target += normal_lpdf(mu | 0, tau);
  target += normal_lpdf(beta_mu | mu, sigma / sqrt_N);
  target += gamma_lpdf(beta_var | nm1_over2, nm1_over2 / sigma^2);
}

generated quantities {
  vector[G] beta = to_vector(normal_rng(mu, sigma));
  vector[G] eta = beta / tau;
}


