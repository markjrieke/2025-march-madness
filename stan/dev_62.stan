data {
  int N;
  vector[N] X;
  vector[N] Y;
  
  real alpha_mu;
  real<lower=0> alpha_sigma;
  real beta_mu;
  real<lower=0> beta_sigma;
  real log_sigma_mu;
  real<lower=0> log_sigma_sigma;
}

parameters {
  real alpha;
  real beta;
  real log_sigma;
}

transformed parameters {
  real<lower=0> sigma = exp(log_sigma);
}

model {
  // priors
  target += normal_lpdf(alpha | alpha_mu, alpha_sigma);
  target += normal_lpdf(beta | beta_mu, beta_sigma);
  target += normal_lpdf(log_sigma | log_sigma_mu, log_sigma_sigma);
  
  // likelihood
  target += normal_lpdf(Y | alpha + beta * X, sigma);
}

generated quantities {
  array[N] real Y_rep = normal_rng(alpha + beta * X, sigma);
}
