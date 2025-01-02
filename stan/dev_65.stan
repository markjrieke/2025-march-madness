data {
  int N;
  vector[N] X_mu;
  vector[N] X_sigma;
  vector[N] Y;
  array[N] int<lower=1,upper=2> lid; 
  
  real alpha_mu;
  real<lower=0> alpha_sigma;
  real beta_mu;
  real<lower=0> beta_sigma;
  real log_sigma_mu;
  real<lower=0> log_sigma_sigma;
}

parameters {
  // linear parameters
  vector[2] alpha;
  vector[2] beta;
  vector[2] log_sigma;
  
  // measurement error
  vector[N] X;
}

transformed parameters {
  vector<lower=0>[2] sigma = exp(log_sigma);
  vector[N] mu;
  vector[N] sigma_vec;
  for (n in 1:N) {
    mu[n] = alpha[lid[n]] + beta[lid[n]] * X[n];
    sigma_vec[n] = sigma[lid[n]];
  }
}

model {
  // priors
  target += normal_lpdf(alpha | alpha_mu, alpha_sigma);
  target += normal_lpdf(beta | beta_mu, beta_sigma);
  target += normal_lpdf(log_sigma | log_sigma_mu, log_sigma_sigma);
  target += normal_lpdf(X | X_mu, X_sigma);
  
  // likelihood
  target += normal_lpdf(Y | mu, sigma_vec);
}

generated quantities {
  array[N] real X_hat = normal_rng(X_mu, X_sigma);
  vector[N] mu_hat;
  for (n in 1:N) {
    mu_hat[n] = alpha[lid[n]] + beta[lid[n]] * X_hat[n];
  }
  array[N] real Y_rep = normal_rng(mu_hat, sigma_vec);
}
