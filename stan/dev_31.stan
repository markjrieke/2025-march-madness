data {
  int N;
  vector[N] Y;
  
  real log_sigma_s;
  real log_sigma_o;
  
  vector[N] mu;
  cov_matrix[N] Sigma;
}

parameters {
  real beta0;
  // real log_sigma_s;
  // real log_sigma_o;
  vector[N-1] eta;
}

transformed parameters {
  vector[N] params = append_row([beta0]', eta);
  
  vector[N] beta;
  beta[1] = beta0;
  for (n in 2:N) {
    beta[n] = beta[n-1] + eta[n-1] * exp(log_sigma_s);
  }
}

model {
  // priors
  target += multi_normal_lpdf(params | mu, Sigma);
  
  // likelihood
  target += normal_lpdf(Y | beta, exp(log_sigma_o));
}
