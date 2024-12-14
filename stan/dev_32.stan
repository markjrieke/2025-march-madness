data {
  int N;
  vector[N] Y;
  int t;
  
  real log_sigma_s;
  real log_sigma_o;
  
  vector[2] mu;
  cov_matrix[2] Sigma;
}

parameters {
  real beta0;
  // real log_sigma_s;
  // real log_sigma_o;
  real eta;
}

transformed parameters {
  vector[2] params = [beta0, eta]';
  real beta;
  if (t == 1) {
    beta = beta0;
  } else {
    beta = beta0 + eta * exp(log_sigma_s);
  }
}

model {
  // priors
  target += multi_normal_lpdf(params | mu, Sigma);
  
  // likelihood
  target += normal_lpdf(Y | beta, exp(log_sigma_o));
}
