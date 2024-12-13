data {
  int N;
  vector[N] X;
  vector[N] Y;
  
  vector[3] mu;
  cov_matrix[3] Sigma;
}

parameters {
  real alpha;
  real beta;
  real log_sigma;
}

transformed parameters {
  real sigma = exp(log_sigma);
  vector[3] params = [alpha, beta, log_sigma]';
}

model {
  // priors
  target += multi_normal_lpdf(params | mu, Sigma);
  
  // likelihood
  target += normal_lpdf(Y | alpha + beta * X, sigma);
}
