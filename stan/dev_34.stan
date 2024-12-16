data {
  int N;
  int G;
  array[N] int K;
  array[N] int Y;
  array[N] int gid;
}

parameters {
  real alpha;
  vector[G] eta;
  real log_sigma;
}

transformed parameters {
  vector[N] mu;
  for (n in 1:N) {
    mu[n] = alpha + eta[gid[n]] * exp(log_sigma);
  }
}

model {
  target += normal_lpdf(alpha | 0, 0.5);
  target += std_normal_lpdf(eta);
  target += normal_lpdf(log_sigma | -1.5, 0.5);
  
  target += binomial_logit_lpmf(Y | K, mu);
}

generated quantities {
  vector[G] beta = eta * exp(log_sigma);
}
