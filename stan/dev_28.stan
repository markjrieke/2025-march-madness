data {
  int N;
  int T;
  int S;
  array[N] int tid;
  array[N] int sid;
  array[N] int Y;
  
  int P;
  vector[P] mu;
  cov_matrix[P] Sigma;
}

parameters {
  vector[T] beta0;
  matrix[T,S-1] eta;
  real log_sigma;
}

transformed parameters {
  // generate parameter vector
  // int P = T + (T * (S-1)) + 1;
  vector[P] params;
  params[1:T] = beta0;
  {
    int E = T;
    for (t in 1:T) {
      for (s in 1:(S-1)) {
        params[E+1] = eta[t,s];
        E += 1;
      }
    }
    params[P] = log_sigma;
  }
  
  // generate linear predictor
  real<lower=0> sigma = exp(log_sigma);
  matrix[T,S] beta;
  beta[:,1] = beta0;
  for (s in 2:S) {
    beta[:,s] = beta[:,s-1] + eta[:,s-1] * sigma;
  }
  matrix[T,S] log_lambda = beta + log(40);
}

model {
  // Priors
  target += multi_normal_lpdf(params | mu, Sigma);
  
  // Likelihood
  for (n in 1:N) {
    target += poisson_log_lpmf(Y[n] | log_lambda[tid[n], sid[n]]);
  }
}

generated quantities {

}
