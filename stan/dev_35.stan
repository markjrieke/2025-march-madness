data {
  int S;
  int G;
  matrix[S,G] y;
}

transformed data {
  vector[G] mean_y;
  vector<lower=0>[G] var_y;
  real nm1_over2 = 0.5 * (S - 1);
  real sqrt_N = sqrt(S);
  
  for (g in 1:G) {
    mean_y[g] = mean(y[:,g]);
    var_y[g] = variance(y[:,g]);
  }
}

parameters {
  vector[G] mu;
  vector[G] log_sigma;
}

transformed parameters {
  vector<lower=0>[G] sigma = exp(log_sigma);
}

model {
  target += normal_lpdf(mean_y | mu, sigma / sqrt_N);
  target += gamma_lpdf(var_y | nm1_over2, nm1_over2 / sigma^2);
}


