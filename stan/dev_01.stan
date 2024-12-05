data {
  // Dimensions of the dataset
  int<lower=0> N;
  
  // Scoreboard
  array[N] int<lower=0> H;
  array[N] int<lower=0> A;
}

parameters {
  real gamma;
  real alpha_h;
  real alpha_a;
}

transformed parameters {
  real lambda_0 = exp(gamma);
  real lambda_h = exp(alpha_h);
  real lambda_a = exp(alpha_a);
}

model {
  // Priors
  target += normal_lpdf(gamma | log(40), 0.3);
  target += normal_lpdf(alpha_h | log(30), 0.3);
  target += normal_lpdf(alpha_a | log(30), 0.3);
  
  // Likelihood
  target += poisson_lpmf(H | lambda_0 + lambda_h) + poisson_lpmf(A | lambda_0 + lambda_a);
}

generated quantities {
  int<lower=0> Y0 = poisson_rng(lambda_0);
  int<lower=0> Yh = poisson_rng(lambda_h);
  int<lower=0> Ya = poisson_rng(lambda_a);
  int<lower=0> Sh = Y0 + Yh;
  int<lower=0> Sa = Y0 + Ya;
}

