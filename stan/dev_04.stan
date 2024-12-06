data {
  // Dimensions of the dataset
  int<lower=0> N;
  
  // Covariates
  vector<lower=0>[N] O;
  vector<lower=0, upper=1>[N] G;
  
  // Scoreboard
  array[N] int<lower=0> H;
  array[N] int<lower=0> A;
}

parameters {
  real alpha_h;
  real alpha_a;
  real beta_g;
}

transformed parameters {
  vector<lower=0>[N] lambda_h = exp(alpha_h) * (40 + O * 5) + beta_g * G;
  vector<lower=0>[N] lambda_a = exp(alpha_a) * (40 + O * 5);
}

model {
  // Priors
  target += normal_lpdf(alpha_h | log(70.0/40), 0.3);
  target += normal_lpdf(alpha_a | log(70.0/40), 0.3);
  target += normal_lpdf(beta_g | 5, 4);
  
  // Likelihood
  target += poisson_lpmf(H | lambda_h); 
  target += poisson_lpmf(A | lambda_a);
}

generated quantities {
  int<lower=0> Yh = poisson_log_rng(alpha_h + log(40));
  int<lower=0> Ya = poisson_log_rng(alpha_a + log(40));
  int<lower=0> Ot = 0;
  for (n in 1:100) {
    if (Yh != Ya) {
      break;
    } else {
      Yh += poisson_log_rng(alpha_h + log(5));
      Ya += poisson_log_rng(alpha_a + log(5));
      Ot += 1;
    }
  }
}

