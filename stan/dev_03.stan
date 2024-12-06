data {
  // Dimensions of the dataset
  int<lower=0> N;
  
  // Covariates
  vector[N] O;
  
  // Scoreboard
  array[N] int<lower=0> H;
  array[N] int<lower=0> A;
}

parameters {
  real alpha_h;
  real alpha_a;
}

transformed parameters {
  vector<lower=0>[N] lambda_h = alpha_h + log(40 + O * 5);
  vector<lower=0>[N] lambda_a = alpha_a + log(40 + O * 5);
}

model {
  // Priors
  target += normal_lpdf(alpha_h | log(30), 0.3);
  target += normal_lpdf(alpha_a | log(30), 0.3);
  
  // Likelihood
  target += poisson_log_lpmf(H | lambda_h); 
  target += poisson_log_lpmf(A | lambda_a);
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

