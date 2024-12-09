functions {
  real half_normal_lpdf(real x,
                        real mu,
                        real sigma) {
    return normal_lpdf(x | mu, sigma) - normal_lccdf(0 | mu, sigma);
  }
}

data {
  // Dimensions of the dataset
  int<lower=0> N;
  int<lower=0> T;
  
  // Mapping ids
  array[N,2] int<lower=0, upper=T> tid;
  
  // Covariates
  array[N] int<lower=0> O;
  vector<lower=0, upper=1>[N] U;

  // Scoreboard
  array[N] int<lower=0> H;
  array[N] int<lower=0> A;
  
  // Priors
  real alpha;
  real<lower=0> sigma_o_mu;
  real<lower=0> sigma_o_sigma;
  real<lower=0> sigma_d_mu;
  real<lower=0> sigma_d_sigma;
  real<lower=0> sigma_g_mu;
  real<lower=0> sigma_g_sigma;
  real gamma_mu;
  real<lower=0> gamma_sigma;
}

transformed data {
  vector[N] M = 40 + to_vector(O) * 5;
}

parameters {
  real gamma;
  vector[T] eta_o;
  vector[T] eta_d;
  vector[T] eta_g;
  real<lower=0> sigma_o;
  real<lower=0> sigma_d;
  real<lower=0> sigma_g;
}

transformed parameters {
  vector[N] beta_h;
  vector[N] beta_a;
  for (n in 1:N) {
    beta_h[n] = alpha + eta_o[tid[n,1]] * sigma_o - eta_d[tid[n,2]] * sigma_d + eta_g[tid[n,1]] * sigma_g * U[n];
    beta_a[n] = alpha + eta_o[tid[n,2]] * sigma_o - eta_d[tid[n,1]] * sigma_d;
  }
  
  vector<lower=0>[N] lambda_h = exp(beta_h) .* M;
  vector<lower=0>[N] lambda_a = exp(beta_a) .* M;
}

model {
  // Priors
  target += std_normal_lpdf(eta_o);
  target += std_normal_lpdf(eta_d);
  target += std_normal_lpdf(eta_g);
  target += half_normal_lpdf(sigma_o | sigma_o_mu, sigma_o_sigma);
  target += half_normal_lpdf(sigma_d | sigma_d_mu, sigma_d_sigma);
  target += half_normal_lpdf(sigma_g | sigma_g_mu, sigma_g_sigma);
  target += normal_lpdf(gamma | gamma_mu, gamma_sigma);

  // Likelihood
  target += poisson_log_lpmf(O | gamma);
  target += poisson_lpmf(H | lambda_h); 
  target += poisson_lpmf(A | lambda_a);
}

generated quantities {
  array[N] int<lower=0> Yh;
  array[N] int<lower=0> Ya;
  array[N] int<lower=0> Ot = poisson_log_rng(rep_vector(gamma, N));
  for (n in 1:N) {
    for (i in 1:100) {
      Yh[n] = poisson_log_rng(beta_h[n] + log(40 + Ot[n] * 5));
      Ya[n] = poisson_log_rng(beta_a[n] + log(40 + Ot[n] * 5));
      if (Yh[n] != Ya[n]) {
        break;
      }
    }
  }
}


