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
  real<lower=0> sigma_i_mu;
  real<lower=0> sigma_i_sigma;
  real gamma_mu;
  real<lower=0> gamma_sigma;
  real delta_mu;
  real<lower=0> delta_sigma;
}

transformed data {
  vector[N] M = 40 + to_vector(O) * 5;
}

parameters {
  real gamma;
  real delta;
  vector[T] eta_o;
  vector[T] eta_d;
  vector[T] eta_g;
  array[2] vector[N] eta_i;
  real<lower=0> sigma_o;
  real<lower=0> sigma_d;
  real<lower=0> sigma_g;
  real<lower=0> sigma_i;
}

transformed parameters {
  vector[N] beta_h;
  vector[N] beta_a;
  for (n in 1:N) {
    beta_h[n] = alpha + eta_o[tid[n,1]] * sigma_o - eta_d[tid[n,2]] * sigma_d + eta_g[tid[n,1]] * sigma_g * U[n];
    beta_a[n] = alpha + eta_o[tid[n,2]] * sigma_o - eta_d[tid[n,1]] * sigma_d;
  }
  
  vector<lower=0>[N] lambda_h = exp(beta_h + eta_i[1] * sigma_i) .* M;
  vector<lower=0>[N] lambda_a = exp(beta_a + eta_i[2] * sigma_i) .* M;
  vector[N] lambda_p = gamma * abs(beta_h - beta_a) + delta;
}

model {
  // Priors
  target += std_normal_lpdf(eta_o);
  target += std_normal_lpdf(eta_d);
  target += std_normal_lpdf(eta_g);
  for (i in 1:2) {
    target += std_normal_lpdf(eta_i[i]);
  }
  target += half_normal_lpdf(sigma_o | sigma_o_mu, sigma_o_sigma);
  target += half_normal_lpdf(sigma_d | sigma_d_mu, sigma_d_sigma);
  target += half_normal_lpdf(sigma_g | sigma_g_mu, sigma_g_sigma);
  target += half_normal_lpdf(sigma_i | sigma_i_mu, sigma_i_sigma);
  target += normal_lpdf(gamma | gamma_mu, gamma_sigma);
  target += normal_lpdf(delta | delta_mu, delta_sigma);

  // Likelihood
  target += poisson_log_lpmf(O | lambda_p);
  target += poisson_lpmf(H | lambda_h); 
  target += poisson_lpmf(A | lambda_a);
}

generated quantities {
  array[N] int<lower=0> Yh;
  array[N] int<lower=0> Ya;
  array[N] int<lower=0> Ot = poisson_log_rng(lambda_p);
  for (n in 1:N) {
    for (i in 1:100) {
      array[2] real beta_i = normal_rng(rep_vector(0, 2), sigma_i);
      Yh[n] = poisson_log_rng(beta_h[n] + beta_i[1] + log(40 + Ot[n] * 5));
      Ya[n] = poisson_log_rng(beta_a[n] + beta_i[2] + log(40 + Ot[n] * 5));
      if (Yh[n] != Ya[n]) {
        break;
      }
    }
  }
}


