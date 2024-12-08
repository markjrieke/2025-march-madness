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
  vector<lower=0>[N] O;
  vector<lower=0, upper=1>[N] U;

  // Scoreboard
  array[N] int<lower=0> H;
  array[N] int<lower=0> A;
  
  // Priors
  real alpha;
  // real alpha_mu;
  // real<lower=0> alpha_sigma;
  // real gamma_mu;
  // real<lower=0> gamma_sigma;
  real<lower=0> sigma_o_mu;
  real<lower=0> sigma_o_sigma;
  real<lower=0> sigma_d_mu;
  real<lower=0> sigma_d_sigma;
  real<lower=0> sigma_g_mu;
  real<lower=0> sigma_g_sigma;
}

parameters {
  // real alpha;
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
  
  vector<lower=0>[N] lambda_h = exp(beta_h) .* (40 + O * 5);
  vector<lower=0>[N] lambda_a = exp(beta_a) .* (40 + O * 5);
}

model {
  // Priors
  // target += normal_lpdf(alpha | alpha_mu, alpha_sigma);
  // target += normal_lpdf(gamma | gamma_mu, gamma_sigma);
  target += std_normal_lpdf(eta_o);
  target += std_normal_lpdf(eta_d);
  target += std_normal_lpdf(eta_g);
  target += half_normal_lpdf(sigma_o | sigma_o_mu, sigma_o_sigma);
  target += half_normal_lpdf(sigma_d | sigma_d_mu, sigma_d_sigma);
  target += half_normal_lpdf(sigma_g | sigma_g_mu, sigma_g_sigma);

  // Likelihood
  target += poisson_lpmf(H | lambda_h); 
  target += poisson_lpmf(A | lambda_a);
}

generated quantities {
  array[N] int<lower=0> Yh = poisson_log_rng(beta_h + log(40));
  array[N] int<lower=0> Ya = poisson_log_rng(beta_a + log(40));
  array[N] int<lower=0> Ot = rep_array(0, N);
  for (n in 1:N) {
    for (i in 1:100) {
      if (Yh[n] != Ya[n]) {
        break;
      } else {
        Yh[n] += poisson_log_rng(beta_h[n] + log(5));
        Ya[n] += poisson_log_rng(beta_a[n] + log(5));
        Ot[n] += 1;
      }
    }
  }
}


