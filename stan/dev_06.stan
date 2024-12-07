data {
  // Dimensions of the dataset
  int<lower=0> N;
  int<lower=0> T;
  
  // Mapping ids
  array[N,2] int<lower=0, upper=T> tid;
  
  // Covariates
  vector<lower=0>[N] O;

  // Scoreboard
  array[N] int<lower=0> H;
  array[N] int<lower=0> A;
  
  // Priors
  real alpha_mu;
  real<lower=0> alpha_sigma;
  real<lower=0> sigma_t_mu;
  real<lower=0> sigma_t_sigma;
}

parameters {
  real alpha;
  vector[T] eta_t;
  real<lower=0> sigma_t;
}

transformed parameters {
  vector[N] beta_h;
  vector[N] beta_a;
  for (n in 1:N) {
    beta_h[n] = eta_t[tid[n,1]];
    beta_a[n] = eta_t[tid[n,2]];
  }
  beta_h *= sigma_t;
  beta_a *= sigma_t;
  beta_h += alpha;
  beta_a += alpha;
  
  vector<lower=0>[N] lambda_h = exp(beta_h) .* (40 + O * 5);
  vector<lower=0>[N] lambda_a = exp(beta_a) .* (40 + O * 5);
}

model {
  // Priors
  target += normal_lpdf(alpha | alpha_mu, alpha_sigma);
  target += std_normal_lpdf(eta_t);
  target += normal_lpdf(sigma_t | sigma_t_mu, sigma_t_sigma) - normal_lccdf(0 | sigma_t_mu, sigma_t_sigma);
  
  // Likelihood
  target += poisson_lpmf(H | lambda_h); 
  target += poisson_lpmf(A | lambda_a);
}


