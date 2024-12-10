functions {
  real half_normal_lpdf(real x,
                        real mu,
                        real sigma) {
    return normal_lpdf(x | mu, sigma) - normal_lccdf(0 | mu, sigma);
  }
}

data {
  // Dimensions of the dataset
  int<lower=0> N;                        // Number of observations (games)
  int<lower=0> T;                        // Number of teams
  
  // Game-level data
  array[N] int<lower=0> O;               // Number of overtimes per game
  vector<lower=0, upper=1>[N] V;         // Whether (1) or not (0) to apply a home-court advantage
  array[2,N] int<lower=0, upper=T> tid;  // Map team id to game [home, away]
  array[2,N] int<lower=0> S;             // Final score [home, away]
  
  // Fixed parameters
  real alpha;                            // Fixed intercept value for score (log scale)
  
  // Hierarchical priors
  real<lower=0> sigma_o_mu;              // Mean for offensive rating half-normal prior
  real<lower=0> sigma_o_sigma;           // Scale for offensive rating half-normal prior
  real<lower=0> sigma_d_mu;              // Mean for defensive rating half-normal prior
  real<lower=0> sigma_d_sigma;           // Scale for defensive rating half-normal prior
  real<lower=0> sigma_h_mu;              // Mean for home-court advantage half-normal prior
  real<lower=0> sigma_h_sigma;           // Scale for home-court advantage half-normal prior
  real<lower=0> sigma_i_mu;              // Mean for observation-level offset half-normal prior
  real<lower=0> sigma_i_sigma;           // Scale for observation-level offset half-normal prior
  
  // Fixed effect priors
  real gamma_mu;                         // Prior mean for effect of team ratings on overtimes
  real<lower=0> gamma_sigma;             // Prior scale for effect of team ratings on overtimes
  real delta_mu;                         // Prior mean for overtime intercept
  real<lower=0> delta_sigma;             // Prior scale for overtime intercept
}

transformed data {
  vector[N] M = log(40 + to_vector(O) * 5);
  matrix[2,N] H = rep_matrix(0, 2, N);
  H[1,:] = to_row_vector(V);
}

parameters {
  real gamma;
  real delta;
  vector[T] eta_o;
  vector[T] eta_d;
  vector[T] eta_h;
  array[2] vector[N] eta_i;
  real<lower=0> sigma_o;
  real<lower=0> sigma_d;
  real<lower=0> sigma_h;
  real<lower=0> sigma_i;
}

transformed parameters {
  vector[T] beta_o = eta_o * sigma_o;
  vector[T] beta_d = eta_d * sigma_d;
  vector[T] beta_h = eta_h * sigma_h;
  
  array[2] vector[N] beta_i;
  for (t in 1:2) {
    beta_i[t] = eta_i[t] * sigma_i;
  }
  
  array[2] vector[N] beta_t;
  for (n in 1:N) {
    for (t in 1:2) {
      beta_t[t,n] = alpha + beta_o[tid[t,n]] - beta_d[tid[2-t+1,n]] + beta_h[tid[t,n]] * H[t,n];
    }
  }
  
  array[2] vector[N] lambda;
  for (t in 1:2) {
    lambda[t] = beta_t[t] + beta_i[t] + M;
  }
  
  vector[N] lambda_p = gamma * abs(beta_t[1] - beta_t[2]) + delta;
}

model {
  // Priors
  target += std_normal_lpdf(eta_o);
  target += std_normal_lpdf(eta_d);
  target += std_normal_lpdf(eta_h);
  for (i in 1:2) {
    target += std_normal_lpdf(eta_i[i]);
  }
  target += half_normal_lpdf(sigma_o | sigma_o_mu, sigma_o_sigma);
  target += half_normal_lpdf(sigma_d | sigma_d_mu, sigma_d_sigma);
  target += half_normal_lpdf(sigma_h | sigma_h_mu, sigma_h_sigma);
  target += half_normal_lpdf(sigma_i | sigma_i_mu, sigma_i_sigma);
  target += normal_lpdf(gamma | gamma_mu, gamma_sigma);
  target += normal_lpdf(delta | delta_mu, delta_sigma);

  // Likelihood
  for (t in 1:2) {
    target += poisson_log_lpmf(S[t] | lambda[t]);
  }
  target += poisson_log_lpmf(O | lambda_p);
}

generated quantities {
  array[2,N] int<lower=0> Y;
  array[N] int<lower=0> Ot = poisson_log_rng(lambda_p);
  for(n in 1:N) {
    for (i in 1:100) {
      array[2] real beta_i_rep = normal_rng(rep_vector(0, 2), sigma_i);
      for (t in 1:2) {
        Y[t,n] = poisson_log_rng(beta_t[t,n] + beta_i_rep[t] + log(40 + Ot[n] * 5));
      }
      if (Y[1,n] != Y[2,n]) {
        break;
      }
    }
  }
}


