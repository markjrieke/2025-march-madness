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
  
  // Hurdle model priors (p(Ot=0) = p(regulation))
  real gamma_0_mu;                       // Prior mean for effect of team ratings on p(regulation)
  real<lower=0> gamma_0_sigma;           // Prior scale for effect of team ratings on p(regulation)
  real delta_0_mu;                       // Prior mean for p(regulation) intercept
  real<lower=0> delta_0_sigma;           // Prior scale for p(regulation) intercept
  
  // Hurdle model priors (p(Ot>0))
  real gamma_t_mu;                       // Prior mean for effect of team ratings on overtimes
  real<lower=0> gamma_t_sigma;           // Prior scale for effect of team ratings on overtimes
  real delta_t_mu;                       // Prior mean for overtime intercept
  real<lower=0> delta_t_sigma;           // Prior scale for overtime intercept
}

transformed data {
  vector[N] M = log(40 + to_vector(O) * 5);
  matrix[2,N] H = rep_matrix(0, 2, N);
  H[1,:] = to_row_vector(V);
}

parameters {
  real gamma_0;
  real delta_0;
  real gamma_t;
  real delta_t;
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
  
  vector[N] lambda_t = gamma_t * abs(beta_t[1] - beta_t[2]) + delta_t;
  vector[N] theta = inv_logit(gamma_0 * abs(beta_t[1] - beta_t[2]) + delta_0);
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
  target += normal_lpdf(gamma_0 | gamma_0_mu, gamma_0_sigma);
  target += normal_lpdf(delta_0 | delta_0_mu, delta_0_sigma);
  target += normal_lpdf(gamma_t | gamma_t_mu, gamma_t_sigma);
  target += normal_lpdf(delta_t | delta_t_mu, delta_t_sigma);

  // Likelihood
  for (t in 1:2) {
    target += poisson_log_lpmf(S[t] | lambda[t]);
  }
  for (n in 1:N) {
    if (O[n] == 0) {
      target += log(theta[n]);
    } else {
      target += log1m(theta[n]) + poisson_log_lpmf(O[n] | lambda_t[n]) - poisson_lccdf(0 | exp(lambda_t[n]));
    }
  }
}

generated quantities {
  array[2,N] int<lower=0> Y;
  array[N] int<lower=0> Ot;
  for(n in 1:N) {
    
    // Hurdle for number of overtimes played
    if (bernoulli_rng(theta[n])) {
      Ot[n] = 0;
    } else {
      for (i in 1:2000) {
        Ot[n] = poisson_log_rng(lambda_t[n]);
        if (Ot[n] > 0) {
          break;
        }
        Ot[n] = 0;
      }
    }
    
    // Simulate team scores
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


