functions {
  #include /functions.stan
}

data {
  // Dimensions of the dataset
  int<lower=0> N;                    // Number of observations
  int<lower=0> T;                    // Number of teams

  // Observations
  array[2,N] int tid;                // Map observations to teams [home, away]
  array[2,N] int Y;                  // Final score [home, away]
  vector[N] O;                       // Number of overtimes per game
  vector[N] V;                       // Whether (1) or not (0) to apply home/away effects
  
  // Fixed values
  real alpha;                        // Log mean full time score
  
  // Priors
  vector[T] eta_o_mu;                // Offensive rating mean
  vector<lower=0>[T] eta_o_sigma;    // Offensive rating scale
  vector[T] eta_d_mu;                // Defensive rating mean
  vector<lower=0>[T] eta_d_sigma;    // Defensive rating scale
  vector[T] eta_h_mu;                // Home advantage mean
  vector<lower=0>[T] eta_h_sigma;    // Home advantage scale
  
  real log_sigma_o_mu;               // Log offensive scale mean
  real<lower=0> log_sigma_o_sigma;   // Log offensive scale scale
  real log_sigma_d_mu;               // Log defensive scale mean
  real<lower=0> log_sigma_d_sigma;   // Log defensive scale scale
  real log_sigma_h_mu;               // Log home advantage scale mean
  real<lower=0> log_sigma_h_sigma;   // Log home advantage scale scale
  
  real log_sigma_i_mu;               // Log overdispersion scale mean
  real<lower=0> log_sigma_i_sigma;   // Log overdispersion scale scale
  
  real gamma_0_mu;                   // Overtime hurdle slope mean
  real<lower=0> gamma_0_sigma;       // Overtime hurdle slope scale
  real delta_0_mu;                   // Overtime hurdle log intercept mean
  real<lower=0> delta_0_sigma;       // Overtime hurdle log intercept scale
  real gamma_ot_mu;                  // Overtime poisson slope mean
  real<lower=0> gamma_ot_sigma;      // Overtime poisson slope scale
  real delta_ot_mu;                  // Overtime poisson log intercept mean
  real<lower=0> delta_ot_sigma;      // Overtime poisson log intercept scale
  
  real<lower=0> beta_o_step_sigma;   // Random walk scale for offensive rating
  real<lower=0> beta_d_step_sigma;   // Random walk scale for defensive rating
  real<lower=0> beta_h_step_sigma;   // Random walk scale for home advantage
}

transformed data {
  vector[N] M = log(40 + O * 5);
  matrix[2,N] H = rep_matrix(0, 2, N);
  H[1,:] = to_row_vector(V);
}

parameters {
  // team rating parameters
  real log_sigma_o;                  // Offensive rating scale
  real log_sigma_d;                  // Defensive rating scale
  real log_sigma_h;                  // Home advantage scale
  vector[T] eta_o;                   // Offensive rating offset
  vector[T] eta_d;                   // Defensive rating offset
  vector[T] eta_h;                   // Home advantage offset
  
  // observation-level hierarchical parameters
  real log_sigma_i;                  // Overdispersion scale
  matrix[2,N] eta_i;                 // Game/team level overdispersion
  
  // overtime hurdle parameters
  real gamma_0;                      // Overtime hurdle slope
  real delta_0;                      // Overtime hurdle intercept
  real gamma_ot;                     // Overtime poisson slope
  real delta_ot;                     // Overtime poisson intercept
}

transformed parameters {
  // convert scale parameters to natural scale
  real<lower=0> sigma_o = exp(log_sigma_o);
  real<lower=0> sigma_d = exp(log_sigma_d);
  real<lower=0> sigma_h = exp(log_sigma_h);
  real<lower=0> sigma_i = exp(log_sigma_i);
  
  // evaluate hierarchical parameters
  vector[T] beta_o = eta_o * sigma_o;
  vector[T] beta_d = eta_d * sigma_d;
  vector[T] beta_h = eta_h * sigma_h;
  matrix[2,N] beta_i = eta_i * sigma_i;
  
  // map parameters to observations
  array[2] vector[N] log_mu = map_mu(alpha, beta_o, beta_d, beta_h, tid, H);
  array[2] vector[N] log_lambda = map_lambda(log_mu, beta_i, M);
  
  // hurdle over number of overtimes
  vector[N] theta = hurdle_probability(log_mu, gamma_0, delta_0);
  vector[N] log_lambda_t = overtime_poisson(log_mu, gamma_ot, delta_ot);
}

model {
  // priors over team rating parameters
  target += normal_lpdf(log_sigma_o | log_sigma_o_mu, log_sigma_o_sigma);
  target += normal_lpdf(log_sigma_d | log_sigma_d_mu, log_sigma_d_sigma);
  target += normal_lpdf(log_sigma_h | log_sigma_h_mu, log_sigma_h_sigma);
  target += normal_lpdf(eta_o | eta_o_mu, eta_o_sigma);
  target += normal_lpdf(eta_d | eta_d_mu, eta_d_sigma);
  target += normal_lpdf(eta_h | eta_h_mu, eta_h_sigma);
  
  // priors over observation-level hierarchical parameters
  target += normal_lpdf(log_sigma_i | log_sigma_i_mu, log_sigma_i_sigma);
  target += std_normal_lpdf(to_vector(eta_i));
  
  // priors over hurdle parameters
  target += normal_lpdf(gamma_0 | gamma_0_mu, gamma_0_sigma);
  target += normal_lpdf(delta_0 | delta_0_mu, delta_0_sigma);
  target += normal_lpdf(gamma_ot | gamma_ot_mu, gamma_ot_sigma);
  target += normal_lpdf(delta_ot | delta_ot_mu, delta_ot_sigma);

  // likelihood
  target += poisson_log_lpmf(Y | log_lambda);
  target += poisson_hurdle_lpmf(to_int(to_array_1d(O)) | theta, log_lambda_t);
}

generated quantities {
  // Estimate one-step-ahead parameters
  array[T] real beta_o_step = normal_rng(beta_o, beta_o_step_sigma);
  array[T] real beta_d_step = normal_rng(beta_d, beta_d_step_sigma);
  array[T] real beta_h_step = normal_rng(beta_h, beta_h_step_sigma);
}
