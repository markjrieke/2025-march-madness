// Index values and observations.
data {
  int<lower = 1> N;               // Number of observations.
  int<lower = 1> K;               // Number of groups.
  vector[N] y;                    // Vector of observations.
  array[N] int<lower=1, upper=K> g; // Vector of group assignments.
}

// Parameters and hyperparameters.
parameters {
  real mu;                        // Mean of the population model.
  real<lower = 0> tau;            // Variance of the population model.
  vector[K] beta;                 // Vector of group intercepts.
  real<lower = 0> sigma;          // Variance of the likelihood.
}

// Hierarchical regression.
model {
  // Hyperpriors.
  mu ~ normal(0, 5);
  tau ~ normal(0, 5);

  // Prior.
  sigma ~ normal(0, 5);

  // Population model and likelihood.
  beta ~ normal(mu, tau);
  for (n in 1:N) {
    y[n] ~ normal(beta[g[n]], sigma);
  }
}
