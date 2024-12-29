functions {
  matrix covariance_matrix(real sigma_o,
                           real sigma_d,
                           real rho) {
    matrix[2,2] cov;
    cov[1,1] = sigma_o^2;
    cov[2,2] = sigma_d^2;
    cov[1,2] = sigma_o * sigma_d * rho;
    cov[2,1] = cov[1,2];
    
    return cov;
  }
  
  real expit(real x,
             real lower_bound,
             real upper_bound) {
    
    real y = 1/(1 + exp(-x));
    y *= (upper_bound - lower_bound);
    y += lower_bound;
    
    return y;
  }
  
  matrix random_walk(vector beta0,
                     matrix eta,
                     real sigma) {
    // instantiate random walk matrix
    int T = num_elements(beta0);
    int S = num_elements(eta[1,:]) + 1;
    matrix[T,S] beta;
    
    // build random walk
    beta[:,1] = beta0;
    beta[:,2:S] = eta * sigma;
    for (s in 2:S) {
      beta[:,s] += beta[:,s-1];
    }
    
    return beta;
  }
  
  array[] matrix random_walk(array[] vector beta0_od,
                             array[] matrix eta_od,
                             matrix Sigma_od) {
    // instantiate random walk matrices
    int T = num_elements(beta0_od[:,1]);
    int S = num_elements(eta_od[1,1,:]) + 1;
    array[T] matrix[2,S] beta_od;
    
    // convert covariance matrix to cholesky factor
    matrix[2,2] L = cholesky_decompose(Sigma_od);
    
    // build random walks
    for (t in 1:T) {
      beta_od[t,:,1] = beta0_od[t,:];
      beta_od[t,:,2:S] = L * eta_od[t];
      for (s in 2:S) {
        beta_od[t,:,s] += beta_od[t,:,s-1];
      }
    }
    
    return beta_od;
  }
  
  real poisson_log_lpmf(array[,] int Y,
                        array[] vector log_lambda) {
    int T = num_elements(log_lambda[:,1]);
    real lp = 0.0;
    for (t in 1:T) {
      lp += poisson_log_lpmf(Y[t,:] | log_lambda[t,:]);
    }
    
    return lp;
  }
  
  array[] vector map_lambda(real alpha,
                            array[] matrix beta_od,
                            matrix beta_h,
                            array[,] int tid,
                            array[] int sid,
                            vector M,
                            matrix H) {
    // convert per-minute to per-game
    int N = num_elements(sid);
    int T = num_elements(beta_h[:,1]);
    int S = num_elements(beta_h[1,:]);

    // map team-per-game to observations
    array[2] vector[N] log_lambda;
    for (t in 1:2) {
      for (n in 1:N) {
        log_lambda[t,n] = alpha
                        + beta_od[tid[t,n], t, sid[n]]
                        - beta_od[tid[2-t+1,n], 2-t+1, sid[n]]
                        + beta_h[tid[t,n], sid[n]] * H[t,n];
      }
      log_lambda[t,:] += M;
    }
    
    return log_lambda;
  }
  
}

data {
  int<lower=0> N;                  // Number of observations
  int<lower=0> T;                  // Number of teams
  int<lower=1> S;                  // Number of seasons
  int<lower=1> P;                  // Number of model parameters
  
  array[N] int sid;                // Map observations to seasons
  array[2,N] int tid;              // Map observations to teams [home, away]
  array[2,N] int Y;                // Final score [home, away]
  vector[N] O;                     // Number of overtimes per game
  vector[N] V;                     // Whether (1) or not (0) to apply the home advantage
  
  real alpha;                      // Log mean full time score
  
  real logit_rho_mu;               // Unconstrained offense/defense covariance mean
  real<lower=0> logit_rho_sigma;   // Unconstrained offense/defense covariance scale
  real log_sigma_o_mu;             // Log offensive random walk scale mean
  real<lower=0> log_sigma_o_sigma; // Log offensive random walk scale scale
  real log_sigma_d_mu;             // Log defensive random walk scale mean
  real<lower=0> log_sigma_d_sigma; // Log defensive random walk scale scale
  real log_sigma_h_mu;             // Log home advantage random walk scale mean
  real<lower=0> log_sigma_h_sigma; // Log home advantage random walk scale scale
  real beta0_h_mu;                 // Initial home advantage mean
  real<lower=0> beta0_h_sigma;     // Initial home advantage scale
  real beta0_od_mu;                // Initial offense/defense mean
  real<lower=0> beta0_od_sigma;    // Initial offense/defense scale
}

transformed data {
  vector[N] M = log(40 + O * 5);
  matrix[2,N] H = rep_matrix(0, 2, N);
  H[1,:] = to_row_vector(V);
}

parameters {
  real logit_rho;
  real log_sigma_o;
  real log_sigma_d;
  real log_sigma_h;
  vector[T] beta0_h;
  matrix[T,S-1] eta_h;
  array[T] vector[2] beta0_od;
  array[T] matrix[2,S-1] eta_od;

}

transformed parameters {
  // separate type declarations so I can profile stan
  real<lower=0> sigma_o;
  real<lower=0> sigma_d;
  real<lower=0> sigma_h;
  real<lower=-1, upper=1> rho;
  matrix[2,2] Sigma_od;
  array[T] matrix[2,S] beta_od;
  matrix[T,S] beta_h;
  array[2] vector[N] log_lambda;
  
  // convert random walk scale parameters to natural scale
  profile("exp") {
    sigma_o = exp(log_sigma_o);
    sigma_d = exp(log_sigma_d);
    sigma_h = exp(log_sigma_h);
  }

  // create cholesky decomposition of covariance matrix
  profile("expit") {
    rho = expit(logit_rho, -1, 1);
  }
  profile("covariance_matrix") {
    Sigma_od = covariance_matrix(sigma_o, sigma_d, rho);
  }
  
  // evaluate random walk
  profile("random_walk (array[] matrix)") {
    beta_od = random_walk(beta0_od, eta_od, Sigma_od);
  }
  profile("random_walk (matrix)") {
    beta_h = random_walk(beta0_h, eta_h, sigma_h);
  }
  
  // map parameters to observations
  profile("map_lambda") {
    log_lambda = map_lambda(alpha, beta_od, beta_h, tid, sid, M, H);
  }
}

model {
  // priors
  profile("priors") {
    target += normal_lpdf(logit_rho | logit_rho_mu, logit_rho_sigma);
    target += normal_lpdf(log_sigma_o | log_sigma_o_mu, log_sigma_o_sigma);
    target += normal_lpdf(log_sigma_d | log_sigma_d_mu, log_sigma_d_sigma);
    target += normal_lpdf(log_sigma_h | log_sigma_h_mu, log_sigma_h_sigma);
    target += normal_lpdf(beta0_h | beta0_h_mu, beta0_h_sigma);
    target += std_normal_lpdf(to_vector(eta_h));
    for (t in 1:T) {
      target += normal_lpdf(beta0_od[t] | beta0_od_mu, beta0_od_sigma);
      target += std_normal_lpdf(to_vector(eta_od[t]));
    }
  }
  
  // likelihood
  profile("likelihood") {
    target += poisson_log_lpmf(Y | log_lambda);
  }
}

generated quantities {
  // add from dev 19 when ready
  array[2,N] int Y_rep;
  profile("generated quantities") {
    for (t in 1:2) {
      Y_rep[t,:] = poisson_log_rng(log_lambda[t,:]);
    }
  }
  
}
