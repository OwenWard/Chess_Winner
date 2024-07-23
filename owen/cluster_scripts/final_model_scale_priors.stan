// Model February 29th
// Current model for Chess data
// logistic regression where
// P(y_ij = 1) \propto 1/(1 + exp(-(alpha_j + beta_j x_{ij} + gamma z_{ij})))
// for game i by player j
// Random effects across both baseline (alpha) and winner effect (beta)
// with covariance structure
// gamma common fixed effects which account for the colour of the focal player
// and the ELO difference between the focal and non focal player.
// here x_{ij} is the win percentage over previous N games, normalised
// relative to overall win percentage of that player
// in the model below beta corresponds to both alpha and beta as
// defined above
// For efficiency and numerical stability, the covariance and
// correlation matrices are Cholesky factored.


data {
  int<lower=0> N;                      // the number of games
  int<lower=0> J;                      // the number of focal players
  array[N] int<lower=0, upper=1> y;    // the outcome of each game
  array[N] int<lower=1, upper = J> id; // indicating focal player involved
  vector[N] colour;                    // the colour of the focal player
  vector[N] elo;                       // diff in elo scores between players
  vector[N] win_prop;                  // current win ratio for focal
  
}


parameters {
  real mu_beta;                        // population average winner effect
  vector[2] nu;                        // location of beta[ , j]
  vector<lower=0>[2] tau;              // scale of beta[ , j], sd of effects
  cholesky_factor_corr[2] L_Omega;     // Cholesky of correlation of beta[ , j]
  matrix[2, J] beta_std;               // standard beta (beta - nu) / Sigma
  real<lower=0> sigma_1;               // sd of mu_beta
  real<lower=0> sigma_g1;              // sd of gamma1
  real<lower=0> sigma_g2;              // sd of gamma2
  real gamma1;                         // effect of colour
  real gamma2;                         // effect of elo difference
}
transformed parameters {
  matrix[2, J] beta = rep_matrix(nu, J)
                      + diag_pre_multiply(tau, L_Omega) * beta_std;
}

model {
  mu_beta ~ normal(0, sigma_1);        // prior for population winner effect
  sigma_g1 ~ normal(0, 1);          // prior for sd of gamma1
  sigma_g2 ~ normal(0, 1);          // prior for sd of gamma2
  gamma1 ~ normal(0, sigma_g1);        // prior for gamma1
  gamma2 ~ normal(0, sigma_g2);        // prior for gamma2
  sigma_1 ~ normal(0, 1);
  nu[1] ~ normal(0, 1);
  nu[2] ~ normal(mu_beta, 1);          // standardized so sds here fixed
  mu_beta ~ normal(0, 1);              // hyperprior for mu_beta
  tau ~ inv_gamma(1, 1);               // prior for sd of both random effects
  L_Omega ~ lkj_corr_cholesky(2);      // prior for correlation matrix
  to_vector(beta_std) ~ normal(0, 1);  // beta[ , j] ~ multi_normal(nu, Sigma)
  vector[N] pred;
  for(i in 1:N){
    pred[i] = beta[1, id[i]] + beta[2, id[i]] * win_prop[i] +
    gamma1 * colour[i] + gamma2 * elo[i];
  }
  y ~ bernoulli_logit(pred);
}


/**
* generated quantities below compute posterior predictive draws and
* log likelihood for computing and comparing loo scores
* for full data this is too large to store, can only be run sometimes
**/



// generated quantities {
//   vector[N] log_lik;
//   vector[N] y_rep;
//   for (n in 1:N) {
//     log_lik[n] = bernoulli_logit_lpmf(y[n] | beta[1, id[n]] +
//     beta[2, id[n]] * win_prop[n] +
//     gamma1 * colour[n] + gamma2 * elo[n]);
// 
//     // generate posterior predictive samples
//     y_rep[n] = bernoulli_logit_rng(beta[1, id[n]] +
//     beta[2, id[n]] * win_prop[n] +
//     gamma1 * colour[n] + gamma2 * elo[n]);
//   }
// }
