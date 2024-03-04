// Model February 29th
// Very simple model for Chess data
// logistic regression where
// P(y_ij = 1) \propto 1/(1 + exp(-(alpha_j + beta_j x_{ij} + gamma z_{ij})))
// for game i by player j
// Random effects across both baseline (alpha) and winner effect (beta)
// with covariance structure
// gamma common fixed effects which account for the colour of the focal player
// and the ELO difference between the focal and non focal player.

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N; // the number of games
  int<lower=0> J; // the number of focal players
  array[N] int<lower=0, upper=1> y; // the outcome of each game
  array[N] int<lower=1, upper = J> id; // indicating which focal player involved
  vector[N] colour; // the colour of the focal player
  vector[N] elo; // diff in elo scores between players
  vector[N] win_prop; // current win ratio for focal
  
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real mu_beta;
  vector[2] nu;                        // location of beta[ , j]
  vector<lower=0>[2] tau;              // scale of beta[ , j]
  cholesky_factor_corr[2] L_Omega;     // Cholesky of correlation of beta[ , j]
  matrix[2, J] beta_std;               // standard beta (beta - nu) / Sigma
  // real<lower=0> sigma;                 // observation error for y
}
transformed parameters {
  matrix[2, J] beta = rep_matrix(nu, J)
                      + diag_pre_multiply(tau, L_Omega) * beta_std;
}

model {
  // mu_beta ~ normal(0, sigma_1);
  // sigma_alpha ~ inv_gamma(1, 1);
  // sigma_beta ~ inv_gamma(1, 1);
  // sigma_g1 ~ inv_gamma(1, 1);
  // sigma_g2 ~ inv_gamma(1, 1);
  // alpha ~ normal(0, sigma_alpha);
  // beta ~ normal(mu_beta, sigma_beta);
  // gamma1 ~ normal(0, sigma_g1);
  // gamma2 ~ normal(0, sigma_g2);
  // sigma_1 ~ inv_gamma(1, 1);
  // L_Omega ~ lkj_corr_cholesky(2);
  nu[1] ~ normal(0, 1);
  nu[2] ~ normal(mu_beta, 1); // for example
  mu_beta ~ normal(0, 1);
  tau ~ inv_gamma(1, 1);
  L_Omega ~ lkj_corr_cholesky(2);
  to_vector(beta_std) ~ normal(0, 1);  // beta[ , j] ~ multi_normal(nu, Sigma)
  // sigma ~ exponential(1);
  vector[N] pred;
  for(i in 1:N){
    pred[i] = beta[1, id[i]] + beta[2, id[i]] * win_prop[i];// +
    // gamma1 * colour[i] + gamma2 * elo[i];
    // y[i] ~ bernoulli_logit(alpha[id[i]] + beta[id[i]] * win_prop[i] +
    // gamma1 * colour[i] + gamma2 * elo[i]);
  }
  y ~ bernoulli_logit(pred);
}



// generated quantities allows us to simulate from the fitted model
// compute log likelihood for model comparison

// generated quantities {
//   vector[N] log_lik;
//   vector[N] y_rep;
//   for (n in 1:N) {
//     log_lik[n] = bernoulli_logit_lpmf(y[n] | alpha[id[n]] +
//     beta[id[n]] * win_prop[n] +
//     gamma1 * colour[n] + gamma2 * elo[n]);
// 
//     // generate posterior predictive samples
//     y_rep[n] = bernoulli_logit_rng(alpha[id[n]] +
//     beta[id[n]] * win_prop[n] +
//     gamma1 * colour[n] + gamma2 * elo[n]);
//   }
// }
