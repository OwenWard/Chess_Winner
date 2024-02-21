 // Model 3a, March 21st
// Very simple model for Chess data
// logistic regression where
// P(y_ij = 1) \propto 1/(1 + exp(-(alpha_j + beta_j x_{ij} + gamma z_{ij})))
// for game i by player j
// Random effects across both baseline (alpha) and winner effect (beta)
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
  array[J] real alpha;
  array[J] real beta;
  // real mu_alpha;
  real mu_beta;
  real gamma1;
  real gamma2;
  real <lower=0> sigma_alpha;
  real <lower=0> sigma_beta;
  real <lower=0> sigma_g1;
  real <lower=0> sigma_g2;
  real <lower=0> sigma_1;
}


model {
  mu_beta ~ normal(0, sigma_1);
  sigma_alpha ~ inv_gamma(1, 1);
  sigma_beta ~ inv_gamma(1, 1);
  sigma_g1 ~ inv_gamma(1, 1);
  sigma_g2 ~ inv_gamma(1, 1);
  alpha ~ normal(0, sigma_alpha);
  beta ~ normal(mu_beta, sigma_beta);
  gamma1 ~ normal(0, sigma_g1);
  gamma2 ~ normal(0, sigma_g2);
  sigma_1 ~ inv_gamma(1, 1);
  vector[N] pred;
  for(i in 1:N){
    pred[i] = alpha[id[i]] + beta[id[i]] * win_prop[i] +
    gamma1 * colour[i] + gamma2 * elo[i];
    // y[i] ~ bernoulli_logit(alpha[id[i]] + beta[id[i]] * win_prop[i] +
    // gamma1 * colour[i] + gamma2 * elo[i]);
  }
  y ~ bernoulli_logit(pred);
}



// generated quantities allows us to simulate from the fitted model
// compute log likelihood for model comparison

generated quantities {
  vector[N] log_lik;
  vector[N] y_rep;
  for (n in 1:N) {
    log_lik[n] = bernoulli_logit_lpmf(y[n] | alpha[id[n]] +
    beta[id[n]] * win_prop[n] +
    gamma1 * colour[n] + gamma2 * elo[n]);

    // generate posterior predictive samples
    y_rep[n] = bernoulli_logit_rng(alpha[id[n]] +
    beta[id[n]] * win_prop[n] +
    gamma1 * colour[n] + gamma2 * elo[n]);
  }
}
