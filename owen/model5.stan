// Model 5, April 25th
// Very simple model for Chess data
// logistic regression where
// P(y_ij = 1) \propto 1/(1 + exp(-(alpha_j + beta_j x_{ij} + gamma z_{ij})))
// for game i by player j
// Fixed effects across both baseline (alpha), with a hierarchical prior
// for effect of current streak (beta)
// gamma common effects which account for the colour of the focal player
// and the ELO difference between the focal and non focal player.
// now x only single previous game



// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N; // the number of games
  int<lower=0> J; // the number of focal players
  array[N] int<lower=0, upper=1> y; // the outcome of each game
  array[N] int<lower=1, upper = J> id; // indicating which focal player involved
  vector[N] colour; // the colour of the focal player
  vector[N] elo; // diff in elo scores between players
  //vector[N] win_prop; // current win ratio for focal
  array[N] int<lower=0, upper=1> prev_game;
  
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  array[J] real alpha;
  // array[J] real beta;
  array[J] real delta; // the parameter for the single previous game
  real gamma1;
  real gamma2;
  // real mu1;
  // real tau1;
  real mu2;
  real tau2;
  real mu3;
  real tau3;
}


model {
  // mu1 ~ normal(0, 5);
  // tau1 ~ cauchy(0, 5);
  mu2 ~ normal(0, 5);
  tau2 ~ cauchy(0, 5);
  mu3 ~ normal(0, 5);
  tau3 ~ normal(0, 5);
  alpha ~ normal(mu2, tau2);
  // beta ~ normal(mu1, tau1);
  delta ~ normal(mu3, tau3);
  gamma1 ~ normal(0, 5);
  gamma2 ~ normal(0, 5);
  vector[N] pred;
  for(i in 1:N){
    pred[i] = alpha[id[i]] + //beta[id[i]] * win_prop[i] +
    gamma1 * colour[i] + gamma2 * elo[i] + delta[id[i]] * prev_game[i];
    // y[i] ~ bernoulli_logit(alpha[id[i]] + beta[id[i]] * win_prop[i] +
    // gamma1 * colour[i] + gamma2 * elo[i]);
  }
  y ~ bernoulli_logit(pred);
}


generated quantities {
  vector[N] log_lik;
  vector[N] y_rep;
  for (n in 1:N) {
    log_lik[n] = bernoulli_logit_lpmf(y[n] | alpha[id[n]] + 
    gamma1 * colour[n] + gamma2 * elo[n] + delta[id[n]] * prev_game[n]);
    y_rep[n] = bernoulli_logit_rng(alpha[id[n]] + 
    gamma1 * colour[n] + gamma2 * elo[n] + delta[id[n]] * prev_game[n]);
  }
}

