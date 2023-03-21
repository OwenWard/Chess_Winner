// Model 3, March 21st
// Very simple model for Chess data
// logistic regression where
// P(y_ij = 1) \propto 1/(1 + exp(-(alpha_j + beta_j x_{ij} + gamma z_{ij})))
// for game i by player j
// Fixed effects across both baseline (alpha), with a hierarchical prior
// for effect of current streak (beta)
// gamma common effects which account for the colour of the focal player
// and the ELO difference between the focal and non focal player.
// Model 2 and Model 3 appear to give largely similar results

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
  real gamma1;
  real gamma2;
  real mu;
  real tau;
}


model {
  mu ~ normal(0, 5);
  tau ~ cauchy(0, 5);
  alpha ~ normal(0, 5);
  beta ~ normal(mu, tau);
  gamma1 ~ normal(0, 5);
  gamma2 ~ normal(0, 5);
  for(i in 1:N){
    y[i] ~ bernoulli_logit(alpha[id[i]] + beta[id[i]] * win_prop[i] +
    gamma1 * colour[i] + gamma2 * elo[i]);
  }
}

