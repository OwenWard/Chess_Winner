// Comp Model 2, May 23rd
// Very simple model for Chess data, trying to match Slide 7 of what James shared
// logistic regression where
// P(y_ij = 1) \propto 1/(1 + exp(-(alpha_focal + alpha_opp +
// beta_focal x_{focal_prev} + beta_opp x_{opp_prev} + 
// gamma_focal x_{focal_dev} + gamma_opp x_{opp dev} )))
// for game i by player j
// Random effects across baseline of focal player (alpha)
// and fixed effect of prev game for focal (beta) and opponent (gamma)
// here these are scaled to relative difference from global average
// for each player

// May 26th, modify this to add generated quantities also



// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N; // the number of games
  int<lower=0> J; // the number of players
  array[N] int<lower=0, upper=1> y; // the outcome of each game
  array[N] int<lower=1, upper=J> focal_id; // indicating which focal player involved
  array[N] int<lower=1, upper=J> opp_id;
  array[N] real<lower=-1, upper=1> focal_prev; // focals prev result
  array[N] real<lower=-1, upper=1> opp_prev; // opponents prev result, if known
  array[N] real<lower=-1, upper=1> focal_avg; // overall average for focal
  array[N] real<lower=-1, upper=1> opp_avg; // overall average for opponent
}

// The parameters accepted by the model.
parameters {
  array[J] real alpha; // overall random effects
  array[J] real beta;  // coefficient for previous win
  array[J] real gamma; // coefficient for deviation from overall avg
  real mu1;
  real tau1;
  real mu2;
  real tau2;
  real mu3;
  real tau3;
}


model {
  mu1 ~ normal(0, 5);
  mu2 ~ normal(0, 5);
  mu3 ~ normal(0, 5);
  tau1 ~ cauchy(0, 5);
  tau2 ~ cauchy(0, 5);
  tau3 ~ cauchy(0, 5);
  alpha ~ normal(mu1, tau1);
  beta ~ normal(mu2, tau2);
  gamma ~ normal(mu3, tau3);
  vector[N] pred;
  for(i in 1:N){
    pred[i] = alpha[focal_id[i]] + alpha[opp_id[i]] + beta[focal_id[i]] * 
    focal_avg[i] + gamma[focal_id[i]] * (focal_prev[i] - focal_avg[i]) +
    beta[opp_id[i]] * opp_avg[i] + gamma[opp_id[i]] * (opp_prev[i] - 
    opp_avg[i]);
  }
  y ~ bernoulli_logit(pred);
}


generated quantities {
  vector[N] log_lik;
  vector[N] y_rep;
  for (i in 1:N) {
    log_lik[i] = bernoulli_logit_lpmf(y[i] | alpha[focal_id[i]] + 
    alpha[opp_id[i]] + beta[focal_id[i]] * 
    focal_avg[i] + gamma[focal_id[i]] * (focal_prev[i] - focal_avg[i]) +
    beta[opp_id[i]] * opp_avg[i] + gamma[opp_id[i]] * (opp_prev[i] - 
    opp_avg[i]));
    y_rep[i] = bernoulli_logit_rng(alpha[focal_id[i]] + alpha[opp_id[i]] + beta[focal_id[i]] * 
    focal_avg[i] + gamma[focal_id[i]] * (focal_prev[i] - focal_avg[i]) +
    beta[opp_id[i]] * opp_avg[i] + gamma[opp_id[i]] * (opp_prev[i] - 
    opp_avg[i]));
  }
}
