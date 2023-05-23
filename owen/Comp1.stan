// Comp Model 1, May 23rd
// Very simple model for Chess data, similar to Slide 6 of what James shared
// logistic regression where
// P(y_ij = 1) \propto 1/(1 + exp(-(alpha_j + beta_j x_{ij} + gamma_oj y_{ij})))
// for game i by player j
// Random effects across baseline of focal player (alpha)
// and fixed effect of prev game for focal (beta) and opponent (gamma)



// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N; // the number of games
  int<lower=0> J; // the number of focal players
  array[N] int<lower=0, upper=1> y; // the outcome of each game
  array[N] int<lower=1, upper = J> id; // indicating which focal player involved
  array[N] int<lower=0, upper =1> focal_prev; // focals prev result
  array[N] int<lower=0, upper=1> opp_prev; // opponents prev result, if known
}

// The parameters accepted by the model.
parameters {
  array[J] real alpha;
  array[J] real beta;
  real mu1;
  real tau1;
  real mu2;
  real tau2;
}


model {
  mu1 ~ normal(0, 5);
  mu2 ~ normal(0, 5);
  tau1 ~ cauchy(0, 5);
  tau2 ~ cauchy(0, 5);
  alpha ~ normal(mu1, tau1);
  beta ~ normal(mu2, tau2);
  vector[N] pred;
  for(i in 1:N){
    pred[i] = alpha[id[i]] + beta[id[i]] * focal_prev[i] + opp_prev[i];
  }
  y ~ bernoulli_logit(pred);
}

