//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

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
  // real gamma1;
  // real gamma2;
  // real mu;
  // real tau;
}


model {
  alpha ~ normal(0, 5);
  beta ~ normal(0, 5);
  for(i in 1:N){
    y[i] ~ bernoulli_logit(alpha[id[i]] + beta[id[i]] * win_prop[i]);
  }
}

