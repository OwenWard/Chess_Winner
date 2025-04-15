#' April 8, 2025
#' 
#' Script for simulating data with varying experiential effect sizes and number of games per player
#' to test how well our model can actually capture this effects if they exist
#' to be run on cluster in parallel

library(tidyverse)
library(here)
library(rstanarm)
library(VGAM)

source(here("utils/helper.R"))

options(mc.cores = parallel::detectCores())

save_path = "/home/adamgee/scratch/results/sim_data/"

#' for running jobs in parallel 
#' each player should have their own job id
jobid = Sys.getenv("SLURM_ARRAY_TASK_ID") %>% as.numeric()

num_players = 10 #number of players in each dataset
player_id = (jobid %% num_players) + 1 #id of player in this job
num_games = 20000 #this is roughly the same as the number of games per player in 1700-1900 cohort

#' assume that the probability player j wins game i is p_ij = inv_logit(alpha_j + beta_j*x_ij + gamma_1*colour + gamma_2*rating_diff) 
#' ie our model is correct
#' also assume that we can't get any draws under our model
#' also technically assuming all of these games will be played in one session...

#' we vary the experiential effect size and number of games per player to see how well our model can detect true experiential effects
#' experiential effects size of logit(0.51) represents 1% increase win percentage when coming from a win... logit(0.53) represents 3% increase...
mu_beta_sizes = c(logit(0.51), logit(0.53), logit(0.6)) #these are the mu_betas

#' if we want to fit our model using smaller number of games, just take a subset of these 20,000 games
#' 
#' we can parallelize the mu_betas and the players
#' so each player will loop over 1 -> num_games for 3 different values of mu_beta
#' 
#' jobs (1 -> num_players) will be smallest mu_beta
#' jobs num_players + (1 -> num_players) will be middle mu_beta
#' jobs 2*num_players + (1 -> num_players) will be largest mu_beta
#' ie for 10 players.. 1-10 have small mu_beta, 11-20 have middle mu_beta, 21-30 have largest mu_beta


### values for other parameters

#population effects values - found from posterior means of original fit
gamma1 = 0.091 #colour effect
gamma2 = 0.0038 #rating effect

#varying player effect values
set.seed(2025)
alphas = rnorm(n = num_players, mean = -0.075, sd = 0.05) #roughly the dist of the alphas from 1700-1900 fit in paper - should be close enough
mu_beta = mu_beta_sizes[ceiling(jobid/num_players)] #mu_beta of current job
betas = mu_beta + rnorm(n = num_players, mean = 0, sd = 0.1) #mu_beta plus some noise to vary the betas, roughly the same as tau_2 in paper


#' now simulate the games for this player 
#' 
#' 
#' RIGHT NOW THIS IS USING NO STANDARISATION!!! JUST USES 0, 0.5, 1 AS HISTORY FOR BETA

#for storage
sim_games = data.frame("player_id" = as.numeric(), "result" = as.numeric(), "colour" = as.numeric(), 
                       "rating_diff" = as.numeric(), "last_result" = as.numeric(), "beta" = as.numeric(), 
                       "mu_beta" = as.numeric(), "alpha" = as.numeric(), "gamma1" = as.numeric(), "gamma2" = as.numeric())
for(i in 1:num_games) {
  #current covariate (game) info
  
  #laplace dist matches the rating diffs distribution in the original data set much better than normal 
  curr_rating_diff = rlaplace(n = 1, location = 0, scale = 30) #want sd ~ 60 (found from the sd of the original 1700-1900 rating diffs) - this means scale 30
  curr_colour = sample(c(0, 1), prob = c(0.5, 0.5), size = 1) #1 is white, 0 is black
  last_result = ifelse(i == 1, 0.5, sim_games$result[i - 1]) #assume first game history is same as draw
  
  #sim result
  lin_comb = alphas[player_id] + betas[player_id]*last_result + gamma1*curr_colour + gamma2*curr_rating_diff #the predictors
  curr_win_prob = invlogit(lin_comb) #simulate probability
  curr_result = sample(c(1, 0), prob = c(curr_win_prob, 1 - curr_win_prob), size = 1)
  
  #store everything - player number, result, colour, rating diff, last result, parameter values
  sim_games[i,] = c(player_id, curr_result, curr_colour, curr_rating_diff, last_result, 
                    betas[player_id], mu_beta, alphas[player_id], gamma1, gamma2) #the row for this data set
}

#save dataset
write.csv(sim_games, file = paste0(save_path, "sim_dataset_", jobid, ".csv"), row.names = FALSE)


