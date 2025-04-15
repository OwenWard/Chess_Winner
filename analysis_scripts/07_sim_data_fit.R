### April 10, 2025
#' Fitting our model on the simulated data
#' to be fit on the cluster in parallel
#' parallelize over 3 different values of number of games and the 3 different values of mu_beta (so 9 jobs)

library(tidyverse)
library(RcppRoll)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(loo)
library(here)

options(mc.cores = parallel::detectCores())

jobid = Sys.getenv("SLURM_ARRAY_TASK_ID") %>% as.numeric()

#source helper functions for reading and transforming data
source(here("utils/helper.R"))

data_path = "/home/adamgee/scratch/data/sim_data/"
save_path = "/home/adamgee/scratch/results/sim_data_fits/"
dir.create(save_path, showWarnings = FALSE) #create save path just in case its not there

#data_path = "C:/Users/adamg/Desktop/Chess_Winner/box_data/sim_data/"
files = list.files(data_path)

#10 players with 3 different values for mu_beta each playing 20,000 games
sim_data = files %>%
  map_dfr(~read.csv(paste0(data_path, .x)))


#max can be 20,000
num_games = c(1000, 5000, 20000)
mu_beta_values = sim_data$mu_beta %>% unique() %>% sort()

#mu_beta value and number of games to fit on in the current job
curr_num_games = num_games[((jobid - 1) %% length(num_games)) + 1]
curr_mu_beta = mu_beta_values[ceiling(jobid/length(mu_beta_values))]

#filter data for right parameter values and getting history
init_data = sim_data %>%
  filter(mu_beta == curr_mu_beta) %>% #filter mu_beta
  group_by(player_id) %>%
  slice_head(n = curr_num_games) #slice the correct number of games

#right now not standardizing, so history for prev_n = 1 is just 0, 0.5, 1...
stan_sim_data = list(N = nrow(init_data),
                     J = init_data$player_id %>% unique() %>% length(),
                     y = init_data$result,
                     id = init_data$player_id,
                     colour = init_data$colour,
                     elo = init_data$rating_diff,
                     win_prop = init_data$last_result)

### FITTING ###

stan_file = here("analysis_scripts", "final_model_scale_priors.stan")

mod = cmdstan_model(stan_file)

mod_fit = mod$sample(data = stan_sim_data,
                     seed = 123,
                     chains = 4,
                     parallel_chains = 4,
                     refresh = 100)

#save fit & dataset (so we can identify the corresponding fit)
mod_fit$save_object(file = paste0(save_path, "sim_fit_", jobid, ".RDS"))
write.csv(init_data, file = paste0(save_path, "sim_dataset_", jobid, ".csv"))

