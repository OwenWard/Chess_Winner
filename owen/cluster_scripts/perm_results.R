#### September 5th 2023

## Considering a permutation test for evaluating the robustness of
## the results to serial correlations in the data


## We consider this first for the subset of data consisting of 18 focal players

library(cmdstanr)

options(mc.cores = parallel::detectCores())

library(tidyverse)
library(here)
library(loo)
library(ggplot2)
library(posterior)


jobid <- Sys.getenv("SLURM_ARRAY_TASK_ID")
jobid <- as.numeric(jobid)
sim_id <- jobid

lichess_data <- readRDS(here("rdata", "lichess_pilot.RData"))

lichess_data %>% 
  filter(Event == "Rated Bullet game") %>% 
  group_by(TimeControl) %>% 
  count() %>% 
  arrange(-n)


bullet_60 <- lichess_data %>% 
  filter(Event == "Rated Bullet game") %>% 
  filter(TimeControl == "60+0") %>% 
  mutate(game_id = row_number()) 



focal_players <- head(sort(table(c(bullet_60$Username)), decreasing = TRUE), 18)


top_players <- names(focal_players)


## take only the games involving focal players, confirm this
## this identical to original data
## then permute the results

focal_games <- bullet_60 %>% 
  filter(White %in% top_players | Black %in% top_players) %>% 
  mutate(Result = sample(Result))

get_hist <- function(user, games, prev_n) {
  hist_games <- games %>% 
    filter(White == user | Black == user) %>% 
    arrange(UTCDate, UTCTime) %>% 
    mutate(focal_white = ifelse(Username == White, 1, 0)) %>% 
    select(White:BlackElo, focal_white) %>% 
    mutate(focal_result = case_when(
      (focal_white == 1 & Result == "1-0") ~ 1,
      (focal_white == 0 & Result == "0-1") ~ 1,
      (Result == "1/2-1/2") ~ 0.5,
      .default = 0
    )) %>% 
    mutate(focal_win_prop = c(cumsum(focal_result[1:(prev_n - 1)])/(1:(prev_n -1)), 
                              roll_mean(focal_result, n = prev_n)))
  
  hist_games
}

tidy_games <- map_dfr(top_players, get_hist, focal_games, prev_n = 10) %>% 
  as_tibble()


init_data <- tidy_games %>% 
  mutate(WhiteElo = as.numeric(WhiteElo), 
         BlackElo = as.numeric(BlackElo)) %>% 
  mutate(focal_user = ifelse(focal_white == 1, White, Black)) %>% 
  mutate(elo_diff = ifelse(focal_white == 1, 
                           WhiteElo - BlackElo, BlackElo - WhiteElo)) %>% 
  mutate(focal_id = match(focal_user, top_players)) %>% 
  select(focal_user, focal_id, focal_white, 
         focal_win_prop, elo_diff, focal_result) %>% 
  group_by(focal_id) %>% 
  mutate(ave_prop = lag(focal_win_prop, default = 0) - mean(focal_win_prop)) %>% 
  filter(focal_result != 0.5)



stan_data_ave <- list(N = nrow(init_data),
                      J = length(top_players),
                      y = init_data$focal_result,
                      id = init_data$focal_id,
                      colour = init_data$focal_white,
                      elo = init_data$elo_diff,
                      win_prop = init_data$ave_prop)


stan_file <- "owen/model3.stan"

mod <- cmdstan_model(stan_file)

fit3_ave <- mod$sample(data = stan_data_ave,
                       seed = 123,
                       chains = 4,
                       parallel_chains = 4,
                       refresh = 100)


## then save the summaries of these models, add a column corresponding
## to the simulation number


par_ests <- fit3_ave$summary(c("alpha", "beta", "gamma1", "gamma2",
                   "mu1", "mu2", "tau1", "tau2")) %>% 
  mutate(sim_id = sim_id)


saveRDS(par_ests, file = here("owen", "cluster_scripts", "perm_outputs",
                              paste0("perm_", sim_id, "_sep_5.RDS")))
