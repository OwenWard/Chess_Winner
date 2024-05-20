#### September 5th 2023
#### Updated May 13th 2024

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
library(RcppRoll)

source(here("analysis/helper.R"))

jobid <- Sys.getenv("SLURM_ARRAY_TASK_ID")
jobid <- as.numeric(jobid)
sim_id <- jobid


all_data_path <- rep(NA, 4)
all_save_path <- rep(NA, 4)
all_data_path[1] <- here("box_data/lichess1700-1900/")
all_data_path[2] <- here("box_data/lichess2000-2200/")
all_data_path[3] <- here("box_data/lichess2300-2500/")
all_data_path[4] <- here("box_data/lichessGrandmasters/")
all_save_path[1] <- here("results/lichess1700-1900_mar/")

# all_save_path[1] <- here("results/Full_Fits/lichess1700-1900/")
# ## if need to run it locally

all_save_path[2] <- here("results/lichess2000-2200_mar/")
all_save_path[3] <- here("results/lichess2300-2500_mar/")
all_save_path[4] <- here("results/lichessGrandmasters_mar/")

path_id <- 2 ## setting it for now
data_path <- all_data_path[path_id]
save_path <- paste0(all_save_path[path_id], "perm/")

dir.create(save_path, showWarnings = FALSE)

files <- list.files(data_path)

lichess_data <- files %>% 
  map_dfr(~read_player(data_path, .x))


small_data <- lichess_data %>%
  filter(TimeControl == "60+0") %>%
  filter(Variant == "Standard") %>%
  filter(grepl("Rated Bullet game", Event))

rm(lichess_data)


select_users <- small_data %>% 
  group_by(Username) %>% 
  tally() %>% 
  arrange(-n) %>% 
  ## will change this to n = 20 when run on cluster potentially
  slice_max(order = n, n = 10) %>% 
  pull(Username)




## just going to use the last games for this
## permute within a player

last_games <- small_data %>% 
  filter(Username %in% select_users) %>% 
  group_by(Username) %>% 
  arrange(UTCDate, UTCTime, .by_group = TRUE) %>% 
  slice_tail(n = 1000) %>% 
  ungroup() %>% 
  group_by(Username) %>% 
  mutate(Result = sample(Result)) %>% 
  ungroup()


users <- select_users

tidy_games <- map_dfr(users, get_hist, last_games, prev_n = 1) %>% 
  as_tibble()



init_data <- tidy_games %>% 
  mutate(WhiteElo = as.numeric(WhiteElo), 
         BlackElo = as.numeric(BlackElo)) %>% 
  mutate(focal_user = ifelse(focal_white == 1, White, Black)) %>% 
  mutate(elo_diff = ifelse(focal_white == 1, 
                           WhiteElo - BlackElo, BlackElo - WhiteElo)) %>% 
  mutate(focal_id = match(focal_user, users)) %>% 
  select(focal_user, focal_id, focal_white, 
         focal_win_prop, elo_diff, focal_result) %>% 
  group_by(focal_id) %>% 
  mutate(ave_prop = lag(focal_win_prop, default = 0) - mean(focal_win_prop)) %>% 
  filter(focal_result != 0.5)

cat("----------\n")
print(dim(init_data))
cat("----------\n")


## then fit to the second half
stan_data_ave_last <- list(N = nrow(init_data),
                           J = length(users),
                           y = init_data$focal_result,
                           id = init_data$focal_id,
                           colour = init_data$focal_white,
                           elo = init_data$elo_diff,
                           win_prop = init_data$ave_prop)

stan_file <- here("owen", "cluster_scripts", "model_feb29_small.stan")

mod <- cmdstan_model(stan_file)

fit3_last <- mod$sample(data = stan_data_ave_last,
                        seed = 123,
                        chains = 4,
                        parallel_chains = 4,
                        refresh = 100)

## then figure out what parameters we want to keep and use them

par_ests <- fit3_last$summary(c("beta", "mu_beta", "gamma1", "gamma2")) %>% 
  mutate(sim_id = sim_id)


saveRDS(par_ests, file = paste0(save_path, "perm_", sim_id, ".RDS"))
