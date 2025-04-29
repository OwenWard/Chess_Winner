##### March 5th 2024 ######
###########
## Fit the current model to a large selection of 
## games on a cluster
## stan model is somewhat optimized for speed to fit to faster, using 
## matrix multiplication
##



##### April 15, 2025 ######
#' Adam revision 
#' also assuming games played in different sessions have no relation, so make their history NULL (ie same as their historical avg past performance)
#' removing all the plots and stuff after we save the fit, we don't use that anymore



library(tidyverse)
library(RcppRoll)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(loo)
library(here)

options(mc.cores = parallel::detectCores())

## source helper functions for reading and transforming data
## along with defaults for plots, etc
source(here("utils/helper.R"))

path_id <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))

n <- 1 ## number of games to use for history

#what time control to fit model on
time_control <- "bullet" #takes on "bullet" or "blitz" values only

### load in the data to use

all_data_path <- rep(NA, 4)
all_save_path <- rep(NA, 4)
all_data_path[1] <- here("box_data", "lichess1700-1900", "/")
all_data_path[2] <- here("box_data", "lichess2000-2200", "/")
all_data_path[3] <- here("box_data", "lichess2300-2500", "/")
all_data_path[4] <- here("box_data", "lichessGrandmasters", "/")

all_save_path[1] <- here("results_revision/lichess1700-1900/")
all_save_path[2] <- here("results_revision/lichess2000-2200/")
all_save_path[3] <- here("results_revision/lichess2300-2500/")
all_save_path[4] <- here("results_revision/lichessGrandmasters/")


data_path <- all_data_path[path_id]
save_path <- all_save_path[path_id]

dir.create(save_path, showWarnings = FALSE)

files <- list.files(data_path)
print(files)
lichess_data <- files |> 
  map_dfr(~read_player(data_path, .x))


small_data <- lichess_data |>
  mutate(Event = tolower(Event)) |>
  filter(Event == paste0("rated ", time_control, " game"),
         Variant == "Standard") |>
  filter(TimeControl %in% c("60+0", "180+0")) |> 
  distinct() #remove the duplicate rows if they exist

## when players play less than 10 games
## otherwise not needed
users <- small_data |>
  group_by(Username) |>
  tally() |>
  filter(n >= 10) |>
  pull(Username)

saveRDS(users, file = paste0(save_path, paste0("users_", time_control, ".RDS")))

tidy_games <- map_dfr(users, get_hist, small_data, prev_n = n) |> 
  as_tibble() 

init_data <- tidy_games |>
  mutate(WhiteElo = as.numeric(WhiteElo),
         BlackElo = as.numeric(BlackElo),
         focal_user = ifelse(focal_white == 1, White, Black),
         elo_diff = ifelse(focal_white == 1,
                           WhiteElo - BlackElo, BlackElo - WhiteElo),
         focal_id = match(focal_user, users),
         UTCDateTime = ymd_hms(paste0(UTCDate, "_", UTCTime))) |>
  dplyr::select(focal_user, focal_id, focal_white, 
                focal_win_prop, elo_diff, focal_result,
                UTCDateTime) |>
  group_by(focal_id) |>
  mutate(time_diff = UTCDateTime - lag(UTCDateTime, default = NA), #default is to ensure first game is always start of a new session
         cum_win_prob = cummean(focal_result), #the mean win probability for the focal player up to the ith (current) game 
         ave_prop = ifelse(time_diff > 300 | is.na(time_diff),  
                           0, #if games played in different session, history is their mean win prob up to the current game
                           lag(focal_win_prop) - cum_win_prob)) |> #if game played in same session, rolling mean over the past n games
  filter(focal_result != 0.5) %>%
  ungroup()


### then fit the models

stan_data_ave <- list(N = nrow(init_data),
                      J = length(users),
                      y = init_data$focal_result,
                      id = init_data$focal_id,
                      colour = init_data$focal_white,
                      elo = init_data$elo_diff,
                      win_prop = init_data$ave_prop)


stan_file <- here("analysis_scripts", "final_model_scale_priors.stan")

mod <- cmdstan_model(stan_file)

fit <- mod$sample(data = stan_data_ave,
                  seed = 123,
                  chains = 4,
                  parallel_chains = 4,
                  refresh = 100)

#save fit
fit$save_object(file = here(save_path, paste0("all_rated_", time_control, "_model_n", n, ".RDS")))

