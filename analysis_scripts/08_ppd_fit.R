##### April 24th 2025 ######
###########
# Fit model on the previous 1000 games to the final 1000 games for each player
# These fits will be used to generate the PPD plots

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
  # filter(Event == "Rated Bullet game") |>
  filter(TimeControl == "60+0") |>
  filter(Variant == "Standard") |>
  filter(grepl("rated bullet game", Event)) |>
  distinct() #remove the duplicate rows if they exist

#selecting previous 1000 to the final 1000 games for each player
mid_games <- small_data |> 
  filter(Username %in% users) |> 
  group_by(Username) |> 
  arrange(UTCDate, UTCTime, .by_group = TRUE) |> 
  slice_tail(n = 2000) |> 
  slice_head(n = 1000) |> 
  ungroup()

## when players play less than 10 games
## otherwise not needed
users <- mid_games |>
  group_by(Username) |>
  tally() |>
  filter(n >= 10) |>
  pull(Username)

tidy_games <- map_dfr(users, get_hist, mid_games, prev_n = n) |> 
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
                           cum_win_prob, #if games played in different session, history is their mean win prob up to the current game
                           lag(focal_win_prop))) |> #if game played in same session, rolling mean over the past n games, removing standardization 
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
fit$save_object(file = here(save_path,
                            paste0("mid_fit.RDS")))
