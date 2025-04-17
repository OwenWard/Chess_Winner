##### Dec 26 2023 ######
###########
## Fit the current model to a large selection of 
## games on a cluster
## here the game history used is only the result of the previous game,
## normalized to the difference relative to the total 
## proportion of games won by the player across the complete dataset
##


library(tidyverse)
library(RcppRoll)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(loo)
library(here)

options(mc.cores = parallel::detectCores())

source(here("utils", "helper.R"))

path_id <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))

n <- 1
### load in the data to use

## rerun for 2000-2200 next
all_data_path <- rep(NA, 4)
all_save_path <- rep(NA, 4)
all_data_path[1] <- here("box_data/lichess1700-1900/")
all_data_path[2] <- here("box_data/lichess2000-2200/")
all_data_path[3] <- here("box_data/lichess2300-2500/")
all_data_path[4] <- here("box_data/lichessGrandmasters/")
all_save_path[1] <- here("results_revision/lichess1700-1900/")
all_save_path[2] <- here("results_revision/lichess2000-2200/")
all_save_path[3] <- here("results_revision/lichess2300-2500/")
all_save_path[4] <- here("results_revision/lichessGrandmasters/")


data_path <- all_data_path[path_id]
save_path <- all_save_path[path_id]

dir.create(save_path, showWarnings = FALSE)

files <- list.files(data_path)
# files <- files[1:8]

lichess_data <- files |> 
  map_dfr(~read_player(data_path, .x))

## restrict to rated rapid and shorter here
## this also removes the NAs, which makes sense

small_data <- lichess_data |>
   # filter(Event == "Rated Bullet game") |>
   mutate(Event = tolower(Event)) |>
   filter(TimeControl == "60+0") |>
   filter(Variant == "Standard") |>
   filter(grepl("rated bullet game", Event))

#small_data <- lichess_data |>
#  mutate(Event = tolower(Event)) |>
#  filter(TimeControl == "180+0") |>
#  filter(Variant == "Standard") |>
#  filter(grepl("rated blitz game", Event))


users <- unique(small_data$Username)

## when players play less than 10 games
## otherwise not needed
users <- small_data |> 
  group_by(Username) |> 
  tally() |> 
  filter(n >= 10) |> 
  pull(Username)

saveRDS(users, file = paste0(save_path, "users_bullet.RDS"))
# saveRDS(users, file = paste0(save_path, "users_blitz.RDS"))

tidy_games <- map_dfr(users, get_hist, small_data, prev_n = 10) |>  
  as_tibble()

cat("----------\n")
print(dim(tidy_games))
cat("----------\n")

hist_data_init <- tidy_games |> 
  mutate(WhiteElo = as.numeric(WhiteElo), 
         BlackElo = as.numeric(BlackElo)) |> 
  mutate(focal_user = ifelse(focal_white == 1, White, Black)) |> 
  mutate(elo_diff = ifelse(focal_white == 1, 
                           WhiteElo - BlackElo, BlackElo - WhiteElo)) |> 
  mutate(focal_id = match(focal_user, users)) |> 
  select(focal_user, focal_id, focal_white, 
         focal_win_prop, elo_diff, focal_result) |> 
  group_by(focal_id) |> 
  mutate(ave_prop = lag(focal_win_prop, n = 1, default = 0) - 
           mean(focal_result), 
         ## focal_win_prop is the average of the running win proportion
         prev_game = lag(focal_result, default = 0)) |> # - mean(focal_result)) |> 
  filter(focal_result != 0.5)


cat("----------\n")
print(dim(hist_data_init))
cat("----------\n")

### then fit the models


stan_data_ave <- list(N = nrow(hist_data_init),
                      J = length(users),
                      y = hist_data_init$focal_result,
                      id = hist_data_init$focal_id,
                      colour = hist_data_init$focal_white,
                      elo = hist_data_init$elo_diff,
                      win_prop = hist_data_init$prev_game)

## coding this in as the win prop now instead

stan_file <- here("analysis_scripts", "final_model_scale_priors.stan")

mod <- cmdstan_model(stan_file)

fit3_ave <- mod$sample(data = stan_data_ave,
                       seed = 123,
                       chains = 4,
                       parallel_chains = 4,
                       refresh = 100)

## save the stan fit as not actually that large here

fit3_ave$save_object(file = here(save_path,
                                 paste0("all_rated_bullet_model_prev_n",
                                        n, ".RDS")))
# fit3_ave$save_object(file = here(save_path,
#                                  paste0("all_rated_blitz_model_prev_n",
#                                         n, ".RDS")))

