##### Dec 26 2023 ######
###########
## Fit the current model to a large selection of 
## games on a cluster
##


library(tidyverse)
library(RcppRoll)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(loo)
library(here)

options(mc.cores = parallel::detectCores())

theme_set(theme_bw())

path_id <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))


### load in the data to use

## rerun for 2000-2200 next
all_data_path <- rep(NA, 3)
all_save_path <- rep(NA, 3)
all_data_path[1] <- here("box_data/lichess1700-1900/")
all_data_path[2] <- here("box_data/lichess2000-2200/")
all_data_path[3] <- here("box_data/lichess2300-2500/")
all_save_path[1] <- here("results/lichess1700-1900/")
all_save_path[2] <- here("results/lichess2000-2200/")
all_save_path[3] <- here("results/lichess2300-2500/")


data_path <- all_data_path[path_id]
save_path <- all_save_path[path_id]

dir.create(save_path, showWarnings = FALSE)

files <- list.files(data_path)
# files <- files[1:8]

## need to write a function to process them separately then I think

read_player <- function(path, file){
  dat <- read_csv(file = paste0(data_path, file),
                  col_types = cols(UTCDate = col_date("%Y.%m.%d"),
                                   WhiteTitle = col_character(),
                                   BlackTitle = col_character(),
                                   WhiteElo = col_character(),
                                   BlackElo = col_character(),
                                   FEN = col_character())) %>% 
    select(Username, Event, White, Black, Result, UTCDate, UTCTime, 
           WhiteElo, BlackElo, Variant, TimeControl, Termination) %>% 
    mutate(WhiteElo = parse_number(if_else(WhiteElo == "?", NA, WhiteElo)),
           BlackElo = parse_number(if_else(BlackElo == "?", NA, BlackElo)))
  dat
}

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


lichess_data <- files %>% 
  map_dfr(~read_player(data_path, .x))




## restrict to rated rapid and shorter here
## this also removes the NAs, which makes sense

# small_data <- lichess_data %>%
#   # filter(Event == "Rated Bullet game") %>%
#   # filter(TimeControl == "60+0") %>%
#   filter(Variant == "Standard") %>%
#   filter(grepl("Rated Bullet game", Event))

small_data <- lichess_data %>%
  # filter(Event == "Rated Bullet game") %>%
  # filter(TimeControl == "60+0") %>%
  filter(Variant == "Standard") %>%
  filter(grepl("Rated Blitz game", Event))

users <- unique(small_data$Username)

## when players play less than 10 games
## otherwise not needed
users <- small_data %>% 
  group_by(Username) %>% 
  tally() %>% 
  filter(n >= 10) %>% 
  pull(Username)

tidy_games <- map_dfr(users, get_hist, small_data, prev_n = 10) %>% 
  as_tibble()



hist_data_init <- tidy_games %>% 
  mutate(WhiteElo = as.numeric(WhiteElo), 
         BlackElo = as.numeric(BlackElo)) %>% 
  mutate(focal_user = ifelse(focal_white == 1, White, Black)) %>% 
  mutate(elo_diff = ifelse(focal_white == 1, 
                           WhiteElo - BlackElo, BlackElo - WhiteElo)) %>% 
  mutate(focal_id = match(focal_user, users)) %>% 
  select(focal_user, focal_id, focal_white, 
         focal_win_prop, elo_diff, focal_result) %>% 
  group_by(focal_id) %>% 
  mutate(ave_prop = lag(focal_win_prop, n = 2, default = 0) - 
           mean(focal_win_prop),
         prev_game = lag(focal_result, default = 0)) %>% 
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
                      prev_game = hist_data_init$prev_game)


stan_file <- here("owen", "model5.stan")

mod <- cmdstan_model(stan_file)

fit3_ave <- mod$sample(data = stan_data_ave,
                       seed = 123,
                       chains = 4,
                       parallel_chains = 4,
                       refresh = 100)


## save the stan fit as not actually that large here

# fit3_ave$save_object(file = here(save_path, "all_rated_bullet_model_prev.RDS"))
fit3_ave$save_object(file = here(save_path, "all_rated_blitz_model_prev.RDS"))

## create some summary plots of these results

players <- users
names(players) <- paste0("delta[", 1:length(users), "]")

player_labels <- as_labeller(players)

mcmc_hist(fit3_ave$draws(c("mu2", "mu1",  "gamma1", "gamma2")),
          facet_args = list(scales = "free"))

# ggsave(filename = paste0(save_path, "/global_pars_all_rated_bullet_model_prev.png"),
                         # width = 8, height = 8, units = "in")
ggsave(filename = paste0(save_path, "/global_pars_all_rated_blitz_model_prev.png"),
       width = 8, height = 8, units = "in")

mcmc_hist(fit3_ave$draws("delta"),
          facet_args = list(labeller = player_labels)) 

# ggsave(filename = paste0(save_path, "/winner_pars_all_rated_bullet_model_prev.png"),
#        width = 8, height = 8, units = "in")
ggsave(filename = paste0(save_path, "/winner_pars_all_rated_blitz_model_prev.png"),
       width = 8, height = 8, units = "in")

names(players) <- paste0("alpha[", 1:length(users), "]")
player_labels <- as_labeller(players)
mcmc_hist(fit3_ave$draws("alpha"),
          facet_args = list(labeller = player_labels))

# ggsave(filename = paste0(save_path, "/indiv_pars_all_rated_bullet_model_prev.png"),
#        width = 8, height = 8, units = "in")
ggsave(filename = paste0(save_path, "/indiv_pars_all_rated_blitz_model_prev.png"),
       width = 8, height = 8, units = "in")
