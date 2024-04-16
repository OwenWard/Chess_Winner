##### January 15 2024 ######
###########
## Fit the current model to two sets of games from each player,
## their first n games and their last n games, to see how potential
## winner effects change
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
all_save_path[1] <- here("results/lichess1700-1900_mar/")

# all_save_path[1] <- here("results/Full_Fits/lichess1700-1900/")
# ## if need to run it locally

all_save_path[2] <- here("results/lichess2000-2200_mar/")
all_save_path[3] <- here("results/lichess2300-2500_mar/")


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

small_data <- lichess_data %>%
  # filter(Event == "Rated Bullet game") %>%
  filter(TimeControl == "60+0") %>%
  filter(Variant == "Standard") %>%
  filter(grepl("Rated Bullet game", Event))
# 
# small_data <- lichess_data %>%
#   # filter(Event == "Rated Bullet game") %>%
#   # filter(TimeControl == "60+0") %>%
#   filter(Variant == "Standard") %>%
#   filter(grepl("Rated Blitz game", Event))


## then identify data for start and finish

select_users <- small_data %>% 
  group_by(Username) %>% 
  tally() %>% 
  arrange(-n) %>% 
  slice_max(order = n, n = 10) %>% 
  pull(Username)


first_games <- small_data %>% 
  filter(Username %in% select_users) %>% 
  group_by(Username) %>% 
  arrange(UTCDate, UTCTime, .by_group = TRUE) %>% 
  slice_head(n = 1000) %>% 
  ungroup()

last_games <- small_data %>% 
  filter(Username %in% select_users) %>% 
  group_by(Username) %>% 
  arrange(UTCDate, UTCTime, .by_group = TRUE) %>% 
  slice_tail(n = 1000) %>% 
  ungroup()




users <- select_users

saveRDS(users, file = paste0(save_path, "select_users_bullet.RDS"))

## fit to the first 1000 games

tidy_games <- map_dfr(users, get_hist, first_games, prev_n = 10) %>% 
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


## then fit to the first half
stan_data_ave_first <- list(N = nrow(init_data),
                      J = length(users),
                      y = init_data$focal_result,
                      id = init_data$focal_id,
                      colour = init_data$focal_white,
                      elo = init_data$elo_diff,
                      win_prop = init_data$ave_prop)


stan_file <- here("owen", "cluster_scripts", "model_feb29_small.stan")

mod <- cmdstan_model(stan_file)

fit3_first <- mod$sample(data = stan_data_ave_first,
                       seed = 123,
                       chains = 4,
                       parallel_chains = 4,
                       refresh = 100)


## save the stan fit as not actually that large here
# fit3_ave$save_object(file = here(save_path, "all_rated_bullet_model.RDS"))


## then repeat for the last games for each
tidy_games <- map_dfr(users, get_hist, last_games, prev_n = 10) %>% 
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



fit3_last <- mod$sample(data = stan_data_ave_last,
                         seed = 123,
                         chains = 4,
                         parallel_chains = 4,
                         refresh = 100)


#### up to date down as far as here ####


fit3_first$summary()


fit3_last$summary()


## plot these histograms together, from first and last side by side


global_winner <- as_draws_df(fit3_first$draws(variables = "mu_beta")) %>% 
  rename("Initial Winner" = mu_beta) %>% 
  bind_cols(as_draws_df(fit3_last$draws(variables = "mu_beta")) %>% 
              rename("Later Winner" = mu_beta)) %>% 
  select(`Initial Winner`, `Later Winner`) %>% 
  pivot_longer(cols = everything()) %>% 
  ggplot(aes(value, fill = name)) +
  geom_histogram(position = "identity") +
  facet_wrap(~name, ncol = 1) +
  labs(fill = element_blank(), y = element_blank(), x = element_blank())

## basically no difference between them here...

## then try to repeat this for the winner effects 

## do this for the individual winner effect estimates, and
## the global parameter estimates, and then save the output



## indiv winner effects

random_effect_first <- fit3_first$draws() %>% as_draws_df() %>%
  select(starts_with("beta[")) %>%
  pivot_longer(cols = everything()) %>%
  mutate(param = stringr::str_extract(name, pattern = "\\d"),
         id = stringr::str_extract(name, pattern = "\\d+]"),
         id = stringr::str_replace(id, "\\]", ""),
         player_id = paste0("beta[", id, "]")) %>% 
  mutate(when = "First")

random_effect_last <- fit3_last$draws() %>% as_draws_df() %>%
  select(starts_with("beta[")) %>%
  pivot_longer(cols = everything()) %>%
  mutate(param = stringr::str_extract(name, pattern = "\\d"),
         id = stringr::str_extract(name, pattern = "\\d+]"),
         id = stringr::str_replace(id, "\\]", ""),
         player_id = paste0("beta[", id, "]")) %>% 
  mutate(when = "Last")



betas <- bind_rows(random_effect_first, random_effect_last) %>% 
  select(value, param, id, when)



winner_effect_plot <- betas %>% filter(param == 2) %>% 
  mutate(id = factor(id, levels = c(1:10))) %>% 
  ggplot(aes(value, fill = when)) +
  geom_histogram() +
  geom_vline(xintercept = 0, alpha = 0.5) +
  xlim(c(-1.5, 1.5)) +
  facet_grid(rows = vars(when), cols = vars(id), scales = "free") +
  labs(fill = element_blank(), y = element_blank(), x = element_blank()) +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank())


## some of these seem to vary from beginning to the end, slightly



## then some posterior predictive checking for both before and 
## after

fit_samples <- as_draws_df(fit3_first$draws())


fit_samp <- fit_samples %>% 
  select(!starts_with(c("log_lik", "yrep")))


y_rep <- fit_samples %>% select(starts_with("y_rep"))

rm(fit_samples)
rm(fit_samp)

### then want to join this with the correct input, i.e player, etc


y_rep_mod <- y_rep %>% 
  rownames_to_column() %>%  
  pivot_longer(-rowname) %>% 
  pivot_wider(names_from=rowname, values_from=value) %>% 
  ## remove the draws and chains here
  mutate(focal_id = stan_data_ave_first$id)

games_won <- y_rep_mod %>% 
  pivot_longer(cols = `1`:`4000`, names_to = "draw", values_to = "y") %>% 
  group_by(focal_id, draw) %>% 
  summarise(games_won = sum(y)) 


orig_data <- tibble(outcome = stan_data_ave_first$y,
                    focal_id = stan_data_ave_first$id)

orig_games_won <- orig_data %>% 
  group_by(focal_id) %>% 
  summarise(games_won = sum(outcome))


p1 <- games_won %>% 
  ggplot(aes(x = games_won)) +
  geom_histogram() +
  facet_wrap(~focal_id, scales = "free", ncol = 5) +
  geom_vline(data = orig_games_won, 
             mapping = aes(xintercept = games_won), col = "red") +
  labs(title = "First Games")

p1


## repeat for last games

fit_samples <- as_draws_df(fit3_last$draws())


fit_samp <- fit_samples %>% 
  select(!starts_with(c("log_lik", "yrep")))


y_rep <- fit_samples %>% select(starts_with("y_rep"))

rm(fit_samples)
rm(fit_samp)

### then want to join this with the correct input, i.e player, etc


y_rep_mod <- y_rep %>% 
  rownames_to_column() %>%  
  pivot_longer(-rowname) %>% 
  pivot_wider(names_from=rowname, values_from=value) %>% 
  ## remove the draws and chains here
  mutate(focal_id = stan_data_ave_last$id)

games_won <- y_rep_mod %>% 
  pivot_longer(cols = `1`:`4000`, names_to = "draw", values_to = "y") %>% 
  group_by(focal_id, draw) %>% 
  summarise(games_won = sum(y)) 


orig_data <- tibble(outcome = stan_data_ave_last$y,
                    focal_id = stan_data_ave_last$id)

orig_games_won <- orig_data %>% 
  group_by(focal_id) %>% 
  summarise(games_won = sum(outcome))

p2 <- games_won %>% 
  ggplot(aes(x = games_won)) +
  geom_histogram() +
  facet_wrap(~focal_id, scales = "free", ncol = 5) +
  geom_vline(data = orig_games_won, 
             mapping = aes(xintercept = games_won), col = "red") +
  labs(title = "Last Games")

p2


## then save all this output and the plots

fit3_first$save_object(file = here(save_path, "blitz_first_games.RDS"))
fit3_last$save_object(file = here(save_path, "blitz_last_games.RDS"))

ggsave(plot = global_winner,
       filename = paste0(save_path, "/global_winner_first_last_blitz.png"),
       width = 8, height = 8, units = "in")

ggsave(plot = winner_effect_plot,
       filename = paste0(save_path, "/indiv_winner_first_last_blitz.png"),
       width = 12, height = 8, units = "in")

ggsave(plot = p1,
       filename = paste0(save_path, "/ppc_first_blitz.png"),
       width = 12, height = 8, units = "in")

ggsave(plot = p2,
       filename = paste0(save_path, "/ppc_last_blitz.png"),
       width = 12, height = 8, units = "in")


##############
##############
##############

## amount of variability on second half is very constant, 
## interesting


## look at some plots to see the difference

# p1 <- mcmc_hist(fit3_first$draws(variables = 
#                                    c("mu1", "mu2", "gamma1", "gamma2"))) +
#   labs(title = "Global Parameters First 1000 Games Each",
#        subtitle = "10 players 1700-1900")
# 
# ggsave(filename = 
#          "owen/Write Ups/figures/1700-1900-bullet-first1000-global.png",
#        plot = p1,
#        width = 8, height = 5, dpi = 200)
# 
# 
# p2 <- mcmc_hist(fit3_last$draws(variables = 
#                                    c("mu1", "mu2", "gamma1", "gamma2"))) +
#   labs(title = "Global Parameters Last 1000 Games Each",
#        subtitle = "10 players 1700-1900")
# 
# ggsave(filename = 
#          "owen/Write Ups/figures/1700-1900-bullet-last1000-global.png",
#        plot = p2,
#        width = 8, height = 5, dpi = 200)
# 
# 
# ## winner effects
# 
# players <- users
# names(players) <- paste0("beta[", 1:length(users), "]")
# 
# player_labels <- as_labeller(players)
# 
# p3 <- mcmc_hist(fit3_first$draws(variables = "beta"),
#   facet_args = list(labeller = player_labels, ncol = 5)) +
#   labs(title = "Winner Effects First 1000 Games Each",
#        subtitle = "10 players 1700-1900")
# 
# ggsave(filename = 
#          "owen/Write Ups/figures/1700-1900-bullet-first1000-winner.png",
#        plot = p3,
#        width = 8, height = 5, dpi = 200)
# 
# p4 <- mcmc_hist(fit3_last$draws(variables = "beta"),
#                 facet_args = list(labeller = player_labels, ncol = 5)) +
#   labs(title = "Winner Effects Last 1000 Games Each",
#        subtitle = "10 players 1700-1900")
# 
# ggsave(filename = 
#          "owen/Write Ups/figures/1700-1900-bullet-last1000-winner.png",
#        plot = p4,
#        width = 8, height = 5, dpi = 200)
# 
# 
# ## player effects
# 
# players <- users
# names(players) <- paste0("alpha[", 1:length(users), "]")
# 
# player_labels <- as_labeller(players)
# 
# p5 <- mcmc_hist(fit3_first$draws(variables = "alpha"),
#                 facet_args = list(labeller = player_labels, ncol = 5)) +
#   labs(title = "Indiv Effects First 1000 Games Each",
#        subtitle = "10 players 1700-1900")
# 
# ggsave(filename = 
#          "owen/Write Ups/figures/1700-1900-bullet-first1000-indiv.png",
#        plot = p5,
#        width = 8, height = 5, dpi = 200)
# 
# p6 <- mcmc_hist(fit3_last$draws(variables = "alpha"),
#                 facet_args = list(labeller = player_labels, ncol = 5)) +
#   labs(title = "Indiv Effects Last 1000 Games Each",
#        subtitle = "10 players 1700-1900")
# 
# ggsave(filename = 
#          "owen/Write Ups/figures/1700-1900-bullet-last1000-indiv.png",
#        plot = p6,
#        width = 8, height = 5, dpi = 200)
