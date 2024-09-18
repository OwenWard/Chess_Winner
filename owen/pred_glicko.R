##### trying to do the ppc for the lichess rating


library(PlayerRatings)
library(tidyverse)
library(here)
library(loo)
library(ggplot2)
library(posterior)
library(RcppRoll)

source(here("analysis/helper.R"))

## get some initial chess data, say the final 1000 games
## for 20 players and compute the ratings for that



all_data_path <- rep(NA, 4)
all_save_path <- rep(NA, 4)
all_data_path[1] <- here("box_data/lichess1700-1900/")
all_data_path[2] <- here("box_data/lichess2000-2200/")
all_data_path[3] <- here("box_data/lichess2300-2500/")
all_data_path[4] <- here("box_data/lichessGrandmasters/")
all_save_path[1] <- here("results/lichess1700-1900_test/")

# all_save_path[1] <- here("results/Full_Fits/lichess1700-1900/")
# ## if need to run it locally

all_save_path[2] <- here("results/lichess2000-2200_test/")
all_save_path[3] <- here("results/lichess2300-2500_test/")
all_save_path[4] <- here("results/lichessGrandmasters_test/")

path_id <- 1  ## setting it for now
data_path <- all_data_path[path_id]
save_path <- paste0(all_save_path[path_id], "perm/")


res_data_path <- rep(NA, 4)
res_data_path[1] <- here("results/lichess1700-1900/")
res_data_path[2] <- here("results/lichess2000-2200/")
res_data_path[3] <- here("results/lichess2300-2500/")
res_data_path[4] <- here("results/lichessGrandmasters/")

# dir.create(save_path, showWarnings = FALSE)

files <- list.files(data_path)

lichess_data <- files %>% 
  map_dfr(~read_player(data_path, .x))


small_data <- lichess_data %>%
  mutate(Event = tolower(Event)) |> 
  filter(TimeControl == "60+0") %>%
  filter(Variant == "Standard") %>%
  filter(grepl("rated bullet game", Event))

rm(lichess_data)



select_users <- small_data %>% 
  group_by(Username) %>% 
  tally() %>% 
  arrange(-n) %>% 
  slice_max(order = n, n = 10) %>% 
  pull(Username)


## just use the first for now
users <- select_users


last_games <- small_data %>% 
  filter(Username %in% users) %>% 
  group_by(Username) %>% 
  arrange(UTCDate, UTCTime, .by_group = TRUE) %>% 
  slice_tail(n = 1000) %>% 
  ungroup() %>% 
  group_by(Username) %>% 
  mutate(Result = sample(Result)) %>% 
  ungroup()


tidy_games <- map_dfr(users, get_hist, last_games, prev_n = 1) %>% 
  as_tibble()


init_data <- tidy_games %>% 
  mutate(WhiteElo = as.numeric(WhiteElo), 
         BlackElo = as.numeric(BlackElo)) %>% 
  mutate(focal_user = ifelse(focal_white == 1, White, Black)) %>% 
  mutate(elo_diff = ifelse(focal_white == 1, 
                           WhiteElo - BlackElo, BlackElo - WhiteElo)) %>% 
  mutate(focal_id = match(focal_user, users)) %>% 
  # select(focal_user, focal_id, focal_white, 
  #        focal_win_prop, elo_diff, focal_result) %>% 
  group_by(focal_id) %>%
  mutate(ave_prop = lag(focal_win_prop, default = 0) - mean(focal_win_prop)) %>%
  filter(focal_result != 0.5)



## need a function which will take in a set of games and results
## and return the corresponding glicko2 scores for the focal players

## first do it for a single player, might be easier
## don't know the deviation, volatility used by lichess so just need
## to guess these
## can take the true elo for opponents throughout, to make it closer?

## get starting rating of focal player also
## starting in the wrong place here
focal_init <- tidy_games |> 
  slice_head(n = 1) |>
  mutate(Player = ifelse(White %in% users, White, Black),
         Rating = ifelse(White %in% users, WhiteElo, BlackElo)) |> 
  select(Player, Rating)

init_data |> group_by(focal_user) |> 
  slice_head(n = 1) |> 
  arrange(focal_id)


## then just pick the player from this data
### need to know the focal id

focal_player_used <- 1

focal_init <- tibble(Player = "d8336",
                     Rating = 1881)

# init_data |> 
#   slice_head(n = 1) |> 
  ## then get the id of that user and use it below
  ## along with their starting elo
  ### get just the games from the last user,
  ## then their last 1001 games,
  ## then take the elo from the FIRST game from those


focal_user <- focal_init$Player

glick_data <- init_data |>
  filter(focal_user == focal_init$Player)
  # mutate(Username = focal_user) |> 
# |> 
#   filter(Username == focal_init$Player)


## first argument to glicko2
glick_part1 <- glick_data |> 
  arrange(UTCDate, UTCTime) |> 
  mutate(index = row_number()) |> 
  mutate(opp_elo = ifelse(focal_user == White, BlackElo, WhiteElo),
         opp = ifelse(focal_user == White, Black, White)) |> 
  select(index, focal_user, opp, focal_result, focal_white) |> 
  mutate(focal_white = ifelse(focal_white == 0, -1, focal_white))




## second argument, giving elo of all opponents and starting of first
glick_part2 <- glick_data |> 
  arrange(UTCDate, UTCTime) |> 
  mutate(index = row_number()) |> 
  mutate(opp_elo = ifelse(focal_user == White, BlackElo, WhiteElo),
         opp = ifelse(focal_user == White, Black, White)) |> 
  select(opp, opp_elo) |> 
  rename(Player = opp, Rating = opp_elo) 


glick_part2 <- bind_rows(focal_init, glick_part2) |> 
  mutate(Deviation = 50, Volatility = 0.05)



initstate <- glick_part2

games <- data.frame(Week = glick_part1$index,
                    Player1 = focal_init$Player,
                    Player2 = glick_part1$opp,
                    Score = glick_part1$focal_result)

a <- glicko2(games, status = initstate, history = TRUE, 
             gamma = glick_part1$focal_white)
a
focal_glick <- a$history[focal_init$Player, ,]

glick_scores <- as.numeric(focal_glick[, "Rating"])

plot(glick_scores, type = "l")




# first try match these to the lichess scores a little -------------------------

true_elo <- last_games |> 
  filter(Username == focal_init$Player) |>
  filter(Result != "1/2-1/2") |> 
  mutate(focal_elo = ifelse(Username == White, WhiteElo, BlackElo)) |> 
  mutate(index = row_number()) |> 
  select(index, focal_elo) |> 
  mutate(Version = "Truth")


true_elo |> 
  bind_rows(
    tibble(focal_elo = glick_scores, Version = "Est") |> 
      mutate(index = row_number())
  ) |> 
  ggplot(aes(index, focal_elo, colour = Version)) +
  geom_line()


## not super close but will have to do for now
## could be the missing draws which are messing it up here also

# then repeat using the simulations ---------------------------------------



## take in the simulation results and compare them here


data_path <- res_data_path[path_id]

fit_17_19_first <- readRDS(file =  here(data_path, "time",
                                       "select_users_first_bullet.RDS"))

fit_17_19_last <- readRDS(file =  here(data_path, "time",
                                       "select_users_last_bullet.RDS"))

fit_samples <- as_draws_df(fit_17_19_last$draws())

fit_samp <- fit_samples %>% 
  select(!starts_with(c("log_lik", "yrep")))

y_rep <- fit_samples %>% select(starts_with("y_rep"))

rm(fit_samples)
rm(fit_samp)

stan_data_ave_last <- readRDS(file = here(data_path, "time",
                                           "stan_data_last.RDS"))

y_rep_mod <- y_rep %>% 
  rownames_to_column() %>%  
  pivot_longer(-rowname) %>% 
  pivot_wider(names_from=rowname, values_from=value) %>% 
  ## remove the draws and chains here
  mutate(focal_id = stan_data_ave_last$id)


## single player

#### modify this if picking a different player!! #####
### check the corresponding id in select_users
### there are several places below which filter on the id == 1
### if using other id be careful

player_sims <- y_rep_mod %>% 
  pivot_longer(cols = `1`:`4000`, names_to = "draw", values_to = "y") |> 
  group_by(focal_id, draw) |> 
  mutate(game_id = row_number()) |> 
  filter(focal_id == focal_player_used)


player_sims |> 
  group_by(draw) |> 
  tally()


## need to make sure this is matching correctly to the right focal player

glicko_sims <- tibble()

for(i in 1:500){
  curr_result <- player_sims |> 
    filter(draw == i) |> 
    pull(y)
  
  ## then compute the glicko scores here 
  games <- data.frame(Week = glick_part1$index,
                      Player1 = focal_init$Player,
                      Player2 = glick_part1$opp,
                      Score = curr_result)
  
  sim_glick <- glicko2(games, status = initstate, history = TRUE, 
               gamma = glick_part1$focal_white)
  focal_glick <- sim_glick$history[focal_init$Player, ,]
  
  glick_scores <- as.numeric(focal_glick[, "Rating"])
  
  glick_tibble <- tibble(glicko = glick_scores, draw = i) |> 
    mutate(game = row_number())
  
  glicko_sims <- glicko_sims |> 
    bind_rows(glick_tibble)
}


## then overlay the truth
tru_glicko <- true_elo |> 
  rename(game = index, glicko = focal_elo)


glicko_sims |> 
  ggplot(aes(game, glicko)) +
  geom_line(alpha = 0.01, mapping = aes(group = draw)) +
  # geom_smooth() +
  geom_line(data = tru_glicko,
            col = "red") +
  NULL


## this is ppc with the model fit to the same data, rather than extended out
## into the future. still useful but just not as good as the one we really want


## looking at that here below

## this takes the true elo diff, which explains the good performance


draws <- fit_17_19_first$draws() %>% as_draws_df() %>%
  select(starts_with(c("beta[", "gamma"))) %>%
  mutate(draw = row_number()) %>% 
  pivot_longer(cols = "beta[1,1]":"beta[2,10]") %>% 
  mutate(param = stringr::str_extract(name, pattern = "\\d"),
         id = stringr::str_extract(name, pattern = "\\d+]"),
         id = stringr::str_replace(id, "\\]", ""),
         player_id = paste0("beta[", id, "]")) %>% 
  select(-name) %>% 
  pivot_wider(names_from = param, values_from = value) %>% 
  rename(beta1 = `1`, beta2 = `2`)



future_games <- tibble(focal = as.character(init_data$focal_id),
                       color = init_data$focal_white,
                       elo_diff = init_data$elo_diff,
                       hist = init_data$ave_prop)


num_draws <- 500

mix <- left_join(draws |>  filter(draw <= num_draws),
                 future_games, by = join_by("id" == "focal"),
                 relationship = "many-to-many") %>% 
  rowwise() %>% 
  mutate(inv_logit = beta1 + beta2 * hist + gamma1 * color + gamma2 * elo_diff,
         prob = exp(inv_logit)/(1 + exp(inv_logit)))


small_sim <- mix %>% 
  filter(id == 1) |> 
  mutate(sim_result = sample(c(1, 0), size = 1, prob = c(prob, 1 - prob))) %>% 
  select(id, draw, sim_result) %>% 
  group_by(id, draw) |> 
  mutate(game = row_number()) |> 
  # filter(id == 1) |> 
  rename(y = sim_result)




## then use this to compute the glicko scores again

glicko_sims_forward <- tibble()

for(i in 1:500){
  curr_result <- small_sim |> 
    filter(draw == i) |> 
    pull(y)
  
  ## then compute the glicko scores here 
  games <- data.frame(Week = glick_part1$index,
                      Player1 = focal_init$Player,
                      Player2 = glick_part1$opp,
                      Score = curr_result)
  
  sim_glick <- glicko2(games, status = initstate, history = TRUE, 
                       gamma = glick_part1$focal_white)
  focal_glick <- sim_glick$history[focal_init$Player, ,]
  
  glick_scores <- as.numeric(focal_glick[, "Rating"])
  
  glick_tibble <- tibble(glicko = glick_scores, draw = i) |> 
    mutate(game = row_number())
  
  glicko_sims_forward <- glicko_sims_forward |> 
    bind_rows(glick_tibble)
}



## then plot this version also

glicko_sims_forward |> 
  ggplot(aes(game, glicko)) +
  geom_line(alpha = 0.01, mapping = aes(group = draw)) +
  # geom_smooth() + 
  geom_line(data = tru_glicko,
            col = "red") +
  NULL



## save the glicko outputs (so could recreate the plots, modify as needed)
## set it up so you can specify the users you want to plot 
## and only run it for them, can do that on the cluster

## add in all the history games in advance of this also, to see the comparison 
## more broadly


rest_games <- small_data |> 
  filter(Username == focal_init$Player) |> 
  arrange(UTCDate, UTCTime) |> 
  slice(1:(n() - 1000)) |> 
  mutate(game = row_number())


num_prev <- nrow(rest_games)


sim_plot_data <- glicko_sims_forward |> 
  mutate(game = game + num_prev)

plot_true <- tru_glicko |> 
  mutate(game = game + num_prev)


rest_games |> 
  mutate(glicko = ifelse(Username == White, WhiteElo, BlackElo),
         game = row_number()) |>
  select(game, glicko) |> 
  ggplot(aes(game, glicko, UTCDate, UTCTime)) +
  geom_line(linewidth = 0.5, col = "darkred") + 
  geom_line(data = sim_plot_data, mapping = aes(group = draw), alpha = 0.005) +
  geom_line(data = plot_true,
            col = "red", linewidth = 0.5) +
  xlim(c(28000, num_prev + 1000)) +
  # xlim(c(85000, num_prev + 1000)) +
  # xlim(c(125000, num_prev + 1000)) +
  labs(x = "Games Played", y = "Glicko Score")







# Modify this to include the simulated Glicko scores ----------------------

## need to do this one game at a time, simulating the results as we go





focal_games <- future_games |> 
  filter(focal == 1)


full_game_info <- init_data |> 
  filter(focal_id == 1)


num_sims <- 100

num_games <- nrow(full_game_info)

glicko_sim <- data.frame(NA, nrow = num_games, ncol = num_sims)

game_sim <- data.frame(NA, nrow = num_games, ncol = num_sims)


## store the true overall average
## focal_win_prop = focal_result when just using previous game
true_avg <- mean(full_game_info$focal_result)

for(i in 1:num_games){
  
  ## get the game information for the ith game
  game_info <- focal_games[i, ]
  ## need to get the opponent elo from original data
  ## and the hist needs to be correct each time
  gamma <- ifelse(game_info$color == 1, 1, -1)
  opp_elo <- full_game_info[i,] |> 
    mutate(opp = ifelse(focal_white == 1, BlackElo, WhiteElo)) |> 
    pull(opp)
  opp <- full_game_info[i,] |> 
    mutate(opp_name = ifelse(focal_white == 1, Black, White)) |> 
    pull(opp_name)
    
  for(j in 1:num_sims){
    
    ## get the jth sim params for that 
    ## simulate the ith game and the corresponding glicko score
    ## store this in glicko_sim[i, j]
    ## need the result of the previous sim game also,
    ## to get the hist term
    ## can assume their average is the same here for this
    curr_draw <- draws |> 
      filter(draw == j & id == 1)
    
    ## if i = 1 use the initial glicko, otherwise use the previous
    if(i == 1){
      curr_glick <- focal_init$Rating
      curr_prop <- 0
    }else{
      curr_glick <- glicko_sim[i-1, j]
      curr_prop <- game_sim[i-1, j] - true_avg ## take lagged win rate
    }
    ## then simulate the outcome and update the glicko
    inv_logit <- curr_draw$beta1 + curr_draw$beta2 * curr_prop +
      curr_draw$gamma1 * game_info$color + 
      curr_draw$gamma2 * (curr_glick - opp_elo)
    prob <- exp(inv_logit)/(1 + exp(inv_logit))
    ## get sim result
    sim_result <- sample(c(1, 0), size = 1, prob = c(prob, 1 - prob))
    game_sim[i, j] <- sim_result
    games <- data.frame(Week = i,
                        Player1 = focal_init$Player,
                        Player2 = opp,
                        Score = sim_result)
    initstate <- tibble(Player = c(focal_init$Player, opp),
                        Rating = c(curr_glick, opp_elo),
                        Deviation = 50,
                        Volatility = 0.05)
    
    sim_glick <- glicko2(games, status = initstate, history = TRUE, 
                         gamma = gamma)
    ## extract the rating of focal player and store it
    focal_glick <- sim_glick$history[focal_init$Player, ,]
    
    glicko_sim[i, j] <- as.numeric(focal_glick)[1]
    
  }
  
}


## then check it out

quick_sim <- glicko_sim[1:928, 1:100]  

colnames(quick_sim) <- paste0("Sim_", 1:100)

as_tibble(quick_sim) |> 
  mutate(game = row_number()) |> 
  pivot_longer(cols = Sim_1:Sim_100, names_to = "Sim",
               values_to = "glicko") |> 
  ggplot(aes(game, glicko)) +
  geom_line(alpha = 0.05, aes(group = Sim)) +
  geom_line(data = tru_glicko,
            col = "red")


