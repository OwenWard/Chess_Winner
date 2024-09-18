#### Sept 18th 2024
#### Construc a PPC of the evolution of the Lichess rating over time
#### Using complete simulated trajectories of their final 1000 games,
#### using parameter estimats from model fit to first 1000 games

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


files <- list.files(data_path)

lichess_data <- files |> 
  map_dfr(~read_player(data_path, .x))


small_data <- lichess_data |>
  mutate(Event = tolower(Event)) |> 
  filter(TimeControl == "60+0") |>
  filter(Variant == "Standard") |>
  filter(grepl("rated bullet game", Event))

rm(lichess_data)

select_users <- small_data |> 
  group_by(Username) |> 
  tally() |> 
  arrange(-n) |> 
  slice_max(order = n, n = 10) |> 
  pull(Username)


users <- select_users


## will use last 1000 games each user plays

last_games <- small_data |> 
  filter(Username %in% users) |> 
  group_by(Username) |> 
  arrange(UTCDate, UTCTime, .by_group = TRUE) |> 
  slice_tail(n = 1000) |> 
  ungroup() |> 
  group_by(Username) |> 
  mutate(Result = sample(Result)) |> 
  ungroup()


tidy_games <- map_dfr(users, get_hist, last_games, prev_n = 1) |> 
  as_tibble()


init_data <- tidy_games |> 
  mutate(WhiteElo = as.numeric(WhiteElo), 
         BlackElo = as.numeric(BlackElo)) |> 
  mutate(focal_user = ifelse(focal_white == 1, White, Black)) |> 
  mutate(elo_diff = ifelse(focal_white == 1, 
                           WhiteElo - BlackElo, BlackElo - WhiteElo)) |> 
  mutate(focal_id = match(focal_user, users)) |> 
  group_by(focal_id)  |> 
  mutate(ave_prop = lag(focal_win_prop, default = 0) - mean(focal_win_prop)) |>
  filter(focal_result != 0.5)




## get their starting scores for each player



## then just pick the player from this data
## need to choose the focal id and format for the glicko2 function

focal_player_used <- 1

focal_init <- init_data |> group_by(focal_user) |> 
  slice_head(n = 1) |> 
  arrange(focal_id) |> 
  filter(focal_id == focal_player_used) |> 
  mutate(focal_elo = ifelse(focal_user == White, WhiteElo, BlackElo)) |> 
  select(focal_user, focal_elo) |> 
  rename(Player = focal_user, Rating = focal_elo)



glick_data <- init_data |>
  filter(focal_user == focal_init$Player)


# First compare computed Glicko with Lichess ------------------------------


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

## need to select the Deviation (to start with) and Volatility 

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

## plot against lichess estimates

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



# Setup to Simulate Complete Trajectory of Games and Scores ---------------------------------------

data_path <- res_data_path[path_id]


## get the draws from the first
fit_17_19_first <- readRDS(file =  here(data_path, "time",
                                       "select_users_first_bullet.RDS"))

fit_17_19_last <- readRDS(file =  here(data_path, "time",
                                       "select_users_last_bullet.RDS"))



## extract draws from model fit to first

draws <- fit_17_19_first$draws() |> as_draws_df() |>
  select(starts_with(c("beta[", "gamma"))) |>
  mutate(draw = row_number()) |> 
  pivot_longer(cols = "beta[1,1]":"beta[2,10]") |> 
  mutate(param = stringr::str_extract(name, pattern = "\\d"),
         id = stringr::str_extract(name, pattern = "\\d+]"),
         id = stringr::str_replace(id, "\\]", ""),
         player_id = paste0("beta[", id, "]")) |> 
  select(-name) |> 
  pivot_wider(names_from = param, values_from = value) |> 
  rename(beta1 = `1`, beta2 = `2`)


## the covariates for the future games

future_games <- tibble(focal = as.character(init_data$focal_id),
                       color = init_data$focal_white,
                       elo_diff = init_data$elo_diff,
                       hist = init_data$ave_prop)


## process for below

focal_games <- future_games |> 
  filter(focal == focal_player_used)


full_game_info <- init_data |> 
  filter(focal_id == focal_player_used)

# slow for even 100 sims at the moment, due to the for loop
num_sims <- 100

num_games <- nrow(full_game_info)

glicko_sim <- data.frame(NA, nrow = num_games, ncol = num_sims)

game_sim <- data.frame(NA, nrow = num_games, ncol = num_sims)


## store the true overall average
## focal_win_prop = focal_result when just using previous game
true_avg <- mean(full_game_info$focal_result)

focal_draws <- draws |> 
  filter(id == focal_player_used)

for(i in 1:num_games){
  
  ## get the game information for the ith game
  game_info <- focal_games[i, ]
  # gamma the colour effect for glicko calc
  gamma <- ifelse(game_info$color == 1, 1, -1)
  
  # get opponent name and ability
  opp_elo <- full_game_info[i,] |> 
    mutate(opp = ifelse(focal_white == 1, BlackElo, WhiteElo)) |> 
    pull(opp)
  opp <- full_game_info[i,] |> 
    mutate(opp_name = ifelse(focal_white == 1, Black, White)) |> 
    pull(opp_name)
  
  curr_game <- draws[]
    
  for(j in 1:num_sims){
    
    curr_draw <- focal_draws[focal_draws$draw == j, ]
    
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


## quick plot to confirm

quick_sim <- glicko_sim[, 1:100]  

colnames(quick_sim) <- paste0("Sim_", 1:100)


quick_sim <- as_tibble(quick_sim) |> 
  mutate(game = row_number()) |> 
  pivot_longer(cols = Sim_1:Sim_100, names_to = "Sim",
               values_to = "glicko") 

quick_sim |> 
  ggplot(aes(game, glicko)) +
  geom_line(alpha = 0.05, aes(group = Sim)) +
  geom_line(data = tru_glicko,
            col = "red")



# More complete plot for poster -------------------------------------------


rest_games <- small_data |> 
  filter(Username == focal_init$Player) |> 
  arrange(UTCDate, UTCTime) |> 
  slice(1:(n() - 1000)) |> 
  mutate(game = row_number())


num_prev <- nrow(rest_games)


sim_plot_data <- quick_sim |> 
  mutate(game = game + num_prev)

plot_true <- tru_glicko |> 
  mutate(game = game + num_prev)


rest_games |> 
  mutate(glicko = ifelse(Username == White, WhiteElo, BlackElo),
         game = row_number()) |>
  select(game, glicko) |> 
  ggplot(aes(game, glicko, UTCDate, UTCTime)) +
  geom_line(linewidth = 0.5, col = "darkred") + 
  geom_line(data = sim_plot_data, mapping = aes(group = Sim), alpha = 0.05) +
  geom_line(data = plot_true,
            col = "red", linewidth = 0.5) +
  # xlim(c(28000, num_prev + 1000)) +
  # xlim(c(85000, num_prev + 1000)) +
  xlim(c(125000, num_prev + 1000)) +
  labs(x = "Games Played", y = "Glicko Score")




# Can also plot the number of Games Won -----------------------------------

## compare this to the truth for the single player


quick_sim_games <- game_sim[, 1:100]  

colnames(quick_sim_games) <- paste0("Sim_", 1:100)


quick_sim_games <- as_tibble(quick_sim_games) |> 
  mutate(game = row_number()) |> 
  pivot_longer(cols = Sim_1:Sim_100, names_to = "Sim",
               values_to = "Result") 

true_games_won <- sum(full_game_info$focal_result)

quick_sim_games |> 
  group_by(Sim) |> 
  summarise(games_won = sum(Result)) |> 
  ggplot(aes(games_won)) +
  geom_histogram() +
  geom_vline(aes(xintercept = true_games_won), col = "red")
