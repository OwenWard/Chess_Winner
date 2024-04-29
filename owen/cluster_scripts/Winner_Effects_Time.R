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

## source helper functions for reading and transforming data
## along with defaults for plots, etc
source(here("analysis/helper.R"))


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
save_path <- paste0(all_save_path[path_id], "time/")

dir.create(save_path, showWarnings = FALSE)

files <- list.files(data_path)


lichess_data <- files %>% 
  map_dfr(~read_player(data_path, .x))



## restrict to rated blitz and shorter here
## this also removes the NAs, which makes sense

small_data <- lichess_data %>%
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

## save the users chosen for this
saveRDS(users, file = paste0(save_path, "select_users_bullet.RDS"))

## fit to the first 1000 games
tidy_games <- map_dfr(users, get_hist, first_games, prev_n = 1) %>% 
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
fit3_first$save_object(file = here(save_path, "select_users_first_bullet.RDS"))


## then repeat for the last 1000 games for each
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



fit3_last <- mod$sample(data = stan_data_ave_last,
                         seed = 123,
                         chains = 4,
                         parallel_chains = 4,
                         refresh = 100)

fit3_last$save_object(file = here(save_path, "select_users_last_bullet.RDS"))


fit3_first$summary()
fit3_last$summary()


## plot these histograms together, from first and last side by side


global_winner <- as_draws_df(fit3_first$draws(variables = "mu_beta")) %>% 
  rename("Initial Games" = mu_beta) %>% 
  bind_cols(as_draws_df(fit3_last$draws(variables = "mu_beta")) %>% 
              rename("Later Games" = mu_beta)) %>% 
  select(`Initial Games`, `Later Games`) %>% 
  pivot_longer(cols = everything()) %>% 
  ggplot(aes(value, fill = name)) +
  geom_histogram(position = "identity") +
  facet_wrap(~name, ncol = 1) +
  labs(fill = element_blank(), y = element_blank(), x = element_blank()) +
  theme(legend.position = "none",
        axis.text = element_text(size = axis_text_size),
        axis.title = element_text(size = axis_title),
        legend.text = element_text(size = legend_text),
        strip.text = element_text(size = axis_title),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size = title_size)) +
  # labs(title = "Selected 1700-1900 Bullet Players") +
  NULL
  
global_winner


ggsave(filename = paste0(save_path, "global_winner_first_last_bullet.png"),
       global_winner,
       width = 7, height = 6, dpi = 600)

## basically no difference between them here, slightly more variability early on

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
  mutate(when = "Initial")

random_effect_last <- fit3_last$draws() %>% as_draws_df() %>%
  select(starts_with("beta[")) %>%
  pivot_longer(cols = everything()) %>%
  mutate(param = stringr::str_extract(name, pattern = "\\d"),
         id = stringr::str_extract(name, pattern = "\\d+]"),
         id = stringr::str_replace(id, "\\]", ""),
         player_id = paste0("beta[", id, "]")) %>% 
  mutate(when = "Later")



betas <- bind_rows(random_effect_first, random_effect_last) %>% 
  select(value, param, id, when)


## need to give these the correct names here
player_id <- tibble(id = as.character(1:10), player = users)

betas %>% left_join(player_id, by = "id") %>% 
  mutate(player = factor(player, levels = users))



winner_effect_plot <- betas %>% 
  left_join(player_id, by = "id") %>% 
  mutate(player = factor(player, levels = users)) %>% 
  filter(param == 2) %>% 
  mutate(id = factor(id, levels = c(1:10))) %>% 
  ggplot(aes(value, fill = when)) +
  geom_histogram() +
  geom_vline(xintercept = 0, alpha = 0.5) +
  scale_x_continuous(breaks = c(-0.25, 0.25)) +
  # xlim(c(-1.5, 1.5)) +
  facet_grid(rows = vars(when), cols = vars(player), scales = "free") +
  labs(fill = element_blank(), y = element_blank(), 
       # title = "Individual Winner Effects, Selected 1700-1900 Bullet Players",
       x = element_blank()) +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = axis_text_size - 4),
        axis.title = element_text(size = axis_title),
        legend.text = element_text(size = legend_text),
        strip.text = element_text(size = axis_title - 6),
        legend.position = "none",
        plot.title = element_text(size = title_size))


winner_effect_plot

ggsave(filename = paste0(save_path, "indiv_winner_first_last_bullet.png"),
       winner_effect_plot,
       width = 9, height = 6, dpi = 600)


## then posterior predictive checking for both before and after
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
  summarise(games_won = sum(outcome)) %>% 
  mutate(focal_id = as.character(focal_id)) %>% 
  left_join(player_id, by = c("focal_id" = "id"))


p1 <- games_won %>% 
  mutate(focal_id = as.character(focal_id)) %>% 
  left_join(player_id, by = c("focal_id" = "id")) %>% 
  ggplot(aes(x = games_won)) +
  geom_histogram() +
  facet_wrap(~player, scales = "free", ncol = 5) +
  geom_vline(data = orig_games_won, 
             mapping = aes(xintercept = games_won), col = "red") +
  scale_x_continuous(breaks = c(450, 500, 550)) +
  # labs(title = "Posterior Predictive Distribution, Initial Games",
  labs(y = element_blank(),
       x = "Number of Games Won") +
  theme(axis.text = element_text(size = axis_text_size - 2),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size = title_size),
        axis.title = element_text(size = axis_title),
        legend.text = element_text(size = legend_text),
        strip.text = element_text(size = axis_title - 6))

p1

ggsave(filename = paste0(save_path, "ppc_first_bullet.png"),
       p1,
       width = 7, height = 5, dpi = 600)



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
  summarise(games_won = sum(outcome)) %>% 
  mutate(focal_id = as.character(focal_id)) %>% 
  left_join(player_id, by = c("focal_id" = "id"))

p2 <- games_won %>% 
  mutate(focal_id = as.character(focal_id)) %>% 
  left_join(player_id, by = c("focal_id" = "id")) %>% 
  ggplot(aes(x = games_won)) +
  geom_histogram() +
  facet_wrap(~player, scales = "free", ncol = 5) +
  geom_vline(data = orig_games_won, 
             mapping = aes(xintercept = games_won), col = "red") +
  labs(x = "Number of Games Won", y = element_blank()) +
  scale_x_continuous(breaks = c(450, 500, 550)) +
  theme(axis.text = element_text(size = axis_text_size - 2),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size = title_size),
        axis.title = element_text(size = axis_title),
        legend.text = element_text(size = legend_text),
        strip.text = element_text(size = axis_title - 6))

p2


ggsave(filename = paste0(save_path, "ppc_last_bullet.png"),
       p2,
       width = 7, height = 5, dpi = 600)





##########################################
## try to sample for the last data, using the model fitted for the
## first instead

draws <- fit3_first$draws() %>% as_draws_df() %>%
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


future_games <- tibble(focal = as.character(stan_data_ave_last$id), 
       color = stan_data_ave_last$colour, 
       elo_diff = stan_data_ave_last$elo,
       hist = stan_data_ave_last$win_prop)


## need to figure out how to combine these together then...
## maybe it makes sense to get the gamma out separately,
## because then can just bind them together relatively easily
## then some sort of join with the betas based on the ids matching

## left_join(draws, future_games) based on the id

mix <- left_join(draws, #%>% filter(draw <= 1500),
                 future_games, by = join_by("id" == "focal"),
                 relationship = "many-to-many") %>% 
  rowwise() %>% 
  mutate(inv_logit = beta1 + beta2 * hist + gamma1 * color + gamma2 * elo_diff,
         prob = exp(inv_logit)/(1 + exp(inv_logit)))


small_sim <- mix %>% 
  mutate(sim_result = sample(c(1, 0), size = 1, prob = c(prob, 1 - prob))) %>% 
  select(id, draw, sim_result) %>% 
  group_by(id, draw) %>% 
  summarise(games_won = sum(sim_result))

## save this object in case need to improve the plots
saveRDS(small_sim, file = paste0(save_path, "ppc_fit_first_last.RDS"))

## add in true games 
future_results <- tibble(focal = as.character(stan_data_ave_last$id),
                         result = stan_data_ave_last$y) %>% 
  group_by(focal) %>%
  summarise(total = sum(result)) %>% 
  rename(id = focal) %>% 
  left_join(player_id, by = "id")


future_results 
## then compare these to the true number of games won
last_ppc <- small_sim %>%  
  left_join(player_id, by = "id") %>% 
  ggplot(aes(x = games_won)) + 
  geom_histogram() + 
  geom_vline(data = future_results, 
             mapping = aes(xintercept = total), col = "red") +
  facet_wrap(~player, scales = "free_x", ncol = 5) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  labs(y = element_blank(), x = "Number of Games Won") +
  scale_x_continuous(breaks = c(450, 500, 550)) +
  theme(axis.text = element_text(size = axis_text_size - 2),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.spacing.x = unit(1, "lines"),
        plot.title = element_text(size = title_size),
        axis.title = element_text(size = axis_title),
        legend.text = element_text(size = legend_text),
        strip.text = element_text(size = axis_title - 6))


last_ppc

ggsave(filename = paste0(save_path, "ppc_fit_first_last_bullet.png"),
       last_ppc, dpi = 600, height = 5, width = 7)


