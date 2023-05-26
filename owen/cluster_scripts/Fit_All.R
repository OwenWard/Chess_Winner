### May 23rd, 2023,
## to fit simple w/l effect model for large chess data on cluster
## using cmdstanr and see what the output looks like


library(cmdstanr)

options(mc.cores = parallel::detectCores())

library(tidyverse)
library(here)
library(loo)
library(ggplot2)


stan_data_all <- readRDS(here("owen", "cluster_scripts", "stan_data_ppt_n10.RDS"))


stan_file <- here("owen","Comp2.stan")

mod2 <- cmdstan_model(stan_file)

fit2 <- mod2$sample(data = stan_data_all,
                    seed = 123,
                    chains = 4,
                    parallel_chains = 4,
                    refresh = 100,
		                iter_warmup = 2000, 
		                iter_sampling = 2000)


fit2$summary()

fit2$save_object(file = here("owen", "cluster_scripts", "Cluster_stan_ppt_n10.RDS"))


## Do some model checking here using the draws to get 
## posterior predictive distributions

fit_samples <- as_draws_df(fit2$draws())


fit_samp <- fit_samples %>% 
  select(!starts_with(c("log_lik", "yrep")))

y_rep <- fit_samples %>% select(starts_with("y_rep"))



### then want to join this with the correct input, i.e player, etc


y_rep_mod <- y_rep %>% 
  rownames_to_column() %>%  
  pivot_longer(-rowname) %>% 
  pivot_wider(names_from=rowname, values_from=value) %>% 
  ## remove the draws and chains here
  mutate(focal_id = stan_data_all$focal_id)


## what is the check here? number of games won by a player for each of
## the posterior samples, compared to the number of games the actually won


games_won <- y_rep_mod %>% 
  pivot_longer(cols = `1`:`8000`, names_to = "draw", values_to = "y") %>% 
  group_by(focal_id, draw) %>% 
  summarise(games_won = sum(y)) 


orig_data <- tibble(outcome = stan_data_all$y,
                    focal_id = stan_data_all$focal_id)

orig_games_won <- orig_data %>% 
  group_by(focal_id) %>% 
  summarise(games_won = sum(outcome))


p1 <- games_won %>% 
  ggplot(aes(x = games_won)) +
  geom_histogram() +
  facet_wrap(~focal_id, scales = "free") +
  geom_vline(data = orig_games_won, 
             mapping = aes(xintercept = games_won), col = "red") +
  labs(title = "PPT Model, No Covariates")

ggsave(filename = here("owen", "cluster_scripts", "ppd_model.png"), plot = p1)

## then load in the competing model and compare them using loo

fit4 <- readRDS(here("owen", "model_fits", "model4.RDS"))

comp <- loo_compare(fit2, fit4)

print(comp)
