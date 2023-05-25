### May 23rd, 2023,
## to fit simple w/l effect model for large chess data on cluster
## using cmdstanr and see what the output looks like


library(cmdstanr)

options(mc.cores = parallel::detectCores())

library(tidyverse)
library(here)


stan_data_all <- readRDS(here("owen", "cluster_scripts", "stan_data.RDS"))


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

fit2$save_object(file = here("owen", "cluster_scripts", "Cluster_stan.RDS"))


