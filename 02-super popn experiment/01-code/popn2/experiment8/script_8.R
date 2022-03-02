## script for stan files
## AR prior models 

library(rstan)
options(cores=parallel::detectCores())

## generating data
source("~/MRP project/02-super popn/01 code/experiment8/LOO_gen_data.R") # generate a list of things
sim_data = sim1$samp_data # extracting sample data from list

## data
samp_dat <- list(
  N = nrow(sim_data), # number in samples
  N_groups_X_cat = max(sim_data$X1), # number of levels in X1, X2, X3 (categorical)
  N_groups_X4 =  max(sim_data$X4), # number of levels in X4 (depends on choice of discretisation)
  X1 = sim_data$X1,
  X2 = sim_data$X2,
  X3 = sim_data$X3,
  X4 = sim_data$X4, 
  y = sim_data$bin_value # binary outcome
)

model15.stan = stan('script_re_prior.stan', data=samp_dat)
