## script for stan files
## running for all six models

# data generation ---------------------------------------------------------
# function to generate data
source("00-gen_dat_func.R") 

# iteration number (ITE) from cluster
slurm_arrayid <- Sys.getenv('SLURM_ARRAY_TASK_ID')
iter = as.numeric(slurm_arrayid)

# generating data using gen_dat()
set.seed(34567)
sim1 = gen_dat(N = 10000, fx = fx3, samp_size = 500, ITE=iter) # generate a list of things

samp_dat = sim1$samp_data
popn_ps = sim1$popn_ps
popn_data = sim1$popn_data

# using cmdstanr ----------------------------------------------------------
library(cmdstanr)
options(cores=parallel::detectCores())

# list of data for stan
samp_dat_mrp <- list(
  n = nrow(samp_dat), # number in samples
  n_groups_X_cat = sim1$Q, # number of levels in X1, X2, X3 (categorical)
  n_groups_X4 =  sim1$K, # number of levels in X4 (depends on choice of discretisation)
  X1 = samp_dat$X1,
  X2 = samp_dat$X2,
  X3 = samp_dat$X3,
  X4 = samp_dat$X4, 
  y = samp_dat$y_obs, # binary outcome
  X1_pop = popn_ps$X1,
  X2_pop = popn_ps$X2,
  X3_pop = popn_ps$X3,
  X4_pop = popn_ps$X4, 
  J = nrow(popn_ps)
)

## compile stan model
model15a_arPrior = cmdstan_model(file.path('model15a.stan'))
model15_rePrior = cmdstan_model(file.path('model15.stan'))

# without X2
model13a_arPrior = cmdstan_model(file.path('model13a.stan'))
model13_rePrior = cmdstan_model(file.path('model13.stan'))

# without X4
model11_rePrior = cmdstan_model(file.path('model11.stan'))

# without X2 and X4
model06_rePrior = cmdstan_model(file.path('model06.stan'))

## fitting stan model 
model15a_fit_arPrior <- model15a_arPrior$sample(data = samp_dat_mrp, 
                                              seed = 5678) # setting seed within sampling
model15_fit_rePrior <- model15_rePrior$sample(data = samp_dat_mrp, 
                                              seed = 5678) 

model13a_fit_arPrior <- model13a_arPrior$sample(data = samp_dat_mrp, 
                                                seed = 5678)
model13_fit_rePrior <- model13_rePrior$sample(data = samp_dat_mrp, 
                                              seed = 5678)

model11_fit_rePrior <- model11_rePrior$sample(data = samp_dat_mrp, 
                                                seed = 5678)
model06_fit_rePrior <- model06_rePrior$sample(data = samp_dat_mrp, 
                                              seed = 5678)


# saving quantities
loo_15a = model15a_fit_arPrior$loo() # loo is now built-in in cmdstanr
popnest_15a = model15a_fit_arPrior$draws(variables = "theta_pop") %>% 
  as_draws_matrix()
sampest_15a = model15a_fit_arPrior$draws(variables = "theta_samp") %>% 
  as_draws_matrix()

loo_15 = model15_fit_rePrior$loo()
popnest_15 = model15_fit_rePrior$draws(variables = "theta_pop") %>% 
  as_draws_matrix()
sampest_15 = model15_fit_rePrior$draws(variables = "theta_samp") %>% 
  as_draws_matrix()

loo_13a = model13a_fit_arPrior$loo() 
popnest_13a = model13a_fit_arPrior$draws(variables = "theta_pop") %>% 
  as_draws_matrix()
sampest_13a = model13a_fit_arPrior$draws(variables = "theta_samp") %>% 
  as_draws_matrix()

loo_13 = model13_fit_rePrior$loo() 
popnest_13 = model13_fit_rePrior$draws(variables = "theta_pop") %>% 
  as_draws_matrix()
sampest_13 = model13_fit_rePrior$draws(variables = "theta_samp") %>% 
  as_draws_matrix()

loo_11 = model11_fit_rePrior$loo() 
popnest_11 = model11_fit_rePrior$draws(variables = "theta_pop") %>% 
  as_draws_matrix()
sampest_11 = model11_fit_rePrior$draws(variables = "theta_samp") %>% 
  as_draws_matrix()

loo_06 = model06_fit_rePrior$loo()
popnest_06 = model06_fit_rePrior$draws(variables = "theta_pop")%>% 
  as_draws_matrix()
sampest_06 = model06_fit_rePrior$draws(variables = "theta_samp") %>% 
  as_draws_matrix()

save(loo_15a, loo_15, loo_13a,
     loo_13, loo_11, loo_06,
     popnest_15a, popnest_15, popnest_13a,
     popnest_13, popnest_11, popnest_06, 
     sampest_15a, sampest_15, sampest_13a,
     sampest_13, sampest_11, sampest_06, 
     file = paste0('LOO_arPrior_', iter, '.RData'))
