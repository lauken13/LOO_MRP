## script for running stan files
## running for all six models
# setwd('/mnt/lustre/projects/Mona0085/skuh/02-super/exp_N02')

# # iteration number (ITE) from cluster
slurm_arrayid <- Sys.getenv('SLURM_ARRAY_TASK_ID')
iter = as.numeric(slurm_arrayid)

### EXTRACT CODE BELOW TO RUN IN A FOR LOOP LOCALLY ####
# data generation ---------------------------------------------------------
  # function to generate data
  source("func_gen_dat.R") 

  # generating data using gen_dat()
  set.seed(65438)
  sim1 = gen_dat(N = 10000, samp_size = 500, ITE=iter) # generate a list of things
  
  samp_dat = sim1$samp_data
  popn_ps = sim1$popn_ps
  popn_data = sim1$popn_data
  
  # using cmdstanr ----------------------------------------------------------
  library(cmdstanr)
  options(cores=parallel::detectCores())
  
  # list of data for stan
  samp_dat_mrp <- list(
    n = nrow(samp_dat), # number in samples
    n_groups_X_cat = sim1$J, # number of levels in X1, X2, X3 (categorical)
    X1 = samp_dat$X1,
    X2 = samp_dat$X2,
    X3 = samp_dat$X3,
    X4 = samp_dat$X4, 
    y = as.numeric(samp_dat$y_obs), # binary outcome
    X1_pop = popn_ps$X1,
    X2_pop = popn_ps$X2,
    X3_pop = popn_ps$X3,
    X4_pop = popn_ps$X4, 
    J = nrow(popn_ps)
  )
  
  ## compile stan model
  model01_rePrior = cmdstan_model(file.path('stancode/model01.stan'))
  model02_rePrior = cmdstan_model(file.path('stancode/model02.stan'))
  model03_rePrior = cmdstan_model(file.path('stancode/model03.stan'))
  model04_rePrior = cmdstan_model(file.path('stancode/model04.stan'))
  model05_rePrior = cmdstan_model(file.path('stancode/model05.stan'))
  model06_rePrior = cmdstan_model(file.path('stancode/model06.stan'))
  model07_rePrior = cmdstan_model(file.path('stancode/model07.stan'))
  model08_rePrior = cmdstan_model(file.path('stancode/model08.stan'))
  model09_rePrior = cmdstan_model(file.path('stancode/model09.stan'))
  model10_rePrior = cmdstan_model(file.path('stancode/model10.stan'))
  model11_rePrior = cmdstan_model(file.path('stancode/model11.stan'))
  model12_rePrior = cmdstan_model(file.path('stancode/model12.stan'))
  model13_rePrior = cmdstan_model(file.path('stancode/model13.stan'))
  model14_rePrior = cmdstan_model(file.path('stancode/model14.stan'))
  model15_rePrior = cmdstan_model(file.path('stancode/model15.stan'))
  
  
  ## fitting stan model 
  model01_fit_rePrior <- model01_rePrior$sample(data = samp_dat_mrp, seed = 56789)
  model02_fit_rePrior <- model02_rePrior$sample(data = samp_dat_mrp, seed = 56789)
  model03_fit_rePrior <- model03_rePrior$sample(data = samp_dat_mrp, seed = 56789)
  model04_fit_rePrior <- model04_rePrior$sample(data = samp_dat_mrp, seed = 56789)
  model05_fit_rePrior <- model05_rePrior$sample(data = samp_dat_mrp, seed = 56789)
  model06_fit_rePrior <- model06_rePrior$sample(data = samp_dat_mrp, seed = 56789)
  model07_fit_rePrior <- model07_rePrior$sample(data = samp_dat_mrp, seed = 56789)
  model08_fit_rePrior <- model08_rePrior$sample(data = samp_dat_mrp, seed = 56789)
  model09_fit_rePrior <- model09_rePrior$sample(data = samp_dat_mrp, seed = 56789)
  model10_fit_rePrior <- model10_rePrior$sample(data = samp_dat_mrp, seed = 56789)
  model11_fit_rePrior <- model11_rePrior$sample(data = samp_dat_mrp, seed = 56789)
  model12_fit_rePrior <- model12_rePrior$sample(data = samp_dat_mrp, seed = 56789)
  model13_fit_rePrior <- model13_rePrior$sample(data = samp_dat_mrp, seed = 56789)
  model14_fit_rePrior <- model14_rePrior$sample(data = samp_dat_mrp, seed = 56789)
  model15_fit_rePrior <- model15_rePrior$sample(data = samp_dat_mrp, seed = 56789) 
  
  
  # saving quantities
  loo_01 = model01_fit_rePrior$loo()
  popnest_01 = model01_fit_rePrior$draws(variables = "theta_pop") %>% as_draws_matrix()
  sampest_01 = model01_fit_rePrior$draws(variables = "theta_samp") %>% as_draws_matrix()
  
  loo_02 = model02_fit_rePrior$loo()
  popnest_02 = model02_fit_rePrior$draws(variables = "theta_pop") %>% as_draws_matrix()
  sampest_02 = model02_fit_rePrior$draws(variables = "theta_samp") %>% as_draws_matrix()
  
  loo_03 = model03_fit_rePrior$loo()
  popnest_03 = model03_fit_rePrior$draws(variables = "theta_pop") %>% as_draws_matrix()
  sampest_03 = model03_fit_rePrior$draws(variables = "theta_samp") %>% as_draws_matrix()
  
  loo_04 = model04_fit_rePrior$loo()
  popnest_04 = model04_fit_rePrior$draws(variables = "theta_pop") %>% as_draws_matrix()
  sampest_04 = model04_fit_rePrior$draws(variables = "theta_samp") %>% as_draws_matrix()
  
  loo_05 = model05_fit_rePrior$loo()
  popnest_05 = model05_fit_rePrior$draws(variables = "theta_pop") %>% as_draws_matrix()
  sampest_05 = model05_fit_rePrior$draws(variables = "theta_samp") %>% as_draws_matrix()
  
  loo_06 = model06_fit_rePrior$loo()
  popnest_06 = model06_fit_rePrior$draws(variables = "theta_pop") %>% as_draws_matrix()
  sampest_06 = model06_fit_rePrior$draws(variables = "theta_samp") %>% as_draws_matrix()
  
  loo_07 = model07_fit_rePrior$loo()
  popnest_07 = model07_fit_rePrior$draws(variables = "theta_pop") %>% as_draws_matrix()
  sampest_07 = model07_fit_rePrior$draws(variables = "theta_samp") %>% as_draws_matrix()
  
  loo_08 = model08_fit_rePrior$loo()
  popnest_08 = model08_fit_rePrior$draws(variables = "theta_pop") %>% as_draws_matrix()
  sampest_08 = model08_fit_rePrior$draws(variables = "theta_samp") %>% as_draws_matrix()
  
  loo_09 = model09_fit_rePrior$loo()
  popnest_09 = model09_fit_rePrior$draws(variables = "theta_pop") %>% as_draws_matrix()
  sampest_09 = model09_fit_rePrior$draws(variables = "theta_samp") %>% as_draws_matrix()
  
  loo_10 = model10_fit_rePrior$loo()
  popnest_10 = model10_fit_rePrior$draws(variables = "theta_pop") %>% as_draws_matrix()
  sampest_10 = model10_fit_rePrior$draws(variables = "theta_samp") %>% as_draws_matrix()
  
  loo_11 = model11_fit_rePrior$loo() 
  popnest_11 = model11_fit_rePrior$draws(variables = "theta_pop") %>% as_draws_matrix()
  sampest_11 = model11_fit_rePrior$draws(variables = "theta_samp") %>% as_draws_matrix()
  
  loo_12 = model12_fit_rePrior$loo()
  popnest_12 = model12_fit_rePrior$draws(variables = "theta_pop")%>% as_draws_matrix()
  sampest_12 = model12_fit_rePrior$draws(variables = "theta_samp") %>%as_draws_matrix()
  
  loo_13 = model13_fit_rePrior$loo() 
  popnest_13 = model13_fit_rePrior$draws(variables = "theta_pop") %>% as_draws_matrix()
  sampest_13 = model13_fit_rePrior$draws(variables = "theta_samp") %>% as_draws_matrix()
  
  loo_14 = model14_fit_rePrior$loo()
  popnest_14 = model14_fit_rePrior$draws(variables = "theta_pop") %>%  as_draws_matrix()
  sampest_14 = model14_fit_rePrior$draws(variables = "theta_samp") %>% as_draws_matrix()
  
  loo_15 = model15_fit_rePrior$loo()
  popnest_15 = model15_fit_rePrior$draws(variables = "theta_pop") %>% as_draws_matrix()
  sampest_15 = model15_fit_rePrior$draws(variables = "theta_samp") %>% as_draws_matrix()
  
  
  save(loo_01, loo_02, loo_03, loo_04, loo_05,
       loo_06, loo_07, loo_08, loo_09, loo_10,
       loo_11, loo_12, loo_13, loo_14, loo_15,
       popnest_01, popnest_02, popnest_03, popnest_04, popnest_05, 
       popnest_06, popnest_07, popnest_08, popnest_09, popnest_10, 
       popnest_11, popnest_12, popnest_13, popnest_14, popnest_15, 
       sampest_01, sampest_02, sampest_03, sampest_04, sampest_05,
       sampest_06, sampest_07, sampest_08, sampest_09, sampest_10,
       sampest_11, sampest_12, sampest_13, sampest_14, sampest_15,
       file = paste0('LOO_arPrior_', iter, '.RData'), compress=T)
  
## END OF LOOP ####
