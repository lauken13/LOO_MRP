## data generation to calculate weights ####
# function to generate data
source(here::here("02-super popn approach/experiment3/01-code/stan code/00-LOO_gen_data.R")) # generate a list of things

## elpd values 
lpd_06_list = 
lpd_11_list = 
lpd_13_list = 
lpd_13a_list = 
lpd_15_list = 
lpd_15a_list = list()

loo_wtd_06_list =
  loo_wtd_11_list = 
  loo_wtd_13_list = 
  loo_wtd_13a_list = 
  loo_wtd_15_list =
  loo_wtd_15a_list = list()

for (ite in 1:100){
  load(paste0('02-super popn approach/experiment3/03-data/LOO_arPrior_',ite,".RData")) 
  
  # generating data using gen_dat()
  sim1 = gen_dat(N = 10000, fx = fx1, samp_size = 1000, ITE=ite)
  
  samp_data = sim1$samp_data
  popn_data = sim1$popn_data
  
  # calculating population totals for each level
  X1_margin = xtabs(~X1, data=popn_data)
  X2_margin = xtabs(~X2, data=popn_data)
  X3_margin = xtabs(~X3, data=popn_data)
  X4_margin = xtabs(~X4, data=popn_data)
  
  ## creating survey design
  svy1 = svydesign(ids=~1, # cluster id, ~1 for no clusters
                   weights=~rep(1,nrow(samp_data)), # equal weights for each unit
                   data=samp_data)
  
  # raked to the population
  rake1 = rake(design = svy1, sample.margins = list(~X1,~X2,~X3,~X4), 
               population.margins = list(X1_margin, X2_margin, X3_margin, X4_margin))
  
  # raked weights 
  samp_data$wts = weights(rake1)
  
  # creating survey raked weights
  svy_rake = svydesign(ids=~1, # cluster id, ~1 for no clusters
                       weights=~wts, # including raked weights in the survey design
                       data=samp_data) 
  
  ## saving loo object
  lpd_06_list[[ite]] = loo(loglik_06)
  lpd_11_list[[ite]] = loo(loglik_11)
  lpd_13_list[[ite]] = loo(loglik_13)
  lpd_13a_list[[ite]] = loo(loglik_13a)
  lpd_15_list[[ite]] = loo(loglik_15)
  lpd_15a_list[[ite]] = loo(loglik_15a)

  loo_wtd_06_list[[ite]] = loo_wtd(lpd_06_list[[ite]], svy_rake)
  loo_wtd_11_list[[ite]] = loo_wtd(lpd_11_list[[ite]], svy_rake)
  loo_wtd_13_list[[ite]] = loo_wtd(lpd_13_list[[ite]], svy_rake)
  loo_wtd_13a_list[[ite]] = loo_wtd(lpd_13a_list[[ite]], svy_rake)
  loo_wtd_15_list[[ite]] = loo_wtd(lpd_15_list[[ite]], svy_rake)
  loo_wtd_15a_list[[ite]] = loo_wtd(lpd_15a_list[[ite]], svy_rake)
}

