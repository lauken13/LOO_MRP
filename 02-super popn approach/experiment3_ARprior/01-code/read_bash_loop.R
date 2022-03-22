## postprocessing the saved objects from cluster
library(loo)
library(posterior) # to convert draws_array() objects 

# sourcing gen_dat()
source(here::here("02-super popn approach/experiment3/01-code/stan code/00-LOO_gen_dat_func.R"))

# sourcing loo_wtd()
source(here::here("02-super popn approach/functions.R"))

## empty matrices
# for mrp est
popnest_list = list()
pt_list = list() 

# elpd values and wtd loo
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

## elpd values and std. error
elpd_06_mat = matrix(NA, ncol=2, nrow=100) # elpd and se, 100 iterations
elpd_15a_mat = 
  elpd_15_mat = 
  elpd_13a_mat = 
  elpd_13_mat = 
  elpd_11_mat =
  elpd_06_mat

samp_data_list =
  popn_data_list =
  popn_ps_list = list()

## fx2 
iter = c(1:88,92:100)


for(ite in 1:100){
  load(paste0('LOO_arPrior_',ite,".RData")) 
  
  # mrp est -----------------------------------------------------------------
  # generating data using gen_dat()
  set.seed(34567)
  sim1 = gen_dat(N = 10000, fx = fx1, samp_size = 1000, ITE=ite)
  
  samp_data = samp_data_list[[ite]] = sim1$samp_data
  popn_data = popn_data_list[[ite]] = sim1$popn_data
  popn_ps = popn_ps_list[[ite]] = sim1$popn_ps
  
  # calculating popnest
  model06_popnest = apply(as_draws_matrix(popnest_06), 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # applying to dimension 1 and 2 in the array (4 chains of 1000 iterations each)
  model11_popnest = apply(as_draws_matrix(popnest_11), 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj))
  model13_popnest = apply(as_draws_matrix(popnest_13), 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj))
  model13a_popnest = apply(as_draws_matrix(popnest_13a), 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj))
  model15_popnest = apply(as_draws_matrix(popnest_15), 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj))
  model15a_popnest = apply(as_draws_matrix(popnest_15a), 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj))
  
  popnest_all = list(model06_popnest, 
                     model11_popnest, 
                     model13_popnest, 
                     model13a_popnest,
                     model15_popnest,
                     model15a_popnest)
  
  popnest_list[[ite]] = lapply(popnest_all, function(x)quantile(x,c(0.05, 0.5, 0.95))) %>% 
    do.call(rbind,.) %>% 
    data.frame(.) %>% 
    rename(popnestX5 = X5., popnestX50 = X50., popnestX95 = X95.) %>% 
    mutate(model = paste0('model', c('06','11','13','13a','15','15a')),
           iter = ite)
  
  # prob of outcome for each iteration
  pt_list[ite] = mean(popn_data$y_obs) # or mean(popn_data$y_prob) 
  
  
  # weighted loo ------------------------------------------------------------
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
  
  ## saving loo object ####
  lpd_06_list[[ite]] = loo(loglik_06)
  lpd_11_list[[ite]] = loo(loglik_11)
  lpd_13_list[[ite]] = loo(loglik_13)
  lpd_13a_list[[ite]] = loo(loglik_13a)
  lpd_15_list[[ite]] = loo(loglik_15)
  lpd_15a_list[[ite]] = loo(loglik_15a)
  
  ## elpd_loo ####
  ## elpd values 
  elpd_06_mat[ite,1] = lpd_06_list[[ite]]$estimates[1,1]
  elpd_06_mat[ite,2] = lpd_06_list[[ite]]$estimates[1,2]
  
  elpd_11_mat[ite,1] = lpd_11_list[[ite]]$estimates[1,1]
  elpd_11_mat[ite,2] = lpd_11_list[[ite]]$estimates[1,2]
  
  elpd_13_mat[ite,1] = lpd_13_list[[ite]]$estimates[1,1]
  elpd_13_mat[ite,2] = lpd_13_list[[ite]]$estimates[1,2]
  
  elpd_13a_mat[ite,1] = lpd_13a_list[[ite]]$estimates[1,1]
  elpd_13a_mat[ite,2] = lpd_13a_list[[ite]]$estimates[1,2]
  
  elpd_15_mat[ite,1] = lpd_15_list[[ite]]$estimates[1,1]
  elpd_15_mat[ite,2] = lpd_15_list[[ite]]$estimates[1,2]
  
  elpd_15a_mat[ite,1] = lpd_15a_list[[ite]]$estimates[1,1]
  elpd_15a_mat[ite,2] = lpd_15a_list[[ite]]$estimates[1,2]
  
  ## wtd_loo ####
  loo_wtd_06_list[[ite]] = loo_wtd(lpd_06_list[[ite]], svy_rake)
  loo_wtd_11_list[[ite]] = loo_wtd(lpd_11_list[[ite]], svy_rake)
  loo_wtd_13_list[[ite]] = loo_wtd(lpd_13_list[[ite]], svy_rake)
  loo_wtd_13a_list[[ite]] = loo_wtd(lpd_13a_list[[ite]], svy_rake)
  loo_wtd_15_list[[ite]] = loo_wtd(lpd_15_list[[ite]], svy_rake)
  loo_wtd_15a_list[[ite]] = loo_wtd(lpd_15a_list[[ite]], svy_rake)
}  


