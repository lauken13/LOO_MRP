## postprocessing the saved objects from cluster
library(loo)
library(posterior) # to convert draws_array() objects 

# sourcing gen_dat()
source("00-gen_dat_func.R")

# sourcing loo_wtd()
source("../functions.R")

## empty matrices
# for mrp est
popnest_all_list =
  popnest_summary_list =  
  sampest_tab_all = 
  popnest_sae_X2_all =
  popnest_sae_X4_all =
  popnest_sae_X2_list = 
  popnest_sae_X4_list = list()
pt_list = list() 

# elpd values and wtd loo
loo_06_list = 
  loo_11_list = 
  loo_13_list = 
  loo_13a_list = 
  loo_15_list = 
  loo_15a_list = list()

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

pt_samp_list = 
pt_popn_list = 
samp_data_list =
  popn_data_list =
  popn_ps_list = list()

model06_popnest_sae_X4 = 
  model11_popnest_sae_X4 =
  model13_popnest_sae_X4 = 
  model13a_popnest_sae_X4 = 
  model15_popnest_sae_X4 =
  model15a_popnest_sae_X4 = lapply(1:100,matrix, data=NA,nrow=4000, ncol=12)

model06_popnest_sae_X2 = 
  model11_popnest_sae_X2 =
  model13_popnest_sae_X2 = 
  model13a_popnest_sae_X2 = 
  model15_popnest_sae_X2 =
  model15a_popnest_sae_X2 = lapply(1:100,matrix, data=NA,nrow=4000, ncol=5)

#iter = c(1:16,19:100)
iter = 19:100
for(ite in iter){
  print(ite)
  load(paste0('LOO_arPrior_',ite,".RData")) 
  
  # mrp est -----------------------------------------------------------------
  # generating data using gen_dat()
  set.seed(65438)
  sim1 = gen_dat(N = 10000, fx = fx3, samp_size = 500, ITE=ite)
  
  samp_data = samp_data_list[[ite]] = sim1$samp_data
  popn_data = popn_data_list[[ite]] = sim1$popn_data
  popn_ps = popn_ps_list[[ite]] = sim1$popn_ps
 
 pt_samp_list[[ite]] = mean(samp_data$y_prob)
 pt_popn_list[[ite]] = mean(popn_data$y_prob)
 
 ## individual estimates
 sampest_tab_06 = apply(sampest_06,2,quantile,c(0.05,0.5,0.95)) %>% 
   t() %>% 
   as_tibble() %>% 
   rename(sampestX5 = `5%`,
          sampestX50 = `50%`,
          sampestX95 = `95%`) %>% 
   mutate(model = 'model06',
          prob_truth = samp_data$y_prob,
          range_int = sampestX95 - sampestX5,
          coverage = ifelse(prob_truth >= sampestX5 & prob_truth <= sampestX95, 1, 0))
 
 sampest_tab_11 = apply(sampest_11,2,quantile,c(0.05,0.5,0.95)) %>% 
   t() %>% 
   as_tibble() %>% 
   rename(sampestX5 = `5%`,
          sampestX50 = `50%`,
          sampestX95 = `95%`) %>% 
   mutate(model = 'model11',
          prob_truth = samp_data$y_prob,
          range_int = sampestX95 - sampestX5,
          coverage = ifelse(prob_truth >= sampestX5 & prob_truth <= sampestX95, 1, 0))
 
 sampest_tab_13 = apply(sampest_13,2,quantile,c(0.05,0.5,0.95)) %>% 
   t() %>% 
   as_tibble() %>% 
   rename(sampestX5 = `5%`,
          sampestX50 = `50%`,
          sampestX95 = `95%`) %>% 
   mutate(model = 'model13',
          prob_truth = samp_data$y_prob,
          range_int = sampestX95 - sampestX5,
          coverage = ifelse(prob_truth >= sampestX5 & prob_truth <= sampestX95, 1, 0))
 
 sampest_tab_13a = apply(sampest_13a,2,quantile,c(0.05,0.5,0.95)) %>% 
   t() %>% 
   as_tibble() %>% 
   rename(sampestX5 = `5%`,
          sampestX50 = `50%`,
          sampestX95 = `95%`) %>% 
   mutate(model = 'model13a',
          prob_truth = samp_data$y_prob,
          range_int = sampestX95 - sampestX5,
          coverage = ifelse(prob_truth >= sampestX5 & prob_truth <= sampestX95, 1, 0))
 
 sampest_tab_15 = apply(sampest_15,2,quantile,c(0.05,0.5,0.95)) %>% 
   t() %>% 
   as_tibble() %>% 
   rename(sampestX5 = `5%`,
          sampestX50 = `50%`,
          sampestX95 = `95%`) %>% 
   mutate(model = 'model15',
          prob_truth = samp_data$y_prob,
          range_int = sampestX95 - sampestX5,
          coverage = ifelse(prob_truth >= sampestX5 & prob_truth <= sampestX95, 1, 0))
 
 sampest_tab_15a = apply(sampest_15a,2,quantile,c(0.05,0.5,0.95)) %>% 
   t() %>% 
   as_tibble() %>% 
   rename(sampestX5 = `5%`,
          sampestX50 = `50%`,
          sampestX95 = `95%`) %>% 
   mutate(model = 'model15a',
          prob_truth = samp_data$y_prob,
          range_int = sampestX95 - sampestX5,
          coverage = ifelse(prob_truth >= sampestX5 & prob_truth <= sampestX95, 1, 0))
 
 
 ## getting posterior of individuals estimate for each of the models
 sampest_tab_all[[ite]] = list(sampest_tab_06, sampest_tab_11, sampest_tab_13,
                               sampest_tab_13a, sampest_tab_15, sampest_tab_15a) %>% 
   do.call(rbind,.)
 
  # calculating popnest
  model06_popnest = apply(as_draws_matrix(popnest_06), 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # applying to each iteration
  model11_popnest = apply(as_draws_matrix(popnest_11), 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj))
  model13_popnest = apply(as_draws_matrix(popnest_13), 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj))
  model13a_popnest = apply(as_draws_matrix(popnest_13a), 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj))
  model15_popnest = apply(as_draws_matrix(popnest_15), 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj))
  model15a_popnest = apply(as_draws_matrix(popnest_15a), 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj))
  
  popnest_all_list[[ite]] = list(model06_popnest, 
                     model11_popnest, 
                     model13_popnest, 
                     model13a_popnest,
                     model15_popnest,
                     model15a_popnest)
  
  popnest_summary_list[[ite]] = lapply(popnest_all_list[[ite]], function(x)quantile(x,c(0.05, 0.5, 0.95))) %>% 
    do.call(rbind,.) %>% 
    data.frame(.) %>% 
    rename(popnestX5 = X5., popnestX50 = X50., popnestX95 = X95.) %>% 
    mutate(model = paste0('model', c('06','11','13','13a','15','15a')), iter = ite)
  
  ## small area estimation for X4
  for (s in 1:12){
    
    lvl_loc = which(popn_ps$X4 == s)

    # calculating group popnest for X4-levels
    model06_popnest_sae_X4[[ite]][,s] = apply(as_draws_matrix(popnest_06[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc])) # applying to each iteration
    model11_popnest_sae_X4[[ite]][,s] = apply(as_draws_matrix(popnest_11[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model13_popnest_sae_X4[[ite]][,s] = apply(as_draws_matrix(popnest_13[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model13a_popnest_sae_X4[[ite]][,s] = apply(as_draws_matrix(popnest_13a[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model15_popnest_sae_X4[[ite]][,s] = apply(as_draws_matrix(popnest_15[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model15a_popnest_sae_X4[[ite]][,s] = apply(as_draws_matrix(popnest_15a[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
  }
  
  popnest_sae_X4_all[[ite]] = list(model06_popnest_sae_X4[[ite]], 
                                   model11_popnest_sae_X4[[ite]], 
                                   model13_popnest_sae_X4[[ite]], 
                                   model13a_popnest_sae_X4[[ite]],
                                   model15_popnest_sae_X4[[ite]],
                                   model15a_popnest_sae_X4[[ite]])
  
  popnest_sae_X4_list[[ite]] = lapply(popnest_sae_X4_all[[ite]], function(x)quantile(x,c(0.05, 0.5, 0.95))) %>% 
    do.call(rbind,.) %>% 
    data.frame(.) %>% 
    rename(popnestX5 = X5., popnestX50 = X50., popnestX95 = X95.) %>% 
    mutate(model = paste0('model', c('06','11','13','13a','15','15a')),
           iter = ite)
  
  ## small area estimation for X2
  for (s2 in 1:5){
    
    lvl_loc = which(popn_ps$X2 == s2)
    
    # calculating group popnest for X4-levels
    model06_popnest_sae_X2[[ite]][,s2] = apply(as_draws_matrix(popnest_06[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc])) # applying to each iteration
    model11_popnest_sae_X2[[ite]][,s2] = apply(as_draws_matrix(popnest_11[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model13_popnest_sae_X2[[ite]][,s2] = apply(as_draws_matrix(popnest_13[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model13a_popnest_sae_X2[[ite]][,s2] = apply(as_draws_matrix(popnest_13a[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model15_popnest_sae_X2[[ite]][,s2] = apply(as_draws_matrix(popnest_15[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model15a_popnest_sae_X2[[ite]][,s2] = apply(as_draws_matrix(popnest_15a[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
  }
  
  popnest_sae_X2_all[[ite]] = list(model06_popnest_sae_X2[[ite]], 
                                   model11_popnest_sae_X2[[ite]], 
                                   model13_popnest_sae_X2[[ite]], 
                                   model13a_popnest_sae_X2[[ite]],
                                   model15_popnest_sae_X2[[ite]],
                                   model15a_popnest_sae_X2[[ite]])
  
  popnest_sae_X2_list[[ite]] = lapply(popnest_sae_X2_all[[ite]], function(x)quantile(x,c(0.05, 0.5, 0.95))) %>% 
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
  loo_06_list[[ite]] = loo_06
  loo_11_list[[ite]] = loo_11
  loo_13_list[[ite]] = loo_13
  loo_13a_list[[ite]] = loo_13a
  loo_15_list[[ite]] = loo_15
  loo_15a_list[[ite]] = loo_15a
  
  ## elpd_loo ####
  ## elpd values 
  elpd_06_mat[ite,1] = loo_06_list[[ite]]$estimates[1,1]
  elpd_06_mat[ite,2] = loo_06_list[[ite]]$estimates[1,2]
  
  elpd_11_mat[ite,1] = loo_11_list[[ite]]$estimates[1,1]
  elpd_11_mat[ite,2] = loo_11_list[[ite]]$estimates[1,2]
  
  elpd_13_mat[ite,1] = loo_13_list[[ite]]$estimates[1,1]
  elpd_13_mat[ite,2] = loo_13_list[[ite]]$estimates[1,2]
  
  elpd_13a_mat[ite,1] = loo_13a_list[[ite]]$estimates[1,1]
  elpd_13a_mat[ite,2] = loo_13a_list[[ite]]$estimates[1,2]
  
  elpd_15_mat[ite,1] = loo_15_list[[ite]]$estimates[1,1]
  elpd_15_mat[ite,2] = loo_15_list[[ite]]$estimates[1,2]
  
  elpd_15a_mat[ite,1] = loo_15a_list[[ite]]$estimates[1,1]
  elpd_15a_mat[ite,2] = loo_15a_list[[ite]]$estimates[1,2]
  
  ## wtd_loo ####
  loo_wtd_06_list[[ite]] = loo_wtd(loo_06_list[[ite]], svy_rake)
  loo_wtd_11_list[[ite]] = loo_wtd(loo_11_list[[ite]], svy_rake)
  loo_wtd_13_list[[ite]] = loo_wtd(loo_13_list[[ite]], svy_rake)
  loo_wtd_13a_list[[ite]] = loo_wtd(loo_13a_list[[ite]], svy_rake)
  loo_wtd_15_list[[ite]] = loo_wtd(loo_15_list[[ite]], svy_rake)
  loo_wtd_15a_list[[ite]] = loo_wtd(loo_15a_list[[ite]], svy_rake)
}  

#save.image(file="loo_sae_fx3.Rbin", compress=T)
#save(elpd_06_mat, elpd_11_mat, elpd_13_mat, 
 #    elpd_13a_mat, elpd_15_mat, elpd_15a_mat,
 #   loo_wtd_06_list, loo_wtd_11_list, loo_wtd_13_list, 
 #    loo_wtd_13a_list, loo_wtd_15_list, loo_wtd_15a_list,
 #    sampest_list, popnest_list, pt_popn_list, pt_samp_list,
 #    popnest_sae_X2_list, popnest_sae_X4_list, file="loo_sae_fx3.Rbin", compress=T)

save(elpd_06_mat, elpd_11_mat, elpd_13_mat, 
     elpd_13a_mat, elpd_15_mat, elpd_15a_mat,
    loo_wtd_06_list, loo_wtd_11_list, loo_wtd_13_list, 
     loo_wtd_13a_list, loo_wtd_15_list, loo_wtd_15a_list,
     sampest_all_list, sampest_summary_list, popnest_all_list,popnest_summary_list,
 pt_popn_list, pt_samp_list, pt_list,
     popnest_sae_X2_list, popnest_sae_X4_list, file="loo_sae_fx3_edit.Rbin", compress=T)


