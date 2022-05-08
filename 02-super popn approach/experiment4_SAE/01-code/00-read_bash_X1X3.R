## postprocessing the saved objects from cluster
library(posterior) # to convert draws_array() objects 

# sourcing gen_dat()
source("00-gen_dat_func.R")

# sourcing loo_wtd()
source("../functions.R")

# empty lists
popnest_all_X1X3 =
  popnest_sae_X1_all =
  popnest_sae_X3_all =
  popnest_sae_X1_list = 
  popnest_sae_X3_list = list()

model12_popnest_sae_X1 = 
  model14_popnest_sae_X1 = 
  model06_popnest_sae_X1 = 
  model11_popnest_sae_X1 =
  model13_popnest_sae_X1 = 
  model15_popnest_sae_X1 =  
  model13a_popnest_sae_X1 = 
  model15a_popnest_sae_X1 =
  lapply(1:100,matrix, data=NA,nrow=4000, ncol=5)

model12_popnest_sae_X3 = 
  model14_popnest_sae_X3 = 
  model06_popnest_sae_X3 = 
  model11_popnest_sae_X3 =
  model13_popnest_sae_X3 = 
  model15_popnest_sae_X3 =
  model13a_popnest_sae_X3 = 
  model15a_popnest_sae_X3
  lapply(1:100,matrix, data=NA,nrow=4000, ncol=5)


iter = 1:2
for(ite in iter){
  print(ite)
  load(paste0('LOO_arPrior_',ite,".RData")) 
  
  # mrp est -----------------------------------------------------------------
  # generating data using gen_dat()
  set.seed(65438)
  sim1 = gen_dat(N = 10000, fx = fx3, samp_size = 500, ITE=ite)
  
  ## small area estimation for X1
  for (s in 1:5){
    
    lvl_loc = which(popn_ps$X1 == s)
    
    # calculating group popnest for X1-levels
    model12_popnest_sae_X1[[ite]][,s] = apply(as_draws_matrix(popnest_12[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc])) # applying to each iteration
    model14_popnest_sae_X1[[ite]][,s] = apply(as_draws_matrix(popnest_14[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    
    # calculating group popnest for X1-levels
    model06_popnest_sae_X1[[ite]][,s] = apply(as_draws_matrix(popnest_06[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc])) # applying to each iteration
    model11_popnest_sae_X1[[ite]][,s] = apply(as_draws_matrix(popnest_11[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model13_popnest_sae_X1[[ite]][,s] = apply(as_draws_matrix(popnest_13[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model15_popnest_sae_X1[[ite]][,s] = apply(as_draws_matrix(popnest_15[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model13a_popnest_sae_X1[[ite]][,s] = apply(as_draws_matrix(popnest_13a[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model15a_popnest_sae_X1[[ite]][,s] = apply(as_draws_matrix(popnest_15a[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    
  }
  
  popnest_sae_X1_all[[ite]] = list(model12_popnest_sae_X1[[ite]], 
                                   model14_popnest_sae_X1[[ite]],
                                   model06_popnest_sae_X1[[ite]], 
                                   model11_popnest_sae_X1[[ite]], 
                                   model13_popnest_sae_X1[[ite]], 
                                   model15_popnest_sae_X1[[ite]],
                                   model13a_popnest_sae_X1[[ite]], 
                                   model15a_popnest_sae_X1[[ite]] )
  
  popnest_sae_X1_list[[ite]] = lapply(popnest_sae_X1_all[[ite]], function(x)quantile(x,c(0.05, 0.5, 0.95))) %>% 
    do.call(rbind,.) %>% 
    data.frame(.) %>% 
    rename(popnestX5 = X5., popnestX50 = X50., popnestX95 = X95.) %>% 
    mutate(model = paste0('model', c('12','14','06','11','13','15','13a','15a')),
           iter = ite)
  
  ## small area estimation for X3
  for (s2 in 1:5){
    
    lvl_loc = which(popn_ps$X3 == s2)
    
    # calculating group popnest for X3-levels
    model12_popnest_sae_X3[[ite]][,s2] = apply(as_draws_matrix(popnest_12[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc])) # applying to each iteration
    model14_popnest_sae_X3[[ite]][,s2] = apply(as_draws_matrix(popnest_14[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    
    # calculating group popnest for X1-levels
    model06_popnest_sae_X3[[ite]][,s2] = apply(as_draws_matrix(popnest_06[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc])) # applying to each iteration
    model11_popnest_sae_X3[[ite]][,s2] = apply(as_draws_matrix(popnest_11[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model13_popnest_sae_X3[[ite]][,s2] = apply(as_draws_matrix(popnest_13[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model15_popnest_sae_X3[[ite]][,s2] = apply(as_draws_matrix(popnest_15[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model13a_popnest_sae_X3[[ite]][,s2] = apply(as_draws_matrix(popnest_13a[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model15a_popnest_sae_X3[[ite]][,s2] = apply(as_draws_matrix(popnest_15a[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
  }
  
  popnest_sae_X3_all[[ite]] = list(model12_popnest_sae_X3[[ite]], 
                                   model14_popnest_sae_X3[[ite]],
                                   model06_popnest_sae_X3[[ite]], 
                                   model11_popnest_sae_X3[[ite]], 
                                   model13_popnest_sae_X3[[ite]], 
                                   model15_popnest_sae_X3[[ite]],
                                   model13a_popnest_sae_X3[[ite]], 
                                   model15a_popnest_sae_X3[[ite]])
  
  popnest_sae_X3_list[[ite]] = lapply(popnest_sae_X3_all[[ite]], function(x)quantile(x,c(0.05, 0.5, 0.95))) %>% 
    do.call(rbind,.) %>% 
    data.frame(.) %>% 
    rename(popnestX5 = X5., popnestX50 = X50., popnestX95 = X95.) %>% 
    mutate(model = paste0('model', c('12','14','06','11','13','15','13a','15a')),
           iter = ite)
}  


save(popnest_sae_X1_all, popnest_sae_X3_all,
     popnest_sae_X1_list, popnest_sae_X3_list, file="X1X3.Rbin", compress=T)
