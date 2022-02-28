## loading first iteration of the bash loop results
# exp_6c3
setwd("~/MRP project/02-super popn/02-data/experiment6c3")

# loading the first file
load("simulated_1.RData")

n = nrow(samp_data) # sample size

coef_list_list = list(coef_list)
elpd_popnest_list = list(elpd_popnest_all)
samp_data_list = list(samp_data)
samp_data2_list = list(samp_data2)
sim_list1 = list(sim_list)
pt_list = prob_truth

pred_list = list(do.call(rbind, pred_all))
pred_list[[1]]$model = rep(c(paste0('model0', 1:9), paste0('model', 10:15),
                             paste0('model', c(4,7,9,10,12:15), 'a')), each=n)
pred_list[[1]]$prob_truth = prob_truth

summ_list = list(do.call(rbind, summ_all))
summ_list[[1]]$model = c(paste0('model0', 1:9), paste0('model', 10:15),  
                         paste0('model', c(4,7,9,10,12:15), 'a'))


# loading the rest of the results
for(ite in c(2:100)) {
  load(paste0("simulated_",ite,".RData")) 
  coef_list_list[[ite]] = coef_list
  elpd_popnest_list[[ite]] = elpd_popnest_all
  samp_data_list[[ite]] = samp_data
  samp_data2_list[[ite]] = samp_data2
  sim_list1[[ite]] = sim_list
  pt_list[ite] = prob_truth
  
  pred_list[[ite]] = do.call(rbind, pred_all)
  pred_list[[ite]]$model = rep(c(paste0('model0', 1:9), paste0('model', 10:15),
                                 paste0('model', c(4,7,9,10,12:15), 'a')), each=n)
  pred_list[[ite]]$prob_truth = prob_truth
  
  summ_list[[ite]] = do.call(rbind, summ_all)
  summ_list[[ite]]$model = c(paste0('model0', 1:9), paste0('model', 10:15),  
                             paste0('model', c(4,7,9,10,12:15), 'a'))
  
}

