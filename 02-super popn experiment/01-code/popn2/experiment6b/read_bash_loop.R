## loading first iteration of the bash loop results
setwd("~/GitHub/LOO_MRP/02-super popn experiment/01-code/popn2/experiment6b")

# loading the first file
load("simulated_2.RData")

coef_list_list = list(coef_list)
elpd_popnest_list = list(elpd_popnest_all)
samp_data_list = list(samp_data)
samp_data2_list = list(samp_data2)
sim_list1 = list(sim_list)
pt_list = prob_truth

# loading the rest of the results
for(ite in c(3:13,15,16,19:30,32:39,41,43,56,59:69,72:77,79:81,83:88,90,91,93:100)) {
  load(paste0("simulated_",ite,".RData")) 
  coef_list_list[[ite]] = coef_list
  elpd_popnest_list[[ite]] = elpd_popnest_all
  samp_data_list[[ite]] = samp_data
  samp_data2_list[[ite]] = samp_data2
  sim_list1[[ite]] = sim_list
  pt_list[ite] = prob_truth
}

