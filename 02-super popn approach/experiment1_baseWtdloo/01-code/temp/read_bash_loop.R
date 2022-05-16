## loading first iteration of the bash loop results
setwd("~/MRP project/02-super popn/02-data/experiment3b")

# loading the first file

setwd(here::here("02-super popn approach/experiment1_baseWtdloo/03-data/temp"))

load("simulated_1.RData")

elpd_popnest_list = list(elpd_popnest_all)
samp_data_list = list(samp_data)
samp_data2_list = list(samp_data2)
sim_list1 = list(sim_list)
pt_list = list(prob_truth)

# loading the rest of the results
for(ite in c(2:64,66:76,78:100)) {
  load(paste0("simulated_",ite,".RData")) 
  elpd_popnest_list[[ite]] = elpd_popnest_all
  samp_data_list[[ite]] = samp_data
  samp_data2_list[[ite]] = samp_data2
  sim_list1[[ite]] = sim_list
  pt_list[ite] = prob_truth
}


