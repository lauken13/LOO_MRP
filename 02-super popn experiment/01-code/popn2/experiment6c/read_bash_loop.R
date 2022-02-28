## plotting loo values for all the models 
## AR prior models 
## 13/02/2022
library(ggplot2)
library(reshape2)
library(dplyr)
library(magrittr)
library(tidyverse)
library(paletteer)
library(ggthemes)

setwd("~/GitHub/LOO_MRP/02-super popn experiment/02-data/popn2/experiment6c")

## plotting prob of truth
ITE =  c(2:13,15,16,19:21,23:30,33,34,36:39,41,44:56,59:69,72:77,79:81,83:88,90,91,93:100)

# loading the first file
load("simulated_2.RData")

coef_list_list = list(coef_list)
elpd_popnest_list = list(elpd_popnest_all)
samp_data_list = list(samp_data)
samp_data2_list = list(samp_data2)
sim_list1 = list(sim_list)
pt_list = prob_truth

# loading the rest of the results
for(ite in ITE) {
  load(paste0("simulated_",ite,".RData")) 
  coef_list_list[[ite]] = coef_list
  elpd_popnest_list[[ite]] = elpd_popnest_all
  samp_data_list[[ite]] = samp_data
  samp_data2_list[[ite]] = samp_data2
  sim_list1[[ite]] = sim_list
  pt_list[ite] = prob_truth
}


samp_data_list2 = samp_data_list[ite]
sim_out = sapply(samp_data_list2, function(x)mean(x$outcome))
prob_truth = pt_list[ite]
