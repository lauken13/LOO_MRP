## postprocessing the saved objects from cluster
library(loo)
library(posterior) # to convert draws_array() objects 
library(dplyr)
library(ggplot2)
library(survey)

## elpd_SAE
source(here::here("02-super popn approach/experiment1_baseWtdloo/01-code/SAE_elpd_all.R"), echo=T)
source(here::here("02-super popn approach/experiment1_baseWtdloo/01-code/SAE_elpd_wtd.R"), echo=T)

# SAE calculations --------------------------------------------------------
X1_group_mean_list =
  X2_group_mean_list =
  X3_group_mean_list = 
  X4_group_mean_list = list()

for (ite in iter){

# y_prob_mean by SAE ------------------------------------------------------------
  X1_group_mean_list[[ite]] =  popn_data_list[[ite]] %>% 
    group_by(X1) %>% 
    summarise(mean_yObs_sae = mean(y_prob)) %>% 
    mutate(iteration = ite)
  
  X2_group_mean_list[[ite]] =  popn_data_list[[ite]] %>% 
    group_by(X2) %>% 
    summarise(mean_yObs_sae = mean(y_prob)) %>% 
    mutate(iteration = ite)
  
  X3_group_mean_list[[ite]] =  popn_data_list[[ite]] %>% 
    group_by(X3) %>% 
    summarise(mean_yObs_sae = mean(y_prob)) %>% 
    mutate(iteration = ite)
  
  X4_group_mean_list[[ite]] =  popn_data_list[[ite]] %>% 
    group_by(X4) %>% 
    summarise(mean_yObs_sae = mean(y_prob)) %>% 
    mutate(iteration = ite)
}

# true group mean
X1_group_mean_tab = do.call(rbind, X1_group_mean_list)
X2_group_mean_tab = do.call(rbind, X2_group_mean_list)
X3_group_mean_tab = do.call(rbind, X3_group_mean_list)
X4_group_mean_tab = do.call(rbind, X4_group_mean_list)


# popnest for each level of X1,X2..X4 for all eight models --------
popnest_sae_X1_tab =
  popnest_sae_X1_model = 
  popnest_sae_X1 = list()
popnest_sae_X2_tab =
  popnest_sae_X2_model = 
  popnest_sae_X2 = list()
popnest_sae_X3_tab =
  popnest_sae_X3_model = 
  popnest_sae_X3 = list()
popnest_sae_X4_tab =
  popnest_sae_X4_model = 
  popnest_sae_X4 = list()

## getting the group prob for each iteration/population
for (ite in iter){
  for(m in 1:15){ # getting quantiles for each of the model
    popnest_sae_X1_model[[m]] = popnest_sae_X1_all[[ite]][m] %>%
      as.data.frame() %>% 
      apply(., 2, quantile, c(0.05, 0.5, 0.95))
    
    popnest_sae_X2_model[[m]] = popnest_sae_X2_all[[ite]][m] %>%
      as.data.frame() %>% 
      apply(., 2, quantile, c(0.05, 0.5, 0.95))
    
    popnest_sae_X3_model[[m]] = popnest_sae_X3_all[[ite]][m] %>%
      as.data.frame() %>% 
      apply(., 2, quantile, c(0.05, 0.5, 0.95))
    
    popnest_sae_X4_model[[m]] = popnest_sae_X4_all[[ite]][m] %>%
      as.data.frame() %>% 
      apply(., 2, quantile, c(0.05, 0.5, 0.95))
  }
  
  popnest_sae_X1[[ite]] = popnest_sae_X1_model
  popnest_sae_X2[[ite]] = popnest_sae_X2_model
  popnest_sae_X3[[ite]] = popnest_sae_X3_model
  popnest_sae_X4[[ite]] = popnest_sae_X4_model
}

# extracting every 1st,2nd ... 8th list of the main list
# X1 ----------------------------------------------------------------------
popnest_sae_X1_m1 = sapply(popnest_sae_X1, '[', 1) %>% 
  lapply(., function(x)t(x))
popnest_sae_X1_m2 = sapply(popnest_sae_X1, '[', 2) %>% 
  lapply(., function(x)t(x))
popnest_sae_X1_m3 = sapply(popnest_sae_X1, '[', 3) %>% 
  lapply(., function(x)t(x))
popnest_sae_X1_m4 = sapply(popnest_sae_X1, '[', 4) %>% 
  lapply(., function(x)t(x))
popnest_sae_X1_m5 = sapply(popnest_sae_X1, '[', 5) %>% 
  lapply(., function(x)t(x))
popnest_sae_X1_m6 = sapply(popnest_sae_X1, '[', 6) %>% 
  lapply(., function(x)t(x))
popnest_sae_X1_m7 = sapply(popnest_sae_X1, '[', 7) %>% 
  lapply(., function(x)t(x))
popnest_sae_X1_m8 = sapply(popnest_sae_X1, '[', 8) %>% 
  lapply(., function(x)t(x))
popnest_sae_X1_m9 = sapply(popnest_sae_X1, '[', 9) %>% 
  lapply(., function(x)t(x))
popnest_sae_X1_m10 = sapply(popnest_sae_X1, '[', 10) %>% 
  lapply(., function(x)t(x))
popnest_sae_X1_m11 = sapply(popnest_sae_X1, '[', 11) %>% 
  lapply(., function(x)t(x))
popnest_sae_X1_m12 = sapply(popnest_sae_X1, '[', 12) %>% 
  lapply(., function(x)t(x))
popnest_sae_X1_m13 = sapply(popnest_sae_X1, '[', 13) %>% 
  lapply(., function(x)t(x))
popnest_sae_X1_m14 = sapply(popnest_sae_X1, '[', 14) %>% 
  lapply(., function(x)t(x))
popnest_sae_X1_m15 = sapply(popnest_sae_X1, '[', 15) %>% 
  lapply(., function(x)t(x))


# X2 ----------------------------------------------------------------------
popnest_sae_X2_m1 = sapply(popnest_sae_X2, '[', 1) %>% 
  lapply(., function(x)t(x))
popnest_sae_X2_m2 = sapply(popnest_sae_X2, '[', 2) %>% 
  lapply(., function(x)t(x))
popnest_sae_X2_m3 = sapply(popnest_sae_X2, '[', 3) %>% 
  lapply(., function(x)t(x))
popnest_sae_X2_m4 = sapply(popnest_sae_X2, '[', 4) %>% 
  lapply(., function(x)t(x))
popnest_sae_X2_m5 = sapply(popnest_sae_X2, '[', 5) %>% 
  lapply(., function(x)t(x))
popnest_sae_X2_m6 = sapply(popnest_sae_X2, '[', 6) %>% 
  lapply(., function(x)t(x))
popnest_sae_X2_m7 = sapply(popnest_sae_X2, '[', 7) %>% 
  lapply(., function(x)t(x))
popnest_sae_X2_m8 = sapply(popnest_sae_X2, '[', 8) %>% 
  lapply(., function(x)t(x))
popnest_sae_X2_m9 = sapply(popnest_sae_X2, '[', 9) %>% 
  lapply(., function(x)t(x))
popnest_sae_X2_m10 = sapply(popnest_sae_X2, '[', 10) %>% 
  lapply(., function(x)t(x))
popnest_sae_X2_m11 = sapply(popnest_sae_X2, '[', 11) %>% 
  lapply(., function(x)t(x))
popnest_sae_X2_m12 = sapply(popnest_sae_X2, '[', 12) %>% 
  lapply(., function(x)t(x))
popnest_sae_X2_m13 = sapply(popnest_sae_X2, '[', 13) %>% 
  lapply(., function(x)t(x))
popnest_sae_X2_m14 = sapply(popnest_sae_X2, '[', 14) %>% 
  lapply(., function(x)t(x))
popnest_sae_X2_m15 = sapply(popnest_sae_X2, '[', 15) %>% 
  lapply(., function(x)t(x))


# X3 ----------------------------------------------------------------------
popnest_sae_X3_m1 = sapply(popnest_sae_X3, '[', 1) %>% 
  lapply(., function(x)t(x))
popnest_sae_X3_m2 = sapply(popnest_sae_X3, '[', 2) %>% 
  lapply(., function(x)t(x))
popnest_sae_X3_m3 = sapply(popnest_sae_X3, '[', 3) %>% 
  lapply(., function(x)t(x))
popnest_sae_X3_m4 = sapply(popnest_sae_X3, '[', 4) %>% 
  lapply(., function(x)t(x))
popnest_sae_X3_m5 = sapply(popnest_sae_X3, '[', 5) %>% 
  lapply(., function(x)t(x))
popnest_sae_X3_m6 = sapply(popnest_sae_X3, '[', 6) %>% 
  lapply(., function(x)t(x))
popnest_sae_X3_m7 = sapply(popnest_sae_X3, '[', 7) %>% 
  lapply(., function(x)t(x))
popnest_sae_X3_m8 = sapply(popnest_sae_X3, '[', 8) %>% 
  lapply(., function(x)t(x))
popnest_sae_X3_m9 = sapply(popnest_sae_X3, '[', 9) %>% 
  lapply(., function(x)t(x))
popnest_sae_X3_m10 = sapply(popnest_sae_X3, '[', 10) %>% 
  lapply(., function(x)t(x))
popnest_sae_X3_m11 = sapply(popnest_sae_X3, '[', 11) %>% 
  lapply(., function(x)t(x))
popnest_sae_X3_m12 = sapply(popnest_sae_X3, '[', 12) %>% 
  lapply(., function(x)t(x))
popnest_sae_X3_m13 = sapply(popnest_sae_X3, '[', 13) %>% 
  lapply(., function(x)t(x))
popnest_sae_X3_m14 = sapply(popnest_sae_X3, '[', 14) %>% 
  lapply(., function(x)t(x))
popnest_sae_X3_m15 = sapply(popnest_sae_X3, '[', 15) %>% 
  lapply(., function(x)t(x))

# X4 ----------------------------------------------------------------------
popnest_sae_X4_m1 = sapply(popnest_sae_X4, '[', 1) %>% 
  lapply(., function(x)t(x))
popnest_sae_X4_m2 = sapply(popnest_sae_X4, '[', 2) %>% 
  lapply(., function(x)t(x))
popnest_sae_X4_m3 = sapply(popnest_sae_X4, '[', 3) %>% 
  lapply(., function(x)t(x))
popnest_sae_X4_m4 = sapply(popnest_sae_X4, '[', 4) %>% 
  lapply(., function(x)t(x))
popnest_sae_X4_m5 = sapply(popnest_sae_X4, '[', 5) %>% 
  lapply(., function(x)t(x))
popnest_sae_X4_m6 = sapply(popnest_sae_X4, '[', 6) %>% 
  lapply(., function(x)t(x))
popnest_sae_X4_m7 = sapply(popnest_sae_X4, '[', 7) %>% 
  lapply(., function(x)t(x))
popnest_sae_X4_m8 = sapply(popnest_sae_X4, '[', 8) %>% 
  lapply(., function(x)t(x))
popnest_sae_X4_m9 = sapply(popnest_sae_X4, '[', 9) %>% 
  lapply(., function(x)t(x))
popnest_sae_X4_m10 = sapply(popnest_sae_X4, '[', 10) %>% 
  lapply(., function(x)t(x))
popnest_sae_X4_m11 = sapply(popnest_sae_X4, '[', 11) %>% 
  lapply(., function(x)t(x))
popnest_sae_X4_m12 = sapply(popnest_sae_X4, '[', 12) %>% 
  lapply(., function(x)t(x))
popnest_sae_X4_m13 = sapply(popnest_sae_X4, '[', 13) %>% 
  lapply(., function(x)t(x))
popnest_sae_X4_m14 = sapply(popnest_sae_X4, '[', 14) %>% 
  lapply(., function(x)t(x))
popnest_sae_X4_m15 = sapply(popnest_sae_X4, '[', 15) %>% 
  lapply(., function(x)t(x))


calc_list_X1_m1 = calc_list_X1_m2 = calc_list_X1_m3 = 
  calc_list_X1_m4 = calc_list_X1_m5 = calc_list_X1_m6 = 
  calc_list_X1_m7 = calc_list_X1_m8 = calc_list_X1_m9 = 
  calc_list_X1_m10 = calc_list_X1_m11 = calc_list_X1_m12 = 
  calc_list_X1_m13 = calc_list_X1_m14 = calc_list_X1_m15 = list()

calc_list_X2_m1 = calc_list_X2_m2 = calc_list_X2_m3 = 
  calc_list_X2_m4 = calc_list_X2_m5 = calc_list_X2_m6 = 
  calc_list_X2_m7 = calc_list_X2_m8 = calc_list_X2_m9 = 
  calc_list_X2_m10 = calc_list_X2_m11 = calc_list_X2_m12 = 
  calc_list_X2_m13 = calc_list_X2_m14 = calc_list_X2_m15 = list()

calc_list_X3_m1 = calc_list_X3_m2 = calc_list_X3_m3 = 
  calc_list_X3_m4 = calc_list_X3_m5 = calc_list_X3_m6 = 
  calc_list_X3_m7 = calc_list_X3_m8 = calc_list_X3_m9 = 
  calc_list_X3_m10 = calc_list_X3_m11 = calc_list_X3_m12 = 
  calc_list_X3_m13 = calc_list_X3_m14 = calc_list_X3_m15 = list()

calc_list_X4_m1 = calc_list_X4_m2 = calc_list_X4_m3 = 
  calc_list_X4_m4 = calc_list_X4_m5 = calc_list_X4_m6 = 
  calc_list_X4_m7 = calc_list_X4_m8 = calc_list_X4_m9 = 
  calc_list_X4_m10 = calc_list_X4_m11 = calc_list_X4_m12 = 
  calc_list_X4_m13 = calc_list_X4_m14 = calc_list_X4_m15 = list()


for(i in iter){
  # truth value for each iteration

# X1 ----------------------------------------------------------------------
  t1 = X1_group_mean_tab %>% 
    filter(iteration == i) 
  
  # combining truth and quantile of each iteration
  calc_tab_X1_m1 = cbind(popnest_sae_X1_m1[[i]], t1)
  calc_tab_X1_m2 = cbind(popnest_sae_X1_m2[[i]], t1)
  calc_tab_X1_m3 = cbind(popnest_sae_X1_m3[[i]], t1)
  calc_tab_X1_m4 = cbind(popnest_sae_X1_m4[[i]], t1)
  calc_tab_X1_m5 = cbind(popnest_sae_X1_m5[[i]], t1)
  calc_tab_X1_m6 = cbind(popnest_sae_X1_m6[[i]], t1)
  calc_tab_X1_m7 = cbind(popnest_sae_X1_m7[[i]], t1)
  calc_tab_X1_m8 = cbind(popnest_sae_X1_m8[[i]], t1)
  calc_tab_X1_m9 = cbind(popnest_sae_X1_m9[[i]], t1)
  calc_tab_X1_m10 = cbind(popnest_sae_X1_m10[[i]], t1)
  calc_tab_X1_m11 = cbind(popnest_sae_X1_m11[[i]], t1)
  calc_tab_X1_m12 = cbind(popnest_sae_X1_m12[[i]], t1)
  calc_tab_X1_m13 = cbind(popnest_sae_X1_m13[[i]], t1)
  calc_tab_X1_m14 = cbind(popnest_sae_X1_m14[[i]], t1)
  calc_tab_X1_m15 = cbind(popnest_sae_X1_m15[[i]], t1)

  
  ## comparing to 'truth'
  calc_tab2_X1_m1 = calc_tab_X1_m1 %>% 
    mutate(bias_X50_X1sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X1sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X1sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X1sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X1_m2 = calc_tab_X1_m2 %>% 
    mutate(bias_X50_X1sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X1sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X1sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X1sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X1_m3 = calc_tab_X1_m3 %>% 
    mutate(bias_X50_X1sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X1sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X1sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X1sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X1_m4 = calc_tab_X1_m4 %>% 
    mutate(bias_X50_X1sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X1sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X1sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X1sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X1_m5 = calc_tab_X1_m5 %>% 
    mutate(bias_X50_X1sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X1sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X1sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X1sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X1_m6 = calc_tab_X1_m6 %>% 
    mutate(bias_X50_X1sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X1sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X1sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X1sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X1_m7 = calc_tab_X1_m7 %>% 
    mutate(bias_X50_X1sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X1sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X1sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X1sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X1_m8 = calc_tab_X1_m8 %>% 
    mutate(bias_X50_X1sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X1sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X1sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X1sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X1_m9 = calc_tab_X1_m9 %>% 
    mutate(bias_X50_X1sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X1sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X1sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X1sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X1_m10 = calc_tab_X1_m10 %>% 
    mutate(bias_X50_X1sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X1sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X1sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X1sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X1_m11 = calc_tab_X1_m11 %>% 
    mutate(bias_X50_X1sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X1sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X1sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X1sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X1_m12 = calc_tab_X1_m12 %>% 
    mutate(bias_X50_X1sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X1sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X1sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X1sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X1_m13 = calc_tab_X1_m13 %>% 
    mutate(bias_X50_X1sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X1sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X1sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X1sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X1_m14 = calc_tab_X1_m14 %>% 
    mutate(bias_X50_X1sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X1sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X1sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X1sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X1_m15 = calc_tab_X1_m15 %>% 
    mutate(bias_X50_X1sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X1sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X1sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X1sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_list_X1_m1[[i]] = calc_tab2_X1_m1
  calc_list_X1_m2[[i]] = calc_tab2_X1_m2
  calc_list_X1_m3[[i]] = calc_tab2_X1_m3
  calc_list_X1_m4[[i]] = calc_tab2_X1_m4
  calc_list_X1_m5[[i]] = calc_tab2_X1_m5
  calc_list_X1_m6[[i]] = calc_tab2_X1_m6
  calc_list_X1_m7[[i]] = calc_tab2_X1_m7
  calc_list_X1_m8[[i]] = calc_tab2_X1_m8
  calc_list_X1_m9[[i]] = calc_tab2_X1_m9
  calc_list_X1_m10[[i]] = calc_tab2_X1_m10
  calc_list_X1_m11[[i]] = calc_tab2_X1_m11
  calc_list_X1_m12[[i]] = calc_tab2_X1_m12
  calc_list_X1_m13[[i]] = calc_tab2_X1_m13
  calc_list_X1_m14[[i]] = calc_tab2_X1_m14
  calc_list_X1_m15[[i]] = calc_tab2_X1_m15

# X2 ----------------------------------------------------------------------
  t2 = X2_group_mean_tab %>% 
    filter(iteration == i) 
  
  calc_tab_X2_m1 = cbind(popnest_sae_X2_m1[[i]], t2)
  calc_tab_X2_m2 = cbind(popnest_sae_X2_m2[[i]], t2)
  calc_tab_X2_m3 = cbind(popnest_sae_X2_m3[[i]], t2)
  calc_tab_X2_m4 = cbind(popnest_sae_X2_m4[[i]], t2)
  calc_tab_X2_m5 = cbind(popnest_sae_X2_m5[[i]], t2)
  calc_tab_X2_m6 = cbind(popnest_sae_X2_m6[[i]], t2)
  calc_tab_X2_m7 = cbind(popnest_sae_X2_m7[[i]], t2)
  calc_tab_X2_m8 = cbind(popnest_sae_X2_m8[[i]], t2)
  calc_tab_X2_m9 = cbind(popnest_sae_X2_m9[[i]], t2)
  calc_tab_X2_m10 = cbind(popnest_sae_X2_m10[[i]], t2)
  calc_tab_X2_m11 = cbind(popnest_sae_X2_m11[[i]], t2)
  calc_tab_X2_m12 = cbind(popnest_sae_X2_m12[[i]], t2)
  calc_tab_X2_m13 = cbind(popnest_sae_X2_m13[[i]], t2)
  calc_tab_X2_m14 = cbind(popnest_sae_X2_m14[[i]], t2)
  calc_tab_X2_m15 = cbind(popnest_sae_X2_m15[[i]], t2)
  
  ## comparing to 'truth'
  calc_tab2_X2_m1 = calc_tab_X2_m1 %>% 
    mutate(bias_X50_X2sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X2sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X2sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X2sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X2_m2 = calc_tab_X2_m2 %>% 
    mutate(bias_X50_X2sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X2sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X2sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X2sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X2_m3 = calc_tab_X2_m3 %>% 
    mutate(bias_X50_X2sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X2sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X2sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X2sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X2_m4 = calc_tab_X2_m4 %>% 
    mutate(bias_X50_X2sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X2sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X2sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X2sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X2_m5 = calc_tab_X2_m5 %>% 
    mutate(bias_X50_X2sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X2sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X2sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X2sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X2_m6 = calc_tab_X2_m6 %>% 
    mutate(bias_X50_X2sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X2sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X2sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X2sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X2_m7 = calc_tab_X2_m7 %>% 
    mutate(bias_X50_X2sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X2sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X2sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X2sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X2_m8 = calc_tab_X2_m8 %>% 
    mutate(bias_X50_X2sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X2sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X2sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X2sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X2_m9 = calc_tab_X2_m9 %>% 
    mutate(bias_X50_X2sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X2sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X2sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X2sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X2_m10 = calc_tab_X2_m10 %>% 
    mutate(bias_X50_X2sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X2sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X2sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X2sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X2_m11 = calc_tab_X2_m11 %>% 
    mutate(bias_X50_X2sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X2sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X2sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X2sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X2_m12 = calc_tab_X2_m12 %>% 
    mutate(bias_X50_X2sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X2sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X2sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X2sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X2_m13 = calc_tab_X2_m13 %>% 
    mutate(bias_X50_X2sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X2sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X2sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X2sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X2_m14 = calc_tab_X2_m14 %>% 
    mutate(bias_X50_X2sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X2sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X2sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X2sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X2_m15 = calc_tab_X2_m15 %>% 
    mutate(bias_X50_X2sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X2sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X2sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X2sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_list_X2_m1[[i]] = calc_tab2_X2_m1
  calc_list_X2_m2[[i]] = calc_tab2_X2_m2
  calc_list_X2_m3[[i]] = calc_tab2_X2_m3
  calc_list_X2_m4[[i]] = calc_tab2_X2_m4
  calc_list_X2_m5[[i]] = calc_tab2_X2_m5
  calc_list_X2_m6[[i]] = calc_tab2_X2_m6
  calc_list_X2_m7[[i]] = calc_tab2_X2_m7
  calc_list_X2_m8[[i]] = calc_tab2_X2_m8
  calc_list_X2_m9[[i]] = calc_tab2_X2_m9
  calc_list_X2_m10[[i]] = calc_tab2_X2_m10
  calc_list_X2_m11[[i]] = calc_tab2_X2_m11
  calc_list_X2_m12[[i]] = calc_tab2_X2_m12
  calc_list_X2_m13[[i]] = calc_tab2_X2_m13
  calc_list_X2_m14[[i]] = calc_tab2_X2_m14
  calc_list_X2_m15[[i]] = calc_tab2_X2_m15
 
# X3 ----------------------------------------------------------------------
  # truth value for each iteration
  t3 = X3_group_mean_tab %>% 
    filter(iteration == i) 
  
  calc_tab_X3_m1 = cbind(popnest_sae_X3_m1[[i]], t3)
  calc_tab_X3_m2 = cbind(popnest_sae_X3_m2[[i]], t3)
  calc_tab_X3_m3 = cbind(popnest_sae_X3_m3[[i]], t3)
  calc_tab_X3_m4 = cbind(popnest_sae_X3_m4[[i]], t3)
  calc_tab_X3_m5 = cbind(popnest_sae_X3_m5[[i]], t3)
  calc_tab_X3_m6 = cbind(popnest_sae_X3_m6[[i]], t3)
  calc_tab_X3_m7 = cbind(popnest_sae_X3_m7[[i]], t3)
  calc_tab_X3_m8 = cbind(popnest_sae_X3_m8[[i]], t3)
  calc_tab_X3_m9 = cbind(popnest_sae_X3_m9[[i]], t3)
  calc_tab_X3_m10 = cbind(popnest_sae_X3_m10[[i]], t3)
  calc_tab_X3_m11 = cbind(popnest_sae_X3_m11[[i]], t3)
  calc_tab_X3_m12 = cbind(popnest_sae_X3_m12[[i]], t3)
  calc_tab_X3_m13 = cbind(popnest_sae_X3_m13[[i]], t3)
  calc_tab_X3_m14 = cbind(popnest_sae_X3_m14[[i]], t3)
  calc_tab_X3_m15 = cbind(popnest_sae_X3_m15[[i]], t3)
  
  ## comparing to 'truth'
  calc_tab2_X3_m1 = calc_tab_X3_m1 %>% 
    mutate(bias_X50_X3sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X3sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X3sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X3sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X3_m2 = calc_tab_X3_m2 %>% 
    mutate(bias_X50_X3sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X3sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X3sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X3sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X3_m3 = calc_tab_X3_m3 %>% 
    mutate(bias_X50_X3sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X3sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X3sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X3sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X3_m4 = calc_tab_X3_m4 %>% 
    mutate(bias_X50_X3sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X3sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X3sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X3sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X3_m5 = calc_tab_X3_m5 %>% 
    mutate(bias_X50_X3sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X3sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X3sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X3sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X3_m6 = calc_tab_X3_m6 %>% 
    mutate(bias_X50_X3sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X3sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X3sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X3sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X3_m7 = calc_tab_X3_m7 %>% 
    mutate(bias_X50_X3sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X3sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X3sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X3sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X3_m8 = calc_tab_X3_m8 %>% 
    mutate(bias_X50_X3sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X3sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X3sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X3sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X3_m9 = calc_tab_X3_m9 %>% 
    mutate(bias_X50_X3sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X3sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X3sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X3sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X3_m10 = calc_tab_X3_m10 %>% 
    mutate(bias_X50_X3sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X3sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X3sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X3sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X3_m11 = calc_tab_X3_m11 %>% 
    mutate(bias_X50_X3sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X3sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X3sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X3sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X3_m12 = calc_tab_X3_m12 %>% 
    mutate(bias_X50_X3sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X3sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X3sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X3sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X3_m13 = calc_tab_X3_m13 %>% 
    mutate(bias_X50_X3sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X3sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X3sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X3sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X3_m14 = calc_tab_X3_m14 %>% 
    mutate(bias_X50_X3sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X3sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X3sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X3sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X3_m15 = calc_tab_X3_m15 %>% 
    mutate(bias_X50_X3sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X3sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X3sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X3sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_list_X3_m1[[i]] = calc_tab2_X3_m1
  calc_list_X3_m2[[i]] = calc_tab2_X3_m2
  calc_list_X3_m3[[i]] = calc_tab2_X3_m3
  calc_list_X3_m4[[i]] = calc_tab2_X3_m4
  calc_list_X3_m5[[i]] = calc_tab2_X3_m5
  calc_list_X3_m6[[i]] = calc_tab2_X3_m6
  calc_list_X3_m7[[i]] = calc_tab2_X3_m7
  calc_list_X3_m8[[i]] = calc_tab2_X3_m8
  calc_list_X3_m9[[i]] = calc_tab2_X3_m9
  calc_list_X3_m10[[i]] = calc_tab2_X3_m10
  calc_list_X3_m11[[i]] = calc_tab2_X3_m11
  calc_list_X3_m12[[i]] = calc_tab2_X3_m12
  calc_list_X3_m13[[i]] = calc_tab2_X3_m13
  calc_list_X3_m14[[i]] = calc_tab2_X3_m14
  calc_list_X3_m15[[i]] = calc_tab2_X3_m15
  
  
# X4 ----------------------------------------------------------------------
  # truth value for each iteration
  t4 = X4_group_mean_tab %>% 
    filter(iteration == i) 
  
  calc_tab_X4_m1 = cbind(popnest_sae_X4_m1[[i]], t4)
  calc_tab_X4_m2 = cbind(popnest_sae_X4_m2[[i]], t4)
  calc_tab_X4_m3 = cbind(popnest_sae_X4_m3[[i]], t4)
  calc_tab_X4_m4 = cbind(popnest_sae_X4_m4[[i]], t4)
  calc_tab_X4_m5 = cbind(popnest_sae_X4_m5[[i]], t4)
  calc_tab_X4_m6 = cbind(popnest_sae_X4_m6[[i]], t4)
  calc_tab_X4_m7 = cbind(popnest_sae_X4_m7[[i]], t4)
  calc_tab_X4_m8 = cbind(popnest_sae_X4_m8[[i]], t4)
  calc_tab_X4_m9 = cbind(popnest_sae_X4_m9[[i]], t4)
  calc_tab_X4_m10 = cbind(popnest_sae_X4_m10[[i]], t4)
  calc_tab_X4_m11 = cbind(popnest_sae_X4_m11[[i]], t4)
  calc_tab_X4_m12 = cbind(popnest_sae_X4_m12[[i]], t4)
  calc_tab_X4_m13 = cbind(popnest_sae_X4_m13[[i]], t4)
  calc_tab_X4_m14 = cbind(popnest_sae_X4_m14[[i]], t4)
  calc_tab_X4_m15 = cbind(popnest_sae_X4_m15[[i]], t4)
  
  ## comparing to 'truth'
  calc_tab2_X4_m1 = calc_tab_X4_m1 %>% 
    mutate(bias_X50_X4sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X4sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X4sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X4sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X4_m2 = calc_tab_X4_m2 %>% 
    mutate(bias_X50_X4sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X4sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X4sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X4sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X4_m3 = calc_tab_X4_m3 %>% 
    mutate(bias_X50_X4sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X4sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X4sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X4sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X4_m4 = calc_tab_X4_m4 %>% 
    mutate(bias_X50_X4sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X4sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X4sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X4sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X4_m5 = calc_tab_X4_m5 %>% 
    mutate(bias_X50_X4sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X4sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X4sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X4sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X4_m6 = calc_tab_X4_m6 %>% 
    mutate(bias_X50_X4sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X4sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X4sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X4sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X4_m7 = calc_tab_X4_m7 %>% 
    mutate(bias_X50_X4sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X4sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X4sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X4sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X4_m8 = calc_tab_X4_m8 %>% 
    mutate(bias_X50_X4sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X4sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X4sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X4sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X4_m9 = calc_tab_X4_m9 %>% 
    mutate(bias_X50_X4sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X4sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X4sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X4sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X4_m10 = calc_tab_X4_m10 %>% 
    mutate(bias_X50_X4sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X4sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X4sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X4sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X4_m11 = calc_tab_X4_m11 %>% 
    mutate(bias_X50_X4sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X4sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X4sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X4sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X4_m12 = calc_tab_X4_m12 %>% 
    mutate(bias_X50_X4sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X4sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X4sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X4sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X4_m13 = calc_tab_X4_m13 %>% 
    mutate(bias_X50_X4sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X4sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X4sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X4sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X4_m14 = calc_tab_X4_m14 %>% 
    mutate(bias_X50_X4sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X4sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X4sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X4sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X4_m15 = calc_tab_X4_m15 %>% 
    mutate(bias_X50_X4sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X4sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X4sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X4sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_list_X4_m1[[i]] = calc_tab2_X4_m1
  calc_list_X4_m2[[i]] = calc_tab2_X4_m2
  calc_list_X4_m3[[i]] = calc_tab2_X4_m3
  calc_list_X4_m4[[i]] = calc_tab2_X4_m4
  calc_list_X4_m5[[i]] = calc_tab2_X4_m5
  calc_list_X4_m6[[i]] = calc_tab2_X4_m6
  calc_list_X4_m7[[i]] = calc_tab2_X4_m7
  calc_list_X4_m8[[i]] = calc_tab2_X4_m8
  calc_list_X4_m9[[i]] = calc_tab2_X4_m9
  calc_list_X4_m10[[i]] = calc_tab2_X4_m10
  calc_list_X4_m11[[i]] = calc_tab2_X4_m11
  calc_list_X4_m12[[i]] = calc_tab2_X4_m12
  calc_list_X4_m13[[i]] = calc_tab2_X4_m13
  calc_list_X4_m14[[i]] = calc_tab2_X4_m14
  calc_list_X4_m15[[i]] = calc_tab2_X4_m15
  
} 


# combining all of the iterations - X1 ------------------------------------
sae_X1_tab_m1 = do.call(rbind, calc_list_X1_m1) %>% 
  mutate(model = "X1")
sae_X1_tab_m2 = do.call(rbind, calc_list_X1_m2) %>% 
  mutate(model = "X2")
sae_X1_tab_m3 = do.call(rbind, calc_list_X1_m3) %>% 
  mutate(model = "X3")
sae_X1_tab_m4 = do.call(rbind, calc_list_X1_m4) %>% 
  mutate(model = "X4")
sae_X1_tab_m5 = do.call(rbind, calc_list_X1_m5) %>% 
  mutate(model = "X1 + X2")

sae_X1_tab_m6 = do.call(rbind, calc_list_X1_m6) %>% 
  mutate(model = "X1 + X3")
sae_X1_tab_m7 = do.call(rbind, calc_list_X1_m7) %>% 
  mutate(model = "X1 + X4")
sae_X1_tab_m8 = do.call(rbind, calc_list_X1_m8) %>% 
  mutate(model = "X2 + X3")
sae_X1_tab_m9 = do.call(rbind, calc_list_X1_m9) %>% 
  mutate(model = "X2 + X4")
sae_X1_tab_m10 = do.call(rbind, calc_list_X1_m10) %>% 
  mutate(model = "X3 + X4")

sae_X1_tab_m11 = do.call(rbind, calc_list_X1_m11) %>% 
  mutate(model = "X1 + X2 + X3")
sae_X1_tab_m12 = do.call(rbind, calc_list_X1_m12) %>% 
  mutate(model = "X1 + X2 + X4")
sae_X1_tab_m13 = do.call(rbind, calc_list_X1_m13) %>% 
  mutate(model = "X1 + X3 + X4")
sae_X1_tab_m14 = do.call(rbind, calc_list_X1_m14) %>% 
  mutate(model = "X2 + X3 + X4")
sae_X1_tab_m15 = do.call(rbind, calc_list_X1_m15) %>% 
  mutate(model = "X1 + X2 + X3 + X4")



alph = 0.1
model_sae_X1_tab = rbind(sae_X1_tab_m1, sae_X1_tab_m2, sae_X1_tab_m3, sae_X1_tab_m4, sae_X1_tab_m5,
                         sae_X1_tab_m6, sae_X1_tab_m7, sae_X1_tab_m8, sae_X1_tab_m9, sae_X1_tab_m10,
                         sae_X1_tab_m11, sae_X1_tab_m12, sae_X1_tab_m13, sae_X1_tab_m14, sae_X1_tab_m15) %>% 
  rename(X1saeEst_X5 = `5%`,
         X1saeEst_X50 = `50%`,
         X1saeEst_X95 = `95%`) %>% 
  mutate(intervalScr_X1sae = (X1saeEst_X95 - X1saeEst_X5) + 
           ((2 / alph * (X1saeEst_X5 - mean_yObs_sae)) * ifelse(mean_yObs_sae < X1saeEst_X5, 1, 0)) + 
           ((2 / alph * (mean_yObs_sae -  X1saeEst_X95)) * ifelse(mean_yObs_sae > X1saeEst_X95, 1, 0))) %>% 
  left_join(., elpd_X1sae_sum_tab, by=c('model', 'iteration', 'X1')) %>% 
  left_join(., elpd_X1sae_mean_tab, by=c('model', 'iteration', 'X1')) %>% 
  left_join(., elpd_X1sae_se_tab, by=c('model', 'iteration', 'X1'))  %>% 
  left_join(., tabX1_all, by=c('model', 'iteration', 'X1'))

# combining all of the iterations - X2 ------------------------------------
sae_X2_tab_m1 = do.call(rbind, calc_list_X2_m1) %>% 
  mutate(model = "X1")
sae_X2_tab_m2 = do.call(rbind, calc_list_X2_m2) %>% 
  mutate(model = "X2")
sae_X2_tab_m3 = do.call(rbind, calc_list_X2_m3) %>% 
  mutate(model = "X3")
sae_X2_tab_m4 = do.call(rbind, calc_list_X2_m4) %>% 
  mutate(model = "X4")
sae_X2_tab_m5 = do.call(rbind, calc_list_X2_m5) %>% 
  mutate(model = "X1 + X2")

sae_X2_tab_m6 = do.call(rbind, calc_list_X2_m6) %>% 
  mutate(model = "X1 + X3")
sae_X2_tab_m7 = do.call(rbind, calc_list_X2_m7) %>% 
  mutate(model = "X1 + X4")
sae_X2_tab_m8 = do.call(rbind, calc_list_X2_m8) %>% 
  mutate(model = "X2 + X3")
sae_X2_tab_m9 = do.call(rbind, calc_list_X2_m9) %>% 
  mutate(model = "X2 + X4")
sae_X2_tab_m10 = do.call(rbind, calc_list_X2_m10) %>% 
  mutate(model = "X3 + X4")

sae_X2_tab_m11 = do.call(rbind, calc_list_X2_m11) %>% 
  mutate(model = "X1 + X2 + X3")
sae_X2_tab_m12 = do.call(rbind, calc_list_X2_m12) %>% 
  mutate(model = "X1 + X2 + X4")
sae_X2_tab_m13 = do.call(rbind, calc_list_X2_m13) %>% 
  mutate(model = "X1 + X3 + X4")
sae_X2_tab_m14 = do.call(rbind, calc_list_X2_m14) %>% 
  mutate(model = "X2 + X3 + X4")
sae_X2_tab_m15 = do.call(rbind, calc_list_X2_m15) %>% 
  mutate(model = "X1 + X2 + X3 + X4")

model_sae_X2_tab = rbind(sae_X2_tab_m1, sae_X2_tab_m2, sae_X2_tab_m3, sae_X2_tab_m4, sae_X2_tab_m5,
                         sae_X2_tab_m6, sae_X2_tab_m7, sae_X2_tab_m8, sae_X2_tab_m9, sae_X2_tab_m10,
                         sae_X2_tab_m11, sae_X2_tab_m12, sae_X2_tab_m13, sae_X2_tab_m14, sae_X2_tab_m15) %>% 
  rename(X2saeEst_X5 = `5%`,
         X2saeEst_X50 = `50%`,
         X2saeEst_X95 = `95%`) %>% 
  mutate(intervalScr_X2sae = (X2saeEst_X95 - X2saeEst_X5) + 
           ((2 / alph * (X2saeEst_X5 - mean_yObs_sae)) * ifelse(mean_yObs_sae < X2saeEst_X5, 1, 0)) + 
           ((2 / alph * (mean_yObs_sae -  X2saeEst_X95)) * ifelse(mean_yObs_sae > X2saeEst_X95, 1, 0))) %>% 
  left_join(., elpd_X2sae_sum_tab, by=c('model', 'iteration', 'X2')) %>% 
  left_join(., elpd_X2sae_mean_tab, by=c('model', 'iteration', 'X2')) %>% 
  left_join(., elpd_X2sae_se_tab, by=c('model', 'iteration', 'X2'))  %>% 
  left_join(., tabX2_all, by=c('model', 'iteration', 'X2'))

# combining all of the iterations - X3 ------------------------------------
sae_X3_tab_m1 = do.call(rbind, calc_list_X3_m1) %>% 
  mutate(model = "X1")
sae_X3_tab_m2 = do.call(rbind, calc_list_X3_m2) %>% 
  mutate(model = "X2")
sae_X3_tab_m3 = do.call(rbind, calc_list_X3_m3) %>% 
  mutate(model = "X3")
sae_X3_tab_m4 = do.call(rbind, calc_list_X3_m4) %>% 
  mutate(model = "X4")
sae_X3_tab_m5 = do.call(rbind, calc_list_X3_m5) %>% 
  mutate(model = "X1 + X2")

sae_X3_tab_m6 = do.call(rbind, calc_list_X3_m6) %>% 
  mutate(model = "X1 + X3")
sae_X3_tab_m7 = do.call(rbind, calc_list_X3_m7) %>% 
  mutate(model = "X1 + X4")
sae_X3_tab_m8 = do.call(rbind, calc_list_X3_m8) %>% 
  mutate(model = "X2 + X3")
sae_X3_tab_m9 = do.call(rbind, calc_list_X3_m9) %>% 
  mutate(model = "X2 + X4")
sae_X3_tab_m10 = do.call(rbind, calc_list_X3_m10) %>% 
  mutate(model = "X3 + X4")

sae_X3_tab_m11 = do.call(rbind, calc_list_X3_m11) %>% 
  mutate(model = "X1 + X2 + X3")
sae_X3_tab_m12 = do.call(rbind, calc_list_X3_m12) %>% 
  mutate(model = "X1 + X2 + X4")
sae_X3_tab_m13 = do.call(rbind, calc_list_X3_m13) %>% 
  mutate(model = "X1 + X3 + X4")
sae_X3_tab_m14 = do.call(rbind, calc_list_X3_m14) %>% 
  mutate(model = "X2 + X3 + X4")
sae_X3_tab_m15 = do.call(rbind, calc_list_X3_m15) %>% 
  mutate(model = "X1 + X2 + X3 + X4")

model_sae_X3_tab = rbind(sae_X3_tab_m1, sae_X3_tab_m2, sae_X3_tab_m3, sae_X3_tab_m4, sae_X3_tab_m5,
                         sae_X3_tab_m6, sae_X3_tab_m7, sae_X3_tab_m8, sae_X3_tab_m9, sae_X3_tab_m10,
                         sae_X3_tab_m11, sae_X3_tab_m12, sae_X3_tab_m13, sae_X3_tab_m14, sae_X3_tab_m15) %>% 
  rename(X3saeEst_X5 = `5%`,
         X3saeEst_X50 = `50%`,
         X3saeEst_X95 = `95%`) %>% 
  mutate(intervalScr_X3sae = (X3saeEst_X95 - X3saeEst_X5) + 
           ((2 / alph * (X3saeEst_X5 - mean_yObs_sae)) * ifelse(mean_yObs_sae < X3saeEst_X5, 1, 0)) + 
           ((2 / alph * (mean_yObs_sae -  X3saeEst_X95)) * ifelse(mean_yObs_sae > X3saeEst_X95, 1, 0))) %>% 
  left_join(., elpd_X3sae_sum_tab, by=c('model', 'iteration', 'X3')) %>% 
  left_join(., elpd_X3sae_mean_tab, by=c('model', 'iteration', 'X3')) %>% 
  left_join(., elpd_X3sae_se_tab, by=c('model', 'iteration', 'X3'))  %>% 
  left_join(., tabX3_all, by=c('model', 'iteration', 'X3'))


# combining all of the iterations - X4 ------------------------------------
sae_X4_tab_m1 = do.call(rbind, calc_list_X4_m1) %>% 
  mutate(model = "X1")
sae_X4_tab_m2 = do.call(rbind, calc_list_X4_m2) %>% 
  mutate(model = "X2")
sae_X4_tab_m3 = do.call(rbind, calc_list_X4_m3) %>% 
  mutate(model = "X3")
sae_X4_tab_m4 = do.call(rbind, calc_list_X4_m4) %>% 
  mutate(model = "X4")
sae_X4_tab_m5 = do.call(rbind, calc_list_X4_m5) %>% 
  mutate(model = "X1 + X2")

sae_X4_tab_m6 = do.call(rbind, calc_list_X4_m6) %>% 
  mutate(model = "X1 + X3")
sae_X4_tab_m7 = do.call(rbind, calc_list_X4_m7) %>% 
  mutate(model = "X1 + X4")
sae_X4_tab_m8 = do.call(rbind, calc_list_X4_m8) %>% 
  mutate(model = "X2 + X3")
sae_X4_tab_m9 = do.call(rbind, calc_list_X4_m9) %>% 
  mutate(model = "X2 + X4")
sae_X4_tab_m10 = do.call(rbind, calc_list_X4_m10) %>% 
  mutate(model = "X3 + X4")

sae_X4_tab_m11 = do.call(rbind, calc_list_X4_m11) %>% 
  mutate(model = "X1 + X2 + X3")
sae_X4_tab_m12 = do.call(rbind, calc_list_X4_m12) %>% 
  mutate(model = "X1 + X2 + X4")
sae_X4_tab_m13 = do.call(rbind, calc_list_X4_m13) %>% 
  mutate(model = "X1 + X3 + X4")
sae_X4_tab_m14 = do.call(rbind, calc_list_X4_m14) %>% 
  mutate(model = "X2 + X3 + X4")
sae_X4_tab_m15 = do.call(rbind, calc_list_X4_m15) %>% 
  mutate(model = "X1 + X2 + X3 + X4")

model_sae_X4_tab = rbind(sae_X4_tab_m1, sae_X4_tab_m2, sae_X4_tab_m3, sae_X4_tab_m4, sae_X4_tab_m5,
                         sae_X4_tab_m6, sae_X4_tab_m7, sae_X4_tab_m8, sae_X4_tab_m9, sae_X4_tab_m10,
                         sae_X4_tab_m11, sae_X4_tab_m12, sae_X4_tab_m13, sae_X4_tab_m14, sae_X4_tab_m15) %>% 
  rename(X4saeEst_X5 = `5%`,
         X4saeEst_X50 = `50%`,
         X4saeEst_X95 = `95%`) %>% 
  mutate(intervalScr_X4sae = (X4saeEst_X95 - X4saeEst_X5) + 
           ((2 / alph * (X4saeEst_X5 - mean_yObs_sae)) * ifelse(mean_yObs_sae < X4saeEst_X5, 1, 0)) + 
           ((2 / alph * (mean_yObs_sae -  X4saeEst_X95)) * ifelse(mean_yObs_sae > X4saeEst_X95, 1, 0))) %>% 
  left_join(., elpd_X4sae_sum_tab, by=c('model', 'iteration', 'X4')) %>% 
  left_join(., elpd_X4sae_mean_tab, by=c('model', 'iteration', 'X4')) %>% 
  left_join(., elpd_X4sae_se_tab, by=c('model', 'iteration', 'X4')) %>% 
  left_join(., tabX4_all, by=c('model', 'iteration', 'X4'))

