## postprocessing the saved objects from cluster
# load(here::here("02-super popn approach/experiment4_SAE/03-data/loo_sae_1000.RData"))

library(loo)
library(posterior) # to convert draws_array() objects 
library(dplyr)
library(ggplot2)
library(survey)

# elpd_SAE
source(here::here("02-super popn approach/experiment4_SAE/01-code/SAE_elpd_all.R"), echo=T)
source(here::here("02-super popn approach/experiment4_SAE/01-code/SAE_elpd_wtd.R"), echo=T)

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
  for(m in 1:8){ # getting quantiles for each of the model
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
popnest_sae_X1_m6 = sapply(popnest_sae_X1, '[', 1) %>% 
  lapply(., function(x)t(x))
popnest_sae_X1_m11 = sapply(popnest_sae_X1, '[', 2) %>% 
  lapply(., function(x)t(x))
popnest_sae_X1_m13 = sapply(popnest_sae_X1, '[', 3) %>% 
  lapply(., function(x)t(x))
popnest_sae_X1_m13a = sapply(popnest_sae_X1, '[', 4) %>% 
  lapply(., function(x)t(x))
popnest_sae_X1_m15 = sapply(popnest_sae_X1, '[', 5) %>% 
  lapply(., function(x)t(x))
popnest_sae_X1_m15a = sapply(popnest_sae_X1, '[', 6) %>% 
  lapply(., function(x)t(x))
popnest_sae_X1_m12 = sapply(popnest_sae_X1, '[', 7) %>% 
  lapply(., function(x)as.data.frame(x)) %>% 
  lapply(., function(x)t(x))
popnest_sae_X1_m14 = sapply(popnest_sae_X1, '[', 8) %>% 
  lapply(., function(x)as.data.frame(x)) %>% 
  lapply(., function(x)t(x))


# X2 ----------------------------------------------------------------------
popnest_sae_X2_m6 = sapply(popnest_sae_X2, '[', 1) %>% 
  lapply(., function(x)t(x))
popnest_sae_X2_m11 = sapply(popnest_sae_X2, '[', 2) %>% 
  lapply(., function(x)t(x))
popnest_sae_X2_m13 = sapply(popnest_sae_X2, '[', 3) %>% 
  lapply(., function(x)t(x))
popnest_sae_X2_m13a = sapply(popnest_sae_X2, '[', 4) %>% 
  lapply(., function(x)t(x))
popnest_sae_X2_m15 = sapply(popnest_sae_X2, '[', 5) %>% 
  lapply(., function(x)t(x))
popnest_sae_X2_m15a = sapply(popnest_sae_X2, '[', 6) %>% 
  lapply(., function(x)t(x))
popnest_sae_X2_m12 = sapply(popnest_sae_X2, '[', 7) %>% 
  lapply(., function(x)as.data.frame(x)) %>% 
  lapply(., function(x)t(x))
popnest_sae_X2_m14 = sapply(popnest_sae_X2, '[', 8) %>% 
  lapply(., function(x)as.data.frame(x)) %>% 
  lapply(., function(x)t(x))


# X3 ----------------------------------------------------------------------
popnest_sae_X3_m6 = sapply(popnest_sae_X3, '[', 1) %>% 
  lapply(., function(x)t(x))
popnest_sae_X3_m11 = sapply(popnest_sae_X3, '[', 2) %>% 
  lapply(., function(x)t(x))
popnest_sae_X3_m13 = sapply(popnest_sae_X3, '[', 3) %>% 
  lapply(., function(x)t(x))
popnest_sae_X3_m13a = sapply(popnest_sae_X3, '[', 4) %>% 
  lapply(., function(x)t(x))
popnest_sae_X3_m15 = sapply(popnest_sae_X3, '[', 5) %>% 
  lapply(., function(x)t(x))
popnest_sae_X3_m15a = sapply(popnest_sae_X3, '[', 6) %>% 
  lapply(., function(x)t(x))
popnest_sae_X3_m12 = sapply(popnest_sae_X3, '[', 7) %>% 
  lapply(., function(x)as.data.frame(x)) %>% 
  lapply(., function(x)t(x))
popnest_sae_X3_m14 = sapply(popnest_sae_X3, '[', 8) %>% 
  lapply(., function(x)as.data.frame(x)) %>% 
  lapply(., function(x)t(x))


# X4 ----------------------------------------------------------------------
popnest_sae_X4_m6 = sapply(popnest_sae_X4, '[', 1) %>% 
  lapply(., function(x)as.data.frame(x)) %>% 
  lapply(., function(x)t(x))
popnest_sae_X4_m11 = sapply(popnest_sae_X4, '[', 2) %>% 
  lapply(., function(x)as.data.frame(x)) %>% 
  lapply(., function(x)t(x))
popnest_sae_X4_m13 = sapply(popnest_sae_X4, '[', 3) %>% 
  lapply(., function(x)as.data.frame(x)) %>% 
  lapply(., function(x)t(x))
popnest_sae_X4_m13a = sapply(popnest_sae_X4, '[', 4) %>% 
  lapply(., function(x)as.data.frame(x)) %>% 
  lapply(., function(x)t(x))
popnest_sae_X4_m15 = sapply(popnest_sae_X4, '[', 5) %>% 
  lapply(., function(x)as.data.frame(x)) %>% 
  lapply(., function(x)t(x))
popnest_sae_X4_m15a = sapply(popnest_sae_X4, '[', 6) %>% 
  lapply(., function(x)as.data.frame(x)) %>% 
  lapply(., function(x)t(x))
popnest_sae_X4_m12 = sapply(popnest_sae_X4, '[', 7) %>% 
  lapply(., function(x)as.data.frame(x)) %>% 
  lapply(., function(x)t(x))
popnest_sae_X4_m14 = sapply(popnest_sae_X4, '[', 8) %>% 
  lapply(., function(x)as.data.frame(x)) %>% 
  lapply(., function(x)t(x))


calc_list_X1_m6 = calc_list_X1_m11 = calc_list_X1_m13 = 
  calc_list_X1_m13a = calc_list_X1_m15 = calc_list_X1_m15a = 
  calc_list_X1_m12 =  calc_list_X1_m14 = list()
calc_list_X2_m6 = calc_list_X2_m11 =  calc_list_X2_m13 = 
  calc_list_X2_m13a = calc_list_X2_m15 = calc_list_X2_m15a =
  calc_list_X2_m12 = calc_list_X2_m14 = list()
calc_list_X3_m6 = calc_list_X3_m11 = calc_list_X3_m13 = 
  calc_list_X3_m13a = calc_list_X3_m15 = calc_list_X3_m15a =
  calc_list_X3_m12 = calc_list_X3_m14 = list()
calc_list_X4_m6 = calc_list_X4_m11 =calc_list_X4_m13 = 
  calc_list_X4_m13a = calc_list_X4_m15 = calc_list_X4_m15a =
  calc_list_X4_m12 = calc_list_X4_m14 = list()

for(i in iter){
  # truth value for each iteration

# X1 ----------------------------------------------------------------------
  t1 = X1_group_mean_tab %>% 
    filter(iteration == i) 
  
  # combining truth and quantile of each iteration
  calc_tab_X1_m6 = cbind(popnest_sae_X1_m6[[i]], t1)
  calc_tab_X1_m11 = cbind(popnest_sae_X1_m11[[i]], t1)
  calc_tab_X1_m13 = cbind(popnest_sae_X1_m13[[i]], t1)
  calc_tab_X1_m13a = cbind(popnest_sae_X1_m13a[[i]], t1)
  calc_tab_X1_m15 = cbind(popnest_sae_X1_m15[[i]], t1)
  calc_tab_X1_m15a = cbind(popnest_sae_X1_m15a[[i]], t1)
  calc_tab_X1_m12 = cbind(popnest_sae_X1_m12[[i]], t1)
  calc_tab_X1_m14 = cbind(popnest_sae_X1_m14[[i]], t1)
  
  ## comparing to 'truth'
  calc_tab2_X1_m6 = calc_tab_X1_m6 %>% 
    mutate(bias_X50_X1sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X1sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X1sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X1sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X1_m11 = calc_tab_X1_m11 %>% 
    mutate(bias_X50_X1sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X1sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X1sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X1sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X1_m13 = calc_tab_X1_m13 %>% 
    mutate(bias_X50_X1sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X1sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X1sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X1sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X1_m13a = calc_tab_X1_m13a %>% 
    mutate(bias_X50_X1sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X1sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X1sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X1sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X1_m15 = calc_tab_X1_m15 %>% 
    mutate(bias_X50_X1sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X1sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X1sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X1sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X1_m15a = calc_tab_X1_m15a %>% 
    mutate(bias_X50_X1sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X1sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X1sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X1sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X1_m12 = calc_tab_X1_m12 %>% 
    mutate(bias_X50_X1sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X1sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X1sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X1sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X1_m14 = calc_tab_X1_m14 %>% 
    mutate(bias_X50_X1sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X1sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X1sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X1sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_list_X1_m6[[i]] = calc_tab2_X1_m6
  calc_list_X1_m11[[i]] = calc_tab2_X1_m11
  calc_list_X1_m13[[i]] = calc_tab2_X1_m13
  calc_list_X1_m13a[[i]] = calc_tab2_X1_m13a
  calc_list_X1_m15[[i]] = calc_tab2_X1_m15
  calc_list_X1_m15a[[i]] = calc_tab2_X1_m15a
  calc_list_X1_m12[[i]] = calc_tab2_X1_m12
  calc_list_X1_m14[[i]] = calc_tab2_X1_m14
  

# X2 ----------------------------------------------------------------------
  t2 = X2_group_mean_tab %>% 
    filter(iteration == i) 
  
  calc_tab_X2_m6 = cbind(popnest_sae_X2_m6[[i]], t2)
  calc_tab_X2_m11 = cbind(popnest_sae_X2_m11[[i]], t2)
  calc_tab_X2_m13 = cbind(popnest_sae_X2_m13[[i]], t2)
  calc_tab_X2_m13a = cbind(popnest_sae_X2_m13a[[i]], t2)
  calc_tab_X2_m15 = cbind(popnest_sae_X2_m15[[i]], t2)
  calc_tab_X2_m15a = cbind(popnest_sae_X2_m15a[[i]], t2)
  calc_tab_X2_m12 = cbind(popnest_sae_X2_m12[[i]], t2)
  calc_tab_X2_m14 = cbind(popnest_sae_X2_m14[[i]], t2)
  
  ## comparing to 'truth'
  calc_tab2_X2_m6 = calc_tab_X2_m6 %>% 
    mutate(bias_X50_X2sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X2sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X2sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X2sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X2_m11 = calc_tab_X2_m11 %>% 
    mutate(bias_X50_X2sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X2sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X2sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X2sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X2_m13 = calc_tab_X2_m13 %>% 
    mutate(bias_X50_X2sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X2sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X2sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X2sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X2_m13a = calc_tab_X2_m13a %>% 
    mutate(bias_X50_X2sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X2sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X2sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X2sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X2_m15 = calc_tab_X2_m15 %>% 
    mutate(bias_X50_X2sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X2sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X2sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X2sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X2_m15a= calc_tab_X2_m15a %>% 
    mutate(bias_X50_X2sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X2sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X2sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X2sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X2_m12 = calc_tab_X2_m12 %>% 
    mutate(bias_X50_X2sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X2sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X2sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X2sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X2_m14 = calc_tab_X2_m14 %>% 
    mutate(bias_X50_X2sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X2sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X2sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X2sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  
  calc_list_X2_m6[[i]] = calc_tab2_X2_m6
  calc_list_X2_m11[[i]] = calc_tab2_X2_m11
  calc_list_X2_m13[[i]] = calc_tab2_X2_m13
  calc_list_X2_m13a[[i]] = calc_tab2_X2_m13a
  calc_list_X2_m15[[i]] = calc_tab2_X2_m15
  calc_list_X2_m15a[[i]] = calc_tab2_X2_m15a
  calc_list_X2_m12[[i]] = calc_tab2_X2_m12
  calc_list_X2_m14[[i]] = calc_tab2_X2_m14
  

# X3 ----------------------------------------------------------------------
  # truth value for each iteration
  t3 = X3_group_mean_tab %>% 
    filter(iteration == i) 
  
  # combining truth and quantile of each iteration
  calc_tab_X3_m6 = cbind(popnest_sae_X3_m6[[i]], t3)
  calc_tab_X3_m11 = cbind(popnest_sae_X3_m11[[i]], t3)
  calc_tab_X3_m13 = cbind(popnest_sae_X3_m13[[i]], t3)
  calc_tab_X3_m13a = cbind(popnest_sae_X3_m13a[[i]], t3)
  calc_tab_X3_m15 = cbind(popnest_sae_X3_m15[[i]], t3)
  calc_tab_X3_m15a = cbind(popnest_sae_X3_m15a[[i]], t3)
  calc_tab_X3_m12 = cbind(popnest_sae_X3_m12[[i]], t3)
  calc_tab_X3_m14 = cbind(popnest_sae_X3_m14[[i]], t3)
  
  ## comparing to 'truth'
  calc_tab2_X3_m6 = calc_tab_X3_m6 %>% 
    mutate(bias_X50_X3sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X3sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X3sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X3sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X3_m11 = calc_tab_X3_m11 %>% 
    mutate(bias_X50_X3sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X3sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X3sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X3sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X3_m13 = calc_tab_X3_m13 %>% 
    mutate(bias_X50_X3sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X3sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X3sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X3sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X3_m13a = calc_tab_X3_m13a %>% 
    mutate(bias_X50_X3sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X3sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X3sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X3sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X3_m15 = calc_tab_X3_m15 %>% 
    mutate(bias_X50_X3sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X3sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X3sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X3sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X3_m15a= calc_tab_X3_m15a %>% 
    mutate(bias_X50_X3sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X3sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X3sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X3sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X3_m12 = calc_tab_X3_m12 %>% 
    mutate(bias_X50_X3sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X3sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X3sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X3sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X3_m14 = calc_tab_X3_m14 %>% 
    mutate(bias_X50_X3sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X3sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X3sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X3sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_list_X3_m6[[i]] = calc_tab2_X3_m6
  calc_list_X3_m11[[i]] = calc_tab2_X3_m11
  calc_list_X3_m13[[i]] = calc_tab2_X3_m13
  calc_list_X3_m13a[[i]] = calc_tab2_X3_m13a
  calc_list_X3_m15[[i]] = calc_tab2_X3_m15
  calc_list_X3_m15a[[i]] = calc_tab2_X3_m15a
  calc_list_X3_m12[[i]] = calc_tab2_X3_m12
  calc_list_X3_m14[[i]] = calc_tab2_X3_m14  
  
# X4 ----------------------------------------------------------------------
  # truth value for each iteration
  t4 = X4_group_mean_tab %>% 
    filter(iteration == i) 
  
  # combining truth and quantile of each iteration
  calc_tab_X4_m6 = cbind(popnest_sae_X4_m6[[i]], t4)
  calc_tab_X4_m11 = cbind(popnest_sae_X4_m11[[i]], t4)
  calc_tab_X4_m13 = cbind(popnest_sae_X4_m13[[i]], t4)
  calc_tab_X4_m13a = cbind(popnest_sae_X4_m13a[[i]], t4)
  calc_tab_X4_m15 = cbind(popnest_sae_X4_m15[[i]], t4)
  calc_tab_X4_m15a = cbind(popnest_sae_X4_m15a[[i]], t4)
  calc_tab_X4_m12 = cbind(popnest_sae_X4_m12[[i]], t4)
  calc_tab_X4_m14 = cbind(popnest_sae_X4_m14[[i]], t4)
  
  ## comparing to 'truth'
  calc_tab2_X4_m6 = calc_tab_X4_m6 %>% 
    mutate(bias_X50_X4sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X4sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X4sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X4sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X4_m11 = calc_tab_X4_m11 %>% 
    mutate(bias_X50_X4sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X4sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X4sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X4sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X4_m13 = calc_tab_X4_m13 %>% 
    mutate(bias_X50_X4sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X4sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X4sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X4sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X4_m13a = calc_tab_X4_m13a %>% 
    mutate(bias_X50_X4sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X4sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X4sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X4sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X4_m15 = calc_tab_X4_m15 %>% 
    mutate(bias_X50_X4sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X4sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X4sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X4sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X4_m15a = calc_tab_X4_m15a %>% 
    mutate(bias_X50_X4sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X4sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X4sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X4sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X4_m12 = calc_tab_X4_m12 %>% 
    mutate(bias_X50_X4sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X4sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X4sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X4sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_tab2_X4_m14 = calc_tab_X4_m14 %>% 
    mutate(bias_X50_X4sae = (as.numeric(`50%`) - as.numeric(mean_yObs_sae)),
           bias_X5_X4sae = (as.numeric(`5%`) - as.numeric(mean_yObs_sae)),
           bias_X95_X4sae = (as.numeric(`95%`) - as.numeric(mean_yObs_sae)),
           coverage_X4sae = ifelse(mean_yObs_sae >= `5%` & mean_yObs_sae <= `95%`, 1, 0))
  
  calc_list_X4_m6[[i]] = calc_tab2_X4_m6
  calc_list_X4_m11[[i]] = calc_tab2_X4_m11
  calc_list_X4_m13[[i]] = calc_tab2_X4_m13
  calc_list_X4_m13a[[i]] = calc_tab2_X4_m13a
  calc_list_X4_m15[[i]] = calc_tab2_X4_m15
  calc_list_X4_m15a[[i]] = calc_tab2_X4_m15a
  calc_list_X4_m12[[i]] = calc_tab2_X4_m12
  calc_list_X4_m14[[i]] = calc_tab2_X4_m14
} 


# combining all of the iterations - X1 ------------------------------------
sae_X1_tab_m6 = do.call(rbind, calc_list_X1_m6) %>% 
  mutate(model = "X1 + X3")
sae_X1_tab_m11 = do.call(rbind, calc_list_X1_m11) %>% 
  mutate(model = "X1 + X2 + X3")
sae_X1_tab_m13 = do.call(rbind, calc_list_X1_m13) %>% 
  mutate(model = "X1 + X3 + X4")
sae_X1_tab_m13a = do.call(rbind, calc_list_X1_m13a) %>% 
  mutate(model = "*X1 + X3 + X4")
sae_X1_tab_m15 = do.call(rbind, calc_list_X1_m15) %>% 
  mutate(model = "X1 + X2 + X3 + X4")
sae_X1_tab_m15a = do.call(rbind, calc_list_X1_m15a) %>% 
  mutate(model = "*X1 + X2 + X3 + X4")
sae_X1_tab_m12 = do.call(rbind, calc_list_X1_m12) %>% 
  mutate(model = "X1 + X2 + X4")
sae_X1_tab_m14 = do.call(rbind, calc_list_X1_m14) %>% 
  mutate(model = "X2 + X3 + X4")

alph = 0.1
model_sae_X1_tab = rbind(sae_X1_tab_m6, sae_X1_tab_m11, sae_X1_tab_m13,
                         sae_X1_tab_m13a, sae_X1_tab_m15, sae_X1_tab_m15a,
                         sae_X1_tab_m12, sae_X1_tab_m14) %>% 
  rename(X1saeEst_X5 = `5%`,
         X1saeEst_X50 = `50%`,
         X1saeEst_X95 = `95%`) %>% 
  mutate(intervalScr_X1sae = (X1saeEst_X95 - X1saeEst_X5) + 
           ((2 / alph * (X1saeEst_X5 - mean_yObs_sae)) * ifelse(mean_yObs_sae < X1saeEst_X5, 1, 0)) + 
           ((2 / alph * (mean_yObs_sae -  X1saeEst_X95)) * ifelse(mean_yObs_sae > X1saeEst_X95, 1, 0))) %>% 
  left_join(., elpd_X1sae_sum_tab, by=c('model', 'iteration', 'X1')) %>% 
  left_join(., elpd_X1sae_mean_tab, by=c('model', 'iteration', 'X1')) %>% 
  left_join(., elpd_X1sae_se_tab, by=c('model', 'iteration', 'X1'))  %>% 
  left_join(., tabX1_all, by=c('model', 'iteration', 'X1'))  %>% 
  left_join(., wtdTabX1_all, by=c('model', 'iteration', 'X1'))

# combining all of the iterations - X2 ------------------------------------
sae_X2_tab_m6 = do.call(rbind, calc_list_X2_m6) %>% 
  mutate(model = "X1 + X3")
sae_X2_tab_m11 = do.call(rbind, calc_list_X2_m11) %>% 
  mutate(model = "X1 + X2 + X3")
sae_X2_tab_m13 = do.call(rbind, calc_list_X2_m13) %>% 
  mutate(model = "X1 + X3 + X4")
sae_X2_tab_m13a = do.call(rbind, calc_list_X2_m13a) %>% 
  mutate(model = "*X1 + X3 + X4")
sae_X2_tab_m15 = do.call(rbind, calc_list_X2_m15) %>% 
  mutate(model = "X1 + X2 + X3 + X4")
sae_X2_tab_m15a = do.call(rbind, calc_list_X2_m15a) %>% 
  mutate(model = "*X1 + X2 + X3 + X4")
sae_X2_tab_m12 = do.call(rbind, calc_list_X2_m12) %>% 
  mutate(model = "X1 + X2 + X4")
sae_X2_tab_m14 = do.call(rbind, calc_list_X2_m14) %>% 
  mutate(model = "X2 + X3 + X4")

model_sae_X2_tab = rbind(sae_X2_tab_m6, sae_X2_tab_m11, sae_X2_tab_m13,
                         sae_X2_tab_m13a, sae_X2_tab_m15, sae_X2_tab_m15a,
                         sae_X2_tab_m12, sae_X2_tab_m14) %>% 
  rename(X2saeEst_X5 = `5%`,
         X2saeEst_X50 = `50%`,
         X2saeEst_X95 = `95%`) %>% 
  mutate(intervalScr_X2sae = (X2saeEst_X95 - X2saeEst_X5) + 
           ((2 / alph * (X2saeEst_X5 - mean_yObs_sae)) * ifelse(mean_yObs_sae < X2saeEst_X5, 1, 0)) + 
           ((2 / alph * (mean_yObs_sae -  X2saeEst_X95)) * ifelse(mean_yObs_sae > X2saeEst_X95, 1, 0))) %>% 
  left_join(., elpd_X2sae_sum_tab, by=c('model', 'iteration', 'X2')) %>% 
  left_join(., elpd_X2sae_mean_tab, by=c('model', 'iteration', 'X2')) %>% 
  left_join(., elpd_X2sae_se_tab, by=c('model', 'iteration', 'X2'))  %>% 
  left_join(., tabX2_all, by=c('model', 'iteration', 'X2'))  %>% 
  left_join(., wtdTabX2_all, by=c('model', 'iteration', 'X2'))

# combining all of the iterations - X3 ------------------------------------
sae_X3_tab_m6 = do.call(rbind, calc_list_X3_m6) %>% 
  mutate(model = "X1 + X3")
sae_X3_tab_m11 = do.call(rbind, calc_list_X3_m11) %>% 
  mutate(model = "X1 + X2 + X3")
sae_X3_tab_m13 = do.call(rbind, calc_list_X3_m13) %>% 
  mutate(model = "X1 + X3 + X4")
sae_X3_tab_m13a = do.call(rbind, calc_list_X3_m13a) %>% 
  mutate(model = "*X1 + X3 + X4")
sae_X3_tab_m15 = do.call(rbind, calc_list_X3_m15) %>% 
  mutate(model = "X1 + X2 + X3 + X4")
sae_X3_tab_m15a = do.call(rbind, calc_list_X3_m15a) %>% 
  mutate(model = "*X1 + X2 + X3 + X4")
sae_X3_tab_m12 = do.call(rbind, calc_list_X3_m12) %>% 
  mutate(model = "X1 + X2 + X4")
sae_X3_tab_m14 = do.call(rbind, calc_list_X3_m14) %>% 
  mutate(model = "X2 + X3 + X4")

model_sae_X3_tab = rbind(sae_X3_tab_m6, sae_X3_tab_m11, sae_X3_tab_m13,
                         sae_X3_tab_m13a, sae_X3_tab_m15, sae_X3_tab_m15a,
                         sae_X3_tab_m12, sae_X3_tab_m14) %>% 
  rename(X3saeEst_X5 = `5%`,
         X3saeEst_X50 = `50%`,
         X3saeEst_X95 = `95%`) %>% 
  mutate(intervalScr_X3sae = (X3saeEst_X95 - X3saeEst_X5) + 
           ((2 / alph * (X3saeEst_X5 - mean_yObs_sae)) * ifelse(mean_yObs_sae < X3saeEst_X5, 1, 0)) + 
           ((2 / alph * (mean_yObs_sae -  X3saeEst_X95)) * ifelse(mean_yObs_sae > X3saeEst_X95, 1, 0))) %>% 
  left_join(., elpd_X3sae_sum_tab, by=c('model', 'iteration', 'X3')) %>% 
  left_join(., elpd_X3sae_mean_tab, by=c('model', 'iteration', 'X3')) %>% 
  left_join(., elpd_X3sae_se_tab, by=c('model', 'iteration', 'X3'))  %>% 
  left_join(., tabX3_all, by=c('model', 'iteration', 'X3'))  %>% 
  left_join(., wtdTabX3_all, by=c('model', 'iteration', 'X3'))


# combining all of the iterations - X4 ------------------------------------
sae_X4_tab_m6 = do.call(rbind, calc_list_X4_m6) %>% 
  mutate(model = "X1 + X3")
sae_X4_tab_m11 = do.call(rbind, calc_list_X4_m11) %>% 
  mutate(model = "X1 + X2 + X3")
sae_X4_tab_m13 = do.call(rbind, calc_list_X4_m13) %>% 
  mutate(model = "X1 + X3 + X4")
sae_X4_tab_m13a = do.call(rbind, calc_list_X4_m13a) %>% 
  mutate(model = "*X1 + X3 + X4")
sae_X4_tab_m15 = do.call(rbind, calc_list_X4_m15) %>% 
  mutate(model = "X1 + X2 + X3 + X4")
sae_X4_tab_m15a = do.call(rbind, calc_list_X4_m15a) %>% 
  mutate(model = "*X1 + X2 + X3 + X4")
sae_X4_tab_m12 = do.call(rbind, calc_list_X4_m12) %>% 
  mutate(model = "X1 + X2 + X4")
sae_X4_tab_m14 = do.call(rbind, calc_list_X4_m14) %>% 
  mutate(model = "X2 + X3 + X4")

model_sae_X4_tab = rbind(sae_X4_tab_m6, sae_X4_tab_m11, sae_X4_tab_m13,
                         sae_X4_tab_m13a, sae_X4_tab_m15, sae_X4_tab_m15a,
                         sae_X4_tab_m12, sae_X4_tab_m14) %>% 
  rename(X4saeEst_X5 = `5%`,
         X4saeEst_X50 = `50%`,
         X4saeEst_X95 = `95%`) %>% 
  mutate(intervalScr_X4sae = (X4saeEst_X95 - X4saeEst_X5) + 
           ((2 / alph * (X4saeEst_X5 - mean_yObs_sae)) * ifelse(mean_yObs_sae < X4saeEst_X5, 1, 0)) + 
           ((2 / alph * (mean_yObs_sae -  X4saeEst_X95)) * ifelse(mean_yObs_sae > X4saeEst_X95, 1, 0))) %>% 
  left_join(., elpd_X4sae_sum_tab, by=c('model', 'iteration', 'X4')) %>% 
  left_join(., elpd_X4sae_mean_tab, by=c('model', 'iteration', 'X4')) %>% 
  left_join(., elpd_X4sae_se_tab, by=c('model', 'iteration', 'X4')) %>% 
  left_join(., tabX4_all, by=c('model', 'iteration', 'X4')) %>% 
  left_join(., wtdTabX4_all, by=c('model', 'iteration', 'X4'))

