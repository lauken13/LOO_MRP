## postprocessing the saved objects from cluster
library(loo)
library(posterior) # to convert draws_array() objects 
library(dplyr)
library(ggplot2)

load(here::here("02-super popn approach/experiment4_SAE/03-data/loo_sae_fx3_all.RData"))

# SAE calculations --------------------------------------------------------
X1_group_mean_list =
  X2_group_mean_list =
  X3_group_mean_list = 
  X4_group_mean_list = list()

elpd_X1sae_list = 
  elpd_X2sae_list = 
  elpd_X3sae_list = 
  elpd_X4sae_list = list()

for (ite in iter){

# y_prob_mean by SAE ------------------------------------------------------------
  X1_group_mean_list[[ite]] =  popn_data_list[[ite]] %>% 
    group_by(X1) %>% 
    summarise(mean_y_prob = mean(y_prob)) %>% 
    mutate(iteration = ite)
  
  X2_group_mean_list[[ite]] =  popn_data_list[[ite]] %>% 
    group_by(X2) %>% 
    summarise(mean_y_prob = mean(y_prob)) %>% 
    mutate(iteration = ite)
  
  X3_group_mean_list[[ite]] =  popn_data_list[[ite]] %>% 
    group_by(X3) %>% 
    summarise(mean_y_prob = mean(y_prob)) %>% 
    mutate(iteration = ite)
  
  X4_group_mean_list[[ite]] =  popn_data_list[[ite]] %>% 
    group_by(X4) %>% 
    summarise(mean_y_prob = mean(y_prob)) %>% 
    mutate(iteration = ite)
  

# LOO by SAE --------------------------------------------------------------
  # calculating the sum and mean by X1 groups
  elpd_X1sae_list[[ite]] =  samp_data_list[[ite]] %>% 
    group_by(X1) %>% 
    summarise(sum_elpd_06 = sum(elpd_loo_06),
              sum_elpd_11 = sum(elpd_loo_11),
              sum_elpd_13 = sum(elpd_loo_13),
              sum_elpd_13a = sum(elpd_loo_13a),
              sum_elpd_15a = sum(elpd_loo_15a), 
              sum_elpd_15 = sum(elpd_loo_15),
              sum_elpd_12 = sum(elpd_loo_12), 
              sum_elpd_14 = sum(elpd_loo_14),
              mean_elpd_06 = mean(elpd_loo_06),
              mean_elpd_11 = mean(elpd_loo_11),
              mean_elpd_13 = mean(elpd_loo_13),
              mean_elpd_13a = mean(elpd_loo_13a),
              mean_elpd_15a = mean(elpd_loo_15a), 
              mean_elpd_15 = mean(elpd_loo_15),
              mean_elpd_12 = mean(elpd_loo_12), 
              mean_elpd_14 = mean(elpd_loo_14)) %>% 
    mutate(iteration = ite)
  
  # X2
  elpd_X2sae_list[[ite]] =  samp_data_list[[ite]] %>% 
    group_by(X2) %>% 
    summarise(sum_elpd_06 = sum(elpd_loo_06),
              sum_elpd_11 = sum(elpd_loo_11),
              sum_elpd_13 = sum(elpd_loo_13),
              sum_elpd_13a = sum(elpd_loo_13a),
              sum_elpd_15a = sum(elpd_loo_15a), 
              sum_elpd_15 = sum(elpd_loo_15),
              sum_elpd_12 = sum(elpd_loo_12), 
              sum_elpd_14 = sum(elpd_loo_14),
              mean_elpd_06 = mean(elpd_loo_06),
              mean_elpd_11 = mean(elpd_loo_11),
              mean_elpd_13 = mean(elpd_loo_13),
              mean_elpd_13a = mean(elpd_loo_13a),
              mean_elpd_15a = mean(elpd_loo_15a), 
              mean_elpd_15 = mean(elpd_loo_15),
              mean_elpd_12 = mean(elpd_loo_12), 
              mean_elpd_14 = mean(elpd_loo_14)) %>% 
    mutate(iteration = ite)
  
  # X3
  elpd_X3sae_list[[ite]] =  samp_data_list[[ite]] %>% 
    group_by(X3) %>% 
    summarise(sum_elpd_06 = sum(elpd_loo_06),
              sum_elpd_11 = sum(elpd_loo_11),
              sum_elpd_13 = sum(elpd_loo_13),
              sum_elpd_13a = sum(elpd_loo_13a),
              sum_elpd_15a = sum(elpd_loo_15a), 
              sum_elpd_15 = sum(elpd_loo_15),
              sum_elpd_12 = sum(elpd_loo_12), 
              sum_elpd_14 = sum(elpd_loo_14),
              mean_elpd_06 = mean(elpd_loo_06),
              mean_elpd_11 = mean(elpd_loo_11),
              mean_elpd_13 = mean(elpd_loo_13),
              mean_elpd_13a = mean(elpd_loo_13a),
              mean_elpd_15a = mean(elpd_loo_15a), 
              mean_elpd_15 = mean(elpd_loo_15),
              mean_elpd_12 = mean(elpd_loo_12), 
              mean_elpd_14 = mean(elpd_loo_14)) %>% 
    mutate(iteration = ite)
  
  # X4 
  elpd_X4sae_list[[ite]] =  samp_data_list[[ite]] %>% 
    group_by(X4) %>% 
    summarise(sum_elpd_06 = sum(elpd_loo_06),
              sum_elpd_11 = sum(elpd_loo_11),
              sum_elpd_13 = sum(elpd_loo_13),
              sum_elpd_13a = sum(elpd_loo_13a),
              sum_elpd_15a = sum(elpd_loo_15a), 
              sum_elpd_15 = sum(elpd_loo_15),
              sum_elpd_12 = sum(elpd_loo_12), 
              sum_elpd_14 = sum(elpd_loo_14),
              mean_elpd_06 = mean(elpd_loo_06),
              mean_elpd_11 = mean(elpd_loo_11),
              mean_elpd_13 = mean(elpd_loo_13),
              mean_elpd_13a = mean(elpd_loo_13a),
              mean_elpd_15a = mean(elpd_loo_15a), 
              mean_elpd_15 = mean(elpd_loo_15),
              mean_elpd_12 = mean(elpd_loo_12), 
              mean_elpd_14 = mean(elpd_loo_14)) %>% 
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

# extracting every 1st/2nd ... 6th list of the main list
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
    mutate(bias_X50_saeX1 = (as.numeric(`50%`) - as.numeric(mean_y_prob)),
           bias_X5_saeX1 = (as.numeric(`5%`) - as.numeric(mean_y_prob)),
           bias_X95_saeX1 = (as.numeric(`95%`) - as.numeric(mean_y_prob)),
           coverage_saeX1 = ifelse(mean_y_prob >= `5%` & mean_y_prob <= `95%`, 1, 0))
  
  calc_tab2_X1_m11 = calc_tab_X1_m11 %>% 
    mutate(bias_X50_saeX1 = (as.numeric(`50%`) - as.numeric(mean_y_prob)),
           bias_X5_saeX1 = (as.numeric(`5%`) - as.numeric(mean_y_prob)),
           bias_X95_saeX1 = (as.numeric(`95%`) - as.numeric(mean_y_prob)),
           coverage_saeX1 = ifelse(mean_y_prob >= `5%` & mean_y_prob <= `95%`, 1, 0))
  
  calc_tab2_X1_m13 = calc_tab_X1_m13 %>% 
    mutate(bias_X50_saeX1 = (as.numeric(`50%`) - as.numeric(mean_y_prob)),
           bias_X5_saeX1 = (as.numeric(`5%`) - as.numeric(mean_y_prob)),
           bias_X95_saeX1 = (as.numeric(`95%`) - as.numeric(mean_y_prob)),
           coverage_saeX1 = ifelse(mean_y_prob >= `5%` & mean_y_prob <= `95%`, 1, 0))
  
  calc_tab2_X1_m13a = calc_tab_X1_m13a %>% 
    mutate(bias_X50_saeX1 = (as.numeric(`50%`) - as.numeric(mean_y_prob)),
           bias_X5_saeX1 = (as.numeric(`5%`) - as.numeric(mean_y_prob)),
           bias_X95_saeX1 = (as.numeric(`95%`) - as.numeric(mean_y_prob)),
           coverage_saeX1 = ifelse(mean_y_prob >= `5%` & mean_y_prob <= `95%`, 1, 0))
  
  calc_tab2_X1_m15 = calc_tab_X1_m15 %>% 
    mutate(bias_X50_saeX1 = (as.numeric(`50%`) - as.numeric(mean_y_prob)),
           bias_X5_saeX1 = (as.numeric(`5%`) - as.numeric(mean_y_prob)),
           bias_X95_saeX1 = (as.numeric(`95%`) - as.numeric(mean_y_prob)),
           coverage_saeX1 = ifelse(mean_y_prob >= `5%` & mean_y_prob <= `95%`, 1, 0))
  
  calc_tab2_X1_m15a = calc_tab_X1_m15a %>% 
    mutate(bias_X50_saeX1 = (as.numeric(`50%`) - as.numeric(mean_y_prob)),
           bias_X5_saeX1 = (as.numeric(`5%`) - as.numeric(mean_y_prob)),
           bias_X95_saeX1 = (as.numeric(`95%`) - as.numeric(mean_y_prob)),
           coverage_saeX1 = ifelse(mean_y_prob >= `5%` & mean_y_prob <= `95%`, 1, 0))
  
  calc_tab2_X1_m12 = calc_tab_X1_m12 %>% 
    mutate(bias_X50_saeX1 = (as.numeric(`50%`) - as.numeric(mean_y_prob)),
           bias_X5_saeX1 = (as.numeric(`5%`) - as.numeric(mean_y_prob)),
           bias_X95_saeX1 = (as.numeric(`95%`) - as.numeric(mean_y_prob)),
           coverage_saeX1 = ifelse(mean_y_prob >= `5%` & mean_y_prob <= `95%`, 1, 0))
  
  calc_tab2_X1_m14 = calc_tab_X1_m14 %>% 
    mutate(bias_X50_saeX1 = (as.numeric(`50%`) - as.numeric(mean_y_prob)),
           bias_X5_saeX1 = (as.numeric(`5%`) - as.numeric(mean_y_prob)),
           bias_X95_saeX1 = (as.numeric(`95%`) - as.numeric(mean_y_prob)),
           coverage_saeX1 = ifelse(mean_y_prob >= `5%` & mean_y_prob <= `95%`, 1, 0))
  
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
    mutate(bias_X50_saeX2 = (as.numeric(`50%`) - as.numeric(mean_y_prob)),
           bias_X5_saeX2 = (as.numeric(`5%`) - as.numeric(mean_y_prob)),
           bias_X95_saeX2 = (as.numeric(`95%`) - as.numeric(mean_y_prob)),
           coverage_saeX2 = ifelse(mean_y_prob >= `5%` & mean_y_prob <= `95%`, 1, 0))
  
  calc_tab2_X2_m11 = calc_tab_X2_m11 %>% 
    mutate(bias_X50_saeX2 = (as.numeric(`50%`) - as.numeric(mean_y_prob)),
           bias_X5_saeX2 = (as.numeric(`5%`) - as.numeric(mean_y_prob)),
           bias_X95_saeX2 = (as.numeric(`95%`) - as.numeric(mean_y_prob)),
           coverage_saeX2 = ifelse(mean_y_prob >= `5%` & mean_y_prob <= `95%`, 1, 0))
  
  calc_tab2_X2_m13 = calc_tab_X2_m13 %>% 
    mutate(bias_X50_saeX2 = (as.numeric(`50%`) - as.numeric(mean_y_prob)),
           bias_X5_saeX2 = (as.numeric(`5%`) - as.numeric(mean_y_prob)),
           bias_X95_saeX2 = (as.numeric(`95%`) - as.numeric(mean_y_prob)),
           coverage_saeX2 = ifelse(mean_y_prob >= `5%` & mean_y_prob <= `95%`, 1, 0))
  
  calc_tab2_X2_m13a = calc_tab_X2_m13a %>% 
    mutate(bias_X50_saeX2 = (as.numeric(`50%`) - as.numeric(mean_y_prob)),
           bias_X5_saeX2 = (as.numeric(`5%`) - as.numeric(mean_y_prob)),
           bias_X95_saeX2 = (as.numeric(`95%`) - as.numeric(mean_y_prob)),
           coverage_saeX2 = ifelse(mean_y_prob >= `5%` & mean_y_prob <= `95%`, 1, 0))
  
  calc_tab2_X2_m15 = calc_tab_X2_m15 %>% 
    mutate(bias_X50_saeX2 = (as.numeric(`50%`) - as.numeric(mean_y_prob)),
           bias_X5_saeX2 = (as.numeric(`5%`) - as.numeric(mean_y_prob)),
           bias_X95_saeX2 = (as.numeric(`95%`) - as.numeric(mean_y_prob)),
           coverage_saeX2 = ifelse(mean_y_prob >= `5%` & mean_y_prob <= `95%`, 1, 0))
  
  calc_tab2_X2_m15a= calc_tab_X2_m15a %>% 
    mutate(bias_X50_saeX2 = (as.numeric(`50%`) - as.numeric(mean_y_prob)),
           bias_X5_saeX2 = (as.numeric(`5%`) - as.numeric(mean_y_prob)),
           bias_X95_saeX2 = (as.numeric(`95%`) - as.numeric(mean_y_prob)),
           coverage_saeX2 = ifelse(mean_y_prob >= `5%` & mean_y_prob <= `95%`, 1, 0))
  
  calc_tab2_X2_m12 = calc_tab_X2_m12 %>% 
    mutate(bias_X50_saeX2 = (as.numeric(`50%`) - as.numeric(mean_y_prob)),
           bias_X5_saeX2 = (as.numeric(`5%`) - as.numeric(mean_y_prob)),
           bias_X95_saeX2 = (as.numeric(`95%`) - as.numeric(mean_y_prob)),
           coverage_saeX2 = ifelse(mean_y_prob >= `5%` & mean_y_prob <= `95%`, 1, 0))
  
  calc_tab2_X2_m14 = calc_tab_X2_m14 %>% 
    mutate(bias_X50_saeX2 = (as.numeric(`50%`) - as.numeric(mean_y_prob)),
           bias_X5_saeX2 = (as.numeric(`5%`) - as.numeric(mean_y_prob)),
           bias_X95_saeX2 = (as.numeric(`95%`) - as.numeric(mean_y_prob)),
           coverage_saeX2 = ifelse(mean_y_prob >= `5%` & mean_y_prob <= `95%`, 1, 0))
  
  
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
    mutate(bias_X50_saeX3 = (as.numeric(`50%`) - as.numeric(mean_y_prob)),
           bias_X5_saeX3 = (as.numeric(`5%`) - as.numeric(mean_y_prob)),
           bias_X95_saeX3 = (as.numeric(`95%`) - as.numeric(mean_y_prob)),
           coverage_saeX3 = ifelse(mean_y_prob >= `5%` & mean_y_prob <= `95%`, 1, 0))
  
  calc_tab2_X3_m11 = calc_tab_X3_m11 %>% 
    mutate(bias_X50_saeX3 = (as.numeric(`50%`) - as.numeric(mean_y_prob)),
           bias_X5_saeX3 = (as.numeric(`5%`) - as.numeric(mean_y_prob)),
           bias_X95_saeX3 = (as.numeric(`95%`) - as.numeric(mean_y_prob)),
           coverage_saeX3 = ifelse(mean_y_prob >= `5%` & mean_y_prob <= `95%`, 1, 0))
  
  calc_tab2_X3_m13 = calc_tab_X3_m13 %>% 
    mutate(bias_X50_saeX3 = (as.numeric(`50%`) - as.numeric(mean_y_prob)),
           bias_X5_saeX3 = (as.numeric(`5%`) - as.numeric(mean_y_prob)),
           bias_X95_saeX3 = (as.numeric(`95%`) - as.numeric(mean_y_prob)),
           coverage_saeX3 = ifelse(mean_y_prob >= `5%` & mean_y_prob <= `95%`, 1, 0))
  
  calc_tab2_X3_m13a = calc_tab_X3_m13a %>% 
    mutate(bias_X50_saeX3 = (as.numeric(`50%`) - as.numeric(mean_y_prob)),
           bias_X5_saeX3 = (as.numeric(`5%`) - as.numeric(mean_y_prob)),
           bias_X95_saeX3 = (as.numeric(`95%`) - as.numeric(mean_y_prob)),
           coverage_saeX3 = ifelse(mean_y_prob >= `5%` & mean_y_prob <= `95%`, 1, 0))
  
  calc_tab2_X3_m15 = calc_tab_X3_m15 %>% 
    mutate(bias_X50_saeX3 = (as.numeric(`50%`) - as.numeric(mean_y_prob)),
           bias_X5_saeX3 = (as.numeric(`5%`) - as.numeric(mean_y_prob)),
           bias_X95_saeX3 = (as.numeric(`95%`) - as.numeric(mean_y_prob)),
           coverage_saeX3 = ifelse(mean_y_prob >= `5%` & mean_y_prob <= `95%`, 1, 0))
  
  calc_tab2_X3_m15a= calc_tab_X3_m15a %>% 
    mutate(bias_X50_saeX3 = (as.numeric(`50%`) - as.numeric(mean_y_prob)),
           bias_X5_saeX3 = (as.numeric(`5%`) - as.numeric(mean_y_prob)),
           bias_X95_saeX3 = (as.numeric(`95%`) - as.numeric(mean_y_prob)),
           coverage_saeX3 = ifelse(mean_y_prob >= `5%` & mean_y_prob <= `95%`, 1, 0))
  
  calc_tab2_X3_m12 = calc_tab_X3_m12 %>% 
    mutate(bias_X50_saeX3 = (as.numeric(`50%`) - as.numeric(mean_y_prob)),
           bias_X5_saeX3 = (as.numeric(`5%`) - as.numeric(mean_y_prob)),
           bias_X95_saeX3 = (as.numeric(`95%`) - as.numeric(mean_y_prob)),
           coverage_saeX3 = ifelse(mean_y_prob >= `5%` & mean_y_prob <= `95%`, 1, 0))
  
  calc_tab2_X3_m14 = calc_tab_X3_m14 %>% 
    mutate(bias_X50_saeX3 = (as.numeric(`50%`) - as.numeric(mean_y_prob)),
           bias_X5_saeX3 = (as.numeric(`5%`) - as.numeric(mean_y_prob)),
           bias_X95_saeX3 = (as.numeric(`95%`) - as.numeric(mean_y_prob)),
           coverage_saeX3 = ifelse(mean_y_prob >= `5%` & mean_y_prob <= `95%`, 1, 0))
  
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
    mutate(bias_X50_saeX4 = (as.numeric(`50%`) - as.numeric(mean_y_prob)),
           bias_X5_saeX4 = (as.numeric(`5%`) - as.numeric(mean_y_prob)),
           bias_X95_saeX4 = (as.numeric(`95%`) - as.numeric(mean_y_prob)),
           coverage_saeX4 = ifelse(mean_y_prob >= `5%` & mean_y_prob <= `95%`, 1, 0))
  
  calc_tab2_X4_m11 = calc_tab_X4_m11 %>% 
    mutate(bias_X50_saeX4 = (as.numeric(`50%`) - as.numeric(mean_y_prob)),
           bias_X5_saeX4 = (as.numeric(`5%`) - as.numeric(mean_y_prob)),
           bias_X95_saeX4 = (as.numeric(`95%`) - as.numeric(mean_y_prob)),
           coverage_saeX4 = ifelse(mean_y_prob >= `5%` & mean_y_prob <= `95%`, 1, 0))
  
  calc_tab2_X4_m13 = calc_tab_X4_m13 %>% 
    mutate(bias_X50_saeX4 = (as.numeric(`50%`) - as.numeric(mean_y_prob)),
           bias_X5_saeX4 = (as.numeric(`5%`) - as.numeric(mean_y_prob)),
           bias_X95_saeX4 = (as.numeric(`95%`) - as.numeric(mean_y_prob)),
           coverage_saeX4 = ifelse(mean_y_prob >= `5%` & mean_y_prob <= `95%`, 1, 0))
  
  calc_tab2_X4_m13a = calc_tab_X4_m13a %>% 
    mutate(bias_X50_saeX4 = (as.numeric(`50%`) - as.numeric(mean_y_prob)),
           bias_X5_saeX4 = (as.numeric(`5%`) - as.numeric(mean_y_prob)),
           bias_X95_saeX4 = (as.numeric(`95%`) - as.numeric(mean_y_prob)),
           coverage_saeX4 = ifelse(mean_y_prob >= `5%` & mean_y_prob <= `95%`, 1, 0))
  
  calc_tab2_X4_m15 = calc_tab_X4_m15 %>% 
    mutate(bias_X50_saeX4 = (as.numeric(`50%`) - as.numeric(mean_y_prob)),
           bias_X5_saeX4 = (as.numeric(`5%`) - as.numeric(mean_y_prob)),
           bias_X95_saeX4 = (as.numeric(`95%`) - as.numeric(mean_y_prob)),
           coverage_saeX4 = ifelse(mean_y_prob >= `5%` & mean_y_prob <= `95%`, 1, 0))
  
  calc_tab2_X4_m15a = calc_tab_X4_m15a %>% 
    mutate(bias_X50_saeX4 = (as.numeric(`50%`) - as.numeric(mean_y_prob)),
           bias_X5_saeX4 = (as.numeric(`5%`) - as.numeric(mean_y_prob)),
           bias_X95_saeX4 = (as.numeric(`95%`) - as.numeric(mean_y_prob)),
           coverage_saeX4 = ifelse(mean_y_prob >= `5%` & mean_y_prob <= `95%`, 1, 0))
  
  calc_tab2_X4_m12 = calc_tab_X4_m12 %>% 
    mutate(bias_X50_saeX4 = (as.numeric(`50%`) - as.numeric(mean_y_prob)),
           bias_X5_saeX4 = (as.numeric(`5%`) - as.numeric(mean_y_prob)),
           bias_X95_saeX4 = (as.numeric(`95%`) - as.numeric(mean_y_prob)),
           coverage_saeX4 = ifelse(mean_y_prob >= `5%` & mean_y_prob <= `95%`, 1, 0))
  
  calc_tab2_X4_m14 = calc_tab_X4_m14 %>% 
    mutate(bias_X50_saeX4 = (as.numeric(`50%`) - as.numeric(mean_y_prob)),
           bias_X5_saeX4 = (as.numeric(`5%`) - as.numeric(mean_y_prob)),
           bias_X95_saeX4 = (as.numeric(`95%`) - as.numeric(mean_y_prob)),
           coverage_saeX4 = ifelse(mean_y_prob >= `5%` & mean_y_prob <= `95%`, 1, 0))
  
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

model_sae_X1_tab = rbind(sae_X1_tab_m6, sae_X1_tab_m11, sae_X1_tab_m13,
                         sae_X1_tab_m13a, sae_X1_tab_m15, sae_X1_tab_m15a,
                         sae_X1_tab_m12, sae_X1_tab_m14) %>% 
  rename(X1sae_pred_X5 = `5%`,
         X1sae_pred_X50 = `50%`,
         X1sae_pred_X95 = `95%`) %>% 
  mutate(intervalScr_sae = (sampestX95 - sampestX5) + 
           ((2 / alph * (sampestX5 - prob_truth)) * ifelse(prob_truth < sampestX5, 1, 0)) + 
           ((2 / alph * (prob_truth - sampestX95)) * ifelse(prob_truth > sampestX95, 1, 0)))

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
  rename(X2sae_pred_X5 = `5%`,
         X2sae_pred_X50 = `50%`,
         X2sae_pred_X95 = `95%`)

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
  rename(X3sae_pred_X5 = `5%`,
         X3sae_pred_X50 = `50%`,
         X3sae_pred_X95 = `95%`)

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
  rename(X4sae_pred_X5 = `5%`,
         X4sae_pred_X50 = `50%`,
         X4sae_pred_X95 = `95%`)


# ## looking at specific models only
# model_sel_pn_tab = model_all_pn_tab %>% 
#   filter(model == 'X1 + X2 + X3 + X4' | model == 'X1 + X2 + X3' |
#            model == 'X1 + X3 + X4' | model == 'X1 + X3') %>% 
#   mutate(model = factor(model)) 
# model_sel_pn_tab$model = forcats::fct_relevel(model_sel_pn_tab$model, c('X1 + X2 + X3 + X4',
#                                                                         'X1 + X3 + X4',                                                                    'X1 + X2 + X3',
#                                                                         'X1 + X3'))
# ## plot diff in mean
# xloc4 = 0.45
# (p3 = ggplot(model_sel_pn_tab, aes(x = abs(bias_X50_saeX1), y = X1, colour = model)) +
#     geom_vline(aes(xintercept = 0)) +
#     geom_point(position = position_dodge(width = 0.3), alpha=0.7) +
#     theme(legend.position = c(0.85,0.8)) +
#     scale_colour_manual(values = c("#1C73B1FF", 
#                                    "#FB964EFF",
#                                    "#09622AFF",
#                                    "#879195FF")) + 
#     stat_summary(aes(group=model), width=0.7, size=0.4,
#                  position = position_dodge(width = .5),
#                  fun=mean, geom="crossbar", colour=rep(c("#1C73B1FF",
#                                                          "#FB964EFF",
#                                                          "#09622AFF",
#                                                          "#879195FF"), each = 5), alpha=0.8) + #drawing the mean line
#     labs(title="Difference in X1-levels estimate and truth",
#          y = 'Levels of X1', x = 'Absolute bias' ))
# 
# ggsave(here::here("02-super popn approach/experiment4_SAE/02-results/bias_sae_X1_fx3.png"), p3, width=6, height=7.5, units="in", device="png")
# 
