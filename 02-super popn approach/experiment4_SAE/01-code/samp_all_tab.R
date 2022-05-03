## table for individual data set 
library(tidyverse)

## data file
load(here::here('02-super popn approach/experiment4_SAE/03-data/loo_sae_fx3_all.RData'))

# individual bias ---------------------------------------------------------
alph = 0.1
sampest_list = list()
for(i in iter){
  temp_06 = sampest_tab_all[[i]] %>% 
    filter(model == 'model06') %>% 
    mutate(elpd_ind = loo_06_list[[ite]]$pointwise[,1])
  temp_11 = sampest_tab_all[[i]] %>% 
    filter(model == 'model11') %>% 
    mutate(elpd_ind = loo_11_list[[ite]]$pointwise[,1])
  temp_13 = sampest_tab_all[[i]] %>% 
    filter(model == 'model13') %>% 
    mutate(elpd_ind = loo_13_list[[ite]]$pointwise[,1])
  temp_13a = sampest_tab_all[[i]] %>% 
    filter(model == 'model13a') %>% 
    mutate(elpd_ind = loo_13a_list[[ite]]$pointwise[,1])
  temp_15 = sampest_tab_all[[i]] %>% 
    filter(model == 'model15') %>% 
    mutate(elpd_ind = loo_15_list[[ite]]$pointwise[,1])
  temp_15a = sampest_tab_all[[i]] %>% 
    filter(model == 'model15a') %>% 
    mutate(elpd_ind = loo_15a_list[[ite]]$pointwise[,1])
  temp_12 = sampest_tab_all[[i]] %>% 
    filter(model == 'model12') %>% 
    mutate(elpd_ind = loo_12_list[[ite]]$pointwise[,1])
  temp_14 = sampest_tab_all[[i]] %>% 
    filter(model == 'model14') %>% 
    mutate(elpd_ind = loo_14_list[[ite]]$pointwise[,1])
  
  temp_tab = rbind(temp_06, temp_11, temp_13,
                   temp_13a, temp_15, temp_15a,
                   temp_12, temp_14)
  
  sampest_list[[i]] = mutate(temp_tab,
                             ci_width = sampestX95 - sampestX5,
                             bias_ind_X50 =  sampestX50 - prob_truth,
                             bias_ind_X5 =  sampestX5 - prob_truth,
                             bias_ind_X95 =  sampestX95 - prob_truth, 
                             bias_ind_X50_abs =  abs(sampestX50 - prob_truth),
                             bias_ind_X5_abs =  abs(sampestX5 - prob_truth),
                             bias_ind_X95_abs =  abs(sampestX95 - prob_truth), 
                             bias_ind_width = bias_ind_X95 - bias_ind_X5, 
                             intervalScr_ind = (sampestX95 - sampestX5) + 
                               ((2 / alph * (sampestX5 - prob_truth)) * ifelse(prob_truth < sampestX5, 1, 0)) + 
                               ((2 / alph * (prob_truth - sampestX95)) * ifelse(prob_truth > sampestX95, 1, 0)),
                             iter = i) %>% 
    rename(coverage_ind = coverage,
           iteration = iter)
}

indv_all_tab = do.call(rbind, sampest_list[iter]) %>% 
  mutate(model = plyr::revalue(model, c('model06' = 'X1 + X3',
                                        'model11' = 'X1 + X2 + X3',  
                                        'model13' = 'X1 + X3 + X4', 
                                        'model13a' = '*X1 + X3 + X4', 
                                        'model15' = 'X1 + X2 + X3 + X4',
                                        'model15a' = '*X1 + X2 + X3 + X4',
                                        'model12' = 'X1 + X2 + X4',
                                        'model14' = 'X2 + X3 + X4')))

indv_summ_tab = indv_all_tab %>% 
  group_by(iteration, model) %>% 
  summarise(mean_ci_width = mean(ci_width),
            mean_bias_ind = mean(bias_ind_X50),
            sd_bias_ind = sd(bias_ind_X50), 
            mean_bias_ind_abs = mean(bias_ind_X50_abs),
            sd_bias_ind_abs = sd(bias_ind_X50_abs),
            mean_cover_ind = mean(coverage_ind),
            mean_bias_ind_width = mean(bias_ind_width),
            sd_bias_ind_width = sd(bias_ind_width),
            mean_intervalScr_ind = mean(intervalScr_ind),
            sd_intervalScr_ind = sd(intervalScr_ind),
            sum_elpd_ind = sum(elpd_ind))

# unweighted loo ----------------------------------------------------------
elpd_06_tab = elpd_06_mat[iter,] %>% 
  as_tibble() %>% 
  mutate(model = 'X1 + X3') %>% 
  rename('elpd_X50' = V1, 'elpd_SE' = V2)

elpd_11_tab = elpd_11_mat[iter,] %>% 
  as_tibble() %>% 
  mutate(model = 'X1 + X2 + X3') %>% 
  rename('elpd_X50' = V1, 'elpd_SE' = V2)

elpd_13_tab = elpd_13_mat[iter,] %>% 
  as_tibble() %>% 
  mutate(model = 'X1 + X3 + X4') %>% 
  rename('elpd_X50' = V1, 'elpd_SE' = V2)

elpd_13a_tab = elpd_13a_mat[iter,] %>% 
  as_tibble() %>% 
  mutate(model = '*X1 + X3 + X4') %>% 
  rename('elpd_X50' = V1, 'elpd_SE' = V2)

elpd_15_tab = elpd_15_mat[iter,] %>% 
  as_tibble() %>% 
  mutate(model = 'X1 + X2 + X3 + X4') %>% 
  rename('elpd_X50' = V1, 'elpd_SE' = V2)

elpd_15a_tab = elpd_15a_mat[iter,] %>% 
  as_tibble() %>% 
  mutate(model = '*X1 + X2 + X3 + X4') %>% 
  rename('elpd_X50' = V1, 'elpd_SE' = V2)

elpd_12_tab = elpd_12_mat[iter,] %>% 
  as_tibble() %>% 
  mutate(model = 'X1 + X2 + X4') %>% 
  rename('elpd_X50' = V1, 'elpd_SE' = V2)

elpd_14_tab = elpd_14_mat[iter,] %>% 
  as_tibble() %>% 
  mutate(model = 'X2 + X3 + X4') %>% 
  rename('elpd_X50' = V1, 'elpd_SE' = V2)

elpd_all_tab = rbind(elpd_06_tab, elpd_11_tab, elpd_13_tab,
                     elpd_13a_tab, elpd_15_tab, elpd_15a_tab,
                     elpd_12_tab, elpd_14_tab) %>% 
  mutate(elpd_X5 = .$elpd_X50 - (.$elpd_SE*1.64), # calculating upper and lower bound of the elpd values
         elpd_X95 = .$elpd_X50 + (.$elpd_SE*1.64),
         elpd_width = elpd_X95 - elpd_X5,
         iteration = rep(1:length(iter), 8)) %>% 
  arrange(iteration)

indv_summ_tab = left_join(indv_summ_tab, elpd_all_tab, by=c('model', 'iteration')) %>% 
  as_tibble() %>% 
  mutate(model = factor(model)) 

indv_summ_tab$model = forcats::fct_relevel(indv_summ_tab$model, c('X1 + X2 + X3 + X4', '*X1 + X2 + X3 + X4', 
                                                                'X1 + X3 + X4', '*X1 + X3 + X4',
                                                                'X1 + X2 + X3', 'X1 + X2 + X4',
                                                                'X2 + X3 + X4', 'X1 + X3'))

