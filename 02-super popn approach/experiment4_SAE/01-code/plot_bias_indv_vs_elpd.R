## individual bias
library(tidyverse)

setwd("~/GitHub/LOO_MRP/02-super popn approach/experiment4_SAE/03-data")

load('loo_sae_fx3.Rbin')


# individual biases -------------------------------------------------------
sampest_list = sampest_tab_all %>% 
  lapply(., function(x)mutate(x, mean_pe_ind =  sampestX50 - prob_truth,
                              low_pe_ind =  sampestX5 - prob_truth,
                              upp_pe_ind =  sampestX95 - prob_truth, 
                              range_pe_ind = upp_pe_ind - low_pe_ind))
  
# iter = c(1:16,19:34,36:54,56:71,73:91,93:100)

for(i in iter){
  sampest_list[[i]]$iter = i
}

sampest_all_tab = do.call(rbind, sampest_list[iter]) %>% 
  mutate(model = plyr::revalue(model, c('model06' = 'X1 + X3',
                                        'model11' = 'X1 + X2 + X3',  
                                        'model13' = 'X1 + X3 + X4', 
                                        'model13a' = '*X1 + X3 + X4', 
                                        'model15' = 'X1 + X2 + X3 + X4',
                                        'model15a' = '*X1 + X2 + X3 + X4')))

sampest_all_sum = sampest_all_tab %>% 
  group_by(iter, model) %>% 
  summarise(mean_bias = mean(mean_pe_ind))

# unweighted loo ----------------------------------------------------------
elpd_06_tab = elpd_06_mat[iter,] %>% 
  as_tibble() %>% 
  mutate(model = 'X1 + X3') %>% 
  rename('elpd_loo' = V1, 'SE' = V2)

elpd_11_tab = elpd_11_mat[iter,] %>% 
  as_tibble() %>% 
  mutate(model = 'X1 + X2 + X3') %>% 
  rename('elpd_loo' = V1, 'SE' = V2)

elpd_13_tab = elpd_13_mat[iter,] %>% 
  as_tibble() %>% 
  mutate(model = 'X1 + X3 + X4') %>% 
  rename('elpd_loo' = V1, 'SE' = V2)

elpd_13a_tab = elpd_13a_mat[iter,] %>% 
  as_tibble() %>% 
  mutate(model = '*X1 + X3 + X4') %>% 
  rename('elpd_loo' = V1, 'SE' = V2)

elpd_15_tab = elpd_15_mat[iter,] %>% 
  as_tibble() %>% 
  mutate(model = 'X1 + X2 + X3 + X4') %>% 
  rename('elpd_loo' = V1, 'SE' = V2)

elpd_15a_tab = elpd_15a_mat[iter,] %>% 
  as_tibble() %>% 
  mutate(model = '*X1 + X2 + X3 + X4') %>% 
  rename('elpd_loo' = V1, 'SE' = V2)

elpd_all_tab = rbind(elpd_06_tab, elpd_11_tab, elpd_13_tab,
                     elpd_13a_tab, elpd_15_tab, elpd_15a_tab) %>% 
  mutate(low_elpd = .$elpd_loo - (.$SE*1.64), # calculating upper and lower bound of the elpd values
         upp_elpd = .$elpd_loo + (.$SE*1.64),
         range_elpd = upp_elpd - low_elpd,
         iter = rep(1:length(iter), 6)) %>% 
  arrange(iter)

all_tab = left_join(sampest_all_sum, elpd_all_tab, by=c('model', 'iter')) %>% 
  as_tibble() %>% 
  mutate(model = factor(model)) 

all_tab$model = forcats::fct_relevel(all_tab$model, c('X1 + X2 + X3 + X4', '*X1 + X2 + X3 + X4', 
                                                      'X1 + X3 + X4', '*X1 + X3 + X4',
                                                      'X1 + X2 + X3',
                                                      'X1 + X3'))

all_tab1 = all_tab %>% 
  filter(model == 'X1 + X2 + X3 + X4')



