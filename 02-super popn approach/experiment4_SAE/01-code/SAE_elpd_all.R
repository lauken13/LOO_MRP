## elpd_SAE
elpd_X1sae_list = 
  elpd_X2sae_list = 
  elpd_X3sae_list = 
  elpd_X4sae_list = list()


# LOO by SAE --------------------------------------------------------------
# calculating the sum and mean by X1 groups
for (ite in iter){
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
              mean_elpd_14 = mean(elpd_loo_14),
              n_X1sae = n(), 
              se_elpd_06 = sqrt(n_X1sae * var(elpd_loo_06)),
              se_elpd_11 = sqrt(n_X1sae * var(elpd_loo_11)),
              se_elpd_13 = sqrt(n_X1sae * var(elpd_loo_13)),
              se_elpd_15 = sqrt(n_X1sae * var(elpd_loo_15)),
              se_elpd_13a = sqrt(n_X1sae * var(elpd_loo_13a)),
              se_elpd_15a = sqrt(n_X1sae * var(elpd_loo_15a)),
              se_elpd_12 = sqrt(n_X1sae * var(elpd_loo_12)),
              se_elpd_14 = sqrt(n_X1sae * var(elpd_loo_14))) %>% 
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
              mean_elpd_14 = mean(elpd_loo_14),
              n_X2sae = n(), 
              se_elpd_06 = sqrt(n_X2sae * var(elpd_loo_06)),
              se_elpd_11 = sqrt(n_X2sae * var(elpd_loo_11)),
              se_elpd_13 = sqrt(n_X2sae * var(elpd_loo_13)),
              se_elpd_15 = sqrt(n_X2sae * var(elpd_loo_15)),
              se_elpd_13a = sqrt(n_X2sae * var(elpd_loo_13a)),
              se_elpd_15a = sqrt(n_X2sae * var(elpd_loo_15a)),
              se_elpd_12 = sqrt(n_X2sae * var(elpd_loo_12)),
              se_elpd_14 = sqrt(n_X2sae * var(elpd_loo_14))) %>% 
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
              mean_elpd_14 = mean(elpd_loo_14),
              n_X3sae = n(), 
              se_elpd_06 = sqrt(n_X3sae * var(elpd_loo_06)),
              se_elpd_11 = sqrt(n_X3sae * var(elpd_loo_11)),
              se_elpd_13 = sqrt(n_X3sae * var(elpd_loo_13)),
              se_elpd_15 = sqrt(n_X3sae * var(elpd_loo_15)),
              se_elpd_13a = sqrt(n_X3sae * var(elpd_loo_13a)),
              se_elpd_15a = sqrt(n_X3sae * var(elpd_loo_15a)),
              se_elpd_12 = sqrt(n_X3sae * var(elpd_loo_12)),
              se_elpd_14 = sqrt(n_X3sae * var(elpd_loo_14))) %>% 
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
              mean_elpd_14 = mean(elpd_loo_14),
              n_X4sae = n(), 
              se_elpd_06 = sqrt(n_X4sae * var(elpd_loo_06)),
              se_elpd_11 = sqrt(n_X4sae * var(elpd_loo_11)),
              se_elpd_13 = sqrt(n_X4sae * var(elpd_loo_13)),
              se_elpd_15 = sqrt(n_X4sae * var(elpd_loo_15)),
              se_elpd_13a = sqrt(n_X4sae * var(elpd_loo_13a)),
              se_elpd_15a = sqrt(n_X4sae * var(elpd_loo_15a)),
              se_elpd_12 = sqrt(n_X4sae * var(elpd_loo_12)),
              se_elpd_14 = sqrt(n_X4sae * var(elpd_loo_14))) %>% 
    mutate(iteration = ite)
}

# binding all elpd
elpd_X1sae_sum_tab = do.call(rbind, elpd_X1sae_list) %>% 
  pivot_longer(c(sum_elpd_06:sum_elpd_14), 
               names_to = "model", 
               values_to = "elpd_sum_X1sae") %>% 
  mutate(model = plyr::revalue(model, c('sum_elpd_06' = 'X1 + X3',
                                        'sum_elpd_11' = 'X1 + X2 + X3',  
                                        'sum_elpd_13' = 'X1 + X3 + X4', 
                                        'sum_elpd_15' = 'X1 + X2 + X3 + X4',
                                        'sum_elpd_13a' = '*X1 + X3 + X4', 
                                        'sum_elpd_15a' = '*X1 + X2 + X3 + X4',
                                        'sum_elpd_12' = 'X1 + X2 + X4', 
                                        'sum_elpd_14' = 'X2 + X3 + X4'))) %>% 
  select(., X1,iteration:elpd_sum_X1sae)

elpd_X1sae_mean_tab = do.call(rbind, elpd_X1sae_list) %>% 
  pivot_longer(c(mean_elpd_06:mean_elpd_14), 
               names_to = "model", 
               values_to = "elpd_mean_X1sae") %>% 
  mutate(model = plyr::revalue(model, c('mean_elpd_06' = 'X1 + X3',
                                        'mean_elpd_11' = 'X1 + X2 + X3',  
                                        'mean_elpd_13' = 'X1 + X3 + X4', 
                                        'mean_elpd_15' = 'X1 + X2 + X3 + X4',
                                        'mean_elpd_13a' = '*X1 + X3 + X4', 
                                        'mean_elpd_15a' = '*X1 + X2 + X3 + X4',
                                        'mean_elpd_12' = 'X1 + X2 + X4', 
                                        'mean_elpd_14' = 'X2 + X3 + X4')))  %>% 
  select(., X1, iteration:elpd_mean_X1sae)
elpd_X1sae_se_tab = do.call(rbind, elpd_X1sae_list) %>% 
  pivot_longer(c(se_elpd_06:se_elpd_14), 
               names_to = "model", 
               values_to = "elpd_se_X1sae") %>% 
  select(., X1, iteration:elpd_se_X1sae) %>%
  mutate(model = plyr::revalue(model, c('se_elpd_06' = 'X1 + X3',
                                        'se_elpd_11' = 'X1 + X2 + X3',  
                                        'se_elpd_13' = 'X1 + X3 + X4', 
                                        'se_elpd_15' = 'X1 + X2 + X3 + X4',
                                        'se_elpd_13a' = '*X1 + X3 + X4', 
                                        'se_elpd_15a' = '*X1 + X2 + X3 + X4',
                                        'se_elpd_12' = 'X1 + X2 + X4', 
                                        'se_elpd_14' = 'X2 + X3 + X4'))) 
elpd_X1sae_n_tab = do.call(rbind, elpd_X1sae_list) %>% 
  pivot_longer(n_X1sae, 
               names_to = "model", 
               values_to = "n_X1sae") %>% 
  select(., X1,iteration:n_X1sae) 

elpd_X2sae_sum_tab = do.call(rbind, elpd_X2sae_list) %>% 
  pivot_longer(c(sum_elpd_06:sum_elpd_14), 
               names_to = "model", 
               values_to = "elpd_sum_X2sae") %>% 
  mutate(model = plyr::revalue(model, c('sum_elpd_06' = 'X1 + X3',
                                        'sum_elpd_11' = 'X1 + X2 + X3',  
                                        'sum_elpd_13' = 'X1 + X3 + X4', 
                                        'sum_elpd_15' = 'X1 + X2 + X3 + X4',
                                        'sum_elpd_13a' = '*X1 + X3 + X4', 
                                        'sum_elpd_15a' = '*X1 + X2 + X3 + X4',
                                        'sum_elpd_12' = 'X1 + X2 + X4', 
                                        'sum_elpd_14' = 'X2 + X3 + X4'))) %>% 
  select(., X2, iteration:elpd_sum_X2sae)

elpd_X2sae_mean_tab = do.call(rbind, elpd_X2sae_list) %>% 
  pivot_longer(c(mean_elpd_06:mean_elpd_14), 
               names_to = "model", 
               values_to = "elpd_mean_X2sae") %>% 
  mutate(model = plyr::revalue(model, c('mean_elpd_06' = 'X1 + X3',
                                        'mean_elpd_11' = 'X1 + X2 + X3',  
                                        'mean_elpd_13' = 'X1 + X3 + X4', 
                                        'mean_elpd_15' = 'X1 + X2 + X3 + X4',
                                        'mean_elpd_13a' = '*X1 + X3 + X4', 
                                        'mean_elpd_15a' = '*X1 + X2 + X3 + X4',
                                        'mean_elpd_12' = 'X1 + X2 + X4', 
                                        'mean_elpd_14' = 'X2 + X3 + X4')))  %>% 
  select(., X2,iteration:elpd_mean_X2sae)
elpd_X2sae_se_tab = do.call(rbind, elpd_X2sae_list) %>% 
  pivot_longer(c(se_elpd_06:se_elpd_14), 
               names_to = "model", 
               values_to = "elpd_se_X2sae") %>% 
  select(., X2,iteration:elpd_se_X2sae) %>%
  mutate(model = plyr::revalue(model, c('se_elpd_06' = 'X1 + X3',
                                        'se_elpd_11' = 'X1 + X2 + X3',  
                                        'se_elpd_13' = 'X1 + X3 + X4', 
                                        'se_elpd_15' = 'X1 + X2 + X3 + X4',
                                        'se_elpd_13a' = '*X1 + X3 + X4', 
                                        'se_elpd_15a' = '*X1 + X2 + X3 + X4',
                                        'se_elpd_12' = 'X1 + X2 + X4', 
                                        'se_elpd_14' = 'X2 + X3 + X4'))) 
elpd_X2sae_n_tab = do.call(rbind, elpd_X2sae_list) %>% 
  pivot_longer(n_X2sae, 
               names_to = "model", 
               values_to = "n_X2sae") %>% 
  select(., X2,iteration:n_X2sae) 

elpd_X3sae_sum_tab = do.call(rbind, elpd_X3sae_list) %>% 
  pivot_longer(c(sum_elpd_06:sum_elpd_14), 
               names_to = "model", 
               values_to = "elpd_sum_X3sae") %>% 
  mutate(model = plyr::revalue(model, c('sum_elpd_06' = 'X1 + X3',
                                        'sum_elpd_11' = 'X1 + X2 + X3',  
                                        'sum_elpd_13' = 'X1 + X3 + X4', 
                                        'sum_elpd_15' = 'X1 + X2 + X3 + X4',
                                        'sum_elpd_13a' = '*X1 + X3 + X4', 
                                        'sum_elpd_15a' = '*X1 + X2 + X3 + X4',
                                        'sum_elpd_12' = 'X1 + X2 + X4', 
                                        'sum_elpd_14' = 'X2 + X3 + X4')))  %>% 
  select(., X3, iteration:elpd_sum_X3sae)

elpd_X3sae_mean_tab = do.call(rbind, elpd_X3sae_list) %>% 
  pivot_longer(c(mean_elpd_06:mean_elpd_14), 
               names_to = "model", 
               values_to = "elpd_mean_X3sae") %>% 
  mutate(model = plyr::revalue(model, c('mean_elpd_06' = 'X1 + X3',
                                        'mean_elpd_11' = 'X1 + X2 + X3',  
                                        'mean_elpd_13' = 'X1 + X3 + X4', 
                                        'mean_elpd_15' = 'X1 + X2 + X3 + X4',
                                        'mean_elpd_13a' = '*X1 + X3 + X4', 
                                        'mean_elpd_15a' = '*X1 + X2 + X3 + X4',
                                        'mean_elpd_12' = 'X1 + X2 + X4', 
                                        'mean_elpd_14' = 'X2 + X3 + X4')))  %>% 
  select(., X3,iteration:elpd_mean_X3sae)

elpd_X3sae_se_tab = do.call(rbind, elpd_X3sae_list) %>% 
  pivot_longer(c(se_elpd_06:se_elpd_14), 
               names_to = "model", 
               values_to = "elpd_se_X3sae") %>% 
  select(., X3,iteration:elpd_se_X3sae) %>%
  mutate(model = plyr::revalue(model, c('se_elpd_06' = 'X1 + X3',
                                        'se_elpd_11' = 'X1 + X2 + X3',  
                                        'se_elpd_13' = 'X1 + X3 + X4', 
                                        'se_elpd_15' = 'X1 + X2 + X3 + X4',
                                        'se_elpd_13a' = '*X1 + X3 + X4', 
                                        'se_elpd_15a' = '*X1 + X2 + X3 + X4',
                                        'se_elpd_12' = 'X1 + X2 + X4', 
                                        'se_elpd_14' = 'X2 + X3 + X4'))) 
elpd_X3sae_n_tab = do.call(rbind, elpd_X3sae_list) %>% 
  pivot_longer(n_X3sae, 
               names_to = "model", 
               values_to = "n_X3sae") %>% 
  select(., X3,iteration:n_X3sae) 

elpd_X4sae_sum_tab = do.call(rbind, elpd_X4sae_list) %>% 
  pivot_longer(c(sum_elpd_06:sum_elpd_14), 
               names_to = "model", 
               values_to = "elpd_sum_X4sae") %>% 
  mutate(model = plyr::revalue(model, c('sum_elpd_06' = 'X1 + X3',
                                        'sum_elpd_11' = 'X1 + X2 + X3',  
                                        'sum_elpd_13' = 'X1 + X3 + X4', 
                                        'sum_elpd_15' = 'X1 + X2 + X3 + X4',
                                        'sum_elpd_13a' = '*X1 + X3 + X4', 
                                        'sum_elpd_15a' = '*X1 + X2 + X3 + X4',
                                        'sum_elpd_12' = 'X1 + X2 + X4', 
                                        'sum_elpd_14' = 'X2 + X3 + X4')))  %>% 
  select(., X4,iteration:elpd_sum_X4sae)

elpd_X4sae_se_tab = do.call(rbind, elpd_X4sae_list) %>% 
  pivot_longer(c(se_elpd_06:se_elpd_14), 
               names_to = "model", 
               values_to = "elpd_se_X4sae") %>% 
  select(., X4,iteration:elpd_se_X4sae) %>%
  mutate(model = plyr::revalue(model, c('se_elpd_06' = 'X1 + X3',
                                        'se_elpd_11' = 'X1 + X2 + X3',  
                                        'se_elpd_13' = 'X1 + X3 + X4', 
                                        'se_elpd_15' = 'X1 + X2 + X3 + X4',
                                        'se_elpd_13a' = '*X1 + X3 + X4', 
                                        'se_elpd_15a' = '*X1 + X2 + X3 + X4',
                                        'se_elpd_12' = 'X1 + X2 + X4', 
                                        'se_elpd_14' = 'X2 + X3 + X4'))) 
elpd_X4sae_n_tab = do.call(rbind, elpd_X4sae_list) %>% 
  pivot_longer(n_X4sae, 
               names_to = "model", 
               values_to = "n_X4sae") %>% 
  select(., X4,iteration:n_X4sae) 

elpd_X4sae_mean_tab = do.call(rbind, elpd_X4sae_list) %>% 
  pivot_longer(c(mean_elpd_06:mean_elpd_14), 
               names_to = "model", 
               values_to = "elpd_mean_X4sae") %>% 
  mutate(model = plyr::revalue(model, c('mean_elpd_06' = 'X1 + X3',
                                        'mean_elpd_11' = 'X1 + X2 + X3',  
                                        'mean_elpd_13' = 'X1 + X3 + X4', 
                                        'mean_elpd_15' = 'X1 + X2 + X3 + X4',
                                        'mean_elpd_13a' = '*X1 + X3 + X4', 
                                        'mean_elpd_15a' = '*X1 + X2 + X3 + X4',
                                        'mean_elpd_12' = 'X1 + X2 + X4', 
                                        'mean_elpd_14' = 'X2 + X3 + X4'))) %>% 
  select(., X4,iteration:elpd_mean_X4sae)

