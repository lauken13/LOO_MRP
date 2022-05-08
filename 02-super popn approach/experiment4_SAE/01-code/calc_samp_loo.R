# unweighted loo ----------------------------------------------------------
elpd_06_tab = elpd_06_mat[iter,] %>% 
  as_tibble() %>% 
  mutate(model = 'X1 + X3') %>% 
  rename('elpd_loo' = V1, 'elpd_SE' = V2)

elpd_11_tab = elpd_11_mat[iter,] %>% 
  as_tibble() %>% 
  mutate(model = 'X1 + X2 + X3') %>% 
  rename('elpd_loo' = V1, 'elpd_SE' = V2)

elpd_13_tab = elpd_13_mat[iter,] %>% 
  as_tibble() %>% 
  mutate(model = 'X1 + X3 + X4') %>% 
  rename('elpd_loo' = V1, 'elpd_SE' = V2)

elpd_13a_tab = elpd_13a_mat[iter,] %>% 
  as_tibble() %>% 
  mutate(model = '*X1 + X3 + X4') %>% 
  rename('elpd_loo' = V1, 'elpd_SE' = V2)

elpd_15_tab = elpd_15_mat[iter,] %>% 
  as_tibble() %>% 
  mutate(model = 'X1 + X2 + X3 + X4') %>% 
  rename('elpd_loo' = V1, 'elpd_SE' = V2)

elpd_15a_tab = elpd_15a_mat[iter,] %>% 
  as_tibble() %>% 
  mutate(model = '*X1 + X2 + X3 + X4') %>% 
  rename('elpd_loo' = V1, 'elpd_SE' = V2)

elpd_12_tab = elpd_12_mat[iter,] %>% 
  as_tibble() %>% 
  mutate(model = 'X1 + X2 + X4') %>% 
  rename('elpd_loo' = V1, 'elpd_SE' = V2)

elpd_14_tab = elpd_14_mat[iter,] %>% 
  as_tibble() %>% 
  mutate(model = 'X2 + X3 + X4') %>% 
  rename('elpd_loo' = V1, 'elpd_SE' = V2)

elpd_all_tab = rbind(elpd_06_tab, elpd_11_tab, elpd_13_tab,
                     elpd_13a_tab, elpd_15_tab, elpd_15a_tab,
                     elpd_12_tab, elpd_14_tab) %>% 
  mutate(elpd_low90 = .$elpd_loo - (.$elpd_SE*1.64), # calculating upper and lower bound of the elpd values
         elpd_upp90 = .$elpd_loo + (.$elpd_SE*1.64),
         elpd_width = elpd_upp90 - elpd_low90,
         iteration = rep(1:length(iter), 8)) %>% 
  arrange(iteration)

# weighted loo ------------------------------------------------------------
loo_wtd_06_tab = loo_wtd_06_list[iter] %>% 
  do.call(rbind, .) %>% 
  mutate(model = 'model06',
         iteration = 1:length(iter))
loo_wtd_11_tab = loo_wtd_11_list[iter] %>% 
  do.call(rbind, .) %>% 
  mutate(model = 'model11',
         iteration = 1:length(iter))
loo_wtd_13_tab = loo_wtd_13_list[iter] %>% 
  do.call(rbind, .) %>% 
  mutate(model = 'model13',
         iteration = 1:length(iter))
loo_wtd_13a_tab = loo_wtd_13a_list[iter] %>% 
  do.call(rbind, .) %>% 
  mutate(model = 'model13a',
         iteration = 1:length(iter))
loo_wtd_15_tab = loo_wtd_15_list[iter] %>% 
  do.call(rbind, .) %>% 
  mutate(model = 'model15',
         iteration = 1:length(iter))
loo_wtd_15a_tab = loo_wtd_15a_list[iter] %>% 
  do.call(rbind, .) %>% 
  mutate(model = 'model15a',
         iteration = 1:length(iter))
loo_wtd_12_tab = loo_wtd_12_list[iter] %>% 
  do.call(rbind, .) %>% 
  mutate(model = 'model12',
         iteration = 1:length(iter))
loo_wtd_14_tab = loo_wtd_14_list[iter] %>% 
  do.call(rbind, .) %>% 
  mutate(model = 'model14',
         iteration = 1:length(iter))


wtd_tab_all = rbind(loo_wtd_06_tab, loo_wtd_11_tab, loo_wtd_13_tab,
                        loo_wtd_13a_tab, loo_wtd_15_tab, loo_wtd_15a_tab,
                        loo_wtd_12_tab, loo_wtd_14_tab)

wtd_elpd_se_tab = wtd_tab_all %>% 
  mutate(model = factor(model),
         wtd_elpd_low90 = .$wtd_elpd_loo - (.$wtd_SE*1.64), # calculating upper and lower bound of the elpd values
         wtd_elpd_upp90 = .$wtd_elpd_loo + (.$wtd_SE*1.64),
         wtd_elpd_width = wtd_elpd_upp90 - wtd_elpd_low90) %>% 
  mutate(model = plyr::revalue(model, c('model06' = 'X1 + X3',
                                        'model11' = 'X1 + X2 + X3',  
                                        'model13' = 'X1 + X3 + X4', 
                                        'model13a' = '*X1 + X3 + X4', 
                                        'model15' = 'X1 + X2 + X3 + X4',
                                        'model15a' = '*X1 + X2 + X3 + X4',
                                        'model12' = 'X1 + X2 + X4',
                                        'model14' = 'X2 + X3 + X4')))


