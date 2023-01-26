# elpd for population 
# unweighted loo ----------------------------------------------------------
elpd_01_tab = elpd_01_mat[iter,] %>% 
  as_tibble() %>% 
  mutate(model = 'X1') %>% 
  rename('elpd_loo' = V1, 'elpd_SE' = V2)

elpd_02_tab = elpd_02_mat[iter,] %>% 
  as_tibble() %>% 
  mutate(model = 'X2') %>% 
  rename('elpd_loo' = V1, 'elpd_SE' = V2)

elpd_03_tab = elpd_03_mat[iter,] %>% 
  as_tibble() %>% 
  mutate(model = 'X3') %>% 
  rename('elpd_loo' = V1, 'elpd_SE' = V2)

elpd_04_tab = elpd_04_mat[iter,] %>% 
  as_tibble() %>% 
  mutate(model = 'X4') %>% 
  rename('elpd_loo' = V1, 'elpd_SE' = V2)

elpd_05_tab = elpd_05_mat[iter,] %>% 
  as_tibble() %>% 
  mutate(model = 'X1 + X2') %>% 
  rename('elpd_loo' = V1, 'elpd_SE' = V2)

elpd_06_tab = elpd_06_mat[iter,] %>% 
  as_tibble() %>% 
  mutate(model = 'X1 + X3') %>% 
  rename('elpd_loo' = V1, 'elpd_SE' = V2)

elpd_07_tab = elpd_07_mat[iter,] %>% 
  as_tibble() %>% 
  mutate(model = 'X1 + X4') %>% 
  rename('elpd_loo' = V1, 'elpd_SE' = V2)

elpd_08_tab = elpd_08_mat[iter,] %>% 
  as_tibble() %>% 
  mutate(model = 'X2 + X3') %>% 
  rename('elpd_loo' = V1, 'elpd_SE' = V2)

elpd_09_tab = elpd_09_mat[iter,] %>% 
  as_tibble() %>% 
  mutate(model = 'X2 + X4') %>% 
  rename('elpd_loo' = V1, 'elpd_SE' = V2)

elpd_10_tab = elpd_10_mat[iter,] %>% 
  as_tibble() %>% 
  mutate(model = 'X3 + X4') %>% 
  rename('elpd_loo' = V1, 'elpd_SE' = V2)

elpd_11_tab = elpd_11_mat[iter,] %>% 
  as_tibble() %>% 
  mutate(model = 'X1 + X2 + X3') %>% 
  rename('elpd_loo' = V1, 'elpd_SE' = V2)

elpd_12_tab = elpd_12_mat[iter,] %>% 
  as_tibble() %>% 
  mutate(model = 'X1 + X2 + X4') %>%
  rename('elpd_loo' = V1, 'elpd_SE' = V2)

elpd_13_tab = elpd_13_mat[iter,] %>% 
  as_tibble() %>% 
  mutate(model = 'X1 + X3 + X4') %>% 
  rename('elpd_loo' = V1, 'elpd_SE' = V2)

elpd_14_tab = elpd_14_mat[iter,] %>% 
  as_tibble() %>% 
  mutate(model = 'X2 + X3 + X4') %>% 
  rename('elpd_loo' = V1, 'elpd_SE' = V2)

elpd_15_tab = elpd_15_mat[iter,] %>% 
  as_tibble() %>% 
  mutate(model = 'X1 + X2 + X3 + X4') %>% 
  rename('elpd_loo' = V1, 'elpd_SE' = V2)
  
elpd_all_tab = rbind(elpd_01_tab, elpd_02_tab, elpd_03_tab, elpd_04_tab, elpd_05_tab,
                     elpd_06_tab, elpd_07_tab, elpd_08_tab, elpd_09_tab, elpd_10_tab,
                     elpd_11_tab, elpd_12_tab, elpd_13_tab, elpd_14_tab, elpd_15_tab) %>% 
  mutate(elpd_low90 = .$elpd_loo - (.$elpd_SE*1.64), # calculating upper and lower bound of the elpd values
         elpd_upp90 = .$elpd_loo + (.$elpd_SE*1.64),
         elpd_width = elpd_upp90 - elpd_low90,
         iteration = rep(1:length(iter), 15))


# weighted loo ------------------------------------------------------------
loo_wtd_01_tab = loo_wtd_01_list[iter] %>% 
  do.call(rbind, .) %>% 
  mutate(model = 'X1',
         iteration = 1:length(iter))
loo_wtd_02_tab = loo_wtd_02_list[iter] %>% 
  do.call(rbind, .) %>% 
  mutate(model = 'X2',
         iteration = 1:length(iter))
loo_wtd_03_tab = loo_wtd_03_list[iter] %>% 
  do.call(rbind, .) %>% 
  mutate(model = 'X3',
         iteration = 1:length(iter))
loo_wtd_04_tab = loo_wtd_04_list[iter] %>% 
  do.call(rbind, .) %>% 
  mutate(model = 'X4',
         iteration = 1:length(iter))
loo_wtd_05_tab = loo_wtd_05_list[iter] %>% 
  do.call(rbind, .) %>% 
  mutate(model = 'X1 + X2',
         iteration = 1:length(iter))
loo_wtd_06_tab = loo_wtd_06_list[iter] %>% 
  do.call(rbind, .) %>% 
  mutate(model = 'X1 + X3',
         iteration = 1:length(iter))
loo_wtd_07_tab = loo_wtd_07_list[iter] %>% 
  do.call(rbind, .) %>% 
  mutate(model = 'X1 + X4',
         iteration = 1:length(iter))
loo_wtd_08_tab = loo_wtd_08_list[iter] %>% 
  do.call(rbind, .) %>% 
  mutate(model = 'X2 + X3',
         iteration = 1:length(iter))
loo_wtd_09_tab = loo_wtd_09_list[iter] %>% 
  do.call(rbind, .) %>% 
  mutate(model = 'X2 + X4',
         iteration = 1:length(iter))
loo_wtd_10_tab = loo_wtd_10_list[iter] %>% 
  do.call(rbind, .) %>% 
  mutate(model = 'X3 + X4',
         iteration = 1:length(iter))
loo_wtd_11_tab = loo_wtd_11_list[iter] %>% 
  do.call(rbind, .) %>% 
  mutate(model = 'X1 + X2 + X3',
         iteration = 1:length(iter))
loo_wtd_12_tab = loo_wtd_12_list[iter] %>% 
  do.call(rbind, .) %>% 
  mutate(model = 'X1 + X2 + X4',
         iteration = 1:length(iter))
loo_wtd_13_tab = loo_wtd_13_list[iter] %>% 
  do.call(rbind, .) %>% 
  mutate(model = 'X1 + X3 + X4',
         iteration = 1:length(iter))
loo_wtd_14_tab = loo_wtd_14_list[iter] %>% 
  do.call(rbind, .) %>% 
  mutate(model = 'X2 + X3 + X4',
         iteration = 1:length(iter)) 
loo_wtd_15_tab = loo_wtd_15_list[iter] %>% 
  do.call(rbind, .) %>% 
  mutate(model = 'X1 + X2 + X3 + X4',
         iteration = 1:length(iter))


wtdElpd_all_tab = rbind(loo_wtd_01_tab, loo_wtd_02_tab, loo_wtd_03_tab, loo_wtd_04_tab, loo_wtd_05_tab, 
                        loo_wtd_06_tab, loo_wtd_07_tab, loo_wtd_08_tab, loo_wtd_09_tab, loo_wtd_10_tab, 
                        loo_wtd_11_tab, loo_wtd_12_tab, loo_wtd_13_tab, loo_wtd_14_tab, loo_wtd_15_tab) %>% 
  rename(wtdElpd_loo = wtd_elpd_loo,
         wtdElpd_SE = wtd_SE) %>%  
  mutate(wtdElpd_low90 = .$wtdElpd_loo - (.$wtdElpd_SE*1.64), # calculating upper and lower bound of the elpd values
         wtdElpd_upp90 = .$wtdElpd_loo + (.$wtdElpd_SE*1.64),
         wtdElpd_width = wtdElpd_upp90 - wtdElpd_low90)
