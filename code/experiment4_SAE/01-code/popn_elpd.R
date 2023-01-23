# elpd for population 
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
         iteration = rep(1:length(iter), 8))


# weighted loo ------------------------------------------------------------
loo_wtd_06_tab = loo_wtd_06_list[iter] %>% 
  do.call(rbind, .) %>% 
  mutate(model = 'X1 + X3',
         iteration = 1:length(iter))
loo_wtd_11_tab = loo_wtd_11_list[iter] %>% 
  do.call(rbind, .) %>% 
  mutate(model = 'X1 + X2 + X3',
         iteration = 1:length(iter))
loo_wtd_13_tab = loo_wtd_13_list[iter] %>% 
  do.call(rbind, .) %>% 
  mutate(model = 'X1 + X3 + X4',
         iteration = 1:length(iter))
loo_wtd_13a_tab = loo_wtd_13a_list[iter] %>% 
  do.call(rbind, .) %>% 
  mutate(model = '*X1 + X3 + X4',
         iteration = 1:length(iter))
loo_wtd_15_tab = loo_wtd_15_list[iter] %>% 
  do.call(rbind, .) %>% 
  mutate(model = 'X1 + X2 + X3 + X4',
         iteration = 1:length(iter))
loo_wtd_15a_tab = loo_wtd_15a_list[iter] %>% 
  do.call(rbind, .) %>% 
  mutate(model = '*X1 + X2 + X3 + X4',
         iteration = 1:length(iter))
loo_wtd_12_tab = loo_wtd_12_list[iter] %>% 
  do.call(rbind, .) %>% 
  mutate(model = 'X1 + X2 + X4',
         iteration = 1:length(iter)) 
loo_wtd_14_tab = loo_wtd_14_list[iter] %>% 
  do.call(rbind, .) %>% 
  mutate(model = 'X2 + X3 + X4',
         iteration = 1:length(iter)) 

wtdElpd_all_tab = rbind(loo_wtd_06_tab, loo_wtd_11_tab, loo_wtd_13_tab,
                        loo_wtd_13a_tab, loo_wtd_15_tab, loo_wtd_15a_tab,
                        loo_wtd_12_tab, loo_wtd_14_tab) %>% 
  rename(wtdElpd_loo = wtd_elpd_loo,
         wtdElpd_SE = wtd_SE) %>%  
  mutate(wtdElpd_low90 = .$wtdElpd_loo - (.$wtdElpd_SE*1.64), # calculating upper and lower bound of the elpd values
         wtdElpd_upp90 = .$wtdElpd_loo + (.$wtdElpd_SE*1.64),
         wtdElpd_width = wtdElpd_upp90 - wtdElpd_low90)
