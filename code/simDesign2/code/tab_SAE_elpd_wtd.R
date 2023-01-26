## weighted loo by SAE

elpd_X1sae_list = 
  elpd_X2sae_list = 
  elpd_X3sae_list = 
  elpd_X4sae_list = list()

tabX1_sum_ite = tabX2_sum_ite = 
  tabX3_sum_ite = tabX4_sum_ite = list()

tabX1_mean_ite = tabX2_mean_ite = 
  tabX3_mean_ite = tabX4_mean_ite = list()

# LOO by SAE --------------------------------------------------------------
# calculating the sum and mean by X1 groups
for (ite in iter){
  
  samp_data = samp_data_list[[ite]]
  
  ## creating survey design
  svy1 = svydesign(ids=~1, # cluster id, ~1 for no clusters
                   weights=~rep(1,nrow(samp_data)), # equal weights for each unit
                   data=samp_data)
  
  # raked to the population
  rake1 = rake(design = svy1, sample.margins = list(~X1,~X2,~X3,~X4),
               population.margins = list(X1_margin, X2_margin, X3_margin, X4_margin))
  
  # raked weights 
  samp_data$wts = weights(rake1)
  
  # creating survey raked weights
  svy_rake = svydesign(ids=~1, # cluster id, ~1 for no clusters
                       weights=~wts, # including raked weights in the survey design
                       data=samp_data)
  

# X1 ----------------------------------------------------------------------
  tabX1_sum = tabX1_mean = list()
   for (lvl in 1:5){
    looX1 = samp_data %>%
      filter(X1 == lvl) %>%
      select(., elpd_loo_06:elpd_loo_15a)
    ind = which(samp_data$X1 == lvl)

    # sum of weighted loo
    tabX1_sum[[lvl]] = svytotal(looX1, svy_rake[ind,]) %>%
      data.frame(.) %>%
      rownames_to_column(., var = "model") %>%
      mutate(X1 = lvl,
             n_X1sae = length(ind),
             iteration = ite) %>%
      mutate(model = plyr::revalue(model, c('elpd_loo_06' = 'X1 + X3',
                                            'elpd_loo_11' = 'X1 + X2 + X3',
                                            'elpd_loo_13' = 'X1 + X3 + X4',
                                            'elpd_loo_15' = 'X1 + X2 + X3 + X4',
                                            'elpd_loo_13a' = '*X1 + X3 + X4',
                                            'elpd_loo_15a' = '*X1 + X2 + X3 + X4',
                                            'elpd_loo_12' = 'X1 + X2 + X4',
                                            'elpd_loo_14' = 'X2 + X3 + X4'))) %>% 
      mutate(X1 = as.factor(X1))
    
    # mean of weighted LOO
    tabX1_mean[[lvl]] = svymean(looX1, svy_rake[ind,]) %>%
      data.frame(.) %>%
      rownames_to_column(., var = "model") %>%
      mutate(X1 = lvl,
             iteration = ite) %>% 
      mutate(model = plyr::revalue(model, c('elpd_loo_06' = 'X1 + X3',
                                            'elpd_loo_11' = 'X1 + X2 + X3',
                                            'elpd_loo_13' = 'X1 + X3 + X4',
                                            'elpd_loo_15' = 'X1 + X2 + X3 + X4',
                                            'elpd_loo_13a' = '*X1 + X3 + X4',
                                            'elpd_loo_15a' = '*X1 + X2 + X3 + X4',
                                            'elpd_loo_12' = 'X1 + X2 + X4',
                                            'elpd_loo_14' = 'X2 + X3 + X4'))) %>% 
      mutate(X1 = as.factor(X1))
  }
  tabX1_sum_ite[[ite]] = do.call(rbind, tabX1_sum)
  tabX1_mean_ite[[ite]] = do.call(rbind, tabX1_mean)
  

# X2 ----------------------------------------------------------------------
  tabX2_sum = tabX2_mean = list()
  for (lvl in 1:5){
    looX2 = samp_data %>%
      filter(X2 == lvl) %>%
      select(., elpd_loo_06:elpd_loo_15a)
    ind = which(samp_data$X2 == lvl)

    tabX2_sum[[lvl]] = svytotal(looX2, svy_rake[ind,]) %>%
      data.frame(.) %>%
      rownames_to_column(., var = "model") %>%
      mutate(X2 = lvl,
             n_X2sae = length(ind),
             iteration = ite) %>%
      mutate(model = plyr::revalue(model, c('elpd_loo_06' = 'X1 + X3',
                                            'elpd_loo_11' = 'X1 + X2 + X3',
                                            'elpd_loo_13' = 'X1 + X3 + X4',
                                            'elpd_loo_15' = 'X1 + X2 + X3 + X4',
                                            'elpd_loo_13a' = '*X1 + X3 + X4',
                                            'elpd_loo_15a' = '*X1 + X2 + X3 + X4',
                                            'elpd_loo_12' = 'X1 + X2 + X4',
                                            'elpd_loo_14' = 'X2 + X3 + X4'))) %>% 
      mutate(X2 = as.factor(X2))
    
    tabX2_mean[[lvl]] = svymean(looX2, svy_rake[ind,]) %>%
      data.frame(.) %>%
      rownames_to_column(., var = "model") %>%
      mutate(X2 = lvl,
             iteration = ite) %>%
      mutate(model = plyr::revalue(model, c('elpd_loo_06' = 'X1 + X3',
                                            'elpd_loo_11' = 'X1 + X2 + X3',
                                            'elpd_loo_13' = 'X1 + X3 + X4',
                                            'elpd_loo_15' = 'X1 + X2 + X3 + X4',
                                            'elpd_loo_13a' = '*X1 + X3 + X4',
                                            'elpd_loo_15a' = '*X1 + X2 + X3 + X4',
                                            'elpd_loo_12' = 'X1 + X2 + X4',
                                            'elpd_loo_14' = 'X2 + X3 + X4'))) %>% 
      mutate(X2 = as.factor(X2))
  }
  tabX2_sum_ite[[ite]] = do.call(rbind, tabX2_sum)
  tabX2_mean_ite[[ite]] = do.call(rbind, tabX2_mean)

# X3 ----------------------------------------------------------------------
  tabX3_sum = tabX3_mean = list()
  for (lvl in 1:5){
    looX3 = samp_data %>%
      filter(X3 == lvl) %>%
      select(., elpd_loo_06:elpd_loo_15a)
    ind = which(samp_data$X3 == lvl)

    tabX3_sum[[lvl]] = svytotal(looX3, svy_rake[ind,]) %>%
      data.frame(.) %>%
      rownames_to_column(., var = "model") %>%
      mutate(X3 = lvl,
             n_X3sae = length(ind),
             iteration = ite) %>%
      mutate(model = plyr::revalue(model, c('elpd_loo_06' = 'X1 + X3',
                                            'elpd_loo_11' = 'X1 + X2 + X3',
                                            'elpd_loo_13' = 'X1 + X3 + X4',
                                            'elpd_loo_15' = 'X1 + X2 + X3 + X4',
                                            'elpd_loo_13a' = '*X1 + X3 + X4',
                                            'elpd_loo_15a' = '*X1 + X2 + X3 + X4',
                                            'elpd_loo_12' = 'X1 + X2 + X4',
                                            'elpd_loo_14' = 'X2 + X3 + X4'))) %>% 
      mutate(X3 = as.factor(X3))
    
    tabX3_mean[[lvl]] = svymean(looX3, svy_rake[ind,]) %>%
      data.frame(.) %>%
      rownames_to_column(., var = "model") %>%
      mutate(X3 = lvl,
             iteration = ite) %>%
      mutate(model = plyr::revalue(model, c('elpd_loo_06' = 'X1 + X3',
                                            'elpd_loo_11' = 'X1 + X2 + X3',
                                            'elpd_loo_13' = 'X1 + X3 + X4',
                                            'elpd_loo_15' = 'X1 + X2 + X3 + X4',
                                            'elpd_loo_13a' = '*X1 + X3 + X4',
                                            'elpd_loo_15a' = '*X1 + X2 + X3 + X4',
                                            'elpd_loo_12' = 'X1 + X2 + X4',
                                            'elpd_loo_14' = 'X2 + X3 + X4'))) %>% 
      mutate(X3 = as.factor(X3))
  }
  tabX3_sum_ite[[ite]] = do.call(rbind, tabX3_sum)
  tabX3_mean_ite[[ite]] = do.call(rbind, tabX3_mean)
  


  # X4 ----------------------------------------------------------------------
  tabX4_sum = tabX4_mean = list()
  for (lvl in 1:12){
    looX4 = samp_data %>% 
      filter(X4 == lvl) %>% 
      select(., elpd_loo_06:elpd_loo_15a) 
    ind = which(samp_data$X4 == lvl)
    
    tabX4_sum[[lvl]] = svytotal(looX4, svy_rake[ind,]) %>% 
      data.frame(.) %>% 
      rownames_to_column(., var = "model") %>% 
      mutate(X4 = lvl,
             n_X4sae = length(ind),
             iteration = ite) %>% 
      mutate(model = plyr::revalue(model, c('elpd_loo_06' = 'X1 + X3',
                                            'elpd_loo_11' = 'X1 + X2 + X3',  
                                            'elpd_loo_13' = 'X1 + X3 + X4', 
                                            'elpd_loo_15' = 'X1 + X2 + X3 + X4',
                                            'elpd_loo_13a' = '*X1 + X3 + X4', 
                                            'elpd_loo_15a' = '*X1 + X2 + X3 + X4',
                                            'elpd_loo_12' = 'X1 + X2 + X4', 
                                            'elpd_loo_14' = 'X2 + X3 + X4'))) %>% 
  mutate(X4 = as.factor(X4))
    
    tabX4_mean[[lvl]] = svymean(looX4, svy_rake[ind,]) %>% 
      data.frame(.) %>% 
      rownames_to_column(., var = "model") %>% 
      mutate(X4 = lvl,
             iteration = ite) %>% 
      mutate(model = plyr::revalue(model, c('elpd_loo_06' = 'X1 + X3',
                                            'elpd_loo_11' = 'X1 + X2 + X3',  
                                            'elpd_loo_13' = 'X1 + X3 + X4', 
                                            'elpd_loo_15' = 'X1 + X2 + X3 + X4',
                                            'elpd_loo_13a' = '*X1 + X3 + X4', 
                                            'elpd_loo_15a' = '*X1 + X2 + X3 + X4',
                                            'elpd_loo_12' = 'X1 + X2 + X4', 
                                            'elpd_loo_14' = 'X2 + X3 + X4'))) %>% 
      mutate(X4 = as.factor(X4))
  }
  tabX4_mean_ite[[ite]] = do.call(rbind, tabX4_mean)
  tabX4_sum_ite[[ite]] = do.call(rbind, tabX4_sum)
}

tabX1_mean_all = do.call(rbind,tabX1_mean_ite) %>% 
  rename(wtdMean_SE_X1sae = SE)
tabX2_mean_all = do.call(rbind,tabX2_mean_ite) %>% 
  rename(wtdMean_SE_X2sae = SE)
tabX3_mean_all = do.call(rbind,tabX3_mean_ite) %>% 
  rename(wtdMean_SE_X3sae = SE)
tabX4_mean_all = do.call(rbind,tabX4_mean_ite) %>% 
  rename(wtdMean_SE_X4sae = SE)

tabX1_all = do.call(rbind,tabX1_sum_ite) %>% 
  left_join(., tabX1_mean_all, by=c('model', 'iteration', 'X1')) %>% 
  rename(wtdTotal_X1sae = total,
         wtdMean_X1sae = mean,
         wtdTotal_SE_X1sae = SE) %>% 
  mutate(wtdMean_low90_X1sae = .$wtdMean_X1sae - (.$wtdMean_SE_X1sae*1.64), # calculating upper and lower bound of the elpd values
         wtdMean_upp90_X1sae = .$wtdMean_X1sae + (.$wtdMean_SE_X1sae*1.64),
         wtdMean_width_X1sae = wtdMean_upp90_X1sae - wtdMean_low90_X1sae)
tabX2_all = do.call(rbind,tabX2_sum_ite) %>% 
  left_join(., tabX2_mean_all, by=c('model', 'iteration', 'X2')) %>% 
  rename(wtdTotal_X2sae = total,
         wtdMean_X2sae = mean,
         wtdTotal_SE_X2sae = SE) %>% 
  mutate(wtdMean_low90_X2sae = .$wtdMean_X2sae - (.$wtdMean_SE_X2sae*1.64), # calculating upper and lower bound of the elpd values
         wtdMean_upp90_X2sae = .$wtdMean_X2sae + (.$wtdMean_SE_X2sae*1.64),
         wtdMean_width_X2sae = wtdMean_upp90_X2sae - wtdMean_low90_X2sae)
tabX3_all = do.call(rbind,tabX3_sum_ite) %>% 
  left_join(., tabX3_mean_all, by=c('model', 'iteration', 'X3')) %>%  
  rename(wtdTotal_X3sae = total,
         wtdMean_X3sae = mean,
         wtdTotal_SE_X3sae = SE) %>% 
  mutate(wtdMean_low90_X3sae = .$wtdMean_X3sae - (.$wtdMean_SE_X3sae*1.64), # calculating upper and lower bound of the elpd values
         wtdMean_upp90_X3sae = .$wtdMean_X3sae + (.$wtdMean_SE_X3sae*1.64),
         wtdMean_width_X3sae = wtdMean_upp90_X3sae - wtdMean_low90_X3sae)
tabX4_all = do.call(rbind,tabX4_sum_ite) %>% 
  left_join(., tabX4_mean_all, by=c('model', 'iteration', 'X4')) %>% 
  rename(wtdTotal_X4sae = total,
         wtdMean_X4sae = mean,
         wtdTotal_SE_X4sae = SE) %>% 
  mutate(wtdMean_low90_X4sae = .$wtdMean_X4sae - (.$wtdMean_SE_X4sae*1.64), # calculating upper and lower bound of the elpd values
         wtdMean_upp90_X4sae = .$wtdMean_X4sae + (.$wtdMean_SE_X4sae*1.64),
         wtdMean_width_X4sae = wtdMean_upp90_X4sae - wtdMean_low90_X4sae)

