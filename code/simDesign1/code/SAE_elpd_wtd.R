## weighted loo by SAE
## elpd_SAE
elpd_X1sae_list = 
  elpd_X2sae_list = 
  elpd_X3sae_list = 
  elpd_X4sae_list = list()

tabX1 = tabX2 = 
  tabX3 = tabX4 = 
tabX1_ite = tabX2_ite = 
  tabX3_ite = tabX4_ite = list()

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
  for (lvl in 1:5){
    looX1 = samp_data %>%
      filter(X1 == lvl) %>%
      select(., elpd_loo_01:elpd_loo_15)
    ind = which(samp_data$X1 == lvl)
    
    t1 = svymean(looX1, svy_rake[ind,]) %>%
      data.frame(.) %>%
      rename(wtdMean_X1sae = mean,
             wtdMean_SE_X1sae = SE)

    tabX1[[lvl]] = svytotal(looX1, svy_rake[ind,]) %>%
      data.frame(.) %>%
      rownames_to_column(., var = "model") %>%
      mutate(X1 = lvl,
             n_X1sae = length(ind),
             iteration = ite) %>%
      rename(wtdTotal_X1sae = total,
             wtdTotal_SE_X1sae = SE) %>% 
      cbind(., t1) %>% 
      mutate(model = plyr::revalue(model, c('elpd_loo_01' = 'X1', 
                                            'elpd_loo_02' = 'X2', 
                                            'elpd_loo_03' = 'X3',
                                            'elpd_loo_04' = 'X4',
                                            'elpd_loo_05' = 'X1 + X2',
                                            'elpd_loo_06' = 'X1 + X3',
                                            'elpd_loo_07' = 'X1 + X4', 
                                            'elpd_loo_08' = 'X2 + X3', 
                                            'elpd_loo_09' = 'X2 + X4',
                                            'elpd_loo_10' = 'X3 + X4',
                                            'elpd_loo_11' = 'X1 + X2 + X3', 
                                            'elpd_loo_12' = 'X1 + X2 + X4', 
                                            'elpd_loo_13' = 'X1 + X3 + X4',
                                            'elpd_loo_14' = 'X2 + X3 + X4',
                                            'elpd_loo_15' = 'X1 + X2 + X3 + X4'))) 
  }
  tabX1_ite[[ite]] = do.call(rbind, tabX1)

# X2 ----------------------------------------------------------------------
  tabX2 = list()
  for (lvl in 1:5){
    looX2 = samp_data %>%
      filter(X2 == lvl) %>%
      select(., elpd_loo_01:elpd_loo_15)
    ind = which(samp_data$X2 == lvl)
    
    t2 = svymean(looX2, svy_rake[ind,]) %>%
      data.frame(.) %>%
      rename(wtdMean_X2sae = mean,
             wtdMean_SE_X2sae = SE)
    
    tabX2[[lvl]] = svytotal(looX2, svy_rake[ind,]) %>%
      data.frame(.) %>%
      rownames_to_column(., var = "model") %>%
      mutate(X2 = lvl,
             n_X2sae = length(ind),
             iteration = ite) %>%
      rename(wtdTotal_X2sae = total,
             wtdTotal_SE_X2sae = SE) %>%
      cbind(., t2) %>% 
      mutate(model = plyr::revalue(model, c('elpd_loo_01' = 'X1', 
                                            'elpd_loo_02' = 'X2', 
                                            'elpd_loo_03' = 'X3',
                                            'elpd_loo_04' = 'X4',
                                            'elpd_loo_05' = 'X1 + X2',
                                            'elpd_loo_06' = 'X1 + X3',
                                            'elpd_loo_07' = 'X1 + X4', 
                                            'elpd_loo_08' = 'X2 + X3', 
                                            'elpd_loo_09' = 'X2 + X4',
                                            'elpd_loo_10' = 'X3 + X4',
                                            'elpd_loo_11' = 'X1 + X2 + X3', 
                                            'elpd_loo_12' = 'X1 + X2 + X4', 
                                            'elpd_loo_13' = 'X1 + X3 + X4',
                                            'elpd_loo_14' = 'X2 + X3 + X4',
                                            'elpd_loo_15' = 'X1 + X2 + X3 + X4')))
  }
  tabX2_ite[[ite]] = do.call(rbind, tabX2)


# X3 ----------------------------------------------------------------------
  tabX3 = list()
  for (lvl in 1:5){
    looX3 = samp_data %>%
      filter(X3 == lvl) %>%
      select(., elpd_loo_01:elpd_loo_15)
    ind = which(samp_data$X3 == lvl)
    
    t3 = svymean(looX3, svy_rake[ind,]) %>%
      data.frame(.) %>%
      rename(wtdMean_X3sae = mean,
             wtdMean_SE_X3sae = SE)

    tabX3[[lvl]] = svytotal(looX3, svy_rake[ind,]) %>%
      data.frame(.) %>%
      rownames_to_column(., var = "model") %>%
      mutate(X3 = lvl,
             n_X3sae = length(ind),
             iteration = ite) %>%
      rename(wtdTotal_X3sae = total,
             wtdTotal_SE_X3sae = SE) %>%
      cbind(., t3) %>% 
      mutate(model = plyr::revalue(model, c('elpd_loo_01' = 'X1', 
                                            'elpd_loo_02' = 'X2', 
                                            'elpd_loo_03' = 'X3',
                                            'elpd_loo_04' = 'X4',
                                            'elpd_loo_05' = 'X1 + X2',
                                            'elpd_loo_06' = 'X1 + X3',
                                            'elpd_loo_07' = 'X1 + X4', 
                                            'elpd_loo_08' = 'X2 + X3', 
                                            'elpd_loo_09' = 'X2 + X4',
                                            'elpd_loo_10' = 'X3 + X4',
                                            'elpd_loo_11' = 'X1 + X2 + X3', 
                                            'elpd_loo_12' = 'X1 + X2 + X4', 
                                            'elpd_loo_13' = 'X1 + X3 + X4',
                                            'elpd_loo_14' = 'X2 + X3 + X4',
                                            'elpd_loo_15' = 'X1 + X2 + X3 + X4'))) 
  }
  tabX3_ite[[ite]] = do.call(rbind, tabX3)


  # X4 ----------------------------------------------------------------------
  tabX4 = list()
  for (lvl in 1:5){
    looX4 = samp_data %>% 
      filter(X4 == lvl) %>% 
      select(., elpd_loo_01:elpd_loo_15) 
    ind = which(samp_data$X4 == lvl)
    
    t4 = svymean(looX4, svy_rake[ind,]) %>%
      data.frame(.) %>%
      rename(wtdMean_X4sae = mean,
             wtdMean_SE_X4sae = SE)
    
    tabX4[[lvl]] = svytotal(looX4, svy_rake[ind,]) %>% 
      data.frame(.) %>% 
      rownames_to_column(., var = "model") %>% 
      mutate(X4 = lvl,
             n_X4sae = length(ind),
             iteration = ite) %>% 
      rename(wtdTotal_X4sae = total,
             wtdTotal_SE_X4sae = SE) %>%
      cbind(., t4) %>% 
      mutate(model = plyr::revalue(model, c('elpd_loo_01' = 'X1', 
                                            'elpd_loo_02' = 'X2', 
                                            'elpd_loo_03' = 'X3',
                                            'elpd_loo_04' = 'X4',
                                            'elpd_loo_05' = 'X1 + X2',
                                            'elpd_loo_06' = 'X1 + X3',
                                            'elpd_loo_07' = 'X1 + X4', 
                                            'elpd_loo_08' = 'X2 + X3', 
                                            'elpd_loo_09' = 'X2 + X4',
                                            'elpd_loo_10' = 'X3 + X4',
                                            'elpd_loo_11' = 'X1 + X2 + X3', 
                                            'elpd_loo_12' = 'X1 + X2 + X4', 
                                            'elpd_loo_13' = 'X1 + X3 + X4',
                                            'elpd_loo_14' = 'X2 + X3 + X4',
                                            'elpd_loo_15' = 'X1 + X2 + X3 + X4'))) 
  }
  tabX4_ite[[ite]] = do.call(rbind, tabX4)
}

tabX1_all = do.call(rbind,tabX1_ite) %>% 
  mutate(wtdMean_low90_X1sae = .$wtdMean_X1sae - (.$wtdMean_SE_X1sae*1.64), # calculating upper and lower bound of the elpd values
         wtdMean_upp90_X1sae = .$wtdMean_X1sae + (.$wtdMean_SE_X1sae*1.64),
         wtdMean_width_X1sae = wtdMean_upp90_X1sae - wtdMean_low90_X1sae)
tabX2_all = do.call(rbind,tabX2_ite)  %>% 
  mutate(wtdMean_low90_X2sae = .$wtdMean_X2sae - (.$wtdMean_SE_X2sae*1.64), # calculating upper and lower bound of the elpd values
         wtdMean_upp90_X2sae = .$wtdMean_X2sae + (.$wtdMean_SE_X2sae*1.64),
         wtdMean_width_X2sae = wtdMean_upp90_X2sae - wtdMean_low90_X2sae)
tabX3_all = do.call(rbind,tabX3_ite) %>% 
  mutate(wtdMean_low90_X3sae = .$wtdMean_X3sae - (.$wtdMean_SE_X3sae*1.64), # calculating upper and lower bound of the elpd values
         wtdMean_upp90_X3sae = .$wtdMean_X3sae + (.$wtdMean_SE_X3sae*1.64),
         wtdMean_width_X3sae = wtdMean_upp90_X3sae - wtdMean_low90_X3sae)
tabX4_all = do.call(rbind,tabX4_ite)  %>% 
  mutate(wtdMean_low90_X4sae = .$wtdMean_X4sae - (.$wtdMean_SE_X4sae*1.64), # calculating upper and lower bound of the elpd values
         wtdMean_upp90_X4sae = .$wtdMean_X4sae + (.$wtdMean_SE_X4sae*1.64),
         wtdMean_width_X4sae = wtdMean_upp90_X4sae - wtdMean_low90_X4sae)
