## preparing indv est and popn est for SAE of X4
## data file
load(here::here('02-super popn approach/experiment4_SAE/03-data/loo_sae_fx3_image.Rbin'))
iter = c(1:16,19:34,36:54,56:71,73:91,93:100)

samp_all = 
  sae_all_list = list()
for(i in iter){
  temp06 = sampest_tab_all[[i]] %>% 
    filter(model == 'model06') %>% 
    rename(sampestX5_06 = sampestX5,
           sampestX50_06 = sampestX50,
           sampestX95_06 = sampestX95,
           range_int_06 = range_int, 
           coverage_06 = coverage,
           model_06 = model) %>% 
    select(-prob_truth)
  
  
  temp11 = sampest_tab_all[[i]] %>% 
    filter(model == 'model11') %>% 
    rename(sampestX5_11 = sampestX5,
           sampestX50_11 = sampestX50,
           sampestX95_11 = sampestX95,
           range_int_11 = range_int,
           coverage_11 = coverage,
           model_11 = model) %>% 
    select(-prob_truth)
  
  
  temp13 = sampest_tab_all[[i]] %>% 
    filter(model == 'model13') %>% 
    rename(sampestX5_13 = sampestX5,
           sampestX50_13 = sampestX50,
           sampestX95_13 = sampestX95,  
           range_int_13 = range_int, 
           coverage_13 = coverage,
           model_13 = model) %>% 
    select(-prob_truth)
  
  
  temp15 = sampest_tab_all[[i]] %>% 
    filter(model == 'model15') %>% 
    rename(sampestX5_15 = sampestX5,
           sampestX50_15 = sampestX50,
           sampestX95_15 = sampestX95,
           range_int_15 = range_int, 
           coverage_15 = coverage,
           model_15 = model) %>% 
    select(-prob_truth)
  
  samp_all[[i]] = cbind(samp_data_list[[i]], temp06, temp11, temp13, temp15)

sae_all = samp_all[[i]] %>% 
  group_by(X4) %>% 
  summarise(sampestX5_sae_06 = mean(sampestX5_06),
            sampestX50_sae_06 = mean(sampestX50_06),
            sampestX95_sae_06 = mean(sampestX95_06),
            sampestX5_sae_11 = mean(sampestX5_11),
            sampestX50_sae_11 = mean(sampestX50_11),
            sampestX95_sae_11 = mean(sampestX95_11),
            sampestX5_sae_13 = mean(sampestX5_13),
            sampestX50_sae_13 = mean(sampestX50_13),
            sampestX95_sae_13 = mean(sampestX95_13),
            sampestX5_sae_15 = mean(sampestX5_15),
            sampestX50_sae_15 = mean(sampestX50_15),
            sampestX95_sae_15 = mean(sampestX95_15),
            y_prob_sae = mean(y_prob))

sae_06 = sae_all %>% 
  pivot_longer(sampestX5_sae_06, 
               names_to = "model", 
               values_to = "sampestX5_sae") %>% 
  select(model, sampestX5_sae, sampestX50_sae_06, sampestX95_sae_06) %>% 
  rename(sampestX50_sae = sampestX50_sae_06, 
         sampestX95_sae = sampestX95_sae_06) %>% 
  mutate(X4 = as.factor(1:12))

sae_11 = sae_all %>% 
  pivot_longer(sampestX5_sae_11, 
               names_to = "model", 
               values_to = "sampestX5_sae") %>% 
  select(model, sampestX5_sae, sampestX50_sae_11, sampestX95_sae_11) %>% 
  rename(sampestX50_sae = sampestX50_sae_11,
         sampestX95_sae = sampestX95_sae_11) %>% 
  mutate(X4 = as.factor(1:12))


sae_13 = sae_all %>% 
  pivot_longer(sampestX5_sae_13, 
               names_to = "model", 
               values_to = "sampestX5_sae") %>% 
  select(model, sampestX5_sae, sampestX50_sae_13, sampestX95_sae_13) %>% 
  rename(sampestX50_sae = sampestX50_sae_13,
         sampestX95_sae = sampestX95_sae_13) %>% 
  mutate(X4 = as.factor(1:12))


sae_15 = sae_all %>% 
  pivot_longer(sampestX5_sae_15, 
               names_to = "model", 
               values_to = "sampestX5_sae") %>% 
  select(model, sampestX5_sae, sampestX50_sae_15, sampestX95_sae_15) %>% 
  rename(sampestX50_sae = sampestX50_sae_15,
         sampestX95_sae = sampestX95_sae_15) %>% 
  mutate(X4 = as.factor(1:12))

sae_all_list[[i]] = rbind(sae_06, sae_11,
                     sae_13, sae_15) %>% 
  mutate(model = plyr::revalue(model, c('sampestX5_sae_06' = 'X1 + X3',
                                        'sampestX5_sae_11' = 'X1 + X2 + X3',  
                                        'sampestX5_sae_13' = 'X1 + X3 + X4', 
                                        'sampestX5_sae_15' = 'X1 + X2 + X3 + X4')), 
         iteration = i)
}

sampest_sae_tab = do.call(rbind,sae_all_list)

12*94*4

## population estimate
## mean of posterior of sae_popnest
ps_sae_all = list()
for(i in iter){
  ps_sae_06 = apply(data.frame(popnest_sae_X4_all[[i]][1]), 2, quantile,c(0.05, 0.5,0.95)) %>% 
    t(.) %>% 
    as_tibble(.) %>% 
    mutate(model = 'model06', 
           iteration = i, 
           X4 = 1:12)
  
  ps_sae_11 = apply(data.frame(popnest_sae_X4_all[[i]][2]), 2, quantile,c(0.05, 0.5,0.95)) %>% 
    t(.) %>% 
    as_tibble(.) %>% 
    mutate(model = 'model11', 
           iteration = i, 
           X4 = 1:12)
  
  ps_sae_13 = apply(data.frame(popnest_sae_X4_all[[i]][3]), 2, quantile,c(0.05, 0.5,0.95)) %>% 
    t(.) %>% 
    as_tibble(.) %>% 
    mutate(model = 'model13', 
           iteration = i, 
           X4 = 1:12)
  
  ps_sae_15 = apply(data.frame(popnest_sae_X4_all[[i]][5]), 2, quantile,c(0.05, 0.5,0.95)) %>% 
    t(.) %>% 
    as_tibble(.) %>% 
    mutate(model = 'model15', 
           iteration = i, 
           X4 = 1:12)
  
  ps_sae_all[[i]] = rbind(ps_sae_06, ps_sae_11,
                     ps_sae_13, ps_sae_15) 
}

ps_sae_tab = do.call(rbind, ps_sae_all) %>% 
  rename(popnestX5_sae = `5%`,
         popnestX50_sae = `50%`,
         popnestX95_sae = `95%`) %>% 
  mutate(model = plyr::revalue(model, c('model06' = 'X1 + X3',
                                        'model11' = 'X1 + X2 + X3',  
                                        'model13' = 'X1 + X3 + X4', 
                                        'model15' = 'X1 + X2 + X3 + X4')),
         X4 = as.factor(X4))

temp1 = left_join(loo_bias_tab, ps_sae_tab, by=c("model", "iteration", "X4"))

loo_bias_est_tab = left_join(temp1, sampest_sae_tab, by=c("model", "iteration", "X4"))

save(loo_bias_est_tab, loo_bias_tab, ps_sae_tab, sampest_sae_tab, file="loo_bias_est_tab.RData", compress=T)
