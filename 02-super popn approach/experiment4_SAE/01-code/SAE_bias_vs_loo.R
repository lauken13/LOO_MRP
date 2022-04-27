library(data.table) # melt(.)
library(tidyverse)


## data file
load(here::here('02-super popn approach/experiment4_SAE/03-data/loo_sae_fx3_image.Rbin'))
iter = c(1:16,19:34,36:54,56:71,73:91,93:100)


# small area loo calculation ----------------------------------------------------------
elpd_all_list = list()

for(ite in iter){
  # merging loo values to sample data 
  samp_data_list[[ite]]$elpd_loo_06 = loo_06_list[[ite]]$pointwise[,1]
  samp_data_list[[ite]]$elpd_loo_11 = loo_11_list[[ite]]$pointwise[,1]
  samp_data_list[[ite]]$elpd_loo_13 = loo_13_list[[ite]]$pointwise[,1]
  samp_data_list[[ite]]$elpd_loo_13a = loo_13a_list[[ite]]$pointwise[,1]
  samp_data_list[[ite]]$elpd_loo_15 = loo_15_list[[ite]]$pointwise[,1]
  samp_data_list[[ite]]$elpd_loo_15a = loo_15a_list[[ite]]$pointwise[,1]
  
  # calculating the sum and mean by X4 groups
  elpd_all_list[[ite]] =  samp_data_list[[ite]] %>% 
    group_by(X4) %>% 
    summarise(sum_elpd_06 = sum(elpd_loo_06),
              sum_elpd_11 = sum(elpd_loo_11),
              sum_elpd_13 = sum(elpd_loo_13),
              # sum_elpd_13a = sum(elpd_loo_13a),
              # sum_elpd_15a = sum(elpd_loo_15a), 
              sum_elpd_15 = sum(elpd_loo_15)) %>% 
    mutate(iteration = ite)
}

elpd_all = do.call(rbind, elpd_all_list)

## converting this to long format for merging 
elpd_tab = elpd_all %>% 
  pivot_longer(c(sum_elpd_06:sum_elpd_13, sum_elpd_15), 
               names_to = "model", 
               values_to = "elpd_sae") %>% 
  mutate(model = plyr::revalue(model, c('sum_elpd_06' = 'X1 + X3',
                                        'sum_elpd_11' = 'X1 + X2 + X3',  
                                        'sum_elpd_13' = 'X1 + X3 + X4', 
                                        'sum_elpd_15' = 'X1 + X2 + X3 + X4')))

elpd_tab$model = forcats::fct_relevel(elpd_tab$model, c('X1 + X2 + X3 + X4',
                                                        'X1 + X3 + X4',
                                                        'X1 + X2 + X3',
                                                        'X1 + X3'))

# group/small area prediction and bias --------------------------------------------------------
X4_group_mean_list = list()

## getting the group prob for each iteration/population
for (ite in iter){
  X4_group_mean_list[[ite]] =  popn_data_list[[ite]] %>% 
    group_by(X4) %>% 
    summarise(mean_prob = mean(y_prob)) %>% 
    mutate(iteration = ite)
}

# true group mean
X4_group_mean_tab = do.call(rbind, X4_group_mean_list)

## calculating the popnest for each level of X4 for all six of the models
popnest_sae_X4_tab =
  popnest_sae_X4_model = 
  popnest_sae_X4 = list()

for(ite in iter){
  for(m in 1:6){ # getting quantiles for each of the model
    popnest_sae_X4_model[[m]] = popnest_sae_X4_all[[ite]][m] %>%
      as.data.frame() %>% 
      apply(., 2, quantile, c(0.05, 0.5, 0.95))
  }
  popnest_sae_X4[[ite]] = popnest_sae_X4_model
} 

# extracting every 1st/2nd ... 6th list of the main list
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

calc_list_m6 =
  calc_list_m11 = 
  calc_list_m13 = 
  calc_list_m13a = 
  calc_list_m15 = 
  calc_list_m15a = list()

for(i in iter){
  # truth value for each iteration
  t1 = X4_group_mean_tab %>% 
    filter(iteration == i) 
  
  # combining truth and quantile of each iteration
  calc_tab_m6 = cbind(popnest_sae_X4_m6[[i]], t1)
  calc_tab_m11 = cbind(popnest_sae_X4_m11[[i]], t1)
  calc_tab_m13 = cbind(popnest_sae_X4_m13[[i]], t1)
  calc_tab_m13a = cbind(popnest_sae_X4_m13a[[i]], t1)
  calc_tab_m15 = cbind(popnest_sae_X4_m15[[i]], t1)
  calc_tab_m15a = cbind(popnest_sae_X4_m15a[[i]], t1)
  
  ## comparing to 'truth'
  calc_tab2_m6 = calc_tab_m6 %>% 
    mutate(mean_bias_abs = abs(as.numeric(`50%`) - as.numeric(mean_prob)),
           low_bias_abs = abs(as.numeric(`5%`) - as.numeric(mean_prob)),
           upp_bias_abs = abs(as.numeric(`95%`) - as.numeric(mean_prob)),
           coverage = ifelse(mean_prob >= `5%` & mean_prob <= `95%`, 1, 0))
  
  calc_tab2_m11 = calc_tab_m11 %>% 
    mutate(mean_bias_abs = abs(as.numeric(`50%`) - as.numeric(mean_prob)),
           low_bias_abs = abs(as.numeric(`5%`) - as.numeric(mean_prob)),
           upp_bias_abs = abs(as.numeric(`95%`) - as.numeric(mean_prob)),
           coverage = ifelse(mean_prob >= `5%` & mean_prob <= `95%`, 1, 0))
  
  calc_tab2_m13 = calc_tab_m13 %>% 
    mutate(mean_bias_abs = abs(as.numeric(`50%`) - as.numeric(mean_prob)),
           low_bias_abs = abs(as.numeric(`5%`) - as.numeric(mean_prob)),
           upp_bias_abs = abs(as.numeric(`95%`) - as.numeric(mean_prob)),
           coverage = ifelse(mean_prob >= `5%` & mean_prob <= `95%`, 1, 0))
  
  calc_tab2_m13a = calc_tab_m13a %>% 
    mutate(mean_bias_abs = abs(as.numeric(`50%`) - as.numeric(mean_prob)),
           low_bias_abs = abs(as.numeric(`5%`) - as.numeric(mean_prob)),
           upp_bias_abs = abs(as.numeric(`95%`) - as.numeric(mean_prob)),
           coverage = ifelse(mean_prob >= `5%` & mean_prob <= `95%`, 1, 0))
  
  calc_tab2_m15 = calc_tab_m15 %>% 
    mutate(mean_bias_abs = abs(as.numeric(`50%`) - as.numeric(mean_prob)),
           low_bias_abs = abs(as.numeric(`5%`) - as.numeric(mean_prob)),
           upp_bias_abs = abs(as.numeric(`95%`) - as.numeric(mean_prob)),
           coverage = ifelse(mean_prob >= `5%` & mean_prob <= `95%`, 1, 0))
  
  calc_tab2_m15a= calc_tab_m15a %>% 
    mutate(mean_bias_abs = abs(as.numeric(`50%`) - as.numeric(mean_prob)),
           low_bias_abs = abs(as.numeric(`5%`) - as.numeric(mean_prob)),
           upp_bias_abs = abs(as.numeric(`95%`) - as.numeric(mean_prob)),
           coverage = ifelse(mean_prob >= `5%` & mean_prob <= `95%`, 1, 0))
  
  calc_list_m6[[i]] = calc_tab2_m6
  calc_list_m11[[i]] = calc_tab2_m11
  calc_list_m13[[i]] = calc_tab2_m13
  calc_list_m13a[[i]] = calc_tab2_m13a
  calc_list_m15[[i]] = calc_tab2_m15
  calc_list_m15a[[i]] = calc_tab2_m15a
} 

# combining all of the iterations
sae_X4_tab_m6 = do.call(rbind, calc_list_m6) %>% 
  mutate(model = "X1 + X3")

sae_X4_tab_m11 = do.call(rbind, calc_list_m11) %>% 
  mutate(model = "X1 + X2 + X3")

sae_X4_tab_m13 = do.call(rbind, calc_list_m13) %>% 
  mutate(model = "X1 + X3 + X4")

sae_X4_tab_m13a = do.call(rbind, calc_list_m13a) %>% 
  mutate(model = "*X1 + X3 + X4")

sae_X4_tab_m15 = do.call(rbind, calc_list_m15) %>% 
  mutate(model = "X1 + X2 + X3 + X4")

sae_X4_tab_m15a = do.call(rbind, calc_list_m15a) %>% 
  mutate(model = "*X1 + X2 + X3 + X4")

model_all_pn_tab = rbind(sae_X4_tab_m6, sae_X4_tab_m11, sae_X4_tab_m13,
                         sae_X4_tab_m13a, sae_X4_tab_m15, sae_X4_tab_m15a)

## looking at specific models only
model_sel_pn_tab = model_all_pn_tab %>% 
  filter(model == 'X1 + X2 + X3 + X4' | model == 'X1 + X2 + X3' |
           model == 'X1 + X3 + X4' | model == 'X1 + X3') %>% 
  mutate(model = factor(model)) 
model_sel_pn_tab$model = forcats::fct_relevel(model_sel_pn_tab$model, c('X1 + X2 + X3 + X4',
                                                                        'X1 + X3 + X4',
                                                                        'X1 + X2 + X3',
                                                                        'X1 + X3'))

# merging loo and bias for plotting ---------------------------------------
loo_bias_tab = 
  left_join(elpd_tab, model_sel_pn_tab, by = c('model', 'iteration', 'X4')) %>% 
  rename(pred_int_X5 = `5%`,
         pred_int_X50 = `50%`,
         pred_int_X95 = `95%`) %>% 
  arrange(X4)
# View(loo_bias_tab)

shp = c('1' = '7', '2' = '8', '3' = '9', '4' = '10',
        '5' = '11', '6' = '12', '7' = '13', '8' = '14',
        '9' = '15', '10' = '16', '11' = '17', '12' = '18')
## range of elpd values (weighted)
(p3 = ggplot(loo_bias_tab, aes(x = elpd_sae, y = mean_bias_abs, shape = X4, colour = model)) +
    geom_point(position = position_dodge(width = .5), alpha=0.7, size=2) +
    scale_colour_manual(values = c("#1C73B1FF", "#26456EFF",
                                   "#FB964EFF", "#DF5948FF",
                                   "#09622AFF",
                                   "#879195FF")) +
    scale_shape_manual(values = c(0:5,7,8,10,11,15:16)) +
    labs(title="SAE bias vs loo for each X4-level", 
         x = "SAE elpd values",
         y = "SAE absolute mean bias") )

# ggsave(here::here("02-super popn approach/experiment4_SAE/02-results/plot_loo_wtd_range_fx1.png"), p3, width=6, height=7.5, units="in", device="png")
