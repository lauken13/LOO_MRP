## postprocessing the saved objects from cluster
library(loo)
library(posterior) # to convert draws_array() objects 
library(dplyr)
library(ggplot2)

load(here::here("02-super popn approach/experiment4_SAE/03-data/loo_sae_fx3.Rbin"))

load(here::here("02-super popn approach/experiment4_SAE/03-data/X1X3.Rbin"))

# group/small area prediction and bias --------------------------------------------------------
X3_group_mean_list = list()

## getting the group prob for each iteration/population
for (ite in iter){
  X3_group_mean_list[[ite]] =  popn_data_list[[ite]] %>% 
    group_by(X3) %>% 
    summarise(mean_prob = mean(y_prob)) %>% 
    mutate(iteration = ite)
}


# true group mean
X3_group_mean_tab = do.call(rbind, X3_group_mean_list)

## calculating the popnest for each level of X4 for all six of the models
popnest_sae_X3_tab =
  popnest_sae_X3_model = 
  popnest_sae_X3 = list()

for(ite in iter){
  for(m in 1:6){ # getting quantiles for each of the model
    popnest_sae_X3_model[[m]] = popnest_sae_X3_all[[ite]][m] %>%
      as.data.frame() %>% 
      apply(., 2, quantile, c(0.05, 0.5, 0.95))
  }
  popnest_sae_X3[[ite]] = popnest_sae_X3_model
} 


# extracting every 1st/2nd ... 6th list of the main list
popnest_sae_X3_m6 = sapply(popnest_sae_X3, '[', 1) %>% 
  lapply(., function(x)t(x))
popnest_sae_X3_m11 = sapply(popnest_sae_X3, '[', 2) %>% 
  lapply(., function(x)t(x))
popnest_sae_X3_m13 = sapply(popnest_sae_X3, '[', 3) %>% 
  lapply(., function(x)t(x))
popnest_sae_X3_m13a = sapply(popnest_sae_X3, '[', 4) %>% 
  lapply(., function(x)t(x))
popnest_sae_X3_m15 = sapply(popnest_sae_X3, '[', 5) %>% 
  lapply(., function(x)t(x))
popnest_sae_X3_m15a = sapply(popnest_sae_X3, '[', 6) %>% 
  lapply(., function(x)t(x))

calc_list_m6 =
  calc_list_m11 = 
  calc_list_m13 = 
  calc_list_m13a = 
  calc_list_m15 = 
  calc_list_m15a = list()

for(i in iter){
  # truth value for each iteration
  t1 = X3_group_mean_tab %>% 
    filter(iteration == i) 
  
  # combining truth and quantile of each iteration
  calc_tab_m6 = cbind(popnest_sae_X3_m6[[i]], t1)
  calc_tab_m11 = cbind(popnest_sae_X3_m11[[i]], t1)
  calc_tab_m13 = cbind(popnest_sae_X3_m13[[i]], t1)
  calc_tab_m13a = cbind(popnest_sae_X3_m13a[[i]], t1)
  calc_tab_m15 = cbind(popnest_sae_X3_m15[[i]], t1)
  calc_tab_m15a = cbind(popnest_sae_X3_m15a[[i]], t1)
  
  ## comparing to 'truth'
  calc_tab2_m6 = calc_tab_m6 %>% 
    mutate(mean_pe = (as.numeric(`50%`) - as.numeric(mean_prob)),
           low_pe = (as.numeric(`5%`) - as.numeric(mean_prob)),
           upp_pe = (as.numeric(`95%`) - as.numeric(mean_prob)),
           coverage = ifelse(mean_prob >= `5%` & mean_prob <= `95%`, 1, 0))
  
  calc_tab2_m11 = calc_tab_m11 %>% 
    mutate(mean_pe = (as.numeric(`50%`) - as.numeric(mean_prob)),
           low_pe = (as.numeric(`5%`) - as.numeric(mean_prob)),
           upp_pe = (as.numeric(`95%`) - as.numeric(mean_prob)),
           coverage = ifelse(mean_prob >= `5%` & mean_prob <= `95%`, 1, 0))
  
  calc_tab2_m13 = calc_tab_m13 %>% 
    mutate(mean_pe = (as.numeric(`50%`) - as.numeric(mean_prob)),
           low_pe = (as.numeric(`5%`) - as.numeric(mean_prob)),
           upp_pe = (as.numeric(`95%`) - as.numeric(mean_prob)),
           coverage = ifelse(mean_prob >= `5%` & mean_prob <= `95%`, 1, 0))
  
  calc_tab2_m13a = calc_tab_m13a %>% 
    mutate(mean_pe = (as.numeric(`50%`) - as.numeric(mean_prob)),
           low_pe = (as.numeric(`5%`) - as.numeric(mean_prob)),
           upp_pe = (as.numeric(`95%`) - as.numeric(mean_prob)),
           coverage = ifelse(mean_prob >= `5%` & mean_prob <= `95%`, 1, 0))
  
  calc_tab2_m15 = calc_tab_m15 %>% 
    mutate(mean_pe = (as.numeric(`50%`) - as.numeric(mean_prob)),
           low_pe = (as.numeric(`5%`) - as.numeric(mean_prob)),
           upp_pe = (as.numeric(`95%`) - as.numeric(mean_prob)),
           coverage = ifelse(mean_prob >= `5%` & mean_prob <= `95%`, 1, 0))
  
  calc_tab2_m15a= calc_tab_m15a %>% 
    mutate(mean_pe = (as.numeric(`50%`) - as.numeric(mean_prob)),
           low_pe = (as.numeric(`5%`) - as.numeric(mean_prob)),
           upp_pe = (as.numeric(`95%`) - as.numeric(mean_prob)),
           coverage = ifelse(mean_prob >= `5%` & mean_prob <= `95%`, 1, 0))
  
  calc_list_m6[[i]] = calc_tab2_m6
  calc_list_m11[[i]] = calc_tab2_m11
  calc_list_m13[[i]] = calc_tab2_m13
  calc_list_m13a[[i]] = calc_tab2_m13a
  calc_list_m15[[i]] = calc_tab2_m15
  calc_list_m15a[[i]] = calc_tab2_m15a
} 

# combining all of the iterations
sae_X3_tab_m6 = do.call(rbind, calc_list_m6) %>% 
  mutate(model = "X1 + X3")

sae_X3_tab_m11 = do.call(rbind, calc_list_m11) %>% 
  mutate(model = "X1 + X2 + X3")

sae_X3_tab_m13 = do.call(rbind, calc_list_m13) %>% 
  mutate(model = "X1 + X3 + X4")

sae_X3_tab_m13a = do.call(rbind, calc_list_m13a) %>% 
  mutate(model = "*X1 + X3 + X4")

sae_X3_tab_m15 = do.call(rbind, calc_list_m15) %>% 
  mutate(model = "X1 + X2 + X3 + X4")

sae_X3_tab_m15a = do.call(rbind, calc_list_m15a) %>% 
  mutate(model = "*X1 + X2 + X3 + X4")

model_all_pn_tab = rbind(sae_X3_tab_m6, sae_X3_tab_m11, sae_X3_tab_m13,
                         sae_X3_tab_m13a, sae_X3_tab_m15, sae_X3_tab_m15a)

## looking at specific models only
model_sel_pn_tab = model_all_pn_tab %>% 
  filter(model == 'X1 + X2 + X3 + X4' | model == 'X1 + X2 + X3' |
           model == 'X1 + X3 + X4' | model == 'X1 + X3') %>% 
  mutate(model = factor(model)) 
model_sel_pn_tab$model = forcats::fct_relevel(model_sel_pn_tab$model, c('X1 + X2 + X3 + X4',
                                                                        'X1 + X3 + X4',
                                                                        'X1 + X2 + X3',
                                                                        'X1 + X3'))
## plot diff in mean
xloc4 = 0.45
(g3 = ggplot(model_sel_pn_tab, aes(x = abs(mean_pe), y = X3, colour = model)) +
    geom_vline(aes(xintercept = 0)) +
    geom_point(position = position_dodge(width = 0.4), alpha=0.7) +
    theme(legend.position = c(0.85,0.8)) +
    scale_colour_manual(values = c("#1C73B1FF", 
                                   "#FB964EFF",
                                   "#09622AFF",
                                   "#879195FF")) + 
    stat_summary(aes(group=model), width=0.7, size=0.4,
                 position = position_dodge(width = .5),
                 fun=mean, geom="crossbar", colour=rep(c("#1C73B1FF",
                                                         "#FB964EFF",
                                                         "#09622AFF",
                                                         "#879195FF"), each = 5), alpha=0.8) + #drawing the mean line
    labs(title="Difference in X3-levels estimate and truth",
         y = 'Levels of X3', x = 'Absolute bias' ))

ggsave(here::here("02-super popn approach/experiment4_SAE/02-results/bias_sae_X3_fx3.png"), g3, width=6, height=7.5, units="in", device="png")

