## plotting the MRP estimates against 'truth'
library(ggplot2)
library(reshape2)
library(dplyr)
library(magrittr)
library(survey)

load("~/GitHub/LOO_MRP/02-super popn experiment/03-results/experiment3e/simulated100_2.RData")

sim_list2 = sim_doub_list
samp_data = samp_data_list
pt = prob_truth

sim_list2[[1]]$popnestX50 - prob_truth[1]


## extracting popn. estimates and upper and lower bound
popnest_tab = sim_list2 %>% 
  lapply(., function(x)(x[,'popnestX50'])) %>% 
  do.call(rbind, .) %>%   
  as.data.frame() %>% 
  select(.,-last_col()) %>% 
  set_colnames(c(paste0('popnest0',5:9), 'popnest10')) 

## lower bound
popnest_low_tab = sim_list2 %>% 
  lapply(., function(x)(x[,'popnestX5'])) %>% 
  do.call(rbind, .) %>% 
  as.data.frame() %>% 
  select(.,-last_col()) %>% 
  set_colnames(c(paste0('popnest0',5:9), 'popnest10')) 

## upper bound
popnest_upp_tab = sim_list2 %>% 
  lapply(., function(x)(x[,'popnestX95'])) %>% 
  do.call(rbind, .) %>% 
  as.data.frame() %>% 
  select(.,-last_col()) %>% 
  set_colnames(c(paste0('popnest0',5:9), 'popnest10')) 

## comparing to 'truth'
len = length(samp_data)
wid = ncol(popnest_tab)
popnest_diff = matrix(NA, nrow = len, ncol = wid)
popnest_low_diff = matrix(NA, nrow = len, ncol = wid)
popnest_upp_diff = matrix(NA, nrow = len, ncol = wid)
for(i in 1:len){
  popnest_diff[i,] = as.numeric(popnest_tab[i,] - pt[i])
  popnest_low_diff[i,] = as.numeric(popnest_low_tab[i,] - pt[i])
  popnest_upp_diff[i,] = as.numeric(popnest_upp_tab[i,] - pt[i])
}

popnest_mean = popnest_diff %>% 
  set_colnames(c(paste0('model0',5:9), 'model10')) %>% 
  melt(.) %>% 
  rename( iter = Var1, model = Var2, mean_pe = value)

popnest_low = popnest_low_diff %>% 
  set_colnames(c(paste0('model0',5:9), 'model10')) %>% 
  melt(.) %>% 
  select(.,-Var1, -Var2) %>% 
  rename(low_pe = value)

popnest_upp = popnest_upp_diff %>% 
  set_colnames(c(paste0('model0',5:9), 'model10')) %>% 
  melt(.) %>% 
  select(.,-Var1, -Var2) %>% 
  rename(upp_pe = value)

popnest_comb = cbind(popnest_mean, popnest_low, popnest_upp) %>% 
  mutate(range_pe = upp_pe - low_pe)

pc2 = popnest_comb

## plot
ggplot(pc2, aes(x = mean_pe, y = model, group = iter, colour = model))+
  geom_vline(aes(xintercept = 0)) +
  geom_point(position = position_dodge(width = .5)) +
  xlim(c(-0.25, 0.25)) +
  geom_errorbarh(mapping = aes(xmin = low_pe, 
                               xmax = upp_pe), 
                 position = position_dodge(width = .5),
                 height = 0, alpha = .7) +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  scale_y_discrete(limits = rev) +
  scale_colour_manual(values = pals::tableau20(20)[c(1,2,9,10,3,4,7,8,13,14,5,6,17,18)]) + 
  labs(title="Difference in MRP estimate and truth") 

## plotting mean as violin 
ggplot(pc2, aes(group = model, fill = model))+
  geom_vline(aes(xintercept = 0)) +
  geom_violin(aes(x = low_pe, y = model),alpha=0.3) +
  geom_violin(aes(x = upp_pe, y = model), alpha=0.3) +
  geom_violin(aes(x = mean_pe, y = model), alpha=1) +
  xlim(c(-0.25, 0.25)) +
  scale_y_discrete(limits = rev) +
  scale_fill_manual(values = pals::tableau20(20)[c(1,2,9,10,3,4,7,8,13,14,5,6,17,18)]) 


## plotting range
ggplot(pc2, aes(x = range_pe, y = model, group = model, fill = model))+
  geom_vline(aes(xintercept = 0)) +
  geom_violin() +
  xlim(c(-0.05, 0.1)) +
  scale_y_discrete(limits = rev) +
  scale_fill_manual(values = pals::tableau20(20)[c(1,2,9,10,3,4,7,8,13,14,5,6,17,18)]) +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  labs(title="Difference in MRP estimate and truth") 







