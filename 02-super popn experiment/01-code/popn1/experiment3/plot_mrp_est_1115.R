## plotting the MRP estimates against 'truth'
library(ggplot2)
library(reshape2)
library(dplyr)
library(magrittr)
library(survey)

load("~/GitHub/LOO_MRP/02-super popn experiment/03-results/experiment3/simulated29temp_3.RData")

sim_list2 = sim_trip_list[1:29]
samp_data = samp_data_list[1:29]
pt = prob_truth[1:29]

sim_list2[[1]]$popnestX50 - prob_truth[1]


## extracting elpd_loo and calculating difference with model15
popnest_tab = sim_list2 %>% 
  lapply(., function(x)(x[,'popnestX50'])) %>% 
  do.call(rbind, .) %>%   
  as.data.frame() %>% 
  set_colnames(c(paste0('popnest',11:15))) 

## lower bound
popnest_low_tab = sim_list2 %>% 
  lapply(., function(x)(x[,'popnestX5'])) %>% 
  do.call(rbind, .) %>%   
  set_colnames(c(paste0('model',11:15)))

## upper bound
popnest_upp_tab = sim_list2 %>% 
  lapply(., function(x)(x[,'popnestX95'])) %>% 
  do.call(rbind, .) %>% 
  set_colnames(c(paste0('model',11:15))) 

## comparing to 'truth'
popnest_diff = matrix(NA, nrow = 29, ncol = 5)
popnest_low_diff = matrix(NA, nrow = 29, ncol = 5)
popnest_upp_diff = matrix(NA, nrow = 29, ncol = 5)
for(i in 1:29){
  popnest_diff[i,] = as.numeric(popnest_tab[i,] - pt[i])
  popnest_low_diff[i,] = as.numeric(popnest_low_tab[i,] - pt[i])
  popnest_upp_diff[i,] = as.numeric(popnest_upp_tab[i,] - pt[i])
}

popnest_mean = popnest_diff %>% 
  set_colnames(c(paste0('model',11:15))) %>% 
  melt(.) %>% 
  rename(model = Var2, iter = Var1, pe_mean = value)

popnest_low = popnest_low_diff %>% 
  set_colnames(c(paste0('model',11:15))) %>% 
  melt(.) %>% 
  select(.,-Var1, -Var2) %>% 
  rename(low_pe = value)

popnest_upp = popnest_upp_diff %>% 
  set_colnames(c(paste0('model',11:15))) %>% 
  melt(.) %>% 
  select(.,-Var1, -Var2) %>% 
  rename(upp_pe = value)

popnest_comb = cbind(popnest_mean, popnest_low, popnest_upp)


## plot
ggplot(popnest_comb, aes(x = pe_mean, y = model, group = iter, colour = model))+
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

annotate("label", x = -1000, y = 0.8, label = "Model 15 preferred") +
  annotate("label", x = 1000, y = 0.8, label = "Alt model preferred") 


