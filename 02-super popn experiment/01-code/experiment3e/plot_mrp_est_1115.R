## plotting the MRP estimates against 'truth'
library(ggplot2)
library(reshape2)
library(dplyr)
library(magrittr)
library(survey)

load("~/GitHub/LOO_MRP/02-super popn experiment/03-results/experiment3e/simulated100_3.RData")

sim_list2 = sim_trip_list
samp_data = samp_data_list
pt = prob_truth

sim_list2[[1]]$popnestX50 - prob_truth[1]

## extracting popn. estimates and upper and lower bound
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
  set_colnames(c(paste0('model',11:15))) %>% 
  melt(.) %>% 
  rename(model = Var2, iter = Var1, mean_pe = value)

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

popnest_comb = cbind(popnest_mean, popnest_low, popnest_upp) %>% 
  mutate(range_pe = upp_pe - low_pe)

## combining with other models
pc3 = popnest_comb 
pc = bind_rows(pc1,pc2,pc3)

## plot
ggplot(popnest_comb, aes(x = mean_pe, y = model, group = iter, colour = model))+
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


## plot
g1 = ggplot(pc, aes(x = mean_pe, y = model, group = iter, colour = model))+
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
  scale_colour_manual(values = pals::tableau20(20)[c(1,2,9,10,3,4,7,8,13,14,5,6,17,18,20)]) + 
  labs(title="Difference in MRP estimate and truth") 

#ggsave("plot_mrp_diff.pdf", g1, width=6, height=7.5, units="in", device="pdf")

## plotting mean as violin 
g2 = ggplot(pc, aes(group = model, fill = model))+
  geom_vline(aes(xintercept = 0)) +
  geom_violin(aes(x = low_pe, y = model),alpha=0.3) +
  geom_violin(aes(x = upp_pe, y = model), alpha=0.3) +
  geom_violin(aes(x = mean_pe, y = model), alpha=1) +
  xlim(c(-0.25, 0.25)) +
  scale_y_discrete(limits = rev) +
  scale_fill_manual(values = pals::tableau20(20)[c(1,2,9,10,3,4,7,8,13,14,5,6,17,18,12)]) +
  theme(legend.position = "none",
      axis.title = element_blank()) +
  labs(title="Difference in MRP estimate and truth (mean and 90% quantile)") 

ggsave("plot_mrp_diff_qt.pdf", g2, width=6, height=7.5, units="in", device="pdf")

g3 = ggplot(pc, aes(group = model, fill = model))+
  geom_vline(aes(xintercept = 0)) +
  geom_violin(aes(x = mean_pe, y = model), alpha=1) +
  xlim(c(-0.25, 0.25)) +
  scale_y_discrete(limits = rev) +
  scale_fill_manual(values = pals::tableau20(20)[c(1,2,9,10,3,4,7,8,13,14,5,6,17,18,12)]) +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  labs(title="Difference in MRP estimate and truth (mean)") 

ggsave("plot_mrp_diff_mean.pdf", g3, width=6, height=7.5, units="in", device="pdf")

## plotting range
g4 = ggplot(pc, aes(x = range_pe, y = model, group = model, fill = model))+
  geom_vline(aes(xintercept = 0)) +
  geom_violin() +
  xlim(c(-0.01, 0.12)) +
  scale_y_discrete(limits = rev) +
  scale_fill_manual(values = pals::tableau20(20)[c(1,2,9,10,3,4,7,8,13,14,5,6,17,18,12)]) +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  labs(title="Difference in MRP estimate and truth (quantile range)") 

ggsave("plot_mrp_diff_qt_range.pdf", g4, width=6, height=7.5, units="in", device="pdf")

