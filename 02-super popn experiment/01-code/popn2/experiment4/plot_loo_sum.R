## plotting lo(c)o values for all the models 
## 24/01/22
library(ggplot2)
library(reshape2)
library(dplyr)
library(magrittr)
library(tidyverse)

## plotting prob of truth
ite = c(1:94,96:100)
samp_data_list2 = samp_data_list[ite]
sim_out = sapply(samp_data_list2, function(x)mean(x$outcome))
prob_truth = pt_list[ite]

plot(1:length(ite), prob_truth, ylim=c(0.45,.7), pch=1)
points(1:length(ite), sim_out, pch=19)
legend(78, 0.58, pch=c(19,1), legend = c('sample', 'popn'))

plot(sim_out, prob_truth, xlim=c(0.47,0.68), ylim=c(0.47,0.68), pch=19)
abline(a=0, b=1)


# unweighted loo ----------------------------------------------------------
## extracting elpd_loo 
elpd_tab = sim_list1 %>% 
  lapply(., function(x)(x[,'elpd_loo'])) %>% 
  do.call(rbind, .) %>%   
  as.data.frame() %>% 
  set_colnames(c(paste0('model0',1:9), paste0('model',10:15)))

elpd_se_tab = sim_list1 %>% 
  lapply(., function(x)(x[,'SE'])) %>% 
  do.call(rbind, .) %>%   
  as.data.frame() %>% 
  set_colnames(c(paste0('model0',1:9), paste0('model',10:15)))

tmp1 = elpd_tab %>%
  gather(elpd_tab, "total", model01:model15) %>% 
  rename(model = elpd_tab)

tmp2 = elpd_se_tab %>% 
  gather(elpd_se_tab, "SE", model01:model15) %>% 
  rename(model = elpd_se_tab)

elpd_se_tab2 = cbind(tmp1, tmp2[,-1]) %>%  
  rename(SE = "tmp2[, -1]") %>% 
  mutate(low_elpd = .$total - (.$SE*1.64), # calculating upper and lower bound of the elpd values
         upp_elpd = .$total + (.$SE*1.64),
         iter = rep(1:length(ite), 15))

pu = elpd_se_tab2 %>% 
  mutate(model = factor(model)) %>% 
  mutate(model = plyr::revalue(model, c('model01' = 'X1',
                                        'model02' = 'X2',
                                        'model03' = 'X3',
                                        'model04' = 'X4',
                                        'model05' = 'X1 + X2',
                                        'model06' = 'X1 + X3',
                                        'model07' = 'X1 + X4',
                                        'model08' = 'X2 + X3', 
                                        'model09' = 'X2 + X4',
                                        'model10' = 'X3 + X4',
                                        'model11' = 'X1 + X2 + X3', 
                                        'model12' = 'X1 + X2 + X4', 
                                        'model13' = 'X1 + X3 + X4', 
                                        'model14' = 'X2 + X3 + X4', 
                                        'model15' = 'X1 + X2 + X3 + X4'))) 
pu$model = fct_relevel(pu$model, c('X2 + X4', 'X1 + X2 + X4', 'X2 + X3 + X4', 'X1 + X2 + X3 + X4', 
                                   'X4', 'X1 + X4', 'X3 + X4', 'X1 + X3 + X4',
                                   'X2', 'X1 + X2', 'X2 + X3', 'X1 + X2 + X3',
                                   'X1', 'X3', 'X1 + X3'))


## plotting all models 
g2 = ggplot(pu, aes(x = total, y = model, group = iter, colour = model))+
  geom_point(position = position_dodge(width = .5)) +
  geom_errorbarh(mapping = aes(xmin = low_elpd, 
                               xmax = upp_elpd), 
                 position = position_dodge(width = .5),
                 height = 0, alpha = .5) +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  scale_y_discrete(limits = rev) +
  scale_colour_manual(values = pals::tableau20(20)[c(1,2,9,10,3,4,7,8,13,14,5,6,17,18,12)]) + 
  labs(title="Raw elpd values (unweighted)") +
  annotate("label", x = -200, y = 13.5, label = "X2 and X4") +
  annotate("label", x = -60, y = 9.5, label = "X4 only") +
  annotate("label", x = -60, y = 5.5,  label = "X2 only") +
  annotate("label", x = -60, y = 2, label = "None") 

ggsave("plot_loo_raw_unwtd.png", g2, width=6, height=7.5, units="in", device="png")

