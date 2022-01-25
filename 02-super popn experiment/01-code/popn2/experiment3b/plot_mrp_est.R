## plotting the MRP estimates against 'truth'
library(ggplot2)
library(reshape2)
library(dplyr)
library(magrittr)
library(survey)
library(forcats)

samp_data_list[[1]][,c('elpd_1', 'X1')]

## extracting popn. estimates and upper and lower bound
popnest_tab = sim_list1[c(1:64,66:76,78:100)] %>% 
  lapply(., function(x)(x[,c('popnestX5','popnestX50', 'popnestX95')])) %>% 
  lapply(., function(x) cbind(x, model=c(paste0('model0',1:9), paste0('model',10:15)))) %>% 
  do.call(rbind, .) %>%
  mutate(iter = rep(1:98, each=15)) %>% 
  arrange(., model) %>% 
  set_rownames(., 1:(98*15)) 
  
rep(pt_list, 15)
## comparing to 'truth'
popnest_tab$mean_pe = as.numeric(popnest_tab[,2] - rep(pt_list[c(1:64,66:76,78:100)], 15)) # recursive diff for pt_list
popnest_tab$low_pe = as.numeric(popnest_tab[,1]  - rep(pt_list[c(1:64,66:76,78:100)], 15))
popnest_tab$upp_pe = as.numeric(popnest_tab[,3]  - rep(pt_list[c(1:64,66:76,78:100)], 15))

pc = popnest_tab %>% 
  mutate(range_pe = popnestX95 - popnestX5) %>% 
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
pc$model = fct_relevel(pc$model, c('X2 + X4', 'X1 + X2 + X4', 'X2 + X3 + X4', 'X1 + X2 + X3 + X4', 
                                   'X4', 'X1 + X4', 'X3 + X4', 'X1 + X3 + X4',
                                   'X2', 'X1 + X2', 'X2 + X3', 'X1 + X2 + X3',
                                   'X1', 'X3', 'X1 + X3'))

## plot diff in mean
g1 = ggplot(pc, aes(x = mean_pe, y = model, group = iter, colour = model))+
  geom_vline(aes(xintercept = 0)) +
  geom_point(position = position_dodge(width = .5)) +
  xlim(c(-0.06, 0.25)) +
  geom_errorbarh(mapping = aes(xmin = low_pe, 
                               xmax = upp_pe), 
                 position = position_dodge(width = .5),
                 height = 0, alpha = .7) +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  scale_y_discrete(limits = rev) +
  scale_colour_manual(values = pals::tableau20(20)[c(1,2,9,10,3,4,7,8,13,14,5,6,17,18,12)]) + 
  labs(title="Difference in MRP estimate and truth") +
  annotate("label", x = 0.23, y = 13.5, label = "X2 and X4") +
  annotate("label", x = 0.23, y = 9.5, label = "X4 only") +
  annotate("label", x = 0.23, y = 5.5,  label = "X2 only") +
  annotate("label", x = 0.23, y = 2, label = "None") 

ggsave("plot_mrp_truth.png", g1, width=6, height=7.5, units="in", device="png")

## plotting variance range
g4 = ggplot(pc, aes(x = range_pe, y = model, group = model, fill = model)) +
  geom_violin() +
  #xlim(c(0.04, 0.1)) +
  scale_y_discrete(limits = rev) +
  scale_fill_manual(values = pals::tableau20(20)[c(1,2,9,10,3,4,7,8,13,14,5,6,17,18,12)]) +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  labs(title="Quantile range for MRP estimate ") +
  annotate("label", x = 0.09, y = 13.5, label = "X2 and X4") +
  annotate("label", x = 0.09, y = 9.5, label = "X4 only") +
  annotate("label", x = 0.09, y = 5.5,  label = "X2 only") +
  annotate("label", x = 0.09, y = 2, label = "None")


ggsave("plot_mrp_qt_range.png", g4, width=6, height=7.5, units="in", device="png")

