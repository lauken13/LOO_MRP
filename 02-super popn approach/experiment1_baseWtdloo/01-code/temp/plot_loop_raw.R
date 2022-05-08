## plotting loop values for all the models 
## 13/12/2021
library(ggplot2)
library(reshape2)
library(dplyr)
library(magrittr)

## extracting loop 
loop_se_tab = sim_list1[c(1:64,66:76,78:100)] %>% 
  lapply(., function(x)(x[,c('elpd.popnestX5', 'elpd.popnestX50', 'elpd.popnestX95')])) %>% 
  lapply(., function(x) cbind(x, model=c(paste0('model0',1:9), paste0('model',10:15)))) %>% 
   do.call(rbind, .) %>%
  mutate(iter = rep(1:98, each=15)) %>% 
  arrange(., model) %>% 
  set_rownames(., 1:(98*15)) %>% 
  dplyr::rename(LOOP.X5 = elpd.popnestX5, 
                LOOP.X50 = elpd.popnestX50,
                LOOP.X95 = elpd.popnestX95)
 

loop_tab = loop_se_tab %>% 
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
loop_tab$model = fct_relevel(loop_tab$model, c('X2 + X4', 'X1 + X2 + X4', 'X2 + X3 + X4', 'X1 + X2 + X3 + X4', 
                                               'X4', 'X1 + X4', 'X3 + X4', 'X1 + X3 + X4',
                                               'X2', 'X1 + X2', 'X2 + X3', 'X1 + X2 + X3',
                                               'X1', 'X3', 'X1 + X3'))

## plot all models together
g1 = ggplot(loop_tab, aes(x = LOOP.X50, y = model, group = iter, colour = model)) +
  geom_point(position = position_dodge(width = .5)) +
  geom_errorbarh(mapping = aes(xmin = LOOP.X5, 
                               xmax = LOOP.X95), 
                 position = position_dodge(width = .5),
                 height = 0, alpha = .7) +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  scale_y_discrete(limits = rev) +
  scale_colour_manual(values = pals::tableau20(20)[c(1,2,9,10,3,4,7,8,13,14,5,6,17,18,12)]) + 
  labs(title="Raw LOOP values") +
  annotate("label", x = -7500, y = 13.5, label = "X2 and X4") +
  annotate("label", x = -3100, y = 9.5, label = "X4 only") +
  annotate("label", x = -3100, y = 5.5,  label = "X2 only") +
  annotate("label", x = -3100, y = 2, label = "None") 

ggsave("plot_loop.png", g1, width=6, height=7.5, units="in", device="png")

