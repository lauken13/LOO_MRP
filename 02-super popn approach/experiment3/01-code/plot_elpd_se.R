## plotting loo values and its se
library(ggplot2)
library(reshape2)
library(dplyr)
library(magrittr)
library(forcats)


# unweighted loo ----------------------------------------------------------
elpd_06_tab = elpd_06_mat %>% 
  as_tibble() %>% 
  mutate(model = 'X1 + X3') %>% 
  rename('elpd_loo' = V1, 'SE' = V2)

elpd_11_tab = elpd_11_mat %>% 
  as_tibble() %>% 
  mutate(model = 'X1 + X2 + X3') %>% 
  rename('elpd_loo' = V1, 'SE' = V2)

elpd_13_tab = elpd_13_mat %>% 
  as_tibble() %>% 
  mutate(model = 'X1 + X3 + X4') %>% 
  rename('elpd_loo' = V1, 'SE' = V2)

elpd_13a_tab = elpd_13a_mat %>% 
  as_tibble() %>% 
  mutate(model = '*X1 + X3 + X4') %>% 
  rename('elpd_loo' = V1, 'SE' = V2)

elpd_15_tab = elpd_15_mat %>% 
  as_tibble() %>% 
  mutate(model = 'X1 + X2 + X3 + X4') %>% 
  rename('elpd_loo' = V1, 'SE' = V2)

elpd_15a_tab = elpd_15a_mat %>% 
  as_tibble() %>% 
  mutate(model = '*X1 + X2 + X3 + X4') %>% 
  rename('elpd_loo' = V1, 'SE' = V2)

elpd_all_tab = rbind(elpd_06_tab, elpd_11_tab, elpd_13_tab,
      elpd_13a_tab, elpd_15_tab, elpd_15a_tab) %>% 
  mutate(low_elpd = .$elpd_loo - (.$SE*1.64), # calculating upper and lower bound of the elpd values
         upp_elpd = .$elpd_loo + (.$SE*1.64),
         range_pe = upp_elpd - low_elpd,
         iter = rep(1:100, 6))

pu = elpd_all_tab %>% 
  mutate(model = factor(model))
           
pu$model = fct_relevel(pu$model, c('X1 + X2 + X3 + X4', '*X1 + X2 + X3 + X4',
                                   'X1 + X3 + X4', '*X1 + X3 + X4',
                                   'X1 + X2 + X3',
                                   'X1 + X3'))

## elpd values plot ####
xloc = -400
p1 = ggplot(pu, aes(x = elpd_loo, y = model, group = iter, colour = model))+
  geom_point(position = position_dodge(width = .5), alpha=0.7) +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  scale_y_discrete(limits = rev) +
  scale_colour_manual(values = c(paletteer::paletteer_c("ggthemes::Classic Blue", 2),
                                 paletteer::paletteer_c("ggthemes::Red-Gold", 2),
                                 paletteer::paletteer_c("ggthemes::Classic Green", 1),
                                 paletteer::paletteer_c("ggthemes::Gray", 3))) + 
  labs(title="Raw elpd values (unweighted)") +
  annotate("label", x = xloc, y = 5.5, label = "X2 and X4") +
  annotate("label", x = xloc, y = 3.5, label = "X4 only") +
  annotate("label", x = xloc, y = 2,  label = "X2 only") +
  annotate("label", x = xloc, y = 1, label = "None") +
  xlim(c(-590,-390)) 
  geom_errorbarh(mapping = aes(xmin = low_elpd,
                               xmax = upp_elpd),
                 position = position_dodge(width = .5),
                 height = 0, alpha = .5)
  

ggsave(here::here("02-super popn approach/experiment3/02-results/stan code/plot_loo_unwtd_fx1.png"), p1, width=6, height=7.5, units="in", device="png")

## range of elpd values 
xloc2 = 65
p1a = ggplot(pu, aes(x = range_pe, y = model, group = iter, colour = model)) +
  geom_point() +
  scale_colour_manual(values = c(paletteer::paletteer_c("ggthemes::Classic Blue", 2),
                                 paletteer::paletteer_c("ggthemes::Red-Gold", 2),
                                 paletteer::paletteer_c("ggthemes::Classic Green", 1),
                                 paletteer::paletteer_c("ggthemes::Gray", 3)))  +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  annotate("label", x = xloc2, y = 5.5, label = "X2 and X4") +
  annotate("label", x = xloc2, y = 3.5, label = "X4 only") +
  annotate("label", x = xloc2, y = 2,  label = "X2 only") +
  annotate("label", x = xloc2, y = 1, label = "None") +
  labs(title="Range of interval for elpd values (unweighted)") +
  xlim(c(52,66))

ggsave(here::here("02-super popn approach/experiment3/02-results/stan code/plot_loo_unwtd_range_fx1.png"), p1a, width=6, height=7.5, units="in", device="png")


# weighted loo ------------------------------------------------------------
# calculating weighted loo (not done in cluster so postprocessing)

loo_wtd_06_tab = loo_wtd_06_list %>% 
  do.call(rbind, .) %>% 
  mutate(model = 'model06',
         iter = 1:100)
loo_wtd_11_tab = loo_wtd_11_list %>% 
  do.call(rbind, .) %>% 
  mutate(model = 'model11',
         iter = 1:100)
loo_wtd_13_tab = loo_wtd_13_list %>% 
  do.call(rbind, .) %>% 
  mutate(model = 'model13',
         iter = 1:100)
loo_wtd_13a_tab = loo_wtd_13a_list %>% 
  do.call(rbind, .) %>% 
  mutate(model = 'model13a',
         iter = 1:100)
loo_wtd_15_tab = loo_wtd_15_list %>% 
  do.call(rbind, .) %>% 
  mutate(model = 'model15',
         iter = 1:100)
loo_wtd_15a_tab = loo_wtd_15a_list %>% 
  do.call(rbind, .) %>% 
  mutate(model = 'model15a',
         iter = 1:100)

wtd_elpd_se_tab = rbind(loo_wtd_06_tab, loo_wtd_11_tab, loo_wtd_13_tab,
                loo_wtd_13a_tab, loo_wtd_15_tab, loo_wtd_15a_tab)


ph = wtd_elpd_se_tab %>% 
  mutate(model = factor(model),
         low_elpd = .$wtd_elpd_loo - (.$wtd_SE*1.64), # calculating upper and lower bound of the elpd values
         upp_elpd = .$wtd_elpd_loo + (.$wtd_SE*1.64)) %>% 
  mutate(model = plyr::revalue(model, c('model06' = 'X1 + X3',
                                        'model11' = 'X1 + X2 + X3',  
                                        'model13' = 'X1 + X3 + X4', 
                                        'model13a' = '*X1 + X3 + X4', 
                                        'model15' = 'X1 + X2 + X3 + X4',
                                        'model15a' = '*X1 + X2 + X3 + X4')))
ph$model = fct_relevel(ph$model, c('X1 + X2 + X3 + X4', '*X1 + X2 + X3 + X4', 
                                   'X1 + X3 + X4', '*X1 + X3 + X4',
                                   'X1 + X2 + X3',
                                   'X1 + X3'))

## plot
xloc3 = -4000
p2 = ggplot(ph, aes(x = wtd_elpd_loo, y = model, group = iter, colour = model))+
  geom_point(position = position_dodge(width = .5), alpha=0.7) +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  scale_y_discrete(limits = rev) +
  scale_colour_manual(values = c(paletteer::paletteer_c("ggthemes::Classic Blue", 2),
                                 paletteer::paletteer_c("ggthemes::Red-Gold", 2),
                                 paletteer::paletteer_c("ggthemes::Classic Green", 1),
                                 paletteer::paletteer_c("ggthemes::Gray", 3))) + 
  labs(title="Raw elpd values (weighted)") +
  annotate("label", x = xloc3, y = 5.5, label = "X2 and X4") +
  annotate("label", x = xloc3, y = 3.5, label = "X4 only") +
  annotate("label", x = xloc3, y = 2, label = "X2 only") +
  annotate("label", x = xloc3, y = 1, label = "None") +
    xlim(-6000,-3900) 
   geom_errorbarh(mapping = aes(xmin = low_elpd,
                                xmax = upp_elpd),
                  position = position_dodge(width = .5),
                  height = 0, alpha = .5)


ggsave(here::here("02-super popn approach/experiment3/02-results/stan code/plot_loo_wtd_fx1.png"), p2, width=6, height=7.5, units="in", device="png")

