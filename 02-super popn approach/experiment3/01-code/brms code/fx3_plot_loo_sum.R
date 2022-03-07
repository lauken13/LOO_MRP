ite = c(1:100)

# weighted elpd -----------------------------------------------------------
## extracting wtd_elpd_loo (sum)
elpd_wtd_tab = sim_list1 %>% 
  lapply(., function(x)(x[,'wtd_elpd_loo'])) %>% 
  do.call(rbind, .) %>%   
  as.data.frame() %>% 
  set_colnames(c(paste0('model0',1:9), paste0('model',10:15),
                 paste0('model', c(4,7,9,10,12:15), 'a')))

elpd_wtd_se_tab = sim_list1 %>% 
  lapply(., function(x)(x[,'wtd_SE'])) %>% 
  do.call(rbind, .) %>%   
  as.data.frame() %>% 
  set_colnames(c(paste0('model0',1:9), paste0('model',10:15),
                 paste0('model', c(4,7,9,10,12:15), 'a')))

tmp1 = elpd_wtd_tab %>%
  gather(elpd_wtd_tab, "total", model01:model15a) %>% 
  rename(model = elpd_wtd_tab)

tmp2 = elpd_wtd_se_tab %>% 
  gather(elpd_wtd_se_tab, "SE", model01:model15a) %>% 
  rename(model = elpd_wtd_se_tab)

wtd_elpd_se_tab = cbind(tmp1, tmp2[,-1]) %>%  
  rename(SE = "tmp2[, -1]") %>% 
  mutate(low_elpd = .$total - (.$SE*1.64), # calculating upper and lower bound of the elpd values
         upp_elpd = .$total + (.$SE*1.64),
         iter = rep(ite, 23))

## renaming 
ph = wtd_elpd_se_tab %>% 
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
                                        'model15' = 'X1 + X2 + X3 + X4',
                                        'model10a' = '*X3 + X4',
                                        'model12a' = '*X1 + X2 + X4', 
                                        'model13a' = '*X1 + X3 + X4',
                                        'model14a' = '*X2 + X3 + X4',
                                        'model15a' = '*X1 + X2 + X3 + X4',
                                        'model4a' = '*X4',
                                        'model7a' = '*X1 + X4',
                                        'model9a' = '*X2 + X4')))
ph$model = fct_relevel(ph$model, c('X2 + X4', '*X2 + X4', 'X1 + X2 + X4', '*X1 + X2 + X4', 'X2 + X3 + X4',
                                   '*X2 + X3 + X4', 'X1 + X2 + X3 + X4', '*X1 + X2 + X3 + X4', 
                                   'X4', '*X4', 'X1 + X4', '*X1 + X4', 'X3 + X4',  '*X3 + X4',
                                   'X1 + X3 + X4', '*X1 + X3 + X4', 'X2', 'X1 + X2', 'X2 + X3', 
                                   'X1 + X2 + X3', 'X1', 'X3', 'X1 + X3'))

# colours 
pal.bands(alphabet2(26))

#Example plot
g = ggplot(ph, aes(x = total, y = model, group = iter, colour = model))+
  geom_point(position = position_dodge(width = .5)) +
  geom_errorbarh(mapping = aes(xmin = low_elpd, 
                               xmax = upp_elpd), 
                 position = position_dodge(width = .5),
                 height = 0, alpha = .5) +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  scale_y_discrete(limits = rev) +
  scale_colour_manual(values = c(paletteer_c("ggthemes::Classic Blue", 8),
                                 paletteer_c("ggthemes::Red-Gold", 8),
                                 paletteer_c("ggthemes::Classic Green", 4),
                                 paletteer_c("ggthemes::Gray", 3))) + 
  labs(title="Raw elpd values (weighted)") +
  xlim(-7800,-3000) + 
  annotate("label", x = -3400, y = 19.5, label = "X2 and X4") +
  annotate("label", x = -3400, y = 11.5, label = "X4 only") +
  annotate("label", x = -3400, y = 6, label = "X2 only") +
  annotate("label", x = -3400, y = 2, label = "None") 

setwd('/Users/skuh0001/GitHub/LOO_MRP/02-super popn experiment/01-code/popn2/experiment6c3')
ggsave("plot_loo_wtd.png", g, width=6, height=7.5, units="in", device="png")

library(pals)
pal.bands(alphabet, alphabet2, cols25, glasbey, kelly, polychrome, 
          stepped, tol, watlington,
          show.names=FALSE)

# unweighted loo (with new models) ----------------------------------------------------------
## extracting elpd_loo 
elpd_tab = sim_list1 %>% 
  lapply(., function(x)(x[,'elpd_loo'])) %>% 
  do.call(rbind, .) %>%   
  as.data.frame() %>% 
  set_colnames(c(paste0('model0',1:9), paste0('model',10:15),
                 paste0('model', c(4,7,9,10,12:15), 'a')))

elpd_se_tab = sim_list1 %>% 
  lapply(., function(x)(x[,'SE'])) %>% 
  do.call(rbind, .) %>%   
  as.data.frame() %>% 
  set_colnames(c(paste0('model0',1:9), paste0('model',10:15),
                 paste0('model', c(4,7,9,10,12:15), 'a')))

tmp1 = elpd_tab %>%
  gather(elpd_tab, "total", model01:model15a) %>% 
  rename(model = elpd_tab)

tmp2 = elpd_se_tab %>% 
  gather(elpd_se_tab, "SE", model01:model15a) %>% 
  rename(model = elpd_se_tab)

elpd_se_tab2 = cbind(tmp1, tmp2[,-1]) %>%  
  rename(SE = "tmp2[, -1]") %>% 
  mutate(low_elpd = .$total - (.$SE*1.64), # calculating upper and lower bound of the elpd values
         upp_elpd = .$total + (.$SE*1.64),
         iter = rep(ite, 23))

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
                                        'model15' = 'X1 + X2 + X3 + X4',
                                        'model10a' = '*X3 + X4',
                                        'model12a' = '*X1 + X2 + X4', 
                                        'model13a' = '*X1 + X3 + X4',
                                        'model14a' = '*X2 + X3 + X4',
                                        'model15a' = '*X1 + X2 + X3 + X4',
                                        'model4a' = '*X4',
                                        'model7a' = '*X1 + X4',
                                        'model9a' = '*X2 + X4'))) 
pu$model = fct_relevel(pu$model, c('X2 + X4', '*X2 + X4', 'X1 + X2 + X4', '*X1 + X2 + X4', 'X2 + X3 + X4',
                                   '*X2 + X3 + X4', 'X1 + X2 + X3 + X4', '*X1 + X2 + X3 + X4', 
                                   'X4', '*X4', 'X1 + X4', '*X1 + X4', 'X3 + X4',  '*X3 + X4',
                                   'X1 + X3 + X4', '*X1 + X3 + X4', 'X2', 'X1 + X2', 'X2 + X3', 
                                   'X1 + X2 + X3', 'X1', 'X3', 'X1 + X3'))


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
  xlim(-360,-150)+
  scale_colour_manual(values = c(paletteer_c("ggthemes::Classic Blue", 8),
                                 paletteer_c("ggthemes::Red-Gold", 8),
                                 paletteer_c("ggthemes::Classic Green", 4),
                                 paletteer_c("ggthemes::Gray", 3))) + 
  labs(title="Raw elpd values (unweighted)") +
  annotate("label", x = -165, y = 19.5, label = "X2 and X4") +
  annotate("label", x = -165, y = 11.5, label = "X4 only") +
  annotate("label", x = -165, y = 6,  label = "X2 only") +
  annotate("label", x = -165, y = 2, label = "None") 

ggsave("plot_loo_unwtd.png", g2, width=6, height=7.5, units="in", device="png")




