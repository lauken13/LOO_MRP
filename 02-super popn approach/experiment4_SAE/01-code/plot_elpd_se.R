## plotting loo values and its se
library(ggplot2)
library(reshape2)
library(dplyr)
library(magrittr)
library(forcats)

# unweighted loo ----------------------------------------------------------
elpd_06_tab = elpd_06_mat[iter,] %>% 
  as_tibble() %>% 
  mutate(model = 'X1 + X3') %>% 
  rename('elpd_loo' = V1, 'SE' = V2)

elpd_11_tab = elpd_11_mat[iter,] %>% 
  as_tibble() %>% 
  mutate(model = 'X1 + X2 + X3') %>% 
  rename('elpd_loo' = V1, 'SE' = V2)

elpd_13_tab = elpd_13_mat[iter,] %>% 
  as_tibble() %>% 
  mutate(model = 'X1 + X3 + X4') %>% 
  rename('elpd_loo' = V1, 'SE' = V2)

elpd_13a_tab = elpd_13a_mat[iter,] %>% 
  as_tibble() %>% 
  mutate(model = '*X1 + X3 + X4') %>% 
  rename('elpd_loo' = V1, 'SE' = V2)

elpd_15_tab = elpd_15_mat[iter,] %>% 
  as_tibble() %>% 
  mutate(model = 'X1 + X2 + X3 + X4') %>% 
  rename('elpd_loo' = V1, 'SE' = V2)

elpd_15a_tab = elpd_15a_mat[iter,] %>% 
  as_tibble() %>% 
  mutate(model = '*X1 + X2 + X3 + X4') %>% 
  rename('elpd_loo' = V1, 'SE' = V2)

elpd_all_tab = rbind(elpd_06_tab, elpd_11_tab, elpd_13_tab,
                     elpd_13a_tab, elpd_15_tab, elpd_15a_tab) %>% 
  mutate(low_elpd = .$elpd_loo - (.$SE*1.64), # calculating upper and lower bound of the elpd values
         upp_elpd = .$elpd_loo + (.$SE*1.64),
         range_pe = upp_elpd - low_elpd,
         iter = rep(1:length(iter), 6))

pu = elpd_all_tab %>% 
  mutate(model = factor(model))

pu$model = fct_relevel(pu$model, c('X1 + X2 + X3 + X4', '*X1 + X2 + X3 + X4',
                                   'X1 + X3 + X4', '*X1 + X3 + X4',
                                   'X1 + X2 + X3',
                                   'X1 + X3'))


# elpd values plot --------------------------------------------------------
xloc = -280

(p1 = ggplot(data=pu, aes(x = elpd_loo, y = model, group = iter, colour = model),)+
    geom_point(position = position_dodge(width = .5), alpha=0.7) +
    theme(legend.position = "none",
          axis.title = element_blank()) +
    scale_y_discrete(limits = rev) +
    scale_colour_manual(values = c("#1C73B1FF", "#26456EFF",
                                   "#FB964EFF", "#DF5948FF",
                                   "#09622AFF",
                                   "#879195FF")) + 
    stat_summary(aes(group=model), width=0.5, size=.2, fun=mean, geom="crossbar", colour="black") + #drawing the mean line
    labs(title="Raw elpd values (unweighted)") +
    annotate("label", x = -340, y = 5.5, label = "X2 and X4") +
    annotate("label", x = xloc, y = 3.5, label = "X4 only") +
    annotate("label", x = xloc, y = 2,  label = "X2 only") +
    annotate("label", x = xloc, y = 1, label = "None")  +
     xlim(c(range(pu$elpd_loo)[1] - 2, range(pu$elpd_loo)[2] + 2)) )

ggsave(here::here("02-super popn approach/experiment4_SAE/02-results/plot_loo_unwtd_fx3.png"), p1, width=6, height=7.5, units="in", device="png")

## range of elpd values 
xloc2 = 38
(p1a = ggplot(pu, aes(x = range_pe, y = model, group = iter, colour = model)) +
    geom_point(position = position_dodge(width = .5), alpha=0.7) +
    scale_colour_manual(values = c("#1C73B1FF", "#26456EFF",
                                   "#FB964EFF", "#DF5948FF",
                                   "#09622AFF",
                                   "#879195FF"))  +
    scale_y_discrete(limits = rev) +
    stat_summary(aes(group=model), width=0.5, size=.2, fun=mean, geom="crossbar", colour="black") + #drawing the mean line
    theme(legend.position = "none",
          axis.title = element_blank()) +
    annotate("label", x = 10, y = 5.5, label = "X2 and X4") +
    annotate("label", x = xloc2, y = 3.5, label = "X4 only") +
    annotate("label", x = xloc2, y = 2,  label = "X2 only") +
    annotate("label", x = xloc2, y = 1, label = "None") +
    labs(title="Range of interval for elpd values (unweighted)") + xlim(c(2,42)))

ggsave(here::here("02-super popn approach/experiment4_SAE/02-results/plot_loo_unwtd_range_fx3.png"), p1a, width=6, height=7.5, units="in", device="png")

# weighted loo ------------------------------------------------------------
# calculating weighted loo (not done in cluster so postprocessing here)

loo_wtd_06_tab = loo_wtd_06_list[iter] %>% 
  do.call(rbind, .) %>% 
  mutate(model = 'model06',
         iter = 1:length(iter))
loo_wtd_11_tab = loo_wtd_11_list[iter] %>% 
  do.call(rbind, .) %>% 
  mutate(model = 'model11',
         iter = 1:length(iter))
loo_wtd_13_tab = loo_wtd_13_list[iter] %>% 
  do.call(rbind, .) %>% 
  mutate(model = 'model13',
         iter = 1:length(iter))
loo_wtd_13a_tab = loo_wtd_13a_list[iter] %>% 
  do.call(rbind, .) %>% 
  mutate(model = 'model13a',
         iter = 1:length(iter))
loo_wtd_15_tab = loo_wtd_15_list[iter] %>% 
  do.call(rbind, .) %>% 
  mutate(model = 'model15',
         iter = 1:length(iter))
loo_wtd_15a_tab = loo_wtd_15a_list[iter] %>% 
  do.call(rbind, .) %>% 
  mutate(model = 'model15a',
         iter = 1:length(iter))

wtd_elpd_se_tab = rbind(loo_wtd_06_tab, loo_wtd_11_tab, loo_wtd_13_tab,
                        loo_wtd_13a_tab, loo_wtd_15_tab, loo_wtd_15a_tab)


ph = wtd_elpd_se_tab %>% 
  mutate(model = factor(model),
         low_elpd = .$wtd_elpd_loo - (.$wtd_SE*1.64), # calculating upper and lower bound of the elpd values
         upp_elpd = .$wtd_elpd_loo + (.$wtd_SE*1.64),
         range_pe = upp_elpd - low_elpd) %>% 
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
xloc3 = -5000
(p2 = ggplot(ph, aes(x = wtd_elpd_loo, y = model, group = iter, colour = model))+
  geom_point(position = position_dodge(width = .5), alpha=0.7) +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  scale_y_discrete(limits = rev) +
  stat_summary(aes(group=model), width=0.5, size=.2, fun=mean, geom="crossbar", colour="black") + #drawing the mean line
  scale_colour_manual(values = c("#1C73B1FF", "#26456EFF",
                                 "#FB964EFF", "#DF5948FF",
                                 "#09622AFF",
                                 "#879195FF")) + 
  labs(title="Raw elpd values (weighted)") +
  annotate("label", x = -6600, y = 5.5, label = "X2 and X4") +
  annotate("label", x = xloc3, y = 3.5, label = "X4 only") +
  annotate("label", x = xloc3, y = 2, label = "X2 only") +
  annotate("label", x = xloc3, y = 1, label = "None")   + xlim(-7000,-4650) )

ggsave(here::here("02-super popn approach/experiment4_SAE/02-results/plot_loo_wtd_fx3.png"), p2, width=6, height=7.5, units="in", device="png")

## range of elpd values (weighted)
xloc2 = 2000
(p3 = ggplot(ph, aes(x = range_pe, y = model, group = iter, colour = model)) +
  geom_point(position = position_dodge(width = .5), alpha=0.7) +
  scale_colour_manual(values = c("#1C73B1FF", "#26456EFF",
                                 "#FB964EFF", "#DF5948FF",
                                 "#09622AFF",
                                 "#879195FF"))  +
  scale_y_discrete(limits = rev) +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  annotate("label", x = xloc2, y = 5.5, label = "X2 and X4") +
  annotate("label", x = xloc2, y = 3.5, label = "X4 only") +
  annotate("label", x = xloc2, y = 2,  label = "X2 only") +
  annotate("label", x = xloc2, y = 1, label = "None") +
  labs(title="Range of interval for elpd values (weighted)") + xlim(c(550,2200)))

ggsave(here::here("02-super popn approach/experiment4_SAE/02-results/plot_loo_wtd_range_fx3.png"), p3, width=6, height=7.5, units="in", device="png")


# small area loo ----------------------------------------------------------

sum_elpd_all = list()

iter=1:100
for(ite in iter){
  samp_data_list[[ite]]$elpd_loo_06 = loo_06_list[[ite]]$pointwise[,1]
  samp_data_list[[ite]]$elpd_loo_11 = loo_11_list[[ite]]$pointwise[,1]
  samp_data_list[[ite]]$elpd_loo_13 = loo_13_list[[ite]]$pointwise[,1]
  samp_data_list[[ite]]$elpd_loo_13a = loo_13a_list[[ite]]$pointwise[,1]
  samp_data_list[[ite]]$elpd_loo_15 = loo_15_list[[ite]]$pointwise[,1]
  samp_data_list[[ite]]$elpd_loo_15a = loo_15a_list[[ite]]$pointwise[,1]
  
  
  sum_elpd_all[[ite]] =  samp_data_list[[ite]] %>% 
    group_by(X4) %>% 
    summarise(sum_elpd_06 = sum(elpd_loo_06),
              sum_elpd_11 = sum(elpd_loo_11),
              sum_elpd_13 = sum(elpd_loo_13),
              sum_elpd_13a = sum(elpd_loo_13a),
              sum_elpd_15 = sum(elpd_loo_15),
              sum_elpd_15a = sum(elpd_loo_15a)) %>% 
    mutate(iteration = ite)
  
}

# combining all loo estimate by group
sum_elpd_all_tab = do.call(rbind, sum_elpd_all)

sum_elpd_all_tab %>%
  select(X4, sum_elpd_15) %>% 
  group_by(X4) %>% 
  summarise(sum_loo_group = sum(sum_elpd_15))

sum_elpd_all_tab %>%
  select(X4, sum_elpd_15a) %>% 
  group_by(X4) %>% 
  summarise(sum_loo_group = sum(sum_elpd_15a))



## potential plotting
test = sum_elpd_all_tab %>%
  select(sum_elpd_15, sum_elpd_15a) %>% 
  gather(.) %>% 
  mutate(X4 = rep(1:12, 200),
         iter = c(rep(1:100, each=12), rep(1:100, each=12))) %>% 
  rename(model = key, elpd_loo = value) %>% 
  mutate(model = as.factor(model))




