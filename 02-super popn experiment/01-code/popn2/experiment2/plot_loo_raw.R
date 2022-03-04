## plotting raw elpd values 
# 06/12/2021
library(ggplot2)
library(reshape2)
library(dplyr)
library(magrittr)
library(survey)

load("~/GitHub/LOO_MRP/02-super popn experiment/01-code/popn2/experiment2/simulated100_1.RData")
samp_data_list2 = samp_data_list
n = nrow(samp_data_list[[1]])

## extracting elpd_loo 
elpd_tab = sim_list %>% 
  lapply(., function(x)(x[,'elpd_loo'])) %>% 
  do.call(rbind, .) %>%   
  as.data.frame() %>% 
  set_colnames(c(paste0('elpd0',1:4), "elpd15"))


# weighted elpd -----------------------------------------------------------
## calculate SE for weighted loo_diff
# calculating svy_rake weights for each sample
svy_rake_list = lapply(samp_data_list, function(x)svydesign(ids=~1, # cluster id, ~1 for no clusters
                                                            weights=~wts, # including raked weights in the survey design
                                                            data=x))

wtd_elpd_se_tab1 = list()
wtd_elpd_se_tab2 = list()
wtd_elpd_se_tab3 = list()
wtd_elpd_se_tab4 = list()

for(i in 1:length(samp_data_list)){
  wtd_elpd_se_tab1[[i]] = svytotal(samp_data_list[[i]]$elpd_1, svy_rake_list[[i]]) %>% 
    as.data.frame(.)
  
  wtd_elpd_se_tab2[[i]] = svytotal(samp_data_list[[i]]$elpd_2, svy_rake_list[[i]]) %>% 
    as.data.frame(.)
  
  wtd_elpd_se_tab3[[i]] = svytotal(samp_data_list[[i]]$elpd_3, svy_rake_list[[i]]) %>% 
    as.data.frame(.)
  
  wtd_elpd_se_tab4[[i]] = svytotal(samp_data_list[[i]]$elpd_4, svy_rake_list[[i]]) %>% 
    as.data.frame(.)
}

wtd_elpd_se_tab1 = do.call(rbind, wtd_elpd_se_tab1) %>% 
  mutate(model = "model01")

wtd_elpd_se_tab2 = do.call(rbind, wtd_elpd_se_tab2) %>% 
  mutate(model = "model02")

wtd_elpd_se_tab3 = do.call(rbind, wtd_elpd_se_tab3) %>% 
  mutate(model = "model03")

wtd_elpd_se_tab4 = do.call(rbind, wtd_elpd_se_tab4) %>% 
  mutate(model = "model04")

wtd_elpd_se_tab = bind_rows(wtd_elpd_se_tab1,
                            wtd_elpd_se_tab2,
                            wtd_elpd_se_tab3,
                            wtd_elpd_se_tab4) %>% 
  mutate(low_elpd = .$total - (.$SE*1.64), # calculating upper and lower bound of the elpd values
         upp_elpd = .$total + (.$SE*1.64),
         iter = rep(1:100, 4))

# plot 
ggplot(wtd_elpd_se_tab, aes(x = total, y = model, group = iter, colour = model))+
  # geom_vline(aes(xintercept = 0)) +
  # xlim(c(-3800, 2200)) +
  geom_point(position = position_dodge(width = .5)) +
  geom_errorbarh(mapping = aes(xmin = low_elpd, 
                               xmax = upp_elpd), 
                 position = position_dodge(width = .5),
                 height = 0, alpha = .7) +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  scale_y_discrete(limits = rev) +
  scale_colour_manual(values = pals::tableau20(20)[c(1,2,9,10,3,4,7,8,13,14,5,6,17,18)]) +
  labs(title="Difference in elpd values (weighted)") 

ph1 = wtd_elpd_se_tab #weighted elpd

# non-weighted elpd -------------------------------------------------------
elpd_se_tab01 = samp_data_list %>% 
  sapply(., function(x)sum(x$elpd_1)) %>% 
  as.data.frame() %>% 
  set_colnames(., 'total')  %>% 
  mutate(SE =  sapply(samp_data_list, function(x) sqrt(n) * sd(x$elpd_1)),
         model = 'model01')
  
elpd_se_tab02 = samp_data_list %>% 
  sapply(., function(x)sum(x$elpd_2)) %>% 
  as.data.frame() %>% 
  set_colnames(., 'total')  %>% 
  mutate(SE =  sapply(samp_data_list, function(x) sqrt(n) * sd(x$elpd_2)),
         model = 'model02')

elpd_se_tab03 = samp_data_list %>% 
  sapply(., function(x)sum(x$elpd_3)) %>% 
  as.data.frame() %>% 
  set_colnames(., 'total')  %>% 
  mutate(SE =  sapply(samp_data_list, function(x) sqrt(n) * sd(x$elpd_3)),
         model = 'model03')

elpd_se_tab04 = samp_data_list %>% 
  sapply(., function(x)sum(x$elpd_4)) %>% 
  as.data.frame() %>% 
  set_colnames(., 'total')  %>% 
  mutate(SE =  sapply(samp_data_list, function(x) sqrt(n) * sd(x$elpd_4)),
         model = 'model04')

elpd_se_tab = bind_rows(elpd_se_tab01,
                        elpd_se_tab02,
                        elpd_se_tab03,
                        elpd_se_tab04) %>% 
  mutate(low_elpd = .$total - (.$SE*1.64), # calculating upper and lower bound of the elpd values
         upp_elpd = .$total + (.$SE*1.64),
         iter = rep(1:100, 4))

# plot
ggplot(elpd_se_tab, aes(x = total, y = model, group = iter, colour = model))+
  # geom_vline(aes(xintercept = 0), size=1) +
  # xlim(c(-200, 100)) +
  geom_point(position = position_dodge(width = .5)) +
  geom_errorbarh(mapping = aes(xmin = low_elpd, 
                               xmax = upp_elpd), 
                 position = position_dodge(width = .5),
                 height = 0, alpha = .5) +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  scale_y_discrete(limits = rev) +
  scale_colour_manual(values = pals::tableau20(20)[c(1,2,9,10,3,4,7,8,13,14,5,6,17,18)]) + 
  labs(title="Difference in elpd values (unweighted)") 

pu1 = elpd_se_tab #unweighted elpd

# models 05-10 ------------------------------------------------------------
load("~/GitHub/LOO_MRP/02-super popn experiment/01-code/popn2/experiment2/simulated100temp_2.RData")
sim_list2 = sim_doub_list[1:40]
samp_data = samp_data_list[1:40]


## extracting elpd_loo 
elpd_tab = sim_list2 %>% 
  lapply(., function(x)(x[,'elpd_loo'])) %>% 
  do.call(rbind, .) %>%   
  as.data.frame() %>% 
  set_colnames(c(paste0('elpd0',5:9), "elpd10", "elpd15")) 

## calculate SE for weighted loo
# calculating svy_rake weights for each sample
svy_rake_list = lapply(samp_data, function(x)svydesign(ids=~1, # cluster id, ~1 for no clusters
                                                       weights=~wts, # including raked weights in the survey design
                                                       data=x))
wtd_elpd_se_tab5 = list()
wtd_elpd_se_tab6 = list()
wtd_elpd_se_tab7 = list()
wtd_elpd_se_tab8 = list()
wtd_elpd_se_tab9 = list()
wtd_elpd_se_tab10 = list()

## calculating weighted LOO values and its SE
for(i in 1:length(samp_data)){
  wtd_elpd_se_tab5[[i]] = svytotal(samp_data[[i]]$elpd_5 , svy_rake_list[[i]]) %>% 
    as.data.frame(.)
  
  wtd_elpd_se_tab6[[i]] = svytotal(samp_data[[i]]$elpd_6, svy_rake_list[[i]]) %>% 
    as.data.frame(.)
  
  wtd_elpd_se_tab7[[i]] = svytotal(samp_data[[i]]$elpd_7, svy_rake_list[[i]]) %>% 
    as.data.frame(.)
  
  wtd_elpd_se_tab8[[i]] = svytotal(samp_data[[i]]$elpd_8, svy_rake_list[[i]]) %>% 
    as.data.frame(.)
  
  wtd_elpd_se_tab9[[i]] = svytotal(samp_data[[i]]$elpd_9, svy_rake_list[[i]]) %>% 
    as.data.frame(.)
  
  wtd_elpd_se_tab10[[i]] = svytotal(samp_data[[i]]$elpd_10, svy_rake_list[[i]]) %>% 
    as.data.frame(.)
}

# weighted elpd -----------------------------------------------------------
wtd_elpd_se_tab5 = do.call(rbind, wtd_elpd_se_tab5) %>% 
  mutate(model = "model05")

wtd_elpd_se_tab6 = do.call(rbind, wtd_elpd_se_tab6) %>% 
  mutate(model = "model06")

wtd_elpd_se_tab7 = do.call(rbind, wtd_elpd_se_tab7) %>% 
  mutate(model = "model07")

wtd_elpd_se_tab8 = do.call(rbind, wtd_elpd_se_tab8) %>% 
  mutate(model = "model08")

wtd_elpd_se_tab9 = do.call(rbind, wtd_elpd_se_tab9) %>% 
  mutate(model = "model09")

wtd_elpd_se_tab10 = do.call(rbind, wtd_elpd_se_tab10) %>% 
  mutate(model = "model10")

wtd_elpd_se_tab = bind_rows(wtd_elpd_se_tab5,
                            wtd_elpd_se_tab6,
                            wtd_elpd_se_tab7,
                            wtd_elpd_se_tab8,
                            wtd_elpd_se_tab9,
                            wtd_elpd_se_tab10) %>% 
  mutate(low_elpd = .$total - (.$SE*1.64), # calculating upper and lower bound of the elpd values
         upp_elpd = .$total + (.$SE*1.64),
         iter = rep(1:length(samp_data), 6))

#Example plot
ggplot(wtd_elpd_se_tab, aes(x = total, y = model, group = iter, colour = model))+
  geom_vline(aes(xintercept = 0)) +
  # xlim(c(-3800, 2200)) +
  geom_point(position = position_dodge(width = .5)) +
  geom_errorbarh(mapping = aes(xmin = low_elpd, 
                               xmax = upp_elpd), 
                 position = position_dodge(width = .5),
                 height = 0, alpha = .7) +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  annotate("label", x = -1000, y = 0.5, label = "Model 15 preferred") +
  annotate("label", x = 1000, y = 0.5, label = "Alt model preferred") +
  scale_y_discrete(limits = rev) +
  scale_colour_manual(values = pals::tableau20(20)[c(3,4,7,8,13,14,5,6,17,18)]) 

ph2 = wtd_elpd_se_tab #weighted

# non-weighted elpd -------------------------------------------------------
elpd_se_tab05 = samp_data %>% 
  sapply(., function(x)sum(x$elpd_5)) %>% 
  as.data.frame() %>% 
  set_colnames(., 'total')  %>% 
  mutate(SE =  sapply(samp_data, function(x) sqrt(n) * sd(x$elpd_5)),
         model = 'model05')

elpd_se_tab06 = samp_data %>% 
  sapply(., function(x)sum(x$elpd_6)) %>% 
  as.data.frame() %>% 
  set_colnames(., 'total')  %>% 
  mutate(SE =  sapply(samp_data, function(x) sqrt(n) * sd(x$elpd_6)),
         model = 'model06')

elpd_se_tab07 = samp_data %>% 
  sapply(., function(x)sum(x$elpd_7)) %>% 
  as.data.frame() %>% 
  set_colnames(., 'total')  %>% 
  mutate(SE =  sapply(samp_data, function(x) sqrt(n) * sd(x$elpd_7)),
         model = 'model07')

elpd_se_tab08 = samp_data %>% 
  sapply(., function(x)sum(x$elpd_8)) %>% 
  as.data.frame() %>% 
  set_colnames(., 'total')  %>% 
  mutate(SE =  sapply(samp_data, function(x) sqrt(n) * sd(x$elpd_8)),
         model = 'model08')

elpd_se_tab09 = samp_data %>% 
  sapply(., function(x)sum(x$elpd_9)) %>% 
  as.data.frame() %>% 
  set_colnames(., 'total')  %>% 
  mutate(SE =  sapply(samp_data, function(x) sqrt(n) * sd(x$elpd_9)),
         model = 'model09')

elpd_se_tab10 = samp_data %>% 
  sapply(., function(x)sum(x$elpd_10)) %>% 
  as.data.frame() %>% 
  set_colnames(., 'total')  %>% 
  mutate(SE =  sapply(samp_data, function(x) sqrt(n) * sd(x$elpd_10)),
         model = 'model10')

elpd_se_tab = bind_rows(elpd_se_tab05,
                        elpd_se_tab06,
                        elpd_se_tab07,
                        elpd_se_tab08,
                        elpd_se_tab09,
                        elpd_se_tab10) %>% 
  mutate(low_elpd = .$total - (.$SE*1.64), # calculating upper and lower bound of the elpd values
         upp_elpd = .$total + (.$SE*1.64),
         iter = rep(1:length(samp_data), 6))

#Example plot
ggplot(elpd_se_tab, aes(x = total, y = model, group = iter, colour = model))+
  # geom_vline(aes(xintercept = 0), size=1) +
  # xlim(c(-200, 100)) +
  geom_point(position = position_dodge(width = .5)) +
  geom_errorbarh(mapping = aes(xmin = low_elpd, 
                               xmax = upp_elpd), 
                 position = position_dodge(width = .5),
                 height = 0, alpha = .5) +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  scale_y_discrete(limits = rev) +
  scale_colour_manual(values = pals::tableau20(20)[c(1,2,9,10,3,4,7,8,13,14,5,6,17,18)]) + 
  labs(title="Difference in elpd values (unweighted)") 

pu2 = elpd_se_tab #unweighted

# models 11-15 ------------------------------------------------------------
load("~/GitHub/LOO_MRP/02-super popn experiment/01-code/popn2/experiment2/simulated100temp_3.RData")
sim_list2 = sim_trip_list[1:52]
samp_data = samp_data_list[1:52]

## extracting elpd_loo 
elpd_tab = sim_list2 %>% 
  lapply(., function(x)(x[,'elpd_loo'])) %>% 
  do.call(rbind, .) %>%   
  as.data.frame() %>% 
  set_colnames(c(paste0('elpd',11:15))) 
  
## calculate SE for weighted loo
# calculating svy_rake weights for each sample
svy_rake_list = lapply(samp_data, function(x)svydesign(ids=~1, # cluster id, ~1 for no clusters
                                                       weights=~wts, # including raked weights in the survey design
                                                       data=x))

wtd_elpd_se_tab11 = list()
wtd_elpd_se_tab12 = list()
wtd_elpd_se_tab13 = list()
wtd_elpd_se_tab14 = list()
wtd_elpd_se_tab15 = list()

## calculating weighted LOO values and its SE
for(i in 1:length(samp_data)){
  wtd_elpd_se_tab11[[i]] = svytotal(samp_data[[i]]$elpd_11, svy_rake_list[[i]]) %>% 
    as.data.frame(.)
  
  wtd_elpd_se_tab12[[i]] = svytotal(samp_data[[i]]$elpd_12, svy_rake_list[[i]]) %>% 
    as.data.frame(.)
  
  wtd_elpd_se_tab13[[i]] = svytotal(samp_data[[i]]$elpd_13, svy_rake_list[[i]]) %>% 
    as.data.frame(.)
  
  wtd_elpd_se_tab14[[i]] = svytotal(samp_data[[i]]$elpd_14 , svy_rake_list[[i]]) %>% 
    as.data.frame(.)
  
  wtd_elpd_se_tab15[[i]] = svytotal(samp_data[[i]]$elpd_15 , svy_rake_list[[i]]) %>% 
    as.data.frame(.)
}


# weighted elpd -----------------------------------------------------------
wtd_elpd_se_tab11 = do.call(rbind, wtd_elpd_se_tab11) %>% 
  mutate(model = "model11")

wtd_elpd_se_tab12 = do.call(rbind, wtd_elpd_se_tab12) %>% 
  mutate(model = "model12")

wtd_elpd_se_tab13 = do.call(rbind, wtd_elpd_se_tab13) %>% 
  mutate(model = "model13")

wtd_elpd_se_tab14 = do.call(rbind, wtd_elpd_se_tab14) %>% 
  mutate(model = "model14")

wtd_elpd_se_tab15 = do.call(rbind, wtd_elpd_se_tab15) %>% 
  mutate(model = "model15")

wtd_elpd_se_tab = bind_rows(wtd_elpd_se_tab11,
                            wtd_elpd_se_tab12,
                            wtd_elpd_se_tab13,
                            wtd_elpd_se_tab14, 
                            wtd_elpd_se_tab15) %>% 
  mutate(low_elpd = .$total - (.$SE*1.64), # calculating upper and lower bound of the elpd values
         upp_elpd = .$total + (.$SE*1.64),
         iter = rep(1:length(samp_data), 5))

## combining with other models
ph3 = wtd_elpd_se_tab
ph = bind_rows(ph1,ph2,ph3) %>% 
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
ph$model = fct_relevel(ph$model, c('X2 + X4', 'X1 + X2 + X4', 'X2 + X3 + X4', 'X1 + X2 + X3 + X4', 
                                   'X4', 'X1 + X4', 'X3 + X4', 'X1 + X3 + X4',
                                   'X2', 'X1 + X2', 'X2 + X3', 'X1 + X2 + X3',
                                   'X1', 'X3', 'X1 + X3'))

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
  scale_colour_manual(values = pals::tableau20(20)[c(1,2,9,10,3,4,7,8,13,14,5,6,17,18,12)]) + 
  labs(title="Raw elpd values (weighted)") +
  annotate("label", x = -6100, y = 13.5, label = "X2 and X4") +
  annotate("label", x = -4100, y = 9.5, label = "X4 only") +
  annotate("label", x = -4100, y = 5.5, label = "X2 only") +
  annotate("label", x = -4100, y = 2, label = "None") 
  
ggsave("plot_loo_raw_wtd.png", g, width=6, height=7.5, units="in", device="png")

# violin plot
ggplot(ph, aes(group = model, fill = model))+
  geom_violin(aes(x = low_elpd, y = model),alpha=0.3) +
  geom_violin(aes(x = upp_elpd, y = model), alpha=0.3) +
  geom_violin(aes(x = total, y = model), alpha=1) +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  scale_y_discrete(limits = rev) +
  scale_fill_manual(values = pals::tableau20(20)[c(1,2,9,10,3,4,7,8,13,14,5,6,17,18,12)]) + 
  labs(title="Difference in elpd values (weighted)") 

ggsave("plot_loo_diff_wtd_violin.pdf", gv, width=6, height=7.5, units="in", device="pdf")


# non-weighted elpd -------------------------------------------------------
elpd_se_tab11 = samp_data %>% 
  sapply(., function(x)sum(x$elpd_11)) %>% 
  as.data.frame() %>% 
  set_colnames(., 'total')  %>% 
  mutate(SE =  sapply(samp_data, function(x) sqrt(n) * sd(x$elpd_11)),
         model = 'model11')

elpd_se_tab12 = samp_data %>% 
  sapply(., function(x)sum(x$elpd_12)) %>% 
  as.data.frame() %>% 
  set_colnames(., 'total')  %>% 
  mutate(SE =  sapply(samp_data, function(x) sqrt(n) * sd(x$elpd_12)),
         model = 'model12')

elpd_se_tab13 = samp_data %>% 
  sapply(., function(x)sum(x$elpd_13)) %>% 
  as.data.frame() %>% 
  set_colnames(., 'total')  %>% 
  mutate(SE =  sapply(samp_data, function(x) sqrt(n) * sd(x$elpd_13)),
         model = 'model13')

elpd_se_tab14 = samp_data %>% 
  sapply(., function(x)sum(x$elpd_14)) %>% 
  as.data.frame() %>% 
  set_colnames(., 'total')  %>% 
  mutate(SE =  sapply(samp_data, function(x) sqrt(n) * sd(x$elpd_14)),
         model = 'model14')

elpd_se_tab15 = samp_data %>% 
  sapply(., function(x)sum(x$elpd_15)) %>% 
  as.data.frame() %>% 
  set_colnames(., 'total')  %>% 
  mutate(SE =  sapply(samp_data, function(x) sqrt(n) * sd(x$elpd_15)),
         model = 'model15')

elpd_se_tab = bind_rows(elpd_se_tab11,
                        elpd_se_tab12,
                        elpd_se_tab13,
                        elpd_se_tab14, 
                        elpd_se_tab15) %>% 
  mutate(low_elpd = .$total - (.$SE*1.64), # calculating upper and lower bound of the elpd values
         upp_elpd = .$total + (.$SE*1.64),
         iter = rep(1:length(samp_data), 5))

pu3 = elpd_se_tab

pu = bind_rows(pu1, pu2, pu3) %>% 
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
  annotate("label", x = -850, y = 13.5, label = "X2 and X4") +
  annotate("label", x = -610, y = 9.5, label = "X4 only") +
  annotate("label", x = -610, y = 5.5,  label = "X2 only") +
  annotate("label", x = -610, y = 2, label = "None") 


ggsave("plot_loo_raw_unwtd.png", g2, width=6, height=7.5, units="in", device="png")

