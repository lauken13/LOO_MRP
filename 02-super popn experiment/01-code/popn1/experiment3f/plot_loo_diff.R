## plotting the elpd values against 'best' model 
library(ggplot2)
library(reshape2)
library(dplyr)
library(magrittr)
library(survey)

load("~/GitHub/LOO_MRP/02-super popn experiment/01-code/popn1/experiment3f/simulated100temp_1.RData")

samp_data_list2 = samp_data_list[1:75]

## extracting elpd_loo and calculating difference with model15
elpd_tab = sim_list[1:75] %>% 
  lapply(., function(x)(x[,'elpd_loo'])) %>% 
  do.call(rbind, .) %>%   
  as.data.frame() %>% 
  set_colnames(c(paste0('elpd0',1:4), "elpd15")) %>% 
  mutate(model01 = .[,1] - .[,'elpd15'], 
         model02 = .[,2] - .[,'elpd15'],
         model03 = .[,3] - .[,'elpd15'],
         model04 = .[,4] - .[,'elpd15'])

# weighted elpd -----------------------------------------------------------
## calculate SE for weighted loo_diff
# calculating svy_rake weights for each sample
svy_rake_list = lapply(samp_data_list2, function(x)svydesign(ids=~1, # cluster id, ~1 for no clusters
                                                            weights=~wts, # including raked weights in the survey design
                                                            data=x))
wtd_elpd_se_tab1 = list()
wtd_elpd_se_tab2 = list()
wtd_elpd_se_tab3 = list()
wtd_elpd_se_tab4 = list()

loo_diff_01 = list()
loo_diff_02 = list()
loo_diff_03 = list()
loo_diff_04 = list()

## calculating weighted LOO values and its SE
for(i in 1:length(samp_data_list2)){
  loo_diff_01[[i]] = samp_data_list2[[i]]$elpd_1 - samp_data_list2[[i]]$elpd_15
  wtd_elpd_se_tab1[[i]] = svytotal(loo_diff_01[[i]], svy_rake_list[[i]]) %>% 
    as.data.frame(.)
  
  loo_diff_02[[i]] = samp_data_list2[[i]]$elpd_2 - samp_data_list2[[i]]$elpd_15
  wtd_elpd_se_tab2[[i]] = svytotal(loo_diff_02[[i]], svy_rake_list[[i]]) %>% 
    as.data.frame(.)
  
  loo_diff_03[[i]] = samp_data_list2[[i]]$elpd_3 - samp_data_list2[[i]]$elpd_15
  wtd_elpd_se_tab3[[i]] = svytotal(loo_diff_03[[i]], svy_rake_list[[i]]) %>% 
    as.data.frame(.)
  
  loo_diff_04[[i]] = samp_data_list2[[i]]$elpd_4 - samp_data_list2[[i]]$elpd_15
  wtd_elpd_se_tab4[[i]] = svytotal(loo_diff_04[[i]], svy_rake_list[[i]]) %>% 
    as.data.frame(.)
}

wtd_elpd_se_tab1 = do.call(rbind, wtd_elpd_se_tab1)
wtd_elpd_se_tab1$model = "model01"

wtd_elpd_se_tab2 = do.call(rbind, wtd_elpd_se_tab2)
wtd_elpd_se_tab2$model = "model02"

wtd_elpd_se_tab3 = do.call(rbind, wtd_elpd_se_tab3)
wtd_elpd_se_tab3$model = "model03"

wtd_elpd_se_tab4 = do.call(rbind, wtd_elpd_se_tab4)
wtd_elpd_se_tab4$model = "model04"

wtd_elpd_se_tab = bind_rows(wtd_elpd_se_tab1,
                            wtd_elpd_se_tab2,
                            wtd_elpd_se_tab3,
                            wtd_elpd_se_tab4)

## calculating upper and lower bound of the elpd values
wtd_elpd_se_tab$low_elpd = wtd_elpd_se_tab[,'total'] - 1.64*wtd_elpd_se_tab[,'SE']
wtd_elpd_se_tab$upp_elpd = wtd_elpd_se_tab[,'total'] + 1.64*wtd_elpd_se_tab[,'SE']
wtd_elpd_se_tab$iter = rep(1:length(samp_data_list2), 4)

#Example plot
ggplot(wtd_elpd_se_tab, aes(x = total, y = model, group = iter, colour = model))+
  geom_vline(aes(xintercept = 0)) +
  xlim(c(-3800, 2200)) +
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
  scale_colour_manual(values = pals::tableau20(20)[c(1,2,9,10,3,4,7,8,13,14,5,6,17,18)]) +
  labs(title="Difference in elpd values (weighted)") 

ph1 = wtd_elpd_se_tab #weighted elpd

# non-weighted elpd -------------------------------------------------------
elpd_se_tab01 = sapply(loo_diff_01, sum) %>% as.data.frame() %>% set_colnames(., 'total')
elpd_se_tab01$sd = sapply(loo_diff_01, sd) %>% as.numeric() 
elpd_se_tab01$SE = as.numeric(sqrt(500) * elpd_se_tab01$sd)
elpd_se_tab01$model = 'model01'

elpd_se_tab02 = sapply(loo_diff_02, sum) %>% as.data.frame() %>% set_colnames(., 'total')
elpd_se_tab02$sd = sapply(loo_diff_02, sd) %>% as.numeric() 
elpd_se_tab02$SE = as.numeric(sqrt(500) * elpd_se_tab02$sd)
elpd_se_tab02$model = 'model02'

elpd_se_tab03= sapply(loo_diff_03, sum) %>% as.data.frame() %>% set_colnames(., 'total')
elpd_se_tab03$sd = sapply(loo_diff_03, sd) %>% as.numeric() 
elpd_se_tab03$SE = as.numeric(sqrt(500) * elpd_se_tab03$sd)
elpd_se_tab03$model = 'model03'

elpd_se_tab04 = sapply(loo_diff_04, sum) %>% as.data.frame() %>% set_colnames(., 'total')
elpd_se_tab04$sd = sapply(loo_diff_04, sd) %>% as.numeric() 
elpd_se_tab04$SE = as.numeric(sqrt(500) * elpd_se_tab04$sd) 
elpd_se_tab04$model = 'model04'

elpd_se_tab = bind_rows(elpd_se_tab01,
                        elpd_se_tab02,
                        elpd_se_tab03,
                        elpd_se_tab04)

elpd_se_tab$low_elpd = elpd_se_tab[,'total'] - 1.64*elpd_se_tab[,'SE'] 
elpd_se_tab$upp_elpd = elpd_se_tab[,'total'] + 1.64*elpd_se_tab[,'SE'] 
elpd_se_tab$iter = rep(1:length(samp_data_list2), 4)


#Example plot
ggplot(elpd_se_tab, aes(x = total, y = model, group = iter, colour = model))+
  geom_vline(aes(xintercept = 0), size=1) +
  xlim(c(-200, 100)) +
  geom_point(position = position_dodge(width = .5)) +
  geom_errorbarh(mapping = aes(xmin = low_elpd, 
                               xmax = upp_elpd), 
                 position = position_dodge(width = .5),
                 height = 0, alpha = .5) +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  annotate("label", x = -50, y = 0.5, label = "Model 15 preferred") +
  annotate("label", x = 50, y = 0.5, label = "Alt model preferred") +
  scale_y_discrete(limits = rev) +
  scale_colour_manual(values = pals::tableau20(20)[c(1,2,9,10,3,4,7,8,13,14,5,6,17,18)]) + 
  labs(title="Difference in elpd values (unweighted)") 

pu1 = elpd_se_tab #unweighted elpd

# models 05-10 ------------------------------------------------------------
load("~/GitHub/LOO_MRP/02-super popn experiment/01-code/popn1/experiment3f/simulated100temp_2.RData")
sim_list2 = sim_doub_list[1:51]
samp_data = samp_data_list[1:51]


## extracting elpd_loo and calculating difference with model15
elpd_tab = sim_list2 %>% 
  lapply(., function(x)(x[,'elpd_loo'])) %>% 
  do.call(rbind, .) %>%   
  as.data.frame() %>% 
  set_colnames(c(paste0('elpd0',5:9), "elpd10", "elpd15")) %>% 
  mutate(model05 = .[,1] - .[,'elpd15'],
         model06 = .[,2] - .[,'elpd15'],
         model07 = .[,3] - .[,'elpd15'],
         model08 = .[,4] - .[,'elpd15'],
         model09 = .[,5] - .[,'elpd15'],
         model10 = .[,6] - .[,'elpd15'])

## calculate SE for weighted loo_diff
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

loo_diff_05 = list()
loo_diff_06 = list()
loo_diff_07 = list()
loo_diff_08 = list()
loo_diff_09 = list()
loo_diff_10 = list()

## calculating weighted LOO values and its SE
for(i in 1:length(samp_data)){
  loo_diff_05[[i]] = samp_data[[i]]$elpd_5 - samp_data[[i]]$elpd_15
  wtd_elpd_se_tab5[[i]] = svytotal(loo_diff_05[[i]], svy_rake_list[[i]]) %>% 
    as.data.frame(.)
  
  loo_diff_06[[i]] = samp_data[[i]]$elpd_6 - samp_data[[i]]$elpd_15
  wtd_elpd_se_tab6[[i]] = svytotal(loo_diff_06[[i]], svy_rake_list[[i]]) %>% 
    as.data.frame(.)
  
  loo_diff_07[[i]] = samp_data[[i]]$elpd_7 - samp_data[[i]]$elpd_15
  wtd_elpd_se_tab7[[i]] = svytotal(loo_diff_07[[i]], svy_rake_list[[i]]) %>% 
    as.data.frame(.)
  
  loo_diff_08[[i]] = samp_data[[i]]$elpd_8 - samp_data[[i]]$elpd_15
  wtd_elpd_se_tab8[[i]] = svytotal(loo_diff_08[[i]], svy_rake_list[[i]]) %>% 
    as.data.frame(.)
  
  loo_diff_09[[i]] = samp_data[[i]]$elpd_9 - samp_data[[i]]$elpd_15
  wtd_elpd_se_tab9[[i]] = svytotal(loo_diff_09[[i]], svy_rake_list[[i]]) %>% 
    as.data.frame(.)
  
  loo_diff_10[[i]] = samp_data[[i]]$elpd_10 - samp_data[[i]]$elpd_15
  wtd_elpd_se_tab10[[i]] = svytotal(loo_diff_10[[i]], svy_rake_list[[i]]) %>% 
    as.data.frame(.)
}

# weighted elpd -----------------------------------------------------------
wtd_elpd_se_tab5 = do.call(rbind, wtd_elpd_se_tab5)
wtd_elpd_se_tab5$model = "model05"

wtd_elpd_se_tab6 = do.call(rbind, wtd_elpd_se_tab6)
wtd_elpd_se_tab6$model = "model06"

wtd_elpd_se_tab7 = do.call(rbind, wtd_elpd_se_tab7)
wtd_elpd_se_tab7$model = "model07"

wtd_elpd_se_tab8 = do.call(rbind, wtd_elpd_se_tab8)
wtd_elpd_se_tab8$model = "model08"

wtd_elpd_se_tab9 = do.call(rbind, wtd_elpd_se_tab9)
wtd_elpd_se_tab9$model = "model09"

wtd_elpd_se_tab10 = do.call(rbind, wtd_elpd_se_tab10)
wtd_elpd_se_tab10$model = "model10"

wtd_elpd_se_tab = bind_rows(wtd_elpd_se_tab5,
                            wtd_elpd_se_tab6,
                            wtd_elpd_se_tab7,
                            wtd_elpd_se_tab8,
                            wtd_elpd_se_tab9,
                            wtd_elpd_se_tab10)

## calculating upper and lower bound of the elpd values
wtd_elpd_se_tab$low_elpd = wtd_elpd_se_tab[,'total'] - 1.64*wtd_elpd_se_tab[,'SE']
wtd_elpd_se_tab$upp_elpd = wtd_elpd_se_tab[,'total'] + 1.64*wtd_elpd_se_tab[,'SE']
wtd_elpd_se_tab$iter = rep(1:length(samp_data), 6)

#Example plot
ggplot(wtd_elpd_se_tab, aes(x = total, y = model, group = iter, colour = model))+
  geom_vline(aes(xintercept = 0)) +
  xlim(c(-3800, 2200)) +
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

ggsave("plot_loo_diff_0510.pdf", g, width=6, height=7.5, units="in", device="pdf")


ph2 = wtd_elpd_se_tab #weighted

# non-weighted elpd -------------------------------------------------------
elpd_se_tab05 = sapply(loo_diff_05, sum) %>% as.data.frame() %>% set_colnames(., 'total')
elpd_se_tab05$sd = sapply(loo_diff_05, sd) %>% as.numeric() 
elpd_se_tab05$SE = as.numeric(sqrt(500) * elpd_se_tab05$sd)
elpd_se_tab05$model = 'model05'

elpd_se_tab06 = sapply(loo_diff_06, sum) %>% as.data.frame() %>% set_colnames(., 'total')
elpd_se_tab06$sd = sapply(loo_diff_06, sd) %>% as.numeric() 
elpd_se_tab06$SE = as.numeric(sqrt(500) * elpd_se_tab06$sd)
elpd_se_tab06$model = 'model06'

elpd_se_tab07= sapply(loo_diff_07, sum) %>% as.data.frame() %>% set_colnames(., 'total')
elpd_se_tab07$sd = sapply(loo_diff_07, sd) %>% as.numeric() 
elpd_se_tab07$SE = as.numeric(sqrt(500) * elpd_se_tab07$sd)
elpd_se_tab07$model = 'model07'

elpd_se_tab08 = sapply(loo_diff_08, sum) %>% as.data.frame() %>% set_colnames(., 'total')
elpd_se_tab08$sd = sapply(loo_diff_08, sd) %>% as.numeric() 
elpd_se_tab08$SE = as.numeric(sqrt(500) * elpd_se_tab08$sd) 
elpd_se_tab08$model = 'model08'

elpd_se_tab09 = sapply(loo_diff_09, sum) %>% as.data.frame() %>% set_colnames(., 'total')
elpd_se_tab09$sd = sapply(loo_diff_09, sd) %>% as.numeric() 
elpd_se_tab09$SE = as.numeric(sqrt(500) * elpd_se_tab09$sd) 
elpd_se_tab09$model = 'model09'

elpd_se_tab10 = sapply(loo_diff_10, sum) %>% as.data.frame() %>% set_colnames(., 'total')
elpd_se_tab10$sd = sapply(loo_diff_10, sd) %>% as.numeric() 
elpd_se_tab10$SE = as.numeric(sqrt(500) * elpd_se_tab10$sd) 
elpd_se_tab10$model = 'model10'

elpd_se_tab = bind_rows(elpd_se_tab05,
                        elpd_se_tab06,
                        elpd_se_tab07,
                        elpd_se_tab08,
                        elpd_se_tab09,
                        elpd_se_tab10)

elpd_se_tab$low_elpd = elpd_se_tab[,'total'] - 1.64*elpd_se_tab[,'SE'] 
elpd_se_tab$upp_elpd = elpd_se_tab[,'total'] + 1.64*elpd_se_tab[,'SE'] 
elpd_se_tab$iter = rep(1:length(samp_data), 6)


#Example plot
ggplot(elpd_se_tab, aes(x = total, y = model, group = iter, colour = model))+
  geom_vline(aes(xintercept = 0), size=1) +
  xlim(c(-200, 100)) +
  geom_point(position = position_dodge(width = .5)) +
  geom_errorbarh(mapping = aes(xmin = low_elpd, 
                               xmax = upp_elpd), 
                 position = position_dodge(width = .5),
                 height = 0, alpha = .5) +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  annotate("label", x = -50, y = 0.5, label = "Model 15 preferred") +
  annotate("label", x = 50, y = 0.5, label = "Alt model preferred") +
  scale_y_discrete(limits = rev) +
  scale_colour_manual(values = pals::tableau20(20)[c(1,2,9,10,3,4,7,8,13,14,5,6,17,18)]) + 
  labs(title="Difference in elpd values") 

pu2 = elpd_se_tab #unweighted

# models 11-15 ------------------------------------------------------------
load("~/GitHub/LOO_MRP/02-super popn experiment/01-code/popn1/experiment3f/simulated100temp_3.RData")
sim_list2 = sim_trip_list[1:65]
samp_data = samp_data_list[1:65]

## extracting elpd_loo and calculating difference with model15
elpd_tab = sim_list2 %>% 
  lapply(., function(x)(x[,'elpd_loo'])) %>% 
  do.call(rbind, .) %>%   
  as.data.frame() %>% 
  set_colnames(c(paste0('elpd',11:15))) %>% 
  mutate(model11 = .[,1] - .[,'elpd15'],
         model12 = .[,2] - .[,'elpd15'],
         model13 = .[,3] - .[,'elpd15'],
         model14 = .[,4] - .[,'elpd15'])

## se for loo_diff
sqrt(500) * sd(samp_data[[1]]$elpd_11 - samp_data[[1]]$elpd_15)

## calculate SE for weighted loo_diff
# calculating svy_rake weights for each sample
svy_rake_list = lapply(samp_data, function(x)svydesign(ids=~1, # cluster id, ~1 for no clusters
                                                       weights=~wts, # including raked weights in the survey design
                                                       data=x))
wtd_elpd_se_tab11 = list()
wtd_elpd_se_tab12 = list()
wtd_elpd_se_tab13 = list()
wtd_elpd_se_tab14 = list()

loo_diff_11 = list()
loo_diff_12 = list()
loo_diff_13 = list()
loo_diff_14 = list()

## calculating weighted LOO values and its SE
for(i in 1:length(samp_data)){
  loo_diff_11[[i]] = samp_data[[i]]$elpd_11 - samp_data[[i]]$elpd_15
  wtd_elpd_se_tab11[[i]] = svytotal(loo_diff_11[[i]], svy_rake_list[[i]]) %>% 
    as.data.frame(.)
  
  loo_diff_12[[i]] = samp_data[[i]]$elpd_12 - samp_data[[i]]$elpd_15
  wtd_elpd_se_tab12[[i]] = svytotal(loo_diff_12[[i]], svy_rake_list[[i]]) %>% 
    as.data.frame(.)
  
  loo_diff_13[[i]] = samp_data[[i]]$elpd_13 - samp_data[[i]]$elpd_15
  wtd_elpd_se_tab13[[i]] = svytotal(loo_diff_13[[i]], svy_rake_list[[i]]) %>% 
    as.data.frame(.)
  
  loo_diff_14[[i]] = samp_data[[i]]$elpd_14 - samp_data[[i]]$elpd_15
  wtd_elpd_se_tab14[[i]] = svytotal(loo_diff_14[[i]], svy_rake_list[[i]]) %>% 
    as.data.frame(.)
}

# weighted elpd -----------------------------------------------------------
wtd_elpd_se_tab11 = do.call(rbind, wtd_elpd_se_tab11)
wtd_elpd_se_tab11$model = "model11"

wtd_elpd_se_tab12 = do.call(rbind, wtd_elpd_se_tab12)
wtd_elpd_se_tab12$model = "model12"

wtd_elpd_se_tab13 = do.call(rbind, wtd_elpd_se_tab13)
wtd_elpd_se_tab13$model = "model13"

wtd_elpd_se_tab14 = do.call(rbind, wtd_elpd_se_tab14)
wtd_elpd_se_tab14$model = "model14"

wtd_elpd_se_tab = bind_rows(wtd_elpd_se_tab11,
                            wtd_elpd_se_tab12,
                            wtd_elpd_se_tab13,
                            wtd_elpd_se_tab14)

wtd_elpd_se_tab$low_elpd = wtd_elpd_se_tab[,'total'] - 1.64*wtd_elpd_se_tab[,'SE']
wtd_elpd_se_tab$upp_elpd = wtd_elpd_se_tab[,'total'] + 1.64*wtd_elpd_se_tab[,'SE']
wtd_elpd_se_tab$iter = rep(1:length(samp_data), 4)

## combining with other models
ph3 = wtd_elpd_se_tab
ph = bind_rows(ph1,ph2,ph3)


#Example plot
ggplot(ph, aes(x = total, y = model, group = iter, colour = model))+
  geom_vline(aes(xintercept = 0), size=1) +
  xlim(c(-3800, 2200)) +
  geom_point(position = position_dodge(width = .5)) +
  geom_errorbarh(mapping = aes(xmin = low_elpd, 
                               xmax = upp_elpd), 
                 position = position_dodge(width = .5),
                 height = 0, alpha = .5) +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  annotate("label", x = -1000, y = 0.8, label = "Model 15 preferred") +
  annotate("label", x = 1000, y = 0.8, label = "Alt model preferred") +
  scale_y_discrete(limits = rev) +
  scale_colour_manual(values = pals::tableau20(20)[c(1,2,9,10,3,4,7,8,13,14,5,6,17,18)]) + 
  labs(title="Difference in elpd values (weighted)") 

ggsave("plot_loo_diff_wtd.pdf", g, width=6, height=7.5, units="in", device="pdf")

# violin plot
ggplot(ph, aes(group = model, fill = model))+
  geom_vline(aes(xintercept = 0), size=1) +
  xlim(c(-3800, 2200)) +
  geom_violin(aes(x = low_elpd, y = model),alpha=0.3) +
  geom_violin(aes(x = upp_elpd, y = model), alpha=0.3) +
  geom_violin(aes(x = total, y = model), alpha=1) +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  annotate("label", x = -1000, y = 0.8, label = "Model 15 preferred") +
  annotate("label", x = 1000, y = 0.8, label = "Alt model preferred") +
  scale_y_discrete(limits = rev) +
  scale_fill_manual(values = pals::tableau20(20)[c(1,2,9,10,3,4,7,8,13,14,5,6,17,18)]) + 
  labs(title="Difference in elpd values (weighted)") 

ggsave("plot_loo_diff_wtd_violin.pdf", gv, width=6, height=7.5, units="in", device="pdf")



# non-weighted elpd -------------------------------------------------------
elpd_se_tab11 = sapply(loo_diff_11, sum) %>% as.data.frame() %>% set_colnames(., 'total')
elpd_se_tab11$sd = sapply(loo_diff_11, sd) %>% as.numeric() 
elpd_se_tab11$SE = as.numeric(sqrt(500) * elpd_se_tab11$sd)
elpd_se_tab11$model = 'model11'

elpd_se_tab12 = sapply(loo_diff_12, sum) %>% as.data.frame() %>% set_colnames(., 'total')
elpd_se_tab12$sd = sapply(loo_diff_12, sd) %>% as.numeric() 
elpd_se_tab12$SE = as.numeric(sqrt(500) * elpd_se_tab12$sd)
elpd_se_tab12$model = 'model12'

elpd_se_tab13= sapply(loo_diff_13, sum) %>% as.data.frame() %>% set_colnames(., 'total')
elpd_se_tab13$sd = sapply(loo_diff_13, sd) %>% as.numeric() 
elpd_se_tab13$SE = as.numeric(sqrt(500) * elpd_se_tab13$sd)
elpd_se_tab13$model = 'model13'

elpd_se_tab14 = sapply(loo_diff_14, sum) %>% as.data.frame() %>% set_colnames(., 'total')
elpd_se_tab14$sd = sapply(loo_diff_14, sd) %>% as.numeric() 
elpd_se_tab14$SE = as.numeric(sqrt(500) * elpd_se_tab14$sd) 
elpd_se_tab14$model = 'model14'

elpd_se_tab = bind_rows(elpd_se_tab11,
                        elpd_se_tab12,
                        elpd_se_tab13,
                        elpd_se_tab14)

elpd_se_tab$low_elpd = elpd_se_tab[,'total'] - 1.64*elpd_se_tab[,'SE'] 
elpd_se_tab$upp_elpd = elpd_se_tab[,'total'] + 1.64*elpd_se_tab[,'SE'] 
elpd_se_tab$iter = rep(1:length(samp_data), 4)

#Example plot
ggplot(elpd_se_tab, aes(x = total, y = model, group = iter, colour = model))+
  geom_vline(aes(xintercept = 0), size=1) +
  xlim(c(-150, 50)) +
  geom_point(position = position_dodge(width = .5)) +
  geom_errorbarh(mapping = aes(xmin = low_elpd, 
                               xmax = upp_elpd), 
                 position = position_dodge(width = .5),
                 height = 0, alpha = .5) +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  annotate("label", x = -35, y = 0.8, label = "Model 15 preferred") +
  annotate("label", x = 35, y = 0.8, label = "Alt model preferred") +
  scale_y_discrete(limits = rev) +
  scale_colour_manual(values = pals::tableau20(20)[c(1,2,9,10,3,4,7,8,13,14,5,6,17,18)]) + 
  labs(title="Difference in elpd values") 

pu3 = elpd_se_tab

pu = bind_rows(pu1, pu2, pu3)

## plotting all models 
g2 = ggplot(pu, aes(x = total, y = model, group = iter, colour = model))+
  geom_vline(aes(xintercept = 0), size=1) +
  xlim(c(-200, 100)) +
  geom_point(position = position_dodge(width = .5)) +
  geom_errorbarh(mapping = aes(xmin = low_elpd, 
                               xmax = upp_elpd), 
                 position = position_dodge(width = .5),
                 height = 0, alpha = .5) +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  annotate("label", x = -50, y = 0.8, label = "Model 15 preferred") +
  annotate("label", x = 50, y = 0.8, label = "Alt model preferred") +
  scale_y_discrete(limits = rev) +
  scale_colour_manual(values = pals::tableau20(20)[c(1,2,9,10,3,4,7,8,13,14,5,6,17,18)]) + 
  labs(title="Difference in elpd values (unweighted)") 

ggsave("plot_loo_diff_unwtd.pdf", g2, width=6, height=7.5, units="in", device="pdf")

## violin plot
g3 = ggplot(pu, aes(group = model, fill = model))+
  geom_vline(aes(xintercept = 0), size=1) +
  xlim(c(-200, 100)) +
  geom_violin(aes(x = low_elpd, y = model),alpha=0.3) +
  geom_violin(aes(x = upp_elpd, y = model), alpha=0.3) +
  geom_violin(aes(x = total, y = model), alpha=1) +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  annotate("label", x = -50, y = 0.8, label = "Model 15 preferred") +
  annotate("label", x = 50, y = 0.8, label = "Alt model preferred") +
  scale_y_discrete(limits = rev) +
  scale_fill_manual(values = pals::tableau20(20)[c(1,2,9,10,3,4,7,8,13,14,5,6,17,18)]) + 
  labs(title="Difference in elpd values (unweighted)") 
ggsave("plot_loo_diff_unwtd_violin.pdf", g3, width=6, height=7.5, units="in", device="pdf")


