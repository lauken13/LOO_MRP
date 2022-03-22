## calculating svymean mean of elpd as opposed to sum
library(ggplot2)
library(reshape2)
library(dplyr)
library(magrittr)
library(survey)

## reading individual Rdata files
source("~/GitHub/LOO_MRP/02-super popn experiment/01-code/popn2/experiment3b/read_bash_loop.R")

## only selected files (2 didn't work)
samp_data_list2 = samp_data_list[c(1:64,66:76,78:100)]
n = nrow(samp_data_list2[[1]]) # number of samples

## extracting elpd_loo 
elpd_tab = samp_data_list2 %>% 
  lapply( ., function(x) x[,grep('elpd_*', colnames(x))] ) %>% 
  do.call(rbind, .) %>%   
  as.data.frame() %>% 
  set_colnames(c(paste0('elpd0',1:9), paste0('elpd',10:15)))

# weighted elpd -----------------------------------------------------------
## calculate SE for weighted loo_diff
# calculating svy_rake weights for each sample
svy_rake_list = lapply(samp_data_list2, function(x)svydesign(ids=~1, # cluster id, ~1 for no clusters
                                                            weights=~wts, # including raked weights in the survey design
                                                            data=x))
## calculating mean of elpds
wtd_elpd_se_tab1 = wtd_elpd_se_tab2 = wtd_elpd_se_tab3 = 
  wtd_elpd_se_tab4 = wtd_elpd_se_tab5 = wtd_elpd_se_tab6 = 
  wtd_elpd_se_tab7 = wtd_elpd_se_tab8 = wtd_elpd_se_tab9 = 
  wtd_elpd_se_tab10 = wtd_elpd_se_tab11 = wtd_elpd_se_tab12 = 
  wtd_elpd_se_tab13 = wtd_elpd_se_tab14 = wtd_elpd_se_tab15 = list()

for(i in 1:length(samp_data_list2)){
  wtd_elpd_se_tab1[[i]] = svymean(samp_data_list2[[i]]$elpd_1, svy_rake_list[[i]]) %>% 
    as.data.frame(.)
  
  wtd_elpd_se_tab2[[i]] = svymean(samp_data_list2[[i]]$elpd_2, svy_rake_list[[i]]) %>% 
    as.data.frame(.)
  
  wtd_elpd_se_tab3[[i]] = svymean(samp_data_list2[[i]]$elpd_3, svy_rake_list[[i]]) %>% 
    as.data.frame(.)
  
  wtd_elpd_se_tab4[[i]] = svymean(samp_data_list2[[i]]$elpd_4, svy_rake_list[[i]]) %>% 
    as.data.frame(.)
  
  wtd_elpd_se_tab5[[i]] = svymean(samp_data_list2[[i]]$elpd_5 , svy_rake_list[[i]]) %>% 
    as.data.frame(.)
  
  wtd_elpd_se_tab6[[i]] = svymean(samp_data_list2[[i]]$elpd_6, svy_rake_list[[i]]) %>% 
    as.data.frame(.)
  
  wtd_elpd_se_tab7[[i]] = svymean(samp_data_list2[[i]]$elpd_7, svy_rake_list[[i]]) %>% 
    as.data.frame(.)
  
  wtd_elpd_se_tab8[[i]] = svymean(samp_data_list2[[i]]$elpd_8, svy_rake_list[[i]]) %>% 
    as.data.frame(.)
  
  wtd_elpd_se_tab9[[i]] = svymean(samp_data_list2[[i]]$elpd_9, svy_rake_list[[i]]) %>% 
    as.data.frame(.)
  
  wtd_elpd_se_tab10[[i]] = svymean(samp_data_list2[[i]]$elpd_10, svy_rake_list[[i]]) %>% 
    as.data.frame(.)
  
  wtd_elpd_se_tab11[[i]] = svymean(samp_data_list2[[i]]$elpd_11, svy_rake_list[[i]]) %>% 
    as.data.frame(.)
  
  wtd_elpd_se_tab12[[i]] = svymean(samp_data_list2[[i]]$elpd_12, svy_rake_list[[i]]) %>% 
    as.data.frame(.)
  
  wtd_elpd_se_tab13[[i]] = svymean(samp_data_list2[[i]]$elpd_13, svy_rake_list[[i]]) %>% 
    as.data.frame(.)
  
  wtd_elpd_se_tab14[[i]] = svymean(samp_data_list2[[i]]$elpd_14 , svy_rake_list[[i]]) %>% 
    as.data.frame(.)
  
  wtd_elpd_se_tab15[[i]] = svymean(samp_data_list2[[i]]$elpd_15 , svy_rake_list[[i]]) %>% 
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

wtd_elpd_se_tab = bind_rows(wtd_elpd_se_tab1,wtd_elpd_se_tab2,wtd_elpd_se_tab3,
                            wtd_elpd_se_tab4,wtd_elpd_se_tab5,wtd_elpd_se_tab6,
                            wtd_elpd_se_tab7,wtd_elpd_se_tab8,wtd_elpd_se_tab9,
                            wtd_elpd_se_tab10,wtd_elpd_se_tab11,wtd_elpd_se_tab12,
                            wtd_elpd_se_tab13,wtd_elpd_se_tab14,wtd_elpd_se_tab15) %>% 
  mutate(low_elpd = .$mean - (.$SE*1.64), # calculating upper and lower bound of the elpd values
         upp_elpd = .$mean + (.$SE*1.64),
         iter = rep(1:length(samp_data_list2), 15))

# renaming models
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
                                  'model15' = 'X1 + X2 + X3 + X4'))) 
ph$model = fct_relevel(ph$model, c('X2 + X4', 'X1 + X2 + X4', 'X2 + X3 + X4', 'X1 + X2 + X3 + X4', 
                                   'X2', 'X1 + X2', 'X2 + X3', 'X1 + X2 + X3',
                                   'X4', 'X1 + X4', 'X3 + X4', 'X1 + X3 + X4',
                                   'X1', 'X3', 'X1 + X3'))


# plot ------------------------------------------------------------
g = ggplot(ph, aes(x = mean, y = model, group = iter, colour = model))+
  geom_point(position = position_dodge(width = .5)) +
  geom_errorbarh(mapping = aes(xmin = low_elpd, 
                               xmax = upp_elpd), 
                 position = position_dodge(width = .5),
                 height = 0, alpha = .5) +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  scale_y_discrete(limits = rev) +
  scale_colour_manual(values = pals::tableau20(20)[c(1,2,9,10,3,4,7,8,13,14,5,6,17,18,12)]) + 
  labs(title="Raw mean elpd values (weighted)") +
  annotate("label", x = -0.7, y = 13.5, label = "X2 and X4") +
  annotate("label", x = -0.4, y = 9.5, label = "X2 only") +
  annotate("label", x = -0.4, y = 5.5, label = "X4 only") +
  annotate("label", x = -0.4, y = 2, label = "None") 

ggsave("plot_loo_mean_raw_wtd.png", g, width=6, height=7.5, units="in", device="png")

# non-weighted mean of elpd -------------------------------------------------------
elpd_se_tab01 = samp_data_list2 %>% 
  sapply(., function(x)mean(x$elpd_1)) %>% 
  as.data.frame() %>% 
  set_colnames(., 'mean')  %>% 
  mutate(SE =  sapply(samp_data_list2, function(x) sqrt(n) * sd(x$elpd_1)),
         model = 'model01')

elpd_se_tab02 = samp_data_list2 %>% 
  sapply(., function(x)mean(x$elpd_2)) %>% 
  as.data.frame() %>% 
  set_colnames(., 'mean')  %>% 
  mutate(SE =  sapply(samp_data_list2, function(x) sqrt(n) * sd(x$elpd_2)),
         model = 'model02')

elpd_se_tab03 = samp_data_list2 %>% 
  sapply(., function(x)mean(x$elpd_3)) %>% 
  as.data.frame() %>% 
  set_colnames(., 'mean')  %>% 
  mutate(SE =  sapply(samp_data_list2, function(x) sqrt(n) * sd(x$elpd_3)),
         model = 'model03')

elpd_se_tab04 = samp_data_list2 %>% 
  sapply(., function(x)mean(x$elpd_4)) %>% 
  as.data.frame() %>% 
  set_colnames(., 'mean')  %>% 
  mutate(SE =  sapply(samp_data_list2, function(x) sqrt(n) * sd(x$elpd_4)),
         model = 'model04')

elpd_se_tab05 = samp_data_list2 %>% 
  sapply(., function(x)mean(x$elpd_5)) %>% 
  as.data.frame() %>% 
  set_colnames(., 'mean')  %>% 
  mutate(SE =  sapply(samp_data_list2, function(x) sqrt(n) * sd(x$elpd_5)),
         model = 'model05')

elpd_se_tab06 = samp_data_list2 %>% 
  sapply(., function(x)mean(x$elpd_6)) %>% 
  as.data.frame() %>% 
  set_colnames(., 'mean')  %>% 
  mutate(SE =  sapply(samp_data_list2, function(x) sqrt(n) * sd(x$elpd_6)),
         model = 'model06')

elpd_se_tab07 = samp_data_list2 %>% 
  sapply(., function(x)mean(x$elpd_7)) %>% 
  as.data.frame() %>% 
  set_colnames(., 'mean')  %>% 
  mutate(SE =  sapply(samp_data_list2, function(x) sqrt(n) * sd(x$elpd_7)),
         model = 'model07')

elpd_se_tab08 = samp_data_list2 %>% 
  sapply(., function(x)mean(x$elpd_8)) %>% 
  as.data.frame() %>% 
  set_colnames(., 'mean')  %>% 
  mutate(SE =  sapply(samp_data_list2, function(x) sqrt(n) * sd(x$elpd_8)),
         model = 'model08')

elpd_se_tab09 = samp_data_list2 %>% 
  sapply(., function(x)mean(x$elpd_9)) %>% 
  as.data.frame() %>% 
  set_colnames(., 'mean')  %>% 
  mutate(SE =  sapply(samp_data_list2, function(x) sqrt(n) * sd(x$elpd_9)),
         model = 'model09')

elpd_se_tab10 = samp_data_list2 %>% 
  sapply(., function(x)mean(x$elpd_10)) %>% 
  as.data.frame() %>% 
  set_colnames(., 'mean')  %>% 
  mutate(SE =  sapply(samp_data_list2, function(x) sqrt(n) * sd(x$elpd_10)),
         model = 'model10')

elpd_se_tab11 = samp_data_list2 %>% 
  sapply(., function(x)mean(x$elpd_11)) %>% 
  as.data.frame() %>% 
  set_colnames(., 'mean')  %>% 
  mutate(SE =  sapply(samp_data_list2, function(x) sqrt(n) * sd(x$elpd_11)),
         model = 'model11')

elpd_se_tab12 = samp_data_list2 %>% 
  sapply(., function(x)mean(x$elpd_12)) %>% 
  as.data.frame() %>% 
  set_colnames(., 'mean')  %>% 
  mutate(SE =  sapply(samp_data_list2, function(x) sqrt(n) * sd(x$elpd_12)),
         model = 'model12')

elpd_se_tab13 = samp_data_list2 %>% 
  sapply(., function(x)mean(x$elpd_13)) %>% 
  as.data.frame() %>% 
  set_colnames(., 'mean')  %>% 
  mutate(SE =  sapply(samp_data_list2, function(x) sqrt(n) * sd(x$elpd_13)),
         model = 'model13')

elpd_se_tab14 = samp_data_list2 %>% 
  sapply(., function(x)mean(x$elpd_14)) %>% 
  as.data.frame() %>% 
  set_colnames(., 'mean')  %>% 
  mutate(SE =  sapply(samp_data_list2, function(x) sqrt(n) * sd(x$elpd_14)),
         model = 'model14')

elpd_se_tab15 = samp_data_list2 %>% 
  sapply(., function(x)mean(x$elpd_15)) %>% 
  as.data.frame() %>% 
  set_colnames(., 'mean')  %>% 
  mutate(SE =  sapply(samp_data_list2, function(x) sqrt(n) * sd(x$elpd_15)),
         model = 'model15')

elpd_se_tab = bind_rows(elpd_se_tab01,elpd_se_tab02,elpd_se_tab03,
                        elpd_se_tab04,elpd_se_tab05,elpd_se_tab06,
                        elpd_se_tab07,elpd_se_tab08,elpd_se_tab09,
                        elpd_se_tab10,elpd_se_tab11,elpd_se_tab12,
                        elpd_se_tab13,elpd_se_tab14,elpd_se_tab15) %>% 
  mutate(low_elpd = .$mean - (.$SE*1.64), # calculating upper and lower bound of the elpd values
         upp_elpd = .$mean + (.$SE*1.64),
         iter = rep(1:length(samp_data_list2), 15))

# renaming model
pu = elpd_se_tab %>% 
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
                                   'X2', 'X1 + X2', 'X2 + X3', 'X1 + X2 + X3',
                                   'X4', 'X1 + X4', 'X3 + X4', 'X1 + X3 + X4',
                                   'X1', 'X3', 'X1 + X3'))


## plotting all models 
g2 = ggplot(pu, aes(x = mean, y = model, group = iter, colour = model))+
  geom_point(position = position_dodge(width = .5)) +
  geom_errorbarh(mapping = aes(xmin = low_elpd, 
                               xmax = upp_elpd), 
                 position = position_dodge(width = .5),
                 height = 0, alpha = .5) +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  scale_y_discrete(limits = rev) +
  scale_colour_manual(values = pals::tableau20(20)[c(1,2,9,10,3,4,7,8,13,14,5,6,17,18,12)]) + 
  labs(title="Raw mean elpd values (unweighted)") 

ggsave("plot_loo_mean_raw_unwtd.png", g2, width=6, height=7.5, units="in", device="png")
