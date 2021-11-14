## plotting the elpd values against 'best' model 
library(ggplot2)
library(reshape2)
library(dplyr)
library(magrittr)
library(survey)

load("~/GitHub/LOO_MRP/02-super popn experiment/03-results/experiment3e/simulated100_3.RData")
sim_list2 = sim_trip_list

samp_data = samp_data_list

## difference in elpd_loo values
sim_list2[[1]][,'elpd_loo']

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
g = ggplot(ph, aes(x = total, y = model, group = iter, colour = model))+
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




