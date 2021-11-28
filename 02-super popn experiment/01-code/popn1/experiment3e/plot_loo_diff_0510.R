## plotting the elpd values against 'best' model 
library(ggplot2)
library(reshape2)
library(dplyr)
library(magrittr)
library(survey)

load("~/GitHub/LOO_MRP/02-super popn experiment/03-results/experiment3e/simulated100_2.RData")
sim_list2 = sim_doub_list
samp_data = samp_data_list

## difference in elpd_loo values
sim_list2[[1]][,'elpd_loo']

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

## se for loo_diff
sqrt(500) * sd(samp_data[[1]]$elpd_1 - samp_data[[1]]$elpd_15)

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

