## plotting the elpd values against 'best' model 
library(ggplot2)
library(reshape2)
library(dplyr)
library(magrittr)
library(survey)

load("~/GitHub/LOO_MRP/02-super popn experiment/03-results/experiment3/simulated29temp_3.RData")
sim_list2 = sim_trip_list[1:29]

samp_data = samp_data_list[1:29]

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
elpd_se_tab11 = list()
elpd_se_tab12 = list()
elpd_se_tab13 = list()
elpd_se_tab14 = list()

for(i in 1:length(samp_data)){
  loo_diff_11 = samp_data[[i]]$elpd_11 - samp_data[[i]]$elpd_15
  elpd_se_tab11[[i]] = svytotal(loo_diff_11, svy_rake_list[[i]]) %>% 
    as.data.frame(.)
  
  loo_diff_12 = samp_data[[i]]$elpd_12 - samp_data[[i]]$elpd_15
  elpd_se_tab12[[i]] = svytotal(loo_diff_12, svy_rake_list[[i]]) %>% 
    as.data.frame(.)
  
  loo_diff_13 = samp_data[[i]]$elpd_13 - samp_data[[i]]$elpd_15
  elpd_se_tab13[[i]] = svytotal(loo_diff_13, svy_rake_list[[i]]) %>% 
    as.data.frame(.)
  
  loo_diff_14 = samp_data[[i]]$elpd_14 - samp_data[[i]]$elpd_15
  elpd_se_tab14[[i]] = svytotal(loo_diff_14, svy_rake_list[[i]]) %>% 
    as.data.frame(.)
}

elpd_se_tab11 = do.call(rbind, elpd_se_tab11)
elpd_se_tab11$model = "model11"

elpd_se_tab12 = do.call(rbind, elpd_se_tab12)
elpd_se_tab12$model = "model12"

elpd_se_tab13 = do.call(rbind, elpd_se_tab13)
elpd_se_tab13$model = "model13"

elpd_se_tab14 = do.call(rbind, elpd_se_tab14)
elpd_se_tab14$model = "model14"

elpd_se_tab = bind_rows(elpd_se_tab11,
                        elpd_se_tab12,
                        elpd_se_tab13,
                        elpd_se_tab14)

elpd_se_tab$low_elpd = elpd_se_tab[,'total'] - 1.64*elpd_se_tab[,'SE']
elpd_se_tab$upp_elpd = elpd_se_tab[,'total'] + 1.64*elpd_se_tab[,'SE']
elpd_se_tab$iter = rep(1:length(samp_data), 4)

## combining with other models
ph3 = elpd_se_tab
ph = bind_rows(ph1,ph2,ph3)


#Example plot
g = ggplot(ph, aes(x = total, y = model, group = iter, colour = model))+
  geom_vline(aes(xintercept = 0)) +
  xlim(c(-2500, 1500)) +
  geom_point(position = position_dodge(width = .5)) +
  geom_errorbarh(mapping = aes(xmin = low_elpd, 
                               xmax = upp_elpd), 
                 position = position_dodge(width = .5),
                 height = 0, alpha = .7) +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  annotate("label", x = -1000, y = 0.8, label = "Model 15 preferred") +
  annotate("label", x = 1000, y = 0.8, label = "Alt model preferred") +
  scale_y_discrete(limits = rev) +
  scale_colour_manual(values = pals::tableau20(20)[c(1,2,9,10,3,4,7,8,13,14,5,6,17,18)]) + 
  labs(title="Difference in elpd values") 
  
ggsave("plot_loo_diff.pdf", g, width=6, height=7.5, units="in", device="pdf")






