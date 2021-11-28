## plotting the elpd values against 'best' model 
library(ggplot2)
library(reshape2)
library(dplyr)
library(magrittr)
library(survey)

load("~/GitHub/LOO_MRP/02-super popn experiment/03-results/experiment3/simulated12temp_1.RData")
sim_list2 = sim_list[1:12]

samp_data = samp_data_list[1:12]

## difference in elpd_loo values
sim_list2[[1]][,'elpd_loo']

## extracting elpd_loo and calculating difference with model15
elpd_tab = sim_list2 %>% 
  lapply(., function(x)(x[,'elpd_loo'])) %>% 
  do.call(rbind, .) %>%   
  as.data.frame() %>% 
  set_colnames(c(paste0('elpd0',1:4), "elpd15")) %>% 
  mutate(model01 = .[,1] - .[,'elpd15'], 
         model02 = .[,2] - .[,'elpd15'],
         model03 = .[,3] - .[,'elpd15'],
         model04 = .[,4] - .[,'elpd15'])

## se for loo_diff
sqrt(500) * sd(samp_data[[1]]$elpd_1 - samp_data[[1]]$elpd_15)

## calculate SE for weighted loo_diff
# calculating svy_rake weights for each sample
svy_rake_list = lapply(samp_data, function(x)svydesign(ids=~1, # cluster id, ~1 for no clusters
                                                       weights=~wts, # including raked weights in the survey design
                                                       data=x))
elpd_se_tab1 = list()
elpd_se_tab2 = list()
elpd_se_tab3 = list()
elpd_se_tab4 = list()

for(i in 1:length(samp_data)){
  loo_diff_1 = samp_data[[i]]$elpd_1 - samp_data[[i]]$elpd_15
 elpd_se_tab1[[i]] = svytotal(loo_diff_1, svy_rake_list[[i]]) %>% 
    as.data.frame(.)
 
 loo_diff_2 = samp_data[[i]]$elpd_2 - samp_data[[i]]$elpd_15
 elpd_se_tab2[[i]] = svytotal(loo_diff_2, svy_rake_list[[i]]) %>% 
   as.data.frame(.)
 
 loo_diff_3 = samp_data[[i]]$elpd_3 - samp_data[[i]]$elpd_15
 elpd_se_tab3[[i]] = svytotal(loo_diff_3, svy_rake_list[[i]]) %>% 
   as.data.frame(.)
 
 loo_diff_4 = samp_data[[i]]$elpd_4 - samp_data[[i]]$elpd_15
 elpd_se_tab4[[i]] = svytotal(loo_diff_4, svy_rake_list[[i]]) %>% 
   as.data.frame(.)
}

elpd_se_tab1 = do.call(rbind, elpd_se_tab1)
elpd_se_tab1$model = "model01"

elpd_se_tab2 = do.call(rbind, elpd_se_tab2)
elpd_se_tab2$model = "model02"

elpd_se_tab3 = do.call(rbind, elpd_se_tab3)
elpd_se_tab3$model = "model03"

elpd_se_tab4 = do.call(rbind, elpd_se_tab4)
elpd_se_tab4$model = "model04"

elpd_se_tab = bind_rows(elpd_se_tab1,
                        elpd_se_tab2,
                        elpd_se_tab3,
                        elpd_se_tab4)

elpd_se_tab$low_elpd = elpd_se_tab[,'total'] - 1.64*elpd_se_tab[,'SE']
elpd_se_tab$upp_elpd = elpd_se_tab[,'total'] + 1.64*elpd_se_tab[,'SE']
elpd_se_tab$iter = rep(1:length(samp_data), 4)

#Example plot
ggplot(elpd_se_tab, aes(x = total, y = model, group = iter, colour = model))+
  geom_vline(aes(xintercept = 0)) +
  xlim(c(-2500, 1500)) +
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
  scale_colour_manual(values = pals::tableau20(20)[c(1,2,9,10,3,4,7,8,13,14,5,6,17,18)]) 


ph1 = elpd_se_tab





  
