## plotting the elpd values against 'best' model 
library(ggplot2)
library(reshape2)
library(dplyr)
library(magrittr)
library(survey)

load("~/GitHub/LOO_MRP/02-super popn experiment/03-results/experiment3e/simulated100_1.RData")


## difference in elpd_loo values
sim_list[[1]][,'elpd_loo']

## extracting elpd_loo and calculating difference with model15
elpd_tab = sim_list %>% 
  lapply(., function(x)(x[,'elpd_loo'])) %>% 
  do.call(rbind, .) %>%   
  as.data.frame() %>% 
  set_colnames(c(paste0('elpd0',1:4), "elpd15")) %>% 
  mutate(model01 = .[,1] - .[,'elpd15'], 
         model02 = .[,2] - .[,'elpd15'],
         model03 = .[,3] - .[,'elpd15'],
         model04 = .[,4] - .[,'elpd15'])

## se for loo_diff
sqrt(500) * sd(samp_data_list[[1]]$elpd_1 - samp_data_list[[1]]$elpd_15)

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

loo_diff_01 = list()
loo_diff_02 = list()
loo_diff_03 = list()
loo_diff_04 = list()

## calculating weighted LOO values and its SE
for(i in 1:length(samp_data_list)){
  loo_diff_01[[i]] = samp_data_list[[i]]$elpd_1 - samp_data_list[[i]]$elpd_15
 wtd_elpd_se_tab1[[i]] = svytotal(loo_diff_01[[i]], svy_rake_list[[i]]) %>% 
    as.data.frame(.)
 
 loo_diff_02[[i]] = samp_data_list[[i]]$elpd_2 - samp_data_list[[i]]$elpd_15
 wtd_elpd_se_tab2[[i]] = svytotal(loo_diff_02[[i]], svy_rake_list[[i]]) %>% 
   as.data.frame(.)
 
 loo_diff_03[[i]] = samp_data_list[[i]]$elpd_3 - samp_data_list[[i]]$elpd_15
 wtd_elpd_se_tab3[[i]] = svytotal(loo_diff_03[[i]], svy_rake_list[[i]]) %>% 
   as.data.frame(.)
 
 loo_diff_04[[i]] = samp_data_list[[i]]$elpd_4 - samp_data_list[[i]]$elpd_15
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
wtd_elpd_se_tab$iter = rep(1:length(samp_data_list), 4)

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
elpd_se_tab$iter = rep(1:length(samp_data_list), 4)


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




  
