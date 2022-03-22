## looking at individual prediction 
library(dplyr)
library(ggplot2)

# sourcing the other read file
load(here::here('02-super popn approach/experiment3/03-data/experiment3c/loo_fx3.RData'))

pt_samp_list = cov_prop_samp_list = 
  list()

for(ite in c(1:100)){
  pt_samp_list[[ite]] = samp_data_list[[ite]]$y_prob

  # subtracting the truth for each of the models
  ## getting posterior of individuals estimate for each of the models
  
  sampest_tab_06 = apply(sampest_06,2,function(x)quantile(x,c(0.05, 0.5, 0.95))) %>% # posterior of sample prediction
    t() %>% 
    as_tibble() %>% 
    rename(low_int = '5%',
           upp_int = '95%', 
           mean_int = '50%') %>% 
    mutate( range_int = upp_int - low_int,
            prob_truth = pt_samp_list[[ite]],  # truth
            coverage = ifelse(prob_truth >= low_int & prob_truth <= upp_int, 1, 0),
            model = 'model06',
            iter = ite)
  
  sampest_tab_11 = apply(sampest_11,2,function(x)quantile(x,c(0.05, 0.5, 0.95))) %>% 
    t() %>% 
    as_tibble() %>% 
    rename(low_int = '5%',
           upp_int = '95%', 
           mean_int = '50%') %>% 
    mutate( range_int = upp_int - low_int,
            prob_truth = pt_samp_list[[ite]],  # truth
            coverage = ifelse(prob_truth >= low_int & prob_truth <= upp_int, 1, 0),
            model = 'model11',
            iter = ite)
  
  sampest_tab_13 = apply(sampest_13,2,function(x)quantile(x,c(0.05, 0.5, 0.95))) %>% 
    t() %>% 
    as_tibble() %>% 
    rename(low_int = '5%',
           upp_int = '95%', 
           mean_int = '50%') %>% 
    mutate( range_int = upp_int - low_int,
            prob_truth = pt_samp_list[[ite]],  # truth
            coverage = ifelse(prob_truth >= low_int & prob_truth <= upp_int, 1, 0),
            model = 'model13',
            iter = ite)
  
  sampest_tab_13a = apply(sampest_13a,2,function(x)quantile(x,c(0.05, 0.5, 0.95))) %>% 
    t() %>% 
    as_tibble() %>% 
    rename(low_int = '5%',
           upp_int = '95%', 
           mean_int = '50%') %>% 
    mutate( range_int = upp_int - low_int,
            prob_truth = pt_samp_list[[ite]],  # truth
            coverage = ifelse(prob_truth >= low_int & prob_truth <= upp_int, 1, 0),
            model = 'model13a',
            iter = ite)
  
  sampest_tab_15 = apply(sampest_15,2,function(x)quantile(x,c(0.05, 0.5, 0.95))) %>% 
    t() %>% 
    as_tibble() %>% 
    rename(low_int = '5%',
           upp_int = '95%', 
           mean_int = '50%') %>% 
    mutate( range_int = upp_int - low_int,
            prob_truth = pt_samp_list[[ite]],  # truth
            coverage = ifelse(prob_truth >= low_int & prob_truth <= upp_int, 1, 0),
            model = 'model15',
            iter = ite)
  
  sampest_tab_15a = apply(sampest_15a,2,function(x)quantile(x,c(0.05, 0.5, 0.95))) %>% 
    t() %>% 
    as_tibble() %>% 
    rename(low_int = '5%',
           upp_int = '95%', 
           mean_int = '50%') %>% 
    mutate( range_int = upp_int - low_int,
            prob_truth = pt_samp_list[[ite]],  # truth
            coverage = ifelse(prob_truth >= low_int & prob_truth <= upp_int, 1, 0),
            model = 'model15a',
            iter = ite)
  
  sampest_list_all = list(sampest_tab_06,
                         sampest_tab_11,
                         sampest_tab_13,
                         sampest_tab_13a,
                         sampest_tab_15,
                         sampest_tab_15a)
  
  sampest_tab_all = do.call(rbind,sampest_list_all)
  
  cov_prop_samp_list[[ite]] = sampest_tab_all %>% 
    group_by(model) %>% 
    summarise(mean_coverage = mean(coverage))
    
}

cov_prop_samp_tab = do.call(rbind,cov_prop_samp_list) %>% 
  mutate(iter = rep(1:100,each = 6))


pc2 = cov_prop_samp_tab %>% 
  mutate(model = factor(model)) %>% 
  mutate(model = plyr::revalue(model, c('model06' = 'X1 + X3',
                                        'model11' = 'X1 + X2 + X3',  
                                        'model13' = 'X1 + X3 + X4', 
                                        'model13a' = '*X1 + X3 + X4', 
                                        'model15' = 'X1 + X2 + X3 + X4',
                                        'model15a' = '*X1 + X2 + X3 + X4')))
pc2$model = forcats::fct_relevel(pc2$model, c('X1 + X2 + X3 + X4', '*X1 + X2 + X3 + X4', 
                                     'X1 + X3 + X4', '*X1 + X3 + X4',
                                     'X1 + X2 + X3',
                                     'X1 + X3'))

## plot diff in mean
xloc4 = 0.1
(p1 = ggplot(pc2, aes(x = mean_coverage, y = model, group = iter, colour = model))+
  geom_point(position = position_dodge(width = .5), alpha=0.7) +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  scale_y_discrete(limits = rev) +
  scale_colour_manual(values = c("#1C73B1FF", "#26456EFF",
                                 "#FB964EFF", "#DF5948FF",
                                 "#09622AFF",
                                 "#879195FF")) + 
  labs(title="Mean proportion of coverage in individual-level prediction") +
  xlim(c(0.05, 0.35)) +
  annotate("label", x = xloc4, y = 5.5, label = "X2 and X4") +
  annotate("label", x = xloc4, y = 3.5, label = "X4 only") +
  annotate("label", x = xloc4, y = 2,  label = "X2 only") +
  annotate("label", x = 0.27, y = 1, label = "None") )

ggsave(here::here("02-super popn approach/experiment3/02-results/stan code/plot_ind_est_fx3.png"), p1, width=6, height=7.5, units="in", device="png")


