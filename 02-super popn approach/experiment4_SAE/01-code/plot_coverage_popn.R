## looking at MRP prediction 
library(dplyr)
library(ggplot2)
library(brms)

## choose which set of data to load
load("~/GitHub/LOO_MRP/02-super popn approach/experiment4_SAE/03-data/loo_sae_fx3.RData")

# empty lists
pt_popn_list = cov_prop_popn_list = list()

iter = 1:100
# getting coverage for individual estimates
for(ite in 1:100){
  # # generating data using gen_dat()
  # set.seed(65438)
  # sim1 = gen_dat(N = 10000, fx = fx2, samp_size = 500, ITE=ite) # generate a list of things
  # 
  # pt_popn_list[[ite]] = mean(sim1$popn_data$y_prob)
  pt_popn_list[[ite]] = mean(popn_data_list[[ite]]$y_prob)

  # getting the popnest for each iteration
  popnest_tab = do.call(rbind, popnest_list)
  
  # subtracting the truth for each of the models
  ## getting posterior of individuals estimate for each of the models
  popnest_tab_06 = popnest_tab %>% 
    filter(model == 'model06') %>% 
    mutate(prob_truth = pt_popn_list[[ite]], # truth
           range_int = popnestX95 - popnestX5,
           coverage = ifelse(prob_truth >= popnestX5 & prob_truth <= popnestX95, 1, 0))
  
  popnest_tab_11 = popnest_tab %>% 
    filter(model == 'model11') %>% 
    mutate(prob_truth = pt_popn_list[[ite]], # truth
           range_int = popnestX95 - popnestX5,
           coverage = ifelse(prob_truth >= popnestX5 & prob_truth <= popnestX95, 1, 0))
  
  popnest_tab_13 = popnest_tab %>% 
    filter(model == 'model13') %>% 
    mutate(prob_truth = pt_popn_list[[ite]], # truth
           range_int = popnestX95 - popnestX5,
           coverage = ifelse(prob_truth >= popnestX5 & prob_truth <= popnestX95, 1, 0))
  
  popnest_tab_13a = popnest_tab %>% 
    filter(model == 'model13a') %>% 
    mutate(prob_truth = pt_popn_list[[ite]], # truth
           range_int = popnestX95 - popnestX5,
           coverage = ifelse(prob_truth >= popnestX5 & prob_truth <= popnestX95, 1, 0))
 
   popnest_tab_15 = popnest_tab %>% 
    filter(model == 'model15') %>% 
    mutate(prob_truth = pt_popn_list[[ite]], # truth
           range_int = popnestX95 - popnestX5,
           coverage = ifelse(prob_truth >= popnestX5 & prob_truth <= popnestX95, 1, 0))
 
  popnest_tab_15a = popnest_tab %>% 
    filter(model == 'model15a') %>% 
    mutate(prob_truth = pt_popn_list[[ite]], # truth
           range_int = popnestX95 - popnestX5,
           coverage = ifelse(prob_truth >= popnestX5 & prob_truth <= popnestX95, 1, 0))
  
  popnest_list_all = list(popnest_tab_06,
                          popnest_tab_11,
                          popnest_tab_13,
                          popnest_tab_13a,
                          popnest_tab_15,
                          popnest_tab_15a)
  
  popnest_tab_all = do.call(rbind,popnest_list_all)
  
  cov_prop_popn_list[[ite]] = popnest_tab_all %>% 
    group_by(model) %>% 
    summarise(mean_coverage_ite = mean(coverage))
}

# combine all the coverage proportion for all iterations
cov_prop_popn_tab = do.call(rbind,cov_prop_popn_list) %>% 
  mutate(ite = rep(iter,each = 6))

## calculating mean of coverage for indv. prediction across all interations
(cov_prop_popn_tab = cov_prop_popn_tab %>% 
    mutate(model = plyr::revalue(model, c('model06' = 'X1 + X3',
                                          'model11' = 'X1 + X2 + X3',  
                                          'model13' = 'X1 + X3 + X4', 
                                          'model13a' = '*X1 + X3 + X4', 
                                          'model15' = 'X1 + X2 + X3 + X4',
                                          'model15a' = '*X1 + X2 + X3 + X4'))))

# renaming and relevelling models
pc2 = cov_prop_popn_tab %>% 
  mutate(model = factor(model)) 
pc2$model = forcats::fct_relevel(pc2$model, c('X1 + X2 + X3 + X4', '*X1 + X2 + X3 + X4', 
                                              'X1 + X3 + X4', '*X1 + X3 + X4',
                                              'X1 + X2 + X3',
                                              'X1 + X3'))

## plot diff in mean
xloc4 = 0.97
(p1 = ggplot(pc2, aes(x = mean_coverage_ite, y = model, group = ite, colour = model))+
    geom_point(position = position_dodge(width = .5), alpha=0.7) +
    theme(legend.position = "none",
          axis.title = element_blank()) +
    scale_y_discrete(limits = rev) +
    scale_colour_manual(values = c("#1C73B1FF", "#26456EFF",
                                   "#FB964EFF", "#DF5948FF",
                                   "#09622AFF",
                                   "#879195FF")) + 
    labs(title="Mean proportion of coverage in population-level prediction") +
    xlim(c(0.65, 1)) +
    annotate("label", x = xloc4, y = 5.5, label = "X2 and X4") +
    annotate("label", x = xloc4, y = 3.5, label = "X4 only") +
    annotate("label", x = xloc4, y = 2,  label = "X2 only") +
    annotate("label", x = xloc4, y = 1, label = "None") )

ggsave(here::here("02-super popn approach/experiment4_SAE/02-results/plot_popn_est_fx2.png"), p1, width=6, height=7.5, units="in", device="png")
