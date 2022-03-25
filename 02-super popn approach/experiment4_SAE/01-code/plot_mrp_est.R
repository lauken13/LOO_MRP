## plotting mrp_est
library(dplyr)
library(ggplot2)


# mrp estimate at popn level ----------------------------------------------
## poststratification step using theta_pop (predicted values)
# calculating popnest and prob. of outcome for each iteration
popnest_tab = do.call(rbind,popnest_list[iter])

pt_tab = do.call(rbind,pt_list) %>% 
  as_tibble() %>% 
  mutate(ite = iter) %>% 
  rename(prob_truth = V1)


## comparing to 'truth'
popnest_tab$mean_pe = as.numeric(popnest_tab[,2] - as.numeric(rep(pt_tab$prob_truth, each=6))) # recursive diff for pt_list
popnest_tab$low_pe = as.numeric(popnest_tab[,1]  - as.numeric(rep(pt_tab$prob_truth, each=6)))
popnest_tab$upp_pe = as.numeric(popnest_tab[,3]  - as.numeric(rep(pt_tab$prob_truth, each=6)))

pc = popnest_tab %>% 
  mutate(range_pe = popnestX95 - popnestX5) %>% 
  mutate(model = factor(model)) %>% 
  mutate(model = plyr::revalue(model, c('model06' = 'X1 + X3',
                                        'model11' = 'X1 + X2 + X3',  
                                        'model13' = 'X1 + X3 + X4', 
                                        'model13a' = '*X1 + X3 + X4', 
                                        'model15' = 'X1 + X2 + X3 + X4',
                                        'model15a' = '*X1 + X2 + X3 + X4')))
pc$model = forcats::fct_relevel(pc$model, c('X1 + X2 + X3 + X4', '*X1 + X2 + X3 + X4', 
                                   'X1 + X3 + X4', '*X1 + X3 + X4',
                                   'X1 + X2 + X3',
                                   'X1 + X3'))

## plot diff in mean
xloc4 = 0.16
(p3 = ggplot(pc, aes(x = mean_pe, y = model, group = iter, colour = model))+
    geom_vline(aes(xintercept = 0)) +
    geom_point(position = position_dodge(width = .5), alpha=0.7) +
    theme(legend.position = "none",
          axis.title = element_blank()) +
    scale_y_discrete(limits = rev) +
    stat_summary(aes(group=model), width=0.5, size=.2, fun=mean, geom="crossbar", colour="black") + #drawing the mean line 
    scale_colour_manual(values = c("#1C73B1FF", "#26456EFF",
                                   "#FB964EFF", "#DF5948FF",
                                   "#09622AFF",
                                   "#879195FF")) + 
    labs(title="Difference in MRP estimate and truth") +
    xlim(c(-0.04, 0.18)) +
    annotate("label", x = xloc4, y = 5.5, label = "X2 and X4") +
    annotate("label", x = xloc4, y = 3.5, label = "X4 only") +
    annotate("label", x = xloc4, y = 2,  label = "X2 only") +
    annotate("label", x = xloc4, y = 1, label = "None") )

ggsave(here::here("02-super popn approach/experiment4_SAE/02-results/plot_mrp_truth_fx3.png"), p3, width=6, height=7.5, units="in", device="png")

## plotting variance range
xloc5 = 0.095
range(pc$range_pe)
(p4 = ggplot(pc, aes(x = range_pe, y = model, group = iter, colour = model)) +
    geom_point(position = position_dodge(width = .5), alpha=0.7) +
    scale_y_discrete(limits = rev) +
    stat_summary(aes(group=model), width=0.5, size=.2, fun=mean, geom="crossbar", colour="black") + #drawing the mean line 
    scale_colour_manual(values = c("#1C73B1FF", "#26456EFF",
                                 "#FB964EFF", "#DF5948FF",
                                 "#09622AFF",
                                 "#879195FF")) + 
    theme(legend.position = "none",
          axis.title = element_blank()) +
    labs(title="Quantile range for MRP estimates") +
    xlim(c(range(pc$range_pe)[1]*0.9, range(pc$range_pe)[2]*1.1)) +
    annotate("label", x = xloc5, y = 5.5, label = "X2 and X4") +
    annotate("label", x = xloc5, y = 3.5, label = "X4 only") +
    annotate("label", x = xloc5, y = 2,  label = "X2 only") +
    annotate("label", x = xloc5, y = 1, label = "None"))

ggsave(here::here("02-super popn approach/experiment4_SAE/02-results/plot_mrp_qt_range_fx3.png"), p4, width=6, height=7.5, units="in", device="png")


# group/small area prediction --------------------------------------------------------
# checking the coverage for small area/group

X4_group_mean_list = list()

## getting the group prob for each iteration/population
for (ite in iter){
  X4_group_mean_list[[ite]] =  popn_data_list[[ite]] %>% 
    group_by(X4) %>% 
    summarise(mean_prob = mean(y_prob)) %>% 
    mutate(iteration = ite)
}

# true group mean
X4_group_mean_tab = do.call(rbind, X4_group_mean_list)

# calculating quantile of the group estimate for each model
model06_pn_tab = model06_popnest_sae %>% 
  lapply(., function(x)apply(x,2,quantile,c(0.05,0.5,0.95))) %>% 
  lapply(., function(x)t(x)) %>% 
  do.call(rbind, .) %>% 
  as_tibble() %>% 
  mutate(model = "model06",
         X4_group =rep(1:12,100),
         iter = rep(1:100, each=12))

model11_pn_tab = model11_popnest_sae %>% 
  lapply(., function(x)apply(x,2,quantile,c(0.05,0.5,0.95))) %>% 
  lapply(., function(x)t(x)) %>% 
  do.call(rbind, .) %>% 
  as_tibble() %>% 
  mutate(model = "model11",
         X4_group =rep(1:12,100),
         iter = rep(1:100, each=12))

model13_pn_tab = model13_popnest_sae %>% 
  lapply(., function(x)apply(x,2,quantile,c(0.05,0.5,0.95))) %>% 
  lapply(., function(x)t(x)) %>% 
  do.call(rbind, .) %>% 
  as_tibble() %>% 
  mutate(model = "model13",
         X4_group =rep(1:12,100),
         iter = rep(1:100, each=12))

model13a_pn_tab = model13a_popnest_sae %>% 
  lapply(., function(x)apply(x,2,quantile,c(0.05,0.5,0.95))) %>% 
  lapply(., function(x)t(x)) %>% 
  do.call(rbind, .) %>% 
  as_tibble() %>% 
  mutate(model = "model13a",
         X4_group =rep(1:12,100),
         iter = rep(1:100, each=12))

model15_pn_tab = model15_popnest_sae %>% 
  lapply(., function(x)apply(x,2,quantile,c(0.05,0.5,0.95))) %>% 
  lapply(., function(x)t(x)) %>% 
  do.call(rbind, .) %>% 
  as_tibble() %>% 
  mutate(model = "model15",
         X4_group =rep(1:12,100),
         iter = rep(1:100, each=12))

model15a_pn_tab = model15a_popnest_sae %>% 
  lapply(., function(x)apply(x,2,quantile,c(0.05,0.5,0.95))) %>% 
  lapply(., function(x)t(x)) %>% 
  do.call(rbind, .) %>% 
  as_tibble() %>% 
  mutate(model = "model15a",
         X4_group =rep(1:12,100),
         iter = rep(1:100, each=12))

model_all_pn_tab = rbind(model06_pn_tab, model11_pn_tab,
                         model13_pn_tab, model13a_pn_tab,
                         model15_pn_tab, model15a_pn_tab) %>% 
  mutate(mean_prob = rep(X4_group_mean_tab$mean_prob, 6)) %>% 
  mutate(model = plyr::revalue(model, c('model06' = 'X1 + X3',
                                        'model11' = 'X1 + X2 + X3',  
                                        'model13' = 'X1 + X3 + X4', 
                                        'model13a' = '*X1 + X3 + X4', 
                                        'model15' = 'X1 + X2 + X3 + X4',
                                        'model15a' = '*X1 + X2 + X3 + X4')))
model_all_pn_tab$model = forcats::fct_relevel(model_all_pn_tab$model, c('X1 + X2 + X3 + X4', '*X1 + X2 + X3 + X4',
                                                                        'X1 + X3 + X4', '*X1 + X3 + X4',
                                                                        'X1 + X2 + X3',
                                                                        'X1 + X3'))

## to be edited
## comparing to 'truth'
model_all_pn_tab$mean_pe = as.numeric(model_all_pn_tab$`50%`) - as.numeric(model_all_pn_tab$mean_prob) # recursive diff for pt_list
model_all_pn_tab$low_pe = as.numeric(model_all_pn_tab$`5%`) - as.numeric(model_all_pn_tab$mean_prob)
model_all_pn_tab$upp_pe = as.numeric(model_all_pn_tab$`95%`) - as.numeric(model_all_pn_tab$mean_prob)

## plot diff in mean
xloc4 = 0.45
(p3 = ggplot(model_all_pn_tab, aes(x = mean_pe, y = model, group = iter, colour = model))+
    geom_vline(aes(xintercept = 0)) +
    geom_point(position = position_dodge(width = .5), alpha=0.7) +
    theme(legend.position = "none",
          axis.title = element_blank()) +
    scale_y_discrete(limits = rev) +
    stat_summary(aes(group=model), width=0.5, size=.2, fun=mean, geom="crossbar", colour="black") + #drawing the mean line 
    scale_colour_manual(values = c("#1C73B1FF", "#26456EFF",
                                   "#FB964EFF", "#DF5948FF",
                                   "#09622AFF",
                                   "#879195FF")) + 
    labs(title="Difference in MRP estimate and truth") +
    # xlim(c(-0.04, 0.18)) +
    annotate("label", x = xloc4, y = 5.5, label = "X2 and X4") +
    annotate("label", x = xloc4, y = 3.5, label = "X4 only") +
    annotate("label", x = xloc4, y = 2,  label = "X2 only") +
    annotate("label", x = xloc4, y = 1, label = "None") )

# ggsave(here::here("02-super popn approach/experiment4_SAE/02-results/plot_mrp_truth_fx3.png"), p3, width=6, height=7.5, units="in", device="png")

