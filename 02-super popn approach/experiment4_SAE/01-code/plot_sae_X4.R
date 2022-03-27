## plotting small area estimations 


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
                                        'model15a' = '*X1 + X2 + X3 + X4')),
         X4_group = as.factor(X4_group))
model_all_pn_tab$model = forcats::fct_relevel(model_all_pn_tab$model, c('X1 + X2 + X3 + X4', '*X1 + X2 + X3 + X4',
                                                                        'X1 + X3 + X4', '*X1 + X3 + X4',
                                                                        'X1 + X2 + X3',
                                                                        'X1 + X3'))

## to be edited
## comparing to 'truth'
model_all_pn_tab$mean_pe = as.numeric(model_all_pn_tab$`50%`) - as.numeric(model_all_pn_tab$mean_prob) # recursive diff for pt_list
model_all_pn_tab$low_pe = as.numeric(model_all_pn_tab$`5%`) - as.numeric(model_all_pn_tab$mean_prob)
model_all_pn_tab$upp_pe = as.numeric(model_all_pn_tab$`95%`) - as.numeric(model_all_pn_tab$mean_prob)

## looking at two models only
model_15_pn_tab = model_all_pn_tab %>% 
  filter(model == 'X1 + X2 + X3 + X4' | model == '*X1 + X2 + X3 + X4')

model_13_pn_tab = model_all_pn_tab %>% 
  filter(model == 'X1 + X3 + X4' | model == '*X1 + X3 + X4')

## plot diff in mean
xloc4 = 0.45
(p3 = ggplot(model_15_pn_tab, aes(x = mean_pe, y = X4_group, group = model, colour = model))+
    geom_vline(aes(xintercept = 0)) +
    geom_point(position = position_dodge(width = 1), alpha=0.7) +
    theme(legend.position = c(0.8,0.8)) +
    scale_colour_manual(values = c("#1C73B1FF", "#FB964EFF")) + 
    stat_summary(aes(group=model), width=0.5, size=0.7, fun=mean, geom="crossbar", colour=rep(c("#1C73B1FF", "#FB964EFF"), each = 12)) + #drawing the mean line 
    labs(title="Difference in X4-levels estimate and truth",
         y = 'Levels of X4', x = 'Bias' ))

ggsave(here::here("02-super popn approach/experiment4_SAE/02-results/plot_sae_X4.png"), p3, width=6, height=7.5, units="in", device="png")

## y_prob for each level of X4
popn_data_list[[10]] %>% 
  group_by(X4) %>% 
  summarise(X4_prob = mean(y_prob))

