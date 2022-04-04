## postprocessing the saved objects from cluster
library(loo)
library(posterior) # to convert draws_array() objects 

# sourcing gen_dat()
source("00-gen_dat_func.R")

# sourcing loo_wtd()
source("../functions.R")

model06_popnest_sae_X2 = 
  model11_popnest_sae_X2 =
  model13_popnest_sae_X2 = 
  model13a_popnest_sae_X2 = 
  model15_popnest_sae_X2 =
  model15a_popnest_sae_X2 = lapply(1:100,matrix, data=NA,nrow=4000, ncol=5)

model06_popnest_sae = 
  model11_popnest_sae =
  model13_popnest_sae = 
  model13a_popnest_sae = 
  model15_popnest_sae =
  model15a_popnest_sae = lapply(1:100,matrix, data=NA,nrow=4000, ncol=12)

iter = c(1:20,22,23,25,27,28,30:41,43,45,46,48:65,67:74,76,77,79,81,83,89:92,94:100)
for(ite in iter){
  # set.seed(65438)
  # sim1 = gen_dat(N = 10000, fx = fx3, samp_size = 500, ITE=ite)
  
  popn_ps = popn_ps_list[[ite]]
  
  for (s in 1:5){
    
    lvl_loc = which(popn_ps$X2 == s)
    
    # calculating group popnest for X4-levels
    model06_popnest_sae_X2[[ite]][,s] = apply(as_draws_matrix(popnest_06[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc])) # applying to each iteration
    # model11_popnest_sae_X2[[ite]][,s] = apply(as_draws_matrix(popnest_11[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    # model13_popnest_sae_X2[[ite]][,s] = apply(as_draws_matrix(popnest_13[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    # model13a_popnest_sae_X2[[ite]][,s] = apply(as_draws_matrix(popnest_13a[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    # model15_popnest_sae_X2[[ite]][,s] = apply(as_draws_matrix(popnest_15[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    # model15a_popnest_sae_X2[[ite]][,s] = apply(as_draws_matrix(popnest_15a[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    
  }
  
  # for (s in 1:12){
  #   
  #   lvl_loc = which(popn_ps$X4 == s)
  #   
  #   # calculating group popnest for X4-levels
  #   model06_popnest_sae[[ite]][,s] = apply(as_draws_matrix(popnest_06[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc])) # applying to each iteration
  #   model11_popnest_sae[[ite]][,s] = apply(as_draws_matrix(popnest_11[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
  #   model13_popnest_sae[[ite]][,s] = apply(as_draws_matrix(popnest_13[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
  #   model13a_popnest_sae[[ite]][,s] = apply(as_draws_matrix(popnest_13a[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
  #   model15_popnest_sae[[ite]][,s] = apply(as_draws_matrix(popnest_15[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
  #   model15a_popnest_sae[[ite]][,s] = apply(as_draws_matrix(popnest_15a[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
  #   
  # }
}  


# group/small area prediction --------------------------------------------------------
X2_group_mean_list = list()

## getting the group prob for each iteration/population
for (ite in iter){
  X2_group_mean_list[[ite]] =  popn_data_list[[ite]] %>% 
    group_by(X2) %>% 
    summarise(mean_prob = mean(y_prob)) %>% 
    mutate(iteration = ite)
}

# true group mean
X2_group_mean_tab = do.call(rbind, X2_group_mean_list)

# calculating quantile of the group estimate for each model
model06_pn_tab = model06_popnest_sae_X2[iter] %>% 
  lapply(., function(x)apply(x,2,quantile,c(0.05,0.5,0.95))) %>% 
  lapply(., function(x)t(x)) %>% 
  do.call(rbind, .) %>% 
  as_tibble() %>% 
  mutate(model = "model06",
         X2_group =rep(1:5,length(iter)),
         iter = rep(1:length(iter), each=5))

model11_pn_tab = model11_popnest_sae_X2[iter] %>% 
  lapply(., function(x)apply(x,2,quantile,c(0.05,0.5,0.95))) %>% 
  lapply(., function(x)t(x)) %>% 
  do.call(rbind, .) %>% 
  as_tibble() %>% 
  mutate(model = "model11",
         X2_group =rep(1:5,length(iter)),
         iter = rep(1:length(iter), each=5))

model13_pn_tab = model13_popnest_sae_X2[iter] %>% 
  lapply(., function(x)apply(x,2,quantile,c(0.05,0.5,0.95))) %>% 
  lapply(., function(x)t(x)) %>% 
  do.call(rbind, .) %>% 
  as_tibble() %>% 
  mutate(model = "model13",
         X2_group =rep(1:5,length(iter)),
         iter = rep(1:length(iter), each=5))

model13a_pn_tab = model13a_popnest_sae_X2[iter] %>% 
  lapply(., function(x)apply(x,2,quantile,c(0.05,0.5,0.95))) %>% 
  lapply(., function(x)t(x)) %>% 
  do.call(rbind, .) %>% 
  as_tibble() %>% 
  mutate(model = "model13a",
         X2_group =rep(1:5,length(iter)),
         iter = rep(1:length(iter), each=5))

model15_pn_tab = model15_popnest_sae_X2[iter] %>% 
  lapply(., function(x)apply(x,2,quantile,c(0.05,0.5,0.95))) %>% 
  lapply(., function(x)t(x)) %>% 
  do.call(rbind, .) %>% 
  as_tibble() %>% 
  mutate(model = "model15",
         X2_group =rep(1:5,length(iter)),
         iter = rep(1:length(iter), each=5))

model15a_pn_tab = model15a_popnest_sae_X2[iter] %>% 
  lapply(., function(x)apply(x,2,quantile,c(0.05,0.5,0.95))) %>% 
  lapply(., function(x)t(x)) %>% 
  do.call(rbind, .) %>% 
  as_tibble() %>% 
  mutate(model = "model15a",
         X2_group =rep(1:5,length(iter)),
         iter = rep(1:length(iter), each=5))

model_all_pn_tab = rbind(model06_pn_tab, model11_pn_tab,
                         model13_pn_tab, model13a_pn_tab,
                         model15_pn_tab, model15a_pn_tab) %>% 
  mutate(mean_prob = rep(X2_group_mean_tab$mean_prob, 6)) %>% 
  mutate(model = plyr::revalue(model, c('model06' = 'X1 + X3',
                                        'model11' = 'X1 + X2 + X3',  
                                        'model13' = 'X1 + X3 + X4', 
                                        'model13a' = '*X1 + X3 + X4', 
                                        'model15' = 'X1 + X2 + X3 + X4',
                                        'model15a' = '*X1 + X2 + X3 + X4')),
         X2_group = as.factor(X2_group))
model_all_pn_tab$model = forcats::fct_relevel(model_all_pn_tab$model, c('X1 + X2 + X3 + X4', '*X1 + X2 + X3 + X4',
                                                                        'X1 + X3 + X4', '*X1 + X3 + X4',
                                                                        'X1 + X2 + X3',
                                                                        'X1 + X3'))

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
(p3 = ggplot(model_13_pn_tab, aes(x = mean_pe, y = X2_group, group = model, colour = model))+
    geom_vline(aes(xintercept = 0)) +
    geom_point(position = position_dodge(width = 1), alpha=0.7) +
    theme(legend.position = c(0.8,0.8)) +
    scale_colour_manual(values = c("#1C73B1FF", "#FB964EFF")) + 
    stat_summary(aes(group=model), width=0.5, size=0.3, fun=mean, geom="crossbar", colour=rep(c("#1C73B1FF", "#FB964EFF"), each = 12)) + #drawing the mean line 
    labs(title="Difference in X4-levels estimate and truth",
         y = 'Levels of X4', x = 'Bias' ))

ggsave(here::here("02-super popn approach/experiment4_SAE/02-results/plot_bias_sae_M13_fx3.png"), p3, width=6, height=7.5, units="in", device="png")

## y_prob for each level of X4
popn_data_list[[10]] %>% 
  group_by(X4) %>% 
  summarise(X4_prob = mean(y_prob))


