source('LOO_wtd_sim_popn_2.R')

ITE = 100
sim_list = lapply(1:ITE, function(x)matrix(NA, nrow=4, ncol=6))
samp_data_list = lapply(1:ITE, function(x)matrix(NA))

slurm_arrayid <- Sys.getenv('SLURM_ARRAY_TASK_ID')
i = as.numeric(slurm_arrayid)


  samp_size = 500
  samp_loc = sample(1:nrow(popn_data), size = samp_size, replace=F, prob = popn_data$inclusion)
  samp_data = popn_data[samp_loc,]
  
  ## creating survey design
  svy1 = svydesign(ids=~1, # cluster id, ~1 for no clusters
                   weights=~rep(1,nrow(samp_data)), # equal weights for each unit
                   data=samp_data)
  
  ## calculating population totals for each level
  X1_margin = xtabs(~X1, data=popn_data)
  X2_margin = xtabs(~X2, data=popn_data)
  X3_margin = xtabs(~X3, data=popn_data)
  X4_margin = xtabs(~X4, data=popn_data)
  
  ## raked to the population
  rake1 = rake(design = svy1, sample.margins = list(~X1,~X2,~X3,~X4), 
               population.margins = list(X1_margin, X2_margin, X3_margin, X4_margin))
  
  ## raked weights ####
  samp_data$wts = weights(rake1)
  
  ## factorise the relevant variables
  str(samp_data)
  ind = c(1:4,6)
  samp_data[,ind] = apply(samp_data[,ind], 2, function(x)as.factor(x))
  
  ## the rest of the models ####
  model1 = brm(bin_value ~ (1|X1), data = samp_data,
               backend = "cmdstanr",
               family = bernoulli(link = "logit"), 
               control = list(adapt_delta = 0.99))
  
  model2 = brm(bin_value ~ (1|X2), data = samp_data,
               backend = "cmdstanr",
               family = bernoulli(link = "logit"), 
               control = list(adapt_delta = 0.99)) 
  
  model3 = brm(bin_value ~ (1|X3), data = samp_data,
               backend = "cmdstanr",
               family = bernoulli(link = "logit"), 
               control = list(adapt_delta = 0.99)) 
  
  model4 = brm(bin_value ~ (1|X4), data = samp_data,
               backend = "cmdstanr",
               family = bernoulli(link = "logit"), 
               control = list(adapt_delta = 0.99)) 
  
  model5 = brm(bin_value ~ (1|X1) + (1|X2), data = samp_data,
               backend = "cmdstanr",
               family = bernoulli(link = "logit"), 
               control = list(adapt_delta = 0.99))
  
  model6 = brm(bin_value ~ (1|X1) + (1|X3), data = samp_data,
               backend = "cmdstanr",
               family = bernoulli(link = "logit"), 
               control = list(adapt_delta = 0.99))
  
  model7 = brm(bin_value ~ (1|X1) + (1|X4), data = samp_data,
               backend = "cmdstanr",
               family = bernoulli(link = "logit"), 
               control = list(adapt_delta = 0.99))
  
  model8 = brm(bin_value ~ (1|X2) + (1|X3), data = samp_data,
               backend = "cmdstanr",
               family = bernoulli(link = "logit"), 
               control = list(adapt_delta = 0.99)) 
  
  model9 = brm(bin_value ~ (1|X2) + (1|X4), data = samp_data,
               backend = "cmdstanr",
               family = bernoulli(link = "logit"), 
               control = list(adapt_delta = 0.99)) 
  
  model10 = brm(bin_value ~ (1|X3) + (1|X4), data = samp_data,
                backend = "cmdstanr",
                family = bernoulli(link = "logit"), 
                control = list(adapt_delta = 0.99)) 
  
  model11 = brm(bin_value ~ (1|X1) + (1|X2) + (1|X3), data = samp_data,
                backend = "cmdstanr",
                family = bernoulli(link = "logit"), 
                control = list(adapt_delta = 0.99)) 
  
  model12 = brm(bin_value ~ (1|X1) + (1|X2) + (1|X4), data = samp_data,
                backend = "cmdstanr",
                family = bernoulli(link = "logit"), 
                control = list(adapt_delta = 0.99)) 
  
  model13 = brm(bin_value ~ (1|X1) + (1|X3) + (1|X4), data = samp_data,
                backend = "cmdstanr",
                family = bernoulli(link = "logit"), 
                control = list(adapt_delta = 0.99))
  
  model14 = brm(bin_value ~ (1|X2) + (1|X3) + (1|X4), data = samp_data,
                backend = "cmdstanr",
                family = bernoulli(link = "logit"), 
                control = list(adapt_delta = 0.99)) 
  
  model15 = brm(bin_value ~ (1|X1) + (1|X2) + (1|X3) + (1|X4), data = samp_data,
                backend = "cmdstanr",
                family = bernoulli(link = "logit"), 
                control = list(adapt_delta = 0.99)) 
  
  ## comparing loo for the models, with weights and without weights
  # calculating loo
  loo1 <- loo(model1)
  loo2 <- loo(model2)
  loo3 <- loo(model3)
  loo4 <- loo(model4)
  loo5 <- loo(model5)
  loo6 <- loo(model6)
  loo7 <- loo(model7)
  loo8 <- loo(model8)
  loo9 <- loo(model9)
  loo10 <- loo(model10)
  loo11 <- loo(model11)
  loo12 <- loo(model12)
  loo13 <- loo(model13)
  loo14 <- loo(model14)
  loo15 <- loo(model15)
  
  loo_all = list(loo1, loo2, loo3, loo4,loo5,
                 loo6, loo7, loo8, loo9, loo10,
                 loo11, loo12, loo13, loo14, loo15)
  
  ## extracting loo estimates to rank them
  loo_tab = lapply(loo_all,function(x)x$estimates[1,]) %>% 
    do.call(rbind,.) %>% 
    data.frame(.) %>% 
    rename(elpd_loo = Estimate)
  rownames(loo_tab) = c(paste0('model0', c(1:9)), paste0('model', c(10:15)))
  colnames(loo_tab) = c('elpd_loo', 'SE')
  
  
  # creating survey raked weights
  svy_rake = svydesign(ids=~1, # cluster id, ~1 for no clusters
                       weights=~wts, # including raked weights in the survey design
                       data=samp_data)
  
  loo_wtd_tab = lapply(loo_all, function(x)loo_wtd(x,svy_rake)) %>% do.call(rbind,.) %>% data.frame(.)
  rownames(loo_wtd_tab) = c(paste0('model0', c(1:9)), paste0('model', c(10:15)))
  
  loo_rank = rank(-loo_tab[,1])
  loo_wtd_rank = rank(-loo_wtd_tab[,1])
  
  sim_list[[i]] = cbind(loo_tab, loo_wtd_tab, loo_rank, loo_wtd_rank)
  
  samp_data <- samp_data %>% 
    mutate(elpd_1 = loo1$pointwise[,1],
           elpd_2 = loo2$pointwise[,1],
           elpd_3 = loo3$pointwise[,1],
           elpd_4 = loo4$pointwise[,1],
           elpd_5 = loo5$pointwise[,1],
           elpd_6 = loo6$pointwise[,1],
           elpd_7 = loo7$pointwise[,1],
           elpd_8 = loo8$pointwise[,1],
           elpd_9 = loo9$pointwise[,1],
           elpd_10 = loo10$pointwise[,1],
           elpd_11 = loo11$pointwise[,1],
           elpd_12 = loo12$pointwise[,1],
           elpd_13 = loo13$pointwise[,1],
           elpd_14 = loo14$pointwise[,1],
           elpd_15 = loo15$pointwise[,1]) 
  
  samp_data_list[[i]] = samp_data
  
save(samp_data_list, sim_list, file=paste0("simulated100_", i, ".RData"))

