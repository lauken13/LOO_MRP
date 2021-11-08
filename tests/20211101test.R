source('LOO_wtd_sim_popn_2.R')

ITE = 100
sim_list = lapply(1:ITE, function(x)matrix(NA, nrow=4, ncol=6))
samp_data_list = lapply(1:ITE, function(x)matrix(NA))

i = 1
for (i in 1:ITE){
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
  
  ## four models ####
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
  
  ## make MRP estimates
  popn_ps = popn_data %>% 
    group_by(X1, X2, X3, X4) %>% 
    summarise(Nj = n()) %>% 
    ungroup()
    
  model1_predict = posterior_linpred(model1, newdata = popn_ps, transform = T) # getting estimate for each cell
  model1_popnest = apply(model1_predict, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.
  !!!quantile(model1_popnest, c(0.05, 0.5, 0.95))
  
  prob_truth = mean(popn_data$bin_value)
  
  model2_predict = posterior_linpred(model2, newdata = popn_ps, transform = T) # getting estimate for each cell
  model2_popnest = apply(model2_predict, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.
  
  model3_predict = posterior_linpred(model3, newdata = popn_ps, transform = T) # getting estimate for each cell
  model3_popnest = apply(model3_predict, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.
  
  model4_predict = posterior_linpred(model4, newdata = popn_ps, transform = T) # getting estimate for each cell
  model4_popnest = apply(model4_predict, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.
  
  ## comparing loo for two models, with weights and without weights
  # calculating loo
  loo1 <- loo(model1)
  loo2 <- loo(model2)
  loo3 <- loo(model3)
  loo4 <- loo(model4)
  
  loo_all = list(loo1, loo2, loo3, loo4)
  
  ## extracting loo estimates to rank them
  loo_tab = lapply(loo_all,function(x)x$estimates[1,]) %>% do.call(rbind,.) %>% data.frame(.)
  rownames(loo_tab) = c(paste0('model',1:4))
  colnames(loo_tab) = c('elpd_loo', 'SE')
  
  # creating survey raked weights
  svy_rake = svydesign(ids=~1, # cluster id, ~1 for no clusters
                       weights=~wts, # including raked weights in the survey design
                       data=samp_data)
  
  loo_wtd_tab = lapply(loo_all, function(x)loo_wtd(x,svy_rake)) %>% do.call(rbind,.) %>% data.frame(.)
  rownames(loo_wtd_tab) = c(paste0('model',1:4))
  
  loo_rank = rank(-loo_tab[,1])
  loo_wtd_rank = rank(-loo_wtd_tab[,1])
  
  sim_list[[i]] = cbind(loo_tab, loo_wtd_tab, loo_rank, loo_wtd_rank)
  
  samp_data <- samp_data %>% 
    mutate(elpd_1 = loo1$pointwise[,1],
           elpd_2 = loo2$pointwise[,1],
           elpd_3 = loo3$pointwise[,1],
           elpd_4 = loo4$pointwise[,1]) 
  
  ## MRP estimate of loo
  elpd_model1 = brm(elpd_1 ~ (1|X1) + (1|X2) + (1|X3) + (1|X4), data = samp_data,
                    backend = "cmdstanr", 
                    control = list(adapt_delta = 0.99))
  
  elpd_model1_predict = posterior_predict(elpd_model1, newdata = popn_ps) 
  elpd_model1_popnest = apply(elpd_model1_predict, 1, function(x)sum(x*popn_ps$Nj)) ## ~~equivalent to weighted elpd
  summary(elpd_model1_popnest)
  
  samp_data_list[[i]] = samp_data

  save(samp_data_list, sim_list, file="simulated100temp_1.RData")
}

save(samp_data_list, sim_list, file="simulated100_1.RData")
