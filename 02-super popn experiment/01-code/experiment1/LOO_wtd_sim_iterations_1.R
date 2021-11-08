## models 1-4, 15

source('LOO_wtd_sim_popn.R')

ITE = 100
sim_list = lapply(1:ITE, function(x)matrix(NA, nrow=4, ncol=6))
samp_data_list = lapply(1:ITE, function(x)matrix(NA))
prob_truth = matrix(NA, nrow=ITE)
  
for (i in 1:ITE){
  ## generating diff. population in each iteration
  set.seed(seed[i])
  wkly1 = round(rnorm(J[1], sd=0.1),2)
  strg1 = round(rnorm(J[2], sd=1),2)
  popn_data$bin_outcome <- inv_logit_scaled(wkly1[popn_data$X1] + # apply inv-logit for 'simulated' coefficients
                                              strg1[popn_data$X2] +
                                              wkly1[popn_data$X3] +
                                              strg1[popn_data$X4])
  popn_data$bin_value <- rbinom(N,1,popn_data$bin_outcome)
  
  ## generate inclusion prob. for each individual
  # weakly predictive - 0.1 (sd), strongly predictive - 1 (sd)
  wkly2 = round(rnorm(J[1], sd=0.1),2)
  strg2 = round(rnorm(J[2], sd=1),2)
  popn_data$inclusion <- inv_logit_scaled(wkly2[popn_data$X1] + # apply inv-logit for 'simulated' coefficients
                                            wkly2[popn_data$X2] +
                                            strg2[popn_data$X3] +
                                            strg2[popn_data$X4])
  
  ## generating samples
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
  
  model15 = brm(bin_value ~ (1|X1) + (1|X2) + (1|X3) + (1|X4), data = samp_data,
                backend = "cmdstanr",
                family = bernoulli(link = "logit"), 
                control = list(adapt_delta = 0.99)) 
  
  ## make MRP estimates
  popn_ps = popn_data %>% 
    group_by(X1, X2, X3, X4) %>% 
    summarise(Nj = n()) %>% 
    ungroup()
  
  model1_predict = posterior_linpred(model1, newdata = popn_ps, transform = T) # getting model estimate for each cell
  model1_popnest = apply(model1_predict, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.
  
  model2_predict = posterior_linpred(model2, newdata = popn_ps, transform = T) # getting estimate for each cell
  model2_popnest = apply(model2_predict, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.
  
  model3_predict = posterior_linpred(model3, newdata = popn_ps, transform = T) # getting estimate for each cell
  model3_popnest = apply(model3_predict, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.
  
  model4_predict = posterior_linpred(model4, newdata = popn_ps, transform = T) # getting estimate for each cell
  model4_popnest = apply(model4_predict, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.
  
  model15_predict = posterior_linpred(model15, newdata = popn_ps, transform = T) # getting estimate for each cell
  model15_popnest = apply(model15_predict, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.
  
  ## comparing loo for two models, with weights and without weights
  # calculating loo
  loo1 <- loo(model1)
  loo2 <- loo(model2)
  loo3 <- loo(model3)
  loo4 <- loo(model4)
  loo15 <- loo(model15)
  
  loo_all = list(loo1, loo2, loo3, loo4, loo15)
  
  ## extracting loo estimates to rank them
  loo_tab = lapply(loo_all,function(x)x$estimates[1,]) %>% 
    do.call(rbind,.) %>% 
    data.frame(.)
  rownames(loo_tab) = c(paste0('model0', 1:4), 'model15')
  colnames(loo_tab) = c('elpd_loo', 'SE')
  
  # creating survey raked weights
  svy_rake = svydesign(ids=~1, # cluster id, ~1 for no clusters
                       weights=~wts, # including raked weights in the survey design
                       data=samp_data)
  
  loo_wtd_tab = lapply(loo_all, function(x)loo_wtd(x,svy_rake)) %>% 
    do.call(rbind,.) %>% 
    data.frame(.)
  rownames(loo_wtd_tab) = c(paste0('model0', 1:4), 'model15')
  
  loo_rank = rank(-loo_tab[,1])
  loo_wtd_rank = rank(-loo_wtd_tab[,1])
  
  samp_data <- samp_data %>% 
    mutate(elpd_1 = loo1$pointwise[,1],
           elpd_2 = loo2$pointwise[,1],
           elpd_3 = loo3$pointwise[,1],
           elpd_4 = loo4$pointwise[,1],
           elpd_15 = loo15$pointwise[,1]) 
  
  ## MRP estimate of loo
  elpd_model1 = brm(elpd_1 ~ (1|X1) + (1|X2) + (1|X3) + (1|X4), data = samp_data,
                    backend = "cmdstanr", 
                    control = list(adapt_delta = 0.99))
  
  elpd_model1_predict = posterior_predict(elpd_model1, newdata = popn_ps) 
  elpd_model1_popnest = apply(elpd_model1_predict, 1, function(x)sum(x*popn_ps$Nj)) ## ~~equivalent to weighted elpd
  
  elpd_model2 = brm(elpd_2 ~ (1|X1) + (1|X2) + (1|X3) + (1|X4), data = samp_data,
                    backend = "cmdstanr", 
                    control = list(adapt_delta = 0.99))
  
  elpd_model2_predict = posterior_predict(elpd_model2, newdata = popn_ps) 
  elpd_model2_popnest = apply(elpd_model2_predict, 1, function(x)sum(x*popn_ps$Nj)) ## ~~equivalent to weighted elpd
  
  elpd_model3 = brm(elpd_3 ~ (1|X1) + (1|X2) + (1|X3) + (1|X4), data = samp_data,
                    backend = "cmdstanr", 
                    control = list(adapt_delta = 0.99))
  
  elpd_model3_predict = posterior_predict(elpd_model3, newdata = popn_ps) 
  elpd_model3_popnest = apply(elpd_model3_predict, 1, function(x)sum(x*popn_ps$Nj)) ## ~~equivalent to weighted elpd
  
  elpd_model4 = brm(elpd_4 ~ (1|X1) + (1|X2) + (1|X3) + (1|X4), data = samp_data,
                    backend = "cmdstanr", 
                    control = list(adapt_delta = 0.99))
  
  elpd_model4_predict = posterior_predict(elpd_model4, newdata = popn_ps) 
  elpd_model4_popnest = apply(elpd_model4_predict, 1, function(x)sum(x*popn_ps$Nj)) ## ~~equivalent to weighted elpd
  
  elpd_model15 = brm(elpd_15 ~ (1|X1) + (1|X2) + (1|X3) + (1|X4), data = samp_data,
                    backend = "cmdstanr", 
                    control = list(adapt_delta = 0.99))
  
  elpd_model15_predict = posterior_predict(elpd_model15, newdata = popn_ps) 
  elpd_model15_popnest = apply(elpd_model15_predict, 1, function(x)sum(x*popn_ps$Nj)) ## ~~equivalent to weighted elpd
  
 popnest_all = list(model1_popnest, 
                    model2_popnest, 
                    model3_popnest,
                    model4_popnest,
                    model15_popnest)
 
 elpd_popnest_all = list(elpd_model1_popnest, 
                         elpd_model2_popnest, 
                         elpd_model3_popnest,
                         elpd_model4_popnest,
                         elpd_model15_popnest)
 
 popnest_tab = lapply(popnest_all, function(x)quantile(x,c(0.05, 0.5, 0.95))) %>% 
   do.call(rbind,.) %>% 
   data.frame(.) %>% 
   rename(popnestX5 = X5., popnestX50 = X50., popnestX95 = X95.)
 
 elpd_popnest_tab = lapply(elpd_popnest_all, function(x)quantile(x,c(0.05, 0.5, 0.95))) %>% 
   do.call(rbind,.) %>% 
   data.frame(.) %>% 
   rename(elpd.popnestX5 = X5., elpd.popnestX50 = X50., elpd.popnestX95 = X95.)
 
 elpd_popnest_rank = rank(-elpd_popnest_tab[,2])

 ## saving the results
 sim_list[[i]] = cbind(loo_tab, loo_wtd_tab, loo_rank, loo_wtd_rank,
                       elpd_popnest_rank, popnest_tab, elpd_popnest_tab)

  prob_truth[i,] = mean(popn_data$bin_value)
  
  samp_data_list[[i]] = samp_data
  
  save(samp_data_list, sim_list, prob_truth, file="simulated100temp_1.RData")
}

save(samp_data_list, sim_list, prob_truth, file="simulated100_1.RData")

