## models 11-15
source('LOO_wtd_sim_popn.R')

ITE = 100
sim_trip_list = lapply(1:ITE, function(x)matrix(NA, nrow=4, ncol=6))
samp_data_list = lapply(1:ITE, function(x)matrix(NA))
prob_truth = matrix(NA, nrow=ITE)
coef_list = lapply(1:ITE, function(x)matrix(NA))

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
  
  ## the rest of the models ###
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
  
  ## make MRP estimates
  popn_ps = popn_data %>% 
    group_by(X1, X2, X3, X4) %>% 
    summarise(Nj = n()) %>% 
    ungroup()
  
  model11_predict = posterior_linpred(model11, newdata = popn_ps, transform = T) # getting model estimate for each cell
  model11_popnest = apply(model11_predict, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.
  
  model12_predict = posterior_linpred(model12, newdata = popn_ps, transform = T) # getting model estimate for each cell
  model12_popnest = apply(model12_predict, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.
  
  model13_predict = posterior_linpred(model13, newdata = popn_ps, transform = T) # getting model estimate for each cell
  model13_popnest = apply(model13_predict, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.
  
  model14_predict = posterior_linpred(model14, newdata = popn_ps, transform = T) # getting model estimate for each cell
  model14_popnest = apply(model14_predict, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.
  
  model15_predict = posterior_linpred(model15, newdata = popn_ps, transform = T) # getting estimate for each cell
  model15_popnest = apply(model15_predict, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.
  

  ## comparing loo for the models, with weights and without weights
  # calculating loo
  loo11 <- loo(model11)
  loo12 <- loo(model12)
  loo13 <- loo(model13)
  loo14 <- loo(model14)
  loo15 <- loo(model15)
  
  loo_trip = list(loo11, loo12, loo13, loo14, loo15)
  
  ## extracting loo estimates to rank them
  loo_trip_tab = lapply(loo_trip,function(x)x$estimates[1,]) %>% 
    do.call(rbind,.) %>% 
    data.frame(.) %>% 
    mutate(loo_trip_rank = rank(-.[,1])) %>% 
    rename(elpd_loo = Estimate)
  rownames(loo_trip_tab) = c(paste0('model', 11:15))
  
  # creating survey raked weights
  svy_rake = svydesign(ids=~1, # cluster id, ~1 for no clusters
                       weights=~wts, # including raked weights in the survey design
                       data=samp_data)
  
  loo_wtd_trip_tab = lapply(loo_trip, function(x)loo_wtd(x,svy_rake)) %>% 
    do.call(rbind,.) %>% 
    data.frame(.) %>% 
    mutate(loo_wtd_trip_rank =  rank(-.[,1]))
  rownames(loo_wtd_trip_tab) =  c(paste0('model', c(11:15)))
  
  sim_trip_list[[i]] = cbind(loo_trip_tab, loo_wtd_trip_tab) %>% 
    arrange(., loo_wtd_trip_rank)
  
  samp_data <- samp_data %>% 
    mutate(elpd_11 = loo11$pointwise[,1],
           elpd_12 = loo12$pointwise[,1],
           elpd_13 = loo13$pointwise[,1],
           elpd_14 = loo14$pointwise[,1],
           elpd_15 = loo15$pointwise[,1]) 
  
  ## MRP estimate of loo
  elpd_model11 = brm(elpd_11 ~ (1|X1) + (1|X2) + (1|X3) + (1|X4), data = samp_data,
                     backend = "cmdstanr", 
                     control = list(adapt_delta = 0.99))
  
  elpd_model11_predict = posterior_predict(elpd_model11, newdata = popn_ps) 
  elpd_model11_popnest = apply(elpd_model11_predict, 1, function(x)sum(x*popn_ps$Nj)) ## ~~equivalent to weighted elpd
  
  elpd_model12 = brm(elpd_12 ~ (1|X1) + (1|X2) + (1|X3) + (1|X4), data = samp_data,
                     backend = "cmdstanr", 
                     control = list(adapt_delta = 0.99))
  
  elpd_model12_predict = posterior_predict(elpd_model12, newdata = popn_ps) 
  elpd_model12_popnest = apply(elpd_model12_predict, 1, function(x)sum(x*popn_ps$Nj)) ## ~~equivalent to weighted elpd
  
  elpd_model13 = brm(elpd_13 ~ (1|X1) + (1|X2) + (1|X3) + (1|X4), data = samp_data,
                     backend = "cmdstanr", 
                     control = list(adapt_delta = 0.99))
  
  elpd_model13_predict = posterior_predict(elpd_model13, newdata = popn_ps) 
  elpd_model13_popnest = apply(elpd_model13_predict, 1, function(x)sum(x*popn_ps$Nj)) ## ~~equivalent to weighted elpd
  
  elpd_model14 = brm(elpd_14 ~ (1|X1) + (1|X2) + (1|X3) + (1|X4), data = samp_data,
                     backend = "cmdstanr", 
                     control = list(adapt_delta = 0.99))
  
  elpd_model14_predict = posterior_predict(elpd_model14, newdata = popn_ps) 
  elpd_model14_popnest = apply(elpd_model14_predict, 1, function(x)sum(x*popn_ps$Nj)) ## ~~equivalent to weighted elpd
  
  elpd_model15 = brm(elpd_15 ~ (1|X1) + (1|X2) + (1|X3) + (1|X4), data = samp_data,
                     backend = "cmdstanr", 
                     control = list(adapt_delta = 0.99))
  
  elpd_model15_predict = posterior_predict(elpd_model15, newdata = popn_ps) 
  elpd_model15_popnest = apply(elpd_model15_predict, 1, function(x)sum(x*popn_ps$Nj)) ## ~~equivalent to weighted elpd
  
  popnest_all = list(model11_popnest, 
                     model12_popnest, 
                     model13_popnest,
                     model14_popnest,
                     model15_popnest)
  
  elpd_popnest_all = list(elpd_model11_popnest, 
                          elpd_model12_popnest, 
                          elpd_model13_popnest,
                          elpd_model14_popnest,
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
  
  sim_trip_list[[i]] = cbind(loo_trip_tab, loo_wtd_trip_tab,
                             elpd_popnest_rank,
                             popnest_tab, elpd_popnest_tab)
  
  prob_truth[i,] = mean(popn_data$bin_value)
  
  samp_data_list[[i]] = samp_data
  
  coef_list[[i]] = c(coef(model11), coef(model12), coef(model13), coef(model14),
                     coef(model15))
  names(coef_list[[i]]) = c(paste0("11.",names(coef(model11))[grep("*", names(coef(model11)))]),
                            paste0("12.",names(coef(model12))[grep("*", names(coef(model12)))]),
                            paste0("13.",names(coef(model13))[grep("*", names(coef(model13)))]),
                            paste0("14.",names(coef(model14))[grep("*", names(coef(model14)))]),
                            paste0("15.",names(coef(model15))[grep("*", names(coef(model15)))]))
  
  save(samp_data_list, sim_trip_list, prob_truth, coef_list, file="simulated100temp_3.RData")
  
}

save(samp_data_list, sim_trip_list, prob_truth, coef_list, file="simulated100_3.RData")



