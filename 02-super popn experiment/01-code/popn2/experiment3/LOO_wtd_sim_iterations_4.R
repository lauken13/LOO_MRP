## models 1-4, 15
source('LOO_wtd_sim_popn.R')

ITE = 100
sim_list = lapply(1:ITE, function(x)matrix(NA, nrow=4, ncol=6))
samp_data_list = lapply(1:ITE, function(x)matrix(NA))
samp_data2_list = lapply(1:ITE, function(x)matrix(NA))
df_list = lapply(1:ITE, function(x)matrix(NA))

prob_truth = matrix(NA, nrow=ITE)
coef_list = lapply(1:ITE, function(x)matrix(NA))
elpd_popnest_list = list()

for (i in 76:100){
  set.seed(seed[i])
  popn_data <- data.frame(X1_cont = rnorm(N, 0, 2), 
                          X2_cont = rnorm(N, 0, 2),
                          X3_cont = rnorm(N, 0, 2), 
                          X4_cont = rnorm(N, 0, 2))
  
  
  wkly1 = 0.1
  strg1 = 1
  
  ## generating continuous and binary outcome
  popn_data$outcome <- inv_logit_scaled(wkly1*popn_data$X1_cont +
                                          strg1*popn_data$X2_cont +
                                          wkly1*popn_data$X3_cont +
                                          strg1*popn_data$X4_cont)
  popn_data$bin_value <- rbinom(N,1,popn_data$outcome)
  
  
  ## generate inclusion prob. for each individual
  # weakly predictive - 0.1 (sd), strongly predictive - 1 (sd)
  wkly2 = 0.1
  strg2 = 1
  popn_data$incl_prob <- inv_logit_scaled(wkly2*popn_data$X1_cont + 
                                            wkly2*popn_data$X2_cont + 
                                            strg2*popn_data$X3_cont +
                                            strg2*popn_data$X4_cont)
  
  
  ## categorising the continuous covariates 
  J = 5
  
  popn_data <- popn_data %>% 
    mutate(X1_fct = cut_interval(X1_cont,J),
           X2_fct = cut_interval(X2_cont,J),
           X3_fct = cut_interval(X3_cont,J),
           X4_fct = cut_interval(X4_cont,J)) %>% 
    mutate(across(X1_fct:X4_fct, ~ as.numeric(.x))) 
  
  df <- popn_data %>%
    pivot_longer(c(X1_fct:X4_fct, X1_cont:X4_cont),
                 names_to = c("variable","type"),
                 values_to = "value",
                 names_sep = "_") %>% 
    filter(type=="fct")
  
  popn_data <- popn_data %>%
    rename(X1 = X1_fct,
           X2 = X2_fct,
           X3 = X3_fct,
           X4 = X4_fct)
  
  ## generating samples
  samp_size = 500
  
  samp_loc = sample(1:nrow(popn_data), size = samp_size-(J*4), replace=F, prob = popn_data$incl_prob)
  
  ## making sure at least each level of the covariates are sampled
  for(j in 1:J){
    samp_loc[length(samp_loc)+1] = sample(which(popn_data$X1 == j), size=1)
    samp_loc[length(samp_loc)+1] = sample(which(popn_data$X2 == j), size=1)
    samp_loc[length(samp_loc)+1] = sample(which(popn_data$X3 == j), size=1)
    samp_loc[length(samp_loc)+1] = sample(which(popn_data$X4 == j), size=1)
  }
  
  samp_data = popn_data[samp_loc,]
  
  # random sample
  samp_loc2 = sample(1:nrow(popn_data), size = samp_size)
  samp_data2 = popn_data[samp_loc2,]
  
  
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
  
  ## all models ####
  model01 = brm(bin_value ~ (1|X1), data = samp_data,
                backend = "cmdstanr",
                family = bernoulli(link = "logit"), 
                control = list(adapt_delta = 0.99))
  
  model02 = brm(bin_value ~ (1|X2), data = samp_data,
                backend = "cmdstanr",
                family = bernoulli(link = "logit"), 
                control = list(adapt_delta = 0.99)) 
  
  model03 = brm(bin_value ~ (1|X3), data = samp_data,
                backend = "cmdstanr",
                family = bernoulli(link = "logit"), 
                control = list(adapt_delta = 0.99)) 
  
  model04 = brm(bin_value ~ (1|X4), data = samp_data,
                backend = "cmdstanr",
                family = bernoulli(link = "logit"), 
                control = list(adapt_delta = 0.99)) 
  
  model05 = brm(bin_value ~ (1|X1) + (1|X2), data = samp_data,
                backend = "cmdstanr",
                family = bernoulli(link = "logit"), 
                control = list(adapt_delta = 0.99))
  
  model06 = brm(bin_value ~ (1|X1) + (1|X3), data = samp_data,
                backend = "cmdstanr",
                family = bernoulli(link = "logit"), 
                control = list(adapt_delta = 0.99))
  
  model07 = brm(bin_value ~ (1|X1) + (1|X4), data = samp_data,
                backend = "cmdstanr",
                family = bernoulli(link = "logit"), 
                control = list(adapt_delta = 0.99))
  
  model08 = brm(bin_value ~ (1|X2) + (1|X3), data = samp_data,
                backend = "cmdstanr",
                family = bernoulli(link = "logit"), 
                control = list(adapt_delta = 0.99)) 
  
  model09 = brm(bin_value ~ (1|X2) + (1|X4), data = samp_data,
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
  
  ## make MRP estimates
  popn_ps = popn_data %>% 
    group_by(X1, X2, X3, X4) %>% 
    summarise(Nj = n()) %>% 
    ungroup()
  
  model1_predict = posterior_linpred(model01, newdata = popn_ps, transform = T) # getting model estimate for each cell
  model1_popnest = apply(model1_predict, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.
  
  model2_predict = posterior_linpred(model02, newdata = popn_ps, transform = T) # getting estimate for each cell
  model2_popnest = apply(model2_predict, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.
  
  model3_predict = posterior_linpred(model03, newdata = popn_ps, transform = T) # getting estimate for each cell
  model3_popnest = apply(model3_predict, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.
  
  model4_predict = posterior_linpred(model04, newdata = popn_ps, transform = T) # getting estimate for each cell
  model4_popnest = apply(model4_predict, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.
  
  model05_predict = posterior_linpred(model05, newdata = popn_ps, transform = T) # getting model estimate for each cell
  model05_popnest = apply(model05_predict, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.
  
  model06_predict = posterior_linpred(model06, newdata = popn_ps, transform = T) # getting model estimate for each cell
  model06_popnest = apply(model06_predict, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.
  
  model07_predict = posterior_linpred(model07, newdata = popn_ps, transform = T) # getting model estimate for each cell
  model07_popnest = apply(model07_predict, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.
  
  model08_predict = posterior_linpred(model08, newdata = popn_ps, transform = T) # getting model estimate for each cell
  model08_popnest = apply(model08_predict, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.
  
  model09_predict = posterior_linpred(model09, newdata = popn_ps, transform = T) # getting model estimate for each cell
  model09_popnest = apply(model09_predict, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.
  
  model10_predict = posterior_linpred(model10, newdata = popn_ps, transform = T) # getting model estimate for each cell
  model10_popnest = apply(model10_predict, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.
  
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
  
  ## comparing loo for two models, with weights and without weights
  # calculating loo
  loo1 <- loo(model01)
  loo2 <- loo(model02)
  loo3 <- loo(model03)
  loo4 <- loo(model04)
  loo5 <- loo(model05)
  loo6 <- loo(model06)
  loo7 <- loo(model07)
  loo8 <- loo(model08)
  loo9 <- loo(model09)
  loo10 <- loo(model10)
  loo11 <- loo(model11)
  loo12 <- loo(model12)
  loo13 <- loo(model13)
  loo14 <- loo(model14)
  loo15 <- loo(model15)
  
  loo_all = list(loo1, loo2, loo3, loo4, loo5, 
                 loo6, loo7, loo8, loo9, loo10,
                 loo11, loo12, loo13, loo14, loo15)
  
  ## extracting loo estimates to rank them
  loo_tab = lapply(loo_all,function(x)x$estimates[1,]) %>% 
    do.call(rbind,.) %>% 
    data.frame(.)
  rownames(loo_tab) = c(paste0('model0', 1:9), paste0('model', 10:15))
  colnames(loo_tab) = c('elpd_loo', 'SE')
  
  # creating survey raked weights
  svy_rake = svydesign(ids=~1, # cluster id, ~1 for no clusters
                       weights=~wts, # including raked weights in the survey design
                       data=samp_data)
  
  loo_wtd_tab = lapply(loo_all, function(x)loo_wtd(x,svy_rake)) %>% 
    do.call(rbind,.) %>% 
    data.frame(.)
  rownames(loo_wtd_tab) =  c(paste0('model0', 1:9), paste0('model', 10:15))
  
  loo_rank = rank(-loo_tab[,1])
  loo_wtd_rank = rank(-loo_wtd_tab[,1])
  
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
  
  elpd_model05 = brm(elpd_5 ~ (1|X1) + (1|X2) + (1|X3) + (1|X4), data = samp_data,
                     backend = "cmdstanr", 
                     control = list(adapt_delta = 0.99))
  elpd_model05_predict = posterior_predict(elpd_model05, newdata = popn_ps) 
  elpd_model05_popnest = apply(elpd_model05_predict, 1, function(x)sum(x*popn_ps$Nj)) ## ~~equivalent to weighted elpd
  
  elpd_model06 = brm(elpd_6 ~ (1|X1) + (1|X2) + (1|X3) + (1|X4), data = samp_data,
                     backend = "cmdstanr", 
                     control = list(adapt_delta = 0.99))
  elpd_model06_predict = posterior_predict(elpd_model06, newdata = popn_ps) 
  elpd_model06_popnest = apply(elpd_model06_predict, 1, function(x)sum(x*popn_ps$Nj)) ## ~~equivalent to weighted elpd
  
  elpd_model07 = brm(elpd_7 ~ (1|X1) + (1|X2) + (1|X3) + (1|X4), data = samp_data,
                     backend = "cmdstanr", 
                     control = list(adapt_delta = 0.99))
  elpd_model07_predict = posterior_predict(elpd_model07, newdata = popn_ps) 
  elpd_model07_popnest = apply(elpd_model07_predict, 1, function(x)sum(x*popn_ps$Nj)) ## ~~equivalent to weighted elpd
  
  elpd_model08 = brm(elpd_8 ~ (1|X1) + (1|X2) + (1|X3) + (1|X4), data = samp_data,
                     backend = "cmdstanr", 
                     control = list(adapt_delta = 0.99))
  elpd_model08_predict = posterior_predict(elpd_model08, newdata = popn_ps) 
  elpd_model08_popnest = apply(elpd_model08_predict, 1, function(x)sum(x*popn_ps$Nj)) ## ~~equivalent to weighted elpd
  
  elpd_model09 = brm(elpd_9 ~ (1|X1) + (1|X2) + (1|X3) + (1|X4), data = samp_data,
                     backend = "cmdstanr", 
                     control = list(adapt_delta = 0.99))
  elpd_model09_predict = posterior_predict(elpd_model09, newdata = popn_ps) 
  elpd_model09_popnest = apply(elpd_model09_predict, 1, function(x)sum(x*popn_ps$Nj)) ## ~~equivalent to weighted elpd
  
  elpd_model10 = brm(elpd_10 ~ (1|X1) + (1|X2) + (1|X3) + (1|X4), data = samp_data,
                     backend = "cmdstanr", 
                     control = list(adapt_delta = 0.99))
  elpd_model10_predict = posterior_predict(elpd_model10, newdata = popn_ps) 
  elpd_model10_popnest = apply(elpd_model10_predict, 1, function(x)sum(x*popn_ps$Nj)) ## ~~equivalent to weighted elpd
  
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
  
  popnest_all = list(model1_popnest, 
                     model2_popnest, 
                     model3_popnest,
                     model4_popnest,
                     model05_popnest, 
                     model06_popnest, 
                     model07_popnest,
                     model08_popnest,
                     model09_popnest,
                     model10_popnest,
                     model11_popnest, 
                     model12_popnest, 
                     model13_popnest,
                     model14_popnest,
                     model15_popnest)
  
  elpd_popnest_all = list(elpd_model1_popnest, 
                          elpd_model2_popnest, 
                          elpd_model3_popnest,
                          elpd_model4_popnest,
                          elpd_model05_popnest, 
                          elpd_model06_popnest, 
                          elpd_model07_popnest,
                          elpd_model08_popnest,
                          elpd_model09_popnest,
                          elpd_model10_popnest,
                          elpd_model11_popnest, 
                          elpd_model12_popnest, 
                          elpd_model13_popnest,
                          elpd_model14_popnest,
                          elpd_model15_popnest)
  
  elpd_popnest_list[[length(elpd_popnest_list)+1]] = elpd_popnest_all
  
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
  
  samp_data2_list[[i]] = samp_data2
  
  df_list[[i]] = df
  
  coef_list[[i]] = c(coef(model01), coef(model02), coef(model03), coef(model04), 
                     coef(model05), coef(model06), coef(model07), coef(model08),
                     coef(model09), coef(model10), coef(model11), coef(model12),
                     coef(model13), coef(model14), coef(model15))
  names(coef_list[[i]]) = c(paste0("01.",names(coef(model01))[grep("*", names(coef(model01)))]),
                            paste0("02.",names(coef(model02))[grep("*", names(coef(model02)))]),
                            paste0("03.",names(coef(model03))[grep("*", names(coef(model03)))]),
                            paste0("04.",names(coef(model04))[grep("*", names(coef(model04)))]),
                            paste0("05.",names(coef(model05))[grep("*", names(coef(model05)))]),
                            paste0("06.",names(coef(model06))[grep("*", names(coef(model06)))]),
                            paste0("07.",names(coef(model07))[grep("*", names(coef(model07)))]),
                            paste0("08.",names(coef(model08))[grep("*", names(coef(model08)))]),
                            paste0("09.",names(coef(model09))[grep("*", names(coef(model09)))]),
                            paste0("10.",names(coef(model10))[grep("*", names(coef(model10)))]),
                            paste0("11.",names(coef(model11))[grep("*", names(coef(model11)))]),
                            paste0("12.",names(coef(model12))[grep("*", names(coef(model12)))]),
                            paste0("13.",names(coef(model13))[grep("*", names(coef(model13)))]),
                            paste0("14.",names(coef(model14))[grep("*", names(coef(model14)))]),
                            paste0("15.",names(coef(model15))[grep("*", names(coef(model15)))]))
  
  
  save(samp_data_list, samp_data2_list, df_list,
       sim_list, prob_truth, coef_list,
       elpd_popnest_list, file="simulated25temp_4.RData")
}

save(samp_data_list, samp_data2_list, df_list, 
     sim_list, prob_truth, coef_list, 
     elpd_popnest_list, file="simulated25_4.RData")

