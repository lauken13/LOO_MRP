### data simulation to test the hypothesis on weighted loo
## 05/10/2021

library(tidyverse)
library(brms) # need cmdstanr to use with brms
library(loo) # calculating loo and elpd
library(survey) # creating raked weights 

options(mc.cores = 4)

## loading external functions
source("~/Documents/GitHub/LOO_MRP/01-code/functions.R")

## generating diff. levels of predictors/covariates
## state of residence (6), sex (2),  ethnicity (6), education (4), age (4)
N = 10000
J = c(5,5,5,5) # levels for each variable
popn_data <- data.frame(X1 = sample(1:J[1], N, replace= TRUE), 
                        X2 = sample(1:J[2], N, replace= TRUE),
                        X3 = sample(1:J[3], N, replace= TRUE), 
                        X4 = sample(1:J[4], N, replace= TRUE))

## generating a binary outcome 
# weakly predictive - 2 (sd), strongly predictive - 0.2 (sd)
set.seed(03210321)
popn_data$bin_outcome <- inv_logit_scaled(round(rnorm(J[1], sd=0.1),2)[popn_data$X1] + # apply inv-logit for 'simulated' coefficients
                                          round(rnorm(J[2], sd=1),2)[popn_data$X2] +
                                          round(rnorm(J[3], sd=0.1),2)[popn_data$X3] +
                                          round(rnorm(J[4], sd=1),2)[popn_data$X4])
popn_data$bin_value <- rbinom(N,1,popn_data$bin_outcome)
hist(popn_data$bin_outcome)
hist(popn_data$bin_value)

## generate inclusion prob. for each individual
# weakly predictive - 2 (sd), strongly predictive - 0.2 (sd)
popn_data$inclusion <- inv_logit_scaled(round(rnorm(J[1], sd=0.1),2)[popn_data$X1] + # apply inv-logit for 'simulated' coefficients
                                          round(rnorm(J[2], sd=0.1),2)[popn_data$X2] +
                                          round(rnorm(J[3], sd=1),2)[popn_data$X3] +
                                          round(rnorm(J[4], sd=1),2)[popn_data$X4])
hist(popn_data$inclusion)


## sample from the 'population' ####
## taking a sample data according to the inclusion probability
samp_size = 500
samp_loc = sample(1:nrow(popn_data), size = samp_size, replace=F, prob = popn_data$inclusion)
samp_data = popn_data[samp_loc,]
str(samp_data)

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

## comparing loo for two models, with weights and without weights
# calculating loo
loo1 <- loo(model1)
loo2 <- loo(model2)
loo3 <- loo(model3)
loo4 <- loo(model4)

loo_all = list(loo1, loo2, loo3, loo4)

## extracting loo estimates to rank them
loo_tab = lapply(loo_all,function(x)x$estimates[1,]) %>%
  do.call(rbind,.) %>% 
  data.frame(.)
rownames(loo_tab) = c(paste0('model',1:4))
colnames(loo_tab) = c('elpd_loo', 'SE')
(loo_sort = arrange(loo_tab, desc(elpd_loo)))


## creating survey raked weights
svy_rake = svydesign(ids=~1, # cluster id, ~1 for no clusters
                 weights=~wts, # including raked weights in the survey design
                 data=samp_data)

## rank weighted_loo according to raked weights
loo_wtd_tab = lapply(loo_all, function(x)loo_wtd(x,svy_rake)) %>% 
  do.call(rbind,.) %>% 
  data.frame(.)
rownames(loo_wtd_tab) = c(paste0('model',1:4))
(loo_wtd_sort = arrange(loo_wtd_tab, desc(wtd_elpd_loo)))



loo_compare(loo1, loo2)
loo_compare_wtd(loo1,loo2, svy_rake)

