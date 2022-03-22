## Rstan models for AR prior models
library(tidyverse)
library(brms) # need cmdstanr to use with brms
library(loo) # calculating loo and elpd
library(survey) # creating raked weights 
library(tidybayes)

## generating data - 5 continuous predictors/covariates and a binary outcome 
gen_dat <- function(N, fx, samp_size, ITE){
  set.seed(65438)
  
  pn = 100 # number of different population
  seed = round(runif(pn, min=10, max=100000),0) # fixed seed number
  
  ## first generate 100 different population and sample it each time
  # setting fixed seed using array ID (in cluster)
  set.seed(seed[ITE])
  popn_data <- data.frame(X1_cont = runif(N, -3, 3), 
                          X2_cont = runif(N, -3, 3),
                          X3_cont = runif(N, -3, 3), 
                          X4_cont = runif(N, -3, 3))
  
  ## transforming X4_cont to have normalised scale -- for the beta function in fx
  popn_data$X4_tr = (popn_data$X4_cont - min(popn_data$X4_cont))/
    (max(popn_data$X4_cont) - min(popn_data$X4_cont))
  
  wkly1 = 0.1/2
  strg1 = 1/2
  
  summary(popn_data$X4_cont)
  
  ## generating continuous outcome
  # using -1 as the intercept for we have a better spread of y_prob (probability of outcome)
  popn_data$y_prob <- inv_logit_scaled(-1 + wkly1*popn_data$X1_cont + 
                                          strg1*popn_data$X2_cont +
                                          wkly1*popn_data$X3_cont +
                                          1.5*fx(popn_data$X4_tr))
  
  
  ## generating binary outcome
  popn_data$y_obs <- as.numeric(rbinom(N,1,popn_data$y_prob))
  
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
  K = 12
  popn_data  = popn_data %>% 
    mutate(X1_fct = cut_interval(X1_cont,J, labels=F),
           X2_fct = cut_interval(X2_cont,J, labels=F),
           X3_fct = cut_interval(X3_cont,J, labels=F),
           X4_fct = cut_interval(X4_tr,K, labels=F)) %>%  
    mutate(across(X1_fct:X4_fct, ~ as.factor(.x))) 
  
  popn_data <- popn_data %>% 
    rename(X1 = X1_fct,
           X2 = X2_fct,
           X3 = X3_fct,
           X4 = X4_fct)
  
    
  ## generating samples
  samp_loc = sample(1:nrow(popn_data), size = samp_size-(J*3 + K), replace=F, prob = popn_data$incl_prob)
  
  ## making sure at least each level of the covariates are sampled
  for(j in 1:J){
    X1_loc_j = which(popn_data$X1 == j)
    X2_loc_j = which(popn_data$X2 == j)
    X3_loc_j = which(popn_data$X3 == j)
      samp_loc[length(samp_loc)+1] = sample(X1_loc_j, size=1)
      samp_loc[length(samp_loc)+1] = sample(X2_loc_j, size=1)
      samp_loc[length(samp_loc)+1] = sample(X3_loc_j, size=1)
  }
  
  for(k in 1:K){
    if(length(which(popn_data$X4 == k)) == 1 ){ # for when only 1 indv. exist in that popn. level
      samp_loc[length(samp_loc)+1] = which(popn_data$X4 == k)
    } else
      samp_loc[length(samp_loc)+1] = sample(which(popn_data$X4 == k), size=1)
  }
  
  samp_data = popn_data[samp_loc,]

  ## make poststrat table for popn
  popn_ps = popn_data %>% 
    group_by(X1, X2, X3, X4) %>% 
    summarise(Nj = n(), prob_out = mean(y_prob)) %>% 
    ungroup()
  
  all_list <- list(samp_data, popn_ps, popn_data, N)
  names(all_list) = c('samp_data', 'popn_ps', 'popn_data', 'N')
  all_list
}

## relationship of covariate with outcome
fx1 = function(x) {dbeta(x,2,2)}
fx2 = function(x) {1 - dbeta(x, 2,2)}
fx3 = function(x) {0.7 - (3 * exp(-x/0.2))}

