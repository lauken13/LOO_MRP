## Rstan models for AR prior models
library(tidyverse)
library(brms) # need cmdstanr to use with brms
library(loo) # calculating loo and elpd
library(survey) # creating raked weights 
library(tidybayes)

## generating data - 5 continuous predictors/covariates and a binary outcome 
gen_dat <- function(N, fx, samp_size, ITE){
  set.seed(65438)
  
  # N = 10000 # size of population
  pn = 100 # number of different population
  seed = round(runif(pn, min=10, max=100000),0) # fixed seed number
  
  ## first generate 100 different population and sample it each time
  # setting fixed seed using array ID (in cluster)
  set.seed(seed[ITE])
  popn_data <- data.frame(X1_cont = rnorm(N, 0, 2), 
                          X2_cont = rnorm(N, 0, 2),
                          X3_cont = rnorm(N, 0, 2), 
                          X4_cont = rnorm(N, 0, 2))
  
  ## transforming X4_cont to have unit scale
  popn_data$X4_tr = (popn_data$X4_cont - min(popn_data$X4_cont))/
    (max(popn_data$X4_cont) - min(popn_data$X4_cont))
  
  wkly1 = 0.1
  strg1 = 1
  
  summary(popn_data$X4_cont)
  
  ## generating continuous outcome
  popn_data$outcome <- inv_logit_scaled(wkly1*popn_data$X1_cont +
                                          strg1*popn_data$X2_cont +
                                          wkly1*popn_data$X3_cont +
                                          fx(popn_data$X4_tr))
  
  
  ## generating binary outcome
  popn_data$y <- as.numeric(rbinom(N,1,popn_data$outcome))
  
  ## generate inclusion prob. for each individual
  # weakly predictive - 0.1 (sd), strongly predictive - 1 (sd)
  wkly2 = 0.1
  strg2 = 1
  popn_data$incl_prob <- inv_logit_scaled(wkly2*popn_data$X1_cont + 
                                            wkly2*popn_data$X2_cont + 
                                            strg2*popn_data$X3_cont +
                                            strg2*popn_data$X4_tr)
  
  ## categorising the continuous covariates 
  J = 5
  K = 12
  popn_data <- popn_data %>% 
    mutate(X1_fct = cut_interval(X1_cont,J),
           X2_fct = cut_interval(X2_cont,J),
           X3_fct = cut_interval(X3_cont,J),
           X4_fct = cut_interval(X4_tr,K)) %>% 
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
  samp_loc = sample(1:nrow(popn_data), size = samp_size-(J*3 + K), replace=F, prob = popn_data$incl_prob)
  
  ## making sure at least each level of the covariates are sampled
  for(j in 1:J){
    if(length(which(popn_data$X1 == j)) == 1){
      samp_loc[length(samp_loc)+1] = which(popn_data$X1 == j)
      samp_loc[length(samp_loc)+1] = which(popn_data$X2 == j)
      samp_loc[length(samp_loc)+1] = which(popn_data$X3 == j)
    } else{
      samp_loc[length(samp_loc)+1] = sample(which(popn_data$X1 == j), size=1)
      samp_loc[length(samp_loc)+1] = sample(which(popn_data$X2 == j), size=1)
      samp_loc[length(samp_loc)+1] = sample(which(popn_data$X3 == j), size=1)
    }
  }
  
  for(k in 1:K){
    if( length(which(popn_data$X4 == k)) == 1 ){
      samp_loc[length(samp_loc)+1] = which(popn_data$X4 == k)
    } else
      samp_loc[length(samp_loc)+1] = sample(which(popn_data$X4 == k), size=1)
  }
  
  
  samp_data = popn_data[samp_loc,]
  
  # random sample
  samp_loc2 = sample(1:nrow(popn_data), size = samp_size)
  samp_data2 = popn_data[samp_loc2,]
  
  ## make poststratification table for sample
  samp_ps = samp_data %>% 
    group_by(X1, X2, X3, X4) %>% 
    summarise(n_j = n(), sum_y = sum(y), .groups = 'keep') %>% 
    ungroup()
  
  all_list <- list(samp_data, samp_ps, popn_data, N)
  names(all_list) = c('samp_data', 'samp_ps', 'popn_data', 'N')
  all_list
}

# ## iteration number (ITE) from cluster 
# slurm_arrayid <- Sys.getenv('SLURM_ARRAY_TASK_ID')
# iter = as.numeric(slurm_arrayid)

## relationship of covariate with outcome
fx1 = function(x) dbeta(x,2,2)
fx2 = function(x) 1 - dbeta(x, 2,2)
fx3 = function(x) 0.7 - (3 * exp(-x/0.2))

sim1 = gen_dat(N = 10000, fx = fx1, samp_size = 1000, ITE=52)
