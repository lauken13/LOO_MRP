### data simulation to test the hypothesis on weighted loo
## 08/10/2021
## generating population 

library(tidyverse)
library(brms) # need cmdstanr to use with brms
library(loo) # calculating loo and elpd
library(survey) # creating raked weights 

options(mc.cores = 4)

## loading external functions
source("~/Documents/GitHub/LOO_MRP/01-code/functions.R")

## generating 5 levels of predictors/covariates
N = 10000
J = c(5,5,5,5) # levels for each variable
popn_data <- data.frame(X1 = sample(1:J[1], N, replace= TRUE), 
                        X2 = sample(1:J[2], N, replace= TRUE),
                        X3 = sample(1:J[3], N, replace= TRUE), 
                        X4 = sample(1:J[4], N, replace= TRUE))

## generating a binary outcome 
# weakly predictive - 0.1 (sd), strongly predictive - 1 (sd)
set.seed(748593)
popn_data$bin_outcome <- inv_logit_scaled(round(rnorm(J[1], sd=0.1),2)[popn_data$X1] + # apply inv-logit for 'simulated' coefficients
                                          round(rnorm(J[2], sd=1),2)[popn_data$X2] +
                                          round(rnorm(J[3], sd=0.1),2)[popn_data$X3] +
                                          round(rnorm(J[4], sd=1),2)[popn_data$X4])
popn_data$bin_value <- rbinom(N,1,popn_data$bin_outcome)
hist(popn_data$bin_outcome)
hist(popn_data$bin_value)

## generate inclusion prob. for each individual
# weakly predictive - 0.1 (sd), strongly predictive - 1 (sd)
popn_data$inclusion <- inv_logit_scaled(round(rnorm(J[1], sd=0.1),2)[popn_data$X1] + # apply inv-logit for 'simulated' coefficients
                                        round(rnorm(J[2], sd=0.1),2)[popn_data$X2] +
                                        round(rnorm(J[3], sd=1),2)[popn_data$X3] +
                                        round(rnorm(J[4], sd=1),2)[popn_data$X4])
hist(popn_data$inclusion)
