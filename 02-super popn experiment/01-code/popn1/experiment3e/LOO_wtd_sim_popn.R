## simulation to test the hypothesis on weighted loo
## 01/11/2021
## generating super-population (diff in each iteration)
## fixing multiplier when generating

library(tidyverse)
library(brms) # need cmdstanr to use with brms
library(loo) # calculating loo and elpd
library(survey) # creating raked weights 

options(mc.cores = 1)

## loading external functions
source("../functions.R")

## generating 5 levels of predictors/covariates
N = 10000
J = c(5,5,5,5) # levels for each variable
popn_data <- data.frame(X1 = sample(1:J[1], N, replace= TRUE), 
                        X2 = sample(1:J[2], N, replace= TRUE),
                        X3 = sample(1:J[3], N, replace= TRUE), 
                        X4 = sample(1:J[4], N, replace= TRUE))

## generating a binary outcome 
# weakly predictive - 0.1 (sd), strongly predictive - 1 (sd)
set.seed(65438)
pn = 100 # number of population
seed = round(runif(pn, min=10, max=100000),0) # fixed seed number

