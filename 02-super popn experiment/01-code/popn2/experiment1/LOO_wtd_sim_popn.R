## simulation to test the hypothesis on weighted loo
## 24/11/2021
## generating super-population (diff in each iteration)
## changing relationship of X to outcome 

library(tidyverse)
library(brms) # need cmdstanr to use with brms
library(loo) # calculating loo and elpd
library(survey) # creating raked weights 

options(mc.cores = 1)

## loading external functions
source("~/GitHub/LOO_MRP/02-super popn experiment/01-code/functions.R")

## generating 5 continuous predictors/covariates
N = 10000



## generating a binary outcome 
# weakly predictive - 0.1 (sd), strongly predictive - 1 (sd)
set.seed(65438)
pn = 100 # number of population
seed = round(runif(pn, min=10, max=100000),0) # fixed seed number

