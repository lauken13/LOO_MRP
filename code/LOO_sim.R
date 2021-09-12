#### generate data based on 5 variables 
#### to mimic Gelman and Little (1997)
### also follows https://cran.r-project.org/web/packages/rstanarm/vignettes/mrp.html#effect-of-the-post-stratification-variable-on-preference-for-cats
### 1/9/2021 
library(tidyverse)
library(brms) # need cmdstanr to use with brms
library(loo) # calculating loo and elpd
library(survey) # creating raked weights 

options(mc.cores = parallel::detectCores())

## generating diff. levels of predictors/covariates
## state of residence (6), sex (2),  ethnicity (6), education (4), age (4)

N = 5000
J = c(6,2,6,4,4) # levels for each variable
popn_data <- data.frame(state = sample(1:J[1], N, replace= TRUE), 
                        sex = sample(1:J[2], N, replace= TRUE),
                        eth = sample(1:J[3], N, replace= TRUE), 
                        educ = sample(1:J[5], N, replace= TRUE),
                        age = sample(1:J[4], N, replace= TRUE))


## generating a binary outcome 
## i.e. preference for cats, likely to own a car/to be vaccinated or not(?)
set.seed(01092021)
popn_data$bin_outcome <- inv_logit_scaled(round(rnorm(J[1], sd=0.5),2)[popn_data$state] + # apply inv-logit for 'simulated' coefficients
                                         round(rnorm(J[2], sd=0.35),2)[popn_data$sex] +
                                       round(rnorm(J[3], sd=0.4),2)[popn_data$eth] +
                                        round(rnorm(J[4], sd=0.2),2)[popn_data$educ] + 
                                         round(rnorm(J[5], sd=0.3),2)[popn_data$age])
popn_data$bin_value <- rbinom(n,1,popn_data$bin_outcome)
hist(popn_data$bin_outcome)
hist(popn_data$bin_value)

## factorise the relevant variables
popn_data[,c(1:5,7)] = apply(popn_data[,c(1:5,7)], 2, function(x)as.factor(x))
str(popn_data)

## sample from the 'population', with inclusion prob.
# generate inclusion prob. for each variable
incl_prob = list()
incl_prob$p_state = rbeta(J[1], 1, 1)
incl_prob$p_sex = (rbeta(J[2], 1, 1))
incl_prob$p_eth = (rbeta(J[3], 1, 1))
incl_prob$p_educ = (rbeta(J[4], 1, 1))
incl_prob$p_age = (rbeta(J[5], 1, 1))

incl_prob1 = lapply(incl_prob, prop.table) # create probabilities

(incl_prob2 = lapply(incl_prob1, sum)) # check that the prob sums to 1

p_response <- rep(NA, prod(J))

## need to include inclusion probability 
n = 1000
ind = sample(1:nrow(popn_data), size=n, replace=T)
samp_data = popn_data[ind,]



## creating survey design
svy1 = svydesign(ids=~1, # cluster id, ~1 for no clusters
                 weights=~rep(1,nrow(samp_data)), # equal weights for each unit
                 data=samp_data)

## calculating population totals for each level
age_margin = xtabs(~age, data=popn_data)
state_margin = xtabs(~state, data=popn_data)
eth_margin = xtabs(~eth, data=popn_data)
sex_margin = xtabs(~sex, data=popn_data)
educ_margin = xtabs(~educ, data=popn_data)

## raked to the population
rake1 = rake(design = svy1, sample.margins = list(~age,~state,~eth, ~sex, ~educ), 
             population.margins = list(age_margin, state_margin, eth_margin, sex_margin, educ_margin))

## raked weights
samp_data$wts = weights(rake1)

## two models 
model1 = brm(bin_value ~ (1|state) + factor(sex) + (1|eth) +
               (1|age) + (1|educ), data = samp_data,
             backend = "cmdstanr",
             family = bernoulli(link = "logit"), 
             control = list(adapt_delta = 0.99)) 

model2 = brm(bin_value ~ factor(sex) + (1|eth) +
               (1|age) + (1|educ), data = samp_data,
             backend = "cmdstanr",
             family = bernoulli(link = "logit"), 
             control = list(adapt_delta = 0.99)) 


# Use loo
loo1 <- loo(model1)
loo2 <- loo(model2)

# Get the elpd
samp_data$elpd_loo1 <- loo1$pointwise[,1]
sum(elpd_loo1)

# # If you have weights
# weighted_sum <- sum(elpd_loo1*samp_data$wts)

## creating survey raked weights
svy_rake = svydesign(ids=~1, # cluster id, ~1 for no clusters
                 weights=~wts, # including raked weights in the survey design
                 data=samp_data)

svytotal(~elpd_loo1, svy_rake) # weighted elpd



