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
popn_data$bin_value <- rbinom(N,1,popn_data$bin_outcome)
hist(popn_data$bin_outcome)
hist(popn_data$bin_value)


## sample from the 'population', with inclusion prob.
# generate inclusion prob. for each individual
popn_data$inclusion <- inv_logit_scaled(round(rnorm(J[1], sd=0.2),2)[popn_data$state] + # apply inv-logit for 'simulated' coefficients
                                            round(rnorm(J[2], sd=0.5),2)[popn_data$sex] +
                                            round(rnorm(J[3], sd=0.2),2)[popn_data$eth] +
                                            round(rnorm(J[4], sd=0.1),2)[popn_data$educ] + 
                                            round(rnorm(J[5], sd=0.4),2)[popn_data$age])
hist(popn_data$inclusion)

## checking the inclusion probabilities
popn_data[which(popn_data$inclusion == popn_data$inclusion[5]),]

## factorise the relevant variables
popn_data[,c(1:5,7)] = apply(popn_data[,c(1:5,7)], 2, function(x)as.factor(x))
str(popn_data)

## taking a sample data according to the inclusion probability
samp_size = 1000
samp_loc = sample(1:nrow(popn_data), size = samp_size, replace=F, prob = popn_data$inclusion)
samp_data = popn_data[samp_loc,]
str(samp_data)

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


# calculating loo
loo1 <- loo(model1)
loo2 <- loo(model2)

loo_compare(list(loo1,loo2))

# getting the elpd
samp_data$elpd_loo1 <- loo1$pointwise[,1]
samp_data$elpd_loo2 <- loo2$pointwise[,1]
sum(samp_data$elpd_loo)
samp_data$elpd_diff = samp_data$elpd_loo2 - samp_data$elpd_loo1

## manual calculation of the std error of diff
# sqrt(samp_size) * sd(loo2$pointwise[,1] - loo1$pointwise[,1]) 

# # If you have weights
# weighted_sum <- sum(elpd_loo1*samp_data$wts)

## creating survey raked weights
svy_rake = svydesign(ids=~1, # cluster id, ~1 for no clusters
                 weights=~wts, # including raked weights in the survey design
                 data=samp_data)

svytotal(~samp_data$elpd_diff, svy_rake) # weighted elpd


## checking using svytotal() and loo_compare() for equal weights
svy_rake1 = svydesign(ids=~1, # cluster id, ~1 for no clusters
                     weights=~1, # including raked weights in the survey design
                     data=samp_data)

svytotal(~samp_data$elpd_diff, svy_rake1) # weighted elpd
loo_compare(loo1, loo2)
