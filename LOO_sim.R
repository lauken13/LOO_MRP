#### generate data based on 5 variables 
#### to mimic Gelman and Little (1997)
### also follows https://cran.r-project.org/web/packages/rstanarm/vignettes/mrp.html#effect-of-the-post-stratification-variable-on-preference-for-cats
### 1/9/2021 

library(tidyverse)
library(brms) # You'll need cmdstanr to use with brms
library(survey)
library(loo)


## X1 - state of residence (6), X2 - sex (2), X3 - ethnicity (6), 
# X4 - age (4), X5 - education (4)
n = 5000
J = c(6,2,6,4,4) # levels for each variable
sim_data <- data.frame(X1 = sample(1:J[1], n, replace= TRUE), X2 = sample(1:J[2], n, replace= TRUE),
                        X3 = sample(1:J[3], n, replace= TRUE), X4 = sample(1:J[4], n, replace= TRUE),
                        X5 = sample(1:J[5], n, replace= TRUE))

## generating a binary outcome 
## i.e. preference for cats, own a car or not, gotten vaccinated or not(?)
set.seed(01092021)
sim_data$bin_outcome <- inv_logit_scaled(round(rnorm(J[1], sd=0.5),2)[sim_data$X1] + # apply inv-logit for 'simulated' coefficients
                                         round(rnorm(J[2], sd=0.5),2)[sim_data$X2] +
                                       round(rnorm(J[3], sd=0.5),2)[sim_data$X3] +
                                        round(rnorm(J[4], sd=0.5),2)[sim_data$X4] +
                                        round(rnorm(J[5], sd=0.5),2)[sim_data$X5])
sim_data$bin_value <- rbinom(n,1,sim_data$bin_outcome)
hist(sim_data$bin_outcome)
hist(sim_data$bin_value)

names(sim_data) = c('state', 'sex', 'eth', 'age', 'educ', 'bin_out', 'bin_val')
sim_data[,1:5] = apply(sim_data[,1:5], 2, function(x)as.factor(x))


## sample from the 'population' 
## create raked weights using survey package
library(weights)

target = apply(sim_data[,1:5],2, wpct)

ind = sample(1:nrow(sim_data), size=1000, replace=T)
samp_surv = sim_data[ind,]

samp_marg = apply(samp_surv[,1:5],2, wpct)



# running a simple hierarchical model using brm
fit1 <- brm(bin_val ~ (1|state) + factor(sex) + (1|eth) +
              (1|age) + (1|educ), data = sim_data,
            backend = "cmdstanr",
            family = bernoulli(link = "logit"), iter=8000, warmup = 200, chains=2,
            control = list(adapt_delta = 0.99)) 

ranef(fit1)
ranef(fit2)
fit2 <- stan_glmer(bin_val ~ (1|state) + factor(sex) + (1|eth) +
             (1|age) + (1|educ),
  family = binomial("logit"),
  data = sim_data,
  refresh = 0
)
