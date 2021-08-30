library(tidyverse)
library(brms) # You'll cmdstanr to us with brms
library(survey)
library(loo)


# use fabricatr

# (other packages like Declare Design)
# HILDA dataset - application for it. 

test_data <- data.frame(x = sample(1:4,500, replace= TRUE))
test_data$bin_outcome <- inv_logit_scaled(.5 + c(.5,-.5,-.25,.6)[test_data$x])
test_data$bin_value <- rbinom(500,1,test_data$bin_outcome)

# run a simple model:
fit1 <- brm(bin_value ~ (1|x), data = test_data,
            backend = "cmdstanr",
            family = binomial(link = "logit"))

ranef(fit1)

# Use loo

loo1 <- loo(fit1)

# Get the elpd

elpd_loo1 <- loo1$pointwise[,1]

# If you have weights

weighted_sum <- sum(elpd_loo1*wts)

# Build a little simulation study

# a show that it works and makes a difference

# Compare two models with data that is not randomly drawn from the population

# One that is "correct" the other is not

# Use the survey package to make simple raked weights 

# -  iterative proportional adjustment 

# - attempts to get margins that match population margins 

# Compare model A with model B ...

# Using wtd loo and normal loo
