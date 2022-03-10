## script for stan files
## AR prior models 
library(cmdstanr)
options(cores=parallel::detectCores())

# generating data ---------------------------------------------------------
source(here::here("02-super popn approach/experiment3/01-code/stan code/00-LOO_gen_data.R")) # generate a list of things
samp_dat = sim1$samp_data
popn_ps = sim1$popn_ps

# diagnostics (comparing fit to brms) ---------------------------------------------------
# same priors as cmdstanr 
bprior <- c(prior(normal(0,1), class = "sd"),
            prior(normal(0,1), class = "Intercept"))
model15 = brm(y ~ (1|X1) + (1|X2) + (1|X3) + (1|X4), 
              data = samp_dat, 
              seed = 2345,
              backend = "cmdstanr",
              prior = bprior,
              family = bernoulli(link = "logit"), 
              control = list(adapt_delta = 0.9)) 
stancode(model15)
prior_summary(model15)
summary(model15)$random

# list of data for stan
samp_dat_mrp <- list(
  n = nrow(samp_dat), # number in samples
  n_groups_X_cat = max(samp_dat$X1), # number of levels in X1, X2, X3 (categorical)
  n_groups_X4 =  max(samp_dat$X4), # number of levels in X4 (depends on choice of discretisation)
  X1 = samp_dat$X1,
  X2 = samp_dat$X2,
  X3 = samp_dat$X3,
  X4 = samp_dat$X4, 
  y = samp_dat$y, # binary outcome
  X1_pop = popn_ps$X1,
  X2_pop = popn_ps$X2,
  X3_pop = popn_ps$X3,
  X4_pop = popn_ps$X4, 
  N = nrow(popn_ps)
)


# using cmdstanr ----------------------------------------------------------
# compile stan model
model15_mrp = cmdstan_model(file.path(here::here('02-super popn approach/experiment3/01-code/stan code/01-re_prior_edit.stan')))

# fitting stan model - run MCMC using the 'sample' method
model15_fit_mrp <- model15_mrp$sample(data = samp_dat_mrp, 
                                      seed = 2345, chains =2, num_samples = 100) # setting seed within sampling

model15_fit_mrp$draws(variables = "log_lik")

# comparing fit between brms and cmdstanr
ranef(model15)$X1
model15_fit_mrp$summary("U_X1")


# running the brms stancode with cmdstanr
code <- stancode(model15)

dat = standata(model15)
dat$n = 1000
dat$n_groups_X_cat = 5
dat$y = samp_dat$y
dat$n_groups_X4 = 12
dat$X1 = samp_dat$X1
dat$X2 = samp_dat$X2
dat$X3 = samp_dat$X3
dat$X4 = samp_dat$X4
dat$Nj = nrow(poststrat)

## cmdstanr
# compiling
model15_brms1 = cmdstan_model(file.path(here::here('02-super popn approach/experiment3/01-code/stan code/brms_same_prior_renamed.stan')))

# fitting stan model - run MCMC using the 'sample' method
model15_fit_brms1 <- model15_brms1$sample(data=dat, 
                                          seed = 2345, chains=4) # setting seed within sampling

model15_fit_mrp$summary("U_X1_transformed")
model15_fit_brms1$summary("r_1_1")


# checking against brms default priors --------------------------------------------
model15_brms2 = brm(y ~ (1|X1) + (1|X2) + (1|X3) + (1|X4), 
              data = samp_dat, 
              seed = 2345,
              backend = "cmdstanr",
              family = bernoulli(link = "logit"), 
              control = list(adapt_delta = 0.9)) 

# using cmdstanr
model15_brms2 = cmdstan_model(file.path(here::here('02-super popn approach/experiment3/01-code/stan code/02-brms_prior.stan')))

# fitting stan model - run MCMC using the 'sample' method
model15_fit_brms2 <- model15_brms2$sample(data=samp_dat_mrp, 
                                      seed = 2345, chains=4) # setting seed within sampling

# comparing fit between brms and cmdstanr
ranef(model15_brms2)$X2
model15_fit_brms2$summary("U_X2")

