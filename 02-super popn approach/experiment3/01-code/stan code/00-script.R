## script for stan files
## AR prior models 
library(cmdstanr)
options(cores=parallel::detectCores())

## generating data
source(here::here("02-super popn approach/experiment3/01-code/stan code/00-LOO_gen_data.R")) # generate a list of things
samp_dat = sim1$samp_data

# !! placeholder for poststrat matrix - need to add it in data generation later
poststrat <- data.frame(expand.grid(X1=1:5, X2 = 1:5, X3 = 1:5, X4 = 1:5))

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
  X1_pop = poststrat$X1,
  X2_pop = poststrat$X2,
  X3_pop = poststrat$X3,
  X4_pop = poststrat$X4, 
  Nj = nrow(poststrat)
)

# compile stan model


# using cmdstanr
model15_mrp = cmdstan_model(file.path(here::here('02-super popn approach/experiment3/01-code/stan code/01-re_prior.stan')))

# fitting stan model - run MCMC using the 'sample' method
model15_fit_mrp <- model15_mrp$sample(data=samp_dat_mrp, 
                                      seed = 2345, chains=4) # setting seed within sampling


# poststratification ------------------------------------------------------
# # Draws
# ps_draws <- model15_fit_mrp$draws(variables = "theta_pop",format = "df")%>%
#   select(-c(".chain",".draw",".iteration"))
# 
# # MRP estimate
# Nj = rep(1,nrow(poststrat)) #Replace this with the actual popn counts
# popn_est <- apply(ps_draws,1,function(x) sum(x*Nj)/sum(Nj))


# diagnostics (comparing fit to brms) ---------------------------------------------------
# same priors as stan 
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

# comparing fit between brms and cmdstanr
ranef(model15)$X1
model15_fit_mrp$summary("U_X1_transformed")
# running the brms stancode with cmdstanr

code <- stancode(model15)

model15_brms = cmdstan_model(file.path(here::here('02-super popn approach/experiment3/01-code/stan code/brms_same_prior.stan')))

# fitting stan model - run MCMC using the 'sample' method
model15_fit_brms <- model15_brms$sample(data=standata(model15), 
                                      seed = 2345, chains=4) # setting seed within sampling

model15_fit_mrp$summary("U_X1_transformed")
model15_fit_brms$summary("r_1_1")


# same as brms default priors --------------------------------------------
model15_brms = brm(y ~ (1|X1) + (1|X2) + (1|X3) + (1|X4), 
              data = samp_dat, 
              seed = 2345,
              backend = "cmdstanr",
              family = bernoulli(link = "logit"), 
              control = list(adapt_delta = 0.9)) 

# using cmdstanr
model15_brms_prior = cmdstan_model(file.path(here::here('02-super popn approach/experiment3/01-code/stan code/02-brms_prior.stan')))

# fitting stan model - run MCMC using the 'sample' method
model15_fit_brms_prior <- model15_brms_prior$sample(data=samp_dat_mrp, 
                                      seed = 2345, chains=4) # setting seed within sampling

# comparing fit between brms and cmdstanr
ranef(model15_brms)$X2
model15_fit_brms_prior$summary("U_X2")

