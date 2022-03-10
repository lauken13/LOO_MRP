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

# using cmdstanr ----------------------------------------------------------
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

# compile stan model
model15_mrp = cmdstan_model(file.path(here::here('02-super popn approach/experiment3/01-code/stan code/01-re_prior.stan')))

# fitting stan model - run MCMC using the 'sample' method
model15_fit_mrp <- model15_mrp$sample(data = samp_dat_mrp, 
                                      seed = 2345) # setting seed within sampling

model15_fit_mrp$draws(variables = "log_lik")

# comparing fit between brms and cmdstanr
ranef(model15)$X2
model15_fit_mrp$summary("U_X2")

