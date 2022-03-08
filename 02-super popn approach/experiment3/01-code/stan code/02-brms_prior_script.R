# Script for comparing brm() model to 02-brms_prior.stan 

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