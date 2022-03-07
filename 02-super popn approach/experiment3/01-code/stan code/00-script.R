## script for stan files
## AR prior models 
library(rstan)
options(cores=parallel::detectCores())

## generating data
source("~/GitHub/LOO_MRP/02-super popn experiment/01-code/popn2/experiment8/LOO_gen_data.R") # generate a list of things
sim_data = sim1$samp_data # extracting sample data from list

## data for stan
samp_dat <- list(
  N = nrow(sim_data), # number in samples
  N_groups_X_cat = max(sim_data$X1), # number of levels in X1, X2, X3 (categorical)
  N_groups_X4 =  max(sim_data$X4), # number of levels in X4 (depends on choice of discretisation)
  X1 = sim_data$X1,
  X2 = sim_data$X2,
  X3 = sim_data$X3,
  X4 = sim_data$X4, 
  y = sim_data$bin_value # binary outcome
)

# compile stan model
# using cmdstanr
model15_mrp = cmdstan_model(file.path(here::here('Downloads/script_re_prior_mrp_lauren.stan')))


# fitting stan model
model15_fit_mrp <- model15_mrp$sample(data=samp_dat_mrp, seed=4238) ## setting seed and set the same one in brms

# examining fit
list_of_draws = rstan::extract(model15.fit)
apply(list_of_draws$U_X1, 2, median)



## diagnostics (comparing fit to brms) ---------------------------------------------------
# same prior as brms 
model15b.fit = stan('script_brms_prior.stan', data=samp_dat, iter=4000)

# same prior as stan
bprior <- c(prior(normal(0,1), class = "sd"),
            prior(normal(0,1), class = "Intercept"))
model15 = brm(y ~ (1|X1) + (1|X2) + (1|X3) + (1|X4), data = samp_dat,
              backend = "cmdstanr",
              family = bernoulli(link = "logit"), 
              control = list(adapt_delta = 0.9),
              prior = bprior) 
ranef(model15)$X1

