## script for stan files
## AR prior models 
library(cmdstanr)
options(cores=parallel::detectCores())

## generating data
source(here::here("Stan Model Dev/LOO_gen_data.R")) # generate a list of things
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

#compile stan model
model15 = cmdstan_model(file.path(here::here('Stan Model Dev/script_re_prior.stan')))
#fit stan model
model15_fit <- model15$sample(data=samp_dat)

model15_fit$summary(variable = "U_X1_transformed")


## diagnostics (comparing fit to brms) ---------------------------------------------------

# same prior as stan
bprior <- c(prior(normal(0,1), class = "sd"),
            prior(normal(0,1), class = "Intercept"))
tic()
model15_brms = brm(y ~ (1|X1) + (1|X2) + (1|X3) + (1|X4), data = samp_dat,
              backend = "cmdstanr",
              family = bernoulli(link = "logit"), 
              control = list(adapt_delta = 0.9),
              prior = bprior) 
toc()
ranef(model15_brms)$X1

## Add popn preds for MRP -------------------------------------------------------------

# In your actual code you will have an Nj column too
poststrat <- data.frame(expand.grid(X1=1:5, X2 = 1:5, X3 = 1:5, X4 = 1:5))

samp_dat_mrp <- list(
  #switch to small n for sample, big N for population (standard notation)
  n = nrow(sim_data), # number in samples
  n_groups_X_cat = max(sim_data$X1), # number of levels in X1, X2, X3 (categorical)
  n_groups_X4 =  max(sim_data$X4), # number of levels in X4 (depends on choice of discretisation)
  X1 = sim_data$X1,
  X2 = sim_data$X2,
  X3 = sim_data$X3,
  X4 = sim_data$X4, 
  y = sim_data$bin_value, # binary outcome
  X1_pop = poststrat$X1,
  X2_pop = poststrat$X2,
  X3_pop = poststrat$X3,
  X4_pop = poststrat$X4, 
  Nj = nrow(poststrat)
)

# compile stan model
model15_mrp = cmdstan_model(file.path(here::here('Stan Model Dev/script_re_prior_mrp.stan')))

# fitting stan model
model15_fit_mrp <- model15_mrp$sample(data=samp_dat_mrp)

# Draws
ps_draws <- model15_fit_mrp$draws(variables = "theta_pop",format = "df")%>%
  select(-c(".chain",".draw",".iteration"))

#MRP estimate
Nj = rep(1,nrow(poststrat)) #Replace this with the actual popn counts
popn_est <- apply(ps_draws,1,function(x) sum(x*Nj)/sum(Nj))
