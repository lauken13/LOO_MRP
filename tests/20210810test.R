
samp_data <- samp_data %>% 
  mutate(elpd_1 = loo1$pointwise[,1],
         elpd_2 = loo2$pointwise[,1],
         elpd_3 = loo3$pointwise[,1],
         elpd_4 = loo4$pointwise[,1]) 


samp_data %>% 
  group_by(X2) %>% 
  summarise(mean_elpd_2 = mean(elpd_2),
            mean_elpd_4 = mean(elpd_4),
            mean_wts = mean(wts))

samp_data %>% 
  group_by(X4) %>% 
  summarise(mean_elpd_2 = mean(elpd_2),
            mean_elpd_4 = mean(elpd_4),
            mean_wts = mean(wts))



## generating diff. levels of predictors/covariates
## state of residence (6), sex (2),  ethnicity (6), education (4), age (4)
N = 10000
J = c(5,5,5,5) # levels for each variable
popn_data <- data.frame(X1 = sample(1:J[1], N, replace= TRUE), 
                        X2 = sample(1:J[2], N, replace= TRUE),
                        X3 = sample(1:J[3], N, replace= TRUE), 
                        X4 = sample(1:J[4], N, replace= TRUE))

## generating a binary outcome 
# weakly predictive - 2 (sd), strongly predictive - 0.2 (sd)
set.seed(03210321)
popn_data$bin_outcome <- inv_logit_scaled(round(rnorm(J[2], sd=1),2)[popn_data$X2])
                                        
popn_data$bin_value <- rbinom(N,1,popn_data$bin_outcome)
hist(popn_data$bin_outcome)
hist(popn_data$bin_value)

## generate inclusion prob. for each individual
# weakly predictive - 2 (sd), strongly predictive - 0.2 (sd)
popn_data$inclusion_1 <- inv_logit_scaled(round(rnorm(J[2], sd=0.1),2)[popn_data$X2])
popn_data$inclusion_2 <- inv_logit_scaled(round(rnorm(J[2], sd=1),2)[popn_data$X2])
hist(popn_data$inclusion)


## sample from the 'population' ####
## taking a sample data according to the inclusion probability
samp_size = 500
samp_loc1 = sample(1:nrow(popn_data), size = samp_size, replace=F, prob = popn_data$inclusion_1)
samp_loc2 = sample(1:nrow(popn_data), size = samp_size, replace=F, prob = popn_data$inclusion_2)

samp_data1 = popn_data[samp_loc1,]
samp_data2 = popn_data[samp_loc2,]

model1 = brm(bin_value ~ (1|X1), data = samp_data1,
             backend = "cmdstanr",
             family = bernoulli(link = "logit"), 
             control = list(adapt_delta = 0.99))

model2 = brm(bin_value ~ (1|X1), data = samp_data2,
             backend = "cmdstanr",
             family = bernoulli(link = "logit"), 
             control = list(adapt_delta = 0.99))

model2a = brm(bin_value ~ (1|X2), data = samp_data1,
             backend = "cmdstanr",
             family = bernoulli(link = "logit"), 
             control = list(adapt_delta = 0.99))

model2b = brm(bin_value ~ (1|X2), data = samp_data2,
             backend = "cmdstanr",
             family = bernoulli(link = "logit"), 
             control = list(adapt_delta = 0.99))

## creating survey design ####
svy1 = svydesign(ids=~1, # cluster id, ~1 for no clusters
                 weights=~rep(1,nrow(samp_data)), # equal weights for each unit
                 data=samp_data)

## calculating population totals for each level
X1_margin = xtabs(~X1, data=popn_data)
X2_margin = xtabs(~X2, data=popn_data)
X3_margin = xtabs(~X3, data=popn_data)
X4_margin = xtabs(~X4, data=popn_data)

## raked to the population
rake1 = rake(design = svy1, sample.margins = list(~X1,~X3,~X4), 
             population.margins = list(X1_margin, X3_margin, X4_margin))

## raked weights 
samp_data$wts = weights(rake1)

## creating survey raked weights
svy_rake = svydesign(ids=~1, # cluster id, ~1 for no clusters
                     weights=~wts, # including raked weights in the survey design
                     data=samp_data)


loo1a = loo(model1)
loo1b = loo(model2)
loo2a = loo(model2a)
loo2b = loo(model2b)

loo_compare(loo1a, loo2a)
loo_compare(loo1b, loo2b)
loo_compare_wtd(loo1b,loo2b, svy_rake)
loo_compare_wtd(loo1a,loo2a, svy_rake)
