## trying different priors 

source('LOO_wtd_sim_popn.R')

slurm_arrayid <- Sys.getenv('SLURM_ARRAY_TASK_ID')

ITE = as.numeric(slurm_arrayid)

# setting seed using array ID
set.seed(seed[ITE])
popn_data <- data.frame(X1_cont = rnorm(N, 0, 2), 
                        X2_cont = rnorm(N, 0, 2),
                        X3_cont = rnorm(N, 0, 2), 
                        X4_cont = rnorm(N, 0, 2))

wkly1 = 0.1
strg1 = 1

## generating continuous and binary outcome
popn_data$outcome <- inv_logit_scaled(wkly1*popn_data$X1_cont +
                                        strg1*popn_data$X2_cont +
                                        wkly1*popn_data$X3_cont +
                                        strg1*popn_data$X4_cont)
popn_data$bin_value <- as.numeric(rbinom(N,1,popn_data$outcome))


## generate inclusion prob. for each individual
# weakly predictive - 0.1 (sd), strongly predictive - 1 (sd)
wkly2 = 0.1
strg2 = 1
popn_data$incl_prob <- inv_logit_scaled(wkly2*popn_data$X1_cont + 
                                          wkly2*popn_data$X2_cont + 
                                          strg2*popn_data$X3_cont +
                                          strg2*popn_data$X4_cont)


## categorising the continuous covariates 
J = 5

popn_data <- popn_data %>% 
  mutate(X1_fct = cut_interval(X1_cont,J),
         X2_fct = cut_interval(X2_cont,J),
         X3_fct = cut_interval(X3_cont,J),
         X4_fct = cut_interval(X4_cont,J)) %>% 
  mutate(across(X1_fct:X4_fct, ~ as.numeric(.x))) 

df <- popn_data %>%
  pivot_longer(c(X1_fct:X4_fct, X1_cont:X4_cont),
               names_to = c("variable","type"),
               values_to = "value",
               names_sep = "_") %>% 
  filter(type=="fct")

popn_data <- popn_data %>%
  rename(X1 = X1_fct,
         X2 = X2_fct,
         X3 = X3_fct,
         X4 = X4_fct)

## generating samples
samp_size = 500

samp_loc = sample(1:nrow(popn_data), size = samp_size-(J*4), replace=F, prob = popn_data$incl_prob)

## making sure at least each level of the covariates are sampled
for(j in 1:J){
  samp_loc[length(samp_loc)+1] = sample(which(popn_data$X1 == j), size=1)
  samp_loc[length(samp_loc)+1] = sample(which(popn_data$X2 == j), size=1)
  samp_loc[length(samp_loc)+1] = sample(which(popn_data$X3 == j), size=1)
  samp_loc[length(samp_loc)+1] = sample(which(popn_data$X4 == j), size=1)
}

samp_data = popn_data[samp_loc,]

# random sample
samp_loc2 = sample(1:nrow(popn_data), size = samp_size)
samp_data2 = popn_data[samp_loc2,]

## factorise the relevant variables
str(samp_data)
ind = c(8:11)
samp_data[,ind] = apply(samp_data[,ind], 2, function(x)as.factor(x))

## make poststratification table for sample
samp_ps = samp_data %>% 
  group_by(X1, X2, X3, X4) %>% 
  summarise(n_j = n(), sum_y = sum(bin_value)) %>% 
  ungroup()


## all models ####
model01 = brm(sum_y | trials(n_j) ~ (1|X1), data = samp_ps,
              backend = "cmdstanr",
              family = binomial(link = "logit"), 
              control = list(adapt_delta = 0.99))

modelbern = brm(bin_value ~ (1|X1), data = samp_data,
              backend = "cmdstanr",
              family = bernoulli(link = "logit"), 
              control = list(adapt_delta = 0.99))

modeltest = brm(bin_value ~ (1|X1), data = samp_data,
                 backend = "cmdstanr",
                 family = bernoulli(link = "logit"), 
                 control = list(adapt_delta = 0.99),
                set_prior("ar(p=1)", class="sd", group="X1", coef="Intercept"))

modeltest2 = brm(bin_value ~ (1|X1), data = samp_data,
                backend = "cmdstanr",
                family = bernoulli(link = "logit"), 
                control = list(adapt_delta = 0.99),
                autocor = cor_arma(~1|X1,1))
         
model02 = brm(bin_value ~ (1|X1) + (1|X2), data = samp_data,
                backend = "cmdstanr",
                family = bernoulli(link = "logit"), 
                control = list(adapt_delta = 0.99))

model02a = brm(bin_value ~ (1|X1) + (1|X2), data = samp_data,
              backend = "cmdstanr",
              family = bernoulli(link = "logit"), 
              control = list(adapt_delta = 0.99),
              autocor = c(cor_arma(~1|X1 + ~1|X2,1)))

model02b = brm(bin_value ~ (1|X1) + (1|X2), data = samp_data,
                backend = "cmdstanr",
                family = bernoulli(link = "logit"), 
                control = list(adapt_delta = 0.99),
                set_prior("ar(p=1)", class="sd", group="X1", coef="Intercept"))

