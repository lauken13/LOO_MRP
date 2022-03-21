## models 1-4, 15
## fitting a model with strong relationship (to use AR prior)

source('LOO_wtd_sim_popn.R')

slurm_arrayid <- Sys.getenv('SLURM_ARRAY_TASK_ID')

ITE = as.numeric(slurm_arrayid)

## putting samp size here to test the script
samp_size = 1000



# setting seed using array ID
set.seed(seed[ITE])
popn_data <- data.frame(X1_cont = rnorm(N, 0, 2), 
                        X2_cont = rnorm(N, 0, 2),
                        X3_cont = rnorm(N, 0, 2), 
                        X4_cont = rnorm(N, 0, 2))

## transforming X4_cont to have unit scale
popn_data$X4_tr = (popn_data$X4_cont - min(popn_data$X4_cont))/(max(popn_data$X4_cont) - min(popn_data$X4_cont))
# summary(popn_data$X4_tr)

fx1 = function(x) dbeta(x,2,2)
fx2 = function(x) 1 - dbeta(x, 2,2)
fx3 = function(x) 0.7 - (3 * exp(-x/0.2))

wkly1 = 0.1
strg1 = 1

summary(popn_data$X4_cont)

## generating continuous outcome
popn_data$outcome <- inv_logit_scaled(wkly1*popn_data$X1_cont +
                                        strg1*popn_data$X2_cont +
                                        wkly1*popn_data$X3_cont +
                                        fx2(popn_data$X4_tr))
# par(mfrow=c(2,2))
# plot((popn_data$X4_tr), fx1(popn_data$X4_tr))
# plot((popn_data$X4_tr), fx2(popn_data$X4_tr))
# plot((popn_data$X4_tr), fx3(popn_data$X4_tr))
# dev.off()

## generating binary outcome
popn_data$bin_value <- as.numeric(rbinom(N,1,popn_data$outcome))

## generate inclusion prob. for each individual
# weakly predictive - 0.1 (sd), strongly predictive - 1 (sd)
wkly2 = 0.1
strg2 = 1
popn_data$incl_prob <- inv_logit_scaled(wkly2*popn_data$X1_cont + 
                                          wkly2*popn_data$X2_cont + 
                                          strg2*popn_data$X3_cont +
                                          strg2*popn_data$X4_tr)

## categorising the continuous covariates 
J = 5
K = 12
popn_data <- popn_data %>% 
  mutate(X1_fct = cut_interval(X1_cont,J),
         X2_fct = cut_interval(X2_cont,J),
         X3_fct = cut_interval(X3_cont,J),
         X4_fct = cut_interval(X4_tr,K)) %>% 
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
samp_loc = sample(1:nrow(popn_data), size = samp_size-(J*3 + K), replace=F, prob = popn_data$incl_prob)

## making sure at least each level of the covariates are sampled
for(j in 1:J){
  if(length(which(popn_data$X1 == j)) == 1){
    samp_loc[length(samp_loc)+1] = which(popn_data$X1 == j)
    samp_loc[length(samp_loc)+1] = which(popn_data$X2 == j)
    samp_loc[length(samp_loc)+1] = which(popn_data$X3 == j)
  } else{
  samp_loc[length(samp_loc)+1] = sample(which(popn_data$X1 == j), size=1)
  samp_loc[length(samp_loc)+1] = sample(which(popn_data$X2 == j), size=1)
  samp_loc[length(samp_loc)+1] = sample(which(popn_data$X3 == j), size=1)
  }
}

for(k in 1:K){
  if( length(which(popn_data$X4 == k)) == 1 ){
    samp_loc[length(samp_loc)+1] = which(popn_data$X4 == k)
  } else
    samp_loc[length(samp_loc)+1] = sample(which(popn_data$X4 == k), size=1)
}

samp_data = popn_data[samp_loc,]

# random sample
samp_loc2 = sample(1:nrow(popn_data), size = samp_size)
samp_data2 = popn_data[samp_loc2,]

## factorise the relevant variables
str(samp_data)
ind = c(9:12)
samp_data[,ind] = apply(samp_data[,ind], 2, function(x)as.factor(x))

## make poststratification table for sample
samp_ps = samp_data %>% 
  group_by(X1, X2, X3, X4) %>% 
  summarise(n_j = n(), sum_y = sum(bin_value)) %>% 
  ungroup()

## creating survey design
svy1 = svydesign(ids=~1, # cluster id, ~1 for no clusters
                 weights=~rep(1,nrow(samp_data)), # equal weights for each unit
                 data=samp_data)

## calculating population totals for each level
X1_margin = xtabs(~X1, data=popn_data)
X2_margin = xtabs(~X2, data=popn_data)
X3_margin = xtabs(~X3, data=popn_data)
X4_margin = xtabs(~X4, data=popn_data)

## raked to the population
rake1 = rake(design = svy1, sample.margins = list(~X1,~X2,~X3,~X4), 
             population.margins = list(X1_margin, X2_margin, X3_margin, X4_margin))

## raked weights ####
samp_data$wts = weights(rake1)

## all models ####
model01 = brm(bin_value ~ (1|X1), data = samp_data,
              backend = "cmdstanr",
              family = bernoulli(link = "logit"), 
              control = list(adapt_delta = 0.99))

model02 = brm(bin_value ~ (1|X2), data = samp_data,
              backend = "cmdstanr",
              family = bernoulli(link = "logit"), 
              control = list(adapt_delta = 0.99)) 

model03 = brm(bin_value ~ (1|X3), data = samp_data,
              backend = "cmdstanr",
              family = bernoulli(link = "logit"), 
              control = list(adapt_delta = 0.99)) 

model04 = brm(bin_value ~ (1|X4), data = samp_data,
              backend = "cmdstanr",
              family = bernoulli(link = "logit"), 
              control = list(adapt_delta = 0.99) )

model04a = brm(bin_value ~ 1,
               autocor = cor_arma(~1|X4,p=1, cov=T),
              data = samp_data,
              backend = "cmdstanr",
              family = bernoulli(link = "logit"), 
              control = list(adapt_delta = 0.99),
              prior = set_prior("normal(0, 10)", class = "ar"))


model05 = brm(bin_value ~ (1|X1) + (1|X2), data = samp_data,
              backend = "cmdstanr",
              family = bernoulli(link = "logit"), 
              control = list(adapt_delta = 0.99))

model06 = brm(bin_value ~ (1|X1) + (1|X3), data = samp_data,
              backend = "cmdstanr",
              family = bernoulli(link = "logit"), 
              control = list(adapt_delta = 0.99))

model07 = brm(bin_value ~ (1|X1) + (1|X4), data = samp_data,
              backend = "cmdstanr",
              family = bernoulli(link = "logit"), 
              control = list(adapt_delta = 0.99))

model07a = brm(bin_value ~ (1|X1), data = samp_data,
               autocor = cor_arma(~1|X4,p=1, cov=T),
               backend = "cmdstanr",
               family = bernoulli(link = "logit"), 
               control = list(adapt_delta = 0.99),
               prior = set_prior("normal(0, 10)", class = "ar"))

model08 = brm(bin_value ~ (1|X2) + (1|X3), data = samp_data,
              backend = "cmdstanr",
              family = bernoulli(link = "logit"), 
              control = list(adapt_delta = 0.99)) 

model09 = brm(bin_value ~ (1|X2) + (1|X4), data = samp_data,
              backend = "cmdstanr",
              family = bernoulli(link = "logit"), 
              control = list(adapt_delta = 0.99)) 

model09a = brm(bin_value ~ (1|X2), data = samp_data,
              autocor = cor_arma(~1|X4,p=1, cov=T),
              backend = "cmdstanr",
              family = bernoulli(link = "logit"), 
              control = list(adapt_delta = 0.99),
              prior = set_prior("normal(0, 10)", class = "ar")) 

model10 = brm(bin_value ~ (1|X3) + (1|X4), data = samp_data,
              backend = "cmdstanr",
              family = bernoulli(link = "logit"), 
              control = list(adapt_delta = 0.99)) 

model10a = brm(bin_value ~ (1|X3), data = samp_data,
               autocor = cor_arma(~1|X4,p=1, cov=T),
              backend = "cmdstanr",
              family = bernoulli(link = "logit"), 
              control = list(adapt_delta = 0.99),
              prior = set_prior("normal(0, 10)", class = "ar")) 

model11 = brm(bin_value ~ (1|X1) + (1|X2) + (1|X3), data = samp_data,
              backend = "cmdstanr",
              family = bernoulli(link = "logit"), 
              control = list(adapt_delta = 0.99)) 

model12 = brm(bin_value ~ (1|X1) + (1|X2) + (1|X4), data = samp_data,
              backend = "cmdstanr",
              family = bernoulli(link = "logit"), 
              control = list(adapt_delta = 0.99)) 


model12a = brm(bin_value ~ (1|X1) + (1|X2), data = samp_data,
               autocor = cor_arma(~1|X4,p=1, cov=T),
              backend = "cmdstanr",
              family = bernoulli(link = "logit"), 
              control = list(adapt_delta = 0.99),
              prior = set_prior("normal(0, 10)", class = "ar")) 


model13 = brm(bin_value ~ (1|X1) + (1|X3) + (1|X4), data = samp_data,
              backend = "cmdstanr",
              family = bernoulli(link = "logit"), 
              control = list(adapt_delta = 0.99))

model13a = brm(bin_value ~ (1|X1) + (1|X3), data = samp_data,
               autocor = cor_arma(~1|X4,p=1, cov=T),
              backend = "cmdstanr",
              family = bernoulli(link = "logit"), 
              control = list(adapt_delta = 0.99),
              prior = set_prior("normal(0, 10)", class = "ar"))

model14 = brm(bin_value ~ (1|X2) + (1|X3) + (1|X4), data = samp_data,
              backend = "cmdstanr",
              family = bernoulli(link = "logit"), 
              control = list(adapt_delta = 0.99)) 

model14a = brm(bin_value ~ (1|X2) + (1|X3), data = samp_data,
              autocor = cor_arma(~1|X4,p=1, cov=T),
              backend = "cmdstanr",
              family = bernoulli(link = "logit"), 
              control = list(adapt_delta = 0.99),
              prior = set_prior("normal(0, 10)", class = "ar")) 

model15 = brm(bin_value ~ (1|X1) + (1|X2) + (1|X3) + (1|X4), data = samp_data,
              backend = "cmdstanr",
              family = bernoulli(link = "logit"), 
              control = list(adapt_delta = 0.99)) 

model15a = brm(bin_value ~ (1|X1) + (1|X2) + (1|X3), data = samp_data,
              autocor = cor_arma(~1|X4,p=1, cov=T),
              backend = "cmdstanr",
              family = bernoulli(link = "logit"), 
              control = list(adapt_delta = 0.99),
              prior = set_prior("normal(0, 10)", class = "ar")) 


## make MRP estimates
popn_ps = popn_data %>% 
  group_by(X1, X2, X3, X4) %>% 
  summarise(Nj = n()) %>% 
  ungroup()

prob_truth = mean(popn_data$bin_value)

coef_list = c(coef(model01), coef(model02), coef(model03), coef(model04), 
              coef(model05), coef(model06), coef(model07), coef(model08),
              coef(model09), coef(model10), coef(model11), coef(model12),
              coef(model13), coef(model14), coef(model15), fixef(model04a),
    	      coef(model07a), coef(model09a), coef(model10a), coef(model12a),
	      coef(model13a), coef(model14a), coef(model15a)) 
names(coef_list) = c(paste0("01.",names(coef(model01))[grep("*", names(coef(model01)))]),
                     paste0("02.",names(coef(model02))[grep("*", names(coef(model02)))]),
                     paste0("03.",names(coef(model03))[grep("*", names(coef(model03)))]),
                     paste0("04.",names(coef(model04))[grep("*", names(coef(model04)))]),
                     paste0("05.",names(coef(model05))[grep("*", names(coef(model05)))]),
                     paste0("06.",names(coef(model06))[grep("*", names(coef(model06)))]),
                     paste0("07.",names(coef(model07))[grep("*", names(coef(model07)))]),
                     paste0("08.",names(coef(model08))[grep("*", names(coef(model08)))]),
                     paste0("09.",names(coef(model09))[grep("*", names(coef(model09)))]),
                     paste0("10.",names(coef(model10))[grep("*", names(coef(model10)))]),
                     paste0("11.",names(coef(model11))[grep("*", names(coef(model11)))]),
                     paste0("12.",names(coef(model12))[grep("*", names(coef(model12)))]),
                     paste0("13.",names(coef(model13))[grep("*", names(coef(model13)))]),
                     paste0("14.",names(coef(model14))[grep("*", names(coef(model14)))]),
                     paste0("15.",names(coef(model15))[grep("*", names(coef(model15)))]))

## saving stan summaries
summ01 = model01 %>% 
  spread_draws(c(treedepth__, energy__, divergent__)) %>% 
  summarise(mean_td = mean(treedepth__), 
            mean_en = mean(energy__),
            mean_di = mean(divergent__),
            median_td = median(treedepth__),
            median_en = median(energy__),
            median_di = median(divergent__))

summ02 = model02 %>% 
  spread_draws(c(treedepth__, energy__, divergent__)) %>% 
  summarise(mean_td = mean(treedepth__), 
            mean_en = mean(energy__),
            mean_di = mean(divergent__),
            median_td = median(treedepth__),
            median_en = median(energy__),
            median_di = median(divergent__))

summ03 = model03 %>% 
  spread_draws(c(treedepth__, energy__, divergent__)) %>% 
  summarise(mean_td = mean(treedepth__), 
            mean_en = mean(energy__),
            mean_di = mean(divergent__),
            median_td = median(treedepth__),
            median_en = median(energy__),
            median_di = median(divergent__))

summ04 = model04 %>% 
  spread_draws(c(treedepth__, energy__, divergent__)) %>% 
  summarise(mean_td = mean(treedepth__), 
            mean_en = mean(energy__),
            mean_di = mean(divergent__),
            median_td = median(treedepth__),
            median_en = median(energy__),
            median_di = median(divergent__))

summ05 = model05 %>% 
  spread_draws(c(treedepth__, energy__, divergent__)) %>% 
  summarise(mean_td = mean(treedepth__), 
            mean_en = mean(energy__),
            mean_di = mean(divergent__),
            median_td = median(treedepth__),
            median_en = median(energy__),
            median_di = median(divergent__))

summ06 = model06 %>% 
  spread_draws(c(treedepth__, energy__, divergent__)) %>% 
  summarise(mean_td = mean(treedepth__), 
            mean_en = mean(energy__),
	    mean_di = mean(divergent__),
            median_td = median(treedepth__),
            median_en = median(energy__),
            median_di = median(divergent__))

summ07 = model07 %>% 
  spread_draws(c(treedepth__, energy__, divergent__)) %>% 
  summarise(mean_td = mean(treedepth__), 
            mean_en = mean(energy__),
            mean_di = mean(divergent__),
            median_td = median(treedepth__),
            median_en = median(energy__),
            median_di = median(divergent__))

summ08 = model08 %>% 
  spread_draws(c(treedepth__, energy__, divergent__)) %>% 
  summarise(mean_td = mean(treedepth__), 
            mean_en = mean(energy__),
            mean_di = mean(divergent__),
            median_td = median(treedepth__),
            median_en = median(energy__),
            median_di = median(divergent__))

summ09 = model09 %>% 
  spread_draws(c(treedepth__, energy__, divergent__)) %>% 
  summarise(mean_td = mean(treedepth__), 
            mean_en = mean(energy__),
            mean_di = mean(divergent__),
            median_td = median(treedepth__),
            median_en = median(energy__),
            median_di = median(divergent__))

summ10 = model10 %>% 
  spread_draws(c(treedepth__, energy__, divergent__)) %>% 
  summarise(mean_td = mean(treedepth__), 
            mean_en = mean(energy__),
            mean_di = mean(divergent__),
            median_td = median(treedepth__),
            median_en = median(energy__),
            median_di = median(divergent__))

summ11 = model11 %>% 
  spread_draws(c(treedepth__, energy__, divergent__)) %>% 
  summarise(mean_td = mean(treedepth__), 
            mean_en = mean(energy__),
            mean_di = mean(divergent__),
            median_td = median(treedepth__),
            median_en = median(energy__),
            median_di = median(divergent__))

summ12 = model12 %>% 
  spread_draws(c(treedepth__, energy__, divergent__)) %>% 
  summarise(mean_td = mean(treedepth__), 
            mean_en = mean(energy__),
            mean_di = mean(divergent__),
            median_td = median(treedepth__),
            median_en = median(energy__),
            median_di = median(divergent__))

summ13 = model13 %>% 
  spread_draws(c(treedepth__, energy__, divergent__)) %>% 
  summarise(mean_td = mean(treedepth__), 
            mean_en = mean(energy__),
            mean_di = mean(divergent__),
            median_td = median(treedepth__),
            median_en = median(energy__),
            median_di = median(divergent__))

summ14 = model14 %>% 
  spread_draws(c(treedepth__, energy__, divergent__)) %>% 
  summarise(mean_td = mean(treedepth__), 
            mean_en = mean(energy__),
            mean_di = mean(divergent__),
            median_td = median(treedepth__),
            median_en = median(energy__),
            median_di = median(divergent__))

summ15 = model15 %>% 
  spread_draws(c(treedepth__, energy__, divergent__)) %>% 
  summarise(mean_td = mean(treedepth__), 
            mean_en = mean(energy__),
            mean_di = mean(divergent__),
            median_td = median(treedepth__),
            median_en = median(energy__),
            median_di = median(divergent__))

summ04a = model04a %>% 
  spread_draws(c(treedepth__, energy__, divergent__)) %>% 
  summarise(mean_td = mean(treedepth__), 
            mean_en = mean(energy__),
            mean_di = mean(divergent__),
            median_td = median(treedepth__),
            median_en = median(energy__),
            median_di = median(divergent__))

summ07a = model07a %>% 
  spread_draws(c(treedepth__, energy__, divergent__)) %>% 
  summarise(mean_td = mean(treedepth__), 
            mean_en = mean(energy__),
            mean_di = mean(divergent__),
            median_td = median(treedepth__),
            median_en = median(energy__),
            median_di = median(divergent__))

summ09a = model09a %>% 
  spread_draws(c(treedepth__, energy__, divergent__)) %>% 
  summarise(mean_td = mean(treedepth__), 
            mean_en = mean(energy__),
            mean_di = mean(divergent__),
            median_td = median(treedepth__),
            median_en = median(energy__),
            median_di = median(divergent__))

summ10a = model10a %>% 
  spread_draws(c(treedepth__, energy__, divergent__)) %>% 
  summarise(mean_td = mean(treedepth__), 
            mean_en = mean(energy__),
            mean_di = mean(divergent__),
            median_td = median(treedepth__),
            median_en = median(energy__),
            median_di = median(divergent__))

summ12a = model12a %>% 
  spread_draws(c(treedepth__, energy__, divergent__)) %>% 
  summarise(mean_td = mean(treedepth__), 
            mean_en = mean(energy__),
            mean_di = mean(divergent__),
            median_td = median(treedepth__),
            median_en = median(energy__),
            median_di = median(divergent__))

summ13a = model13a %>% 
  spread_draws(c(treedepth__, energy__, divergent__)) %>% 
  summarise(mean_td = mean(treedepth__), 
            mean_en = mean(energy__),
            mean_di = mean(divergent__),
            median_td = median(treedepth__),
            median_en = median(energy__),
            median_di = median(divergent__))

summ14a = model14a %>% 
  spread_draws(c(treedepth__, energy__, divergent__)) %>% 
  summarise(mean_td = mean(treedepth__), 
            mean_en = mean(energy__),
            mean_di = mean(divergent__),
            median_td = median(treedepth__),
            median_en = median(energy__),
            median_di = median(divergent__))

summ15a = model15a %>% 
  spread_draws(c(treedepth__, energy__, divergent__)) %>% 
  summarise(mean_td = mean(treedepth__), 
            mean_en = mean(energy__),
            mean_di = mean(divergent__),
            median_td = median(treedepth__),
            median_en = median(energy__),
            median_di = median(divergent__))

summ_all = list(summ01, summ02, summ03, summ04, summ05,
                summ06, summ07, summ08, summ09, summ10,
                summ11, summ12, summ13, summ14, summ15,
                summ04a, summ07a, summ09a, summ10a, summ12a,
                summ13a, summ14a, summ15a)

names(summ_all) =  c(paste0('model0', 1:9), paste0('model', 10:15),  
                     paste0('model', c(4,7,9,10,12:15), 'a'))

## save prediction 
pred01 = model01 %>% 
  posterior_linpred(., transform = T) %>% 
  apply(., 2, quantile, c(0.025,0.975))

pred02 = model02 %>% 
  posterior_linpred(., transform = T) %>% 
  apply(., 2, quantile, c(0.025,0.975))

pred03 = model03 %>% 
  posterior_linpred(., transform = T) %>% 
  apply(., 2, quantile, c(0.025,0.975))

pred04 = model04 %>% 
  posterior_linpred(., transform = T) %>% 
  apply(., 2, quantile, c(0.025,0.975))

pred05 = model05 %>% 
  posterior_linpred(., transform = T) %>% 
  apply(., 2, quantile, c(0.025,0.975))

pred06 = model06 %>% 
  posterior_linpred(., transform = T)%>% 
  apply(., 2, quantile, c(0.025,0.975))

pred07 = model07 %>% 
  posterior_linpred(., transform = T) %>% 
  apply(., 2, quantile, c(0.025,0.975))

pred08 = model08 %>% 
  posterior_linpred(., transform = T) %>% 
  apply(., 2, quantile, c(0.025,0.975))

pred09 = model09 %>% 
  posterior_linpred(., transform = T) %>% 
  apply(., 2, quantile, c(0.025,0.975))

pred10 = model10 %>% 
  posterior_linpred(., transform = T) %>% 
  apply(., 2, quantile, c(0.025,0.975))

pred11 = model11 %>% 
  posterior_linpred(., transform = T) %>% 
  apply(., 2, quantile, c(0.025,0.975))

pred12 = model12 %>% 
  posterior_linpred(., transform = T) %>% 
  apply(., 2, quantile, c(0.025,0.975))

pred13 = model13 %>% 
 posterior_linpred(., transform = T) %>% 
  apply(., 2, quantile, c(0.025,0.975))

pred14 = model14 %>% 
  posterior_linpred(., transform = T) %>% 
  apply(., 2, quantile, c(0.025,0.975))

pred15 = model15 %>% 
  posterior_linpred(., transform = T) %>% 
  apply(., 2, quantile, c(0.025,0.975))



pred04a = model04a %>% 
  posterior_linpred(., transform = T) %>% 
  apply(., 2, quantile, c(0.025,0.975))

pred07a = model07a %>% 
  posterior_linpred(., transform = T) %>% 
  apply(., 2, quantile, c(0.025,0.975))

pred09a = model09a %>% 
  posterior_linpred(., transform = T) %>% 
  apply(., 2, quantile, c(0.025,0.975))

pred10a = model10a %>% 
  posterior_linpred(., transform = T) %>% 
  apply(., 2, quantile, c(0.025,0.975))

pred12a = model12a %>% 
  posterior_linpred(., transform = T) %>% 
  apply(., 2, quantile, c(0.025,0.975))

pred13a = model13a %>% 
  posterior_linpred(., transform = T) %>% 
  apply(., 2, quantile, c(0.025,0.975))

pred14a = model14a %>% 
  posterior_linpred(., transform = T) %>% 
  apply(., 2, quantile, c(0.025,0.975))

pred15a = model15a %>% 
  posterior_linpred(., transform = T) %>% 
  apply(., 2, quantile, c(0.025,0.975))

pred_all = list(pred01, pred02, pred03, pred04, pred05,
                pred06, pred07, pred08, pred09, pred10,
                pred11, pred12, pred13, pred14, pred15,
                pred04a, pred07a, pred09a, pred10a, pred12a,
                pred13a, pred14a, pred15a)


pred_all = list(pred01, pred02, pred03, pred04, pred05,
                pred06, pred07, pred08, pred09, pred10,
                pred11, pred12, pred13, pred14, pred15,
                pred04a, pred07a, pred09a, pred10a, pred12a,
                pred13a, pred14a, pred15a)


names(pred_all) =  c(paste0('model0', 1:9), paste0('model', 10:15),  
                     paste0('model', c(4,7,9,10,12:15), 'a'))



save(samp_data, samp_data2,
     prob_truth, coef_list, summ_all, pred_all,
     file = paste0("simulated_temp", ITE, ".RData"))



model01_predict = posterior_linpred(model01, newdata = popn_ps, transform = T) # getting model estimate for each cell
model01_popnest = apply(model01_predict, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

model02_predict = posterior_linpred(model02, newdata = popn_ps, transform = T) # getting estimate for each cell
model02_popnest = apply(model02_predict, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

model03_predict = posterior_linpred(model03, newdata = popn_ps, transform = T) # getting estimate for each cell
model03_popnest = apply(model03_predict, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

model04_predict = posterior_linpred(model04, newdata = popn_ps, transform = T) # getting estimate for each cell
model04_popnest = apply(model04_predict, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

model04a_predict = posterior_linpred(model04a, newdata = popn_ps, transform = T) # getting estimate for each cell
model04a_popnest = apply(model04a_predict, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.


model05_predict = posterior_linpred(model05, newdata = popn_ps, transform = T) # getting model estimate for each cell
model05_popnest = apply(model05_predict, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

model06_predict = posterior_linpred(model06, newdata = popn_ps, transform = T) # getting model estimate for each cell
model06_popnest = apply(model06_predict, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

model07_predict = posterior_linpred(model07, newdata = popn_ps, transform = T) # getting model estimate for each cell
model07_popnest = apply(model07_predict, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

model07a_predict = posterior_linpred(model07a, newdata = popn_ps, transform = T) # getting model estimate for each cell
model07a_popnest = apply(model07a_predict, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.


model08_predict = posterior_linpred(model08, newdata = popn_ps, transform = T) # getting model estimate for each cell
model08_popnest = apply(model08_predict, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

model09_predict = posterior_linpred(model09, newdata = popn_ps, transform = T) # getting model estimate for each cell
model09_popnest = apply(model09_predict, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

model09a_predict = posterior_linpred(model09a, newdata = popn_ps, transform = T) # getting model estimate for each cell
model09a_popnest = apply(model09a_predict, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.



model10_predict = posterior_linpred(model10, newdata = popn_ps, transform = T) # getting model estimate for each cell
model10_popnest = apply(model10_predict, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

model10a_predict = posterior_linpred(model10a, newdata = popn_ps, transform = T) # getting model estimate for each cell
model10a_popnest = apply(model10a_predict, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.



model11_predict = posterior_linpred(model11, newdata = popn_ps, transform = T) # getting model estimate for each cell
model11_popnest = apply(model11_predict, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

model12_predict = posterior_linpred(model12, newdata = popn_ps, transform = T) # getting model estimate for each cell
model12_popnest = apply(model12_predict, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

model12a_predict = posterior_linpred(model12a, newdata = popn_ps, transform = T) # getting model estimate for each cell
model12a_popnest = apply(model12a_predict, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.


model13_predict = posterior_linpred(model13, newdata = popn_ps, transform = T) # getting model estimate for each cell
model13_popnest = apply(model13_predict, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

model13a_predict = posterior_linpred(model13a, newdata = popn_ps, transform = T) # getting model estimate for each cell
model13a_popnest = apply(model13a_predict, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.


model14_predict = posterior_linpred(model14, newdata = popn_ps, transform = T) # getting model estimate for each cell
model14_popnest = apply(model14_predict, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

model14a_predict = posterior_linpred(model14a, newdata = popn_ps, transform = T) # getting model estimate for each cell
model14a_popnest = apply(model14a_predict, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

model15_predict = posterior_linpred(model15, newdata = popn_ps, transform = T) # getting estimate for each cell
model15_popnest = apply(model15_predict, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.

model15a_predict = posterior_linpred(model15a, newdata = popn_ps, transform = T) # getting estimate for each cell
model15a_popnest = apply(model15a_predict, 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # prob of outcome in the popn.


popnest_all = list(model01_popnest, 
                   model02_popnest, 
                   model03_popnest,
                   model04_popnest,
                   model05_popnest, 
                   model06_popnest, 
                   model07_popnest,
                   model08_popnest,
                   model09_popnest,
                   model10_popnest,
                   model11_popnest, 
                   model12_popnest, 
                   model13_popnest,
                   model14_popnest,
                   model15_popnest,
                   model04a_popnest,
                   model07a_popnest,
                   model09a_popnest,
                   model10a_popnest,
                   model12a_popnest,
                   model13a_popnest,
                   model14a_popnest,
                   model15a_popnest)


popnest_tab = lapply(popnest_all, function(x)quantile(x,c(0.05, 0.5, 0.95))) %>% 
  do.call(rbind,.) %>% 
  data.frame(.) %>% 
  rename(popnestX5 = X5., popnestX50 = X50., popnestX95 = X95.)


save(samp_data, samp_data2,
      prob_truth, coef_list, summ_all, pred_all,
      popnest_tab, file = paste0("simulated_temp", ITE, ".RData"))

## comparing loo for two models, with weights and without weights
# calculating loo
loo1 <- loo(model01)
loo2 <- loo(model02)
loo3 <- loo(model03)
loo4 <- loo(model04)
loo5 <- loo(model05)
loo6 <- loo(model06)
loo7 <- loo(model07)
loo8 <- loo(model08)
loo9 <- loo(model09)
loo10 <- loo(model10)
loo11 <- loo(model11)
loo12 <- loo(model12)
loo13 <- loo(model13)
loo14 <- loo(model14)
loo15 <- loo(model15)

loo4a <- loo(model04a)
loo7a <- loo(model07a)
loo9a <- loo(model09a)
loo10a <- loo(model10a)
loo12a <- loo(model12a)
loo13a <- loo(model13a)
loo14a <- loo(model14a)
loo15a <- loo(model15a)


loo_all = list(loo1, loo2, loo3, loo4, loo5, 
               loo6, loo7, loo8, loo9, loo10,
               loo11, loo12, loo13, loo14, loo15,
               loo4a, loo7a, loo9a, loo10a, loo12a, 
               loo13a, loo14a, loo15a)


## extracting loo estimates to rank them
loo_tab = lapply(loo_all,function(x)x$estimates[1,]) %>% 
  do.call(rbind,.) %>% 
  data.frame(.)
rownames(loo_tab) = c(paste0('model0', 1:9), paste0('model', 10:15),  paste0('model', c(4,7,8,10,12:15), 'a'))
colnames(loo_tab) = c('elpd_loo', 'SE')

# creating survey raked weights
svy_rake = svydesign(ids=~1, # cluster id, ~1 for no clusters
                     weights=~wts, # including raked weights in the survey design
                     data=samp_data)

loo_wtd_tab = lapply(loo_all, function(x)loo_wtd(x,svy_rake)) %>% 
  do.call(rbind,.) %>% 
  data.frame(.)
rownames(loo_wtd_tab) =  c(paste0('model0', 1:9), paste0('model', 10:15),  paste0('model', c(4,7,8,10,12:15), 'a'))

loo_rank = rank(-loo_tab[,1])
loo_wtd_rank = rank(-loo_wtd_tab[,1])

samp_data <- samp_data %>% 
  mutate(elpd_1 = loo1$pointwise[,1],
         elpd_2 = loo2$pointwise[,1],
         elpd_3 = loo3$pointwise[,1],
         elpd_4 = loo4$pointwise[,1],
         elpd_5 = loo5$pointwise[,1],
         elpd_6 = loo6$pointwise[,1],
         elpd_7 = loo7$pointwise[,1],
         elpd_8 = loo8$pointwise[,1],
         elpd_9 = loo9$pointwise[,1],
         elpd_10 = loo10$pointwise[,1],
         elpd_11 = loo11$pointwise[,1],
         elpd_12 = loo12$pointwise[,1],
         elpd_13 = loo13$pointwise[,1],
         elpd_14 = loo14$pointwise[,1],
         elpd_15 = loo15$pointwise[,1],
         elpd_04a = loo4a$pointwise[,1],
         elpd_07a = loo7a$pointwise[,1],
         elpd_09a = loo9a$pointwise[,1],
         elpd_10a = loo10a$pointwise[,1],
         elpd_12a = loo12a$pointwise[,1],
         elpd_13a = loo13a$pointwise[,1],
         elpd_14a = loo14a$pointwise[,1],
         elpd_15a = loo15a$pointwise[,1])


## MRP estimate of loo
elpd_model01 = brm(elpd_1 ~ (1|X1) + (1|X2) + (1|X3) + (1|X4), data = samp_data,
                  backend = "cmdstanr", 
                  control = list(adapt_delta = 0.99))

elpd_model01_predict = posterior_predict(elpd_model01, newdata = popn_ps) 
elpd_model01_popnest = apply(elpd_model01_predict, 1, function(x)sum(x*popn_ps$Nj)) ## ~~equivalent to weighted elpd

elpd_model02 = brm(elpd_2 ~ (1|X1) + (1|X2) + (1|X3) + (1|X4), data = samp_data,
                  backend = "cmdstanr", 
                  control = list(adapt_delta = 0.99))

elpd_model02_predict = posterior_predict(elpd_model02, newdata = popn_ps) 
elpd_model02_popnest = apply(elpd_model02_predict, 1, function(x)sum(x*popn_ps$Nj)) ## ~~equivalent to weighted elpd

elpd_model03 = brm(elpd_3 ~ (1|X1) + (1|X2) + (1|X3) + (1|X4), data = samp_data,
                  backend = "cmdstanr", 
                  control = list(adapt_delta = 0.99))

elpd_model03_predict = posterior_predict(elpd_model03, newdata = popn_ps) 
elpd_model03_popnest = apply(elpd_model03_predict, 1, function(x)sum(x*popn_ps$Nj)) ## ~~equivalent to weighted elpd

elpd_model04 = brm(elpd_4 ~ (1|X1) + (1|X2) + (1|X3) + (1|X4), data = samp_data,
                  backend = "cmdstanr", 
                  control = list(adapt_delta = 0.99))

elpd_model04_predict = posterior_predict(elpd_model04, newdata = popn_ps) 
elpd_model04_popnest = apply(elpd_model04_predict, 1, function(x)sum(x*popn_ps$Nj)) ## ~~equivalent to weighted elpd

elpd_model05 = brm(elpd_5 ~ (1|X1) + (1|X2) + (1|X3) + (1|X4), data = samp_data,
                   backend = "cmdstanr", 
                   control = list(adapt_delta = 0.99))
elpd_model05_predict = posterior_predict(elpd_model05, newdata = popn_ps) 
elpd_model05_popnest = apply(elpd_model05_predict, 1, function(x)sum(x*popn_ps$Nj)) ## ~~equivalent to weighted elpd

elpd_model06 = brm(elpd_6 ~ (1|X1) + (1|X2) + (1|X3) + (1|X4), data = samp_data,
                   backend = "cmdstanr", 
                   control = list(adapt_delta = 0.99))
elpd_model06_predict = posterior_predict(elpd_model06, newdata = popn_ps) 
elpd_model06_popnest = apply(elpd_model06_predict, 1, function(x)sum(x*popn_ps$Nj)) ## ~~equivalent to weighted elpd

elpd_model07 = brm(elpd_7 ~ (1|X1) + (1|X2) + (1|X3) + (1|X4), data = samp_data,
                   backend = "cmdstanr", 
                   control = list(adapt_delta = 0.99))
elpd_model07_predict = posterior_predict(elpd_model07, newdata = popn_ps) 
elpd_model07_popnest = apply(elpd_model07_predict, 1, function(x)sum(x*popn_ps$Nj)) ## ~~equivalent to weighted elpd

elpd_model08 = brm(elpd_8 ~ (1|X1) + (1|X2) + (1|X3) + (1|X4), data = samp_data,
                   backend = "cmdstanr", 
                   control = list(adapt_delta = 0.99))
elpd_model08_predict = posterior_predict(elpd_model08, newdata = popn_ps) 
elpd_model08_popnest = apply(elpd_model08_predict, 1, function(x)sum(x*popn_ps$Nj)) ## ~~equivalent to weighted elpd

elpd_model09 = brm(elpd_9 ~ (1|X1) + (1|X2) + (1|X3) + (1|X4), data = samp_data,
                   backend = "cmdstanr", 
                   control = list(adapt_delta = 0.99))
elpd_model09_predict = posterior_predict(elpd_model09, newdata = popn_ps)  # getting model estimate for each cell 
elpd_model09_popnest = apply(elpd_model09_predict, 1, function(x)sum(x*popn_ps$Nj)) ## ~~equivalent to weighted elpd

elpd_model10 = brm(elpd_10 ~ (1|X1) + (1|X2) + (1|X3) + (1|X4), data = samp_data,
                   backend = "cmdstanr", 
                   control = list(adapt_delta = 0.99))
elpd_model10_predict = posterior_predict(elpd_model10, newdata = popn_ps) 
elpd_model10_popnest = apply(elpd_model10_predict, 1, function(x)sum(x*popn_ps$Nj)) ## ~~equivalent to weighted elpd

elpd_model11 = brm(elpd_11 ~ (1|X1) + (1|X2) + (1|X3) + (1|X4), data = samp_data,
                   backend = "cmdstanr", 
                   control = list(adapt_delta = 0.99))

elpd_model11_predict = posterior_predict(elpd_model11, newdata = popn_ps) 
elpd_model11_popnest = apply(elpd_model11_predict, 1, function(x)sum(x*popn_ps$Nj)) ## ~~equivalent to weighted elpd

elpd_model12 = brm(elpd_12 ~ (1|X1) + (1|X2) + (1|X3) + (1|X4), data = samp_data,
                   backend = "cmdstanr", 
                   control = list(adapt_delta = 0.99))

elpd_model12_predict = posterior_predict(elpd_model12, newdata = popn_ps) 
elpd_model12_popnest = apply(elpd_model12_predict, 1, function(x)sum(x*popn_ps$Nj)) ## ~~equivalent to weighted elpd

elpd_model13 = brm(elpd_13 ~ (1|X1) + (1|X2) + (1|X3) + (1|X4), data = samp_data,
                   backend = "cmdstanr", 
                   control = list(adapt_delta = 0.99))

elpd_model13_predict = posterior_predict(elpd_model13, newdata = popn_ps) 
elpd_model13_popnest = apply(elpd_model13_predict, 1, function(x)sum(x*popn_ps$Nj)) ## ~~equivalent to weighted elpd

elpd_model14 = brm(elpd_14 ~ (1|X1) + (1|X2) + (1|X3) + (1|X4), data = samp_data,
                   backend = "cmdstanr", 
                   control = list(adapt_delta = 0.99))

elpd_model14_predict = posterior_predict(elpd_model14, newdata = popn_ps) 
elpd_model14_popnest = apply(elpd_model14_predict, 1, function(x)sum(x*popn_ps$Nj)) ## ~~equivalent to weighted elpd

elpd_model15 = brm(elpd_15 ~ (1|X1) + (1|X2) + (1|X3) + (1|X4), data = samp_data,
                   backend = "cmdstanr", 
                   control = list(adapt_delta = 0.99))

elpd_model15_predict = posterior_predict(elpd_model15, newdata = popn_ps) 
elpd_model15_popnest = apply(elpd_model15_predict, 1, function(x)sum(x*popn_ps$Nj)) ## ~~equivalent to weighted elpd

elpd_model04a = brm(elpd_04a ~ (1|X1) + (1|X2) + (1|X3) + (1|X4), data = samp_data,
                   backend = "cmdstanr", 
                   control = list(adapt_delta = 0.99))

elpd_model04a_predict = posterior_predict(elpd_model04a, newdata = popn_ps) 
elpd_model04a_popnest = apply(elpd_model04a_predict, 1, function(x)sum(x*popn_ps$Nj)) ## ~~equivalent to weighted elpd

elpd_model07a = brm(elpd_07a ~ (1|X1) + (1|X2) + (1|X3) + (1|X4), data = samp_data,
                   backend = "cmdstanr", 
                   control = list(adapt_delta = 0.99))

elpd_model07a_predict = posterior_predict(elpd_model07a, newdata = popn_ps) 
elpd_model07a_popnest = apply(elpd_model07a_predict, 1, function(x)sum(x*popn_ps$Nj)) ## ~~equivalent to weighted elpd

elpd_model09a = brm(elpd_09a ~ (1|X1) + (1|X2) + (1|X3) + (1|X4), data = samp_data,
                   backend = "cmdstanr", 
                   control = list(adapt_delta = 0.99))

elpd_model09a_predict = posterior_predict(elpd_model09a, newdata = popn_ps) 
elpd_model09a_popnest = apply(elpd_model09a_predict, 1, function(x)sum(x*popn_ps$Nj)) ## ~~equivalent to weighted elpd

elpd_model10a = brm(elpd_10a ~ (1|X1) + (1|X2) + (1|X3) + (1|X4), data = samp_data,
                   backend = "cmdstanr", 
                   control = list(adapt_delta = 0.99))

elpd_model10a_predict = posterior_predict(elpd_model10a, newdata = popn_ps) 
elpd_model10a_popnest = apply(elpd_model10a_predict, 1, function(x)sum(x*popn_ps$Nj)) ## ~~equivalent to weighted elpd

elpd_model12a = brm(elpd_12a ~ (1|X1) + (1|X2) + (1|X3) + (1|X4), data = samp_data,
                    backend = "cmdstanr", 
                    control = list(adapt_delta = 0.99))

elpd_model12a_predict = posterior_predict(elpd_model12a, newdata = popn_ps) 
elpd_model12a_popnest = apply(elpd_model12a_predict, 1, function(x)sum(x*popn_ps$Nj)) ## ~~equivalent to weighted elpd


elpd_model13a = brm(elpd_13a ~ (1|X1) + (1|X2) + (1|X3) + (1|X4), data = samp_data,
                   backend = "cmdstanr", 
                   control = list(adapt_delta = 0.99))

elpd_model13a_predict = posterior_predict(elpd_model13a, newdata = popn_ps) 
elpd_model13a_popnest = apply(elpd_model13a_predict, 1, function(x)sum(x*popn_ps$Nj)) ## ~~equivalent to weighted elpd

elpd_model14a = brm(elpd_14a ~ (1|X1) + (1|X2) + (1|X3) + (1|X4), data = samp_data,
                   backend = "cmdstanr", 
                   control = list(adapt_delta = 0.99))

elpd_model14a_predict = posterior_predict(elpd_model14a, newdata = popn_ps) 
elpd_model14a_popnest = apply(elpd_model14a_predict, 1, function(x)sum(x*popn_ps$Nj)) ## ~~equivalent to weighted elpd

elpd_model15a = brm(elpd_15a ~ (1|X1) + (1|X2) + (1|X3) + (1|X4), data = samp_data,
                   backend = "cmdstanr", 
                   control = list(adapt_delta = 0.99))

elpd_model15a_predict = posterior_predict(elpd_model15a, newdata = popn_ps) 
elpd_model15a_popnest = apply(elpd_model15a_predict, 1, function(x)sum(x*popn_ps$Nj)) ## ~~equivalent to weighted elpd



elpd_popnest_all = list(elpd_model01_popnest, 
                        elpd_model02_popnest, 
                        elpd_model03_popnest,
                        elpd_model04_popnest,
                        elpd_model05_popnest, 
                        elpd_model06_popnest, 
                        elpd_model07_popnest,
                        elpd_model08_popnest,
                        elpd_model09_popnest,
                        elpd_model10_popnest,
                        elpd_model11_popnest, 
                        elpd_model12_popnest, 
                        elpd_model13_popnest,
                        elpd_model14_popnest,
                        elpd_model15_popnest,
                        elpd_model04a_popnest,
                        elpd_model07a_popnest,
                        elpd_model09a_popnest,
                        elpd_model10a_popnest,
                        elpd_model12a_popnest,
                        elpd_model13a_popnest,
                        elpd_model14a_popnest,
                        elpd_model15a_popnest)

elpd_popnest_tab = lapply(elpd_popnest_all, function(x)quantile(x,c(0.05, 0.5, 0.95))) %>% 
  do.call(rbind,.) %>% 
  data.frame(.) %>% 
  rename(elpd.popnestX5 = X5., elpd.popnestX50 = X50., elpd.popnestX95 = X95.)

elpd_popnest_rank = rank(-elpd_popnest_tab[,2])

## saving the results
sim_list = cbind(loo_tab, loo_wtd_tab, loo_rank, loo_wtd_rank,
                 elpd_popnest_rank, popnest_tab, elpd_popnest_tab)

prob_truth = mean(popn_data$bin_value)

coef_list = c(coef(model01), coef(model02), coef(model03), coef(model04), 
              coef(model05), coef(model06), coef(model07), coef(model08),
              coef(model09), coef(model10), coef(model11), coef(model12),
              coef(model13), coef(model14), coef(model15), fixef(model04a),
    	      coef(model07a), coef(model09a), coef(model10a), coef(model12a),
	      coef(model13a), coef(model14a), coef(model15a)) 
names(coef_list) = c(paste0("01.",names(coef(model01))[grep("*", names(coef(model01)))]),
                     paste0("02.",names(coef(model02))[grep("*", names(coef(model02)))]),
                     paste0("03.",names(coef(model03))[grep("*", names(coef(model03)))]),
                     paste0("04.",names(coef(model04))[grep("*", names(coef(model04)))]),
                     paste0("05.",names(coef(model05))[grep("*", names(coef(model05)))]),
                     paste0("06.",names(coef(model06))[grep("*", names(coef(model06)))]),
                     paste0("07.",names(coef(model07))[grep("*", names(coef(model07)))]),
                     paste0("08.",names(coef(model08))[grep("*", names(coef(model08)))]),
                     paste0("09.",names(coef(model09))[grep("*", names(coef(model09)))]),
                     paste0("10.",names(coef(model10))[grep("*", names(coef(model10)))]),
                     paste0("11.",names(coef(model11))[grep("*", names(coef(model11)))]),
                     paste0("12.",names(coef(model12))[grep("*", names(coef(model12)))]),
                     paste0("13.",names(coef(model13))[grep("*", names(coef(model13)))]),
                     paste0("14.",names(coef(model14))[grep("*", names(coef(model14)))]),
                     paste0("15.",names(coef(model15))[grep("*", names(coef(model15)))]))


save(samp_data, samp_data2,
     prob_truth, coef_list, summ_all, pred_all, 
     sim_list, elpd_popnest_all, file = paste0("simulated_", ITE, ".RData"))
