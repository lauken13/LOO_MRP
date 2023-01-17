## generating a sample similar to simulation studies
library(brms) # inv_logit_scaled()
library(tidyverse)
library(glmnet)

nhdata = readRDS(file="nhanes/data/nhanes_final.rds")

str(nhdata)

N = nrow(nhdata)
n = 1500

## generate inclusion prob. for each individual
dput(round(runif(4, -3, 3), 1))

nhdata$incl_prob <- inv_logit_scaled(with(nhdata, c(0.3, -0.5, -1.5, .1, 0.2, 1.3)[age]) + 
  with(nhdata, c(-0.8, 0.3)[overweight]) + 
  with(nhdata, c(-1.8, 0.4, -0.6, 1.7, 0.8, 1.5)[ethnicity]) +
    with(nhdata, c(-.8, 1.4, 2.2, 1.9)[elst_status]) + 
    with(nhdata, c(-0.5, 1.3)[gender]) + 
    with(nhdata, c(.1, -1.5, 0.7, 1.8, 2.1)[pov_level]) +
    with(nhdata, c(0.9, -0.5, 1.4, 1.5)[sodium_lvl]) 
    )

hist(nhdata$incl_prob)

# generating samples ------------------------------------------------------
set.seed(428235)
samp_loc = sample(1:nrow(nhdata), size = n, 
                  replace=F, prob = nhdata$incl_prob)

samp_data = nhdata[samp_loc,] %>% 
  mutate(inclusion = 1)

# joining with full data
join1 = left_join(nhdata, samp_data[,c("SEQN", 'inclusion')], by="SEQN") %>% 
  mutate(incl_gen = case_when(inclusion == '1' ~ 1,
                              is.na(inclusion) ~ 0),
         high_bp = as.numeric(high_bp)-1) %>% 
  select(-inclusion)

str(join1)
