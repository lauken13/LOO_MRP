## calculating popn indv estimates


## combining popn estimates for plotting (from Lauren's email)
library(tidyverse)

## data file
load(here::here("code/simDesign1/data/temp/loo_N02_1000.RData"))

names(samp_data_list[[1]])

samp_wts = lapply(1:100, function(x)matrix(NA, nrow=1000,ncol=2))
for(iter in 1:100){
  samp_wts[[iter]][,1] = iter
  samp_wts[[iter]][,2] = samp_data_list[[iter]]$wts
  
  colnames(samp_wts[[iter]]) = c('iteration', 'wts')
}

saveRDS(samp_wts, file='code/simDesign1/data/temp/samp_wts.rds')