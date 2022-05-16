## postprocessing the saved objects from cluster
library(loo)
library(posterior) # to convert draws_array() objects 

# sourcing gen_dat()
source("00-gen_dat_func_base.R")

# sourcing loo_wtd()
source("../functions.R")

## empty matrices
# for mrp est
popnest_all_list =
  popnest_summary_list =  
  sampest_tab_list = 
  popnest_sae_X1_all =
  popnest_sae_X2_all =
  popnest_sae_X3_all =
  popnest_sae_X4_all =
  popnest_sae_X1_list =
  popnest_sae_X2_list = 
  popnest_sae_X3_list =
  popnest_sae_X4_list = 
pt_list = list() 

# elpd values and wtd loo
loo_01_list = loo_02_list = loo_03_list = loo_04_list = loo_05_list = 
  loo_06_list = loo_07_list = loo_08_list = loo_09_list = loo_10_list = 
  loo_11_list = loo_12_list = loo_13_list = loo_14_list = loo_15_list = 
  list()

loo_wtd_01_list = loo_wtd_02_list = loo_wtd_03_list = loo_wtd_04_list = loo_wtd_05_list =
loo_wtd_06_list = loo_wtd_07_list = loo_wtd_08_list = loo_wtd_09_list = loo_wtd_10_list =
  loo_wtd_11_list = loo_wtd_12_list = loo_wtd_13_list = loo_wtd_14_list = loo_wtd_15_list = 
    list()

## elpd values and std. error
elpd_01_mat = elpd_02_mat = elpd_03_mat = elpd_04_mat = elpd_05_mat =
  elpd_06_mat = elpd_07_mat = elpd_08_mat = elpd_09_mat = elpd_10_mat =
  elpd_11_mat = elpd_12_mat = elpd_13_mat = elpd_14_mat = elpd_15_mat =
  matrix(NA, ncol=2, nrow=100) # elpd and se, 100 iterations


pt_samp_list = 
  pt_popn_list = 
  samp_data_list =
  popn_data_list =
  popn_ps_list = list()

model01_popnest_sae_X4 = model02_popnest_sae_X4 = model03_popnest_sae_X4 =
  model04_popnest_sae_X4 = model05_popnest_sae_X4 = model06_popnest_sae_X4 =
  model07_popnest_sae_X4 = model08_popnest_sae_X4 = model09_popnest_sae_X4 =
  model10_popnest_sae_X4 = model11_popnest_sae_X4 = model12_popnest_sae_X4 =
  model13_popnest_sae_X4 = model14_popnest_sae_X4 = model15_popnest_sae_X4 = 
  lapply(1:100,matrix, data=NA,nrow=4000, ncol=5)

model01_popnest_sae_X3 = model02_popnest_sae_X3 = model03_popnest_sae_X3 =
  model04_popnest_sae_X3 = model05_popnest_sae_X3 = model06_popnest_sae_X3 =
  model07_popnest_sae_X3 = model08_popnest_sae_X3 = model09_popnest_sae_X3 =
  model10_popnest_sae_X3 = model11_popnest_sae_X3 = model12_popnest_sae_X3 =
  model13_popnest_sae_X3 = model14_popnest_sae_X3 = model15_popnest_sae_X3 = 
  lapply(1:100,matrix, data=NA,nrow=4000, ncol=5)

model01_popnest_sae_X2 = model02_popnest_sae_X2 = model03_popnest_sae_X2 =
  model04_popnest_sae_X2 = model05_popnest_sae_X2 = model06_popnest_sae_X2 =
  model07_popnest_sae_X2 = model08_popnest_sae_X2 = model09_popnest_sae_X2 =
  model10_popnest_sae_X2 = model11_popnest_sae_X2 = model12_popnest_sae_X2 =
  model13_popnest_sae_X2 = model14_popnest_sae_X2 = model15_popnest_sae_X2 = 
  lapply(1:100,matrix, data=NA,nrow=4000, ncol=5)

model01_popnest_sae_X1 = model02_popnest_sae_X1 = model03_popnest_sae_X1 =
  model04_popnest_sae_X1 = model05_popnest_sae_X1 = model06_popnest_sae_X1 =
  model07_popnest_sae_X1 = model08_popnest_sae_X1 = model09_popnest_sae_X1 =
  model10_popnest_sae_X1 = model11_popnest_sae_X1 = model12_popnest_sae_X1 =
  model13_popnest_sae_X1 = model14_popnest_sae_X1 = model15_popnest_sae_X1 = 
  lapply(1:100,matrix, data=NA,nrow=4000, ncol=5)


iter = 1:100
for(ite in iter){
  print(ite)
  load(paste0('LOO_arPrior_',ite,".RData")) 
  
  # mrp est -----------------------------------------------------------------
  # generating data using gen_dat()
  set.seed(65438)
  sim1 = gen_dat(N = 10000, samp_size = 500, ITE=ite)
  
  samp_data = samp_data_list[[ite]] = sim1$samp_data
  popn_data = popn_data_list[[ite]] = sim1$popn_data
  popn_ps = popn_ps_list[[ite]] = sim1$popn_ps
  
  pt_samp_list[[ite]] = mean(samp_data$y_obs)
  pt_popn_list[[ite]] = mean(popn_data$y_obs)
  
  ## individual estimates
  sampest_tab_01 = apply(sampest_01,2,quantile,c(0.05,0.5,0.95)) %>% 
    t() %>% 
    as_tibble() %>% 
    rename(sampestX5 = `5%`, sampestX50 = `50%`, sampestX95 = `95%`) %>% 
    mutate(model = 'model01',
           prob_truth = samp_data$y_obs,
           range_int = sampestX95 - sampestX5,
           coverage = ifelse(prob_truth >= sampestX5 & prob_truth <= sampestX95, 1, 0))
  
  sampest_tab_02 = apply(sampest_02,2,quantile,c(0.05,0.5,0.95)) %>% 
    t() %>% 
    as_tibble() %>% 
    rename(sampestX5 = `5%`, sampestX50 = `50%`, sampestX95 = `95%`) %>% 
    mutate(model = 'model02',
           prob_truth = samp_data$y_obs,
           range_int = sampestX95 - sampestX5,
           coverage = ifelse(prob_truth >= sampestX5 & prob_truth <= sampestX95, 1, 0))
  
  sampest_tab_03 = apply(sampest_03,2,quantile,c(0.05,0.5,0.95)) %>% 
    t() %>% 
    as_tibble() %>% 
    rename(sampestX5 = `5%`, sampestX50 = `50%`, sampestX95 = `95%`) %>% 
    mutate(model = 'model03',
           prob_truth = samp_data$y_obs,
           range_int = sampestX95 - sampestX5,
           coverage = ifelse(prob_truth >= sampestX5 & prob_truth <= sampestX95, 1, 0))
  
  sampest_tab_04 = apply(sampest_04,2,quantile,c(0.05,0.5,0.95)) %>% 
    t() %>% 
    as_tibble() %>% 
    rename(sampestX5 = `5%`, sampestX50 = `50%`, sampestX95 = `95%`) %>% 
    mutate(model = 'model04',
           prob_truth = samp_data$y_obs,
           range_int = sampestX95 - sampestX5,
           coverage = ifelse(prob_truth >= sampestX5 & prob_truth <= sampestX95, 1, 0))
  
  sampest_tab_05 = apply(sampest_05,2,quantile,c(0.05,0.5,0.95)) %>% 
    t() %>% 
    as_tibble() %>% 
    rename(sampestX5 = `5%`, sampestX50 = `50%`, sampestX95 = `95%`) %>% 
    mutate(model = 'model05',
           prob_truth = samp_data$y_obs,
           range_int = sampestX95 - sampestX5,
           coverage = ifelse(prob_truth >= sampestX5 & prob_truth <= sampestX95, 1, 0))
  
  sampest_tab_06 = apply(sampest_06,2,quantile,c(0.05,0.5,0.95)) %>% 
    t() %>% 
    as_tibble() %>% 
    rename(sampestX5 = `5%`, sampestX50 = `50%`, sampestX95 = `95%`) %>% 
    mutate(model = 'model06',
           prob_truth = samp_data$y_obs,
           range_int = sampestX95 - sampestX5,
           coverage = ifelse(prob_truth >= sampestX5 & prob_truth <= sampestX95, 1, 0))
  
  sampest_tab_07 = apply(sampest_07,2,quantile,c(0.05,0.5,0.95)) %>% 
    t() %>% 
    as_tibble() %>% 
    rename(sampestX5 = `5%`, sampestX50 = `50%`, sampestX95 = `95%`) %>% 
    mutate(model = 'model07',
           prob_truth = samp_data$y_obs,
           range_int = sampestX95 - sampestX5,
           coverage = ifelse(prob_truth >= sampestX5 & prob_truth <= sampestX95, 1, 0))
  
  sampest_tab_08 = apply(sampest_08,2,quantile,c(0.05,0.5,0.95)) %>% 
    t() %>% 
    as_tibble() %>% 
    rename(sampestX5 = `5%`, sampestX50 = `50%`, sampestX95 = `95%`) %>% 
    mutate(model = 'model08',
           prob_truth = samp_data$y_obs,
           range_int = sampestX95 - sampestX5,
           coverage = ifelse(prob_truth >= sampestX5 & prob_truth <= sampestX95, 1, 0))
  
  sampest_tab_09 = apply(sampest_09,2,quantile,c(0.05,0.5,0.95)) %>% 
    t() %>% 
    as_tibble() %>% 
    rename(sampestX5 = `5%`, sampestX50 = `50%`, sampestX95 = `95%`) %>% 
    mutate(model = 'model09',
           prob_truth = samp_data$y_obs,
           range_int = sampestX95 - sampestX5,
           coverage = ifelse(prob_truth >= sampestX5 & prob_truth <= sampestX95, 1, 0))
  
  sampest_tab_10 = apply(sampest_10,2,quantile,c(0.05,0.5,0.95)) %>% 
    t() %>% 
    as_tibble() %>% 
    rename(sampestX5 = `5%`, sampestX50 = `50%`, sampestX95 = `95%`) %>% 
    mutate(model = 'model10',
           prob_truth = samp_data$y_obs,
           range_int = sampestX95 - sampestX5,
           coverage = ifelse(prob_truth >= sampestX5 & prob_truth <= sampestX95, 1, 0))
  
  sampest_tab_11 = apply(sampest_11,2,quantile,c(0.05,0.5,0.95)) %>% 
    t() %>% 
    as_tibble() %>% 
    rename(sampestX5 = `5%`, sampestX50 = `50%`, sampestX95 = `95%`) %>% 
    mutate(model = 'model11',
           prob_truth = samp_data$y_obs,
           range_int = sampestX95 - sampestX5,
           coverage = ifelse(prob_truth >= sampestX5 & prob_truth <= sampestX95, 1, 0))
  
  sampest_tab_12 = apply(sampest_12,2,quantile,c(0.05,0.5,0.95)) %>% 
    t() %>% 
    as_tibble() %>% 
    rename(sampestX5 = `5%`, sampestX50 = `50%`, sampestX95 = `95%`) %>% 
    mutate(model = 'model12',
           prob_truth = samp_data$y_obs,
           range_int = sampestX95 - sampestX5,
           coverage = ifelse(prob_truth >= sampestX5 & prob_truth <= sampestX95, 1, 0))
  
  sampest_tab_13 = apply(sampest_13,2,quantile,c(0.05,0.5,0.95)) %>% 
    t() %>% 
    as_tibble() %>% 
    rename(sampestX5 = `5%`, sampestX50 = `50%`, sampestX95 = `95%`) %>% 
    mutate(model = 'model13',
           prob_truth = samp_data$y_obs,
           range_int = sampestX95 - sampestX5,
           coverage = ifelse(prob_truth >= sampestX5 & prob_truth <= sampestX95, 1, 0))
  
  sampest_tab_14 = apply(sampest_14,2,quantile,c(0.05,0.5,0.95)) %>% 
    t() %>% 
    as_tibble() %>% 
    rename(sampestX5 = `5%`, sampestX50 = `50%`, sampestX95 = `95%`) %>% 
    mutate(model = 'model14',
           prob_truth = samp_data$y_obs,
           range_int = sampestX95 - sampestX5,
           coverage = ifelse(prob_truth >= sampestX5 & prob_truth <= sampestX95, 1, 0))
  
  sampest_tab_15 = apply(sampest_15,2,quantile,c(0.05,0.5,0.95)) %>% 
    t() %>% 
    as_tibble() %>% 
    rename(sampestX5 = `5%`, sampestX50 = `50%`, sampestX95 = `95%`) %>% 
    mutate(model = 'model15',
           prob_truth = samp_data$y_obs,
           range_int = sampestX95 - sampestX5,
           coverage = ifelse(prob_truth >= sampestX5 & prob_truth <= sampestX95, 1, 0))
  
  
  ## getting posterior of individuals estimate for each of the models
  sampest_tab_list[[ite]] = list(sampest_tab_01, sampest_tab_02, sampest_tab_03, sampest_tab_04, sampest_tab_05,
                                sampest_tab_06, sampest_tab_07, sampest_tab_08, sampest_tab_09, sampest_tab_10,
                                sampest_tab_11, sampest_tab_12, sampest_tab_13, sampest_tab_14, sampest_tab_15) %>% 
    do.call(rbind,.)
  

# calculating popnest -----------------------------------------------------
  model01_popnest = apply(as_draws_matrix(popnest_01), 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj))
  model02_popnest = apply(as_draws_matrix(popnest_02), 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj))
  model03_popnest = apply(as_draws_matrix(popnest_03), 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj))
  model04_popnest = apply(as_draws_matrix(popnest_04), 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj))
  model05_popnest = apply(as_draws_matrix(popnest_05), 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj))
  model06_popnest = apply(as_draws_matrix(popnest_06), 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj)) # applying to each iteration
  model07_popnest = apply(as_draws_matrix(popnest_07), 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj))
  model08_popnest = apply(as_draws_matrix(popnest_08), 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj))
  model09_popnest = apply(as_draws_matrix(popnest_09), 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj))
  model10_popnest = apply(as_draws_matrix(popnest_10), 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj))
  model11_popnest = apply(as_draws_matrix(popnest_11), 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj))
  model12_popnest = apply(as_draws_matrix(popnest_12), 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj))
  model13_popnest = apply(as_draws_matrix(popnest_13), 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj))
  model14_popnest = apply(as_draws_matrix(popnest_14), 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj))
  model15_popnest = apply(as_draws_matrix(popnest_15), 1, function(x)sum(x*popn_ps$Nj)/sum(popn_ps$Nj))
  
  popnest_all_list[[ite]] = list(model01_popnest, model02_popnest, model03_popnest, model04_popnest, model05_popnest,
                                 model06_popnest, model07_popnest, model08_popnest, model09_popnest, model10_popnest,
                                 model11_popnest, model12_popnest, model13_popnest, model14_popnest, model15_popnest)
  
  popnest_summary_list[[ite]] = lapply(popnest_all_list[[ite]], function(x)quantile(x,c(0.05, 0.5, 0.95))) %>% 
    do.call(rbind,.) %>% 
    data.frame(.) %>% 
    rename(popnestX5 = X5., popnestX50 = X50., popnestX95 = X95.) %>% 
    mutate(model = c(paste0('model0', 1:9),paste0('model', 10:15)), iter = ite)
  
  # prob of outcome for each iteration
  pt_list[ite] = mean(popn_data$y_obs) # or mean(popn_data$y_obs) 
  

# small area estimation for X1 --------------------------------------------
  for (s1 in 1:5){
    
    lvl_loc = which(popn_ps$X1 == s1)
    
    # calculating group popnest for X4-levels
    model01_popnest_sae_X1[[ite]][,s1] = apply(as_draws_matrix(popnest_01[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model02_popnest_sae_X1[[ite]][,s1] = apply(as_draws_matrix(popnest_02[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model03_popnest_sae_X1[[ite]][,s1] = apply(as_draws_matrix(popnest_03[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model04_popnest_sae_X1[[ite]][,s1] = apply(as_draws_matrix(popnest_04[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model05_popnest_sae_X1[[ite]][,s1] = apply(as_draws_matrix(popnest_05[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    
    model06_popnest_sae_X1[[ite]][,s1] = apply(as_draws_matrix(popnest_06[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc])) # applying to each iteration
    model07_popnest_sae_X1[[ite]][,s1] = apply(as_draws_matrix(popnest_07[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model08_popnest_sae_X1[[ite]][,s1] = apply(as_draws_matrix(popnest_08[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model09_popnest_sae_X1[[ite]][,s1] = apply(as_draws_matrix(popnest_09[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model10_popnest_sae_X1[[ite]][,s1] = apply(as_draws_matrix(popnest_10[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    
    model11_popnest_sae_X1[[ite]][,s1] = apply(as_draws_matrix(popnest_11[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model12_popnest_sae_X1[[ite]][,s1] = apply(as_draws_matrix(popnest_12[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model13_popnest_sae_X1[[ite]][,s1] = apply(as_draws_matrix(popnest_13[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model14_popnest_sae_X1[[ite]][,s1] = apply(as_draws_matrix(popnest_14[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model15_popnest_sae_X1[[ite]][,s1] = apply(as_draws_matrix(popnest_15[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    
  }
  
  popnest_sae_X1_all[[ite]] = list( model01_popnest_sae_X1[[ite]],
                                    model02_popnest_sae_X1[[ite]],
                                    model03_popnest_sae_X1[[ite]],
                                    model04_popnest_sae_X1[[ite]],
                                    model05_popnest_sae_X1[[ite]],
                                    model06_popnest_sae_X1[[ite]],
                                    model07_popnest_sae_X1[[ite]],
                                    model08_popnest_sae_X1[[ite]],
                                    model09_popnest_sae_X1[[ite]],
                                    model10_popnest_sae_X1[[ite]],
                                    model11_popnest_sae_X1[[ite]],
                                    model12_popnest_sae_X1[[ite]],
                                    model13_popnest_sae_X1[[ite]],
                                    model14_popnest_sae_X1[[ite]],
                                    model15_popnest_sae_X1[[ite]])
  
  popnest_sae_X1_list[[ite]] = lapply(popnest_sae_X1_all[[ite]], function(x)quantile(x,c(0.05, 0.5, 0.95))) %>%
    do.call(rbind,.) %>%
    data.frame(.) %>%
    rename(popnestX5 = X5., popnestX50 = X50., popnestX95 = X95.) %>%
    mutate(model = c(paste0('model0',1:9), paste0('model',10:15)),
           iter = ite)
  
  
  ## small area estimation for X2
  for (s2 in 1:5){
    
    lvl_loc = which(popn_ps$X2 == s2)
    
    # calculating group popnest for X4-levels
    model01_popnest_sae_X2[[ite]][,s2] = apply(as_draws_matrix(popnest_01[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model02_popnest_sae_X2[[ite]][,s2] = apply(as_draws_matrix(popnest_02[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model03_popnest_sae_X2[[ite]][,s2] = apply(as_draws_matrix(popnest_03[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model04_popnest_sae_X2[[ite]][,s2] = apply(as_draws_matrix(popnest_04[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model05_popnest_sae_X2[[ite]][,s2] = apply(as_draws_matrix(popnest_05[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    
    model06_popnest_sae_X2[[ite]][,s2] = apply(as_draws_matrix(popnest_06[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc])) # applying to each iteration
    model07_popnest_sae_X2[[ite]][,s2] = apply(as_draws_matrix(popnest_07[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model08_popnest_sae_X2[[ite]][,s2] = apply(as_draws_matrix(popnest_08[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model09_popnest_sae_X2[[ite]][,s2] = apply(as_draws_matrix(popnest_09[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model10_popnest_sae_X2[[ite]][,s2] = apply(as_draws_matrix(popnest_10[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))

    model11_popnest_sae_X2[[ite]][,s2] = apply(as_draws_matrix(popnest_11[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model12_popnest_sae_X2[[ite]][,s2] = apply(as_draws_matrix(popnest_12[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model13_popnest_sae_X2[[ite]][,s2] = apply(as_draws_matrix(popnest_13[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model14_popnest_sae_X2[[ite]][,s2] = apply(as_draws_matrix(popnest_14[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model15_popnest_sae_X2[[ite]][,s2] = apply(as_draws_matrix(popnest_15[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    
  }
  
  popnest_sae_X2_all[[ite]] = list( model01_popnest_sae_X2[[ite]],
                                    model02_popnest_sae_X2[[ite]],
                                    model03_popnest_sae_X2[[ite]],
                                    model04_popnest_sae_X2[[ite]],
                                    model05_popnest_sae_X2[[ite]],
                                    model06_popnest_sae_X2[[ite]],
                                    model07_popnest_sae_X2[[ite]],
                                    model08_popnest_sae_X2[[ite]],
                                    model09_popnest_sae_X2[[ite]],
                                    model10_popnest_sae_X2[[ite]],
                                    model11_popnest_sae_X2[[ite]],
                                    model12_popnest_sae_X2[[ite]],
                                    model13_popnest_sae_X2[[ite]],
                                    model14_popnest_sae_X2[[ite]],
                                    model15_popnest_sae_X2[[ite]])
  
  popnest_sae_X2_list[[ite]] = lapply(popnest_sae_X2_all[[ite]], function(x)quantile(x,c(0.05, 0.5, 0.95))) %>%
    do.call(rbind,.) %>%
    data.frame(.) %>%
    rename(popnestX5 = X5., popnestX50 = X50., popnestX95 = X95.) %>%
    mutate(model = c(paste0('model0',1:9), paste0('model',10:15)),
           iter = ite)
  
  # small area estimation for X3 --------------------------------------------
  for (s3 in 1:5){
    
    lvl_loc = which(popn_ps$X3 == s3)
    
    # calculating group popnest for X4-levels
    model01_popnest_sae_X3[[ite]][,s3] = apply(as_draws_matrix(popnest_01[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model02_popnest_sae_X3[[ite]][,s3] = apply(as_draws_matrix(popnest_02[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model03_popnest_sae_X3[[ite]][,s3] = apply(as_draws_matrix(popnest_03[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model04_popnest_sae_X3[[ite]][,s3] = apply(as_draws_matrix(popnest_04[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model05_popnest_sae_X3[[ite]][,s3] = apply(as_draws_matrix(popnest_05[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    
    model06_popnest_sae_X3[[ite]][,s3] = apply(as_draws_matrix(popnest_06[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc])) # applying to each iteration
    model07_popnest_sae_X3[[ite]][,s3] = apply(as_draws_matrix(popnest_07[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model08_popnest_sae_X3[[ite]][,s3] = apply(as_draws_matrix(popnest_08[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model09_popnest_sae_X3[[ite]][,s3] = apply(as_draws_matrix(popnest_09[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model10_popnest_sae_X3[[ite]][,s3] = apply(as_draws_matrix(popnest_10[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    
    model11_popnest_sae_X3[[ite]][,s3] = apply(as_draws_matrix(popnest_11[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model12_popnest_sae_X3[[ite]][,s3] = apply(as_draws_matrix(popnest_12[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model13_popnest_sae_X3[[ite]][,s3] = apply(as_draws_matrix(popnest_13[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model14_popnest_sae_X3[[ite]][,s3] = apply(as_draws_matrix(popnest_14[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model15_popnest_sae_X3[[ite]][,s3] = apply(as_draws_matrix(popnest_15[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    
  }
  
  popnest_sae_X3_all[[ite]] = list( model01_popnest_sae_X3[[ite]],
                                    model02_popnest_sae_X3[[ite]],
                                    model03_popnest_sae_X3[[ite]],
                                    model04_popnest_sae_X3[[ite]],
                                    model05_popnest_sae_X3[[ite]],
                                    model06_popnest_sae_X3[[ite]],
                                    model07_popnest_sae_X3[[ite]],
                                    model08_popnest_sae_X3[[ite]],
                                    model09_popnest_sae_X3[[ite]],
                                    model10_popnest_sae_X3[[ite]],
                                    model11_popnest_sae_X3[[ite]],
                                    model12_popnest_sae_X3[[ite]],
                                    model13_popnest_sae_X3[[ite]],
                                    model14_popnest_sae_X3[[ite]],
                                    model15_popnest_sae_X3[[ite]])
  
  popnest_sae_X3_list[[ite]] = lapply(popnest_sae_X3_all[[ite]], function(x)quantile(x,c(0.05, 0.5, 0.95))) %>%
    do.call(rbind,.) %>%
    data.frame(.) %>%
    rename(popnestX5 = X5., popnestX50 = X50., popnestX95 = X95.) %>%
    mutate(model = c(paste0('model0',1:9), paste0('model',10:15)),
           iter = ite)
  

# small area estimation for X4 --------------------------------------------
  for (s4 in 1:5){
    
    lvl_loc = which(popn_ps$X4 == s4)
    
    # calculating group popnest for X4-levels
    model01_popnest_sae_X4[[ite]][,s4] = apply(as_draws_matrix(popnest_01[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model02_popnest_sae_X4[[ite]][,s4] = apply(as_draws_matrix(popnest_02[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model03_popnest_sae_X4[[ite]][,s4] = apply(as_draws_matrix(popnest_03[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model04_popnest_sae_X4[[ite]][,s4] = apply(as_draws_matrix(popnest_04[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model05_popnest_sae_X4[[ite]][,s4] = apply(as_draws_matrix(popnest_05[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    
    model06_popnest_sae_X4[[ite]][,s4] = apply(as_draws_matrix(popnest_06[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc])) # applying to each iteration
    model07_popnest_sae_X4[[ite]][,s4] = apply(as_draws_matrix(popnest_07[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model08_popnest_sae_X4[[ite]][,s4] = apply(as_draws_matrix(popnest_08[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model09_popnest_sae_X4[[ite]][,s4] = apply(as_draws_matrix(popnest_09[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model10_popnest_sae_X4[[ite]][,s4] = apply(as_draws_matrix(popnest_10[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    
    model11_popnest_sae_X4[[ite]][,s4] = apply(as_draws_matrix(popnest_11[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model12_popnest_sae_X4[[ite]][,s4] = apply(as_draws_matrix(popnest_12[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model13_popnest_sae_X4[[ite]][,s4] = apply(as_draws_matrix(popnest_13[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model14_popnest_sae_X4[[ite]][,s4] = apply(as_draws_matrix(popnest_14[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
    model15_popnest_sae_X4[[ite]][,s4] = apply(as_draws_matrix(popnest_15[,lvl_loc]), 1, function(x)sum(x*popn_ps$Nj[lvl_loc])/sum(popn_ps$Nj[lvl_loc]))
  }
  
  popnest_sae_X4_all[[ite]] = list( model01_popnest_sae_X4[[ite]],
                                    model02_popnest_sae_X4[[ite]],
                                    model03_popnest_sae_X4[[ite]],
                                    model04_popnest_sae_X4[[ite]],
                                    model05_popnest_sae_X4[[ite]],
                                    model06_popnest_sae_X4[[ite]],
                                    model07_popnest_sae_X4[[ite]],
                                    model08_popnest_sae_X4[[ite]],
                                    model09_popnest_sae_X4[[ite]],
                                    model10_popnest_sae_X4[[ite]],
                                    model11_popnest_sae_X4[[ite]],
                                    model12_popnest_sae_X4[[ite]],
                                    model13_popnest_sae_X4[[ite]],
                                    model14_popnest_sae_X4[[ite]],
                                    model15_popnest_sae_X4[[ite]])
  
  popnest_sae_X4_list[[ite]] = lapply(popnest_sae_X4_all[[ite]], function(x)quantile(x,c(0.05, 0.5, 0.95))) %>%
    do.call(rbind,.) %>%
    data.frame(.) %>%
    rename(popnestX5 = X5., popnestX50 = X50., popnestX95 = X95.) %>%
    mutate(model = c(paste0('model0',1:9), paste0('model',10:15)),
           iter = ite)
  
  # weighted loo ------------------------------------------------------------
  # calculating population totals for each level
  X1_margin = xtabs(~X1, data=popn_data)
  X2_margin = xtabs(~X2, data=popn_data)
  X3_margin = xtabs(~X3, data=popn_data)
  X4_margin = xtabs(~X4, data=popn_data)
  
  ## creating survey design
  svy1 = svydesign(ids=~1, # cluster id, ~1 for no clusters
                   weights=~rep(1,nrow(samp_data)), # equal weights for each unit
                   data=samp_data)
  
  # raked to the population
  rake1 = rake(design = svy1, sample.margins = list(~X1,~X2,~X3,~X4), 
               population.margins = list(X1_margin, X2_margin, X3_margin, X4_margin))
  
  # raked weights 
  samp_data$wts = weights(rake1)
  
  # creating survey raked weights
  svy_rake = svydesign(ids=~1, # cluster id, ~1 for no clusters
                       weights=~wts, # including raked weights in the survey design
                       data=samp_data) 
  
  ## saving loo object ####
  loo_01_list[[ite]] = loo_01; loo_02_list[[ite]] = loo_02; loo_03_list[[ite]] = loo_03
  loo_04_list[[ite]] = loo_04; loo_05_list[[ite]] = loo_05; loo_06_list[[ite]] = loo_06
  loo_07_list[[ite]] = loo_07; loo_08_list[[ite]] = loo_08; loo_09_list[[ite]] = loo_09
  loo_10_list[[ite]] = loo_10; loo_11_list[[ite]] = loo_11; loo_12_list[[ite]] = loo_12
  loo_13_list[[ite]] = loo_13; loo_14_list[[ite]] = loo_14; loo_15_list[[ite]] = loo_15
  
  ## elpd_loo ####
  ## elpd values 
  elpd_01_mat[ite,1] = loo_01_list[[ite]]$estimates[1,1]
  elpd_01_mat[ite,2] = loo_01_list[[ite]]$estimates[1,2]
  
  elpd_02_mat[ite,1] = loo_02_list[[ite]]$estimates[1,1]
  elpd_02_mat[ite,2] = loo_02_list[[ite]]$estimates[1,2]
  
  elpd_03_mat[ite,1] = loo_03_list[[ite]]$estimates[1,1]
  elpd_03_mat[ite,2] = loo_03_list[[ite]]$estimates[1,2]
  
  elpd_04_mat[ite,1] = loo_04_list[[ite]]$estimates[1,1]
  elpd_04_mat[ite,2] = loo_04_list[[ite]]$estimates[1,2]
  
  elpd_05_mat[ite,1] = loo_05_list[[ite]]$estimates[1,1]
  elpd_05_mat[ite,2] = loo_05_list[[ite]]$estimates[1,2]
  
  elpd_06_mat[ite,1] = loo_06_list[[ite]]$estimates[1,1]
  elpd_06_mat[ite,2] = loo_06_list[[ite]]$estimates[1,2]
  
  elpd_07_mat[ite,1] = loo_07_list[[ite]]$estimates[1,1]
  elpd_07_mat[ite,2] = loo_07_list[[ite]]$estimates[1,2]
  
  elpd_08_mat[ite,1] = loo_08_list[[ite]]$estimates[1,1]
  elpd_08_mat[ite,2] = loo_08_list[[ite]]$estimates[1,2]
  
  elpd_09_mat[ite,1] = loo_09_list[[ite]]$estimates[1,1]
  elpd_09_mat[ite,2] = loo_09_list[[ite]]$estimates[1,2]
  
  elpd_10_mat[ite,1] = loo_10_list[[ite]]$estimates[1,1]
  elpd_10_mat[ite,2] = loo_10_list[[ite]]$estimates[1,2]
  
  elpd_11_mat[ite,1] = loo_11_list[[ite]]$estimates[1,1]
  elpd_11_mat[ite,2] = loo_11_list[[ite]]$estimates[1,2]
  
  elpd_12_mat[ite,1] = loo_12_list[[ite]]$estimates[1,1]
  elpd_12_mat[ite,2] = loo_12_list[[ite]]$estimates[1,2]
  
  elpd_13_mat[ite,1] = loo_13_list[[ite]]$estimates[1,1]
  elpd_13_mat[ite,2] = loo_13_list[[ite]]$estimates[1,2]
  
  elpd_14_mat[ite,1] = loo_14_list[[ite]]$estimates[1,1]
  elpd_14_mat[ite,2] = loo_14_list[[ite]]$estimates[1,2]
  
  elpd_15_mat[ite,1] = loo_15_list[[ite]]$estimates[1,1]
  elpd_15_mat[ite,2] = loo_15_list[[ite]]$estimates[1,2]
  
  ## wtd_loo ####
  loo_wtd_01_list[[ite]] = loo_wtd(loo_01_list[[ite]], svy_rake)
  loo_wtd_02_list[[ite]] = loo_wtd(loo_02_list[[ite]], svy_rake)
  loo_wtd_03_list[[ite]] = loo_wtd(loo_03_list[[ite]], svy_rake)
  loo_wtd_04_list[[ite]] = loo_wtd(loo_04_list[[ite]], svy_rake)
  loo_wtd_05_list[[ite]] = loo_wtd(loo_05_list[[ite]], svy_rake)
  
  loo_wtd_06_list[[ite]] = loo_wtd(loo_06_list[[ite]], svy_rake)
  loo_wtd_07_list[[ite]] = loo_wtd(loo_07_list[[ite]], svy_rake)
  loo_wtd_08_list[[ite]] = loo_wtd(loo_08_list[[ite]], svy_rake)
  loo_wtd_09_list[[ite]] = loo_wtd(loo_09_list[[ite]], svy_rake)
  loo_wtd_10_list[[ite]] = loo_wtd(loo_10_list[[ite]], svy_rake)
  
  loo_wtd_11_list[[ite]] = loo_wtd(loo_11_list[[ite]], svy_rake)
  loo_wtd_12_list[[ite]] = loo_wtd(loo_12_list[[ite]], svy_rake)
  loo_wtd_13_list[[ite]] = loo_wtd(loo_13_list[[ite]], svy_rake)
  loo_wtd_14_list[[ite]] = loo_wtd(loo_14_list[[ite]], svy_rake)
  loo_wtd_15_list[[ite]] = loo_wtd(loo_15_list[[ite]], svy_rake)
  

# appending loo individual estimates  -------------------------------------
  samp_data_list[[ite]]$elpd_loo_01 = loo_01$pointwise[,1]
  samp_data_list[[ite]]$elpd_loo_02 = loo_02$pointwise[,1]
  samp_data_list[[ite]]$elpd_loo_03 = loo_03$pointwise[,1]
  samp_data_list[[ite]]$elpd_loo_04 = loo_04$pointwise[,1]
  samp_data_list[[ite]]$elpd_loo_05 = loo_05$pointwise[,1]
  
  samp_data_list[[ite]]$elpd_loo_06 = loo_06$pointwise[,1]
  samp_data_list[[ite]]$elpd_loo_07 = loo_07$pointwise[,1]
  samp_data_list[[ite]]$elpd_loo_08 = loo_08$pointwise[,1]
  samp_data_list[[ite]]$elpd_loo_09 = loo_09$pointwise[,1]
  samp_data_list[[ite]]$elpd_loo_10 = loo_10$pointwise[,1]
  
  samp_data_list[[ite]]$elpd_loo_11 = loo_11$pointwise[,1]
  samp_data_list[[ite]]$elpd_loo_12 = loo_12$pointwise[,1]
  samp_data_list[[ite]]$elpd_loo_13 = loo_13$pointwise[,1]
  samp_data_list[[ite]]$elpd_loo_14 = loo_14$pointwise[,1]
  samp_data_list[[ite]]$elpd_loo_15 = loo_15$pointwise[,1]

  
}  

save.image(file="temp.RData", compress=T)

