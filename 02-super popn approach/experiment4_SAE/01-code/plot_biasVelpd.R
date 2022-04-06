## plotting bias and elpd values 
library(dplyr)
library(ggplot2)

# getting bias at popn level ----------------------------------------------
## poststratification step using theta_pop (predicted values)
# calculating popnest and prob. of outcome for each iteration
popnest_tab = do.call(rbind,popnest_list[iter])

pt_tab = do.call(rbind,pt_list) %>% 
  as_tibble() %>% 
  mutate(ite = iter) %>% 
  rename(prob_truth = V1)


## comparing to 'truth'
popnest_all_tab = popnest_tab %>% 
  mutate(mean_pe = as.numeric(popnest_tab[,2] - as.numeric(rep(pt_tab$prob_truth, each=6)))) %>% # recursive diff for pt_list
  mutate(abs_mean_pe = abs(popnest_tab[,2] - as.numeric(rep(pt_tab$prob_truth, each=6)))) %>% # absolute bias
  mutate(model = plyr::revalue(model, c('model06' = 'X1 + X3',
                                        'model11' = 'X1 + X2 + X3',  
                                        'model13' = 'X1 + X3 + X4', 
                                        'model13a' = '*X1 + X3 + X4', 
                                        'model15' = 'X1 + X2 + X3 + X4',
                                        'model15a' = '*X1 + X2 + X3 + X4')))


# unweighted loo ----------------------------------------------------------
elpd_06_tab = elpd_06_mat[iter,] %>% 
  as_tibble() %>% 
  mutate(model = 'X1 + X3') %>% 
  rename('elpd_loo' = V1, 'SE' = V2)

elpd_11_tab = elpd_11_mat[iter,] %>% 
  as_tibble() %>% 
  mutate(model = 'X1 + X2 + X3') %>% 
  rename('elpd_loo' = V1, 'SE' = V2)

elpd_13_tab = elpd_13_mat[iter,] %>% 
  as_tibble() %>% 
  mutate(model = 'X1 + X3 + X4') %>% 
  rename('elpd_loo' = V1, 'SE' = V2)

elpd_13a_tab = elpd_13a_mat[iter,] %>% 
  as_tibble() %>% 
  mutate(model = '*X1 + X3 + X4') %>% 
  rename('elpd_loo' = V1, 'SE' = V2)

elpd_15_tab = elpd_15_mat[iter,] %>% 
  as_tibble() %>% 
  mutate(model = 'X1 + X2 + X3 + X4') %>% 
  rename('elpd_loo' = V1, 'SE' = V2)

elpd_15a_tab = elpd_15a_mat[iter,] %>% 
  as_tibble() %>% 
  mutate(model = '*X1 + X2 + X3 + X4') %>% 
  rename('elpd_loo' = V1, 'SE' = V2)

elpd_all_tab = rbind(elpd_06_tab, elpd_11_tab, elpd_13_tab,
                     elpd_13a_tab, elpd_15_tab, elpd_15a_tab) %>% 
  mutate(low_elpd = .$elpd_loo - (.$SE*1.64), # calculating upper and lower bound of the elpd values
         upp_elpd = .$elpd_loo + (.$SE*1.64),
         range_pe = upp_elpd - low_elpd,
         iter = rep(1:length(iter), 6)) %>% 
  arrange(iter)

all_tab = left_join(popnest_all_tab, elpd_all_tab, by=c('model', 'iter')) %>% 
  as_tibble() %>% 
  mutate(model = factor(model)) %>% 
  mutate(elpd_loo_scaled = scale(elpd_loo),
         popnestX50_scaled = scale(popnestX50)) 
  
all_tab$model = forcats::fct_relevel(all_tab$model, c('X1 + X2 + X3 + X4', '*X1 + X2 + X3 + X4', 
                                                      'X1 + X3 + X4', '*X1 + X3 + X4',
                                                      'X1 + X2 + X3',
                                                      'X1 + X3'))

  

(p1 = ggplot(all_tab, aes(elpd_loo_scaled, mean_pe, colour=model)) +
  geom_point() +
  # geom_abline(slope=1) +
  scale_colour_manual(values = c("#1C73B1FF", "#26456EFF",
                                 "#FB964EFF", "#DF5948FF",
                                 "#09622AFF",
                                 "#879195FF")) +
  labs(title="Bias against elpd values", 
       y = "Bias", 
       x = "Elpd values") )

ggsave(here::here("02-super popn approach/experiment4_SAE/02-results/biasVelpd.png"), p1, width=6, height=6, units="in", device="png")


(p2 = ggplot(all_tab, aes(elpd_loo_scaled, abs_mean_pe, colour=model)) +
  geom_point() +
  geom_abline(slope=1) +
  scale_colour_manual(values = c("#1C73B1FF", "#26456EFF",
                                 "#FB964EFF", "#DF5948FF",
                                 "#09622AFF",
                                 "#879195FF")))

ggsave(here::here("02-super popn approach/experiment4_SAE/02-results/biasabsVelpd.png"), p2, width=6, height=6, units="in", device="png")

