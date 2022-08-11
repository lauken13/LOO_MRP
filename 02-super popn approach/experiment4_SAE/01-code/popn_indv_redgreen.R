# quick fix to add y_prob from samp_data_list 


load(here::here('02-super popn approach/experiment4_SAE/03-data/temp/loo_sae_500.RData'))

# y prob
iter = 1:100
y_prob_list = lapply(iter, function(x)matrix(NA, nrow=500))
for(i in iter){
  y_prob_list[[i]][,1] = samp_data_list[[i]]$y_prob
}

y_prob_tab = do.call(rbind, y_prob_list) %>% 
  as_tibble(.)
names(y_prob_tab) = c('y_prob')

# elpd
popn_indv_tab_sae_short = popn_indv_tab_sae %>% 
  select(model, iteration, elpd_loo, wtdElpd_loo)

alph = 0.1
w1_AR = indv_all_tab_sae %>% 
  filter(model == '*X1 + X2 + X3 + X4' | model == 'X1 + X2 + X3 + X4') %>%  
  mutate(ar_prior = ifelse(model == '*X1 + X2 + X3 + X4', 1, 0)) %>% 
  filter(ar_prior == 1) %>% 
  bind_cols(., y_prob_tab) %>% # taking y_prob
  mutate(ind_intervalScr_yprob = (sampestX95 - sampestX5) + 
           ((2 / alph * (sampestX5 - y_prob)) * ifelse(y_prob < sampestX5, 1, 0)) + 
           ((2 / alph * (y_prob - sampestX95)) * ifelse(y_prob > sampestX95, 1, 0))) %>% 
  left_join(., popn_indv_tab_sae_short, by=c('model', 'iteration')) %>%  
  group_by(iteration) %>% 
  summarise('ind_intervalScr_yprob_*X1 + X2 + X3 + X4' = mean(ind_intervalScr_yprob),
            'elpd_loo_*X1 + X2 + X3 + X4' = mean(elpd_loo),
            'wtdElpd_loo_*X1 + X2 + X3 + X4' = mean(wtdElpd_loo)) %>% 
  select(-iteration)

w1_nonAR = indv_all_tab_sae %>% 
  filter(model == '*X1 + X2 + X3 + X4' | model == 'X1 + X2 + X3 + X4') %>%  
  mutate(ar_prior = ifelse(model == '*X1 + X2 + X3 + X4', 1, 0)) %>% 
  filter(ar_prior == 0) %>% 
  bind_cols(., y_prob_tab) %>% # taking y_prob
  mutate(ind_intervalScr_yprob = (sampestX95 - sampestX5) + 
           ((2 / alph * (sampestX5 - y_prob)) * ifelse(y_prob < sampestX5, 1, 0)) + 
           ((2 / alph * (y_prob - sampestX95)) * ifelse(y_prob > sampestX95, 1, 0))) %>% 
  left_join(., popn_indv_tab_sae_short, by=c('model', 'iteration')) %>% 
  group_by(iteration) %>% 
  summarise('ind_intervalScr_yprob_X1 + X2 + X3 + X4' = mean(ind_intervalScr_yprob),
            'elpd_loo_X1 + X2 + X3 + X4' = mean(elpd_loo),
            'wtdElpd_loo_X1 + X2 + X3 + X4' = mean(wtdElpd_loo)) %>% 
  select(-iteration)


w1_all = bind_cols(w1_AR, w1_nonAR) %>% 
  mutate(elpd_diff = `elpd_loo_*X1 + X2 + X3 + X4` - `elpd_loo_X1 + X2 + X3 + X4`, 
         wtdElpd_diff = `wtdElpd_loo_*X1 + X2 + X3 + X4` - `wtdElpd_loo_X1 + X2 + X3 + X4`, 
         intScr_diff = -(`ind_intervalScr_yprob_*X1 + X2 + X3 + X4` - `ind_intervalScr_yprob_X1 + X2 + X3 + X4`)) %>% 
  pivot_longer(cols = c("elpd_diff","wtdElpd_diff"), names_to = "type", values_to = "model_score") %>%
  mutate(type = factor(type),
         type = fct_recode(type, `PSIS-LOO` = "elpd_diff", `WTD-PSIS-LOO` = "wtdElpd_diff"))

xloc1 = -57; xloc2 = 250; xloc3 = -1; xloc4 = 5.2
yloc1 = -0.02; yloc2 = 0.15

g4_indv = ggplot(w1_all, aes(x = model_score, y = intScr_diff)) + 
  geom_rect(data =  w1_all[which(w1_all$type == 'WTD-PSIS-LOO'),], aes(xmin = xloc1, xmax = 0, ymin = yloc1, ymax = 0), fill="#98FB98",alpha = 0.02) + # green grid
  geom_rect(data =  w1_all[which(w1_all$type == 'WTD-PSIS-LOO'),], aes(xmin = 0, xmax = xloc2, ymin = 0, ymax = yloc2), fill="#98FB98",alpha = 0.02) +
  geom_rect(data =  w1_all[which(w1_all$type == 'WTD-PSIS-LOO'),], aes(xmin = xloc1, xmax = 0, ymin = 0, ymax = yloc2), fill="#FA8072",alpha = 0.007) + # red grid
  geom_rect(data =  w1_all[which(w1_all$type == 'WTD-PSIS-LOO'),], aes(xmin = 0, xmax = xloc2, ymin = yloc1, ymax = 0), fill="#FA8072",alpha = 0.007) +
  geom_rect(data =  w1_all[which(w1_all$type == 'PSIS-LOO'),], aes(xmin = xloc3, xmax = 0, ymin = yloc1, ymax = 0), fill="#98FB98",alpha = 0.02) + # green grid
  geom_rect(data =  w1_all[which(w1_all$type == 'PSIS-LOO'),], aes(xmin = 0, xmax = xloc4, ymin = 0, ymax = yloc2), fill="#98FB98",alpha = 0.02) +
  geom_rect(data =  w1_all[which(w1_all$type == 'PSIS-LOO'),], aes(xmin = xloc3, xmax = 0, ymin = 0, ymax = yloc2), fill="#FA8072",alpha = 0.007) + # red grid
  geom_rect(data =  w1_all[which(w1_all$type == 'PSIS-LOO'),], aes(xmin = 0, xmax = xloc4, ymin = yloc1, ymax = 0), fill="#FA8072",alpha = 0.007) +
  geom_point(alpha = .5, shape=16, size=3) + 
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  xlab("PSIS-LOO-based model score difference")+
  ylab("Interval score difference")+
  facet_grid(.~type, scales = "free") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw(base_size = 17) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) 
