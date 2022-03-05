## deducting predicted value from observed value 
# exp_6c3
# MSE -- using probabilities
mse_list = list()
for (ite in c(1:100)){
  mse_list[[ite]] = pred_list[[ite]] %>%
    mutate(sq_diff = (Estimate - prob_truth)^2) %>% 
    group_by(model) %>% 
    mutate(sum_sq_diff = sum(sq_diff)) %>% 
    distinct(MSE = sum_sq_diff/n) 
}



mse_model = list()
for(iter in 1:nrow(mse_list[[2]])){
  mse_model[[iter]] = do.call(rbind, lapply(mse_list, '[', iter,,))
  mse_model2 =  do.call(cbind,lapply(mse_model, '[',,2))
}

colnames(mse_model2) = c(paste0('model0',1:9), paste0('model',10:15),
                         paste0('model', c(4,7,9,10,12:15), 'a'))

# long format for plotting
mm2 = mse_model2 %>%
  melt() %>% 
  rename(model = variable) %>% 
  mutate(model = factor(model)) %>% 
  mutate(model = plyr::revalue(model, c('model01' = 'X1',
                                        'model02' = 'X2',
                                        'model03' = 'X3',
                                        'model04' = 'X4',
                                        'model05' = 'X1 + X2',
                                        'model06' = 'X1 + X3',
                                        'model07' = 'X1 + X4',
                                        'model08' = 'X2 + X3', 
                                        'model09' = 'X2 + X4',
                                        'model10' = 'X3 + X4',
                                        'model11' = 'X1 + X2 + X3', 
                                        'model12' = 'X1 + X2 + X4', 
                                        'model13' = 'X1 + X3 + X4', 
                                        'model14' = 'X2 + X3 + X4', 
                                        'model15' = 'X1 + X2 + X3 + X4',
                                        'model10a' = '*X3 + X4',
                                        'model12a' = '*X1 + X2 + X4', 
                                        'model13a' = '*X1 + X3 + X4',
                                        'model14a' = '*X2 + X3 + X4',
                                        'model15a' = '*X1 + X2 + X3 + X4',
                                        'model4a' = '*X4',
                                        'model7a' = '*X1 + X4',
                                        'model9a' = '*X2 + X4'))) 

mm2$model = fct_relevel(mm2$model, c('X2 + X4', '*X2 + X4', 'X1 + X2 + X4', '*X1 + X2 + X4', 'X2 + X3 + X4',
                                     '*X2 + X3 + X4', 'X1 + X2 + X3 + X4', '*X1 + X2 + X3 + X4', 
                                     'X4', '*X4', 'X1 + X4', '*X1 + X4', 'X3 + X4',  '*X3 + X4',
                                     'X1 + X3 + X4', '*X1 + X3 + X4', 'X2', 'X1 + X2', 'X2 + X3', 
                                     'X1 + X2 + X3', 'X1', 'X3', 'X1 + X3'))

g1 = ggplot(mm2, aes(x = value, y = model, group = model, color = model)) +
  geom_point(alpha=0.5) +
  scale_y_discrete(limits = rev) +
  scale_colour_manual(values = c(paletteer_c("ggthemes::Classic Blue", 8),
                                 paletteer_c("ggthemes::Red-Gold", 8),
                                 paletteer_c("ggthemes::Classic Green", 4),
                                 paletteer_c("ggthemes::Gray", 3))) +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  labs(title="MSE for MRP estimates") +
  xlim(0,0.17) + 
  annotate("label", x = 0.16, y = 19.5, label = "X2 and X4") +
  annotate("label", x = 0.16, y = 11.5, label = "X4 only") +
  annotate("label", x = 0.16, y = 6,  label = "X2 only") +
  annotate("label", x = 0.16, y = 2, label = "None")

ggsave("plot_mse.png", g1, width=6, height=7.5, units="in", device="png")




