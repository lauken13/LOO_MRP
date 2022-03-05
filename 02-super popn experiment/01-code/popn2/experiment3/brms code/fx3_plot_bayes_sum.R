## plotting the bayes summaries 
# divergent ---------------------------------------------------------------
## combine summ_list then plot by model
hist(summ_list[[5]]$mean_di)

# energy ------------------------------------------------------------------
hist(summ_list[[5]]$mean_en)

# tree depth -------------------------------------------------------------
hist(summ_list[[5]]$mean_td)

summ_tab = do.call(rbind, summ_list)

pc = summ_tab %>% 
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
pc$model = fct_relevel(pc$model, c('X2 + X4', '*X2 + X4', 'X1 + X2 + X4', '*X1 + X2 + X4', 'X2 + X3 + X4',
                                   '*X2 + X3 + X4', 'X1 + X2 + X3 + X4', '*X1 + X2 + X3 + X4', 
                                   'X4', '*X4', 'X1 + X4', '*X1 + X4', 'X3 + X4',  '*X3 + X4',
                                   'X1 + X3 + X4', '*X1 + X3 + X4', 'X2', 'X1 + X2', 'X2 + X3', 
                                   'X1 + X2 + X3', 'X1', 'X3', 'X1 + X3'))




g1 = ggplot(pc, aes(x = mean_td, y = model, group = model, colour = model)) +
  geom_point(alpha=0.5) +
   xlim(c(4, 8.5)) +
  scale_y_discrete(limits = rev) +
  scale_colour_manual(values = c(paletteer_c("ggthemes::Classic Blue", 8),
                               paletteer_c("ggthemes::Red-Gold", 8),
                               paletteer_c("ggthemes::Classic Green", 4),
                               paletteer_c("ggthemes::Gray", 3))) +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  labs(title="Tree depth") +
  annotate("label", x = 4.15, y = 19.5, label = "X2 and X4") +
  annotate("label", x = 4.15, y = 11.5, label = "X4 only") +
  annotate("label", x = 4.15, y = 6,  label = "X2 only") +
  annotate("label", x = 4.15, y = 2, label = "None")

ggsave("plot_treedepth.png", g1, width=6, height=7.5, units="in", device="png")

