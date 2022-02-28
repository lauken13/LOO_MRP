## calculating coverage 

cov_calc = list()
cov_list = list()
for(iter in 1:length(pred_list)){
  cov_calc[[iter]] = pred_list[[iter]] %>% 
    mutate(marg_err = 1.64 * Est.Error, 
           low_int = Estimate - marg_err,
           upp_int = Estimate + marg_err, 
           range_int = upp_int - low_int,
           coverage = ifelse(prob_truth >= low_int & prob_truth <= upp_int, 1, 0))
  cov_list[[iter]] = cov_calc[[iter]] %>% 
    group_by(model) %>%
    summarise(coverage_sum = sum(coverage),
              coverage_prop = coverage_sum/n)
}

cov_range = lapply(cov_calc, function(x)select(x, model, range_int))
cov_range_int = do.call(rbind, cov_range) %>% 
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
cov_range_int$model = fct_relevel(cov_range_int$model, c('X2 + X4', '*X2 + X4', 'X1 + X2 + X4', '*X1 + X2 + X4', 'X2 + X3 + X4',
                                             '*X2 + X3 + X4', 'X1 + X2 + X3 + X4', '*X1 + X2 + X3 + X4', 
                                             'X4', '*X4', 'X1 + X4', '*X1 + X4', 'X3 + X4',  '*X3 + X4',
                                             'X1 + X3 + X4', '*X1 + X3 + X4', 'X2', 'X1 + X2', 'X2 + X3', 
                                             'X1 + X2 + X3', 'X1', 'X3', 'X1 + X3'))

g3 = ggplot(cov_range_int, aes(x = range_int, y = model, group = model, color = model)) +
  geom_point(alpha=0.5) +
  scale_y_discrete(limits = rev) +
  scale_colour_manual(values = c(paletteer_c("ggthemes::Classic Blue", 8),
                                 paletteer_c("ggthemes::Red-Gold", 8),
                                 paletteer_c("ggthemes::Classic Green", 4),
                                 paletteer_c("ggthemes::Gray", 3))) +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  xlim(0,2) + 
  labs(title="Width of coverage for MRP estimates") +
  annotate("label", x = 1.85, y = 19.5, label = "X2 and X4") +
  annotate("label", x = 1.85, y = 11.5, label = "X4 only") +
  annotate("label", x = 1.85, y = 6,  label = "X2 only") +
  annotate("label", x = 1.85, y = 2, label = "None")

ggsave("plot_coverage_width.png", g3, width=6, height=7.5, units="in", device="png")


cov_tab = do.call(rbind, cov_list) %>% 
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
cov_tab$model = fct_relevel(cov_tab$model, c('X2 + X4', '*X2 + X4', 'X1 + X2 + X4', '*X1 + X2 + X4', 'X2 + X3 + X4',
                                   '*X2 + X3 + X4', 'X1 + X2 + X3 + X4', '*X1 + X2 + X3 + X4', 
                                   'X4', '*X4', 'X1 + X4', '*X1 + X4', 'X3 + X4',  '*X3 + X4',
                                   'X1 + X3 + X4', '*X1 + X3 + X4', 'X2', 'X1 + X2', 'X2 + X3', 
                                   'X1 + X2 + X3', 'X1', 'X3', 'X1 + X3'))



g5 = ggplot(cov_tab, aes(x = coverage_prop, y = model, group = model, color = model)) +
  geom_point(alpha=0.5) +
  scale_y_discrete(limits = rev) +
  scale_colour_manual(values = c(paletteer_c("ggthemes::Classic Blue", 8),
                                 paletteer_c("ggthemes::Red-Gold", 8),
                                 paletteer_c("ggthemes::Classic Green", 4),
                                 paletteer_c("ggthemes::Gray", 3))) +
  theme(legend.position = "none",
        axis.title = element_blank()) +
   xlim(0.5,1) + 
  labs(title="Coverage proportion for MRP estimates") +
  annotate("label", x = 0.55, y = 19.5, label = "X2 and X4") +
  annotate("label", x = 0.55, y = 11.5, label = "X4 only") +
  annotate("label", x = 0.55, y = 6,  label = "X2 only") +
  annotate("label", x = 0.55, y = 2, label = "None")

 ggsave("plot_coverage_prop.png", g5, width=6, height=7.5, units="in", device="png")
 