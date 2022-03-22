## plotting mrp_est

## poststratification step using theta_pop (predicted values)
# calculating popnest and prob. of outcome for each iteration
popnest_tab = do.call(rbind,popnest_list[iter])

pt_list = do.call(rbind,pt_list)

## comparing to 'truth'
popnest_tab$mean_pe = as.numeric(popnest_tab[,2] - as.numeric(rep(pt_list, each=6))) # recursive diff for pt_list
popnest_tab$low_pe = as.numeric(popnest_tab[,1]  - as.numeric(rep(pt_list, each=6)))
popnest_tab$upp_pe = as.numeric(popnest_tab[,3]  - as.numeric(rep(pt_list, each=6)))

pc = popnest_tab %>% 
  mutate(range_pe = popnestX95 - popnestX5) %>% 
  mutate(model = factor(model)) %>% 
  mutate(model = plyr::revalue(model, c('model06' = 'X1 + X3',
                                        'model11' = 'X1 + X2 + X3',  
                                        'model13' = 'X1 + X3 + X4', 
                                        'model13a' = '*X1 + X3 + X4', 
                                        'model15' = 'X1 + X2 + X3 + X4',
                                        'model15a' = '*X1 + X2 + X3 + X4')))
pc$model = fct_relevel(pc$model, c('X1 + X2 + X3 + X4', '*X1 + X2 + X3 + X4', 
                                   'X1 + X3 + X4', '*X1 + X3 + X4',
                                   'X1 + X2 + X3',
                                   'X1 + X3'))

## plot diff in mean
xloc4 = 0.075
(p3 = ggplot(pc, aes(x = mean_pe, y = model, group = iter, colour = model))+
  geom_vline(aes(xintercept = 0)) +
  geom_point(position = position_dodge(width = .5), alpha=0.7) +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  scale_y_discrete(limits = rev) +
  scale_colour_manual(values = c("#1C73B1FF", "#26456EFF",
                                 "#FB964EFF", "#DF5948FF",
                                 "#09622AFF",
                                 "#879195FF")) + 
  labs(title="Difference in MRP estimate and truth") +
  xlim(c(-0.07, 0.1)) +
  annotate("label", x = xloc4, y = 5.5, label = "X2 and X4") +
  annotate("label", x = xloc4, y = 3.5, label = "X4 only") +
  annotate("label", x = xloc4, y = 2,  label = "X2 only") +
  annotate("label", x = xloc4, y = 1, label = "None") )

ggsave(here::here("02-super popn approach/experiment3/02-results/stan code/plot_mrp_truth_fx1.png"), p3, width=6, height=7.5, units="in", device="png")


## plotting variance range
xloc5 = 0.085
(p4 = ggplot(pc, aes(x = range_pe, y = model, group = model, fill = model)) +
  geom_violin() +
  scale_y_discrete(limits = rev) +
  scale_fill_manual(values = c("#1C73B1FF", "#26456EFF",
                               "#FB964EFF", "#DF5948FF",
                               "#09622AFF",
                               "#879195FF")) + 
  theme(legend.position = "none",
        axis.title = element_blank()) +
  labs(title="Quantile range for MRP estimate ") +
   xlim(c(0.06, 0.09)) +
  annotate("label", x = xloc5, y = 5.5, label = "X2 and X4") +
  annotate("label", x = xloc5, y = 3.5, label = "X4 only") +
  annotate("label", x = xloc5, y = 2,  label = "X2 only") +
  annotate("label", x = xloc5, y = 1, label = "None"))

ggsave(here::here("02-super popn approach/experiment3/02-results/stan code/plot_mrp_qt_range_fx1.png"), p4, width=6, height=7.5, units="in", device="png")
