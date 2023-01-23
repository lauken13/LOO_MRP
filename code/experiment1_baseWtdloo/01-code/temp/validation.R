
# population - bias -------------------------------------------------------
pc = popnest_all_tab 
pc$model = fct_relevel(pc$model, c('X2 + X4', 'X1 + X2 + X4', 'X2 + X3 + X4', 'X1 + X2 + X3 + X4', 
                                   'X4', 'X1 + X4', 'X3 + X4', 'X1 + X3 + X4',
                                   'X2', 'X1 + X2', 'X2 + X3', 'X1 + X2 + X3',
                                   'X1', 'X3', 'X1 + X3'))

(g1 = ggplot(pc, aes(x = popn_bias_X50, y = model, fill = model))+
  geom_vline(aes(xintercept = 0)) +
  geom_violin() +
  xlim(c(-0.06, 0.25)) +
  # geom_errorbarh(mapping = aes(xmin = popn_bias_X5, 
  #                              xmax = popn_bias_X95), 
  #                position = position_dodge(width = .5),
  #                height = 0, alpha = .7) +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  scale_y_discrete(limits = rev) +
  scale_fill_manual(values = pals::tableau20(20)[c(1,2,9,10,3,4,7,8,13,14,5,6,17,18,12)]) + 
  labs(title="Difference in MRP estimate and truth (Bias)") +
  annotate("label", x = 0.23, y = 13.5, label = "X2 and X4") +
  annotate("label", x = 0.23, y = 9.5, label = "X4 only") +
  annotate("label", x = 0.23, y = 5.5,  label = "X2 only") +
  annotate("label", x = 0.23, y = 2, label = "None") )

## ploting ci width 
(g4 = ggplot(pc, aes(x = popn_ci_width, y = model, group = model, fill = model)) +
  geom_violin() +
  #xlim(c(0.04, 0.1)) +
  scale_y_discrete(limits = rev) +
  scale_fill_manual(values = pals::tableau20(20)[c(1,2,9,10,3,4,7,8,13,14,5,6,17,18,12)]) +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  labs(title="90% quantile width for MRP estimate ") +
  annotate("label", x = 0.09, y = 13.5, label = "X2 and X4") +
  annotate("label", x = 0.09, y = 9.5, label = "X4 only") +
  annotate("label", x = 0.09, y = 5.5,  label = "X2 only") +
  annotate("label", x = 0.09, y = 2, label = "None"))

## plotting coverage
pc_covProp = pc %>% 
  group_by(model) %>% 
  summarise(mean_coverage_ite = mean(popn_coverage))

xloc4 = 0.1
(p1 = ggplot(pc_covProp, aes(x = mean_coverage_ite, y = model, group = ite, colour = model))+
    geom_point(position = position_dodge(width = .5),size=5) +
    theme(legend.position = "none",
          axis.title = element_blank()) +
    scale_y_discrete(limits = rev) +
    scale_colour_manual(values = pals::tableau20(20)[c(1,2,9,10,3,4,7,8,13,14,5,6,17,18,12)]) + 
    labs(title="Mean proportion of coverage in population-level prediction") +
    # xlim(c(range(pc2$mean_coverage_ite)[1] - 0.1, range(pc2$mean_coverage_ite)[2] + 0.1)) +
    annotate("label", x = xloc4, y = 13.5, label = "X2 and X4") +
    annotate("label", x = xloc4, y = 9.5, label = "X4 only") +
    annotate("label", x = xloc4, y = 5.5,  label = "X2 only") +
    annotate("label", x = xloc4, y = 2, label = "None") )

## plotting coverage for individual 
pn = indv_all_tab %>% 
  group_by(model) %>% 
  summarise(mean_coverage_ite = mean(coverage_ind)) 

xloc4 = 0.3
(p1 = ggplot(pn, aes(x = mean_coverage_ite, y = model, group = ite, colour = model))+
    geom_point(position = position_dodge(width = .5), alpha=0.7) +
    theme(legend.position = "none",
          axis.title = element_blank()) +
    scale_y_discrete(limits = rev) +
    scale_colour_manual(values = pals::tableau20(20)[c(1,2,9,10,3,4,7,8,13,14,5,6,17,18,12)]) + 
    labs(title="Mean proportion of coverage in individual-level prediction") +
    # xlim(c(range(pn2$mean_coverage_ite)[1]-0.1, range(pn2$mean_coverage_ite)[2]+0.1)) +
    annotate("label", x = xloc4-0.5, y = 5.5, label = "X2 and X4") +
    annotate("label", x = xloc4, y = 3.5, label = "X4 only") +
    annotate("label", x = xloc4, y = 2,  label = "X2 only") +
    annotate("label", x = xloc4, y = 1, label = "None") )


## plotting interval scr vs coverage

(p1 = ggplot(pn, aes(x = mean_coverage_ite, y = model, group = ite, colour = model))+
    geom_point(position = position_dodge(width = .5), alpha=0.7) +
    theme(legend.position = "none",
          axis.title = element_blank()) +
    scale_y_discrete(limits = rev) +
    scale_colour_manual(values = pals::tableau20(20)[c(1,2,9,10,3,4,7,8,13,14,5,6,17,18,12)]) + 
    labs(title="Mean proportion of coverage in individual-level prediction") +
    # xlim(c(range(pn2$mean_coverage_ite)[1]-0.1, range(pn2$mean_coverage_ite)[2]+0.1)) +
    annotate("label", x = xloc4-0.5, y = 5.5, label = "X2 and X4") +
    annotate("label", x = xloc4, y = 3.5, label = "X4 only") +
    annotate("label", x = xloc4, y = 2,  label = "X2 only") +
    annotate("label", x = xloc4, y = 1, label = "None") )


## ELPD and individual interval score
indv_summ_tab

corr_indv_intervalscore <- indv_summ_tab %>%
  pivot_longer(cols = c("ind_coverage_mean"), names_to = "type", values_to = "model_score") %>%
  mutate(type = factor(type),
         type = fct_recode(type, `Cov_mean` = "ind_coverage_mean"))%>%
  group_by(type)%>%
  summarise(cor_val = paste0("cor: ",round(cor(model_score, ind_intervalScr_mean),2)))%>%
  mutate(xloc = c(-230),yloc = c(8.5))
indv_summ_tab %>%
  pivot_longer(cols = c("elpd_loo","wtdElpd_loo"), names_to = "type", values_to = "model_score")%>%
  mutate(type = factor(type),
         type = fct_recode(type, `LOO` = "elpd_loo", `WTD LOO` = "wtdElpd_loo"))%>%
  ggplot(., aes(x = model_score, y = ind_intervalScr_mean))+
  geom_point(alpha = .5, aes( colour = model))+
  #    geom_line(aes(group = iteration), method = "lm", se = FALSE, colour = "black", stat = "smooth", alpha = .3)+
  scale_colour_manual(values = colour_palette_var) + 
  # guides(fill = guide_legend(override.aes = list(size=12))) +
  # ggthemes::scale_color_colorblind()+
  geom_text(
    data    = corr_indiv_intervalscore,
    mapping = aes(x = xloc, y = yloc, label = cor_val))+
  # facet_grid(.~type, scales = "free")+
  xlab("Model Score")+ ylab("Mean Individual Interval Score") +
  theme(legend.position = "bottom",
        legend.title = element_blank())

# individual - ci_width -------------------------------------------------------
xloc = 0.18
(g1 = ggplot(indv_summ_tab, aes(x =ind_ci_width_mean, y = model, fill = model))+
    geom_violin() +
    theme(legend.position = "none",
          axis.title = element_blank()) +
    scale_y_discrete(limits = rev) +
    scale_fill_manual(values = pals::tableau20(20)[c(1,2,9,10,3,4,7,8,13,14,5,6,17,18,12)]) + 
    labs(title="Mean CI width across models and iterations for individual") +
    annotate("label", x = xloc, y = 13.5, label = "X2 and X4") +
    annotate("label", x = xloc, y = 9.5, label = "X4 only") +
    annotate("label", x = xloc, y = 5.5,  label = "X2 only") +
    annotate("label", x = xloc, y = 2, label = "None") )


# indv - bias -------------------------------------------------------------
xloc = 0.21 
(g1 = ggplot(indv_summ_tab, aes(x = ind_bias_mean_abs, y = model, fill = model))+
    geom_violin() +
    theme(legend.position = "none",
          axis.title = element_blank()) +
    scale_y_discrete(limits = rev) +
    scale_fill_manual(values = pals::tableau20(20)[c(1,2,9,10,3,4,7,8,13,14,5,6,17,18,12)]) + 
    labs(title="Mean bias across models and iterations for individual") +
    annotate("label", x = xloc, y = 13.5, label = "X2 and X4") +
    annotate("label", x = xloc+0.06, y = 9.5, label = "X4 only") +
    annotate("label", x = xloc, y = 5.5,  label = "X2 only") +
    annotate("label", x = xloc+0.06, y = 2, label = "None") )

xloc = 0.2
(g1 = ggplot(indv_all_tab, aes(x = bias_X50_ind, y = model, fill = model))+
    geom_violin() +
    theme(legend.position = "none",
          axis.title = element_blank()) +
    scale_y_discrete(limits = rev) +
    scale_fill_manual(values = pals::tableau20(20)[c(1,2,9,10,3,4,7,8,13,14,5,6,17,18,12)]) + 
    labs(title="Mean bias across models and iterations for individual") +
    annotate("label", x = xloc, y = 13.5, label = "X2 and X4") +
    annotate("label", x = xloc, y = 9.5, label = "X4 only") +
    annotate("label", x = xloc, y = 5.5,  label = "X2 only") +
    annotate("label", x = xloc, y = 2, label = "None") )


# indv - coverage ---------------------------------------------------------
pn = indv_all_tab %>% 
  group_by(model) %>% 
  summarise(mean_coverage_ite = mean(coverage_ind)) 

xloc4 = 0.3
(p1 = ggplot(pn, aes(x = mean_coverage_ite, y = model, group = ite, colour = model))+
    geom_point(position = position_dodge(width = .5), alpha=0.7) +
    theme(legend.position = "none",
          axis.title = element_blank()) +
    scale_y_discrete(limits = rev) +
    scale_colour_manual(values = pals::tableau20(20)[c(1,2,9,10,3,4,7,8,13,14,5,6,17,18,12)]) + 
    labs(title="Mean proportion of coverage in individual-level prediction") +
    # xlim(c(range(pn2$mean_coverage_ite)[1]-0.1, range(pn2$mean_coverage_ite)[2]+0.1)) +
    annotate("label", x = xloc4-0.5, y = 5.5, label = "X2 and X4") +
    annotate("label", x = xloc4, y = 3.5, label = "X4 only") +
    annotate("label", x = xloc4, y = 2,  label = "X2 only") +
    annotate("label", x = xloc4, y = 1, label = "None") )

