## individual plot files 
names(popn_indv_tab)


#  bias vs elpd -----------------------------------------------------------
pal = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", 
        "#FDBF6F", "#FF7F00")
(g1 = ggplot(popn_indv_tab, aes(x = elpd_loo, y = 1 - ind_bias_mean_abs, colour=model)) +
  geom_point(alpha=0.7, shape=16, size=3) +
  theme(legend.position = "bottom") +
  scale_colour_manual(values = pal) +
  labs(title="1 - absolute individual bias (mean of each iteration) against elpd values", 
       y = "1 - absolute individual average bias", 
       x = "Elpd values") )

(g1a = ggplot(popn_indv_tab, aes(x = wtdElpd_loo, y = 1 - ind_bias_mean_abs, colour=model)) +
    geom_point(alpha=0.7, shape=16, size=3) +
    theme(legend.position = "bottom") +
    scale_colour_manual(values = pal) +
    labs(title="1 - absolute individual bias (mean of each iteration) against wtd elpd values", 
         y = "1 - absolute individual average bias", 
         x = "Wtd elpd values") )

p1 = ggpubr::ggarrange(g1,g1a,common.legend = TRUE, legend="bottom")

 ggsave(here::here("02-super popn approach/experiment4_SAE/02-results/indv_bias.png"), p1, width=8, height=6, units="in", device="png")


# ci_width vs elpd --------------------------------------------------------
(g2 = ggplot(popn_indv_tab, aes(x = elpd_loo, y = ind_ci_width_mean, colour=model)) +
    geom_point(alpha=0.7, shape=16, size=3) +
    theme(legend.position = "bottom") +
    scale_colour_manual(values = pal) +
    labs(title="ci_width against elpd values", 
         y = "ci_width average bias", 
         x = "Elpd values") )

(g2a = ggplot(popn_indv_tab, aes(x = wtdElpd_loo, y = ind_ci_width_mean, colour=model)) +
    geom_point(alpha=0.7, shape=16, size=3) +
    theme(legend.position = "bottom") +
    scale_colour_manual(values = pal) +
    labs(title="ci_width against wtd elpd values", 
         y = "ci_width average bias", 
         x = "Wtd elpd values") )

p2 = ggpubr::ggarrange(g2,g2a,common.legend = TRUE, legend="bottom")
ggsave(here::here("02-super popn approach/experiment4_SAE/02-results/indv_ci_width.png"), p2, width=8, height=6, units="in", device="png")


# intervalSc vs elpd ------------------------------------------------------
(g3 = ggplot(popn_indv_tab, aes(x = elpd_loo, y = ind_intervalScr_mean, colour=model)) +
    geom_point(alpha=0.7, shape=16, size=3) +
    theme(legend.position = "bottom") +
    scale_colour_manual(values = pal) +
    labs(title="Interval score against elpd values", 
         y = "Interval score", 
         x = "Elpd values") )

(g3a = ggplot(popn_indv_tab, aes(x = elpd_loo, y = 1-ind_intervalScr_mean, colour=model)) +
    geom_point(alpha=0.7, shape=16, size=3) +
    theme(legend.position = "bottom") +
    scale_colour_manual(values = pal) +
    labs(title="1 - Interval score against elpd values", 
         y = "1 - Interval score", 
         x = "Elpd values") )

(g3b= ggplot(popn_indv_tab, aes(x = wtdElpd_loo, y = 1-ind_intervalScr_mean, colour=model)) +
    geom_point(alpha=0.7, shape=16, size=3) +
    theme(legend.position = "bottom") +
    scale_colour_manual(values = pal) +
    labs(title="1 - Interval score against wtd elpd values", 
         y = "1 - Interval score", 
         x = "Elpd values") )

g4 = ggpubr::ggarrange(g3,g3a,g3b, ncol=3,common.legend = TRUE, legend="bottom")
ggsave(here::here("02-super popn approach/experiment4_SAE/02-results/indv_intervalSc.png"), g4, width=12, height=6, units="in", device="png")
