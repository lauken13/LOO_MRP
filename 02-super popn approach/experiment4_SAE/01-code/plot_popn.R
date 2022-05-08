## plot for population
library(ggpubr)
library(ggplot2)
library(ggarrange)

# colours


popn_indv_tab_subset = popn_indv_tab %>% 
  filter(model != '*X1 + X2 + X3 + X4' & model != '*X1 + X3 + X4' &
           model != '*X1 + X2 + X3 + X4' & model != '*X1 + X3 + X4')

popn_indv_tab_subset$model = forcats::fct_relevel(popn_indv_tab_subset$model, c('X1 + X2 + X3 + X4', 'X2 + X3 + X4',  'X1 + X2 + X4',
                                                                                'X1 + X3 + X4', 
                                                                          'X1 + X2 + X3', 
                                                                                'X1 + X3'))
pal8 = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", 
         "#FDBF6F", "#FF7F00")
pal6 = c("#1C73B1FF", "#26456EFF",
         "#FB964EFF", "#DF5948FF",
         "#09622AFF",
         "#879195FF")
names(popn_indv_tab)

# bias vs loo -------------------------------------------------------------
(pp1 = ggplot(popn_indv_tab_subset, aes(x = elpd_loo, y = 1 - abs(popn_bias_X50), size = popn_ci_width, colour=model)) +
  geom_point(alpha=0.7, shape=16) +
    # scale_size(popn_ci_width) +
  # theme(legend.position = "none") +
  scale_colour_manual(values = pal8) +
  labs(title="1 - Absolute bias against elpd values", 
       y = "1 - Absolute bias", 
       x = "Elpd values") )

(pp1a = ggplot(popn_indv_tab_subset, aes(x = wtdElpd_loo, y = 1 - abs(popn_bias_X50), size = popn_ci_width, colour=model)) +
    geom_point(alpha=0.7, shape=16) +
    # scale_size(popn_ci_width) +
    theme(legend.position = "none") +
    scale_colour_manual(values = pal8) +
    labs(title="1 - Absolute bias against elpd values", 
         y = "1 - Absolute bias", 
         x = "Wtd elpd values") )

(pl1 = ggpubr::ggarrange(pp1, pp1a,common.legend = TRUE, legend="bottom"))

ggsave(here::here("02-super popn approach/experiment4_SAE/02-results/popn_bias.png"), pl1, width=8, height=6, units="in", device="png")


# ci width vs loo ---------------------------------------------------------
(pp2 = ggplot(popn_indv_tab_subset, aes(x=elpd_loo, popn_ci_width, colour=model)) +
    geom_point(alpha=0.7, shape=16, size=4) +
    geom_abline(slope=1) +
   theme(legend.position = "none") +
    scale_colour_manual(values = pal8) +
    labs(title="Width of MRP estimates against elpd values", 
         y = "Prediction width of MRP estimates", 
         x = "Elpd values"))

(pp2a = ggplot(popn_indv_tab_subset, aes(x=wtdElpd_loo, popn_ci_width, colour=model)) +
    geom_point(alpha=0.7, shape=16, size=4) +
    geom_abline(slope=1) +
    theme(legend.position = "none") +
    scale_colour_manual(values = pal8) +
    labs(title="Width of MRP estimates against elpd values", 
         y = "Prediction width of MRP estimates", 
         x = "Wtd elpd values"))

(pl2 = ggpubr::ggarrange(pp2, pp2a,common.legend = TRUE, legend="bottom"))

ggsave(here::here("02-super popn approach/experiment4_SAE/02-results/popn_ci_width.png"), pl2, width=8, height=6, units="in", device="png")



# intervalSc vs loo -------------------------------------------------------
(pp3 = ggplot(popn_indv_tab_subset, aes(x=elpd_loo, popn_intervalScr, colour=model)) +
   geom_point(alpha=0.7, shape=16, size=4) +
   geom_abline(slope=1) +
   theme(legend.position = "none") +
   scale_colour_manual(values = pal8) +
   labs(title="Interval score against elpd values", 
        y = "Interval score of MRP estimates", 
        x = "Elpd values"))

(pp3a = ggplot(popn_indv_tab_subset, aes(x=wtdElpd_loo, popn_intervalScr, colour=model)) +
    geom_point(alpha=0.7, shape=16, size=4) +
    geom_abline(slope=1) +
    theme(legend.position = "none") +
    scale_colour_manual(values = pal8) +
    labs(title="Interval score against wtd elpd values", 
         y = "Interval score of MRP estimates", 
         x = "Wtd elpd values"))

(pl3 = ggpubr::ggarrange(pp3, pp3a,common.legend = TRUE, legend="bottom"))

ggsave(here::here("02-super popn approach/experiment4_SAE/02-results/popn_ci_width.png"), pl3, width=8, height=6, units="in", device="png")


combined <- p1 + p2 + p3 & theme(legend.position = "bottom")
combined + plot_layout(guides = "collect")

ggarrange(p1,p2,p3, ncol=3,common.legend = TRUE, legend="bottom")
