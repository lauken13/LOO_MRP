## combining different measures and plots

# popn_coverage -----------------------------------------------------------
source('~/GitHub/LOO_MRP/02-super popn approach/experiment4_SAE/01-code/plot_coverage_popn.R')

View(pc2)
View(pu)

# coverage
pc3 = pc2 %>% 
  arrange(ite) %>% 
  arrange(model)

# elpd values
pu2 = pu %>% 
  rename(ite = iter) %>% 
  arrange(ite) %>% 
  arrange(model) %>% 
  select(elpd_loo, model, range_pe, ite)


tab1 = left_join(pc3, pu2, by=c('model', 'ite'))

(p1 = ggplot(tab1, aes(x = elpd_loo, mean_coverage_ite, colour=model)) +
  geom_point() +
  scale_colour_manual(values = c("#1C73B1FF", "#26456EFF",
                                 "#FB964EFF", "#DF5948FF",
                                 "#09622AFF",
                                 "#879195FF")) +
  labs(title = 'Popn. coverage vs. elpd values', 
       x = 'Elpd values', y = 'Popn. coverage'))

ggsave(here::here("02-super popn approach/experiment4_SAE/02-results/popnCovVelpd.png"), p1, width=6, height=7.5, units="in", device="png")

# against range of elpd values

(p1a = ggplot(tab1, aes(x = range_pe, mean_coverage_ite, colour=model)) +
    geom_point() +
    scale_colour_manual(values = c("#1C73B1FF", "#26456EFF",
                                   "#FB964EFF", "#DF5948FF",
                                   "#09622AFF",
                                   "#879195FF")) +
    labs(title = 'Popn. coverage vs. width of interval of elpd values', 
         x = 'Width of elpd values', y = 'Popn. coverage'))

ggsave(here::here("02-super popn approach/experiment4_SAE/02-results/popnCovVelpdInt.png"), p1a, width=6, height=7.5, units="in", device="png")


# indv coverage -----------------------------------------------------------
pn3 = pn2 %>% 
  arrange(ite) %>% 
  arrange(model)

tab2 = left_join(pn3, pu2, by=c('model', 'ite'))

tab2 = tab2 %>% 
  mutate(elpd_loo_scaled = scale(elpd_loo),
         mean_coverage_ite_scaled = scale(mean_coverage_ite))

(p2 = ggplot(tab2, aes(x = elpd_loo, mean_coverage_ite, size=range_pe, colour=model)) +
    geom_point(alpha=0.7) +
    geom_abline(slope=1) +
    scale_size(range = c(.1, 4)) +
    scale_colour_manual(values = c("#1C73B1FF", "#26456EFF",
                                   "#FB964EFF", "#DF5948FF",
                                   "#09622AFF",
                                   "#879195FF")) +
    labs(title = 'Indv. coverage vs. elpd values', 
         x = 'Elpd values', y = 'Indv. coverage'))

ggsave(here::here("02-super popn approach/experiment4_SAE/02-results/indvCovVelpd_bubble.png"), p2, width=6, height=7.5, units="in", device="png")

(p2a = ggplot(tab2, aes(x = range_pe, mean_coverage_ite, colour=model)) +
    geom_point() +
    geom_abline(slope=1) +
    scale_colour_manual(values = c("#1C73B1FF", "#26456EFF",
                                   "#FB964EFF", "#DF5948FF",
                                   "#09622AFF",
                                   "#879195FF")) +
    labs(title = 'Indv. coverage vs. width of elpd values', 
         x = 'Width of elpd values', y = 'Indv. coverage'))

ggsave(here::here("02-super popn approach/experiment4_SAE/02-results/indvCovVelpd_width.png"), p2a, width=6, height=7.5, units="in", device="png")

