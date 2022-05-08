## plotting files 
library(ggplot2)
library(paletteer)

load(here::here("/02-super popn approach/experiment4_SAE/03-data/all_tab.RData"))

model_sae_X1_tab_sub = model_sae_X1_tab %>% 
  filter(model != '*X1 + X2 + X3 + X4' & model != '*X1 + X3 + X4' &
           model != '*X1 + X2 + X3 + X4' & model != '*X1 + X3 + X4')
ggthemes::scale_color_colorblind()

# bias vs loo - X1 -------------------------------------------------------
# range of elpd values (weighted)
cols = c("#1C73B1FF", "#26456EFF",
         "#FB964EFF", "#DF5948FF",
         "#09622AFF",
         "#879195FF")

p = palette.colors(NULL, "Okabe-Ito")
pal = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", 
        "#FDBF6F", "#FF7F00")
(ps1 = ggplot(model_sae_X1_tab_sub, aes(x = elpd_sum_X1sae, y = -abs(bias_X50_X1sae), 
                                    colour = X1, shape = model)) +
  geom_point(alpha=0.9, size=2) +
    geom_smooth(method = "lm", se = FALSE, size = 1) +
  scale_colour_manual(values = pal) +
  scale_shape_manual(values = c(0:5,7,8,10,11,15:16)) +
  labs(title="SAE bias vs. loo for each X1-level", 
       x = "SAE elpd values",
       y = "SAE absolute mean bias") )


# bias vs loo - X2 -------------------------------------------------------
(p2 = ggplot(model_sae_X2_tab, aes(x = elpd_sum_X2sae, y = abs(bias_X50_X2sae), shape = X2, colour = model)) +
   geom_point(position = position_dodge(width = .5), alpha=0.7, size=2) +
   scale_colour_manual(values = pal) +
   scale_shape_manual(values = c(0:5,7,8,10,11,15:16)) +
   labs(title="SAE bias vs. loo for each X2-level", 
        x = "SAE elpd values",
        y = "SAE absolute mean bias") )

# bias vs loo - X3 -------------------------------------------------------

# bias vs loo - X4 -------------------------------------------------------
model_sae_X4_tab_sub = model_sae_X4_tab %>% 
  filter(model != '*X1 + X2 + X3 + X4' & model != '*X1 + X3 + X4' &
           model != 'X1 + X3 + X4' & model != 'X2 + X3 + X4')


# bias vs loo - X1 -------------------------------------------------------
# range of elpd values (weighted)
cols = c("#1C73B1FF", "#26456EFF",
         "#FB964EFF", "#DF5948FF",
         "#09622AFF",
         "#879195FF")

p = palette.colors(NULL, "Okabe-Ito")
pal = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", 
        "#FDBF6F", "#FF7F00","#1C73B1FF", "#26456EFF",
        "#FB964EFF", "#DF5948FF")
(ps4 = ggplot(model_sae_X4_tab_sub, aes(x = elpd_sum_X4sae, y = intervalScr_X4sae, 
                                        colour = X4, shape = model, group=X4)) +
    geom_point(alpha=0.7, size=2) +
    geom_smooth(method = "lm", se = FALSE, size = 1) +
    scale_colour_viridis_d() +
    # scale_colour_manual(values = pal) +
    scale_shape_manual(values = c(0:5,7,8,10,11,15:16)) +
    labs(title="SAE bias vs. loo for each X1-level", 
         x = "SAE elpd values",
         y = "SAE absolute mean bias") )
