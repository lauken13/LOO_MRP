## plotting results
library(tidyverse)

res_list1 <- readRDS(here::here("code/simDesign1/data/res_list1.rds"))

indv_all_tab = res_list1$indv_all_tab
popn_indv_tab = res_list1$popn_indv_tab
model_sae_X1_tab = res_list1$model_sae_X1_tab
model_sae_X2_tab = res_list1$model_sae_X2_tab
model_sae_X3_tab = res_list1$model_sae_X3_tab
model_sae_X4_tab = res_list1$model_sae_X4_tab

popn_indv_tab$model = fct_relevel(popn_indv_tab$model, c('X2 + X4', 'X1 + X2 + X4', 'X2 + X3 + X4', 'X1 + X2 + X3 + X4', 
                                   'X4', 'X1 + X4', 'X3 + X4', 'X1 + X3 + X4',
                                   'X2', 'X1 + X2', 'X2 + X3', 'X1 + X2 + X3',
                                   'X1', 'X3', 'X1 + X3'))


# choosing colour blind friendly colours ----------------------------------
muted <- khroma::colour("muted")
bright <- khroma::colour("bright")
khroma::plot_scheme(muted(9), colours = TRUE, names = TRUE, size = 0.9)
khroma::plot_scheme(bright(7), colours = TRUE, names = TRUE, size = 0.9)


colour_palette_var  =  c(`X2 + X4` =  "#4477AA",
                         `X1 + X2 + X4` = "#88CCEE", 
                         `X2 + X3 + X4` = "#66CCEE",
                         `X1 + X2 + X3 + X4` = "#332288",
                         `X4` = "#CC6677", `X1 + X4` = "#AA4499", 
                         `X3 + X4` = "#EE6677", `X1 + X3 + X4` = "#882255",
                         `X2` = "#228833", `X1 + X2` = "#117733",
                         `X2 + X3` = "#999933", `X1 + X2 + X3` = "#DDCC77",
                         `X1` = "#DDDDDD", `X3` = "#BBBBBB", `X1 + X3` = "#879195FF")
# colour_palette_var  =  c( `X1 + X2 + X3` = "#ef6548",
#                           `X1 + X2 + X3 + X4` = "#014636", `X1 + X2 + X4 ` ="#02818a", `X2 + X3 + X4` = "#67a9cf", 
#                           `X1 + X3` = "#fed976",
#                           `X1 + X3 + X4` = "#f768a1")
# pals::tableau20(20)[c(1,2,9,10,3,4,7,8,13,14,5,6,17,18,12)]

## ELPD and individual interval score
corr_indiv_intervalscore <- popn_indv_tab %>%
  pivot_longer(cols = c("elpd_loo","wtdElpd_loo"), names_to = "type", values_to = "model_score")%>%
  mutate(type = factor(type),
         type = fct_recode(type, `LOO` = "elpd_loo", `WTD LOO` = "wtdElpd_loo"))%>%
  group_by(type)%>%
  summarise(cor_val = paste0("cor: ",round(cor(model_score, ind_intervalScr_mean),2)))%>%
  mutate(xloc = c(-230,-4700),yloc = c(8.5,8.5))
popn_indv_tab %>%
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
  facet_grid(.~type, scales = "free")+
  xlab("Model Score")+ ylab("Mean Individual Interval Score") +
  theme(legend.position = "bottom",
  legend.title = element_blank())


## ELPD and population MRP interval score
corr_popn_intervalscore <- popn_indv_tab %>%
  pivot_longer(cols = c("elpd_loo","wtdElpd_loo"), names_to = "type", values_to = "model_score")%>%
  mutate(type = factor(type),
         type = fct_recode(type, `LOO` = "elpd_loo", `WTD LOO` = "wtdElpd_loo"))%>%
  group_by(type)%>%
  summarise(cor_val = paste0("cor: ",round(cor(model_score, popn_intervalScr),2)))%>%
  mutate(xloc = c(-225,-4700),yloc = c(2,2))
popn_indv_tab %>%
  pivot_longer(cols = c("elpd_loo","wtdElpd_loo"), names_to = "type", values_to = "model_score")%>%
  mutate(type = factor(type),
         type = fct_recode(type, `LOO` = "elpd_loo", `WTD LOO` = "wtdElpd_loo"))%>%
  ggplot(., aes(x = model_score, y =    popn_intervalScr))+
  geom_point(alpha = .5, aes( colour = model))+
  #  geom_line(aes(group = iteration), method = "lm", se = FALSE, colour = "black", stat = "smooth", alpha = .3)+
  geom_text(data  = corr_popn_intervalscore,
            mapping = aes(x = xloc, y = yloc, label = cor_val))+
  scale_colour_manual(values = colour_palette_var) + 
  # ggthemes::scale_color_colorblind()+
  facet_grid(.~type, scales = "free")+
  xlab("Model Score")+ ylab("Population MRP Interval Score")+
  theme(legend.position = "bottom",
        legend.title = element_blank())


# SAE plots ---------------------------------------------------------------
saeX1_clean <- model_sae_X1_tab %>%
  rename(group = X1, elpdmean_sae = elpd_mean_X1sae, intervalScr_sae = intervalScr_X1sae)%>%
  select(group, iteration, intervalScr_sae, elpdmean_sae,model)%>%
  mutate(sae = "X1")

saeX2_clean <- model_sae_X2_tab %>%
  rename(group = X2, elpdmean_sae = elpd_mean_X2sae, intervalScr_sae = intervalScr_X2sae)%>%
  select(group, iteration, intervalScr_sae, elpdmean_sae,model)%>%
  mutate(sae = "X2")

saeX3_clean <- model_sae_X3_tab %>%
  rename(group = X3, elpdmean_sae = elpd_mean_X3sae, intervalScr_sae = intervalScr_X3sae)%>%
  select(group, iteration, intervalScr_sae, elpdmean_sae,model)%>%
  mutate(sae = "X3")

saeX4_clean <- model_sae_X4_tab %>%
  rename(group = X4, elpdmean_sae = elpd_mean_X4sae, intervalScr_sae = intervalScr_X4sae)%>%
  select(group, iteration, intervalScr_sae, elpdmean_sae,model)%>%
  mutate(sae = "X4")

sae_clean <- rbind(saeX1_clean,saeX2_clean, saeX3_clean, saeX4_clean)%>%
  mutate(full_model = grepl("X1 + X2 + X3 + X4",model, fixed = TRUE),
         ar_prior = grepl("*",model, fixed = TRUE))


# X1 - X3 -----------------------------------------------------------------
sae_clean %>%
  select(-full_model)%>%
  filter(!ar_prior, sae != "X4")%>%
  mutate(full_model = grepl("X1 + X2 + X3 + X4",model, fixed = TRUE))%>%
  ggplot(., aes(x = elpdmean_sae, y = intervalScr_sae, colour = model))+
  geom_point(alpha=.6)+
  facet_grid(sae~group)+
  scale_color_manual(values = colour_palette_var)+
  geom_rug(alpha=.3)+
  theme(legend.position = "bottom",
        legend.title = element_blank())+
  xlab("Mean SAE ELDP") + ylab("Interval Score at SAE")


# X4 ----------------------------------------------------------------------


sae_clean %>%
  select(-full_model)%>%
  filter(!ar_prior, sae == "X4")%>%
  mutate(full_model = grepl("X1 + X2 + X3 + X4",model, fixed = TRUE) 
  )%>%
  ggplot(., aes(x = elpdmean_sae, y = intervalScr_sae, colour = model))+
  geom_point(alpha=.6)+
  facet_wrap(.~group)+
  scale_color_manual(values = colour_palette_var)+
  geom_rug(alpha=.3)+
  theme(legend.position = "bottom",
        legend.title = element_blank())+
  xlab("Mean SAE ELDP") + ylab("Interval Score at SAE")

sae_clean %>% 
  select(-full_model)%>%
  filter(!ar_prior)%>%
  mutate(full_model = grepl("X1 + X2 + X3 + X4",model, fixed = TRUE))%>%
  ggplot(., aes(x = elpdmean_sae, y = intervalScr_sae, colour = model))+
  geom_point(alpha=.4, size=1, shape=16)+
  facet_grid(sae~group)+
  scale_color_manual(values = colour_palette_var_base)+
  geom_rug(alpha=.3)+
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank())+
  xlab("Mean SAE ELDP") + ylab("Interval Score at SAE") + 
  guides(colour = guide_legend(override.aes = list(size=3), nrow=4))
