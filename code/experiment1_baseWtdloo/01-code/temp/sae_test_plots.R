## sae test plots

test2 = sae_clean_base %>%
  select(-full_model)%>%
  filter(!ar_prior)%>%
  mutate(full_model = grepl("X1 + X2 + X3 + X4", model, fixed = TRUE)) %>% 
 ggplot(., aes(x = elpdmean_sae, y = intervalScr_sae, size = n_sae, colour = model)) +
  geom_point(alpha=.2, shape=16)+
  scale_size(range = c(1,3), name="SAE sample size") +
  facet_grid(sae~group)+
  scale_color_manual(values = colour_palette_var_base)+
  geom_rug(alpha=.3, size=1)+
  theme_bw(base_size = 15) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title=element_text(size=16))+
  xlab("Mean SAE ELDP") + ylab("Interval Score at SAE") + 
  guides(colour = guide_legend(override.aes = list(size=5), nrow=4)) 

  ggsave(test2, width=11, height=10, filename = here("figures/elpd_sae_bubble.png"))
  

## sample sizes 
freq_sae_base = sae_clean_base %>%
  group_by(sae, group) %>%
  summarise(mean_n = round(mean(n_sae)))
( t1 = freq_sae_base %>% 
    spread(., group, mean_n) %>% .[, c(2:6,1)] %>% 
    rename(s1 = `1`, 
           s2 = `2`, 
           s3 = `3`, 
           s4 = `4`, 
           s5 = `5`, ))

( t2 = popn_counts %>%
    rename(popn_mean_n = value) %>% 
    spread(., group, popn_mean_n) %>% .[, c(2:6,1)] %>% 
    rename(p1 = `1`, 
           p2 = `2`, 
           p3 = `3`, 
           p4 = `4`, 
           p5 = `5`, ))

# getting proportions
( t3 = left_join(t1, t2, by='sae') %>% 
  mutate(prop1 = round(s1/p1,2), 
         prop2 = round(s2/p2,2), 
         prop3 = round(s3/p3,2), 
         prop4 = round(s4/p4,2), 
         prop5 = round(s5/p5,2)) )

 tab2 = sae_clean_base %>%
  select(-full_model)%>%
  filter(!ar_prior)%>%
  mutate(full_model = grepl("X1 + X2 + X3 + X4", model, fixed = TRUE))
 
 t4 =  t3 %>% 
   select(sae, prop1:prop5) 
 
  dat_text <- data.frame(
    label = c(prop1 = 0.05, prop2 = 0.05, prop3 = 0.05, prop4 = 0.05, prop5 = 0.06,
              prop1 = 0.05, prop2 = 0.05, prop3 = 0.05, prop4 = 0.05, prop5 = 0.07,
              prop1 = 0.02, prop2 = 0.02, prop3 = 0.05, prop4 = 0.08, prop5 = 0.1,
              prop1 = 0.02, prop2 = 0.02, prop3 = 0.05, prop4 = 0.08, prop5 = 0.09),
    x     = rep(-2.5, 5),
    y     = rep(8,5),
     group = rep(1:5),
    sae = rep(paste0('X', 1:4), each = 5)
  )
 
( p = ggplot(tab2, aes(x = elpdmean_sae, y = intervalScr_sae, colour = model)) +
  geom_point(alpha=.2, shape=16, size = 1)+
    facet_grid(sae~group) +
  scale_color_manual(values = colour_palette_var_base)+
  geom_rug(alpha=.3, size=1)+
  theme_bw(base_size = 15) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title=element_text(size=16))+
  xlab("Mean SAE ELDP") + ylab("Interval Score at SAE") +
  guides(colour = guide_legend(override.aes = list(size=5), nrow=4)) )
    
 
p + geom_text(data = dat_text, aes(x = x, y = y, label = label, colour = 'black')) +
  facet_grid(sae~group) 

# ggsave(test2, width=11, height=10, filename = here("figures/elpd_sae_bubble.png"))

