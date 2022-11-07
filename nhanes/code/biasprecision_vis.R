library(brms)
library(survey) # svydesign()
library(here)
library(tidyverse)
source(here('02-super popn approach/functions.R'))
nhfinal = readRDS(file="nhanes/data/nhanes_final_gen.rds")

nhsub_dr2 = nhfinal %>% 
  filter(incl_dr2 == 1) 

nhsub_fas = nhfinal %>% 
  filter(incl_fas == 1) 

nhsub_gen = nhfinal %>% 
  filter(incl_gen == 1) 

col_pal = c('#F0E442', '#D55E00', '#CC79A7', "#56B4E9")


# ethnicity  --------------------------------------------------------------
# prob of outcome
( tab_popn_eth = nhfinal %>% 
  mutate(high_bp = as.numeric(high_bp)) %>% 
  group_by(ethnicity) %>% 
  summarise(prob_highBP = mean(high_bp),
            subtotal = n(),
            prop_resp = subtotal/nrow(nhfinal)) %>% 
    mutate(type = "population",
           prop_respPopn = prop_resp/prop_resp))

# prop of sample to population
( tab_dr2_eth = nhsub_dr2 %>% 
    group_by(ethnicity) %>% 
    summarise( prob_highBP = mean(high_bp),
               subtotal = n(),
               prop_resp = subtotal/nrow(nhsub_dr2))  %>% 
    mutate(type = "dietary",
           prop_respPopn = prop_resp/tab_popn_eth$prop_resp) ) 

( tab_fas_eth = nhsub_fas %>% 
  group_by(ethnicity) %>% 
  summarise(prob_highBP = mean(high_bp),
            subtotal = n(),
            prop_resp = subtotal/nrow(nhsub_fas))  %>% 
    mutate(type = "fasting",
           prop_respPopn = prop_resp/tab_popn_eth$prop_resp) )

tab_eth = rbind(tab_popn_eth, tab_dr2_eth, tab_fas_eth)

# overweight ####
( tab_popn_ow = nhfinal %>% 
    group_by(overweight) %>% 
    summarise(prob_highBP = mean(high_bp),
              subtotal = n(),
              prop_resp = subtotal/nrow(nhfinal)) %>% 
    mutate(type = "population",
           prop_respPopn = prop_resp/prop_resp) )

( tab_dr2_ow = nhsub_dr2 %>% 
    group_by(overweight) %>% 
    summarise(prob_highBP = mean(high_bp), 
              subtotal = n(),
              prop_resp = subtotal/nrow(nhsub_dr2))  %>% 
    mutate(type = "dietary",
           prop_respPopn = prop_resp/tab_popn_ow$prop_resp) ) 

( tab_fas_ow = nhsub_fas %>% 
    group_by(overweight) %>% 
    summarise(prob_highBP = mean(high_bp),
              subtotal = n(),
              prop_resp = subtotal/nrow(nhsub_fas))  %>%  
    mutate(type = "fasting",
           prop_respPopn = prop_resp/tab_popn_ow$prop_resp) )

tab_ow = rbind(tab_popn_ow, tab_dr2_ow, tab_fas_ow)

# age ####
tab_popn_age = nhfinal %>% 
    group_by(age) %>% 
  summarise(prob_highBP = mean(high_bp),
            subtotal = n(),
            prop_resp = subtotal/nrow(nhfinal)) %>% 
  mutate(type = "population",
         prop_respPopn = prop_resp/prop_resp) 

( tab_dr2_age = nhsub_dr2 %>% 
    group_by(age) %>% 
    summarise(prob_highBP = mean(high_bp),
              subtotal = n(),
              prop_resp = subtotal/nrow(nhsub_dr2))  %>% 
    mutate(type = "dietary",
           prop_respPopn = prop_resp/tab_popn_age$prop_resp) ) 

( tab_fas_age = nhsub_fas %>% 
    group_by(age) %>% 
    summarise(prob_highBP = mean(high_bp),
              subtotal = n(),
              prop_resp = subtotal/nrow(nhsub_fas))  %>% 
    mutate(type = "fasting",
           prop_respPopn = prop_resp/tab_popn_age$prop_resp) )

tab_age =  rbind(tab_popn_age, tab_dr2_age, tab_fas_age) 

# marital status ####
tab_popn_ms = nhfinal %>% 
    group_by(marital_status) %>% 
  summarise(prob_highBP = mean(high_bp),
            subtotal = n(),
            prop_resp = n()/nrow(nhfinal)) %>% 
  mutate(type = "population",
         prop_respPopn = prop_resp/prop_resp) 

( tab_dr2_ms = nhsub_dr2 %>% 
    group_by(marital_status) %>% 
    summarise(prob_highBP = mean(high_bp),
              subtotal = n(),
              prop_resp = n()/nrow(nhsub_dr2))  %>% 
    mutate(type = "dietary",
           prop_respPopn = prop_resp/tab_popn_ms$prop_resp) ) 

( tab_fas_ms = nhsub_fas %>% 
    group_by(marital_status) %>% 
    summarise(prob_highBP = mean(high_bp),
              subtotal = n(),
              prop_resp = subtotal/nrow(nhsub_fas))  %>% 
    mutate(type = "fasting",
           prop_respPopn = prop_resp/tab_popn_ms$prop_resp) )

tab_ms =  rbind(tab_popn_ms, tab_dr2_ms, tab_fas_ms)

# physical activity ####
tab_popn_pa = nhfinal %>% 
  group_by(phys_act) %>% 
  summarise(prob_highBP = mean(high_bp),
            subtotal = n(),
            prop_resp = subtotal/nrow(nhfinal)) %>% 
  mutate(type = "population",
         prop_respPopn = prop_resp/prop_resp) 

( tab_dr2_pa = nhsub_dr2 %>% 
    group_by(phys_act) %>% 
    summarise(prob_highBP = mean(high_bp),
              subtotal = n(),
              prop_resp = subtotal/nrow(nhsub_dr2))  %>% 
    mutate(type = "dietary",
           prop_respPopn = prop_resp/tab_popn_pa$prop_resp) ) 

( tab_fas_pa = nhsub_fas %>% 
    group_by(phys_act) %>% 
    summarise(prob_highBP = mean(high_bp),
              subtotal = n(),
              prop_resp = subtotal/nrow(nhsub_fas))  %>% 
    mutate(type = "fasting",
           prop_respPopn = prop_resp/tab_popn_pa$prop_resp) )

tab_pa =  rbind(tab_popn_pa, tab_dr2_pa, tab_fas_pa)


# diabetes ####
tab_popn_db = nhfinal %>% 
  group_by(diabetes) %>% 
  summarise(prob_highBP = mean(high_bp),
            subtotal = n(),
            prop_resp = subtotal/nrow(nhfinal)) %>% 
  mutate(type = "population",
         prop_respPopn = prop_resp/prop_resp) 

( tab_dr2_db = nhsub_dr2 %>% 
    group_by(diabetes) %>% 
    summarise(prob_highBP = mean(high_bp),
              subtotal = n(),
              prop_resp = subtotal/nrow(nhsub_dr2))  %>% 
    mutate(type = "dietary",
           prop_respPopn = prop_resp/tab_popn_db$prop_resp) ) 

( tab_fas_db = nhsub_fas %>% 
    group_by(diabetes) %>% 
    summarise(prob_highBP = mean(high_bp),
              subtotal = n(),
              prop_resp = subtotal/nrow(nhsub_fas))  %>% 
    mutate(type = "fasting",
           prop_respPopn = prop_resp/tab_popn_db$prop_resp) ) 

tab_db =  rbind(tab_popn_db, tab_dr2_db, tab_fas_db)

# trb_sleep ####
tab_popn_ts = nhfinal %>% 
  group_by(trb_sleep) %>% 
  summarise(prob_highBP = mean(high_bp),
            subtotal = n(),
            prop_resp = subtotal/nrow(nhfinal)) %>% 
  mutate(type = "population",
         prop_respPopn = prop_resp/prop_resp) 

( tab_dr2_ts = nhsub_dr2 %>% 
    group_by(trb_sleep) %>% 
    summarise(prob_highBP = mean(high_bp),
              subtotal = n(),
              prop_resp = subtotal/nrow(nhsub_dr2))  %>% 
    mutate(type = "dietary",
           prop_respPopn = prop_resp/tab_popn_ts$prop_resp) ) 

( tab_fas_ts = nhsub_fas %>% 
    group_by(trb_sleep) %>% 
    summarise(prob_highBP = mean(high_bp),
              subtotal = n(),
              prop_resp = subtotal/nrow(nhsub_fas))  %>% 
    mutate(type = "fasting",
           prop_respPopn = prop_resp/tab_popn_ts$prop_resp) )

tab_ts =  rbind(tab_popn_ts, tab_dr2_ts, tab_fas_ts)

# elst status ####
tab_popn_es = nhfinal %>% 
  group_by(elst_status) %>% 
  summarise(prob_highBP = mean(high_bp),
            subtotal = n(),
            prop_resp = n()/nrow(nhfinal)) %>% 
  mutate(type = "population",
         prop_respPopn = prop_resp/prop_resp) 

( tab_dr2_es = nhsub_dr2 %>% 
    group_by(elst_status) %>% 
    summarise(prob_highBP = mean(high_bp),
              subtotal = n(),
              prop_resp = n()/nrow(nhsub_dr2))  %>% 
    mutate(type = "dietary",
           prop_respPopn = prop_resp/tab_popn_es$prop_resp) ) 

( tab_fas_es = nhsub_fas %>% 
    group_by(elst_status) %>% 
    summarise(prob_highBP = mean(high_bp),
              subtotal = n(),
              prop_resp = subtotal/nrow(nhsub_fas))  %>% 
    mutate(type = "fasting",
           prop_respPopn = prop_resp/tab_popn_es$prop_resp) )

tab_es =  rbind(tab_popn_es, tab_dr2_es, tab_fas_es)

g1_es = tab_es %>% 
  ggplot(aes(x=elst_status, y=prob_highBP, colour=type)) +
  geom_point(size=2) +
  ggtitle('Probability of high blood pressure') + 
  ylab('') +
  ylim(0,1)  +
  scale_colour_manual(values = col_pal)  +
  xlab('elastography status')

g2_es = tab_es %>% 
  filter(type != 'population') %>% 
  ggplot(aes(x=elst_status, y=prop_respPopn, colour = type)) +
  geom_point(size=2) +
  ggtitle('Ratio of respondents in sample to population') +
  ylab('')  +
  scale_colour_manual(values = col_pal) +
  theme(legend.position = "") +
  geom_hline(yintercept=1, size=1, colour = "grey", linetype="dashed") +
  ylim(0.6,1.25)  +
  xlab('elastography status')

ggpubr::ggarrange(g1_es, g2_es)

# urn volume ####
tab_popn_uv = nhfinal %>% 
  group_by(urn_vol) %>% 
  summarise(prob_highBP = mean(high_bp),
            subtotal = n(),
            prop_resp = n()/nrow(nhfinal)) %>% 
  mutate(type = "population",
         prop_respPopn = prop_resp/prop_resp) 

( tab_dr2_uv = nhsub_dr2 %>% 
    group_by(urn_vol) %>% 
    summarise(prob_highBP = mean(high_bp),
              subtotal = n(),
              prop_resp = n()/nrow(nhsub_dr2))  %>% 
    mutate(type = "dietary",
           prop_respPopn = prop_resp/tab_popn_uv$prop_resp) ) 

( tab_fas_uv = nhsub_fas %>% 
    group_by(urn_vol) %>% 
    summarise(prob_highBP = mean(high_bp),
              subtotal = n(),
              prop_resp = subtotal/nrow(nhsub_fas))  %>% 
    mutate(type = "fasting",
           prop_respPopn = prop_resp/tab_popn_uv$prop_resp) )

tab_uv =  rbind(tab_popn_uv, tab_dr2_uv, tab_fas_uv)

g1_uv = tab_uv %>% 
  ggplot(aes(x=urn_vol, y=prob_highBP, colour=type)) +
  geom_point(size=2) +
  ggtitle('Probability of high blood pressure') + 
  ylab('') +
  ylim(0,1)  +
  scale_colour_manual(values = col_pal)  +
  xlab('urine volume')

g2_uv = tab_uv %>% 
  filter(type != 'population') %>% 
  ggplot(aes(x=urn_vol, y=prop_respPopn, colour = type)) +
  geom_point(size=2) +
  ggtitle('Ratio of respondents in sample to population') +
  ylab('')  +
  scale_colour_manual(values = col_pal) +
  theme(legend.position = "") +
  geom_hline(yintercept=1, size=1, colour = "grey", linetype="dashed") +
  ylim(0.6,1.25)  +
  xlab('urine volume')

ggpubr::ggarrange(g1_uv, g2_uv)

# educ ####
tab_popn_ed = nhfinal %>% 
  group_by(educ) %>% 
  summarise(prob_highBP = mean(high_bp),
            subtotal = n(),
            prop_resp = n()/nrow(nhfinal)) %>% 
  mutate(type = "population",
         prop_respPopn = prop_resp/prop_resp) 

( tab_dr2_ed = nhsub_dr2 %>% 
    group_by(educ) %>% 
    summarise(prob_highBP = mean(high_bp),
              subtotal = n(),
              prop_resp = n()/nrow(nhsub_dr2))  %>% 
    mutate(type = "dietary",
           prop_respPopn = prop_resp/tab_popn_ed$prop_resp) ) 

( tab_fas_ed = nhsub_fas %>% 
    group_by(educ) %>% 
    summarise(prob_highBP = mean(high_bp),
              subtotal = n(),
              prop_resp = subtotal/nrow(nhsub_fas))  %>% 
    mutate(type = "fasting",
           prop_respPopn = prop_resp/tab_popn_ed$prop_resp) )

tab_ed =  rbind(tab_popn_ed, tab_dr2_ed, tab_fas_ed)

g1_ed = tab_ed %>% 
  ggplot(aes(x=educ, y=prob_highBP, colour=type)) +
  geom_point(size=2) +
  ggtitle('Probability of high blood pressure') + 
  ylab('') +
  ylim(0,1)  +
  scale_colour_manual(values = col_pal) +
  xlab('education')

g2_ed = tab_ed %>% 
  filter(type != 'population') %>% 
  ggplot(aes(x=educ, y=prop_respPopn, colour = type)) +
  geom_point(size=2) +
  ggtitle('Ratio of respondents in sample to population') +
  ylab('')  +
  scale_colour_manual(values = col_pal) +
  theme(legend.position = "") +
  geom_hline(yintercept=1, size=1, colour = "grey", linetype="dashed") +
  ylim(0.6,1.25)  +
  xlab('education')

ggpubr::ggarrange(g1_ed, g2_ed)

# gender ####
tab_popn_gd = nhfinal %>% 
  group_by(gender) %>% 
  summarise(prob_highBP = mean(high_bp),
            subtotal = n(),
            prop_resp = n()/nrow(nhfinal)) %>% 
  mutate(type = "population",
         prop_respPopn = prop_resp/prop_resp) 

( tab_dr2_gd = nhsub_dr2 %>% 
    group_by(gender) %>% 
    summarise(prob_highBP = mean(high_bp),
              subtotal = n(),
              prop_resp = n()/nrow(nhsub_dr2))  %>% 
    mutate(type = "dietary",
           prop_respPopn = prop_resp/tab_popn_gd$prop_resp) ) 

( tab_fas_gd = nhsub_fas %>% 
    group_by(gender) %>% 
    summarise(prob_highBP = mean(high_bp),
              subtotal = n(),
              prop_resp = subtotal/nrow(nhsub_fas))  %>% 
    mutate(type = "fasting",
           prop_respPopn = prop_resp/tab_popn_gd$prop_resp) )

tab_gd =  rbind(tab_popn_gd, tab_dr2_gd, tab_fas_gd)

# *generated samples -------------------------------------------------------
# ethnicity ####
tab_gen_eth = nhfinal %>% 
  filter(incl_gen == 1) %>% 
  group_by(ethnicity) %>% 
  summarise(prob_highBP = mean(high_bp),
            subtotal = n(),
            prop_resp = subtotal/nrow(.)) %>% 
  mutate(prop_respPopn = prop_resp/tab_popn_eth$prop_resp,
         type = "generated")

g1_eth = rbind(tab_eth, tab_gen_eth) %>% 
  ggplot(aes(x=ethnicity, y=prob_highBP, colour=type)) +
  geom_point(size=2) +
  ggtitle('Probability of high blood pressure') + 
  ylab('') +
  ylim(0,1)  +
  scale_colour_manual(values = col_pal)

g2_eth =  rbind(tab_eth, tab_gen_eth) %>% 
  filter(type != 'population') %>% 
  ggplot(aes(x=ethnicity, y=prop_respPopn, colour = type)) +
  geom_point(size=2) +
  ggtitle('Ratio of respondents in sample to population') +
  ylab('')  +
  scale_colour_manual(values = col_pal) +
  theme(legend.position = "") +
  geom_hline(yintercept=1, size=1, colour = "grey", linetype="dashed") 

ggpubr::ggarrange(g1_eth, g2_eth)

# age ####
tab_gen_age = nhfinal %>% 
  filter(incl_gen == 1) %>% 
  group_by(age) %>% 
  summarise(prob_highBP = mean(high_bp),
            subtotal = n(),
            prop_resp = subtotal/nrow(.))  %>% 
  mutate(prop_respPopn = prop_resp/tab_popn_age$prop_resp,
         type = "generated")

g1_age = rbind(tab_age, tab_gen_age) %>% 
  ggplot(aes(x=age, y=prob_highBP, colour = type)) +
  geom_point(size=2) +
  ggtitle('Probability of high blood pressure') + 
  ylab('') +
  ylim(0,1)  +
  scale_colour_manual(values = col_pal)

g2_age =  rbind(tab_age, tab_gen_age) %>% 
  filter(type != 'population') %>% 
  ggplot(aes(x=age, y=prop_respPopn, colour = type)) +
  geom_point(size=2) +
  ggtitle('Ratio of respondents in sample to population') +
  ylab('')  +
  scale_colour_manual(values = col_pal) +
  theme(legend.position = "") +
  geom_hline(yintercept=1, size=1, colour = "grey", linetype="dashed") +
  ylim(0.6,1.32)

ggpubr::ggarrange(g1_age, g2_age)

# overweight ####
tab_gen_ow = nhfinal %>% 
  filter(incl_gen == 1) %>% 
  group_by(overweight) %>% 
  summarise(prob_highBP = mean(high_bp),
            subtotal = n(),
            prop_resp = subtotal/n) %>% 
  mutate(prop_respPopn = prop_resp/tab_popn_ow$prop_resp,
         type = "generated")

tab_gen_ow %>% 
  ggplot(aes(x=overweight, y=prop_respPopn, colour = type)) +
  geom_point(size=2) +
  ggtitle('Ratio of respondents in sample to population') +
  ylab('')  +
  scale_colour_manual(values = col_pal) +
  theme(legend.position = "") +
  geom_hline(yintercept=1, size=1, colour = "grey", linetype="dashed") +
  ylim(0.6,1.25)

g1_ow = rbind(tab_ow, tab_gen_ow) %>% 
  ggplot(aes(x=overweight, y=prob_highBP, colour=type)) +
  geom_point(size=2) +
  ggtitle('Probability of high blood pressure') + 
  ylab('') +
  ylim(0,1)  +
  scale_colour_manual(values = col_pal)

g2_ow =  rbind(tab_ow, tab_gen_ow) %>% 
  filter(type != 'population') %>% 
  ggplot(aes(x=overweight, y=prop_respPopn, colour = type)) +
  geom_point(size=2) +
  ggtitle('Ratio of respondents in sample to population') +
  ylab('')  +
  scale_colour_manual(values = col_pal) +
  theme(legend.position = "") +
  geom_hline(yintercept=1, size=1, colour = "grey", linetype="dashed") +
  ylim(0.6,1.25)

ggpubr::ggarrange(g1_ow, g2_ow)

# marital status ####
tab_gen_ms = nhfinal %>% 
  filter(incl_gen == 1) %>% 
  group_by(marital_status) %>% 
  summarise(prob_highBP = mean(high_bp),
            subtotal = n(),
            prop_resp = subtotal/n) %>% 
  mutate(prop_respPopn = prop_resp/tab_popn_ms$prop_resp,
         type = "generated")

g1_ms = rbind(tab_ms, tab_gen_ms) %>% 
  ggplot(aes(x=marital_status, y=prob_highBP, colour=type)) +
  geom_point(size=2) +
  ggtitle('Probability of high blood pressure') + 
  ylab('') +
  ylim(0,1)  +
  scale_colour_manual(values = col_pal) +
  xlab('marital status')

g2_ms = rbind(tab_ms, tab_gen_ms) %>% 
  filter(type != 'population') %>% 
  ggplot(aes(x=marital_status, y=prop_respPopn, colour = type)) +
  geom_point(size=2) +
  ggtitle('Ratio of respondents in sample to population') +
  ylab('')  +
  scale_colour_manual(values = col_pal) +
  theme(legend.position = "") +
  geom_hline(yintercept=1, size=1, colour = "grey", linetype="dashed") +
  ylim(0.6,1.25) +
  xlab('marital status')

ggpubr::ggarrange(g1_ms, g2_ms)

# phys_act ####
tab_gen_pa = nhfinal %>% 
  filter(incl_gen == 1) %>% 
  group_by(phys_act) %>% 
  summarise(prob_highBP = mean(high_bp),
            subtotal = n(),
            prop_resp = subtotal/n) %>% 
  mutate(prop_respPopn = prop_resp/tab_popn_pa$prop_resp,
         type = "generated")

g1_pa = rbind(tab_pa, tab_gen_pa) %>% 
  ggplot(aes(x=phys_act, y=prob_highBP, colour=type)) +
  geom_point(size=2) +
  ggtitle('Probability of high blood pressure') + 
  ylab('') +
  ylim(0,1)  +
  scale_colour_manual(values = col_pal) +
  xlab("physical activity")

g2_pa = rbind(tab_pa, tab_gen_pa) %>% 
  filter(type != 'population') %>% 
  ggplot(aes(x=phys_act, y=prop_respPopn, colour = type)) +
  geom_point(size=2) +
  ggtitle('Ratio of respondents in sample to population') +
  ylab('')  +
  scale_colour_manual(values = col_pal) +
  theme(legend.position = "") +
  xlab("physical activity") +
  geom_hline(yintercept=1, size=1, colour = "grey", linetype="dashed") +
  ylim(0.6,1.25)

ggpubr::ggarrange(g1_pa, g2_pa)

# diabetes ####
tab_gen_db = nhfinal %>% 
  filter(incl_gen == 1) %>% 
  group_by(diabetes) %>% 
  summarise(prob_highBP = mean(high_bp),
            subtotal = n(),
            prop_resp = subtotal/n) %>% 
  mutate(prop_respPopn = prop_resp/tab_popn_db$prop_resp,
         type = "generated")

g1_db = rbind(tab_gen_db, tab_db) %>% 
  ggplot(aes(x=diabetes, y=prob_highBP, colour=type)) +
  geom_point(size=2) +
  ggtitle('Probability of high blood pressure') + 
  ylab('') +
  ylim(0,1)  +
  scale_colour_manual(values = col_pal) 

g2_db = rbind(tab_gen_db, tab_db) %>% 
  filter(type != 'population') %>% 
  ggplot(aes(x=diabetes, y=prop_respPopn, colour = type)) +
  geom_point(size=2) +
  ggtitle('Ratio of respondents in sample to population') +
  ylab('')  +
  scale_colour_manual(values = col_pal) +
  theme(legend.position = "") +
  geom_hline(yintercept=1, size=1, colour = "grey", linetype="dashed") +
  ylim(0.6,1.25)

ggpubr::ggarrange(g1_db, g2_db)

# trb_sleep ####
tab_gen_ts = nhfinal %>% 
  filter(incl_gen == 1) %>% 
  group_by(trb_sleep) %>% 
  summarise(prob_highBP = mean(high_bp),
            subtotal = n(),
            prop_resp = subtotal/n) %>% 
  mutate(prop_respPopn = prop_resp/tab_popn_ts$prop_resp,
         type = "generated")

g1_ts = rbind(tab_ts, tab_gen_ts) %>% 
  ggplot(aes(x=trb_sleep, y=prob_highBP, colour=type)) +
  geom_point(size=2) +
  ggtitle('Probability of high blood pressure') + 
  ylab('') +
  ylim(0,1)  +
  scale_colour_manual(values = col_pal)  +
  xlab('trouble sleeping')

g2_ts = rbind(tab_ts, tab_gen_ts) %>% 
  filter(type != 'population') %>% 
  ggplot(aes(x=trb_sleep, y=prop_respPopn, colour = type)) +
  geom_point(size=2) +
  ggtitle('Ratio of respondents in sample to population') +
  ylab('')  +
  scale_colour_manual(values = col_pal) +
  theme(legend.position = "") +
  geom_hline(yintercept=1, size=1, colour = "grey", linetype="dashed") +
  ylim(0.6,1.25) +
  xlab('trouble sleeping')

ggpubr::ggarrange(g1_ts, g2_ts)

## gender ####
tab_gen_gd = nhfinal %>% 
  filter(incl_gen == 1) %>% 
  group_by(gender) %>% 
  summarise(prob_highBP = mean(high_bp),
            subtotal = n(),
            prop_resp = subtotal/n) %>% 
  mutate(prop_respPopn = prop_resp/tab_popn_gd$prop_resp,
         type = "generated")


g1_gd = rbind(tab_gd, tab_gen_gd) %>% 
  ggplot(aes(x=gender, y=prob_highBP, colour=type)) +
  geom_point(size=2) +
  ggtitle('Probability of high blood pressure') + 
  ylab('') +
  ylim(0,1)  +
  scale_colour_manual(values = col_pal) 

g2_gd = rbind(tab_gd, tab_gen_gd) %>% 
  filter(type != 'population') %>% 
  ggplot(aes(x=gender, y=prop_respPopn, colour = type)) +
  geom_point(size=2) +
  ggtitle('Ratio of respondents in sample to population') +
  ylab('')  +
  scale_colour_manual(values = col_pal) +
  theme(legend.position = "") +
  geom_hline(yintercept=1, size=1, colour = "grey", linetype="dashed") +
  ylim(0.6,1.25)

ggpubr::ggarrange(g1_gd, g2_gd)


## sodium_lvl ####
tab_popn_sl = nhfinal %>% 
  group_by(sodium_lvl) %>% 
  summarise(prob_highBP = mean(high_bp),
            subtotal = n(),
            prop_resp = n()/nrow(nhfinal)) %>% 
  mutate(type = "population",
         prop_respPopn = prop_resp/prop_resp) 

( tab_dr2_sl = nhsub_dr2 %>% 
    group_by(sodium_lvl) %>% 
    summarise(prob_highBP = mean(high_bp),
              subtotal = n(),
              prop_resp = n()/nrow(nhsub_dr2))  %>% 
    mutate(type = "dietary",
           prop_respPopn = prop_resp/tab_popn_sl$prop_resp) ) 

( tab_fas_sl = nhsub_fas %>% 
    group_by(sodium_lvl) %>% 
    summarise(prob_highBP = mean(high_bp),
              subtotal = n(),
              prop_resp = subtotal/nrow(nhsub_fas))  %>% 
    mutate(type = "fasting",
           prop_respPopn = prop_resp/tab_popn_sl$prop_resp) )

tab_gen_sl = nhfinal %>% 
  filter(incl_gen == 1) %>% 
  group_by(sodium_lvl) %>% 
  summarise(prob_highBP = mean(high_bp),
            subtotal = n(),
            prop_resp = subtotal/n) %>% 
  mutate(prop_respPopn = prop_resp/tab_popn_sl$prop_resp,
         type = "generated")

tab_sl =  rbind(tab_popn_sl, tab_dr2_sl, tab_fas_sl, tab_gen_sl)

g1_sl = tab_sl %>% 
  ggplot(aes(x=sodium_lvl, y=prob_highBP, colour=type)) +
  geom_point(size=2) +
  ggtitle('Probability of high blood pressure') + 
  ylab('') +
  ylim(0,1)  +
  scale_colour_manual(values = col_pal) 

g2_sl = tab_sl %>% 
  filter(type != 'population') %>% 
  ggplot(aes(x=sodium_lvl, y=prop_respPopn, colour = type)) +
  geom_point(size=2) +
  ggtitle('Ratio of respondents in sample to population') +
  ylab('')  +
  scale_colour_manual(values = col_pal) +
  theme(legend.position = "") +
  geom_hline(yintercept=1, size=1, colour = "grey", linetype="dashed") +
  ylim(0.3,1.25)

ggpubr::ggarrange(g1_sl, g2_sl)

## pov_level ####
tab_popn_pl = nhfinal %>% 
  group_by(pov_level) %>% 
  summarise(prob_highBP = mean(high_bp),
            subtotal = n(),
            prop_resp = n()/nrow(nhfinal)) %>% 
  mutate(type = "population",
         prop_respPopn = prop_resp/prop_resp) 

( tab_dr2_pl = nhsub_dr2 %>% 
    group_by(pov_level) %>% 
    summarise(prob_highBP = mean(high_bp),
              subtotal = n(),
              prop_resp = n()/nrow(nhsub_dr2))  %>% 
    mutate(type = "dietary",
           prop_respPopn = prop_resp/tab_popn_pl$prop_resp) ) 

( tab_fas_pl = nhsub_fas %>% 
    group_by(pov_level) %>% 
    summarise(prob_highBP = mean(high_bp),
              subtotal = n(),
              prop_resp = subtotal/nrow(nhsub_fas))  %>% 
    mutate(type = "fasting",
           prop_respPopn = prop_resp/tab_popn_pl$prop_resp) )

tab_gen_pl = nhfinal %>% 
  filter(incl_gen == 1) %>% 
  group_by(pov_level) %>% 
  summarise(prob_highBP = mean(high_bp),
            subtotal = n(),
            prop_resp = subtotal/n) %>% 
  mutate(prop_respPopn = prop_resp/tab_popn_pl$prop_resp,
         type = "generated")

tab_pl =  rbind(tab_popn_pl, tab_dr2_pl, tab_fas_pl, tab_gen_pl)

g1_pl = tab_pl %>% 
  ggplot(aes(x=pov_level, y=prob_highBP, colour=type)) +
  geom_point(size=2) +
  ggtitle('Probability of high blood pressure') + 
  ylab('') +
  ylim(0,1)  +
  scale_colour_manual(values = col_pal) +
  xlab('poverty level')

g2_pl = tab_pl %>% 
  filter(type != 'population') %>% 
  ggplot(aes(x=pov_level, y=prop_respPopn, colour = type)) +
  geom_point(size=2) +
  ggtitle('Ratio of respondents in sample to population') +
  ylab('')  +
  scale_colour_manual(values = col_pal) +
  theme(legend.position = "") +
  geom_hline(yintercept=1, size=1, colour = "grey", linetype="dashed") +
  ylim(0.6,1.35) +
  xlab('poverty level')

ggpubr::ggarrange(g1_pl, g2_pl)

