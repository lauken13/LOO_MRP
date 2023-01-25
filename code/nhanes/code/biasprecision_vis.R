library(brms)
library(survey) # svydesign()
library(here)
library(tidyverse)
library(ggpubr)
source(here('02-super popn approach/functions.R'))
nhfinal = readRDS(file="nhanes/data/nhanes_final_gen.rds")

nhsub_dr2 = nhfinal %>% 
  filter(incl_dr2 == 1) 

nhsub_fas = nhfinal %>% 
  filter(incl_fas == 1) 

nhsub_gen = nhfinal %>% 
  filter(incl_gen == 1) 

col_pal2 = c('#E69F00', '#009E73', '#CC79A7')

ylim2 = c(0.3, 1.67)

# age ####
tab_popn_age = nhfinal %>% 
  group_by(age) %>% 
  summarise(prob_highBP = mean(high_bp),
            subtotal = n(),
            prop_resp = subtotal/nrow(nhfinal)) %>% 
  mutate(type = "population",
         prop_respPopn = prop_resp/prop_resp,
         vartype="bias") 

tab_dr2_age = nhsub_dr2 %>% 
    group_by(age) %>% 
    summarise(prob_highBP = mean(high_bp),
              subtotal = n(),
              prop_resp = subtotal/nrow(nhsub_dr2))  %>% 
    mutate(type = "dietary",
           prop_respPopn = prop_resp/tab_popn_age$prop_resp,
           vartype="precision")  

tab_fas_age = nhsub_fas %>% 
    group_by(age) %>% 
    summarise(prob_highBP = mean(high_bp),
              subtotal = n(),
              prop_resp = subtotal/nrow(nhsub_fas))  %>% 
    mutate(type = "fasting",
           prop_respPopn = prop_resp/tab_popn_age$prop_resp,
           vartype="inconsequential")  

tab_gen_age = nhsub_gen %>% 
  group_by(age) %>% 
  summarise(prob_highBP = mean(high_bp),
            subtotal = n(),
            prop_resp = subtotal/nrow(nhsub_gen))  %>% 
  mutate(prop_respPopn = prop_resp/tab_popn_age$prop_resp,
         type = "generated",
         vartype="ignorable") 

tab_age =  rbind(tab_popn_age, tab_dr2_age, tab_fas_age, tab_gen_age) 

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

tab_gen_eth = nhsub_gen %>% 
  group_by(ethnicity) %>% 
  summarise(prob_highBP = mean(high_bp),
            subtotal = n(),
            prop_resp = subtotal/nrow(nhsub_gen)) %>% 
  mutate(prop_respPopn = prop_resp/tab_popn_eth$prop_resp,
         type = "generated")

tab_eth = rbind(tab_popn_eth, tab_dr2_eth, tab_fas_eth, tab_gen_eth)

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

tab_gen_ow = nhsub_gen %>% 
  group_by(overweight) %>% 
  summarise(prob_highBP = mean(high_bp),
            subtotal = n(),
            prop_resp = subtotal/nrow(nhsub_gen)) %>% 
  mutate(prop_respPopn = prop_resp/tab_popn_ow$prop_resp,
         type = "generated")

tab_ow = rbind(tab_popn_ow, tab_dr2_ow, tab_fas_ow, tab_gen_ow)
tab_ow$overweight = recode(tab_ow$overweight, `0` = 'no', `1`= 'yes')

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

tab_gen_db = nhsub_gen %>% 
  group_by(diabetes) %>% 
  summarise(prob_highBP = mean(high_bp),
            subtotal = n(),
            prop_resp = subtotal/nrow(nhsub_gen)) %>% 
  mutate(prop_respPopn = prop_resp/tab_popn_db$prop_resp,
         type = "generated")

tab_db =  rbind(tab_popn_db, tab_dr2_db, tab_fas_db, tab_gen_db)

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

tab_gen_ts = nhsub_gen %>% 
  group_by(trb_sleep) %>% 
  summarise(prob_highBP = mean(high_bp),
            subtotal = n(),
            prop_resp = subtotal/nrow(nhsub_gen)) %>% 
  mutate(prop_respPopn = prop_resp/tab_popn_ts$prop_resp,
         type = "generated")

tab_ts =  rbind(tab_popn_ts, tab_dr2_ts, tab_fas_ts, tab_gen_ts)
tab_ts$trb_sleep = recode(tab_ts$trb_sleep, `0` = 'no', `1`= 'yes')


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

tab_gen_pa = nhsub_gen %>% 
  group_by(phys_act) %>% 
  summarise(prob_highBP = mean(high_bp),
            subtotal = n(),
            prop_resp = subtotal/nrow(nhsub_gen)) %>% 
  mutate(prop_respPopn = prop_resp/tab_popn_pa$prop_resp,
         type = "generated")

tab_pa =  rbind(tab_popn_pa, tab_dr2_pa, tab_fas_pa, tab_gen_pa)
tab_pa$phys_act = recode(tab_pa$phys_act, `0` = 'no', `1`= 'yes')

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

tab_gen_ms = nhsub_gen %>% 
  group_by(marital_status) %>% 
  summarise(prob_highBP = mean(high_bp),
            subtotal = n(),
            prop_resp = subtotal/nrow(nhsub_gen)) %>% 
  mutate(prop_respPopn = prop_resp/tab_popn_ms$prop_resp,
         type = "generated")

tab_ms =  rbind(tab_popn_ms, tab_dr2_ms, tab_fas_ms, tab_gen_ms)

# pov_level ####
tab_popn_pl = nhfinal %>% 
  group_by(pov_level) %>% 
  summarise(prob_highBP = mean(high_bp),
            subtotal = n(),
            prop_resp = n()/nrow(nhfinal)) %>% 
  mutate(type = "population",
         prop_respPopn = prop_resp/prop_resp,
         vartype = "bias") 

( tab_dr2_pl = nhsub_dr2 %>% 
    group_by(pov_level) %>% 
    summarise(prob_highBP = mean(high_bp),
              subtotal = n(),
              prop_resp = subtotal/nrow(nhsub_dr2))  %>% 
    mutate(type = "dietary",
           prop_respPopn = prop_resp/tab_popn_pl$prop_resp,
           vartype = "precision") ) 

( tab_fas_pl = nhsub_fas %>% 
    group_by(pov_level) %>% 
    summarise(prob_highBP = mean(high_bp),
              subtotal = n(),
              prop_resp = subtotal/nrow(nhsub_fas))  %>% 
    mutate(type = "fasting",
           prop_respPopn = prop_resp/tab_popn_pl$prop_resp,
           vartype = "inconsequential") )

tab_gen_pl = nhsub_gen %>% 
  group_by(pov_level) %>% 
  summarise(prob_highBP = mean(high_bp),
            subtotal = n(),
            prop_resp = subtotal/nrow(nhsub_gen)) %>% 
  mutate(prop_respPopn = prop_resp/tab_popn_pl$prop_resp,
         type = "generated",
         vartype = "ignorable")

tab_pl =  rbind(tab_popn_pl, tab_dr2_pl, tab_fas_pl, tab_gen_pl)

# sodium_lvl --------------------------------------------------------------
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

tab_gen_sl = nhsub_gen %>% 
  group_by(sodium_lvl) %>% 
  summarise(prob_highBP = mean(high_bp),
            subtotal = n(),
            prop_resp = subtotal/nrow(nhsub_gen)) %>% 
  mutate(prop_respPopn = prop_resp/tab_popn_sl$prop_resp,
         type = "generated")

tab_sl =  rbind(tab_popn_sl, tab_dr2_sl, tab_fas_sl, tab_gen_sl)

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

tab_gen_gd = nhsub_gen %>% 
  group_by(gender) %>% 
  summarise(prob_highBP = mean(high_bp),
            subtotal = n(),
            prop_resp = subtotal/nrow(nhsub_gen)) %>% 
  mutate(prop_respPopn = prop_resp/tab_popn_gd$prop_resp,
         type = "generated")

tab_gd =  rbind(tab_popn_gd, tab_dr2_gd, tab_fas_gd, tab_gen_gd)

# elst status ####
tab_popn_es = nhfinal %>% 
  group_by(elst_status) %>% 
  summarise(prob_highBP = mean(high_bp),
            subtotal = n(),
            prop_resp = n()/nrow(nhfinal)) %>% 
  mutate(type = "population",
         prop_respPopn = prop_resp/prop_resp,
         vartype="bias") 

( tab_dr2_es = nhsub_dr2 %>% 
    group_by(elst_status) %>% 
    summarise(prob_highBP = mean(high_bp),
              subtotal = n(),
              prop_resp = n()/nrow(nhsub_dr2))  %>% 
    mutate(type = "dietary",
           prop_respPopn = prop_resp/tab_popn_es$prop_resp,
           vartype="precision") ) 

( tab_fas_es = nhsub_fas %>% 
    group_by(elst_status) %>% 
    summarise(prob_highBP = mean(high_bp),
              subtotal = n(),
              prop_resp = subtotal/nrow(nhsub_fas))  %>% 
    mutate(type = "fasting",
           prop_respPopn = prop_resp/tab_popn_es$prop_resp,
           vartype="inconsequential") ) 

tab_gen_es = nhsub_gen %>% 
  group_by(elst_status) %>% 
  summarise(prob_highBP = mean(high_bp),
            subtotal = n(),
            prop_resp = subtotal/nrow(nhsub_gen)) %>% 
  mutate(prop_respPopn = prop_resp/tab_popn_es$prop_resp,
         type = "generated",
         vartype="ignorable")

tab_es =  rbind(tab_popn_es, tab_dr2_es, tab_fas_es, tab_gen_es)

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

( tab_gen_ed = nhsub_gen %>% 
    group_by(educ) %>% 
    summarise(prob_highBP = mean(high_bp),
              subtotal = n(),
              prop_resp = subtotal/nrow(nhsub_gen))  %>% 
    mutate(type = "generated",
           prop_respPopn = prop_resp/tab_popn_ed$prop_resp) )


tab_ed =  rbind(tab_popn_ed, tab_dr2_ed, tab_fas_ed, tab_gen_ed) %>% 
  mutate(educ = recode(educ, 'SomecollegeAA' = 'Some college/AA degree',
                       'HSgrad' = "High school/GED"))


# hiv_test ----------------------------------------------------------------
tab_popn_hv = nhfinal %>% 
  group_by(hiv_test) %>% 
  summarise(prob_highBP = mean(high_bp),
            subtotal = n(),
            prop_resp = subtotal/nrow(nhfinal)) %>% 
  mutate(type = "population",
         prop_respPopn = prop_resp/prop_resp) 

( tab_dr2_hv = nhsub_dr2 %>% 
    group_by(hiv_test) %>% 
    summarise(prob_highBP = mean(high_bp),
              subtotal = n(),
              prop_resp = subtotal/nrow(nhsub_dr2))  %>% 
    mutate(type = "dietary",
           prop_respPopn = prop_resp/tab_popn_hv$prop_resp) ) 

( tab_fas_hv = nhsub_fas %>% 
    group_by(hiv_test) %>% 
    summarise(prob_highBP = mean(high_bp),
              subtotal = n(),
              prop_resp = subtotal/nrow(nhsub_fas))  %>% 
    mutate(type = "fasting",
           prop_respPopn = prop_resp/tab_popn_hv$prop_resp) )

tab_gen_hv = nhsub_gen %>% 
  group_by(hiv_test) %>% 
  summarise(prob_highBP = mean(high_bp),
            subtotal = n(),
            prop_resp = subtotal/nrow(nhsub_gen)) %>% 
  mutate(prop_respPopn = prop_resp/tab_popn_hv$prop_resp,
         type = "generated")

tab_hv =  rbind(tab_popn_hv, tab_dr2_hv, tab_fas_hv, tab_gen_hv)

# smk_tobcig --------------------------------------------------------------
tab_popn_tc = nhfinal %>% 
  group_by(smk_tobcig) %>% 
  summarise(prob_highBP = mean(high_bp),
            subtotal = n(),
            prop_resp = subtotal/nrow(nhfinal)) %>% 
  mutate(type = "population",
         prop_respPopn = prop_resp/prop_resp) 

( tab_dr2_tc = nhsub_dr2 %>% 
    group_by(smk_tobcig) %>% 
    summarise(prob_highBP = mean(high_bp),
              subtotal = n(),
              prop_resp = subtotal/nrow(nhsub_dr2))  %>% 
    mutate(type = "dietary",
           prop_respPopn = prop_resp/tab_popn_tc$prop_resp) ) 

( tab_fas_tc = nhsub_fas %>% 
    group_by(smk_tobcig) %>% 
    summarise(prob_highBP = mean(high_bp),
              subtotal = n(),
              prop_resp = subtotal/nrow(nhsub_fas))  %>% 
    mutate(type = "fasting",
           prop_respPopn = prop_resp/tab_popn_tc$prop_resp) )

tab_gen_tc = nhsub_gen %>% 
  group_by(smk_tobcig) %>% 
  summarise(prob_highBP = mean(high_bp),
            subtotal = n(),
            prop_resp = subtotal/nrow(nhsub_gen)) %>% 
  mutate(prop_respPopn = prop_resp/tab_popn_tc$prop_resp,
         type = "generated")

tab_tc =  rbind(tab_popn_tc, tab_dr2_tc, tab_fas_tc, tab_gen_tc) %>% 
  mutate(smk_tobcig = recode(smk_tobcig, '0' = 'no', '1' = 'yes'))

# urn volume ####
tab_popn_uv = nhfinal %>% 
  group_by(urn_vol) %>% 
  summarise(prob_highBP = mean(high_bp),
            subtotal = n(),
            prop_resp = subtotal/nrow(nhfinal)) %>% 
  mutate(type = "population",
         prop_respPopn = prop_resp/prop_resp) 

( tab_dr2_uv = nhsub_dr2 %>% 
    group_by(urn_vol) %>% 
    summarise(prob_highBP = mean(high_bp),
              subtotal = n(),
              prop_resp = subtotal/nrow(nhsub_dr2))  %>% 
    mutate(type = "dietary",
           prop_respPopn = prop_resp/tab_popn_uv$prop_resp) ) 

( tab_fas_uv = nhsub_fas %>% 
    group_by(urn_vol) %>% 
    summarise(prob_highBP = mean(high_bp),
              subtotal = n(),
              prop_resp = subtotal/nrow(nhsub_fas))  %>% 
    mutate(type = "fasting",
           prop_respPopn = prop_resp/tab_popn_uv$prop_resp) )

( tab_gen_uv = nhsub_gen %>% 
    group_by(urn_vol) %>% 
    summarise(prob_highBP = mean(high_bp),
              subtotal = n(),
              prop_resp = subtotal/nrow(nhsub_gen))  %>% 
    mutate(type = "generated",
           prop_respPopn = prop_resp/tab_popn_uv$prop_resp) )

tab_uv =  rbind(tab_popn_uv, tab_dr2_uv, tab_fas_uv, tab_gen_uv)


# plotting altogether ####
shape_all = c('bias' = 16,
              'precision' = 17, 
              'inconsequential' = 15,
              'ignorable' = 23)
# bias ####
g1_age = pivot_longer(tab_age, age) %>% 
  filter(type == 'population') %>% 
  ggplot(aes(x=value, y=prob_highBP, group=type)) +
  facet_grid(name~type) +
  geom_point(size=3, colour = 'black', pch=12) +
  geom_line() +
  ggtitle('Probability of high blood pressure') + 
  ylab('')  +
  scale_colour_manual(values = col_pal2) +
  ylim(0,1) +
  theme(strip.text.y = element_blank()) +
  xlab('age')

g1_eth = tab_eth %>% 
  filter(type=='population') %>% 
  ggplot(aes(x=ethnicity, y=prob_highBP, group=type)) +
  geom_point(size=3, colour="black", pch=12) +
  geom_line() +
  ylab('') +
  ylim(0,1)  +
  theme(legend.position = "",
        axis.text.x = element_text(angle=15))

g1_ow = tab_ow %>% 
    filter(type=='population') %>% 
    ggplot(aes(x=overweight, y=prob_highBP, group=type)) +
    geom_point(size=3, colour="black", pch=12) +
    geom_line() +
  ylab('') +
  ylim(0,1)  +
  theme(legend.position = "")

shape_age = c('dietary' = 16,
              'fasting' = 17, 
              'generated' = 16)
g2_age = pivot_longer(tab_age, age) %>% 
  filter(type != 'population') %>% 
  ggplot(aes(x=value, y=prop_respPopn, colour = type, shape=type, fill=vartype, group=type)) +
  facet_grid(name~type) +
  geom_hline(yintercept=1, size=1, colour = "grey", linetype="dashed") +
  geom_point(size=3) +
  geom_line() +
  ggtitle('Proportion of respondents in sample to population') +
  scale_colour_manual(values = col_pal2, guide="none") +
  scale_shape_manual(values=shape_age, guide="none") +
  scale_fill_manual(values = shape_all,
                    guide = guide_legend(override.aes =  list(
                      shape = shape_all, colour="black", fill="black")) ) +
  theme(legend.position = "") +
  labs(fill="variable type", x='age', y="") +
  ylim(ylim2)




g2_eth =  pivot_longer(tab_eth, ethnicity) %>% 
  filter(type != 'population') %>% 
  ggplot(aes(x=value, y=prop_respPopn, colour = type, group=type)) +
  facet_grid(name~type) +
  geom_hline(yintercept=1, size=1, colour = "grey", linetype="dashed") +
  geom_point(size=3) +
  geom_line() +
  ylab('')  +
  scale_colour_manual(values = col_pal2) +
  theme(legend.position = "",
        strip.text.x = element_blank(),
        axis.text.x = element_text(size=8, angle = 13)) +
  xlab('ethnicity') +
  ylim(ylim2)

g2_ow = pivot_longer(tab_ow, overweight) %>% 
  filter(type != 'population') %>% 
  ggplot(aes(x=value, y=prop_respPopn, colour = type, group=type)) +
  facet_grid(name~type) +
  geom_hline(yintercept=1, size=1, colour = "grey", linetype="dashed") +
  geom_point(size=3) +
  geom_line() +
  ylab('')  +
  scale_colour_manual(values = col_pal2) +
  theme(legend.position = "",
        strip.text.x = element_blank()) +
  xlab('overweight') + 
  ylim(ylim2)

# precision ####
 g1_ms = pivot_longer(tab_ms, marital_status) %>%
    filter(type == 'population') %>% 
    ggplot(aes(x=value, y=prob_highBP, group=type)) +
    facet_grid(name~type) +
    geom_point(size=3, colour='black', pch = 12) +
    geom_line() +
    ylab('') +
    ylim(0,1)  +
    theme(legend.position = "",
          axis.text.x = element_text(angle=13),
          strip.text.y = element_blank(),
          strip.text.x = element_blank()) +
    xlab('marital status') 

g1_pa = tab_pa %>% 
  filter(type == 'population') %>% 
  ggplot(aes(x=phys_act, y=prob_highBP, group=type)) +
  geom_point(size=3, colour="black", pch=12) +
  geom_line() +
  ylab('') +
  ylim(0,1)  +
  theme(legend.position = "") +
  xlab("physical activity")

g1_db = tab_db %>% 
  filter(type == 'population') %>% 
  ggplot(aes(x=diabetes, y=prob_highBP, group=type)) +
  geom_point(size=3, colour="black", pch=12) +
  geom_line() +
  ylab('') +
  ylim(0,1)  +
  theme(legend.position = "")

g1_ts = tab_ts %>% 
  filter(type == 'population') %>% 
  ggplot(aes(x=trb_sleep, y=prob_highBP, group=type)) +
  geom_point(size=3, colour="black", pch=12) +
  geom_line() +
  ylab('') +
  ylim(0,1)  +
  theme(legend.position = "") +
  xlab('trouble sleeping')

g2_ms =  pivot_longer(tab_ms, marital_status) %>% 
  mutate(name = 'marital status') %>% 
  filter(type != 'population') %>% 
  ggplot(aes(x=value, y=prop_respPopn, colour = type, group=type)) +
  facet_grid(name~type) +
  geom_hline(yintercept=1, size=1, colour = "grey", linetype="dashed") +
  geom_point(size=3, pch=17) +
  geom_line() +
  ylab('')  +
  scale_colour_manual(values = col_pal2) +
  theme(legend.position = "",
        axis.text.x = element_text(angle=13),
        strip.text.x = element_blank()) +
  xlab('marital status') +
  ylim(ylim2)

shape_pa = c('dietary' = 16,
              'fasting' = 17, 
              'generated' = 17)
g2_pa =  pivot_longer(tab_pa, phys_act) %>% 
  mutate(name = 'physical activity') %>% 
  filter(type != 'population') %>% 
  ggplot(aes(x=value, y=prop_respPopn, colour = type, shape = type, group=type)) +
  facet_grid(name~type) +
  geom_hline(yintercept=1, size=1, colour = "grey", linetype="dashed") +
  geom_point(size=3) +
  geom_line() +
  ylab('')  +
  scale_colour_manual(values = col_pal2) +
  scale_shape_manual(values=shape_pa) +
  theme(legend.position = "",
        strip.text.x = element_blank()) +
  xlab('physical activity') +
  ylim(ylim2)


g2_db =  pivot_longer(tab_db, diabetes) %>% 
  filter(type != 'population') %>% 
  ggplot(aes(x=value, y=prop_respPopn, colour = type, shape=type, group=type)) +
  facet_grid(name~type) +
  geom_hline(yintercept=1, size=1, colour = "grey", linetype="dashed") +
  geom_point(size=3) +
  geom_line() +
  ylab('')  +
  scale_colour_manual(values = col_pal2) +
  scale_shape_manual(values=shape_pa) +
  theme(legend.position = "",
        strip.text.x = element_blank()) +
  xlab('diabetes') +
  ylim(ylim2)

g2_ts =  pivot_longer(tab_ts, trb_sleep) %>% 
  mutate(name = 'trouble sleeping') %>% 
  filter(type != 'population') %>% 
  ggplot(aes(x=value, y=prop_respPopn, colour = type, group=type)) +
  facet_grid(name~type) +
  geom_hline(yintercept=1, size=1, colour = "grey", linetype="dashed") +
  geom_point(size=3, pch=17) +
  geom_line() +
  ylab('')  +
  scale_colour_manual(values = col_pal2) +
  theme(legend.position = "",
        strip.text.x = element_blank()) +
  xlab('trouble sleeping') +
  ylim(ylim2)

ggarrange(g1_age, g2_age, g1_eth,g2_eth,
          g1_ow, g2_ow, g1_ms, g2_ms,
          g1_pa, g2_pa, g1_db, g2_db, g1_ts, g2_ts,
          nrow=7, ncol=2, widths=c(0.5, 1),
          common.legend=T)

ggsave(filename = here('nhanes/figures/plot1.png'), device = "png", 
       height=40, width=30, units="cm")

# inconsequential ####
g1_pl = pivot_longer(tab_pl, pov_level) %>% 
  mutate(name = "poverty level") %>% 
  filter(type == 'population') %>% 
  ggplot(aes(x=value, y=prob_highBP, group=type)) +
  facet_grid(name~type) +
  geom_point(size=3, colour="black", pch=12) +
  geom_line() +
  ylab('') +
  ylim(0,1)  +
  theme(legend.position = "",
        axis.text.x = element_text(angle=10),
        strip.text.y = element_blank()) +
  xlab('poverty level') + 
  ggtitle('Probability of high blood pressure') 


g1_sl = tab_sl %>% 
  filter(type == 'population') %>% 
  ggplot(aes(x=sodium_lvl, y=prob_highBP, group=type)) +
  geom_point(size=3, colour="black", pch=12) +
  geom_line() +
  ylab('') +
  ylim(0,1)  +
  theme(legend.position = "") +
  xlab('sodium intake level')

g1_gd = tab_gd %>% 
  filter(type == 'population') %>% 
  ggplot(aes(x=gender, y=prob_highBP, group=type)) +
  geom_point(size=3, colour="black", pch=12) +
  geom_line() +
  ylab('') +
  ylim(0,1)  +
  theme(legend.position = "") +
  scale_colour_manual(values = col_pal) 

g1_es = tab_es %>% 
  filter(type=="population") %>% 
  ggplot(aes(x=elst_status, y=prob_highBP, group=type)) +
  geom_point(size=3, colour="black", pch=12) +
  geom_line() +
  ylab('') +
  theme(legend.position = "") +
  ylim(0,1)  +
  xlab('elastography status')

shape_pl = c('dietary' = 15,
             'fasting' = 23, 
             'generated' = 15)
(g2_pl =  pivot_longer(tab_pl, pov_level) %>% 
  mutate(name = 'poverty level') %>% 
  filter(type != 'population') %>% 
  ggplot(aes(x=value, y=prop_respPopn, colour = type, shape=type, group=type, fill=type)) +
  facet_grid(name~type) +
  geom_hline(yintercept=1, size=1, colour = "grey", linetype="dashed") +
  geom_line() +
  geom_point(size=3) +
  scale_colour_manual(values = col_pal2, guide="none") +
  scale_shape_manual(values=shape_pl, guide="none") +
  scale_fill_manual(values = col_pal2, guide="none") +
  theme( legend.position = "",
         axis.text.x = element_text(angle=13)) +
    labs(x = 'poverty level', y="") +
  ylim(ylim2) + 
  ggtitle('Proportion of respondents in sample to population') )


g2_sl =  pivot_longer(tab_sl, sodium_lvl) %>% 
  mutate(name = 'sodium level') %>% 
  filter(type != 'population') %>% 
  ggplot(aes(x=value, y=prop_respPopn, colour = type, fill=type, shape=type, group=type)) +
  facet_grid(name~type) +
  geom_hline(yintercept=1, size=1, colour = "grey", linetype="dashed") +
  geom_line() +
  geom_point(size=3) +
  ylab('')  +
  scale_fill_manual(values = col_pal2, guide="none") +
  scale_colour_manual(values = col_pal2, guide="none") +
  scale_shape_manual(values=shape_pl, guide="none") +
  theme( legend.position = "",
         strip.text.x = element_blank()) +
  xlab('sodium level') +
  ylim(ylim2)

shape_gd = c('dietary' = 23,
             'fasting' = 23, 
             'generated' = 15)
g2_gd =  pivot_longer(tab_gd, gender) %>% 
  filter(type != 'population') %>% 
  ggplot(aes(x=value, y=prop_respPopn, colour = type, fill=type,shape=type, group=type)) +
  facet_grid(name~type) +
  geom_hline(yintercept=1, size=1, colour = "grey", linetype="dashed") +
  geom_line() +
  geom_point(size=3) +
  ylab('')  +
  scale_fill_manual(values = col_pal2, guide="none") +
  scale_colour_manual(values = col_pal2, guide="none") +
  scale_shape_manual(values=shape_gd, guide="none") +
  theme( legend.position = "",
         strip.text.x = element_blank()) +
  xlab('gender') + 
    ylim(ylim2)

shape_es = c('dietary' = 15,
             'fasting' = 15, 
             'generated' = 15)
g2_es =  pivot_longer(tab_es, elst_status) %>% 
    mutate(name = 'elastography status') %>% 
    filter(type != 'population') %>% 
    ggplot(aes(x=value, y=prop_respPopn, colour = type, group=type, shape=type, fill = vartype)) +
    facet_grid(name~type) +
    geom_hline(yintercept=1, size=1, colour = "grey", linetype="dashed") +
    geom_point(size=3) +
    geom_line() +
    scale_colour_manual(values = col_pal2, guide="none") +
    scale_shape_manual(values=shape_es, guide="none") +
    scale_fill_manual(values = shape_all, 
                      guide = guide_legend(override.aes = list(
                        shape = shape_all, colour="black", fill="black")) ) +
    theme(legend.position = "",
          axis.text.x = element_text(angle=10),
          strip.text.x = element_blank()) +
    labs(fill="variable type", x='elastography status', y="") +
    ylim(ylim2)


# ignorable ####
g1_uv = pivot_longer(tab_uv, urn_vol) %>% 
  filter(type == 'population') %>% 
  ggplot(aes(x=value, y=prob_highBP, group=type)) +
  facet_grid(name~type) +
  geom_point(size=3, colour="black", pch=12) +
  geom_line() +
  ylab('') +
  ylim(0,1)  +
  xlab('urine volume')  +
  theme(strip.text.y = element_blank(),
        strip.text.x=element_blank()) 

g1_ed = tab_ed %>% 
  filter(type == 'population') %>% 
  ggplot(aes(x=educ, y=prob_highBP, group=type)) +
  geom_point(size=3, colour="black", pch=12) +
  geom_line() +
  ylab('') +
  ylim(0,1)  +
  xlab('education') +
  theme(axis.text.x = element_text(angle=10))

g1_hv = tab_hv %>% 
  filter(type == 'population') %>% 
  ggplot(aes(x=hiv_test, y=prob_highBP, group=type)) +
  geom_point(size=3, colour="black", pch=12) +
  geom_line() +
  ylab('') +
  ylim(0,1)  +
  theme(legend.position = "") +
  scale_colour_manual(values = col_pal) +
  xlab('ever tested for HIV')

g1_tc = tab_tc %>% 
  filter(type == 'population') %>% 
  ggplot(aes(x=smk_tobcig, y=prob_highBP, group=type)) +
  geom_point(size=3, colour="black", pch=12) +
  geom_line() +
  ylab('') +
  ylim(0,1)  +
  theme(legend.position = "") +
  scale_colour_manual(values = col_pal) +
  xlab('smokes tobacco or cigarette')

shape_uv = c('dietary' = 15,
             'fasting' = 15, 
             'generated' = 23)
g2_uv =  pivot_longer(tab_uv, urn_vol) %>% 
  mutate(name = 'urine volume collected') %>% 
  filter(type != 'population') %>% 
  ggplot(aes(x=value, y=prop_respPopn, colour = type, fill=type, shape=type, group=type)) +
  facet_grid(name~type) +
  geom_hline(yintercept=1, size=1, colour = "grey", linetype="dashed") +
  geom_line() +
  geom_point(size=3) +
  ylab('')  +
  scale_colour_manual(values = col_pal2, guide="none") +
  scale_fill_manual(values=col_pal2, guide="none") +
  scale_shape_manual(values=shape_uv, guide="none") +
  theme( legend.position = "",
         strip.text.x=element_blank()) +
  xlab('urine volume collected') +
  ylim(ylim2)  

shape_ed = c('dietary' = 15,
             'fasting' = 23, 
             'generated' = 23)
g2_ed =  pivot_longer(tab_ed, educ) %>% 
  mutate(name = 'education level') %>% 
  filter(type != 'population') %>% 
  ggplot(aes(x=value, y=prop_respPopn, colour = type, fill=type, shape=type, group=type)) +
  facet_grid(name~type) +
  geom_hline(yintercept=1, size=1, colour = "grey", linetype="dashed") +
  geom_line() +
  geom_point(size=3) +
  ylab('')  +
  scale_colour_manual(values = col_pal2, guide="none") +
  scale_shape_manual(values=shape_ed, guide="none") +
  scale_fill_manual(values=col_pal2, guide="none") +
  theme( legend.position = "",
         strip.text.x = element_blank(),
         axis.text.x = element_text(size=8,angle=15)) +
  xlab('education level') +
  ylim(ylim2)

g2_tc =  pivot_longer(tab_tc, smk_tobcig) %>% 
  mutate(name = 'smoked tobacco or cigarette') %>% 
  filter(type != 'population') %>% 
  ggplot(aes(x=value, y=prop_respPopn, colour = type, fill=type, shape=type, group=type)) +
  facet_grid(name~type) +
  geom_hline(yintercept=1, size=1, colour = "grey", linetype="dashed") +
  geom_line() +
  geom_point(size=3, pch = 23) +
  ylab('')  +
  scale_colour_manual(values = col_pal2, guide="none") +
  scale_fill_manual(values=col_pal2, guide="none") +
  scale_shape_manual(values=shape_pl, guide="none") +
  theme( legend.position = "",
         strip.text.x = element_blank()) +
  xlab('smoked tobacco or cigarette') +
  ylim(ylim2)


shape_uv = c('dietary' = 15,
             'fasting' = 23, 
             'generated' = 23)
g2_hv =  pivot_longer(tab_hv, hiv_test) %>% 
  mutate(name = 'ever tested for HIV') %>% 
  filter(type != 'population') %>% 
  ggplot(aes(x=value, y=prop_respPopn, colour = type, fill=type, shape=type, group=type)) +
  facet_grid(name~type) +
  geom_hline(yintercept=1, size=1, colour = "grey", linetype="dashed") +
  geom_line() +
  geom_point(size=3) +
  ylab('')  +
  scale_shape_manual(values=shape_uv, guide="none") +
  scale_colour_manual(values = col_pal2, guide="none") +
  scale_fill_manual(values=col_pal2, guide="none") +
  theme( legend.position = "",
         strip.text.x = element_blank()) +
  xlab('ever tested for HIV') +
  ylim(ylim2)


ggarrange(g1_pl, g2_pl, g1_sl, g2_sl,
          g1_gd, g2_gd, g1_es, g2_es, 
          g1_ed, g2_ed, g1_hv, g2_hv,
          g1_tc, g2_tc, g1_uv, g2_uv,  
          ncol=2, nrow=8, widths=c(0.5, 1),
          common.legend=T)

ggsave(filename = here('nhanes/figures/plot2.png'), device = "png", 
       height=45, width=30, units="cm")


