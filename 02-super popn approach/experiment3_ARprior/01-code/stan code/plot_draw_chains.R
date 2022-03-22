## from brms 
model15

## running cmdstan_model() using stan code from brms 
# cmdstanr
model15_brms1
model15_fit_brms1

# brms
ranef(model15)$X1

# cmdstanr
model15_fit_brms1$summary("z_1")
model15_fit_brms1$summary("U_X1")


# drawing ranef for each covariate
par(mfrow=c(3,3))

# plotting X4
for (ite in 1:6){
  # cmdstanr
  ps_draws <- model15_fit_mrp$draws(variables = "U_X4",format = "df")%>%
    select(-c(".chain",".draw",".iteration"))
  
  ranef = data.frame(ps_draws)[,ite]
  plot(1:4000, ranef, type="l", ylim=c(min(ranef) - 4, max(ranef) + 4),
       col=alpha("darkred",0.5), frame.plot=F, main=paste0("X4_",ite))
  
  # brms
  draws = model15 %>% 
    spread_draws(., r_X4[condition,term]) %>% 
    select(-c(".chain",".draw",".iteration")) %>% 
    filter(condition==ite)
  lines(1:4000, data.frame(draws)[,3],  col=alpha("darkblue", 0.5))  
}

# X2
for (ite in 1:5){
  # cmdstanr
  ps_draws <- model15_fit_mrp$draws(variables = "U_X2",format = "df")%>%
    select(-c(".chain",".draw",".iteration"))
  
  ranef = data.frame(ps_draws)[,ite]
  plot(1:4000, ranef, type="l", ylim=c(min(ranef) - 4, max(ranef) + 4),
       col=alpha("darkred",0.5), frame.plot=F, main=paste0("X2_",ite))
  
  # brms
  draws = model15 %>% 
    spread_draws(., r_X2[condition,term]) %>% 
    select(-c(".chain",".draw",".iteration")) %>% 
    filter(condition==ite)
  lines(1:4000, data.frame(draws)[,3],  col=alpha("darkblue", 0.5))  
}

# X1
for (ite in 1:5){
  # cmdstanr
  ps_draws <- model15_fit_mrp_test$draws(variables = "U_X1",format = "df")%>%
    select(-c(".chain",".draw",".iteration"))
  
  ranef = data.frame(ps_draws)[,ite]
  plot(1:4000, ranef, type="l", ylim=c(min(ranef) - 4, max(ranef) + 4),
       col=alpha("darkred",0.5), frame.plot=F, main=paste0("X1_",ite))
  
  # brms
  draws = model15 %>% 
    spread_draws(., r_X1[condition,term]) %>% 
    select(-c(".chain",".draw",".iteration")) %>% 
    filter(condition==ite)
  lines(1:4000, data.frame(draws)[,3],  col=alpha("darkblue", 0.5))  
}

legend(5,4.5, legend=c('cmdstanr', 'brms'), fill=c(alpha("darkred",0.8), alpha("darkblue",0.8)))
