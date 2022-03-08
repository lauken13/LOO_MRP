## plotting the draws from cmdstanr
par(mfrow=c(5,5))

ps_draws <- model15_fit_mrp$draws(variables = "U_X1",format = "df")%>%
  select(-c(".chain",".draw",".iteration"))

# drawing ranef for each covariates
for (ite in 1:5){
  ranef = data.frame(ps_draws)[,ite]
  plot(1:4000, ranef, type="l", ylim=c(min(ranef) - 4, max(ranef) + 4),
       col=alpha("darkred",0.5), frame.plot=F, main=paste0("X1_",ite))
  
  draws = model15 %>% 
    spread_draws(., r_X1[condition,term]) %>% 
    select(-c(".chain",".draw",".iteration")) %>% 
    filter(condition==ite)
  lines(1:4000, data.frame(draws)[,3],  col=alpha("darkblue", 0.5))  
}

legend(5,4.5, legend=c('cmdstanr', 'brms'), fill=c(alpha("darkred",0.8), alpha("darkblue",0.8)))
