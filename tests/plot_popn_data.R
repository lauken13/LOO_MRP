## plotting popn_data 
df_outcome <- popn_data %>%
  pivot_longer(c(X1_cont:X4_cont),
               names_to = c("variable","type"),
               values_to = "value",
               names_sep = "_")

ggplot(df_outcome, aes(x=value, y=outcome))+
  geom_point()+
  facet_wrap(~variable, ncol=2)+
  xlab("Continuous x value")+
  ylab("Probability of outcome")


df_long <- popn_data %>% 
  filter(type=="fct")%>%
  group_by(variable,value) %>%
  summarise(y_avg_prob = mean(outcome),incl_avg_prob = mean(incl_prob))

ggplot(df_long, aes(x=value, y=y_avg_prob))+
  geom_point()+
  facet_wrap(~variable, ncol=2)+
  xlab("Factor x value")+
  ylab("Probability of outcome")

ggplot(df_long, aes(x=value, y=incl_avg_prob))+
  geom_point()+
  facet_wrap(~variable, ncol=2)+
  xlab("Factor x value")+
  ylab("Probability of inclusion")

hist(samp_data$outcome)
