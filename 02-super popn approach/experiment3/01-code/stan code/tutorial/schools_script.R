## script for stan files
## 8 school examples
library(rstan)
options(cores=parallel::detectCores())

## data
schools_dat <- list(
  J = 8, 
  y = c(28,  8, -3,  7, -1,  1, 18, 12),
  sigma = c(15, 10, 16, 11,  9, 11, 10, 18)
)


## fitting the stan model
fit3 <- stan('schools_3.stan', data=schools_dat, iter=4000)

# summary of fit
print(fit3)

# plotting the estimated parameters and its credible intervals
plot(fit3) 

# examining the samples
pairs(fit3, pars=c("mu", "tau"))
pairs(fit3, pars=c("theta"))
traceplot(fit3)
