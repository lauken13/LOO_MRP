// The input data is a vector 'y' of length 'N'.
// full model but with AR prior for X4 (model15a)
data {
  int<lower=0> N; // number of samples
  int<lower=0> N_groups_X_cat; // the number of groups for X1, X2, X3
  int<lower=0> N_groups_X4; // the number of groups for X4

  int<lower=1,upper=N_groups_X4> X4[N]; // levels of X4
  int<lower=1,upper=N_groups_X_cat> X1[N]; // levels of X1
  int<lower=1,upper=N_groups_X_cat> X2[N]; // levels of X2
  int<lower=1,upper=N_groups_X_cat> X3[N]; // levels of X3

  int y[N]; // the response vector
}

parameters {
  vector[N_groups_X_cat] U_X1; // the random effect for X1
  vector[N_groups_X_cat] U_X2; // the random effect for X2
  vector[N_groups_X_cat] U_X3; // the random effect for X3
  vector[N_groups_X4] U_X4; // the random effect for X4
  
  real<lower=0> sigma_X1; // sd of U_X1 (hyperparam). halfnormal prior on this.
  real<lower=0> sigma_X2; // sd of U_X2 (hyperparam). halfnormal prior on this.
  real<lower=0> sigma_X3; // sd of U_X3 (hyperparam). halfnormal prior on this.
  real<lower=0> sigma_X4; // sd of U_X4 (hyperparam). halfnormal prior on this.

  real intercept; // the intercept (global fixed effect)
}

transformed parameters { 
  vector[N_groups_X4] U_X4_transformed;
  vector[N_groups_X_cat] U_X1_transformed;
  vector[N_groups_X_cat] U_X2_transformed;
  vector[N_groups_X_cat] U_X3_transformed;

  vector[N] yhat;
  
  U_X1_transformed = sigma_X1 * U_X1; // the random effect for X1
  U_X2_transformed = sigma_X2 * U_X2; // the random effect for X2
  U_X3_transformed = sigma_X3 * U_X3; // the random effect for X3
  U_X4_transformed = sigma_X4 * U_X4; // the random effect for X4

  for (i in 1:N) {
    yhat[i] =
    U_X1_transformed[X1[i]] + 
    U_X2_transformed[X2[i]] + 
    U_X3_transformed[X3[i]] +
    U_X4_transformed[X4[i]]; // the linear predictor at each point
  }
}

model {
  // priors
  sigma_X1 ~ normal(0,1);
  sigma_X2 ~ normal(0,1);
  sigma_X3 ~ normal(0,1);
  sigma_X4 ~ normal(0,1);
  
  U_X1 ~ normal(0,1); // random effect is normal
  U_X2 ~ normal(0,1);
  U_X3 ~ normal(0,1);
  U_X4 ~ normal(0,1);
  
  intercept ~ normal(0,1); // global intercept
  
  // model
  for (i in 1:N) {
    y[i] ~ bernoulli(inv_logit(yhat[i])); 
  }
}


