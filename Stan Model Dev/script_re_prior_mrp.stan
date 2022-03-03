// The input data is a vector 'y' of length 'n'.
// full model but with AR prior for X4 (model15a)
data {
  int<lower=0> n; // number of samples
  int<lower=0> n_groups_X_cat; // the number of groups for X1, X2, X3
  int<lower=0> n_groups_X4; // the number of groups for X4

  int<lower=1,upper=n_groups_X4> X4[n]; // levels of X4
  int<lower=1,upper=n_groups_X_cat> X1[n]; // levels of X1
  int<lower=1,upper=n_groups_X_cat> X2[n]; // levels of X2
  int<lower=1,upper=n_groups_X_cat> X3[n]; // levels of X3
  
  // Assume no prediction of unobserved groups
  int<lower=0> Nj; // number of poststrat cells
  int<lower=1,upper=n_groups_X4> X4_pop[Nj]; // levels of X4
  int<lower=1,upper=n_groups_X_cat> X1_pop[Nj]; // levels of X1
  int<lower=1,upper=n_groups_X_cat> X2_pop[Nj]; // levels of X2
  int<lower=1,upper=n_groups_X_cat> X3_pop[Nj]; // levels of X3

  int y[n]; // the response vector
}

parameters {
  vector[n_groups_X_cat] U_X1; // the random effect for X1
  vector[n_groups_X_cat] U_X2; // the random effect for X2
  vector[n_groups_X_cat] U_X3; // the random effect for X3
  vector[n_groups_X4] U_X4; // the random effect for X4
  
  real<lower=0> sigma_X1; // sd of U_X1 (hyperparam). halfnormal prior on this.
  real<lower=0> sigma_X2; // sd of U_X2 (hyperparam). halfnormal prior on this.
  real<lower=0> sigma_X3; // sd of U_X3 (hyperparam). halfnormal prior on this.
  real<lower=0> sigma_X4; // sd of U_X4 (hyperparam). halfnormal prior on this.

  real intercept; // the intercept (global fixed effect)
}

transformed parameters { 
  vector[n_groups_X4] U_X4_transformed;
  vector[n_groups_X_cat] U_X1_transformed;
  vector[n_groups_X_cat] U_X2_transformed;
  vector[n_groups_X_cat] U_X3_transformed;

  vector[n] yhat;
  
  U_X1_transformed = sigma_X1 * U_X1; // the random effect for X1
  U_X2_transformed = sigma_X2 * U_X2; // the random effect for X2
  U_X3_transformed = sigma_X3 * U_X3; // the random effect for X3
  U_X4_transformed = sigma_X4 * U_X4; // the random effect for X4

  for (i in 1:n) {
    yhat[i] = intercept +
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
  for (i in 1:n) {
    y[i] ~ bernoulli(inv_logit(yhat[i])); 
  }
}

generated quantities {
  vector[Nj] theta_pop;
  for (j in 1:Nj)
    theta_pop[j] = bernoulli_rng(inv_logit(intercept +
    U_X1_transformed[X1_pop[j]] + 
    U_X2_transformed[X2_pop[j]] + 
    U_X3_transformed[X3_pop[j]] +
    U_X4_transformed[X4_pop[j]]));
}

