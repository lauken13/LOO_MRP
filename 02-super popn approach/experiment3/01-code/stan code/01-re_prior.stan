// The input data is a vector 'y' of length 'n'.
// full model with all covariates (model15)
data {
  int<lower=1> n; // number of samples
  int<lower=1> n_groups_X_cat; // the number of groups for X1, X2, X3
  int<lower=1> n_groups_X4; // the number of groups for X4

  int<lower=1,upper=n_groups_X4> X4[n]; // levels of X4
  int<lower=1,upper=n_groups_X_cat> X1[n]; // levels of X1
  int<lower=1,upper=n_groups_X_cat> X2[n]; // levels of X2
  int<lower=1,upper=n_groups_X_cat> X3[n]; // levels of X3
  
  // Assume no prediction of unobserved groups
  int<lower=0> J; // number of poststrat cells
  int<lower=1,upper=n_groups_X4> X4_pop[J]; // levels of X4
  int<lower=1,upper=n_groups_X_cat> X1_pop[J]; // levels of X1
  int<lower=1,upper=n_groups_X_cat> X2_pop[J]; // levels of X2
  int<lower=1,upper=n_groups_X_cat> X3_pop[J]; // levels of X3

  int y[n]; // the response vector

}

parameters {
  vector[n_groups_X_cat] z_X1; // standardised random effect for X1
  vector[n_groups_X_cat] z_X2; // standardised random effect for X2
  vector[n_groups_X_cat] z_X3; // standardised random effect for X3
  vector[n_groups_X4] z_X4; // standardised random effect for X4
  
  real<lower=0> sigma_X1; // sd of z_X1 (hyperparam). halfnormal prior on this.
  real<lower=0> sigma_X2; // sd of z_X2 (hyperparam). halfnormal prior on this.
  real<lower=0> sigma_X3; // sd of z_X3 (hyperparam). halfnormal prior on this.
  real<lower=0> sigma_X4; // sd of z_X4 (hyperparam). halfnormal prior on this.

  real intercept; // the intercept (global fixed effect)
}

transformed parameters { 
  vector[n_groups_X4] U_X4;
  vector[n_groups_X_cat] U_X1;
  vector[n_groups_X_cat] U_X2;
  vector[n_groups_X_cat] U_X3;
  vector[n] yhat = intercept + rep_vector(0.0, n);   // initialize linear predictor term, intercept added here

  U_X1 = sigma_X1 * z_X1; // the random effect for X1
  U_X2 = sigma_X2 * z_X2; // the random effect for X2
  U_X3 = sigma_X3 * z_X3; // the random effect for X3
  U_X4 = sigma_X4 * z_X4; // the random effect for X4
  
  // faster vectorisation (code from brms) 
  for (ind in 1:n) {
  // add more terms to the linear predictor
    yhat[ind] += U_X1[X1[ind]] + U_X2[X2[ind]] + U_X3[X3[ind]] + U_X4[X4[ind]]; // intercept added before
  }
}

model {
  // priors
  target += normal_lpdf(sigma_X1|0,1)
  -1 * normal_lccdf(0|0,1);
  target += normal_lpdf(sigma_X2|0,1)
  -1 * normal_lccdf(0|0,1);
  target += normal_lpdf(sigma_X3|0,1)
  -1 * normal_lccdf(0|0,1);
  target += normal_lpdf(sigma_X4|0,1)
  -1 * normal_lccdf(0|0,1);
  
  target += std_normal_lpdf(z_X1);
  target += std_normal_lpdf(z_X2);
  target += std_normal_lpdf(z_X3);
  target += std_normal_lpdf(z_X4);
  
  target += normal_lpdf(intercept|0,1);// global intercept
  
  // model
  target += bernoulli_logit_lpmf(y|yhat); 
}

generated quantities {
  vector[J] theta_pop; // poststrat
  vector[n] log_lik;

  for (j in 1:J){
    theta_pop[j] = bernoulli_rng(inv_logit(intercept +
    U_X1[X1_pop[j]] +
    U_X2[X2_pop[j]] +
    U_X3[X3_pop[j]] +
    U_X4[X4_pop[j]]));
  }

   // calculating log likelihood for loo
  for (ind in 1:n){
    log_lik[ind] = bernoulli_logit_lpmf(y[ind] | yhat[ind]);
  }

}
