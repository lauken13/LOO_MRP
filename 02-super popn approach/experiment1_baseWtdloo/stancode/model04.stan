// The input data is a vector 'y' of length 'n'.
// full model with all covariates (model15)
data {
  int<lower=1> n; // number of samples
  int<lower=1> n_groups_X_cat; // the number of groups for X4

  int<lower=1,upper=n_groups_X_cat> X4[n]; // levels of X4

  // Assume no prediction of unobserved groups
  int<lower=0> J; // number of poststrat cells
  int<lower=1,upper=n_groups_X_cat> X4_pop[J]; // levels of X4

  int y[n]; // the response vector

}

parameters {
  vector[n_groups_X_cat] z_X4; // standardised random effect for X4
  
  real<lower=0> sigma_X4; // sd of z_X4 (hyperparam). halfnormal prior on this.

  real intercept; // the intercept (global fixed effect)
}

transformed parameters { 
  vector[n_groups_X_cat] U_X4;
  vector[n] yhat = intercept + rep_vector(0.0, n);   // initialize linear predictor term, intercept added here

  U_X4 = sigma_X4 * z_X4; // the random effect for X4
  
  // faster vectorisation (code from brms) 
  for (ind in 1:n) {
  // add more terms to the linear predictor
    yhat[ind] += U_X4[X4[ind]]; // intercept added in yhat vector before
  }
}

model {
  // priors
  target += normal_lpdf(sigma_X4|0,1)
  -1 * normal_lccdf(0|0,1);
  
  target += std_normal_lpdf(z_X4);
  
  target += normal_lpdf(intercept|0,1);// global intercept
  
  // model
  target += bernoulli_logit_lpmf(y|yhat); 
}

generated quantities {
  vector[J] theta_pop; // prediction on popn. level
  vector[n] theta_samp; // prediction on samp. level
  vector[n] log_lik;

  for (j in 1:J){
    theta_pop[j] = inv_logit(intercept +
    U_X4[X4_pop[j]]);
  }
  
 for (i in 1:n){
    theta_samp[i] = inv_logit(intercept +
    U_X4[X4[i]]);
  }

   // calculating log likelihood for loo
  for (ind in 1:n){
    log_lik[ind] = bernoulli_logit_lpmf(y[ind] | yhat[ind]);
  }

}
