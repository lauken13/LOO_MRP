// The input data is a vector 'y' of length 'n'.
// full model with all covariates (model15)
data {
  int<lower=1> n; // number of samples
  int<lower=1> n_groups_X_cat; // the number of groups for X1, X2, X3

  int<lower=1,upper=n_groups_X_cat> X2[n]; // levels of X2

  // Assume no prediction of unobserved groups
  int<lower=0> J; // number of poststrat cells
  int<lower=1,upper=n_groups_X_cat> X2_pop[J]; // levels of X2

  int y[n]; // the response vector

}

parameters {
  vector[n_groups_X_cat] z_X2; // standardised random effect for X2

  real<lower=0> sigma_X2; // sd of z_X2 (hyperparam). halfnormal prior on this.

  real intercept; // the intercept (global fixed effect)
}

transformed parameters { 
  vector[n_groups_X_cat] U_X2;
  vector[n] yhat = intercept + rep_vector(0.0, n);   // initialize linear predictor term, intercept added here

  U_X2 = sigma_X2 * z_X2; // the random effect for X2

  // faster vectorisation (code from brms) 
  for (ind in 1:n) {
  // add more terms to the linear predictor
    yhat[ind] += U_X2[X2[ind]]; // intercept added in yhat vector before
  }
}

model {
  // priors
  target += normal_lpdf(sigma_X2|0,1)
  -1 * normal_lccdf(0|0,1);

  target += std_normal_lpdf(z_X2);

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
    U_X2[X2_pop[j]]);
  }
  
 for (i in 1:n){
    theta_samp[i] = inv_logit(intercept +
    U_X2[X2[i]]);
  }

   // calculating log likelihood for loo
  for (ind in 1:n){
    log_lik[ind] = bernoulli_logit_lpmf(y[ind] | yhat[ind]);
  }

}
