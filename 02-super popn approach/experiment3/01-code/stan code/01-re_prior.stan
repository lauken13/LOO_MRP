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
  
  // // Assume no prediction of unobserved groups
  // int<lower=0> Nj; // number of poststrat cells
  // int<lower=1,upper=n_groups_X4> X4_pop[Nj]; // levels of X4
  // int<lower=1,upper=n_groups_X_cat> X1_pop[Nj]; // levels of X1
  // int<lower=1,upper=n_groups_X_cat> X2_pop[Nj]; // levels of X2
  // int<lower=1,upper=n_groups_X_cat> X3_pop[Nj]; // levels of X3

  int y[n]; // the response vector
  
  // adding these parameters to match brms
  int<lower=1> M_1;  // number of coefficients per level
  int<lower=1> M_2;  // number of coefficients per level
  int<lower=1> M_3;  // number of coefficients per level
  int<lower=1> M_4;  // number of coefficients per level

}

parameters {
  vector[n_groups_X_cat] z_X1[M_1]; // standardised random effect for X1
  vector[n_groups_X_cat] z_X2[M_2]; // standardised random effect for X2
  vector[n_groups_X_cat] z_X3[M_3]; // standardised random effect for X3
  vector[n_groups_X4] z_X4[M_4]; // standardised random effect for X4
  
  vector<lower=0>[M_1] sigma_X1; // sd of z_X1 (hyperparam). halfnormal prior on this.
  vector<lower=0>[M_2] sigma_X2; // sd of z_X2 (hyperparam). halfnormal prior on this.
  vector<lower=0>[M_3] sigma_X3; // sd of z_X3 (hyperparam). halfnormal prior on this.
  vector<lower=0>[M_4] sigma_X4; // sd of z_X4 (hyperparam). halfnormal prior on this.

  real intercept; // the intercept (global fixed effect)
}

transformed parameters { 
  vector[n_groups_X4] U_X4;
  vector[n_groups_X_cat] U_X1;
  vector[n_groups_X_cat] U_X2;
  vector[n_groups_X_cat] U_X3;

  vector[n] yhat;
  
  U_X1 = sigma_X1[1] * z_X1[1]; // the random effect for X1
  U_X2 = sigma_X2[1] * z_X2[1]; // the random effect for X2
  U_X3 = sigma_X3[1] * z_X3[1]; // the random effect for X3
  U_X4 = sigma_X4[1] * z_X4[1]; // the random effect for X4

  // for (i in 1:n) {
  //   yhat[i] = intercept +
  //   U_X1[X1[i]] + 
  //   U_X2[X2[i]] + 
  //   U_X3[X3[i]] +
  //   U_X4[X4[i]]; // the linear predictor at each point
  // }
}

model {
  // faster vectorisation (code from brms) 
  // initialize linear predictor term
  vector[n] yhat_star = intercept + rep_vector(0.0, n);
  
  for (ind in 1:n) {
  // add more terms to the linear predictor
    yhat_star[ind] += U_X1[X1[ind]] + U_X2[X2[ind]] + U_X3[X3[ind]] + U_X4[X4[ind]];
  }
  
  // priors
  target += normal_lpdf(sigma_X1|0,1)
  -1 * normal_lccdf(0|0,1);
  target += normal_lpdf(sigma_X2|0,1)
  -1 * normal_lccdf(0|0,1);
  target += normal_lpdf(sigma_X3|0,1)
  -1 * normal_lccdf(0|0,1);
  target += normal_lpdf(sigma_X4|0,1)
  -1 * normal_lccdf(0|0,1);
  
  target += std_normal_lpdf(z_X1[1]);
  target += std_normal_lpdf(z_X2[1]);
  target += std_normal_lpdf(z_X3[1]);
  target += std_normal_lpdf(z_X4[1]);
  
  target += normal_lpdf(intercept|0,1);// global intercept
  
  // model
    target += bernoulli_logit_lpmf(y|yhat_star); 
}

// generated quantities {
//   vector[Nj] theta_pop;
//   vector[n] log_lik;
//   
//   for (j in 1:Nj){
//     theta_pop[j] = bernoulli_rng(inv_logit(intercept +
//     U_X1[X1_pop[j]] +
//     U_X2[X2_pop[j]] +
//     U_X3[X3_pop[j]] +
//     U_X4[X4_pop[j]]));
//   }
//     
//    // calculating log likelihood for loo 
//   for (ind in 1:n){
//     log_lik[ind] = log(inv_logit(yhat[ind])) * y[ind] + log((1 - inv_logit(yhat[ind]))) * (1 - y[ind]);
//   }
// }


