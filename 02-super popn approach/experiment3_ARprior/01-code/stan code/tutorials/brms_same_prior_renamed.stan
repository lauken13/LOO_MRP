// generated with brms 2.16.3
functions {
}
data {
  int<lower=1> n;  // total number of observations
  int y[n];  // response variable
  // data for group-level effects of ID 1
  int<lower=1> n_groups_X_cat;  // number of grouping levels
  int<lower=1> M_1;  // number of coefficients per level
  int<lower=1> X1[n];  // grouping indicator per observation
  // data for group-level effects of ID 2
  int<lower=1> M_2;  // number of coefficients per level
  int<lower=1> X2[n];  // grouping indicator per observation
  // data for group-level effects of ID 3
  int<lower=1> M_3;  // number of coefficients per level
  int<lower=1> X3[n];  // grouping indicator per observation
  // data for group-level effects of ID 4
  int<lower=1> n_groups_X4;  // number of grouping levels
  int<lower=1> M_4;  // number of coefficients per level
  int<lower=1> X4[n];  // grouping indicator per observation
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
}
parameters {
  real Intercept;  // temporary intercept for centered predictors
  vector<lower=0>[M_1] sigma_X1;  // group-level standard deviations
  vector[n_groups_X_cat] z_1[M_1];  // standardized group-level effects
  vector<lower=0>[M_2] sigma_X2;  // group-level standard deviations
  vector[n_groups_X_cat] z_2[M_2];  // standardized group-level effects
  vector<lower=0>[M_3] sigma_X3;  // group-level standard deviations
  vector[n_groups_X_cat] z_3[M_3];  // standardized group-level effects
  vector<lower=0>[M_4] sigma_X4;  // group-level standard deviations
  vector[n_groups_X4] z_4[M_4];  // standardized group-level effects
}
transformed parameters {
  vector[n_groups_X_cat] U_X1;  // actual group-level effects
  vector[n_groups_X_cat] U_X2;  // actual group-level effects
  vector[n_groups_X_cat] U_X3;  // actual group-level effects
  vector[n_groups_X4] U_X4;  // actual group-level effects
  U_X1 = (sigma_X1[1] * (z_1[1])); // sigma*standardised random effect
  U_X2 = (sigma_X2[1] * (z_2[1]));
  U_X3 = (sigma_X3[1] * (z_3[1]));
  U_X4 = (sigma_X4[1] * (z_4[1]));
}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[n] yhat = Intercept + rep_vector(0.0, n);
    for (ind in 1:n) {
      // add more terms to the linear predictor
      yhat[ind] += U_X1[X1[ind]] + U_X2[X2[ind]] + U_X3[X3[ind]] + U_X4[X4[ind]];
    }
    target += bernoulli_logit_lpmf(y | yhat);
  }
  // priors including constants
  target += normal_lpdf(Intercept | 0, 1);
  target += normal_lpdf(sigma_X1 | 0, 1)
    - 1 * normal_lccdf(0 | 0, 1);
  target += std_normal_lpdf(z_1[1]);
  target += normal_lpdf(sigma_X2 | 0, 1)
    - 1 * normal_lccdf(0 | 0, 1);
  target += std_normal_lpdf(z_2[1]);
  target += normal_lpdf(sigma_X3 | 0, 1)
    - 1 * normal_lccdf(0 | 0, 1);
  target += std_normal_lpdf(z_3[1]);
  target += normal_lpdf(sigma_X4 | 0, 1)
    - 1 * normal_lccdf(0 | 0, 1);
  target += std_normal_lpdf(z_4[1]);
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept;
}