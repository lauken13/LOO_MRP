> model13a = brm(bin_value ~ (1|X1) + (1|X3), data = samp_data,
+                autocor = cor_arma(~1|X4,p=1, cov=T),
+               backend = "cmdstanr",
+               family = bernoulli(link = "logit"), 
+               control = list(adapt_delta = 0.9))

Warning: 48 of 4000 (1.0%) transitions ended with a divergence.
This may indicate insufficient exploration of the posterior distribution.
Possible remedies include: 
  * Increasing adapt_delta closer to 1 (default is 0.8) 
  * Reparameterizing the model (e.g. using a non-centered parameterization)
  * Using informative or weakly informative prior distributions 

Warning messages:
1: Argument 'autocor' should be specified within the 'formula' argument. See ?brmsformula for help. 
2: Using 'cor_brms' objects for 'autocor' is deprecated. Please see ?cor_brms for details. 
> proc.time() - ptm
     user    system   elapsed 
10455.050    63.221  7179.493 


> stancode(model13a)
// generated with brms 2.16.0
functions {
  /* multi-normal log-PDF for time-series covariance structures 
   * assuming homogoneous variances
   * Args: 
   *   y: response vector 
   *   mu: mean parameter vector
   *   sigma: residual standard deviation
   *   chol_cor: cholesky factor of the correlation matrix
   *   se2: square of user defined standard errors 
   *     should be set to zero if none are defined 
   *   nobs: number of observations in each group 
   *   begin: the first observation in each group 
   *   end: the last observation in each group 
   * Returns: 
   *   sum of the log-PDF values of all observations 
   */ 
  real normal_time_hom_lpdf(vector y, vector mu, real sigma, matrix chol_cor, 
                            data vector se2, int[] nobs, int[] begin, int[] end) {
    int I = size(nobs);
    int has_se = max(se2) > 0;
    vector[I] lp; 
    for (i in 1:I) { 
      matrix[nobs[i], nobs[i]] L;
      L = sigma * chol_cor[1:nobs[i], 1:nobs[i]];
      if (has_se) {
        // need to add 'se' to the correlation matrix itself
        L = multiply_lower_tri_self_transpose(L);
        L += diag_matrix(se2[begin[i]:end[i]]);
        L = cholesky_decompose(L);
      }
      lp[i] = multi_normal_cholesky_lpdf(
        y[begin[i]:end[i]] | mu[begin[i]:end[i]], L
      );
    }                        
    return sum(lp); 
  }
  /* multi-normal log-PDF for time-series covariance structures 
   * assuming heterogenous variances
   * Args: 
   *   y: response vector 
   *   mu: mean parameter vector
   *   sigma: residual standard deviation vector
   *   chol_cor: cholesky factor of the correlation matrix
   *   se2: square of user defined standard errors 
   *     should be set to zero if none are defined 
   *   nobs: number of observations in each group 
   *   begin: the first observation in each group 
   *   end: the last observation in each group 
   * Returns: 
   *   sum of the log-PDF values of all observations 
   */ 
  real normal_time_het_lpdf(vector y, vector mu, vector sigma, matrix chol_cor, 
                            data vector se2, int[] nobs, int[] begin, int[] end) {
    int I = size(nobs);
    int has_se = max(se2) > 0;
    vector[I] lp; 
    for (i in 1:I) { 
      matrix[nobs[i], nobs[i]] L;
      L = diag_pre_multiply(sigma[begin[i]:end[i]], 
                            chol_cor[1:nobs[i], 1:nobs[i]]);
      if (has_se) {
        // need to add 'se' to the correlation matrix itself
        L = multiply_lower_tri_self_transpose(L);
        L += diag_matrix(se2[begin[i]:end[i]]);
        L = cholesky_decompose(L);
      }
      lp[i] = multi_normal_cholesky_lpdf(
        y[begin[i]:end[i]] | mu[begin[i]:end[i]], L
      );
    }                        
    return sum(lp); 
  }
  /* multi-student-t log-PDF for time-series covariance structures 
   * assuming homogoneous variances
   * Args: 
   *   y: response vector 
   *   nu: degrees of freedom parameter 
   *   mu: mean parameter vector
   *   sigma: scale parameter
   *   chol_cor: cholesky factor of the correlation matrix
   *   se2: square of user defined standard errors 
   *     should be set to zero if none are defined 
   *   nobs: number of observations in each group 
   *   begin: the first observation in each group 
   *   end: the last observation in each group 
   * Returns: 
   *   sum of the log-PDF values of all observations 
   */ 
  real student_t_time_hom_lpdf(vector y, real nu, vector mu, real sigma, 
                               matrix chol_cor, data vector se2, int[] nobs,
                               int[] begin, int[] end) { 
    int I = size(nobs);
    int has_se = max(se2) > 0;
    vector[I] lp; 
    for (i in 1:I) { 
      matrix[nobs[i], nobs[i]] Cov; 
      Cov = sigma * chol_cor[1:nobs[i], 1:nobs[i]];
      Cov = multiply_lower_tri_self_transpose(Cov);
      if (has_se) {
        Cov += diag_matrix(se2[begin[i]:end[i]]);
      }
      lp[i] = multi_student_t_lpdf(
        y[begin[i]:end[i]] | nu, mu[begin[i]:end[i]], Cov
      );
    }                        
    return sum(lp); 
  }
  /* multi-student-t log-PDF for time-series covariance structures 
   * assuming heterogenous variances
   * Args: 
   *   y: response vector 
   *   nu: degrees of freedom parameter 
   *   mu: mean parameter vector
   *   sigma: scale parameter vector
   *   chol_cor: cholesky factor of the correlation matrix
   *   se2: square of user defined standard errors 
   *     should be set to zero if none are defined 
   *   nobs: number of observations in each group 
   *   begin: the first observation in each group 
   *   end: the last observation in each group 
   * Returns: 
   *   sum of the log-PDF values of all observations 
   */ 
  real student_t_time_het_lpdf(vector y, real nu, vector mu, vector sigma, 
                               matrix chol_cor, data vector se2, int[] nobs, 
                               int[] begin, int[] end) { 
    int I = size(nobs);
    int has_se = max(se2) > 0;
    vector[I] lp; 
    for (i in 1:I) { 
      matrix[nobs[i], nobs[i]] Cov; 
      Cov = diag_pre_multiply(sigma[begin[i]:end[i]], 
                              chol_cor[1:nobs[i], 1:nobs[i]]);
      Cov = multiply_lower_tri_self_transpose(Cov);
      if (has_se) {
        Cov += diag_matrix(se2[begin[i]:end[i]]);
      }
      lp[i] = multi_student_t_lpdf(
        y[begin[i]:end[i]] | nu, mu[begin[i]:end[i]], Cov
      );
    }                        
    return sum(lp); 
  }
  /* scale and correlate time-series residuals 
   * Args: 
   *   zerr: standardized and independent residuals
   *   sderr: standard deviation of the residuals
   *   chol_cor: cholesky factor of the correlation matrix
   *   nobs: number of observations in each group 
   *   begin: the first observation in each group 
   *   end: the last observation in each group 
   * Returns: 
   *   vector of scaled and correlated residuals
   */ 
   vector scale_time_err(vector zerr, real sderr, matrix chol_cor, 
                         int[] nobs, int[] begin, int[] end) { 
     vector[rows(zerr)] err; 
     for (i in 1:size(nobs)) { 
       err[begin[i]:end[i]] = 
         sderr * chol_cor[1:nobs[i], 1:nobs[i]] * zerr[begin[i]:end[i]];
     }                        
     return err; 
   }
  /* compute the cholesky factor of an AR1 correlation matrix
   * Args: 
   *   ar: AR1 autocorrelation 
   *   nrows: number of rows of the covariance matrix 
   * Returns: 
   *   A nrows x nrows matrix 
   */ 
   matrix cholesky_cor_ar1(real ar, int nrows) { 
     matrix[nrows, nrows] mat; 
     vector[nrows - 1] gamma; 
     mat = diag_matrix(rep_vector(1, nrows)); 
     for (i in 2:nrows) { 
       gamma[i - 1] = pow(ar, i - 1); 
       for (j in 1:(i - 1)) { 
         mat[i, j] = gamma[i - j]; 
         mat[j, i] = gamma[i - j]; 
       } 
     } 
     return cholesky_decompose(mat ./ (1 - ar^2)); 
   }
  /* compute the cholesky factor of a MA1 correlation matrix
   * Args: 
   *   ma: MA1 autocorrelation 
   *   nrows: number of rows of the covariance matrix 
   * Returns: 
   *   A nrows x nrows MA1 covariance matrix 
   */ 
   matrix cholesky_cor_ma1(real ma, int nrows) { 
     matrix[nrows, nrows] mat; 
     mat = diag_matrix(rep_vector(1 + ma^2, nrows)); 
     if (nrows > 1) { 
       mat[1, 2] = ma; 
       for (i in 2:(nrows - 1)) { 
         mat[i, i - 1] = ma; 
         mat[i, i + 1] = ma; 
       } 
       mat[nrows, nrows - 1] = ma; 
     } 
     return cholesky_decompose(mat); 
   }
  /* compute the cholesky factor of an ARMA1 correlation matrix
   * Args: 
   *   ar: AR1 autocorrelation 
   *   ma: MA1 autocorrelation 
   *   nrows: number of rows of the covariance matrix 
   * Returns: 
   *   A nrows x nrows matrix 
   */ 
   matrix cholesky_cor_arma1(real ar, real ma, int nrows) { 
     matrix[nrows, nrows] mat; 
     vector[nrows] gamma; 
     mat = diag_matrix(rep_vector(1 + ma^2 + 2 * ar * ma, nrows)); 
     gamma[1] = (1 + ar * ma) * (ar + ma); 
     for (i in 2:nrows) { 
       gamma[i] = gamma[1] * pow(ar, i - 1); 
       for (j in 1:(i - 1)) { 
         mat[i, j] = gamma[i - j]; 
         mat[j, i] = gamma[i - j]; 
       } 
     } 
     return cholesky_decompose(mat ./ (1 - ar^2)); 
   }
  /* compute the cholesky factor of a compound symmetry correlation matrix
   * Args: 
   *   cosy: compound symmetry correlation
   *   nrows: number of rows of the covariance matrix 
   * Returns: 
   *   A nrows x nrows covariance matrix 
   */ 
   matrix cholesky_cor_cosy(real cosy, int nrows) { 
     matrix[nrows, nrows] mat; 
     mat = diag_matrix(rep_vector(1, nrows)); 
     for (i in 2:nrows) { 
       for (j in 1:(i - 1)) { 
         mat[i, j] = cosy; 
         mat[j, i] = mat[i, j];
       } 
     } 
     return cholesky_decompose(mat); 
   }
}
data {
  int<lower=1> N;  // total number of observations
  int Y[N];  // response variable
  // data needed for ARMA correlations
  int<lower=0> Kar;  // AR order
  int<lower=0> Kma;  // MA order
  // see the functions block for details
  int<lower=1> N_tg;
  int<lower=1> begin_tg[N_tg];
  int<lower=1> end_tg[N_tg];
  int<lower=1> nobs_tg[N_tg];
  // data for group-level effects of ID 1
  int<lower=1> N_1;  // number of grouping levels
  int<lower=1> M_1;  // number of coefficients per level
  int<lower=1> J_1[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_1_1;
  // data for group-level effects of ID 2
  int<lower=1> N_2;  // number of grouping levels
  int<lower=1> M_2;  // number of coefficients per level
  int<lower=1> J_2[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_2_1;
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
  int max_lag = max(Kar, Kma);
  int max_nobs_tg = max(nobs_tg);  // maximum dimension of the autocorrelation matrix
  // no known standard errors specified by the user
  vector[N] se2 = rep_vector(0.0, N);
}
parameters {
  real Intercept;  // temporary intercept for centered predictors
  vector[N] zerr;  // unscaled residuals
  real<lower=0> sderr;  // SD of residuals
  vector<lower=-1,upper=1>[Kar] ar;  // autoregressive coefficients
  vector<lower=0>[M_1] sd_1;  // group-level standard deviations
  vector[N_1] z_1[M_1];  // standardized group-level effects
  vector<lower=0>[M_2] sd_2;  // group-level standard deviations
  vector[N_2] z_2[M_2];  // standardized group-level effects
}
transformed parameters {
  vector[N] err;  // actual residuals
  // cholesky factor of the autocorrelation matrix
  matrix[max_nobs_tg, max_nobs_tg] chol_cor;
  vector[N_1] r_1_1;  // actual group-level effects
  vector[N_2] r_2_1;  // actual group-level effects
  // compute residual covariance matrix
  chol_cor = cholesky_cor_ar1(ar[1], max_nobs_tg);
  // compute correlated time-series residuals
  err = scale_time_err(zerr, sderr, chol_cor, nobs_tg, begin_tg, end_tg);
  r_1_1 = (sd_1[1] * (z_1[1]));
  r_2_1 = (sd_2[1] * (z_2[1]));
}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] mu = Intercept + rep_vector(0.0, N) + err;
    for (n in 1:N) {
      // add more terms to the linear predictor
      mu[n] += r_1_1[J_1[n]] * Z_1_1[n] + r_2_1[J_2[n]] * Z_2_1[n];
    }
    target += bernoulli_logit_lpmf(Y | mu);
  }
  // priors including constants
  target += student_t_lpdf(Intercept | 3, 0, 2.5);
  target += student_t_lpdf(sderr | 3, 0, 2.5);
  target += std_normal_lpdf(zerr);
  target += student_t_lpdf(sd_1 | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  target += std_normal_lpdf(z_1[1]);
  target += student_t_lpdf(sd_2 | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  target += std_normal_lpdf(z_2[1]);
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept;
}