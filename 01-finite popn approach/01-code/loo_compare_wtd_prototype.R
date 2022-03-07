# load survey package
library(survey)

# computing differences in elpd_loo 
elpd_diffs <- function(loo_a, loo_b) {
  pt_a <- loo_a$pointwise
  pt_b <- loo_b$pointwise
  elpd <- grep("^elpd", colnames(pt_a))
  pt_b[, elpd] - pt_a[, elpd]
}

# loos List of `"loo"` objects.
loo_compare_matrix <- function(loos){
  tmp <- sapply(loos, function(x) {
    est <- x$estimates
    setNames(c(est), nm = c(rownames(est), paste0("se_", rownames(est))))
  })
  colnames(tmp) <- find_model_names(loos)
  rnms <- rownames(tmp)
  comp <- tmp
  ord <- loo_compare_order(loos)
  comp <- t(comp)[ord, ]
  patts <- c("elpd", "p_", "^waic$|^looic$", "^se_waic$|^se_looic$")
  col_ord <- unlist(sapply(patts, function(p) grep(p, colnames(comp))),
                    use.names = FALSE)
  comp <- comp[, col_ord]
  comp
}

# Computes the order of loos for comparison
loo_compare_order <- function(loos){
  tmp <- sapply(loos, function(x) {
    est <- x$estimates
    setNames(c(est), nm = c(rownames(est), paste0("se_", rownames(est))))
  })
  colnames(tmp) <- find_model_names(loos)
  rnms <- rownames(tmp)
  ord <- order(tmp[grep("^elpd", rnms), ], decreasing = TRUE)
  ord
}

# Character vector of model names the same length as `x.`
find_model_names <- function(x) {
  stopifnot(is.list(x))
  out_names <- character(length(x))
  
  names1 <- names(x)
  names2 <- lapply(x, "attr", "model_name", exact = TRUE)
  names3 <- lapply(x, "[[", "model_name")
  names4 <- paste0("model", seq_along(x))
  
  for (j in seq_along(x)) {
    if (isTRUE(nzchar(names1[j]))) {
      out_names[j] <- names1[j]
    } else if (length(names2[[j]])) {
      out_names[j] <- names2[[j]]
    } else if (length(names3[[j]])) {
      out_names[j] <- names3[[j]]
    } else {
      out_names[j] <- names4[j]
    }
  }
  
  return(out_names)
}

loo_compare_checks <- function(loos) {
  ## errors
  if (length(loos) <= 1L) {
    stop("'loo_compare' requires at least two models.", call.=FALSE)
  }
  if (!all(sapply(loos, is.loo))) {
    stop("All inputs should have class 'loo'.", call.=FALSE)
  }
  
  Ns <- sapply(loos, function(x) nrow(x$pointwise))
  if (!all(Ns == Ns[1L])) {
    stop("Not all models have the same number of data points.", call.=FALSE)
  }
  
  ## warnings
  
  yhash <- lapply(loos, attr, which = "yhash")
  yhash_ok <- sapply(yhash, function(x) { # ok only if all yhash are same (all NULL is ok)
    isTRUE(all.equal(x, yhash[[1]]))
  })
  if (!all(yhash_ok)) {
    warning("Not all models have the same y variable. ('yhash' attributes do not match)",
            call. = FALSE)
  }
  
  if (all(sapply(loos, is.kfold))) {
    Ks <- unlist(lapply(loos, attr, which = "K"))
    if (!all(Ks == Ks[1])) {
      warning("Not all kfold objects have the same K value. ",
              "For a more accurate comparison use the same number of folds. ",
              call. = FALSE)
    }
  } else if (any(sapply(loos, is.kfold)) && any(sapply(loos, is.psis_loo))) {
    warning("Comparing LOO-CV to K-fold-CV. ",
            "For a more accurate comparison use the same number of folds ",
            "or loo for all models compared.",
            call. = FALSE)
  }
}

se_elpd_diff <- function(diffs) {
  N <- length(diffs)
  # As `elpd_diff` is defined as the sum of N independent components,
  # we can compute the standard error by using the standard deviation
  # of the N components and multiplying by `sqrt(N)`.
  sqrt(N) * sd(diffs)
}

loo_compare_wtd <- function(x, svydesign_obj, ...) {
  if (is.loo(x)) {
    dots <- list(...)
    loos <- c(list(x), dots)
  } else {
    if (!is.list(x) || !length(x)) {
      stop("'x' must be a list if not a 'loo' object.")
    }
    if (length(list(...))) {
      stop("If 'x' is a list then '...' should not be specified.")
    }
    loos <- x
  }
  
  # If subsampling is used
  if (any(sapply(loos, inherits, "psis_loo_ss"))) {
    return(loo_compare.psis_loo_ss_list(loos))
  }
  
  loo_compare_checks(loos)
  
  comp <- loo_compare_matrix(loos)
  ord <- loo_compare_order(loos)
  
  # compute elpd_diff and se_elpd_diff relative to best model
  rnms <- rownames(comp)
  diffs <- mapply(FUN = elpd_diffs, loos[ord[1]], loos[ord])
  elpd_diff <- apply(diffs, 2, function(x)svytotal(x, svydesign_obj))
  se_diff <- apply(diffs, 2, se_elpd_diff)
  comp <- cbind(elpd_diff = elpd_diff, se_diff = se_diff, comp)
  rownames(comp) <- rnms
  
  class(comp) <- c("compare.loo", class(comp))
  return(comp)
}


loo_compare_wtd_bf <- function(x, ...) {
  if (is.loo(x)) {
    dots <- list(...)
    loos <- c(list(x), dots)
  } else {
    if (!is.list(x) || !length(x)) {
      stop("'x' must be a list if not a 'loo' object.")
    }
    if (length(list(...))) {
      stop("If 'x' is a list then '...' should not be specified.")
    }
    loos <- x
  }
  
  # If subsampling is used
  if (any(sapply(loos, inherits, "psis_loo_ss"))) {
    return(loo_compare.psis_loo_ss_list(loos))
  }
  
  loo_compare_checks(loos)
  
  comp <- loo_compare_matrix(loos)
  ord <- loo_compare_order(loos)
  
  # compute elpd_diff and se_elpd_diff relative to best model
  rnms <- rownames(comp)
  diffs <- mapply(FUN = elpd_diffs, loos[ord[1]], loos[ord])
  elpd_diff <- apply(diffs, 2, function(x)svytotal(x, svy_rake))
  se_diff <- apply(diffs, 2, se_elpd_diff)
  comp <- cbind(elpd_diff = elpd_diff, se_diff = se_diff, comp)
  rownames(comp) <- rnms
  
  class(comp) <- c("compare.loo", class(comp))
  return(comp)
}
