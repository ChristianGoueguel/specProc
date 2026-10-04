#' @title Robust Cross-Validation of a Robust Calibration Model
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Robust root mean squared error of cross-validation (R-RMSECV) of the
#' robust PLS ([rsimpls()]) or robust PCR ([rpcr()]) models with 1 to
#' `kmax` components, and the robust component selection (RCS) criterion of
#' Engelen and Hubert (2005), to choose the number of components when the
#' calibration data contain outliers.
#'
#' @details
#' The squared prediction errors of outliers would dominate the usual
#' RMSECV. The robust RMSECV leaves them out: the model is first fitted to
#' all the observations, and only those regular in every model with 1 to
#' `kmax` components (residual distance below its cut-off) enter the error,
#' \deqn{R\text{-}RMSECV_k = \sqrt{\frac{1}{q \sum_i w_i} \sum_i w_i
#'   \|y_i - \hat y_{-i,k}\|^2},}
#' where \eqn{w_i} is 1 for these observations and 0 for the others, and
#' \eqn{\hat y_{-i,k}} is the prediction of observation \eqn{i} by the model
#' with \eqn{k} components fitted without it.
#'
#' The robust root mean squared error of the fit to the calibration data,
#' `RMSE` (the square root of the robust residual sum of squares, R-RSS),
#' decreases with the number of components, while the cross-validated error
#' eventually increases. The RCS criterion combines them,
#' \deqn{RCS_k = \sqrt{\gamma \, R\text{-}RMSECV_k^2 + (1 - \gamma) \,
#'   R\text{-}RSS_k},}
#' and its first local minimum, or the point after which it decreases
#' little, suggests the number of components. `gamma = 1` gives the robust
#' RMSECV, and `gamma = 0` the R-RSS.
#'
#' **Differences from the paper.** The paper computes the leave-one-out
#' errors with a fast approximation that updates the robust fits instead of
#' refitting them. Here, each fold is refitted. Leave-one-out
#' (`folds = nrow(x)`) is then slow, and K-fold cross-validation (default
#' 10 folds) is used instead.
#'
#' The folds are fitted in parallel when a parallel plan is set with
#' `future::plan()` (and the future.apply package is installed). The folds
#' and the fits are random: use [set.seed()] for reproducible results.
#'
#' @inheritParams rsimpls
#' @param method The model: `"rsimpls"` (default), robust PLS, or `"rpcr"`,
#'   robust PCR.
#' @param kmax The largest number of components. Default is 10.
#' @param folds The number of folds of the cross-validation, from 2 to the
#'   number of observations (leave-one-out). Default is 10.
#' @param gamma The weight of the cross-validated error in the RCS
#'   criterion, between 0 and 1. Default is 0.5.
#' @param nsamp The number of random subsets of FAST-MCD (and, for
#'   `"rpcr"`, of the LTS or MCD regression). Default is 500.
#' @param ... Not used.
#'
#' @return A tibble with one row per number of components: `ncomp`, the
#'   robust `R2` and `RMSE` of the fit to all the observations (as the
#'   `components` of [rsimpls()] and [rpcr()]), the robust `RMSECV` and the
#'   `RCS` criterion. The weights \eqn{w_i} are its `"weights"` attribute.
#'
#' @references
#'  - Engelen, S., Hubert, M. (2005). Fast model selection for robust
#'    calibration methods. Analytica Chimica Acta, 544(1-2):219-228.
#'
#' @seealso [rsimpls()], [rpcr()]
#' @export
#'
#' @examples
#' \donttest{
#' data(forageLIBS)
#' wl <- suppressWarnings(as.numeric(names(forageLIBS)))
#' spectra <- forageLIBS[which(wl > 380 & wl < 430)]
#' set.seed(1)
#' robust_rmsecv(spectra[1:300, ], forageLIBS$Ca[1:300], kmax = 8, folds = 5)
#' }
robust_rmsecv <- function(x, y, method = c("rsimpls", "rpcr"), kmax = 10, folds = 10,
                          gamma = 0.5, alpha = 0.75, ndir = 250, nsamp = 500, ...) {
  method <- match.arg(method)
  if (missing(x) || missing(y) || is.null(x) || is.null(y)) {
    stop("Both 'x' and 'y' must be provided.", call. = FALSE)
  }
  x <- robust_pca_input(x)
  y <- calibration_response(y, nrow(x))
  n <- nrow(x)
  check_count(kmax, "kmax")
  check_count(folds, "folds", lower = 2)
  if (folds > n) {
    stop("'folds' cannot exceed the number of observations (", n, ").", call. = FALSE)
  }
  check_number(gamma, "gamma", lower = 0, upper = 1)
  check_number(alpha, "alpha", lower = 0.5, upper = 1)
  check_count(nsamp, "nsamp")

  fit_fun <- switch(method, rsimpls = rsimpls_fit, rpcr = rpcr_fit)
  full <- fit_fun(x, y, kmax, alpha, ndir, nsamp)
  kmax <- full$kmax
  fold <- sample(rep_len(seq_len(folds), n))
  # predictions of each fold by the models fitted without it; a fold whose
  # fit has fewer components leaves the larger models without predictions
  preds <- parallel_lapply(seq_len(folds), function(f) {
    test <- fold == f
    fit <- fit_fun(x[!test, , drop = FALSE], y[!test, , drop = FALSE], kmax, alpha, ndir, nsamp)
    out <- array(NA_real_, c(sum(test), ncol(y), kmax))
    for (k in seq_len(min(kmax, fit$kmax))) {
      m <- fit$models[[k]]
      out[, , k] <- sweep(x[test, , drop = FALSE] %*% m$coefficients, 2, m$intercept, "+")
    }
    out
  }, seed = TRUE)
  errors <- array(NA_real_, c(n, ncol(y), kmax))
  for (f in seq_len(folds)) {
    test <- fold == f
    errors[test, , ] <- preds[[f]] - array(y[test, ], dim(preds[[f]]))
  }

  w <- full$regular
  rmsecv <- vapply(seq_len(kmax), function(k) sqrt(mean(errors[w, , k]^2)), numeric(1))
  res <- full$components
  res$RMSECV <- rmsecv
  res$RCS <- sqrt(gamma * rmsecv^2 + (1 - gamma) * res$RMSE^2)
  # attr<- rather than structure(), which makes tibble print row names
  attr(res, "weights") <- as.numeric(w)
  attr(res, "folds") <- folds
  attr(res, "gamma") <- gamma
  attr(res, "method") <- method
  res
}
