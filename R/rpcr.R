#' @title Robust Principal Component Regression (RPCR)
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Robust principal component regression by the RPCR method of Hubert and
#' Verboven (2003): the robust principal components of the predictors
#' ([robpca()]), followed by a robust regression of the responses on the
#' scores. Outlying spectra and wrong reference values have little influence
#' on the model, and the regression outlier map ([plot_outlier_map()])
#' classifies them.
#'
#' @details
#' The method has two steps:
#' 1. **Robust PCA.** [robpca()] is applied to the predictors with `kmax`
#'    components. It gives a robust center \eqn{\hat\mu_x}, the loadings
#'    \eqn{P} and the eigenvalues \eqn{l_a}, and the scores
#'    \eqn{t_i = P^T (x_i - \hat\mu_x)}.
#' 2. **Robust regression.** The responses are regressed on the first
#'    `ncomp` scores. With one response, this is the reweighted least
#'    trimmed squares (LTS) regression of [robustbase::ltsReg()]. With
#'    several responses, it is the MCD regression of Rousseeuw, Van Aelst,
#'    Van Driessen and Agullo (2004): least squares from the reweighted MCD
#'    of the scores and responses, then least squares on the observations
#'    whose residual distance is below \eqn{\sqrt{\chi^2_{q, 0.99}}}.
#'
#' The regression coefficients are \eqn{B = P_k A_k}, from the loadings
#' \eqn{P_k} and the slopes \eqn{A_k} of the regression on the first `ncomp`
#' scores.
#'
#' **Diagnostics.** As for [rsimpls()]: the score distance
#' \eqn{SD_i = \sqrt{\sum_a t_{ia}^2 / l_a}} has the cut-off
#' \eqn{\sqrt{\chi^2_{k, 0.975}}}, the residual distance (with one response,
#' the absolute standardized residual) the cut-off
#' \eqn{\sqrt{\chi^2_{q, 0.975}}}, and the orthogonal distance
#' \eqn{OD_i = \|x_i - \hat\mu_x - P t_i\|} the cut-off of [robpca()].
#' Observations beyond the score cut-off only are **good leverage** points,
#' beyond the residual cut-off only **vertical outliers**, and beyond both
#' **bad leverage** points. The robust \eqn{R^2} of the model (`R2`) is
#' computed on the observations whose orthogonal and residual distances are
#' both below their cut-offs.
#'
#' **Number of components.** `components` gives, for 1 to `kmax`
#' components, the robust \eqn{R^2} and root mean squared error on the
#' observations that are regular in every one of these models; use
#' [robust_rmsecv()] for the robust cross-validated error. These models all
#' come from the same robust PCA, with `kmax` components, of which they use
#' the first `ncomp`, so the model also depends on `kmax`. With
#' `kmax = ncomp`, the robust PCA has `ncomp` components, as in the paper.
#'
#' **Differences from the paper.**
#'  - The models with fewer components than `kmax` use the first components
#'    of the robust PCA with `kmax` components (see above).
#'  - The residual covariance of the MCD regression is multiplied by the
#'    consistency factor \eqn{0.99 / P(\chi^2_{q+2} \le \chi^2_{q, 0.99})}, as
#'    the observations beyond the cut-off are left out.
#'
#' This is an independent implementation of the published description.
#' [robpca()], the LTS and the MCD use random subsets, so use [set.seed()]
#' for reproducible results.
#'
#' @inheritParams rsimpls
#' @param kmax The largest number of components considered: the number of
#'   components of the robust PCA, so the model also depends on `kmax` (see
#'   Details). Default is 10; it is raised to `ncomp` if needed, and lowered
#'   when there are too few observations or variables.
#' @param alpha The fraction of observations assumed to be regular, between
#'   0.5 and 1, for [robpca()] and the robust regression. Default is 0.75.
#' @param nsamp The number of random subsets of FAST-MCD in [robpca()], and
#'   of the LTS or MCD regression. Default is 500.
#'
#' @return An object of class `specproc_rpcr`, a list with:
#'  - `coefficients`, `intercept`: the regression coefficients (a
#'    \eqn{p \times q} matrix) and intercepts.
#'  - `x_loadings`, `x_scores`, `eigenvalues`: the loadings \eqn{P}, scores
#'    \eqn{T} and eigenvalues of the `ncomp` components.
#'  - `y_loadings`: the slopes \eqn{A} of the regression of the responses on
#'    the scores (`ncomp` rows).
#'  - `center`: the robust center of the predictors.
#'  - `fitted`, `residuals`: the fitted values and residuals.
#'  - `sigma`: the robust covariance matrix of the residuals.
#'  - `sd`, `rd`, `od`: the score, residual and orthogonal distances of
#'    each observation, and `cutoff_sd`, `cutoff_rd`, `cutoff_od` their
#'    cut-offs.
#'  - `R2`: the robust \eqn{R^2} of the model (see Details).
#'  - `outlier_type`: a factor classifying each observation as `"regular"`,
#'    `"good leverage"`, `"vertical outlier"` or `"bad leverage"`.
#'  - `weights`: 1 for the observations of the final (reweighted)
#'    regression, 0 for the others.
#'  - `components`: a tibble with the robust `R2` and `RMSE` of the models
#'    with 1 to `kmax` components.
#'  - `models`: the `coefficients` and `intercept` of the models with 1 to
#'    `kmax` components, for predictions with another number of components
#'    (`predict(fit, newdata, ncomp = )`).
#'  - `ncomp`, `kmax`, `h`, `alpha`: the settings used, and `regression`,
#'    `"LTS"` or `"MCD"`.
#'
#' Use [predict()][predict.specproc_rpcr] for the predictions of new
#' observations.
#'
#' @references
#'  - Hubert, M., Verboven, S. (2003). A robust PCR method for
#'    high-dimensional regressors. Journal of Chemometrics, 17(8-9):438-452.
#'  - Rousseeuw, P.J., Van Aelst, S., Van Driessen, K., Agullo, J. (2004).
#'    Robust multivariate regression. Technometrics, 46(3):293-305.
#'  - Hubert, M., Rousseeuw, P.J., Vanden Branden, K. (2005). ROBPCA: a new
#'    approach to robust principal component analysis. Technometrics,
#'    47(1):64-79.
#'
#' @seealso [predict.specproc_rpcr()], [plot_outlier_map()],
#'   [robust_rmsecv()], [rsimpls()], [robpca()]
#'
#' @export
#'
#' @examples
#' data(forageLIBS)
#' wl <- suppressWarnings(as.numeric(names(forageLIBS)))
#' spectra <- forageLIBS[which(wl > 380 & wl < 430)]  # Ca II and Ca I lines
#' cal <- 1:300
#' set.seed(1)
#' fit <- rpcr(spectra[cal, ], forageLIBS$Ca[cal], ncomp = 5)
#' fit
#' head(predict(fit, spectra[-cal, ]))
#' plot_outlier_map(fit)
rpcr <- function(x, y, ncomp, kmax = 10, alpha = 0.75, ndir = 250, nsamp = 500) {
  if (missing(x) || missing(y) || is.null(x) || is.null(y)) {
    stop("Both 'x' and 'y' must be provided.", call. = FALSE)
  }
  if (missing(ncomp)) {
    stop("'ncomp', the number of components, must be provided.", call. = FALSE)
  }
  x <- robust_pca_input(x)
  y <- calibration_response(y, nrow(x))
  check_count(ncomp, "ncomp")
  check_count(kmax, "kmax")
  check_number(alpha, "alpha", lower = 0.5, upper = 1)
  check_count(nsamp, "nsamp")

  fit <- rpcr_fit(x, y, max(kmax, ncomp), alpha, ndir, nsamp)
  if (ncomp > fit$kmax) {
    stop("'ncomp' cannot exceed ", fit$kmax, " with these data.", call. = FALSE)
  }
  model <- fit$models[[ncomp]]
  k <- seq_len(ncomp)
  comp <- paste0("Comp", k)
  cutoff_rd <- sqrt(stats::qchisq(0.975, ncol(y)))
  od <- pls_od(sweep(x, 2, fit$center), fit$scores[, k, drop = FALSE],
               fit$loadings[, k, drop = FALSE])
  cutoff_od <- od_cutoff(od, fit$h)
  res <- list(
    coefficients = model$coefficients,
    intercept = model$intercept,
    x_loadings = fit$loadings[, k, drop = FALSE],
    x_scores = fit$scores[, k, drop = FALSE],
    eigenvalues = fit$eigenvalues[k],
    y_loadings = model$slopes,
    center = fit$center,
    fitted = model$fitted,
    residuals = model$residuals,
    sigma = model$sigma,
    sd = model$sd,
    rd = model$rd,
    od = od,
    cutoff_sd = sqrt(stats::qchisq(0.975, ncomp)),
    cutoff_rd = cutoff_rd,
    cutoff_od = cutoff_od,
    outlier_type = NULL,
    weights = model$weights,
    R2 = model_r2(model$residuals, y, od <= cutoff_od & model$rd <= cutoff_rd),
    components = fit$components,
    models = lapply(fit$models, `[`, c("coefficients", "intercept")),
    ncomp = as.integer(ncomp),
    kmax = fit$kmax,
    h = fit$h,
    alpha = alpha,
    regression = if (ncol(y) == 1) "LTS" else "MCD"
  )
  colnames(res$x_loadings) <- colnames(res$x_scores) <- comp
  rownames(res$x_loadings) <- rownames(res$coefficients) <- colnames(x)
  dimnames(res$y_loadings) <- list(comp, colnames(y))
  dimnames(res$sigma) <- list(colnames(y), colnames(y))
  res$outlier_type <- regression_outlier_type(res$sd, res$rd, res$cutoff_sd, res$cutoff_rd)
  structure(res, variables = colnames(x), nvar = ncol(x), responses = colnames(y),
            class = "specproc_rpcr")
}

# RPCR with 1 to kmax components, from the robust PCA with kmax components.
# x and y are numeric matrices (y with column names). Returns the loadings,
# scores and eigenvalues of kmax components, the regression of each model
# (models[[k]]), the observations regular in every model and the robust R2
# and RMSE of each model.
rpcr_fit <- function(x, y, kmax, alpha, ndir, nsamp) {
  n <- nrow(x)
  q <- ncol(y)
  # fewer components than variables and observations, and fewer parameters
  # of the robust regression (k q slopes, q intercepts and q (q + 1) / 2
  # residual covariances) than h
  kmax <- min(kmax, ncol(x), n - 2 - q)
  while (kmax >= 1 && kmax * q + q + q * (q + 1) / 2 >= robust_h(n, alpha, kmax)) {
    kmax <- kmax - 1
  }
  if (kmax < 1) {
    stop("Too few observations for a robust PCR model.", call. = FALSE)
  }
  rob <- withCallingHandlers(
    robpca(x, k = kmax, kmax = kmax, alpha = alpha, ndir = ndir, nsamp = nsamp),
    warning = function(w) {
      if (grepl("'k' reduced", conditionMessage(w))) invokeRestart("muffleWarning")
    }
  )
  kmax <- rob$k
  scores <- unname(rob$scores)
  loadings <- unname(rob$loadings)
  values <- rob$eigenvalues
  models <- lapply(seq_len(kmax), function(k) {
    a <- seq_len(k)
    t <- scores[, a, drop = FALSE]
    reg <- if (q == 1) lts_regression(t, y, alpha, nsamp) else mcd_regression(t, y, alpha, nsamp)
    b <- loadings[, a, drop = FALSE] %*% reg$slopes
    colnames(b) <- colnames(y)
    intercept <- stats::setNames(drop(reg$intercept - crossprod(b, rob$center)), colnames(y))
    fitted <- sweep(x %*% b, 2, intercept, "+")
    list(
      coefficients = b,
      intercept = intercept,
      slopes = reg$slopes,
      fitted = fitted,
      residuals = y - fitted,
      sigma = reg$sigma,
      sd = sqrt(rowSums(sweep(t^2, 2, values[a], "/"))),
      rd = reg$rd,
      weights = as.numeric(reg$weights),
      regular = reg$rd <= sqrt(stats::qchisq(0.975, q))
    )
  })
  regular <- regular_in_all(models)
  list(loadings = loadings, scores = scores, eigenvalues = values, models = models,
       center = rob$center, regular = regular, components = components_table(models, y, regular),
       kmax = kmax, h = rob$h)
}

#' @title Predictions of a Robust PCR Model
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Predicts the responses of new observations with a model fitted by
#' [rpcr()], or computes their scores.
#'
#' @inheritParams predict.specproc_rsimpls
#' @param object An object returned by [rpcr()].
#' @param ncomp With `type = "response"`, the number of components of the
#'   predictions, from 1 to the `kmax` of the model. Default is the `ncomp`
#'   of the model. The models with fewer or more components use the same
#'   robust PCA (see [rpcr()]).
#'
#' @return With `type = "response"`, a numeric vector of predictions (one
#'   response) or a matrix with one column per response. With
#'   `type = "scores"`, a tibble with the scores (`Comp1`, ...), the score
#'   distance `sd` and the orthogonal distance `od` of each observation.
#'
#' @seealso [rpcr()]
#' @export
#'
#' @examples
#' data(forageLIBS)
#' wl <- suppressWarnings(as.numeric(names(forageLIBS)))
#' spectra <- forageLIBS[which(wl > 380 & wl < 430)]
#' set.seed(1)
#' fit <- rpcr(spectra[1:300, ], forageLIBS$Ca[1:300], ncomp = 5)
#' head(predict(fit, spectra[301:368, ]))
#' head(predict(fit, spectra[301:368, ], type = "scores"))
predict.specproc_rpcr <- function(object, newdata, type = c("response", "scores"),
                                  ncomp = NULL, ...) {
  type <- match.arg(type)
  check_predict_ncomp(object, type, ncomp)
  x <- filter_newdata(object, newdata)
  if (anyNA(x)) {
    stop("'newdata' contains missing values.", call. = FALSE)
  }
  if (type == "response") {
    return(predict_calibration(object, x, ncomp))
  }
  xc <- sweep(x, 2, object$center)
  scores <- xc %*% object$x_loadings
  out <- tibble::as_tibble(scores)
  out$sd <- sqrt(rowSums(sweep(scores^2, 2, object$eigenvalues, "/")))
  out$od <- pls_od(xc, scores, object$x_loadings)
  out
}

#' @export
print.specproc_rpcr <- function(x, ...) {
  print_calibration(x, paste0("Robust PCR (RPCR, ", x$regression, " regression)"))
}
