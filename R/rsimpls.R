#' @title Robust Partial Least Squares Regression (RSIMPLS)
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Robust PLS regression by the RSIMPLS algorithm of Hubert and Vanden
#' Branden (2003): the SIMPLS algorithm (de Jong, 1993) computed from a
#' robust covariance matrix of the predictors and the responses, followed by
#' a robust regression of the responses on the scores. Outlying spectra and
#' wrong reference values have little influence on the model, and the
#' regression outlier map ([plot_outlier_map()]) classifies them.
#'
#' @details
#' The algorithm follows the published description in three steps:
#' 1. **Robust covariance.** [robpca()] is applied to \eqn{Z = (X, Y)} with
#'    \eqn{k_0 = k_{max} + q} components, where \eqn{q} is the number of
#'    responses. It gives a robust center \eqn{\hat\mu_z} and a robust
#'    covariance matrix \eqn{\hat\Sigma_z = P L P^T}, of rank \eqn{k_0}.
#'    The observations that are regular in this fit (both distances below
#'    their cut-offs) get the weight \eqn{w_i = 1}, the others \eqn{w_i = 0}.
#' 2. **Robust scores.** The SIMPLS weight vectors \eqn{r_a} are computed
#'    from the blocks \eqn{\hat\Sigma_{xy}} and \eqn{\hat\Sigma_x} of
#'    \eqn{\hat\Sigma_z} instead of the empirical covariance matrices, and
#'    the scores are \eqn{t_{ia} = (x_i - \hat\mu_x)^T r_a}.
#' 3. **Robust regression.** The responses are regressed on the scores by
#'    least squares on the observations with \eqn{w_i = 1} (the ROBPCA
#'    regression of the paper). The observations whose residual distance
#'    \eqn{RD_i = \sqrt{r_i^T \hat\Sigma_e^{-1} r_i}} exceeds
#'    \eqn{\sqrt{\chi^2_{q, 0.975}}} are set aside, and the regression is
#'    recomputed by least squares on the others (reweighting).
#'
#' The regression coefficients are \eqn{B = R_k A_k}, from the weight
#' vectors \eqn{R_k} and the slopes \eqn{A_k} of the regression on the first
#' `ncomp` scores.
#'
#' **Diagnostics.** The score distance \eqn{SD_i} is the Mahalanobis
#' distance of the scores, with the center and covariance of the scores of
#' the observations with \eqn{w_i = 1}, and its cut-off is
#' \eqn{\sqrt{\chi^2_{k, 0.975}}}. The residual distance has the cut-off
#' \eqn{\sqrt{\chi^2_{q, 0.975}}}; with one response, it is the absolute
#' standardized residual. Observations beyond the score cut-off only are
#' **good leverage** points, beyond the residual cut-off only **vertical
#' outliers**, and beyond both **bad leverage** points.
#'
#' **Number of components.** `components` gives, for 1 to `kmax`
#' components, the robust \eqn{R^2} of the paper (Remark 7) and the root
#' mean squared error, on the observations that are regular in every one of
#' these models. They describe the fit to the calibration data; for
#' predictions, choose `ncomp` by cross-validation (for example with
#' tidymodels).
#'
#' **Differences from the paper.**
#'  - ROBPCA is not scale equivariant, so the scale of the responses
#'    relative to the spectra matters. Spectral intensities are usually
#'    much larger than concentrations, and a response on its own scale would
#'    play almost no part in the ROBPCA fit, which would then miss wrong
#'    reference values. The responses are therefore scaled before ROBPCA so
#'    that their total robust variance (the sum of the squared MADs, or
#'    standard deviations for variables with a zero MAD) equals that of the
#'    predictors (block scaling). The results are returned in
#'    the units of the responses.
#'  - The cut-off of the orthogonal distances is that of [robpca()] (the
#'    Wilson-Hilferty approximation of Hubert, Rousseeuw and Vanden Branden,
#'    2005), which the paper mentions as an alternative.
#'  - The residual covariance of the reweighted regression is multiplied by
#'    the consistency factor of the reweighted MCD,
#'    \eqn{0.975 / P(\chi^2_{q+2} \le \chi^2_{q, 0.975})}, as the observations
#'    beyond the cut-off are left out.
#'
#' This is an independent implementation of the published description.
#' [robpca()] uses random directions and random subsets, so use
#' [set.seed()] for reproducible results.
#'
#' @param x A numeric matrix or data frame of the predictors (spectra), one
#'   observation per row.
#' @param y A numeric vector, matrix or data frame of the responses, with
#'   one row per observation.
#' @param ncomp The number of components of the model.
#' @param kmax The largest number of components considered. ROBPCA is
#'   applied with `kmax` plus the number of responses components. Default
#'   is 10, as in the paper; it is raised to `ncomp` if needed, and lowered
#'   when there are too few observations or variables (at most one less than
#'   the number of variables).
#' @param alpha The robustness parameter of [robpca()]: the fraction of
#'   observations assumed to be regular, between 0.5 and 1. Default is 0.75.
#' @param ndir The number of random directions of the outlyingness in
#'   [robpca()]. Default is 250.
#' @param nsamp The number of random subsets of FAST-MCD in [robpca()].
#'   Default is 500.
#'
#' @return An object of class `specproc_rsimpls`, a list with:
#'  - `coefficients`, `intercept`: the regression coefficients (a
#'    \eqn{p \times q} matrix) and intercepts, in the units of the responses.
#'  - `x_weights`, `x_loadings`, `x_scores`: the weight vectors \eqn{R},
#'    the loadings \eqn{P} and the scores \eqn{T} of the `ncomp` components.
#'  - `y_loadings`: the slopes \eqn{A} of the regression of the responses on
#'    the scores (`ncomp` rows).
#'  - `center`, `y_center`: the robust centers of the predictors and
#'    responses.
#'  - `fitted`, `residuals`: the fitted values and residuals.
#'  - `sigma`: the robust covariance matrix of the residuals.
#'  - `sd`, `rd`: the score and residual distances of each observation, and
#'    `cutoff_sd`, `cutoff_rd` their cut-offs.
#'  - `outlier_type`: a factor classifying each observation as `"regular"`,
#'    `"good leverage"`, `"vertical outlier"` or `"bad leverage"`.
#'  - `weights`: 1 for the observations of the final regression (residual
#'    distance below the cut-off of the initial fit), 0 for the others.
#'  - `robpca_weights`: the weights \eqn{w_i} of the ROBPCA step.
#'  - `components`: a tibble with the robust `R2` and `RMSE` of the models
#'    with 1 to `kmax` components.
#'  - `ncomp`, `kmax`, `h`, `alpha`: the settings used, and `y_scale`, the
#'    factor applied to the responses before ROBPCA.
#'
#' Use [predict()][predict.specproc_rsimpls] for the predictions of new
#' observations.
#'
#' @references
#'  - Hubert, M., Vanden Branden, K. (2003). Robust methods for partial
#'    least squares regression. Journal of Chemometrics, 17(10):537-549.
#'  - de Jong, S. (1993). SIMPLS: an alternative approach to partial least
#'    squares regression. Chemometrics and Intelligent Laboratory Systems,
#'    18(3):251-263.
#'  - Hubert, M., Rousseeuw, P.J., Vanden Branden, K. (2005). ROBPCA: a new
#'    approach to robust principal component analysis. Technometrics,
#'    47(1):64-79.
#'
#' @seealso [predict.specproc_rsimpls()], [plot_outlier_map()], [robpca()]
#'
#' @export rsimpls
#'
#' @examples
#' data(forageLIBS)
#' wl <- suppressWarnings(as.numeric(names(forageLIBS)))
#' spectra <- forageLIBS[which(wl > 380 & wl < 430)]  # Ca II and Ca I lines
#' cal <- 1:300
#' set.seed(1)
#' fit <- rsimpls(spectra[cal, ], forageLIBS$Ca[cal], ncomp = 4)
#' fit
#' head(predict(fit, spectra[-cal, ]))
#' plot_outlier_map(fit)
rsimpls <- function(x, y, ncomp, kmax = 10, alpha = 0.75, ndir = 250, nsamp = 500) {
  if (missing(x) || missing(y) || is.null(x) || is.null(y)) {
    stop("Both 'x' and 'y' must be provided.", call. = FALSE)
  }
  if (missing(ncomp)) {
    stop("'ncomp', the number of components, must be provided.", call. = FALSE)
  }
  x <- robust_pca_input(x)
  y <- as_response_matrix(y, nrow(x))
  if (anyNA(y)) {
    stop("'y' contains missing values.", call. = FALSE)
  }
  if (is.null(colnames(y))) {
    colnames(y) <- if (ncol(y) == 1) "y" else paste0("y", seq_len(ncol(y)))
  }
  check_count(ncomp, "ncomp")
  check_count(kmax, "kmax")
  check_number(alpha, "alpha", lower = 0.5, upper = 1)
  check_count(nsamp, "nsamp")

  fit <- rsimpls_fit(x, y, max(kmax, ncomp), alpha, ndir, nsamp)
  if (ncomp > fit$kmax) {
    stop("'ncomp' cannot exceed ", fit$kmax, " with these data.", call. = FALSE)
  }
  model <- fit$models[[ncomp]]
  k <- seq_len(ncomp)
  comp <- paste0("Comp", k)
  res <- list(
    coefficients = model$coefficients,
    intercept = model$intercept,
    x_weights = fit$weights[, k, drop = FALSE],
    x_loadings = fit$loadings[, k, drop = FALSE],
    x_scores = fit$scores[, k, drop = FALSE],
    y_loadings = model$slopes,
    center = fit$center,
    y_center = fit$y_center,
    fitted = model$fitted,
    residuals = model$residuals,
    sigma = model$sigma,
    sd = model$sd,
    rd = model$rd,
    cutoff_sd = sqrt(stats::qchisq(0.975, ncomp)),
    cutoff_rd = sqrt(stats::qchisq(0.975, ncol(y))),
    outlier_type = NULL,
    weights = model$weights,
    robpca_weights = as.numeric(fit$w),
    components = fit$components,
    score_center = model$score_center,
    score_cov = model$score_cov,
    ncomp = as.integer(ncomp),
    kmax = fit$kmax,
    h = fit$h,
    alpha = alpha,
    y_scale = fit$y_scale
  )
  colnames(res$x_weights) <- colnames(res$x_loadings) <- colnames(res$x_scores) <- comp
  rownames(res$x_weights) <- rownames(res$x_loadings) <- rownames(res$coefficients) <- colnames(x)
  rownames(res$y_loadings) <- comp
  res$outlier_type <- regression_outlier_type(res$sd, res$rd, res$cutoff_sd, res$cutoff_rd)
  structure(res, variables = colnames(x), nvar = ncol(x), responses = colnames(y),
            class = "specproc_rsimpls")
}

# RSIMPLS with 1 to kmax components. x and y are numeric matrices (y with
# column names). Returns the weights, loadings and scores of kmax
# components, the regression of each model (models[[k]]), the ROBPCA weights
# w and the robust R2 and RMSE of each model.
rsimpls_fit <- function(x, y, kmax, alpha, ndir, nsamp) {
  n <- nrow(x)
  p <- ncol(x)
  q <- ncol(y)
  # ROBPCA needs fewer components (k0 = kmax + q) than the dimension of
  # (X, Y), and (34) of the paper
  kmax <- min(kmax, p - 1, n - 2 - q)
  while (kmax >= 1 && kmax * q + q + q * (q - 1) / 2 >= robust_h(n, alpha, kmax + q)) {
    kmax <- kmax - 1
  }
  if (kmax < 1) {
    stop("Too few observations for a robust PLS model.", call. = FALSE)
  }

  # block scaling of the responses (see Details)
  y_scale <- sqrt(total_variance(x) / total_variance(y))
  if (!is.finite(y_scale) || y_scale <= 0) {
    stop("'y' has no variation.", call. = FALSE)
  }
  ys <- y * y_scale

  # Step 1: ROBPCA of (X, Y)
  k0 <- kmax + q
  rob <- withCallingHandlers(
    robpca(cbind(x, ys), k = k0, kmax = k0, alpha = alpha, ndir = ndir, nsamp = nsamp),
    warning = function(w) {
      if (grepl("'k' reduced", conditionMessage(w))) invokeRestart("muffleWarning")
    }
  )
  kmax <- min(kmax, rob$k - q)
  if (kmax < 1) {
    stop("The predictors and responses span too few dimensions for a robust PLS model.", call. = FALSE)
  }
  w <- rob$outlier_type == "regular"
  ix <- seq_len(p)
  iy <- p + seq_len(q)
  mu_x <- rob$center[ix]
  mu_y <- rob$center[iy]

  # Step 2: SIMPLS from the robust covariance matrix
  pls <- simpls_robust(rob$loadings[ix, , drop = FALSE], rob$loadings[iy, , drop = FALSE],
                       rob$eigenvalues, kmax)
  kmax <- ncol(pls$weights)
  scores <- sweep(x, 2, mu_x) %*% pls$weights

  # Step 3: robust regression for each number of components
  models <- lapply(seq_len(kmax), function(k) {
    reg <- robpca_regression(scores[, seq_len(k), drop = FALSE], ys, w)
    b <- pls$weights[, seq_len(k), drop = FALSE] %*% reg$slopes
    colnames(b) <- colnames(y)
    intercept <- stats::setNames(drop(reg$intercept - crossprod(b, mu_x)), colnames(y))
    fitted <- sweep(x %*% b, 2, intercept, "+")
    list(
      coefficients = b / y_scale,
      intercept = intercept / y_scale,
      slopes = reg$slopes / y_scale,
      fitted = fitted / y_scale,
      residuals = (ys - fitted) / y_scale,
      sigma = reg$sigma / y_scale^2,
      sd = reg$sd,
      rd = reg$rd,
      weights = as.numeric(reg$keep),
      regular = reg$rd <= sqrt(stats::qchisq(0.975, q)),
      score_center = reg$score_center,
      score_cov = reg$score_cov
    )
  })

  # robust R2 and RMSE on the observations regular in every model (Remark 7)
  regular <- Reduce(`&`, lapply(models, `[[`, "regular"))
  if (!any(regular)) regular <- rep(TRUE, n)
  yr <- y[regular, , drop = FALSE]
  ss_tot <- sum(sweep(yr, 2, colMeans(yr))^2)
  components <- tibble::tibble(
    ncomp = seq_len(kmax),
    R2 = vapply(models, function(m) 1 - sum(m$residuals[regular, ]^2) / ss_tot, numeric(1)),
    RMSE = vapply(models, function(m) sqrt(mean(m$residuals[regular, ]^2)), numeric(1))
  )

  list(weights = pls$weights, loadings = pls$loadings, scores = scores, models = models,
       center = mu_x, y_center = stats::setNames(mu_y / y_scale, colnames(y)), w = w,
       components = components, kmax = kmax, h = rob$h, y_scale = y_scale)
}

# Sum of the squared column MADs (consistent at the normal), with the
# standard deviation for columns whose MAD is zero.
total_variance <- function(x) {
  s <- 1.4826 * col_medians(abs(sweep(x, 2, col_medians(x))))
  zero <- s <= 0
  if (any(zero)) s[zero] <- apply(x[, zero, drop = FALSE], 2, stats::sd)
  sum(s^2)
}

# SIMPLS weights and loadings from the robust covariance matrix
# P L P^T of (X, Y), given by the blocks px (p x k0) and py (q x k0) of P and
# the eigenvalues L. The cross-covariance S_xy = px L py^T is deflated by
# the orthonormal basis of the loadings. Stops early when S_xy vanishes.
simpls_robust <- function(px, py, values, kmax) {
  p <- nrow(px)
  s <- px %*% (values * t(py))
  s0 <- sqrt(sum(s^2))
  weights <- loadings <- basis <- matrix(0, p, kmax)
  for (a in seq_len(kmax)) {
    r <- if (ncol(s) == 1) {
      s[, 1]
    } else {
      drop(s %*% eigen(crossprod(s), symmetric = TRUE)$vectors[, 1])
    }
    norm_r <- sqrt(sum(r^2))
    if (norm_r <= 1e-10 * s0) {
      a <- a - 1
      break
    }
    r <- r / norm_r
    sx_r <- drop(px %*% (values * crossprod(px, r)))
    load <- sx_r / sum(r * sx_r)
    v <- load
    if (a > 1) {
      prev <- basis[, seq_len(a - 1), drop = FALSE]
      # twice, for numerical orthogonality
      v <- v - prev %*% crossprod(prev, v)
      v <- v - prev %*% crossprod(prev, v)
    }
    v <- v / sqrt(sum(v^2))
    s <- s - v %*% crossprod(v, s)
    weights[, a] <- r
    loadings[, a] <- load
    basis[, a] <- v
  }
  if (a < 1) {
    stop("The robust cross-covariance of the predictors and responses is zero.", call. = FALSE)
  }
  list(weights = weights[, seq_len(a), drop = FALSE], loadings = loadings[, seq_len(a), drop = FALSE])
}

# Regression of y on the scores t: least squares on the observations with
# weight w, then on those whose residual distance is below the cut-off
# (reweighting). The score distances use the center and covariance of the
# scores with weight w, as in the paper.
robpca_regression <- function(t, y, w) {
  q <- ncol(y)
  ls_fit <- function(rows) {
    tr <- t[rows, , drop = FALSE]
    yr <- y[rows, , drop = FALSE]
    st <- stats::cov(tr)
    slopes <- solve(st, stats::cov(tr, yr))
    list(slopes = slopes, intercept = colMeans(yr) - drop(crossprod(slopes, colMeans(tr))),
         sigma = stats::cov(yr) - crossprod(slopes, st %*% slopes),
         score_center = colMeans(tr), score_cov = st)
  }
  residual_distance <- function(fit) {
    res <- y - sweep(t %*% fit$slopes, 2, fit$intercept, "+")
    sqrt(pmax(rowSums((res %*% solve(fit$sigma)) * res), 0))
  }
  cutoff <- stats::qchisq(0.975, q)
  initial <- ls_fit(w)
  keep <- residual_distance(initial)^2 <= cutoff
  final <- ls_fit(keep)
  final$sigma <- final$sigma * 0.975 / stats::pchisq(cutoff, q + 2)
  list(
    slopes = final$slopes, intercept = final$intercept, sigma = final$sigma,
    rd = residual_distance(final),
    sd = sqrt(pmax(stats::mahalanobis(t, initial$score_center, initial$score_cov), 0)),
    keep = keep, score_center = initial$score_center, score_cov = initial$score_cov
  )
}

regression_outlier_type <- function(sd, rd, cutoff_sd, cutoff_rd) {
  high_sd <- sd > cutoff_sd
  high_rd <- rd > cutoff_rd
  type <- ifelse(high_sd & high_rd, "bad leverage",
                 ifelse(high_sd, "good leverage",
                        ifelse(high_rd, "vertical outlier", "regular")))
  factor(type, levels = c("regular", "good leverage", "vertical outlier", "bad leverage"))
}

#' @title Predictions of a Robust PLS Model
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Predicts the responses of new observations with a model fitted by
#' [rsimpls()], or computes their scores.
#'
#' @param object An object returned by [rsimpls()].
#' @param newdata A numeric matrix or data frame with the same variables as
#'   the calibration data.
#' @param type `"response"` (default) for the predicted responses, or
#'   `"scores"` for the scores and their score distances.
#' @param ... Not used.
#'
#' @return With `type = "response"`, a numeric vector of predictions (one
#'   response) or a matrix with one column per response. With
#'   `type = "scores"`, a tibble with the scores (`Comp1`, ...) and the score
#'   distance `sd` of each observation.
#'
#' @seealso [rsimpls()]
#' @export
#'
#' @examples
#' data(forageLIBS)
#' wl <- suppressWarnings(as.numeric(names(forageLIBS)))
#' spectra <- forageLIBS[which(wl > 380 & wl < 430)]
#' set.seed(1)
#' fit <- rsimpls(spectra[1:300, ], forageLIBS$Ca[1:300], ncomp = 4)
#' head(predict(fit, spectra[301:368, ]))
#' head(predict(fit, spectra[301:368, ], type = "scores"))
predict.specproc_rsimpls <- function(object, newdata, type = c("response", "scores"), ...) {
  type <- match.arg(type)
  x <- filter_newdata(object, newdata)
  if (anyNA(x)) {
    stop("'newdata' contains missing values.", call. = FALSE)
  }
  if (type == "response") {
    pred <- sweep(x %*% object$coefficients, 2, object$intercept, "+")
    return(if (ncol(pred) == 1) drop(pred) else pred)
  }
  scores <- sweep(x, 2, object$center) %*% object$x_weights
  out <- tibble::as_tibble(scores)
  out$sd <- sqrt(pmax(stats::mahalanobis(scores, object$score_center, object$score_cov), 0))
  out
}

#' @export
print.specproc_rsimpls <- function(x, ...) {
  cat("Robust PLS regression (RSIMPLS)\n\n")
  cat("Observations:   ", length(x$sd), " (h = ", x$h, ")\n", sep = "")
  cat("Variables:      ", attr(x, "nvar"), "\n", sep = "")
  cat("Responses:      ", length(attr(x, "responses")), "\n", sep = "")
  cat("Components:     ", x$ncomp, " (robust R2 of 1 to ", x$kmax, " components: ",
      paste(format(x$components$R2, digits = 3), collapse = " "), ")\n", sep = "")
  if (length(x$intercept) == 1) {
    cat("Residual scale: ", format(sqrt(x$sigma[1, 1]), digits = 4), "\n", sep = "")
  }
  cat("\nOutlier types:\n")
  print(table(x$outlier_type))
  invisible(x)
}
