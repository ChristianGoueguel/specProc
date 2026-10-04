# Shared pieces of the robust calibration models rsimpls() and rpcr(): the
# robust regressions of the responses on the scores, the robust R2 and the
# table of the models with 1 to kmax components.

# The responses of a calibration model: a numeric matrix with column names,
# without missing values.
calibration_response <- function(y, n) {
  y <- as_response_matrix(y, n)
  if (anyNA(y)) {
    stop("'y' contains missing values.", call. = FALSE)
  }
  if (is.null(colnames(y))) {
    colnames(y) <- if (ncol(y) == 1) "y" else paste0("y", seq_len(ncol(y)))
  }
  y
}

# Residual distances of y from the regression `fit` (slopes, intercept and
# residual covariance sigma) on t.
residual_distance <- function(t, y, fit) {
  res <- y - sweep(t %*% fit$slopes, 2, fit$intercept, "+")
  sqrt(pmax(rowSums((res %*% solve(fit$sigma)) * res), 0))
}

# Least squares regression of y on t from the center and covariance of
# (t, y): slopes, intercept and residual covariance.
regression_from_moments <- function(center, cov, k) {
  it <- seq_len(k)
  iy <- k + seq_len(length(center) - k)
  slopes <- solve(cov[it, it, drop = FALSE], cov[it, iy, drop = FALSE])
  list(slopes = slopes,
       intercept = center[iy] - drop(crossprod(slopes, center[it])),
       sigma = cov[iy, iy, drop = FALSE] - crossprod(slopes, cov[it, it, drop = FALSE] %*% slopes))
}

# Reweighted MCD (robustbase::covMcd()) of the columns of z, with the weights
# of its reweighting step. covMcd() takes a determinant below exp(-50 p) as
# singular, which depends on the units: the columns are divided by a power
# of 2 close to their MAD (exactly, without rounding), and the estimates are
# scaled back.
scaled_mcd <- function(z, alpha, nsamp = 500) {
  s <- apply(z, 2, stats::mad)
  zero <- !is.finite(s) | s <= 0
  if (any(zero)) s[zero] <- apply(z[, zero, drop = FALSE], 2, stats::sd)
  s <- 2^round(log2(s))
  s[!is.finite(s) | s <= 0] <- 1
  mcd <- robustbase::covMcd(sweep(z, 2, s, "/"), alpha = alpha, nsamp = nsamp)
  list(center = unname(mcd$center) * s, cov = unname(as.matrix(mcd$cov)) * tcrossprod(s),
       weights = mcd$mcd.wt == 1)
}

# LTS regression (robustbase::ltsReg(), reweighted) of a single response y
# on t. The residual distance is the absolute standardized residual.
lts_regression <- function(t, y, alpha, nsamp) {
  fit <- robustbase::ltsReg(t, drop(y), alpha = alpha, nsamp = nsamp)
  coef <- unname(fit$coefficients)
  out <- list(slopes = matrix(coef[-1], ncol = 1), intercept = coef[1],
              sigma = matrix(fit$scale^2), weights = fit$lts.wt == 1)
  out$rd <- residual_distance(t, y, out)
  out
}

# MCD regression (Rousseeuw, Van Aelst, Van Driessen and Agullo, 2004) of y
# on t: least squares from the reweighted MCD of (t, y), then least squares
# on the observations whose residual distance is below sqrt(chi2_{q, 0.99})
# (regression reweighting), with the consistency factor of this cut-off.
mcd_regression <- function(t, y, alpha, nsamp) {
  k <- ncol(t)
  q <- ncol(y)
  z <- cbind(t, y)
  mcd <- scaled_mcd(z, alpha, nsamp)
  raw <- regression_from_moments(mcd$center, mcd$cov, k)
  cutoff <- stats::qchisq(0.99, q)
  keep <- residual_distance(t, y, raw)^2 <= cutoff
  zk <- z[keep, , drop = FALSE]
  out <- regression_from_moments(colMeans(zk), stats::cov(zk), k)
  out$sigma <- out$sigma * 0.99 / stats::pchisq(cutoff, q + 2)
  out$weights <- keep
  out$rd <- residual_distance(t, y, out)
  out
}

# The observations regular (residual distance below its cut-off) in every
# one of the models, or all of them if none is.
regular_in_all <- function(models) {
  regular <- Reduce(`&`, lapply(models, `[[`, "regular"))
  if (!any(regular)) regular <- rep(TRUE, length(regular))
  regular
}

# Robust R2 and RMSE of the models with 1 to kmax components, on the
# observations regular in every model (Remark 7 of Hubert and Vanden
# Branden, 2003).
components_table <- function(models, y, regular) {
  yr <- y[regular, , drop = FALSE]
  ss_tot <- sum(sweep(yr, 2, colMeans(yr))^2)
  tibble::tibble(
    ncomp = seq_along(models),
    R2 = vapply(models, function(m) 1 - sum(m$residuals[regular, ]^2) / ss_tot, numeric(1)),
    RMSE = vapply(models, function(m) sqrt(mean(m$residuals[regular, ]^2)), numeric(1))
  )
}

# Robust R2 of a model on the regular observations: one minus the ratio of
# the determinants of the residual and total sums of squares and
# cross-products (with one response, of the sums of squares).
model_r2 <- function(residuals, y, regular) {
  if (sum(regular) <= ncol(y)) regular <- rep(TRUE, nrow(y))
  yr <- y[regular, , drop = FALSE]
  1 - det(crossprod(residuals[regular, , drop = FALSE])) /
    det(crossprod(sweep(yr, 2, colMeans(yr))))
}

# Predictions of a robust calibration model (rsimpls() or rpcr()), with its
# number of components or another one (`ncomp`, from the `models` it keeps).
predict_calibration <- function(object, x, ncomp) {
  model <- if (is.null(ncomp)) object else object$models[[ncomp]]
  pred <- sweep(x %*% model$coefficients, 2, model$intercept, "+")
  if (ncol(pred) == 1) drop(pred) else pred
}

# Checks the `ncomp` of predict() for a robust calibration model.
check_predict_ncomp <- function(object, type, ncomp) {
  if (is.null(ncomp)) {
    return(invisible(NULL))
  }
  if (type != "response") {
    stop("'ncomp' is only used with type = \"response\".", call. = FALSE)
  }
  check_count(ncomp, "ncomp")
  if (ncomp > length(object$models)) {
    stop("'ncomp' cannot exceed ", length(object$models), ", the 'kmax' of the model.",
         call. = FALSE)
  }
  invisible(NULL)
}

# Print method of the robust calibration models.
print_calibration <- function(x, title) {
  cat(title, "\n\n", sep = "")
  cat("Observations:   ", length(x$sd), " (h = ", x$h, ")\n", sep = "")
  cat("Variables:      ", attr(x, "nvar"), "\n", sep = "")
  cat("Responses:      ", length(attr(x, "responses")), "\n", sep = "")
  cat("Components:     ", x$ncomp, " (robust R2 of 1 to ", x$kmax, " components: ",
      paste(format(x$components$R2, digits = 3), collapse = " "), ")\n", sep = "")
  cat("Robust R2:      ", format(x$R2, digits = 3), "\n", sep = "")
  if (length(x$intercept) == 1) {
    cat("Residual scale: ", format(sqrt(x$sigma[1, 1]), digits = 4), "\n", sep = "")
  }
  cat("\nOutlier types:\n")
  print(table(x$outlier_type))
  cat("\nOrthogonal outliers: ", sum(x$od > x$cutoff_od), "\n", sep = "")
  invisible(x)
}
