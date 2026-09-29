#' @title Robust Principal Component Analysis (ROBPCA)
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Robust PCA by the ROBPCA algorithm of Hubert, Rousseeuw and Vanden
#' Branden (2005), which combines projection pursuit with the minimum
#' covariance determinant (MCD) estimator. It resists up to `n - h` outlying
#' spectra and works when there are more variables than observations. The
#' computational kernels (outlyingness, FAST-MCD) are written in C++.
#'
#' @details
#' The algorithm follows the published description:
#' 1. The data are reduced by a singular value decomposition to the affine
#'    subspace they span (at most \eqn{n - 1} dimensions).
#' 2. The Stahel-Donoho outlyingness of every observation is computed over
#'    directions through pairs of observations, using the univariate MCD
#'    location and scale on each direction. The `h` least outlying
#'    observations form the subset \eqn{H_0}.
#' 3. The number of components `k` is chosen (if not given) as the smallest
#'    number that explains `var_explained` of the variance of \eqn{H_0}, with
#'    at most `kmax`. Observations whose orthogonal distance to the
#'    \eqn{k}-dimensional PCA subspace of \eqn{H_0} is below the cut-off form
#'    \eqn{H_1}, and the subspace is re-estimated from \eqn{H_1}.
#' 4. All observations are projected onto this subspace, and the reweighted
#'    FAST-MCD estimator of the scores gives the final center, loadings and
#'    eigenvalues.
#'
#' Each observation then has a score distance (SD), its robust Mahalanobis
#' distance within the PCA subspace, and an orthogonal distance (OD) to the
#' subspace. The cut-off for the SD is \eqn{\sqrt{\chi^2_{k,0.975}}}; the
#' cut-off for the OD uses the Wilson-Hilferty approximation, with the
#' univariate MCD of \eqn{OD^{2/3}}. [plot_outlier_map()] plots both.
#'
#' This is an independent implementation of the published algorithm, not a
#' port of the rospca or rrcov code, so its results can differ slightly from
#' theirs (random directions and subsets, and small-sample correction
#' factors, which are not applied here). Use [set.seed()] for reproducible
#' results.
#'
#' @param x A numeric matrix or data frame, with one observation per row.
#' @param k The number of principal components. If `NULL` (default), it is
#'   chosen from `var_explained` and `kmax`.
#' @param kmax The maximum number of components. Default is 10.
#' @param alpha The robustness parameter: the fraction of observations the
#'   estimates are based on, between 0.5 and 1. Default is 0.75. The subset
#'   size is \eqn{h = \max(\lfloor\alpha n\rfloor, \lfloor(n + k_{max} + 1)/2\rfloor)}.
#' @param ndir The number of random directions used for the outlyingness.
#'   Default is 250; use `"all"` for all directions through pairs of
#'   observations.
#' @param var_explained The fraction of variance used to choose `k` when it
#'   is not given. Default is 0.8.
#' @param nsamp The number of random subsets of FAST-MCD. Default is 500.
#'
#' @return An object of class `specproc_robpca`, a list with:
#'  - `loadings`: \eqn{p \times k} matrix of robust loadings.
#'  - `eigenvalues`: robust eigenvalues (variances of the scores).
#'  - `center`: robust center.
#'  - `scores`: \eqn{n \times k} matrix of scores.
#'  - `sd`, `od`: score and orthogonal distances of each observation.
#'  - `cutoff_sd`, `cutoff_od`: their cut-offs.
#'  - `outlier_type`: a factor classifying each observation as `"regular"`,
#'    `"good leverage"` (high SD only), `"orthogonal outlier"` (high OD only)
#'    or `"bad leverage"` (both).
#'  - `k`, `h`, `alpha`: the settings used.
#'  - `H0`, `H1`: logical vectors of the observations in the subsets
#'    \eqn{H_0} and \eqn{H_1}.
#'
#' Use [predict()][predict.specproc_robpca] for the scores and distances of
#' new observations.
#'
#' @references
#'  - Hubert, M., Rousseeuw, P.J., Vanden Branden, K. (2005). ROBPCA: a new
#'    approach to robust principal component analysis. Technometrics,
#'    47(1):64-79.
#'  - Rousseeuw, P.J., Van Driessen, K. (1999). A fast algorithm for the
#'    minimum covariance determinant estimator. Technometrics, 41(3):212-223.
#'
#' @seealso [rospca()] for sparse loadings, [macropca()] for data with
#'   cellwise outliers or missing values, [step_robpca()] for a recipe step,
#'   [plot_outlier_map()].
#'
#' @export robpca
#'
#' @examples
#' set.seed(1)
#' x <- matrix(rnorm(100 * 10), 100, 10) %*% diag(10:1)
#' x[1:5, ] <- x[1:5, ] + 30  # outliers
#' fit <- robpca(x, k = 2)
#' fit
#' table(fit$outlier_type)
#'
robpca <- function(x, k = NULL, kmax = 10, alpha = 0.75, ndir = 250, var_explained = 0.8, nsamp = 500) {
  x <- robust_pca_input(x)
  n <- nrow(x)
  check_robust_args(k, kmax, alpha, var_explained)

  red <- svd_reduce(x)
  z <- red$z
  r <- ncol(z)
  kmax <- min(kmax, r, n - 1)
  if (!is.null(k) && k > kmax) {
    warning("'k' reduced to ", kmax, ".", call. = FALSE)
    k <- kmax
  }
  h <- robust_h(n, alpha, kmax)

  # Step 1: h least outlying observations
  H0 <- least_outlying(z, h, ndir)
  e0 <- eigen(stats::cov(z[H0, , drop = FALSE]), symmetric = TRUE)
  if (is.null(k)) {
    k <- choose_k(e0$values, kmax, var_explained)
  }

  # Step 2: reweighting on the orthogonal distances to the H0 subspace
  m0 <- colMeans(z[H0, , drop = FALSE])
  od0 <- orthogonal_distance(sweep(z, 2, m0), e0$vectors[, seq_len(k), drop = FALSE])
  H1 <- od0 <= od_cutoff(od0, h)
  m1 <- colMeans(z[H1, , drop = FALSE])
  p1 <- eigen(stats::cov(z[H1, , drop = FALSE]), symmetric = TRUE)$vectors[, seq_len(k), drop = FALSE]

  # Step 3: MCD of the scores in the k-dimensional subspace
  t1 <- sweep(z, 2, m1) %*% p1
  mcd <- fast_mcd_cpp(t1, h, as.integer(nsamp))
  if (isTRUE(mcd$singular)) {
    stop("More than h observations lie on a lower-dimensional subspace of the ",
         k, "-dimensional PCA space; try a smaller 'k'.", call. = FALSE)
  }
  e2 <- eigen(mcd$cov, symmetric = TRUE)
  p2 <- p1 %*% e2$vectors
  center_z <- m1 + drop(p1 %*% mcd$center)

  loadings <- red$v %*% p2
  center <- red$center + drop(red$v %*% center_z)
  names(center) <- colnames(x)
  res <- list(
    loadings = loadings,
    eigenvalues = e2$values,
    center = center,
    scale = rep(1, ncol(x)),
    k = as.integer(k),
    h = as.integer(h),
    alpha = alpha,
    H0 = seq_len(n) %in% H0,
    H1 = H1
  )
  finish_robust_pca(res, x, "specproc_robpca")
}

#' @title Robust Sparse Principal Component Analysis (ROSPCA)
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Robust sparse PCA by the ROSPCA approach of Hubert, Reynkens, Schmitt and
#' Verdonck (2016): outliers are detected as in [robpca()], and sparse
#' loadings are computed from the clean observations by a grid-search sparse
#' PCA (Croux, Filzmoser and Fritz, 2013). Sparse loadings are zero for most
#' variables, which makes the components easier to interpret, for example as
#' a few emission lines. The computational kernels are written in C++.
#'
#' @details
#' The algorithm follows the published description in three steps:
#' 1. **Outlier detection.** The variables are robustly standardized (median
#'    and \eqn{Q_n}) if `stand = TRUE`. As in [robpca()], the `h` least
#'    outlying observations form \eqn{H_0}, and those whose orthogonal
#'    distance to the \eqn{k}-dimensional PCA subspace of \eqn{H_0} is below
#'    the cut-off form \eqn{H_1}.
#' 2. **Sparsification.** The observations of \eqn{H_1} are standardized and
#'    the sparse loadings are computed by maximizing, component by component,
#'    the variance of the scores minus `lambda` times the \eqn{L_1} norm of
#'    the loadings. Variables with zero loadings on all components are set
#'    aside; the observations whose orthogonal distance to the sparse
#'    subspace is below the cut-off form \eqn{H_2}, and the sparse loadings
#'    are recomputed from \eqn{H_2}, standardized in turn.
#' 3. **Eigenvalues and center.** The eigenvalues are first estimated by the
#'    squared \eqn{Q_n} of the scores of \eqn{H_2}. The `h` observations of
#'    \eqn{H_2} with the smallest score distances form \eqn{H_3}; the center
#'    is their mean,
#'    and the final eigenvalues are the variances of their scores. The
#'    components are sorted by decreasing eigenvalue.
#'
#' Larger `lambda` values give sparser loadings. The loadings apply to the
#' data standardized by `center` and `scale`; they are unit vectors, but
#' those of different components are not exactly orthogonal. The eigenvalues
#' are the variances of the scores in these standardized units. `lambda` is best chosen
#' by validation, for example with [step_rospca()] and tune.
#'
#' This is an independent implementation of the published description, not
#' a port of the rospca code, so results can differ from those of
#' `rospca::rospca()`.
#'
#' @inheritParams robpca
#' @param k The number of principal components. Default is 2.
#' @param lambda A non-negative number: the sparsity parameter. Default is 1.
#' @param stand A logical value: standardize the variables robustly (median
#'   and \eqn{Q_n}) before the analysis (`TRUE`, default) or only center them
#'   by their median.
#' @param ngrid The number of angles in the grid search. Default is 10.
#' @param maxiter The maximum number of grid refinements per component.
#'   Default is 10.
#'
#' @return An object of class `specproc_rospca` (inheriting from
#'   `specproc_robpca`), with the components described in [robpca()], and:
#'  - `scale`: the scales that standardize the variables before projection:
#'    the robust scale (\eqn{Q_n}, if `stand = TRUE`) times the standard
#'    deviation in \eqn{H_2}.
#'  - `lambda`: the sparsity parameter.
#'  - `H2`, `H3`: logical vectors of the observations in these subsets.
#'
#' @references
#'  - Hubert, M., Reynkens, T., Schmitt, E., Verdonck, T. (2016). Sparse PCA
#'    for high-dimensional data with outliers. Technometrics, 58(4):424-434.
#'  - Croux, C., Filzmoser, P., Fritz, H. (2013). Robust sparse principal
#'    component analysis. Technometrics, 55(2):202-214.
#'
#' @seealso [robpca()], [step_rospca()], [plot_outlier_map()]
#'
#' @export rospca
#'
#' @examples
#' set.seed(1)
#' # two latent factors, each loading on 5 of 20 variables
#' f <- matrix(rnorm(100 * 2), 100, 2)
#' x <- cbind(f[, 1] %o% rep(1, 5), f[, 2] %o% rep(1, 5)) * 3 +
#'   matrix(rnorm(100 * 10), 100, 10)
#' x <- cbind(x, matrix(rnorm(100 * 10), 100, 10))
#' x[1:5, ] <- x[1:5, ] + 10  # outliers
#' fit <- rospca(x, k = 2, lambda = 2)
#' round(fit$loadings, 2)
#'
rospca <- function(x, k = 2, lambda = 1, alpha = 0.75, ndir = 250, stand = TRUE,
                   ngrid = 10, maxiter = 10) {
  x <- robust_pca_input(x)
  n <- nrow(x)
  p <- ncol(x)
  check_count(k, "k")
  check_robust_args(k, max(k, 1), alpha, 0.5)
  check_number(lambda, "lambda", lower = 0)
  check_flag(stand, "stand")
  check_count(ngrid, "ngrid", lower = 2)
  check_count(maxiter, "maxiter")

  # Step 1: robust standardization and outlier detection
  med <- col_medians(x)
  scl <- if (stand) apply(x, 2, robust_scale) else rep(1, p)
  xs <- sweep(sweep(x, 2, med), 2, scl, "/")
  red <- svd_reduce(xs)
  z <- red$z
  if (k > min(ncol(z), n - 1)) {
    stop("'k' cannot exceed the rank of the data (", min(ncol(z), n - 1), ").", call. = FALSE)
  }
  h <- robust_h(n, alpha, k)
  H0 <- least_outlying(z, h, ndir)
  m0 <- colMeans(z[H0, , drop = FALSE])
  v0 <- eigen(stats::cov(z[H0, , drop = FALSE]), symmetric = TRUE)$vectors[, seq_len(k), drop = FALSE]
  od0 <- orthogonal_distance(sweep(z, 2, m0), v0)
  H1 <- od0 <= od_cutoff(od0, h)

  # Step 2: sparse PCA on H1, reweighting, sparse PCA on H2. Each subset is
  # standardized by its own mean and standard deviation.
  standardize_on <- function(rows) {
    mu <- colMeans(xs[rows, , drop = FALSE])
    sdev <- apply(xs[rows, , drop = FALSE], 2, stats::sd)
    sdev[!is.finite(sdev) | sdev <= 0] <- 1
    list(center = mu, scale = sdev, z = sweep(sweep(xs, 2, mu), 2, sdev, "/"))
  }
  sparse_fit <- function(z, rows, vars) {
    a <- spca_grid_cpp(z[rows, vars, drop = FALSE], as.integer(k), lambda,
                       as.integer(ngrid), as.integer(maxiter), 1e-5)
    loadings <- matrix(0, p, k)
    loadings[vars, ] <- a
    loadings
  }
  s1 <- standardize_on(H1)
  p1 <- sparse_fit(s1$z, H1, seq_len(p))
  index <- which(rowSums(p1 != 0) > 0)
  # distances in the full space: deviations in the discarded variables count
  od1 <- orthogonal_distance(s1$z, p1)
  H2 <- od1 <= od_cutoff(od1, h)
  s2 <- standardize_on(H2)
  loadings <- sparse_fit(s2$z, H2, index)

  # Step 3: eigenvalues and center. H3 holds the h observations of H2 with
  # the smallest score distances.
  z2 <- s2$z
  t_all <- z2 %*% loadings
  ev0 <- apply(t_all[H2, , drop = FALSE], 2, robust_scale)^2
  sd0 <- sqrt(rowSums(sweep(t_all^2, 2, ev0, "/")))
  in_h2 <- which(H2)
  H3 <- seq_len(n) %in% in_h2[order(sd0[in_h2])[seq_len(min(h, length(in_h2)))]]
  center_z <- colMeans(z2[H3, , drop = FALSE])
  eigenvalues <- apply(sweep(z2[H3, , drop = FALSE], 2, center_z) %*% loadings, 2, stats::var)
  ord <- order(eigenvalues, decreasing = TRUE)

  # back to the units of x: z2 = (x - base) / scale_total
  scale_total <- scl * s2$scale
  center <- med + scl * s2$center + scale_total * center_z
  names(center) <- colnames(x)
  res <- list(
    loadings = loadings[, ord, drop = FALSE],
    eigenvalues = eigenvalues[ord],
    center = center,
    scale = stats::setNames(scale_total, colnames(x)),
    k = as.integer(k),
    h = as.integer(h),
    alpha = alpha,
    lambda = lambda,
    H0 = seq_len(n) %in% H0,
    H1 = H1,
    H2 = H2,
    H3 = H3
  )
  finish_robust_pca(res, x, c("specproc_rospca", "specproc_robpca"))
}

#' @title Robust PCA for Cellwise and Casewise Outliers (MacroPCA)
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Robust PCA that handles both outlying observations (casewise outliers),
#' outlying cells (cellwise outliers) and missing values, by the MacroPCA
#' algorithm of Hubert, Rousseeuw and Van den Bossche (2019). This function
#' is a wrapper around [cellWise::MacroPCA()] that returns the results in the
#' same form as [robpca()], so that [predict()][predict.specproc_robpca] and
#' [plot_outlier_map()] work the same way, and adds [plot_cell_map()].
#'
#' @details
#' MacroPCA first detects outlying cells with the DetectDeviatingCells
#' algorithm, imputes them and the missing values, and then iterates a
#' robust PCA that down-weights outlying observations. The standardized
#' residuals of each cell show which cells deviate from the PCA fit; they
#' are displayed by [plot_cell_map()].
#'
#' Spectra often have many more channels than observations; MacroPCA's DDC
#' step can then be slow, and averaging adjacent channels first helps.
#'
#' @param x A numeric matrix or data frame, with one observation per row.
#'   Missing values are allowed.
#' @param k The number of principal components. If `NULL` (default), it is
#'   chosen by MacroPCA.
#' @param alpha The robustness parameter, between 0.5 and 1. Default is 0.5.
#' @param ... Further parameters of MacroPCA, passed in `MacroPCApars` (see
#'   [cellWise::MacroPCA()]), for example `scale` or `maxdir`.
#'
#' @return An object of class `specproc_macropca` (inheriting from
#'   `specproc_robpca`), with the components described in [robpca()], and:
#'  - `std_resid`: the standardized cell residuals.
#'  - `flagged_cells`: a logical matrix of the flagged cells.
#'  - `imputed`: the data with outlying cells and missing values imputed.
#'  - `fit`: the object returned by [cellWise::MacroPCA()].
#'
#' @references
#'  - Hubert, M., Rousseeuw, P.J., Van den Bossche, W. (2019). MacroPCA: an
#'    all-in-one PCA method allowing for missing values as well as cellwise
#'    and rowwise outliers. Technometrics, 61(4):459-473.
#'
#' @seealso [robpca()], [step_macropca()], [plot_outlier_map()],
#'   [plot_cell_map()]
#'
#' @export macropca
#'
#' @examples
#' set.seed(1)
#' x <- matrix(rnorm(60 * 8), 60, 8) %*% diag(8:1)
#' x[1:3, ] <- x[1:3, ] + 20   # outlying observations
#' x[10, 2] <- 40              # an outlying cell
#' x[12, 5] <- NA              # a missing value
#' fit <- macropca(x, k = 2)
#' fit
#'
macropca <- function(x, k = NULL, alpha = 0.5, ...) {
  if (missing(x)) {
    stop("Missing 'x' argument.")
  }
  x <- as_numeric_matrix(x, "x")
  if (is.null(colnames(x))) colnames(x) <- paste0("V", seq_len(ncol(x)))
  if (!is.null(k)) check_count(k, "k")
  check_number(alpha, "alpha", lower = 0.5, upper = 1)
  pars <- utils::modifyList(list(alpha = alpha, silent = TRUE), list(...))
  fit <- cellWise::MacroPCA(x, k = if (is.null(k)) 0 else k, MacroPCApars = pars)

  k <- as.integer(fit$k)
  loadings <- matrix(fit$loadings, ncol = k)
  scores <- matrix(fit$scores, ncol = k)
  flagged <- matrix(FALSE, nrow(x), ncol(x), dimnames = dimnames(x))
  flagged[fit$indcells] <- TRUE
  res <- list(
    loadings = loadings,
    eigenvalues = fit$eigenvalues,
    center = fit$center,
    scale = if (is.null(fit$scaleX)) rep(1, ncol(x)) else fit$scaleX,
    scores = scores,
    sd = unname(fit$SD),
    od = unname(fit$OD),
    cutoff_sd = fit$cutoffSD,
    cutoff_od = fit$cutoffOD,
    k = k,
    h = as.integer(fit$h),
    alpha = alpha,
    std_resid = fit$stdResid,
    flagged_cells = flagged,
    imputed = fit$X.NAimp,
    fit = fit
  )
  res$outlier_type <- outlier_type(res$sd, res$od, res$cutoff_sd, res$cutoff_od)
  colnames(res$loadings) <- colnames(res$scores) <- paste0("PC", seq_len(k))
  rownames(res$loadings) <- colnames(x)
  structure(res, variables = colnames(x), nvar = ncol(x),
            class = c("specproc_macropca", "specproc_robpca"))
}

#' @title Scores and Distances of New Observations
#'
#' @description
#' Projects new observations onto a robust PCA model fitted by [robpca()],
#' [rospca()] or [macropca()], and computes their score and orthogonal
#' distances with the cut-offs of the calibration data.
#'
#' @param object An object returned by [robpca()], [rospca()] or [macropca()].
#' @param newdata A numeric matrix or data frame with the same variables as
#'   the calibration data. For [macropca()], missing values are allowed.
#' @param ... Not used.
#'
#' @return A tibble with the scores (`PC1`, ..., `PCk`), the score distance
#'   `sd`, the orthogonal distance `od` and the `outlier_type` of each new
#'   observation.
#'
#' @seealso [plot_outlier_map()], which can display new observations.
#' @export
predict.specproc_robpca <- function(object, newdata, ...) {
  x <- filter_newdata(object, newdata)
  if (anyNA(x)) {
    stop("'newdata' contains missing values.", call. = FALSE)
  }
  xs <- sweep(sweep(x, 2, object$center), 2, object$scale, "/")
  scores <- xs %*% object$loadings
  robust_pca_output(object, scores, orthogonal_distance(xs, object$loadings))
}

#' @rdname predict.specproc_robpca
#' @export
predict.specproc_macropca <- function(object, newdata, ...) {
  x <- as_numeric_matrix(newdata, "newdata")
  if (ncol(x) != attr(object, "nvar")) {
    stop("'newdata' must have ", attr(object, "nvar"), " columns, like the calibration data.", call. = FALSE)
  }
  colnames(x) <- attr(object, "variables")
  pred <- cellWise::MacroPCApredict(x, object$fit)
  robust_pca_output(object, matrix(pred$scores, ncol = object$k), unname(pred$OD), unname(pred$SD))
}

#' @export
print.specproc_robpca <- function(x, ...) {
  label <- switch(
    class(x)[1],
    specproc_robpca = "Robust PCA (ROBPCA)",
    specproc_rospca = paste0("Robust sparse PCA (ROSPCA, lambda = ", format(x$lambda), ")"),
    specproc_macropca = "Robust PCA for cellwise and casewise outliers (MacroPCA)"
  )
  cat(label, "\n\n", sep = "")
  cat("Observations:   ", length(x$sd), " (h = ", x$h, ")\n", sep = "")
  cat("Variables:      ", attr(x, "nvar"), "\n", sep = "")
  cat("Components:     ", x$k, "\n", sep = "")
  cat("Eigenvalues:    ", paste(format(x$eigenvalues, digits = 4), collapse = " "), "\n", sep = "")
  if (inherits(x, "specproc_rospca")) {
    cat("Non-zero loadings per component: ", paste(colSums(x$loadings != 0), collapse = " "), "\n", sep = "")
  }
  if (inherits(x, "specproc_macropca")) {
    cat("Flagged cells:  ", sum(x$flagged_cells), "\n", sep = "")
  }
  cat("\nOutlier types:\n")
  print(table(x$outlier_type))
  invisible(x)
}

# ---- internals ---------------------------------------------------------------

robust_pca_input <- function(x) {
  if (missing(x)) {
    stop("Missing 'x' argument.", call. = FALSE)
  }
  x <- as_numeric_matrix(x, "x")
  if (anyNA(x)) {
    stop("'x' contains missing values; use macropca() or impute them first.", call. = FALSE)
  }
  if (nrow(x) < 5 || ncol(x) < 2) {
    stop("At least 5 observations and 2 variables are needed.", call. = FALSE)
  }
  x
}

check_robust_args <- function(k, kmax, alpha, var_explained) {
  if (!is.null(k)) check_count(k, "k")
  check_count(kmax, "kmax")
  check_number(alpha, "alpha", lower = 0.5, upper = 1)
  check_number(var_explained, "var_explained", lower = 0, upper = 1, lower_open = TRUE)
  invisible(TRUE)
}

robust_h <- function(n, alpha, kmax) {
  min(n, max(floor(alpha * n), floor((n + kmax + 1) / 2)))
}

# Reduces centered data to the affine subspace spanned by the observations.
svd_reduce <- function(x) {
  center <- colMeans(x)
  xc <- sweep(x, 2, center)
  s <- svd(xc, nu = 0)
  rank <- sum(s$d > max(dim(x)) * s$d[1] * .Machine$double.eps)
  if (rank < 1) {
    stop("The data have no variation.", call. = FALSE)
  }
  v <- s$v[, seq_len(rank), drop = FALSE]
  list(center = center, v = v, z = xc %*% v)
}

# Indices of the h observations with the smallest Stahel-Donoho outlyingness.
least_outlying <- function(z, h, ndir) {
  if (identical(ndir, "all")) {
    ndir <- 0L
  } else {
    check_count(ndir, "ndir")
  }
  outl <- if (ncol(z) == 1) {
    u <- univariate_mcd_cpp(z[, 1], h)
    abs(z[, 1] - u[["location"]]) / max(u[["scale"]], .Machine$double.eps)
  } else {
    sd_outlyingness_cpp(z, h, as.integer(ndir))
  }
  sort(order(outl)[seq_len(h)])
}

choose_k <- function(values, kmax, var_explained) {
  values <- pmax(values, 0)
  cum <- cumsum(values) / sum(values)
  k <- which(cum >= var_explained - 1e-12)[1]
  max(1L, min(k, kmax))
}

# Distance of each (centered) row to the span of the columns of `loadings`.
orthogonal_distance <- function(xc, loadings) {
  fitted <- xc %*% loadings %*% solve(crossprod(loadings), t(loadings))
  sqrt(pmax(rowSums((xc - fitted)^2), 0))
}

# Cut-off for orthogonal distances: Wilson-Hilferty approximation with the
# univariate MCD of OD^(2/3).
od_cutoff <- function(od, h) {
  if (max(od) <= sqrt(.Machine$double.eps) * max(1, max(od))) {
    return(0)
  }
  u <- univariate_mcd_cpp(od^(2 / 3), min(h, length(od)))
  (u[["location"]] + u[["scale"]] * stats::qnorm(0.975))^(3 / 2)
}

robust_scale <- function(v) {
  s <- robustbase::Qn(v)
  if (!is.finite(s) || s <= 0) s <- stats::mad(v)
  if (!is.finite(s) || s <= 0) s <- stats::sd(v)
  if (!is.finite(s) || s <= 0) s <- 1
  s
}

outlier_type <- function(sd, od, cutoff_sd, cutoff_od) {
  high_sd <- sd > cutoff_sd
  high_od <- od > cutoff_od
  type <- ifelse(high_sd & high_od, "bad leverage",
                 ifelse(high_sd, "good leverage",
                        ifelse(high_od, "orthogonal outlier", "regular")))
  factor(type, levels = c("regular", "good leverage", "orthogonal outlier", "bad leverage"))
}

# Adds scores, distances, cut-offs and outlier types, and the class.
finish_robust_pca <- function(res, x, class) {
  xs <- sweep(sweep(x, 2, res$center), 2, res$scale, "/")
  scores <- xs %*% res$loadings
  comp <- paste0("PC", seq_len(res$k))
  colnames(res$loadings) <- colnames(scores) <- comp
  rownames(res$loadings) <- colnames(x)
  res$scores <- scores
  res$sd <- sqrt(rowSums(sweep(scores^2, 2, res$eigenvalues, "/")))
  res$od <- orthogonal_distance(xs, res$loadings)
  res$cutoff_sd <- sqrt(stats::qchisq(0.975, res$k))
  res$cutoff_od <- od_cutoff(res$od, res$h)
  res$outlier_type <- outlier_type(res$sd, res$od, res$cutoff_sd, res$cutoff_od)
  structure(res, variables = colnames(x), nvar = ncol(x), class = class)
}

robust_pca_output <- function(object, scores, od, sd = NULL) {
  colnames(scores) <- paste0("PC", seq_len(ncol(scores)))
  if (is.null(sd)) {
    sd <- sqrt(rowSums(sweep(scores^2, 2, object$eigenvalues, "/")))
  }
  out <- tibble::as_tibble(scores)
  out$sd <- sd
  out$od <- od
  out$outlier_type <- outlier_type(sd, od, object$cutoff_sd, object$cutoff_od)
  out
}
