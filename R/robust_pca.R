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
#' minerals <- c("Ca", "Cl", "Cu", "Fe", "Mg", "Mn", "Mo", "P", "K", "Na", "S", "Zn")
#' forageLIBS |>
#'   dplyr::select(-Measurement, -Sample, -dplyr::all_of(minerals)) |>
#'   center() |>
#'   robpca() |>
#'   print()
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
#'    the cut-off form \eqn{H_1}
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
#'   and \eqn{Q_n}) before the analysis (`FALSE`, default) or only center them
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
#' minerals <- c("Ca", "Cl", "Cu", "Fe", "Mg", "Mn", "Mo", "P", "K", "Na", "S", "Zn")
#' forageLIBS |>
#'   dplyr::select(-Measurement, -Sample, -dplyr::all_of(minerals)) |>
#'   center() |>
#'   rospca() |>
#'   print()
#'
rospca <- function(x, k = 2, lambda = 1, alpha = 0.75, ndir = 250, stand = FALSE,
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
#' outlying cells (cellwise outliers) and missing values, after the MacroPCA
#' algorithm of Hubert, Rousseeuw and Van den Bossche (2019). The results
#' have the same form as those of [robpca()], so that
#' [predict()][predict.specproc_robpca] and [plot_outlier_map()] work the
#' same way, and [plot_cell_map()] shows the outlying cells.
#'
#' @details
#' The algorithm has four steps:
#'  1. **Deviating cells.** The cells that deviate from the values predicted
#'     by the most correlated variables are detected and imputed by the
#'     detection of deviating cells (DDC) of Rousseeuw and Van den Bossche
#'     (2018), as are the missing values. The neighbor search and the
#'     predictions of DDC are computed in C++, by blocks of variables, so
#'     that spectra with thousands of channels are handled quickly.
#'  2. **Initial subspace.** The `h` observations with the smallest
#'     Stahel-Donoho outlyingness (as in [robpca()], with `ndir` directions)
#'     give an initial PCA of the imputed data. When `k` is `NULL`, it is the
#'     smallest number of components that explain `var_explained` of the
#'     variance of these observations (at most `kmax`).
#'  3. **Iterations.** The flagged and missing cells are imputed by the
#'     fitted values of the current PCA, the observations within the cut-off
#'     of the orthogonal distances are kept, and the PCA is refitted on them,
#'     until the subspace changes by less than `tol` (at most `maxiter`
#'     times).
#'  4. **Final fit.** The center and the eigenvalues are re-estimated by the
#'     minimum covariance determinant of the scores, as in [robpca()].
#'
#' The residuals of each variable are robustly standardized (median and
#' MAD), and the cells beyond \eqn{\sqrt{\chi^2_{1, 0.99}}} are flagged. The
#' scores of each observation are then computed with its flagged and missing
#' cells imputed by the fit (iteratively, as for new data with
#' [predict()][predict.specproc_robpca]), while its orthogonal distance and
#' cell residuals use its observed cells, so that the deviating cells of an
#' observation count in its orthogonal distance. The cut-offs of the score
#' and orthogonal distances are those of [robpca()], so that the outlier
#' maps of the robust PCA methods of specProc can be compared.
#'
#' @param x A numeric matrix or data frame, with one observation per row.
#'   Missing values are allowed.
#' @param k The number of principal components. If `NULL` (default), it is
#'   chosen from `var_explained` and `kmax`.
#' @param alpha The robustness parameter, between 0.5 and 1: the fraction of
#'   observations used in the fit. Default is 0.5.
#' @param kmax The maximum number of components. Default is 10.
#' @param var_explained The fraction of variance used to choose `k` when it
#'   is not given. Default is 0.8.
#' @param scale A logical: scale the variables by their robust scale (`FALSE`,
#'   default: they are only centered, so that intense emission lines are
#'   not outweighed by noise and continuum channels).
#' @param ndir The number of random directions of the outlyingness, or `"all"`.
#'   Default is 250.
#' @param maxiter,tol The maximum number of iterations and the tolerance on
#'   the change of the subspace. Defaults are 20 and 1e-4.
#'
#' @return An object of class `specproc_macropca` (inheriting from
#'   `specproc_robpca`), with the components described in [robpca()], and:
#'  - `std_resid`: the standardized cell residuals (`NA` for missing cells).
#'  - `flagged_cells`: a logical matrix of the flagged cells.
#'  - `imputed`: the data with the missing values imputed by the PCA fit
#'    (the flagged cells keep their values).
#'  - `resid_center`, `resid_scale`: the robust center and scale of the
#'    residuals of each variable, used to standardize those of new data.
#'
#' @references
#'  - Hubert, M., Rousseeuw, P.J., Van den Bossche, W. (2019). MacroPCA: an
#'    all-in-one PCA method allowing for missing values as well as cellwise
#'    and rowwise outliers. Technometrics, 61(4):459-473.
#'  - Rousseeuw, P.J., Van den Bossche, W. (2018). Detecting deviating data
#'    cells. Technometrics, 60(2):135-145.
#'  - Raymaekers, J., Rousseeuw, P.J. (2021). Fast robust correlation for
#'    high-dimensional data. Technometrics, 63(2):184-198.
#'
#' @seealso [robpca()], [step_macropca()], [plot_outlier_map()],
#'   [plot_cell_map()]
#'
#' @export macropca
#'
#' @examples
#' \donttest{
#' set.seed(1)
#' # LIBS spectra of forage samples
#' minerals <- c("Ca", "Cl", "Cu", "Fe", "Mg", "Mn", "Mo", "P", "K", "Na", "S", "Zn")
#' forageLIBS |>
#'   dplyr::select(-Measurement, -Sample, -dplyr::all_of(minerals)) |>
#'   center() |>
#'   macropca() |>
#'   print()
#' }
#'
macropca <- function(x, k = NULL, alpha = 0.5, kmax = 10, var_explained = 0.8, scale = FALSE,
                     ndir = 250, maxiter = 20, tol = 1e-4) {
  if (missing(x)) {
    stop("Missing 'x' argument.")
  }
  x <- as_numeric_matrix(x, "x")
  if (is.null(colnames(x))) colnames(x) <- paste0("V", seq_len(ncol(x)))
  if (nrow(x) < 5 || ncol(x) < 2) {
    stop("At least 5 observations and 2 variables are needed.", call. = FALSE)
  }
  check_robust_args(k, kmax, alpha, var_explained)
  check_flag(scale, "scale")
  check_count(maxiter, "maxiter")
  check_number(tol, "tol", lower = 0, lower_open = TRUE)
  n <- nrow(x)
  missing_cells <- is.na(x)

  # Step 1: deviating cells
  cells <- ddc(x)
  sc <- if (scale) cells$scale else rep(1, ncol(x))
  y_na <- sweep(x, 2, sc, "/")
  replace <- cells$flagged_cells | missing_cells
  y <- y_na
  y[replace] <- (cells$imputed / rep(sc, each = n))[replace]

  # Step 2: initial subspace from the h least outlying observations
  red <- svd_reduce(y)
  kmax <- min(kmax, ncol(red$z), n - 1)
  h <- robust_h(n, alpha, kmax)
  H <- least_outlying(red$z, h, ndir)
  pca <- macropca_pca(y[H, , drop = FALSE])
  if (is.null(k)) {
    k <- choose_k(pca$values, kmax, var_explained)
  } else if (k > kmax) {
    warning("'k' reduced to ", kmax, ".", call. = FALSE)
    k <- kmax
  }
  center <- pca$center
  loadings <- pca$vectors[, seq_len(k), drop = FALSE]

  # Step 3: impute the flagged and missing cells from the PCA fit, and refit
  # on the observations within the cut-off of the orthogonal distances
  for (iter in seq_len(maxiter)) {
    yc <- sweep(y, 2, center)
    fitted <- sweep(yc %*% loadings %*% t(loadings), 2, center, "+")
    y[replace] <- fitted[replace]
    yc <- sweep(y, 2, center)
    od <- orthogonal_distance(yc, loadings)
    keep <- od <= od_cutoff(od, h)
    if (sum(keep) <= k) keep <- seq_len(n) %in% order(od)[seq_len(h)]
    pca <- macropca_pca(y[keep, , drop = FALSE])
    new_loadings <- pca$vectors[, seq_len(k), drop = FALSE]
    change <- k - sum(crossprod(loadings, new_loadings)^2)
    center <- pca$center
    loadings <- new_loadings
    if (change < tol) break
  }

  # Step 4: the scores of each observation with its deviating cells imputed
  # by the fit (as for new data), starting from the observed cells, so that
  # a leverage observation whose cells DDC flagged is not kept away from the
  # subspace by its own imputation; then the center and eigenvalues from the
  # MCD of these scores
  names(center) <- colnames(x)
  res <- list(loadings = loadings, eigenvalues = rep(1, k), center = center, scale = sc,
              k = as.integer(k), h = as.integer(h), alpha = alpha, cutoff = cells$cutoff)
  first <- macropca_distances(res, y, y_na, missing_cells, cells$cutoff)
  res$resid_center <- first$resid_center
  res$resid_scale <- first$resid_scale
  start <- y_na
  start[missing_cells] <- y[missing_cells]
  scores <- macropca_refine(res, start, y_na, missing_cells)$scores
  mcd <- if (k == 1) {
    u <- univariate_mcd_cpp(scores[, 1], h)
    list(center = u[["location"]], cov = matrix(u[["scale"]]^2), singular = FALSE)
  } else {
    fast_mcd_cpp(scores, h, 500L)
  }
  if (isTRUE(mcd$singular)) {
    stop("More than h observations lie on a lower-dimensional subspace of the ",
         k, "-dimensional PCA space; try a smaller 'k'.", call. = FALSE)
  }
  e <- eigen(mcd$cov, symmetric = TRUE)
  res$center <- center <- center + drop(loadings %*% mcd$center)
  res$loadings <- loadings <- loadings %*% e$vectors
  res$eigenvalues <- e$values
  names(res$center) <- colnames(x)
  final <- macropca_refine(res, start, y_na, missing_cells)
  res[c("scores", "sd", "od", "std_resid", "flagged_cells")] <-
    final[c("scores", "sd", "od", "std_resid", "flagged_cells")]
  res$cutoff_sd <- sqrt(stats::qchisq(0.975, k))
  res$cutoff_od <- od_cutoff(res$od, h)
  res$outlier_type <- outlier_type(res$sd, res$od, res$cutoff_sd, res$cutoff_od)
  # the data with the missing values imputed by the fit
  imputed <- y_na
  fitted <- sweep(final$scores %*% t(loadings), 2, center, "+")
  imputed[missing_cells] <- fitted[missing_cells]
  res$imputed <- sweep(imputed, 2, sc, "*")
  dimnames(res$imputed) <- dimnames(x)
  colnames(res$loadings) <- colnames(res$scores) <- paste0("PC", seq_len(k))
  rownames(res$loadings) <- colnames(x)
  structure(res, variables = colnames(x), nvar = ncol(x),
            class = c("specproc_macropca", "specproc_robpca"))
}

# Classical PCA (center and eigenvectors) of the rows of y.
macropca_pca <- function(y) {
  center <- colMeans(y)
  s <- svd(sweep(y, 2, center), nu = 0)
  list(center = center, vectors = s$v, values = s$d^2 / max(nrow(y) - 1, 1))
}

# Scores, distances and standardized cell residuals of a MacroPCA fit, for
# the fully imputed data `y` (scores) and the data with only the missing
# values imputed `y_na` (orthogonal distances and residuals). With
# `resid_center` and `resid_scale` (new data), the residuals are
# standardized with those of the calibration data.
macropca_distances <- function(fit, y, y_na, missing_cells, cutoff, resid_center = NULL,
                               resid_scale = NULL) {
  scores <- sweep(y, 2, fit$center) %*% fit$loadings
  fitted <- sweep(scores %*% t(fit$loadings), 2, fit$center, "+")
  y_na[missing_cells] <- fitted[missing_cells]
  resid <- y_na - fitted
  if (is.null(resid_center)) {
    resid_center <- apply(resid, 2, stats::median)
    resid_scale <- apply(resid, 2, stats::mad)
    resid_scale[!is.finite(resid_scale) | resid_scale <= 0] <- 1
  }
  std_resid <- sweep(sweep(resid, 2, resid_center), 2, resid_scale, "/")
  std_resid[missing_cells] <- NA
  flagged <- !is.na(std_resid) & abs(std_resid) > cutoff
  dimnames(std_resid) <- dimnames(flagged) <- dimnames(y)
  list(scores = scores,
       sd = sqrt(rowSums(sweep(scores^2, 2, fit$eigenvalues, "/"))),
       od = orthogonal_distance(sweep(y_na, 2, fit$center), fit$loadings),
       std_resid = std_resid, flagged_cells = flagged,
       resid_center = resid_center, resid_scale = resid_scale, cutoff = cutoff)
}

# Imputes the flagged and missing cells of each observation by the fit
# until they no longer change, and returns its distances and residuals
# (standardized with the residual scales of the calibration data).
macropca_refine <- function(fit, y, y_na, missing_cells, maxiter = 20) {
  cutoff <- fit$cutoff %||% sqrt(stats::qchisq(0.99, 1))
  for (iter in seq_len(maxiter)) {
    dist <- macropca_distances(fit, y, y_na, missing_cells, cutoff, fit$resid_center, fit$resid_scale)
    replace <- dist$flagged_cells | missing_cells
    fitted <- sweep(dist$scores %*% t(fit$loadings), 2, fit$center, "+")
    updated <- y_na
    updated[replace] <- fitted[replace]
    converged <- max(abs(updated - y)) < 1e-8 * max(1, max(abs(y)))
    y <- updated
    if (converged) break
  }
  macropca_distances(fit, y, y_na, missing_cells, cutoff, fit$resid_center, fit$resid_scale)
}

#' @rdname predict.specproc_robpca
#' @export
predict.specproc_macropca <- function(object, newdata, ...) {
  x <- as_numeric_matrix(newdata, "newdata")
  if (ncol(x) != attr(object, "nvar")) {
    stop("'newdata' must have ", attr(object, "nvar"), " columns, like the calibration data.", call. = FALSE)
  }
  colnames(x) <- attr(object, "variables")
  missing_cells <- is.na(x)
  y_na <- sweep(x, 2, object$scale, "/")
  # missing cells start at the center
  y <- y_na
  y[missing_cells] <- matrix(object$center, nrow(y), ncol(y), byrow = TRUE)[missing_cells]
  dist <- macropca_refine(object, y, y_na, missing_cells)
  robust_pca_output(object, dist$scores, dist$od, dist$sd)
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
