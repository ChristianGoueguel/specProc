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
#' # the 380-430 nm window (Ca II H and K lines)
#' wl <- suppressWarnings(as.numeric(names(forageLIBS)))
#' forageLIBS[, which(wl > 380 & wl < 430)] |>
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
#' outlying cells (cellwise outliers) and missing values, by the MacroPCA
#' algorithm of Hubert, Rousseeuw and Van den Bossche (2019), which combines
#' the detection of deviating cells (DDC) with the steps of ROBPCA (Hubert,
#' Rousseeuw and Vanden Branden, 2005). The results have the same form as
#' those of [robpca()], so that [predict()][predict.specproc_robpca] and
#' [plot_outlier_map()] work the same way, and [plot_cell_map()] shows the
#' outlying cells. [cellpca()] starts from a MacroPCA fit.
#'
#' @details
#' The algorithm follows Hubert, Rousseeuw and Van den Bossche (2019) and
#' the MacroPCA code of the cellWise package, whose results it reproduces
#' (with `scale = FALSE`):
#'  0. **Deviating cells.** The cells that deviate from the values predicted
#'     by the most correlated variables, and the outlying observations, are
#'     detected by the DDC of Rousseeuw and Van den Bossche (2018), computed
#'     in C++ as in cellWise (for more than 750 variables, the neighbors are
#'     those with the largest wrapped correlations, found exactly by blocks
#'     of variables). DDC also imputes the flagged and missing cells. Of the
#'     observations flagged by DDC, at most the \eqn{n - h} most outlying
#'     are set aside.
#'  1. **Standardization.** With `scale = TRUE`, the variables are divided
#'     by their robust scale (1-step M-estimator).
#'  2. **Projection pursuit.** As in [robpca()], the outlyingness of each
#'     observation is its largest standardized distance (with the univariate
#'     MCD) over `ndir` directions through pairs of observations (all pairs
#'     when there are few), on the data in which only the `h` observations
#'     with the fewest flagged cells have their flagged cells imputed. The
#'     `h` least outlying observations not set aside form \eqn{H_0}.
#'  3. **Subspace dimension.** A classical PCA of the observations of
#'     \eqn{H_0}, with their flagged and missing cells imputed, gives the
#'     eigenvalues: when `k` is `NULL`, it is the smallest number of
#'     components that explain `var_explained` of their variance (at most
#'     `kmax`).
#'  4. **Iterative subspace estimation.** The flagged and missing cells are
#'     imputed by the fitted values of the current PCA, and the PCA of
#'     \eqn{H_0} is refitted, until the largest angle between the old and
#'     the new subspace (as a fraction of a right angle) is below `tol` (at
#'     most `maxiter` times).
#'  5. **Reweighting.** The observations whose orthogonal distance is below
#'     the cut-off, and not set aside, form \eqn{H^*}, and the PCA is
#'     refitted on them (with their flagged cells imputed).
#'  6. **Robust basis.** The center and the eigenvectors within the subspace
#'     are estimated by concentration steps on the scores of \eqn{H^*}
#'     followed by the deterministic MCD (DetMCD), so that good leverage
#'     observations do not tilt the loadings.
#'  7. **Distances.** The scores and the distances of all the observations
#'     are computed from the data with only the missing cells imputed. The
#'     cut-off of the orthogonal distances is computed on the data whose
#'     observations of \eqn{H^*} have their flagged cells imputed.
#'  8. **Residuals.** The residuals of the observed cells are standardized by
#'     their 1-step M scale, and the cells beyond
#'     \eqn{\sqrt{\chi^2_{1, 0.99}}} are flagged.
#'
#' As in the paper, the cut-offs of the score distances
#' (\eqn{\sqrt{\chi^2_{k, 0.99}}}) and of the orthogonal distances (the
#' Wilson-Hilferty approximation with the univariate MCD and the 0.99
#' quantile) are at the 99% level. New observations are analyzed as in
#' MacroPCApredict of cellWise: their deviating cells are detected with the
#' DDC model of the fit, and their flagged and missing cells are imputed
#' iteratively by the fit before their distances are computed.
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
#' @param maxiter,tol The maximum number of iterations of step 4, and the
#'   tolerance on the largest angle between successive subspaces (as a
#'   fraction of a right angle). Defaults are 20 and 0.005, as in the paper.
#'
#' @return An object of class `specproc_macropca` (inheriting from
#'   `specproc_robpca`), with the components described in [robpca()], and:
#'  - `std_resid`: the standardized cell residuals (`NA` for missing cells).
#'  - `flagged_cells`: a logical matrix of the flagged cells.
#'  - `flagged_rows`: the observations flagged by DDC.
#'  - `imputed`: the data with the missing values imputed by the PCA fit
#'    (the flagged cells keep their values).
#'  - `resid_scale`: the robust scale of the residuals of each variable,
#'    used to standardize those of new data.
#'
#' @references
#'  - Hubert, M., Rousseeuw, P.J., Van den Bossche, W. (2019). MacroPCA: an
#'    all-in-one PCA method allowing for missing values as well as cellwise
#'    and rowwise outliers. Technometrics, 61(4):459-473.
#'    \doi{10.1080/00401706.2018.1562989}
#'  - Hubert, M., Rousseeuw, P.J., Vanden Branden, K. (2005). ROBPCA: a new
#'    approach to robust principal component analysis. Technometrics,
#'    47(1):64-79.
#'  - Rousseeuw, P.J., Van den Bossche, W. (2018). Detecting deviating data
#'    cells. Technometrics, 60(2):135-145.
#'  - Hubert, M., Rousseeuw, P.J., Verdonck, T. (2012). A deterministic
#'    algorithm for robust location and scatter. Journal of Computational
#'    and Graphical Statistics, 21(3):618-637.
#'
#' @seealso [cellpca()], [robpca()], [step_macropca()], [plot_outlier_map()],
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
                     ndir = 250, maxiter = 20, tol = 0.005) {
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
  if (!identical(ndir, "all")) check_count(ndir, "ndir")
  n <- nrow(x)
  d <- ncol(x)
  kmax <- min(kmax, d)
  if (!is.null(k) && k > kmax) {
    warning("'k' reduced to ", kmax, ".", call. = FALSE)
    k <- kmax
  }
  h <- h_alpha_n(alpha, n, if (is.null(k)) kmax else k)
  missing_cells <- is.na(x)

  # Deviating cells, and the most outlying of the rows flagged by DDC
  cells <- ddc(x)
  Ti <- abs(cells$Ti)
  Ti[is.na(Ti)] <- 0
  rows_ddc <- intersect(order(Ti, decreasing = TRUE)[seq_len(n - h)],
                        which(Ti > sqrt(stats::qchisq(0.99, 1))))
  imputable <- cells$flagged_cells | missing_cells

  # Step 1: standardization
  sc <- if (scale) loc_scale_1step(x)$scale else rep(1, d)
  sc[!is.finite(sc) | sc <= 0] <- 1
  x_obs <- sweep(x, 2, sc, "/")
  x_all <- x_obs
  x_all[imputable] <- (cells$imputed / rep(sc, each = n))[imputable]
  x_na <- x_obs
  x_na[missing_cells] <- x_all[missing_cells]
  rank <- trunc_pc(x_all)$rank
  if (rank == 0) stop("All observations collapse.", call. = FALSE)

  # Step 2: projection pursuit, on the data where only the h rows with the
  # fewest flagged cells (the rows flagged by DDC excluded) are cell-imputed
  flagged_per_row <- rowSums(cells$flagged_cells)
  flagged_per_row[rows_ddc] <- d
  others <- setdiff(seq_len(n), order(flagged_per_row)[seq_len(h)])
  x_ci <- x_all
  x_ci[others, ] <- x_na[others, ]
  outl <- pp_outlyingness(x_ci, ndir, alpha)
  H0 <- setdiff(order(outl), rows_ddc)[seq_len(h)]
  x_ci[others, ] <- x_all[others, ]

  # Step 3: subspace dimension, from the PCA of the cell-imputed rows of H0
  pca <- trunc_pc(x_ci[H0, , drop = FALSE])
  kmax <- min(pca$rank, kmax)
  if (is.null(k)) {
    k <- choose_k(pca$eigenvalues, kmax, var_explained)
  }
  k <- min(k, pca$rank)
  loadings <- pca$loadings[, seq_len(k), drop = FALSE]
  center <- pca$center

  # Step 4: iterative subspace estimation, imputing the flagged and missing
  # cells by the fit, with the PCA of H0
  iterations <- 0
  if (any(imputable) && maxiter > 0) {
    repeat {
      iterations <- iterations + 1
      fitted <- pca_fitted(x_ci, center, loadings)
      x_ci[imputable] <- fitted[imputable]
      pca <- trunc_pc(x_ci[H0, , drop = FALSE], k)
      k <- min(k, pca$rank)
      new_loadings <- pca$loadings[, seq_len(k), drop = FALSE]
      change <- max_angle(new_loadings, loadings)
      loadings <- new_loadings
      center <- pca$center
      if (iterations >= maxiter || change <= tol) break
    }
  }
  fitted <- pca_fitted(x_ci, center, loadings)
  x_ci[imputable] <- fitted[imputable]
  x_na[missing_cells] <- x_ci[missing_cells]
  x_fi <- x_ci
  x_ci[-H0, ] <- x_na[-H0, ]

  # Step 5: reweighting on the orthogonal distances of the cell-imputed data
  if (k < rank) {
    od <- orthogonal_distance(sweep(x_ci, 2, center), loadings)
    H_star <- od <= od_cutoff_unimcd(od, alpha)
    H_star[rows_ddc] <- FALSE
    pca <- trunc_pc(x_fi[H_star, , drop = FALSE], k)
    k <- min(pca$rank, k)
  } else {
    H_star <- seq_len(n) %in% H0
  }
  x_ci <- x_fi
  x_ci[!H_star, ] <- x_na[!H_star, ]

  # Step 6: center and basis within the subspace (C-steps, then DetMCD)
  center <- pca$center
  rot <- pca$loadings[, seq_len(k), drop = FALSE]
  n1 <- sum(H_star)
  h1 <- h_alpha_n(alpha, n1, k)
  scores1 <- sweep(x_ci[H_star, , drop = FALSE], 2, pca$center) %*% rot
  mah <- mahalanobis_diag(scores1, pca$eigenvalues[seq_len(k)])
  old_obj <- prod(pca$eigenvalues[seq_len(k)])
  for (j in seq_len(100)) {
    sub <- trunc_pc(scores1[order(mah)[seq_len(h1)], , drop = FALSE], k)
    obj <- prod(sub$eigenvalues)
    scores1 <- sweep(scores1, 2, sub$center) %*% sub$loadings
    center <- center + drop(rot %*% sub$center)
    rot <- rot %*% sub$loadings
    mah <- mahalanobis_diag(scores1, sub$eigenvalues)
    if (sub$rank == k && abs(old_obj - obj) < 1e-12) break
    old_obj <- obj
    k <- min(k, sub$rank)
  }
  k <- ncol(rot)
  inner <- final_mcd(scores1, h1 / n1, obj, mah, k)
  e <- eigen(inner$cov, symmetric = TRUE)
  center <- center + drop(rot %*% inner$center)
  loadings <- rot %*% e$vectors
  loadings <- sweep(loadings, 2, apply(loadings, 2, function(v) if (v[which.max(abs(v))] < 0) -1 else 1), "*")
  names(center) <- colnames(x)
  dimnames(loadings) <- list(colnames(x), paste0("PC", seq_len(k)))

  # Step 7: scores and distances of the NA-imputed data; cut-off of the
  # orthogonal distances from the cell-imputed data
  res <- list(loadings = loadings, eigenvalues = e$values, center = center, scale = sc,
              k = as.integer(k), h = as.integer(h), alpha = alpha, cutoff = cells$cutoff,
              rank = rank, iterations = iterations)
  od_ci <- orthogonal_distance(sweep(x_ci, 2, center), loadings)
  res$cutoff_od <- if (k < rank) od_cutoff_unimcd(od_ci, alpha) else 0
  res$cutoff_sd <- sqrt(stats::qchisq(0.99, k))

  # Step 8: standardized residuals of the observed cells
  dist <- macropca_distances(res, x_na, x_obs)
  res[c("scores", "sd", "od", "std_resid", "flagged_cells", "resid_scale")] <-
    dist[c("scores", "sd", "od", "std_resid", "flagged_cells", "resid_scale")]
  res$flagged_rows <- seq_len(n) %in% rows_ddc
  res$outlier_type <- outlier_type(res$sd, res$od, res$cutoff_sd, res$cutoff_od)
  res$imputed <- sweep(x_na, 2, sc, "*")
  dimnames(res$imputed) <- dimnames(x)
  # scores of the fully imputed data, the start of cellpca()
  res$scores_imputed <- sweep(x_fi, 2, center) %*% loadings
  res$ddc <- cells[c("center", "scale", "cutoff", "tol_prob", "model")]
  structure(res, variables = colnames(x), nvar = d,
            class = c("specproc_macropca", "specproc_robpca"))
}

#' @title Robust PCA by Casewise and Cellwise Weighting (cellPCA)
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Robust PCA that handles outlying observations (casewise outliers),
#' outlying cells (cellwise outliers) and missing values by minimizing a
#' single objective function: the cellPCA method of Centofanti, Hubert and
#' Rousseeuw. Each cell and each observation gets a weight between 0 and 1
#' that reflects its outlyingness, and regular cells and observations are
#' not downweighted, which makes cellPCA more efficient than [macropca()].
#' The iterations are computed in C++. The results have the same form as
#' those of [robpca()], so that [predict()][predict.specproc_robpca],
#' [plot_outlier_map()] and [plot_cell_map()] work the same way.
#'
#' @details
#' cellPCA approximates the data by a fit \eqn{\hat{X} = 1_n \mu^T + U V^T}
#' of rank `k` that minimizes
#' \deqn{\hat\sigma_2^2 \frac{1}{n} \sum_{i=1}^n m_i \rho_2\left(\frac{t_i}{\hat\sigma_2}\right),
#' \quad t_i = \sqrt{\frac{1}{m_i} \sum_{j=1}^p m_{ij} \hat\sigma_{1,j}^2
#' \rho_1\left(\frac{x_{ij} - \hat{x}_{ij}}{\hat\sigma_{1,j}}\right)}}
#' where \eqn{m_{ij}} is 0 for a missing cell and 1 otherwise and \eqn{m_i}
#' is the number of observed cells of observation \eqn{i}. The bounded
#' function \eqn{\rho_1} limits the effect of the outlying cells (the
#' residuals of variable \eqn{j} divided by their scale
#' \eqn{\hat\sigma_{1,j}}), and \eqn{\rho_2} that of the outlying
#' observations (the casewise total deviations \eqn{t_i} divided by their
#' scale \eqn{\hat\sigma_2}). Both are hyperbolic tangent functions (Hampel
#' et al., 1981): \eqn{\rho_1} with \eqn{b = 1.5} and \eqn{c = 4}, and
#' \eqn{\rho_2} with \eqn{b} and \eqn{c} the 0.70 and 0.99 quantiles of the
#' standardized total deviations of simulated Gaussian residuals. Their
#' weights \eqn{\psi(z)/z} are 1 in the central region and 0 beyond
#' \eqn{c}.
#'
#' The algorithm follows the reference code of the authors:
#'  1. **Initial fit.** A [macropca()] fit (with `alpha`, `kmax`,
#'     `var_explained`, `scale` and `ndir`), which also chooses `k` when it
#'     is `NULL`, with its scores computed from the data whose flagged and
#'     missing cells are imputed by the fit.
#'  2. **Scales.** \eqn{\hat\sigma_{1,j}} is the M-scale of the residuals of
#'     variable \eqn{j}, and \eqn{\hat\sigma_2} that of the total deviations
#'     (with \eqn{\rho_{1.5,4}}: 50% breakdown, consistent at the normal
#'     distribution). They are kept fixed.
#'  3. **Iteratively reweighted least squares.** The loadings (one variable
#'     at a time, with the casewise and cellwise weights) and the scores
#'     (one observation at a time, with the cellwise weights) are updated by
#'     weighted least squares, the loadings are orthonormalized, the center
#'     is updated, and so are the weights, until the fit \eqn{U V^T} changes
#'     by less than `tol` (relative), at most `maxiter` times. Each iteration
#'     decreases the objective. If more than `max_col_frac` of the cells of
#'     a variable get a zero weight, the previous iteration is kept.
#'  4. **Principal directions.** The center and the eigenvectors within the
#'     subspace are estimated by the deterministic MCD of the scores of the
#'     observations with a non-zero casewise weight (the exact MCD when
#'     `k = 1`), and the sign of each loading vector is set so that its
#'     largest element is positive.
#'
#' The residuals of each variable are then standardized by their M-scale,
#' and the cells beyond \eqn{\sqrt{\chi^2_{1, 0.99}}} are flagged. As in the
#' enhanced outlier map of the paper, `od` is the norm of the standardized
#' residuals of each observation, and `sd` the score distance of its
#' projection on the subspace (of its robust scores when it has missing
#' cells). The cut-off of `sd` is \eqn{\sqrt{\chi^2_{k, 0.99}}}. That of
#' `od` is, by default (`od_cutoff = "simulated"`), the 0.99 quantile of the
#' `od` of a cellPCA fit to clean data simulated from the fit, as in the
#' reference code (this needs a second fit); `"chisq"` uses
#' \eqn{\sqrt{\chi^2_{p, 0.99}}} instead.
#'
#' New observations, with missing or outlying cells, are projected by the
#' robust regression of their observed cells on the loadings, also in C++.
#'
#' @inheritParams macropca
#' @param maxiter,tol The maximum number of iterations, and the tolerance on
#'   the relative change of the fit. Defaults are 1000 and 1e-6.
#' @param max_col_frac The largest fraction of cells of a variable that can
#'   get a zero weight. Default is 0.5.
#' @param od_cutoff How to compute the cut-off of `od`: `"simulated"`
#'   (default) or `"chisq"`. See Details.
#'
#' @return An object of class `specproc_cellpca` (inheriting from
#'   `specproc_robpca`), with the components described in [robpca()] and
#'   [macropca()], and:
#'  - `cell_weights`: the cellwise weights (0 for missing cells).
#'  - `case_weights`: the casewise weights.
#'  - `deviation`: the standardized casewise total deviations.
#'  - `fitted`: the fitted values \eqn{\hat{X}}.
#'  - `imputed`: the data in which the outlying cells are moved toward the
#'    fit in proportion to their weights, and the missing cells are
#'    replaced by the fit, so that the projection of each observation on
#'    the subspace is its fitted value.
#'  - `sigma1`, `sigma2`: the scales of the cellwise residuals and of the
#'    casewise total deviations; `resid_scale`: the scales of the final
#'    residuals.
#'  - `objective`: the objective at each iteration; `iterations` and
#'    `converged`.
#'
#' @references
#'  - Centofanti, F., Hubert, M., Rousseeuw, P.J. (2026). Robust principal
#'    components by casewise and cellwise weighting. Technometrics.
#'    \doi{10.1080/00401706.2026.2643216}
#'  - Hampel, F.R., Rousseeuw, P.J., Ronchetti, E. (1981). The change-of-
#'    variance curve and optimal redescending M-estimators. Journal of the
#'    American Statistical Association, 76(375):643-648.
#'  - Hubert, M., Rousseeuw, P.J., Van den Bossche, W. (2019). MacroPCA: an
#'    all-in-one PCA method allowing for missing values as well as cellwise
#'    and rowwise outliers. Technometrics, 61(4):459-473.
#'
#' @seealso [macropca()], [robpca()], [step_cellpca()], [plot_outlier_map()],
#'   [plot_cell_map()]
#'
#' @export cellpca
#'
#' @examples
#' data(forageLIBS)
#' # the K I resonance lines
#' wl <- suppressWarnings(as.numeric(names(forageLIBS)))
#' spectra <- forageLIBS[which(wl > 760 & wl < 780)]
#' set.seed(1)
#' fit <- cellpca(spectra, k = 2, od_cutoff = "chisq")
#' fit
#' # the observations with the lowest casewise weights
#' head(sort(fit$case_weights))
#' plot_outlier_map(fit)
cellpca <- function(x, k = NULL, alpha = 0.5, kmax = 10, var_explained = 0.8, scale = FALSE,
                    ndir = 250, maxiter = 1000, tol = 1e-6, max_col_frac = 0.5,
                    od_cutoff = c("simulated", "chisq")) {
  if (missing(x)) {
    stop("Missing 'x' argument.")
  }
  od_cutoff <- match.arg(od_cutoff)
  x <- as_numeric_matrix(x, "x")
  if (is.null(colnames(x))) colnames(x) <- paste0("V", seq_len(ncol(x)))
  check_count(maxiter, "maxiter")
  check_number(tol, "tol", lower = 0, lower_open = TRUE)
  check_number(max_col_frac, "max_col_frac", lower = 0, upper = 1)
  init <- macropca(x, k = k, alpha = alpha, kmax = kmax, var_explained = var_explained,
                   scale = scale, ndir = ndir)
  sc <- init$scale
  y <- sweep(x, 2, sc, "/")
  fit <- cellpca_fit(y, init, maxiter, tol, max_col_frac)
  k <- fit$k
  n <- nrow(x)
  p <- ncol(x)
  comp <- paste0("PC", seq_len(k))
  observed <- !is.na(y)

  res <- list(loadings = fit$loadings, eigenvalues = fit$eigenvalues, center = fit$center,
              scale = sc, k = as.integer(k), h = init$h, alpha = alpha,
              cutoff = sqrt(stats::qchisq(0.99, 1)), scores = fit$scores)
  dimnames(res$loadings) <- list(colnames(x), comp)
  dimnames(res$scores) <- list(rownames(x), comp)
  names(res$center) <- colnames(x)
  res$sd <- cellpca_sd(y, res)
  res$od <- sqrt(rowSums(fit$std_resid^2, na.rm = TRUE))
  res$std_resid <- fit$std_resid
  res$flagged_cells <- observed & abs(fit$std_resid) > res$cutoff
  res$flagged_cells[is.na(res$flagged_cells)] <- FALSE
  res$cell_weights <- fit$cell_weights
  res$case_weights <- fit$case_weights
  res$deviation <- fit$deviation
  res$fitted <- sweep(fit$fitted, 2, sc, "*")
  res$imputed <- sweep(fit$imputed, 2, sc, "*")
  dimnames(res$std_resid) <- dimnames(res$flagged_cells) <- dimnames(res$cell_weights) <-
    dimnames(res$fitted) <- dimnames(res$imputed) <- dimnames(x)
  res[c("sigma1", "sigma2", "resid_scale", "par2", "objective", "iterations", "converged")] <-
    fit[c("sigma1", "sigma2", "resid_scale", "par2", "objective", "iterations", "converged")]
  res$cutoff_sd <- sqrt(stats::qchisq(0.99, k))
  res$cutoff_od <- if (od_cutoff == "chisq") {
    sqrt(stats::qchisq(0.99, p))
  } else {
    cellpca_od_cutoff(fit, init, n, maxiter, tol, max_col_frac)
  }
  res$outlier_type <- outlier_type(res$sd, res$od, res$cutoff_sd, res$cutoff_od)
  structure(res, variables = colnames(x), nvar = p,
            class = c("specproc_cellpca", "specproc_robpca"))
}

# The cellPCA fit of the (scaled) data `y`, from the MacroPCA fit `init`.
cellpca_fit <- function(y, init, maxiter, tol, max_col_frac, b1 = 1.5) {
  n <- nrow(y)
  p <- ncol(y)
  k <- init$k
  observed <- !is.na(y)
  y0 <- y
  y0[!observed] <- 0

  # Step 1: initial fit, the MacroPCA scores of the fully imputed data
  V0 <- unname(init$loadings)
  mu0 <- unname(init$center)
  U0 <- unname(init$scores_imputed)

  # Step 2: scales of the cellwise residuals and of the casewise deviations
  resid0 <- y - sweep(U0 %*% t(V0), 2, mu0, "+")
  sigma1 <- scale_tanh_cols_cpp(resid0)
  t0 <- total_deviation(resid0, sigma1, b1)
  sigma2 <- scale_tanh_cols_cpp(matrix(t0[is.finite(t0)]))
  par2 <- cellpca_rho2_constants(p, b1)

  # Step 3: IRLS
  fit <- cellpca_irls_cpp(y0, observed * 1, V0, unname(U0), mu0, sigma1, sigma2, par2, b1,
                          as.integer(maxiter), tol, max_col_frac)
  if (isTRUE(fit$stopped)) {
    warning("A variable got zero weights in more than 'max_col_frac' of its cells; ",
            "the previous iteration was kept.", call. = FALSE)
  } else if (!fit$converged) {
    warning("cellPCA did not converge in ", maxiter, " iterations.", call. = FALSE)
  }

  # Step 4: center and directions from the MCD of the scores of the
  # observations with a non-zero casewise weight
  U <- fit$U
  V <- fit$V
  out <- fit$case_weights == 0
  keep <- if (any(out) && !all(out)) !out else rep(TRUE, n)
  alpha2 <- if (any(out) && !all(out)) min(0.5 * n / sum(keep), 0.8) else 0.5
  mcd <- cellpca_mcd(U[keep, , drop = FALSE], alpha2)
  e <- eigen(mcd$cov, symmetric = TRUE)
  center <- fit$mu + drop(V %*% mcd$center)
  loadings <- V %*% e$vectors
  scores <- sweep(U, 2, mcd$center) %*% e$vectors
  flip <- sign(diag(crossprod(loadings, V)))
  flip[flip == 0] <- 1
  # largest element of each loading vector positive
  flip <- flip * apply(loadings %*% diag(flip, k), 2, function(v) if (v[which.max(abs(v))] < 0) -1 else 1)
  loadings <- loadings %*% diag(flip, k)
  scores <- scores %*% diag(flip, k)

  # residuals standardized by their M-scale, and casewise deviations
  fitted <- sweep(scores %*% t(loadings), 2, center, "+")
  imputed <- fitted + ifelse(observed, fit$cell_weights * (y0 - fitted), 0)
  resid <- y - fitted
  resid_scale <- scale_tanh_cols_cpp(resid)
  std_resid <- sweep(resid, 2, resid_scale, "/")
  deviation <- total_deviation(resid, resid_scale, b1) / sigma2
  list(k = k, loadings = loadings, eigenvalues = e$values, center = center, scores = scores,
       fitted = fitted, imputed = imputed, std_resid = std_resid, deviation = deviation,
       cell_weights = fit$cell_weights, case_weights = fit$case_weights, sigma1 = sigma1,
       sigma2 = sigma2, resid_scale = resid_scale, par2 = par2, objective = fit$objective,
       iterations = fit$iterations, converged = fit$converged)
}

# Score distances of a cellPCA fit: those of the projections of the
# observations, or of their robust scores when they have missing cells.
cellpca_sd <- function(y, fit) {
  proj <- sweep(y, 2, fit$center) %*% fit$loadings
  incomplete <- !stats::complete.cases(proj)
  proj[incomplete, ] <- fit$scores[incomplete, , drop = FALSE]
  sqrt(rowSums(sweep(proj^2, 2, fit$eigenvalues, "/")))
}

# Cut-off of the norms of the standardized residuals: their 0.99 quantile
# for a cellPCA fit to clean data simulated from the fit (at most 500
# observations), as in the reference code.
cellpca_od_cutoff <- function(fit, init, n, maxiter, tol, max_col_frac) {
  with_seed(0, {
    n_clean <- min(n, 500)
    index <- sample(seq_len(n), n_clean, replace = TRUE)
    noise <- matrix(stats::rnorm(n_clean * length(fit$resid_scale)), n_clean) *
      rep(fit$resid_scale, each = n_clean)
    clean <- (fit$scores %*% t(fit$loadings))[index, , drop = FALSE] + noise
    colnames(clean) <- rownames(fit$loadings) %||% paste0("V", seq_len(ncol(clean)))
    init_clean <- suppressWarnings(macropca(clean, k = fit$k, alpha = init$alpha))
    fit_clean <- suppressWarnings(cellpca_fit(clean, init_clean, maxiter, tol, max_col_frac))
    unname(stats::quantile(sqrt(rowSums(fit_clean$std_resid^2, na.rm = TRUE)), 0.99))
  })
}

# Center and scatter of the cellPCA scores: deterministic MCD, or the exact
# univariate MCD for one component (the deterministic MCD does not handle
# one variable).
cellpca_mcd <- function(scores, alpha) {
  mcd <- if (ncol(scores) == 1) {
    robustbase::covMcd(scores, alpha = alpha)
  } else {
    tryCatch(robustbase::covMcd(scores, alpha = alpha, nsamp = "deterministic", use.correction = TRUE),
             error = function(e) robustbase::covMcd(scores, alpha = alpha))
  }
  list(center = unname(mcd$center), cov = unname(as.matrix(mcd$cov)))
}

# Tuning constants (b, c, q1, q2) of rho_2, from the standardized total
# deviations of simulated Gaussian residuals of min(p/2, 10) variables, as
# get_tuning_const_rho2() of the reference code.
cellpca_rho2_constants <- local({
  cache <- list()
  function(p, b1 = 1.5) {
    d <- ceiling(min(p / 2, 10))
    key <- paste(d, b1)
    if (is.null(cache[[key]])) {
      cache[[key]] <<- with_seed(10, {
        n <- 10000
        r <- matrix(stats::rnorm(n * d), n, d)
        s1 <- scale_tanh_cols_cpp(r)
        rho <- matrix(rho_tanh_cpp(sweep(r, 2, s1, "/"), b1), n) * rep(s1^2, each = n)
        t <- sqrt(rowSums(rho) / d)
        t <- t / scale_tanh_cols_cpp(matrix(t))
        b <- max(stats::quantile(t, 0.7), 0)
        c <- max(stats::quantile(t, 0.99), 0.3)
        q <- if (b < 100) tanh_q1q2(b, c) else c(NA_real_, NA_real_)
        unname(c(b, c, q))
      })
    }
    cache[[key]]
  }
})

# Constants q1 and q2 that make the tanh psi function continuous for given b
# and c (calculateq1q2() of the reference code).
tanh_q1q2 <- function(b, c, maxit = 500, prec = 1e-10) {
  psi_wrap <- function(x, A, B, k) {
    mid <- abs(x) >= b & abs(x) <= c
    up <- abs(x) >= c
    x[mid] <- sqrt(A * (k - 1)) * tanh(0.5 * sqrt((k - 1) * B^2 / A) * (c - abs(x[mid]))) * sign(x[mid])
    x[up] <- 0
    x
  }
  A <- 2 * stats::pnorm(c) - 1 - 2 * c * stats::dnorm(c)
  B <- 2 * stats::pnorm(c) - 1
  k <- max(1, c)
  for (iter in seq_len(maxit)) {
    k_new <- stats::optimize(function(y) abs(b - sqrt(A * (y - 1)) * tanh(0.5 * sqrt((y - 1) * B^2 / A) * (c - b))),
                             interval = c(1 + prec, 1000), tol = prec)$minimum
    A_new <- stats::integrate(function(y) psi_wrap(y, A, B, k)^2 * stats::dnorm(y), -c, c)$value
    B_new <- stats::integrate(function(y) abs(psi_wrap(y, A, B, k)) * abs(y) * stats::dnorm(y), -c, c)$value
    converged <- max(abs(A_new - A), abs(B_new - B), abs(k_new - k)) < prec
    A <- A_new
    B <- B_new
    k <- k_new
    if (converged) break
  }
  c(sqrt(A * (k - 1)), B / 2 * sqrt((k - 1) / A))
}

# Casewise total deviations (10): residuals `r` (NA for missing cells) with
# the scales `sigma` of the variables.
total_deviation <- function(r, sigma, b1 = 1.5) {
  z <- sweep(r, 2, sigma, "/")
  z[!is.finite(z) & !is.na(r)] <- 0
  rho <- matrix(rho_tanh_cpp(z, b1), nrow(r)) * rep(sigma^2, each = nrow(r))
  sqrt(rowMeans(rho, na.rm = TRUE))
}

# Size of the subsets of robust PCA (h.alpha.n of cellWise and rrcov).
h_alpha_n <- function(alpha, n, p) {
  n2 <- (n + p + 1) %/% 2
  floor(2 * n2 - n + 2 * (n - n2) * alpha)
}

# Classical PCA (truncPC of cellWise): center, the loadings and eigenvalues
# of the `ncomp` (default all) first components with a non-zero singular
# value, the largest element of each loading vector positive.
trunc_pc <- function(y, ncomp = NULL) {
  y <- as.matrix(y)
  center <- colMeans(y)
  s <- svd(sweep(y, 2, center), nu = 0, nv = min(ncomp %||% min(dim(y)), dim(y)))
  rank <- sum(s$d[seq_len(ncol(s$v))] > 1e-10)
  v <- s$v[, seq_len(rank), drop = FALSE]
  v <- sweep(v, 2, apply(v, 2, function(a) if (a[which.max(abs(a))] < 0) -1 else 1), "*")
  list(rank = rank, eigenvalues = s$d[seq_len(rank)]^2 / (nrow(y) - 1), loadings = v,
       center = center)
}

# Largest angle between two subspaces, as a fraction of a right angle
# (maxAngle of cellWise).
max_angle <- function(a, b) {
  lambda <- min(eigen(crossprod(a, b) %*% crossprod(b, a), symmetric = TRUE, only.values = TRUE)$values)
  acos(sqrt(min(max(lambda, 0), 1))) / (pi / 2)
}

mahalanobis_diag <- function(scores, values) {
  rowSums(sweep(scores^2, 2, values, "/"))
}

# Cut-off of the orthogonal distances: Wilson-Hilferty approximation with
# the reweighted univariate MCD of OD^(2/3) at the 0.99 quantile (critOD of
# cellWise).
od_cutoff_unimcd <- function(od, alpha) {
  u <- unimcd_cpp(od^(2 / 3), alpha)
  (u[["location"]] + u[["scale"]] * stats::qnorm(0.99))^(3 / 2)
}

# Outlyingness of each row: the largest standardized distance (with the
# reweighted univariate MCD) over directions through pairs of rows, chosen
# by the deterministic generator of cellWise (all pairs when there are at
# most `ndir`).
pp_outlyingness <- function(y, ndir, alpha) {
  n <- nrow(y)
  all_pairs <- choose(n, 2)
  ndir <- if (identical(ndir, "all")) all_pairs else min(ndir, all_pairs)
  pairs <- if (ndir == all_pairs) t(utils::combn(n, 2)) else direction_pairs(n, ndir)
  B <- y[pairs[, 1], , drop = FALSE] - y[pairs[, 2], , drop = FALSE]
  norms <- sqrt(rowSums(B^2))
  B <- B[norms > 1e-12, , drop = FALSE] / norms[norms > 1e-12]
  proj <- y %*% t(B)
  out <- numeric(n)
  for (j in seq_len(ncol(proj))) {
    u <- unimcd_cpp(proj[, j], alpha)
    if (u[["scale"]] > 1e-12) out <- pmax(out, abs(proj[, j] - u[["location"]]) / u[["scale"]])
  }
  out
}

# Pairs of rows of the directions (randomset() of cellWise and rrcov).
direction_pairs <- function(n, ndir) {
  seed <- 0
  draw <- function() {
    seed <<- floor(seed * 5761) + 999
    quot <- floor(seed / 65536)
    seed <<- floor(seed) - floor(quot * 65536)
    floor(seed / 65536 * n) + 1
  }
  out <- matrix(0L, ndir, 2)
  for (r in seq_len(ndir)) {
    a <- draw()
    b <- draw()
    while (b == a) b <- draw()
    out[r, ] <- c(a, b)
  }
  out
}

# Center and scatter of the scores within the subspace: the deterministic
# MCD when its criterion is below that of the C-steps (as in cellWise),
# otherwise the reweighted covariance of the C-steps.
final_mcd <- function(scores, alpha, obj, mah, k) {
  if (k > 1) {
    mcd <- tryCatch(robustbase::covMcd(scores, nsamp = "deterministic", alpha = alpha),
                    error = function(e) NULL)
  } else {
    # the deterministic MCD does not handle one variable: the exact
    # univariate MCD
    mcd <- robustbase::covMcd(scores, alpha = alpha)
  }
  if (!is.null(mcd) && mcd$crit < obj + 1e-16) {
    return(list(center = unname(mcd$center), cov = unname(as.matrix(mcd$cov))))
  }
  mah <- mah / (stats::median(mah) / stats::qchisq(0.5, k))
  w <- stats::cov.wt(scores, wt = as.numeric(mah <= stats::qchisq(0.975, k)), method = "ML")
  list(center = unname(w$center), cov = unname(w$cov))
}

# Robust location (1-step biweight) and scale (1-step Huber) of each column.
loc_scale_1step <- function(x) {
  loc <- apply(x, 2, function(v) {
    v <- v[is.finite(v)]
    m0 <- stats::median(v)
    s0 <- stats::mad(v, center = m0)
    if (s0 <= 1e-12) return(m0)
    u <- 1 - ((v - m0) / s0 * 1.482602218505602 / 3)^2
    w <- ((u + abs(u)) / 2)^2
    sum(v * w) / sum(w)
  })
  list(loc = loc, scale = scale_1step_cols_cpp(sweep(x, 2, loc)))
}

# Fitted values of a PCA model.
pca_fitted <- function(y, center, loadings) {
  scores <- sweep(y, 2, center) %*% loadings
  sweep(scores %*% t(loadings), 2, center, "+")
}

# Scores, distances and standardized residuals of a MacroPCA fit, for the
# NA-imputed data `y` and the observed data `y_obs` (NA for missing cells).
# The residual scales are those of the fit when it has them (new data).
macropca_distances <- function(fit, y, y_obs) {
  scores <- sweep(y, 2, fit$center) %*% fit$loadings
  fitted <- sweep(scores %*% t(fit$loadings), 2, fit$center, "+")
  resid <- y_obs - fitted
  resid_scale <- fit$resid_scale
  if (is.null(resid_scale)) {
    resid_scale <- if (fit$k < fit$rank) scale_1step_cols_cpp(resid) else rep(0, ncol(y))
  }
  std_resid <- if (fit$k < fit$rank) sweep(resid, 2, resid_scale, "/") else resid * 0
  std_resid[!is.finite(std_resid) & !is.na(y_obs)] <- 0
  flagged <- !is.na(std_resid) & abs(std_resid) > fit$cutoff
  dimnames(std_resid) <- dimnames(flagged) <- dimnames(y_obs)
  colnames(scores) <- paste0("PC", seq_len(ncol(scores)))
  list(scores = scores,
       sd = sqrt(mahalanobis_diag(scores, fit$eigenvalues)),
       od = orthogonal_distance(sweep(y, 2, fit$center), fit$loadings),
       std_resid = std_resid, flagged_cells = flagged, resid_scale = resid_scale)
}

#' @rdname predict.specproc_robpca
#' @export
predict.specproc_macropca <- function(object, newdata, ...) {
  x <- as_numeric_matrix(newdata, "newdata")
  if (ncol(x) != attr(object, "nvar")) {
    stop("'newdata' must have ", attr(object, "nvar"), " columns, like the calibration data.", call. = FALSE)
  }
  colnames(x) <- attr(object, "variables")
  # MacroPCApredict of cellWise: the cells flagged by the DDC model of the
  # fit and the missing cells are imputed, then re-imputed by the fit until
  # they no longer change; the distances use only the missing imputations
  cells <- predict_ddc(object$ddc, x)
  sc <- object$scale
  x_obs <- sweep(x, 2, sc, "/")
  imputable <- cells$flagged_cells | is.na(x)
  x_fi <- x_obs
  x_fi[imputable] <- (cells$imputed / rep(sc, each = nrow(x)))[imputable]
  if (any(imputable)) {
    for (iter in seq_len(20)) {
      old <- x_fi[imputable]
      x_fi[imputable] <- pca_fitted(x_fi, object$center, object$loadings)[imputable]
      if (mean((x_fi[imputable] - old)^2) <= 0.005) break
    }
  }
  x_na <- x_obs
  x_na[is.na(x)] <- x_fi[is.na(x)]
  dist <- macropca_distances(object, x_na, x_obs)
  robust_pca_output(object, dist$scores, dist$od, dist$sd)
}

#' @rdname predict.specproc_robpca
#' @export
predict.specproc_cellpca <- function(object, newdata, ...) {
  x <- as_numeric_matrix(newdata, "newdata")
  if (ncol(x) != attr(object, "nvar")) {
    stop("'newdata' must have ", attr(object, "nvar"), " columns, like the calibration data.", call. = FALSE)
  }
  y <- sweep(x, 2, object$scale, "/")
  observed <- !is.na(y)
  y0 <- y
  y0[!observed] <- 0
  # robust regression of the observed cells on the loadings (from the
  # projection), with the cellwise weights of the fit
  pred <- cellpca_predict_cpp(y0, observed * 1, unname(object$loadings), unname(object$center),
                              object$sigma1, 1.5, 1000L, 1e-6)
  scores <- pred$scores
  fit <- object
  fit$scores <- scores
  fitted <- sweep(scores %*% t(object$loadings), 2, object$center, "+")
  std_resid <- sweep(y - fitted, 2, object$resid_scale, "/")
  od <- sqrt(rowSums(std_resid^2, na.rm = TRUE))
  od[rowSums(observed) == 0] <- NA_real_
  robust_pca_output(object, scores, od, cellpca_sd(y, fit))
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
#'
#' @examples
#' data(forageLIBS)
#' wl <- suppressWarnings(as.numeric(names(forageLIBS)))
#' spectra <- forageLIBS[which(wl > 760 & wl < 780)]  # the K I resonance lines
#' set.seed(1)
#' fit <- robpca(spectra[1:300, ])
#' head(predict(fit, spectra[301:368, ]))
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
    specproc_macropca = "Robust PCA for cellwise and casewise outliers (MacroPCA)",
    specproc_cellpca = "Robust PCA by casewise and cellwise weighting (cellPCA)"
  )
  cat(label, "\n\n", sep = "")
  cat("Observations:   ", length(x$sd), " (h = ", x$h, ")\n", sep = "")
  cat("Variables:      ", attr(x, "nvar"), "\n", sep = "")
  cat("Components:     ", x$k, "\n", sep = "")
  cat("Eigenvalues:    ", paste(format(x$eigenvalues, digits = 4), collapse = " "), "\n", sep = "")
  if (inherits(x, "specproc_rospca")) {
    cat("Non-zero loadings per component: ", paste(colSums(x$loadings != 0), collapse = " "), "\n", sep = "")
  }
  if (inherits(x, c("specproc_macropca", "specproc_cellpca"))) {
    cat("Flagged cells:  ", sum(x$flagged_cells), "\n", sep = "")
  }
  if (inherits(x, "specproc_cellpca")) {
    cat("Downweighted observations: ", sum(x$case_weights < 1), "\n", sep = "")
    cat("IRLS iterations: ", x$iterations, if (!isTRUE(x$converged)) " (not converged)", "\n", sep = "")
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

# Stahel-Donoho outlyingness of each observation.
outlyingness <- function(z, h, ndir) {
  if (identical(ndir, "all")) {
    ndir <- 0L
  } else {
    check_count(ndir, "ndir")
  }
  if (ncol(z) == 1) {
    u <- univariate_mcd_cpp(z[, 1], h)
    abs(z[, 1] - u[["location"]]) / max(u[["scale"]], .Machine$double.eps)
  } else {
    sd_outlyingness_cpp(z, h, as.integer(ndir))
  }
}

# Indices of the h observations with the smallest Stahel-Donoho outlyingness.
least_outlying <- function(z, h, ndir) {
  sort(order(outlyingness(z, h, ndir))[seq_len(h)])
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
# univariate MCD of OD^(2/3), at the given level.
od_cutoff <- function(od, h, level = 0.975) {
  if (max(od) <= sqrt(.Machine$double.eps) * max(1, max(od))) {
    return(0)
  }
  u <- univariate_mcd_cpp(od^(2 / 3), min(h, length(od)))
  (u[["location"]] + u[["scale"]] * stats::qnorm(level))^(3 / 2)
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
