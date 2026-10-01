#' @title Orthogonal Projections to Latent Structures
#'
#' @author Christian L. Goueguel
#'
#' @description
#'  This function fits an Orthogonal Projections to Latent Structures (OPLS)
#'  model to the provided x (predictor) and y (response) data.
#'
#' @details
#'  OPLS is a supervised modeling technique used to find the
#'  multidimensional direction in the x-space that explains the maximum
#'  multidimensional variance in the y-space. It separates the systematic
#'  variation in x into two parts: one that is linearly related to y
#'  (predictive components) and one that is statistically uncorrelated to the
#'  response variable y (orthogonal components).
#'
#'  The model has one predictive component and `ncomp.ortho` orthogonal
#'  components, fitted with the NIPALS algorithm of Trygg and Wold (2002). For
#'  each orthogonal component:
#'  1. The weight \eqn{\textbf{w} = \textbf{X}^T\textbf{y}/\|\textbf{X}^T\textbf{y}\|},
#'     the score \eqn{\textbf{t} = \textbf{Xw}} and the loading
#'     \eqn{\textbf{p} = \textbf{X}^T\textbf{t}/(\textbf{t}^T\textbf{t})} are computed.
#'  2. The orthogonal weight is the part of \eqn{\textbf{p}} orthogonal to
#'     \eqn{\textbf{w}}, \eqn{\textbf{w}_o = \textbf{p} - (\textbf{w}^T\textbf{p})\textbf{w}},
#'     normalized, with score \eqn{\textbf{t}_o = \textbf{Xw}_o} and loading
#'     \eqn{\textbf{p}_o = \textbf{X}^T\textbf{t}_o/(\textbf{t}_o^T\textbf{t}_o)}.
#'  3. \eqn{\textbf{X}} is deflated by \eqn{\textbf{t}_o\textbf{p}_o^T}.
#'
#'  The predictive component is then computed from the filtered
#'  \eqn{\textbf{X}}. The filtered data are the same as those of
#'  [projected_osc()] with `ncomp = ncomp.ortho + 1`, and of [o2pls()].
#'
#'  **Cross-validation.** The observations are split into `crossval`
#'  interleaved groups (observation `i` is in group `(i - 1) %% crossval + 1`).
#'  Each group is predicted by a model fitted to the others, with the same
#'  number of orthogonal components, giving the predictive residual sum of
#'  squares (PRESS) and \eqn{Q^2 = 1 - \text{PRESS}/\text{SS}_y}, where
#'  \eqn{\text{SS}_y} is the total sum of squares of the preprocessed y.
#'  The data are preprocessed once, with the centers and scales of all the
#'  observations.
#'
#'  **Number of orthogonal components.** If `ncomp.ortho = NA`, components
#'  are added (up to `min(10, n, p) - 1`) while each is significant: a
#'  component is significant if it increases \eqn{R^2Y} by at least 0.01 and
#'  \eqn{Q^2} by at least 0.01. If the predictive component alone is not
#'  significant, no model is built. If the first orthogonal component is not
#'  significant, the model has no orthogonal component (a one-component PLS
#'  model), with a warning.
#'
#'  **Permutation test.** The response is permuted `permutation` times and
#'  the model refitted with the same number of components. `pR2Y` and `pQ2`
#'  are the proportions of permuted models whose \eqn{R^2Y} and \eqn{Q^2}
#'  are at least those of the model, \eqn{(1 + \#\{\text{perm} \geq \text{model}\})/\text{permutation}}.
#'  Use [set.seed()] for reproducible p-values.
#'
#'  The results reproduce those of `ropls::opls()` with `predI = 1` and the
#'  same `scaleC`, `crossvalI`, `orthoI` and `permI` (apart from rounding):
#'  specProc no longer depends on ropls. Unlike ropls, variables with zero
#'  variance are kept (their centered values are zero), and when
#'  `ncomp.ortho = NA` a model without orthogonal components is returned
#'  instead of no model when only the predictive component is significant.
#'
#' @references
#'  - Trygg, J., and Wold, S., (2002).
#'    Orthogonal projections to latent structures (O-PLS).
#'    Journal of Chemometrics, 16(3):119-128.
#'  - Galindo-Prieto, B., Eriksson, L., and Trygg, J., (2014).
#'    Variable influence on projection (VIP) for orthogonal projections to
#'    latent structures (OPLS). Journal of Chemometrics, 28(8):623-632.
#'  - Thévenot, E.A., Roux, A., Xu, Y., Ezan, E., and Junot, C., (2015).
#'    Analysis of the human adult urinary metabolome variations with age, body
#'    mass index, and gender by implementing a comprehensive workflow for
#'    univariate and OPLS statistical analyses. Journal of Proteome Research,
#'    14(8):3322-3335.
#'
#' @param x A numeric matrix or data frame of the predictor variables.
#' @param y A numeric vector, or a matrix or data frame with one column, of the response variable.
#' @param scale A character string indicating the scaling method for x and y: "none", "center" (default), "pareto" (divided by the square root of the standard deviation) or "standard" (divided by the standard deviation).
#' @param crossval An integer giving the number of cross-validation groups (default 7), between 2 and the number of observations. With `crossval = 0`, the model is not cross-validated (\eqn{Q^2} is `NA`), which requires a fixed `ncomp.ortho` and no permutation.
#' @param permutation An integer giving the number of permutations for the permutation test. Default is 20; 0 skips the test.
#' @param ncomp.ortho The number of orthogonal components. If `NA` (default), it is determined automatically by cross-validation.
#'
#' @return An object of class `specproc_opls` (a list), which [predict()][predict.specproc_opls] applies to new data, with the following components:
#' \describe{
#'   \item{x_scores}{The predictive scores \eqn{\textbf{t}} (one column, `p1`).}
#'   \item{x_loadings}{The predictive loadings \eqn{\textbf{p}}.}
#'   \item{x_weights}{The predictive weights \eqn{\textbf{w}}.}
#'   \item{orthoScores}{The orthogonal scores \eqn{\textbf{T}_o} (columns `o1`, `o2`, ...).}
#'   \item{orthoLoadings}{The orthogonal loadings \eqn{\textbf{P}_o}.}
#'   \item{orthoWeights}{The orthogonal weights \eqn{\textbf{W}_o}.}
#'   \item{y_weights}{The y-weight \eqn{c} of the predictive component.}
#'   \item{y_scores}{The y-scores \eqn{\textbf{u} = \textbf{y}/c}.}
#'   \item{correction}{The OPLS-filtered x, \eqn{\textbf{X} - \textbf{T}_o\textbf{P}_o^T} (preprocessed).}
#'   \item{fitted}{The fitted response, in the units of y.}
#'   \item{coefficients}{The regression coefficients of the preprocessed filtered x, \eqn{\textbf{w}c}.}
#'   \item{vip, ortho_vip}{The predictive and orthogonal variable importance in projection (Galindo-Prieto et al., 2014), one value per variable.}
#'   \item{components}{A tibble with one row per component: `R2X`, `R2Y` and `Q2` (the increase due to the component), their cumulative values, and `significance` (`"R1"` significant, `"NS"` \eqn{Q^2} increase below 0.01, `"N4"` \eqn{R^2Y} increase below 0.01).}
#'   \item{summary}{A one-row data frame with `R2X(cum)`, `R2Y(cum)`, `Q2(cum)`, `RMSEE` (root mean squared error of estimation), the numbers of predictive (`pre`) and orthogonal (`ort`) components and, with a permutation test, `pR2Y` and `pQ2`.}
#'   \item{permutation}{With a permutation test, a tibble of `R2Y(cum)`, `Q2(cum)` and `sim` (the correlation between the permuted and the original response) of the model (first row) and of each permuted model.}
#'   \item{center, scale}{The column centers and scales applied to x.}
#'   \item{y_center, y_scale}{The center and scale applied to y.}
#' }
#'
#' @seealso [predict.specproc_opls()], and [step_opls()] to use the OPLS
#'   filter in a tidymodels recipe.
#'
#' @export opls
#'
#' @examples
#' data(forageLIBS)
#' spectra <- forageLIBS[-(1:14)]  # the spectral channels
#' cal <- 1:300
#' fit <- opls(spectra[cal, ], forageLIBS$K[cal], permutation = 5)
#' fit
#' head(predict(fit, spectra[-cal, ], type = "response"))
opls <- function(x, y, scale = "center", crossval = 7, permutation = 20, ncomp.ortho = NA) {
  if (missing(x) || missing(y) || is.null(x) || is.null(y)) {
    stop("Both 'x' and 'y' must be provided.", call. = FALSE)
  }
  scale <- match.arg(scale, c("none", "center", "pareto", "standard"))
  x <- as_numeric_matrix(x, "x")
  y <- as_response_matrix(y, nrow(x), "y")
  if (ncol(y) != 1) {
    stop("'y' must be a single response variable.", call. = FALSE)
  }
  if (anyNA(x) || anyNA(y)) {
    stop("'x' and 'y' cannot contain missing values.", call. = FALSE)
  }
  n <- nrow(x)
  if (n < 3) {
    stop("At least 3 observations are required.", call. = FALSE)
  }
  check_count(crossval, "crossval", lower = 0)
  if (crossval == 1 || crossval > n) {
    stop("'crossval' must be 0 or between 2 and the number of observations (", n, ").", call. = FALSE)
  }
  check_count(permutation, "permutation", lower = 0)
  auto <- length(ncomp.ortho) == 1 && is.na(ncomp.ortho)
  max_ortho <- min(10, n, ncol(x)) - 1
  if (auto) {
    if (max_ortho < 1) {
      stop("Too few observations or variables to select the number of orthogonal components.", call. = FALSE)
    }
    ncomp.ortho <- max_ortho
  } else {
    check_count(ncomp.ortho, "ncomp.ortho", lower = 0)
    if (ncomp.ortho + 1 > min(n, ncol(x))) {
      stop("'ncomp.ortho + 1' cannot exceed min(n, number of predictors).", call. = FALSE)
    }
  }
  if (crossval == 0 && (auto || permutation > 0)) {
    stop("'crossval = 0' requires a fixed 'ncomp.ortho' and 'permutation = 0'.", call. = FALSE)
  }

  px <- opls_preprocess(x, scale)
  py <- opls_preprocess(y, scale)
  xs <- px$x
  ys <- drop(py$x)
  folds <- if (crossval > 0) split(seq_len(n), rep(seq_len(crossval), length.out = n)) else list()

  fit <- opls_core(xs, ys, ncomp.ortho, folds, auto)
  k <- fit$n_ortho

  # Model statistics
  ssx <- sum(xs^2)
  r2x_ortho <- colSums(fit$to^2) * colSums(fit$po^2) / ssx
  r2x <- c(sum(fit$t^2) * sum(fit$p^2) / ssx, r2x_ortho)
  comp_names <- c("p1", if (k > 0) paste0("o", seq_len(k)))
  r2y_cum <- fit$r2y[seq_len(k + 1)]
  q2_cum <- fit$q2[seq_len(k + 1)]
  components <- tibble::tibble(
    component = comp_names,
    R2X = r2x, `R2X(cum)` = cumsum(r2x),
    R2Y = diff(c(0, r2y_cum)), `R2Y(cum)` = r2y_cum,
    Q2 = diff(c(0, q2_cum)), `Q2(cum)` = q2_cum,
    significance = fit$signif[seq_len(k + 1)]
  )

  fitted <- drop(fit$t) * fit$c * py$scale + py$center
  rmsee <- sqrt(mean((y[, 1] - fitted)^2) * n / (n - (2 + k)))
  summary <- data.frame(
    `R2X(cum)` = sum(r2x), `R2Y(cum)` = r2y_cum[k + 1], `Q2(cum)` = q2_cum[k + 1],
    RMSEE = rmsee, pre = 1L, ort = as.integer(k),
    row.names = "Total", check.names = FALSE
  )

  perm <- NULL
  if (permutation > 0) {
    perm <- matrix(NA_real_, permutation + 1, 3, dimnames = list(NULL, c("R2Y(cum)", "Q2(cum)", "sim")))
    perm[1, ] <- c(summary$`R2Y(cum)`, summary$`Q2(cum)`, 1)
    for (i in seq_len(permutation)) {
      yp <- sample(ys)
      pf <- opls_core(xs, yp, k, folds, auto = FALSE)
      perm[i + 1, ] <- c(pf$r2y[k + 1], pf$q2[k + 1], stats::cor(ys, yp))
    }
    summary$pR2Y <- (1 + sum(perm[-1, 1] >= perm[1, 1])) / permutation
    summary$pQ2 <- (1 + sum(perm[-1, 2] >= perm[1, 2])) / permutation
    perm <- tibble::as_tibble(perm)
  }

  vip <- opls_vip(fit, ys)
  vars <- colnames(x)
  ortho_names <- if (k > 0) paste0("o", seq_len(k)) else character()
  ortho_tbl <- function(m) as_tbl(m[, seq_len(k), drop = FALSE], ortho_names)
  filtered <- xs - tcrossprod(fit$to, fit$po)

  res <- list(
    x_scores = as_tbl(fit$t, "p1"),
    x_loadings = as_tbl(fit$p, "p1"),
    x_weights = as_tbl(fit$w, "p1"),
    orthoScores = ortho_tbl(fit$to),
    orthoLoadings = ortho_tbl(fit$po),
    orthoWeights = ortho_tbl(fit$wo),
    y_weights = as_tbl(matrix(fit$c), "p1"),
    y_scores = as_tbl(ys / fit$c, "p1"),
    correction = as_tbl(filtered, vars),
    fitted = fitted,
    coefficients = stats::setNames(drop(fit$w) * fit$c, vars),
    vip = stats::setNames(vip$vip, vars),
    ortho_vip = if (k > 0) stats::setNames(vip$ortho, vars) else NULL,
    components = components,
    summary = summary,
    permutation = perm,
    center = px$center,
    scale = px$scale,
    y_center = py$center,
    y_scale = py$scale
  )
  new_filter(res, "specproc_opls", x, scaling = scale)
}

# Centers and scales the columns as ropls does: "pareto" divides by the square
# root of the standard deviation. Constant columns are left unscaled.
opls_preprocess <- function(x, scale) {
  mu <- if (scale == "none") rep(0, ncol(x)) else colMeans(x)
  sdev <- switch(
    scale,
    none = , center = rep(1, ncol(x)),
    pareto = sqrt(apply(x, 2, stats::sd)),
    standard = apply(x, 2, stats::sd)
  )
  sdev[!is.finite(sdev) | sdev == 0] <- 1
  list(x = apply_preprocess(x, list(center = mu, scale = sdev)), center = mu, scale = sdev)
}

# Fits the OPLS model of the preprocessed xs and ys with up to n_ortho
# orthogonal components, cross-validating each stage on `folds`.
#
# The deflated matrices are never formed: a block of rows `r` of the deflated
# x is X[r, ] - To Po^T, with To the orthogonal scores of those rows and Po
# the orthogonal loadings of the model that deflated them. This keeps the
# memory at that of x, however many folds.
opls_core <- function(xs, ys, n_ortho, folds, auto) {
  n <- nrow(xs)
  p <- ncol(xs)
  ssy <- sum(ys^2)

  block <- function(rows) list(rows = rows, to = matrix(0, length(rows), 0), po = matrix(0, p, 0))
  # (X_r - To Po^T) w, from xs_w = X w of all the rows
  xw <- function(b, w, xs_w = xs %*% w) xs_w[b$rows, , drop = FALSE] - b$to %*% crossprod(b$po, w)
  # (X_r - To Po^T)^T v
  xtv <- function(b, v) {
    vf <- numeric(n)
    vf[b$rows] <- v
    crossprod(xs, vf) - b$po %*% crossprod(b$to, v)
  }
  # Predictive component of a block and, if `ortho`, its orthogonal
  # component. The products with all the rows (xs_w, xs_wo) are kept to
  # predict the test rows of a fold.
  stage <- function(b, yb, ortho) {
    w <- xtv(b, yb)
    w <- w / sqrt(sum(w^2))
    xs_w <- xs %*% w
    t <- xw(b, w, xs_w)
    tt <- sum(t^2)
    pl <- xtv(b, t) / tt
    s <- list(w = w, xs_w = xs_w, t = t, c = sum(yb * t) / tt, p = pl)
    if (ortho) {
      wo <- pl - sum(w * pl) * w
      s$wo <- wo / sqrt(sum(wo^2))
      s$xs_wo <- xs %*% s$wo
      s$to <- xw(b, s$wo, s$xs_wo)
      s$po <- xtv(b, s$to) / sum(s$to^2)
      s$co <- sum(yb * s$to) / sum(s$to^2)
    }
    s
  }

  full <- block(seq_len(n))
  train <- lapply(folds, function(f) block(seq_len(n)[-f]))
  test <- lapply(folds, block)
  r2y <- q2 <- rep(NA_real_, n_ortho + 1)
  signif <- rep(NA_character_, n_ortho + 1)
  wo <- matrix(0, p, 0)
  co <- numeric()
  pred <- NULL
  stop_at <- NA

  for (h in seq_len(n_ortho + 1)) {
    if (length(folds) > 0) {
      press <- 0
      for (k in seq_along(folds)) {
        s <- stage(train[[k]], ys[train[[k]]$rows], ortho = h <= n_ortho)
        yt <- ys[folds[[k]]]
        press <- press + sum((yt - xw(test[[k]], s$w, s$xs_w) * s$c)^2)
        if (h <= n_ortho) {
          train[[k]]$to <- cbind(train[[k]]$to, s$to)
          train[[k]]$po <- cbind(train[[k]]$po, s$po)
          test[[k]]$to <- cbind(test[[k]]$to, xw(test[[k]], s$wo, s$xs_wo))
          test[[k]]$po <- train[[k]]$po
        }
      }
      q2[h] <- 1 - press / ssy
    }
    s <- stage(full, ys, ortho = h <= n_ortho)
    r2y[h] <- sum((s$t * s$c)^2) / ssy
    d_r2y <- r2y[h] - if (h > 1) r2y[h - 1] else 0
    d_q2 <- q2[h] - if (h > 1) q2[h - 1] else 0
    signif[h] <- if (d_r2y < 0.01) "N4" else if (!is.na(d_q2) && d_q2 < 0.01) "NS" else "R1"
    if (auto && signif[h] != "R1") {
      stop_at <- h
      break
    }
    pred <- s
    if (h <= n_ortho) {
      full$to <- cbind(full$to, s$to)
      full$po <- cbind(full$po, s$po)
      wo <- cbind(wo, s$wo)
      co <- c(co, s$co)
    }
  }

  k <- n_ortho
  if (auto && !is.na(stop_at)) {
    if (stop_at == 1) {
      stop("No OPLS model could be built: the predictive component is not significant. ",
           "Set 'ncomp.ortho' to force the number of orthogonal components.", call. = FALSE)
    }
    k <- stop_at - 2
    if (k == 0) {
      warning("No significant orthogonal component: the model has none (a one-component PLS model).",
              call. = FALSE)
    }
  }
  keep <- seq_len(k)
  list(
    n_ortho = k, w = pred$w, t = pred$t, p = pred$p, c = pred$c,
    wo = wo[, keep, drop = FALSE], to = full$to[, keep, drop = FALSE],
    po = full$po[, keep, drop = FALSE], co = co[keep],
    r2y = r2y, q2 = q2, signif = signif
  )
}

# Predictive and orthogonal VIP (Galindo-Prieto et al., 2014), as in ropls.
opls_vip <- function(fit, ys) {
  nvar <- nrow(fit$w)
  if (fit$n_ortho == 0) {
    return(list(vip = sqrt(nvar * drop(fit$w)^2), ortho = NULL))
  }
  sxp <- sum(fit$t^2) * sum(fit$p^2)
  sxo <- colSums(fit$to^2) * colSums(fit$po^2)
  syp <- sum(fit$t^2) * fit$c^2
  syo <- colSums(fit$to^2) * fit$co^2
  ssx <- sxp + sum(sxo)
  ssy <- syp + sum(syo)
  p_norm <- drop(fit$p)^2 / sum(fit$p^2)
  po_norm <- sweep(fit$po^2, 2, colSums(fit$po^2), "/")
  kp <- nvar / (sxp / ssx + syp / ssy)
  ko <- nvar / (sum(sxo) / ssx + sum(syo) / ssy)
  list(
    vip = sqrt(kp * (p_norm * sxp / ssx + p_norm * syp / ssy)),
    ortho = sqrt(ko * (drop(po_norm %*% sxo) / ssx + drop(po_norm %*% syo) / ssy))
  )
}


#' @title Predict with an OPLS Model
#'
#' @description
#' Applies an OPLS model fitted by [opls()] to new data: removes the
#' orthogonal components (the OPLS filter), or predicts the response or the
#' scores.
#'
#' @details
#' The new data are preprocessed with the centers and scales of the
#' calibration data, and the orthogonal components are removed one at a time
#' (\eqn{\textbf{t}_o = \textbf{X}\textbf{w}_o},
#' \eqn{\textbf{X} \leftarrow \textbf{X} - \textbf{t}_o\textbf{p}_o^T}).
#' The predicted response is \eqn{\textbf{X}\textbf{w}c}, of the filtered
#' data, back in the units of y.
#'
#' @param object A model returned by [opls()].
#' @param newdata A numeric matrix or data frame of new data, with the same
#'   variables as the calibration data. If both have column names, the
#'   columns of `newdata` are matched by name.
#' @param type The prediction: `"correction"` (default) the filtered data,
#'   like the `correction` component of the model; `"response"` the predicted
#'   response; `"scores"` the predictive (`p1`) and orthogonal (`o1`, ...)
#'   scores.
#' @param ... Not used.
#'
#' @return A tibble of filtered data or of scores, or a numeric vector of
#'   predicted responses.
#'
#' @seealso [opls()], [step_opls()]
#'
#' @export
#'
#' @examples
#' data(forageLIBS)
#' spectra <- forageLIBS[-(1:14)]  # the spectral channels
#' cal <- 1:300
#' fit <- opls(spectra[cal, ], forageLIBS$K[cal], ncomp.ortho = 2, permutation = 0)
#' head(predict(fit, spectra[-cal, ], type = "response"))
predict.specproc_opls <- function(object, newdata, type = c("correction", "response", "scores"), ...) {
  type <- match.arg(type)
  z <- filter_preprocess(object, filter_newdata(object, newdata))
  wo <- as.matrix(object$orthoWeights)
  po <- as.matrix(object$orthoLoadings)
  to <- matrix(0, nrow(z), ncol(wo), dimnames = list(NULL, colnames(wo)))
  for (i in seq_len(ncol(wo))) {
    to[, i] <- z %*% wo[, i]
    z <- z - tcrossprod(to[, i], po[, i])
  }
  switch(
    type,
    correction = filter_output(z, object),
    response = drop(z %*% object$coefficients) * object$y_scale + object$y_center,
    scores = tibble::as_tibble(cbind(p1 = drop(z %*% as.matrix(object$x_weights)), to))
  )
}

#' @export
print.specproc_opls <- function(x, ...) {
  s <- x$summary
  cat("Orthogonal projections to latent structures (OPLS)\n\n")
  cat("Variables:               ", attr(x, "nvar"), "\n", sep = "")
  if (!is.null(x$correction)) {
    cat("Observations:            ", nrow(x$correction), "\n", sep = "")
  }
  cat("Scaling:                 ", attr(x, "scaling"), "\n", sep = "")
  cat("Predictive components:   ", s$pre, "\n", sep = "")
  cat("Orthogonal components:   ", s$ort, "\n\n", sep = "")
  stats <- s[intersect(c("R2X(cum)", "R2Y(cum)", "Q2(cum)", "RMSEE", "pR2Y", "pQ2"), names(s))]
  print(signif(stats, 3))
  cat("\nUse predict(<model>, newdata, type = ) to filter new data or predict the response.\n")
  invisible(x)
}
