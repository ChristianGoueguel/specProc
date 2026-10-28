#' @title Direct Standardization
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Transfers spectra from one instrument (the slave) to another (the
#' master) by direct standardization (DS; Wang et al., 1991): a linear map
#' of the whole slave spectrum onto the whole master spectrum, estimated on
#' transfer samples measured on both instruments.
#'
#' @details
#' With the transfer spectra centered by the mean of each instrument,
#' \eqn{\textbf{X}_1 \approx \textbf{X}_2 \textbf{F}}, and the transfer
#' matrix is \eqn{\textbf{F} = \textbf{X}_2^{+} \textbf{X}_1}, where
#' \eqn{\textbf{X}_2^{+}} is the pseudo-inverse of the slave spectra,
#' truncated to their first `ncomp` singular vectors. A new slave spectrum
#' is transferred as \eqn{\bar{x}_1 + (x_2 - \bar{x}_2) \textbf{F}}.
#'
#' With as many components as the transfer samples allow (`ncomp = NULL`),
#' the transfer spectra are reproduced exactly, and the noise of the slave
#' spectra is amplified: fewer components give a smoother, more robust
#' transfer. Unlike [pds()], DS mixes all wavelengths, so it corrects global
#' differences of the response but needs more transfer samples. The transfer
#' matrix (\eqn{p \times p}) is kept in factored form, as `ncomp` columns of
#' each factor.
#'
#' @param x1 The spectra of the transfer samples on the master instrument: a
#'   numeric matrix or data frame, one spectrum per row.
#' @param x2 The spectra of the same samples, in the same order, on the
#'   slave instrument.
#' @param ncomp The number of singular vectors of the slave spectra kept. By
#'   default, all those with a singular value above `tol` times the largest.
#' @param tol The relative tolerance on the singular values. Default is
#'   1e-8.
#'
#' @return An object of class `specproc_transfer`. Use
#'   [predict()][predict.specproc_transfer] to transfer slave spectra.
#'
#' @references
#'  - Wang, Y., Veltkamp, D.J., Kowalski, B.R. (1991). Multivariate
#'    instrument standardization. Analytical Chemistry, 63(23):2750-2756.
#'
#' @seealso [pds()], [sst()], [slope_bias()], [predict.specproc_transfer()]
#' @export ds
#'
#' @examples
#' data(forageLIBS)
#' wl <- suppressWarnings(as.numeric(names(forageLIBS)))
#' spectra <- as.matrix(forageLIBS[which(wl > 760 & wl < 780)])  # the K I lines
#' # a second instrument, with a shifted and less sensitive response
#' slave <- 0.8 * cbind(spectra[, -1], spectra[, ncol(spectra)]) + 50
#' transfer <- ds(spectra[1:30, ], slave[1:30, ], ncomp = 10)
#' transfer
#' # the other samples, measured on the slave, transferred to the master
#' transferred <- predict(transfer, slave[31:368, ])
#' mean(abs(transferred - spectra[31:368, ])) / mean(spectra)
ds <- function(x1, x2, ncomp = NULL, tol = 1e-8) {
  pair <- transfer_pair(x1, x2)
  check_number(tol, "tol", lower = 0, lower_open = TRUE)
  sv <- svd(pair$x2c)
  rank <- sum(sv$d > tol * sv$d[1])
  if (rank == 0) {
    stop("The slave spectra have no variation.", call. = FALSE)
  }
  k <- if (is.null(ncomp)) rank else min(check_count(ncomp, "ncomp"), rank)
  idx <- seq_len(k)
  transfer_object(
    "ds", pair,
    a = sv$v[, idx, drop = FALSE] %*% diag(1 / sv$d[idx], k, k),
    b = crossprod(sv$u[, idx, drop = FALSE], pair$x1c),
    identity = FALSE, ncomp = k
  )
}

#' @title Spectral Space Transformation
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Transfers spectra from one instrument (the slave) to another (the
#' master) by spectral space transformation (SST; Du et al., 2011): the
#' slave spectra are moved, in the space of the principal components shared
#' by both instruments, by the difference between the instruments.
#'
#' @details
#' The transfer spectra of both instruments, centered by the mean of each
#' instrument, are joined side by side, \eqn{[\textbf{X}_1\ \textbf{X}_2]},
#' and decomposed by a principal component analysis,
#' \eqn{[\textbf{X}_1\ \textbf{X}_2] \approx \textbf{T} [\textbf{P}_1^T\
#' \textbf{P}_2^T]}: the same scores \eqn{\textbf{T}} describe a sample on
#' both instruments, and the loadings \eqn{\textbf{P}_1} and
#' \eqn{\textbf{P}_2} the spectra of each. A new slave spectrum gets the
#' scores \eqn{t = (x_2 - \bar{x}_2) (\textbf{P}_2^T)^{+}} and is transferred
#' by adding the difference of the instruments in that space:
#' \deqn{x_{2 \to 1} = \bar{x}_1 + (x_2 - \bar{x}_2) + t\,(\textbf{P}_1^T - \textbf{P}_2^T)}
#' The part of the spectrum outside the shared components is kept unchanged,
#' so SST corrects only the differences the transfer samples show.
#'
#' By default, `ncomp` is the smallest number of components that explain
#' the fraction `variance` of the variance of the joined spectra.
#'
#' @inheritParams ds
#' @param ncomp The number of principal components. By default, chosen from
#'   `variance`.
#' @param variance The fraction of the variance explained, when `ncomp` is
#'   not given. Default is 0.99.
#'
#' @return An object of class `specproc_transfer`. Use
#'   [predict()][predict.specproc_transfer] to transfer slave spectra.
#'
#' @references
#'  - Du, W., Chen, Z.-P., Zhong, L.-J., Wang, S.-X., Yu, R.-Q., Nordon, A.,
#'    Littlejohn, D., Holden, M. (2011). Maintaining the predictive abilities
#'    of multivariate calibration models by spectral space transformation.
#'    Analytica Chimica Acta, 690(1):64-70.
#'
#' @seealso [ds()], [pds()], [slope_bias()], [predict.specproc_transfer()]
#' @export sst
#'
#' @examples
#' data(forageLIBS)
#' wl <- suppressWarnings(as.numeric(names(forageLIBS)))
#' spectra <- as.matrix(forageLIBS[which(wl > 760 & wl < 780)])  # the K I lines
#' slave <- 0.8 * cbind(spectra[, -1], spectra[, ncol(spectra)]) + 50
#' transfer <- sst(spectra[1:30, ], slave[1:30, ])
#' transfer
#' transferred <- predict(transfer, slave[31:368, ])
#' mean(abs(transferred - spectra[31:368, ])) / mean(spectra)
sst <- function(x1, x2, ncomp = NULL, variance = 0.99) {
  pair <- transfer_pair(x1, x2)
  check_number(variance, "variance", lower = 0, upper = 1, lower_open = TRUE)
  p <- ncol(pair$x1c)
  sv <- svd(cbind(pair$x1c, pair$x2c))
  explained <- cumsum(sv$d^2) / sum(sv$d^2)
  rank <- sum(sv$d > 1e-8 * sv$d[1])
  k <- if (is.null(ncomp)) which(explained >= variance - 1e-12)[1] else check_count(ncomp, "ncomp")
  if (k > rank) {
    stop("'ncomp' must not exceed the rank of the transfer spectra (", rank, ").", call. = FALSE)
  }
  loadings <- sv$v[, seq_len(k), drop = FALSE]
  p1 <- loadings[seq_len(p), , drop = FALSE]
  p2 <- loadings[p + seq_len(p), , drop = FALSE]
  transfer_object(
    "sst", pair,
    a = p2 %*% solve(crossprod(p2)),   # the pseudo-inverse of t(p2)
    b = t(p1 - p2), identity = TRUE, ncomp = k, explained = explained[k]
  )
}

#' @title Transfer Spectra to Another Instrument
#'
#' @description
#' Transfers spectra measured on the slave instrument to the master
#' instrument, with a transfer fitted by [ds()], [sst()] or [pds()].
#'
#' @param object A transfer returned by [ds()], [sst()] or [pds()].
#' @param newdata Spectra of the slave instrument: a numeric matrix or data
#'   frame with the wavelengths of the transfer spectra.
#' @param ... Not used.
#'
#' @return The transferred spectra, a matrix (or a tibble for a data frame)
#'   with the wavelengths of the master spectra.
#'
#' @seealso [ds()], [sst()], [pds()]
#' @export
#'
#' @examples
#' data(forageLIBS)
#' wl <- suppressWarnings(as.numeric(names(forageLIBS)))
#' spectra <- as.matrix(forageLIBS[which(wl > 760 & wl < 780)])
#' slave <- 0.8 * cbind(spectra[, -1], spectra[, ncol(spectra)]) + 50
#' transfer <- pds(spectra[1:30, ], slave[1:30, ], win = 3)
#' transferred <- predict(transfer, slave[31:40, ])
predict.specproc_transfer <- function(object, newdata, ...) {
  x <- as_numeric_matrix(newdata, "newdata")
  p <- length(object$master_mean %||% object$intercept)
  if (ncol(x) != p) {
    stop("'newdata' must have the ", p, " wavelengths of the transfer spectra.", call. = FALSE)
  }
  if (!is.null(object$transfer_matrix)) {
    out <- sweep(x %*% object$transfer_matrix, 2, object$intercept, "+")
  } else {
    xc <- sweep(x, 2, object$slave_mean)
    out <- (xc %*% object$factors$a) %*% object$factors$b
    if (object$factors$identity) out <- out + xc
    out <- sweep(out, 2, object$master_mean, "+")
  }
  colnames(out) <- object$wavelengths %||% names(object$intercept)
  if (is.data.frame(newdata)) tibble::as_tibble(as.data.frame(out, check.names = FALSE)) else out
}

#' @export
print.specproc_transfer <- function(x, ...) {
  label <- c(ds = "Direct standardization", sst = "Spectral space transformation",
             pds = "Piecewise direct standardization")[[x$method]]
  cat(label, " (", x$n, " transfer samples, ", length(x$master_mean %||% x$intercept),
      " wavelengths)\n", sep = "")
  if (x$method == "pds") {
    cat("Window: +/-", x$win, " channels, ", x$ncomp, " PLS components", "\n", sep = "")
  } else {
    cat("Components: ", x$ncomp, if (!is.null(x$explained))
      paste0(" (", format(round(100 * x$explained, 2)), "% of the variance)"), "\n", sep = "")
  }
  invisible(x)
}

#' @title Slope and Bias Correction of Predictions
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Corrects the predictions of a model applied to spectra of another
#' instrument (or measured under other conditions) by a straight line,
#' estimated on transfer samples whose reference values are known.
#'
#' @details
#' The model of the master instrument, applied to the slave spectra of the
#' transfer samples, gives predictions \eqn{\hat{y}}; the reference values
#' \eqn{y} are regressed on them, \eqn{y = a + b\,\hat{y}}, and the
#' predictions of new slave spectra are corrected likewise. With
#' `slope = FALSE`, only the bias is corrected (\eqn{b = 1}). The correction
#' leaves the spectra and the model unchanged, so it suits differences that
#' affect all samples alike; it cannot correct differences of spectral shape
#' that change the ranking of the samples (see [ds()], [sst()] or [pds()]).
#' The tests of the bias against 0 and of the slope against 1 tell whether
#' the correction is needed.
#'
#' @param predicted The predictions of the model for the slave spectra of
#'   the transfer samples.
#' @param reference Their reference values (or the predictions for their
#'   master spectra).
#' @param slope A logical: correct the slope and the bias (`TRUE`, default),
#'   or the bias only.
#'
#' @return An object of class `specproc_slope_bias`, a list with the
#'   `coefficients` (a tibble with the `bias` and `slope`, their standard
#'   errors, the values they are tested against and the p-values), the
#'   number of transfer samples `n`, and the root mean square errors before
#'   and after correction (`rmse`). Use [predict()][predict.specproc_slope_bias]
#'   to correct new predictions.
#'
#' @references
#'  - Feudale, R.N., Woody, N.A., Tan, H., Myles, A.J., Brown, S.D., Ferré,
#'    J. (2002). Transfer of multivariate calibration models: a review.
#'    Chemometrics and Intelligent Laboratory Systems, 64(2):181-192.
#'
#' @seealso [predict.specproc_slope_bias()], [ds()], [sst()], [pds()]
#' @export slope_bias
#'
#' @examples
#' # predictions of the potassium content of transfer samples on a second
#' # instrument, 10% too high and offset by 0.2%
#' set.seed(1)
#' reference <- runif(20, 0.5, 4)
#' predicted <- 0.2 + 1.1 * reference + rnorm(20, sd = 0.05)
#' correction <- slope_bias(predicted, reference)
#' correction
#' predict(correction, c(1.3, 2.4, 3.5))
slope_bias <- function(predicted, reference, slope = TRUE) {
  if (!is.numeric(predicted) || !is.numeric(reference) || length(predicted) != length(reference)) {
    stop("'predicted' and 'reference' must be numeric vectors of the same length.", call. = FALSE)
  }
  check_flag(slope, "slope")
  keep <- !is.na(predicted) & !is.na(reference)
  predicted <- predicted[keep]
  reference <- reference[keep]
  n <- length(predicted)
  if (n < (if (slope) 3 else 2)) {
    stop("The correction needs at least ", if (slope) 3 else 2, " transfer samples.",
         call. = FALSE)
  }
  if (slope) {
    fit <- stats::lm(reference ~ predicted)
    est <- unname(stats::coef(fit))
    se <- unname(sqrt(diag(stats::vcov(fit))))
    df <- n - 2
  } else {
    d <- reference - predicted
    est <- c(mean(d), 1)
    se <- c(stats::sd(d) / sqrt(n), NA_real_)
    df <- n - 1
  }
  null <- c(0, 1)
  p_value <- 2 * stats::pt(-abs((est - null) / se), df)
  corrected <- est[1] + est[2] * predicted
  structure(
    list(coefficients = tibble::tibble(term = c("bias", "slope"), estimate = est,
                                       std_error = se, null = null, p_value = p_value),
         n = n, slope = slope,
         rmse = c(before = sqrt(mean((predicted - reference)^2)),
                  after = sqrt(mean((corrected - reference)^2)))),
    class = "specproc_slope_bias"
  )
}

#' @title Correct Predictions for Slope and Bias
#'
#' @description
#' Applies the correction fitted by [slope_bias()] to new predictions.
#'
#' @param object The result of [slope_bias()].
#' @param newdata Predictions of the model for new slave spectra.
#' @param ... Not used.
#'
#' @return The corrected predictions.
#' @seealso [slope_bias()]
#' @export
#'
#' @examples
#' set.seed(1)
#' reference <- runif(20, 0.5, 4)
#' predicted <- 0.2 + 1.1 * reference + rnorm(20, sd = 0.05)
#' predict(slope_bias(predicted, reference), c(1.3, 2.4, 3.5))
predict.specproc_slope_bias <- function(object, newdata, ...) {
  if (!is.numeric(newdata)) stop("'newdata' must be numeric predictions.", call. = FALSE)
  object$coefficients$estimate[1] + object$coefficients$estimate[2] * newdata
}

#' @export
print.specproc_slope_bias <- function(x, ...) {
  cat(if (x$slope) "Slope and bias" else "Bias", " correction (", x$n, " transfer samples)\n\n",
      sep = "")
  co <- as.data.frame(x$coefficients)
  if (!x$slope) co <- co[1, , drop = FALSE]
  print(co, row.names = FALSE, digits = 4)
  cat("\nRMSE:  ", format(signif(x$rmse[["before"]], 3)), " before, ",
      format(signif(x$rmse[["after"]], 3)), " after\n", sep = "")
  invisible(x)
}

# ---- internals ---------------------------------------------------------------

# The transfer spectra of both instruments, checked and centered.
transfer_pair <- function(x1, x2) {
  x1 <- as_numeric_matrix(x1, "x1")
  x2 <- as_numeric_matrix(x2, "x2")
  if (!identical(dim(x1), dim(x2))) {
    stop("'x1' and 'x2' must have the same samples (rows) and wavelengths (columns).",
         call. = FALSE)
  }
  if (anyNA(x1) || anyNA(x2)) {
    stop("'x1' and 'x2' cannot contain missing values.", call. = FALSE)
  }
  if (nrow(x1) < 3) {
    stop("At least 3 transfer samples are required.", call. = FALSE)
  }
  m1 <- colMeans(x1)
  m2 <- colMeans(x2)
  list(x1c = sweep(x1, 2, m1), x2c = sweep(x2, 2, m2), m1 = m1, m2 = m2,
       names = colnames(x1) %||% colnames(x2), n = nrow(x1))
}

# A transfer x -> m1 + (x - m2) (I [if identity] + a b), in factored form.
transfer_object <- function(method, pair, a, b, identity, ncomp, explained = NULL) {
  structure(
    list(method = method, factors = list(a = a, b = b, identity = identity),
         master_mean = unname(pair$m1), slave_mean = unname(pair$m2), wavelengths = pair$names,
         ncomp = ncomp, explained = explained, n = pair$n),
    class = "specproc_transfer"
  )
}
