#' @title Applicability Domain of a Calibration
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Describes the space of the calibration spectra by a principal component
#' analysis (PCA), so that the spectra of new samples can be flagged when
#' they lie outside it: their predictions, by any model calibrated on these
#' spectra, are then extrapolations.
#'
#' @details
#' A new spectrum is outside the applicability domain when its Hotelling
#' \eqn{T^2}, the distance within the plane of the first `k` components, or
#' its Q residual, the squared distance to that plane, exceeds its limit at
#' `conf_level` (see [q_residuals()]). A high \eqn{T^2} is a sample more
#' extreme than the calibration samples (for example, a higher
#' concentration); a high Q, a sample of another kind (another matrix, a
#' contamination, an instrumental problem). The limit of \eqn{T^2} is that of
#' a new observation, and that of Q the Jackson-Mudholkar limit.
#'
#' By default, `k` is the smallest number of components that explain the
#' fraction `variance` of the variance of the calibration spectra. Prepare
#' the spectra as for the model (baseline correction, normalization), since
#' the domain is that of the variables the model uses. The PCA is classical:
#' remove the outlying calibration spectra first (see [robpca()]).
#'
#' @param x The calibration spectra: a numeric matrix or data frame, one
#'   spectrum per row.
#' @param k The number of principal components. By default, chosen from
#'   `variance`.
#' @param variance The fraction of the variance explained by the
#'   components, when `k` is not given. Default is 0.99.
#' @param conf_level The confidence level of the limits. Default is 0.99.
#' @param center,scale Passed to [stats::prcomp()]. Default is `TRUE` and
#'   `FALSE`.
#'
#' @return An object of class `specproc_domain`, a list with the `pca`
#'   ([stats::prcomp()] fit), the number of components `k`, the fraction of
#'   the variance they `explained`, the `conf_level` and the `limits` of
#'   \eqn{T^2} and Q. [predict()][predict.specproc_domain] flags new
#'   spectra.
#'
#' @references
#'  - Jackson, J.E., Mudholkar, G.S. (1979). Control procedures for
#'    residuals associated with principal component analysis.
#'    Technometrics, 21(3):341-349.
#'  - Nomikos, P., MacGregor, J.F. (1995). Multivariate SPC charts for
#'    monitoring batch processes. Technometrics, 37(1):41-59.
#'
#' @seealso [predict.specproc_domain()], [q_residuals()], [plot_influence()]
#' @export applicability_domain
#'
#' @examples
#' data(forageLIBS)
#' wl <- suppressWarnings(as.numeric(names(forageLIBS)))
#' spectra <- as.matrix(forageLIBS[which(wl > 380 & wl < 430)])
#' domain <- applicability_domain(spectra[1:300, ])
#' domain
#' flags <- predict(domain, spectra[301:368, ])
#' table(flags$inside)
applicability_domain <- function(x, k = NULL, variance = 0.99, conf_level = 0.99, center = TRUE,
                                 scale = FALSE) {
  x <- as_numeric_matrix(x, "x")
  if (anyNA(x)) {
    stop("'x' must not contain missing values.", call. = FALSE)
  }
  check_number(variance, "variance", lower = 0, upper = 1, lower_open = TRUE)
  check_number(conf_level, "conf_level", lower = 0, upper = 1, lower_open = TRUE,
               upper_open = TRUE)
  pca <- stats::prcomp(x, center = center, scale. = scale)
  lambda <- pca$sdev^2
  explained <- cumsum(lambda) / sum(lambda)
  rank <- ncol(pca$x)
  if (rank < 2) {
    stop("The calibration spectra need at least 2 principal components.", call. = FALSE)
  }
  if (is.null(k)) {
    k <- min(which(explained >= variance - 1e-12)[1], rank - 1)
  } else {
    k <- check_count(k, "k")
    if (k >= rank) {
      stop("'k' must be smaller than the number of components (", rank, ").", call. = FALSE)
    }
  }
  n <- nrow(x)
  structure(
    list(pca = pca, k = k, explained = explained[k], conf_level = conf_level, n = n,
         limits = c(t2 = t2_limit(conf_level, k, n, "new"),
                    q = q_limit(lambda[-seq_len(k)], conf_level, "jackson"))),
    class = "specproc_domain"
  )
}

#' @title Flag Spectra Outside the Applicability Domain
#'
#' @description
#' Computes Hotelling's \eqn{T^2} and the Q residual of new spectra in the
#' PCA model of [applicability_domain()], and flags those outside the
#' domain.
#'
#' @param object The result of [applicability_domain()].
#' @param newdata The new spectra: a numeric matrix or data frame with the
#'   variables of the calibration spectra.
#' @param ... Not used.
#'
#' @return A tibble of class `specproc_influence`, as returned by
#'   [q_residuals()], with one row per spectrum: `sample`, `t2` and its
#'   limit, `q` and its limit, `outlier` (`"regular"`, `"extreme"` for a high
#'   \eqn{T^2}, `"residual"` for a high Q, or `"both"`), and `inside`, whether
#'   the spectrum lies inside the domain. Draw it with [plot_influence()].
#'
#' @seealso [applicability_domain()]
#' @export
#'
#' @examples
#' data(forageLIBS)
#' wl <- suppressWarnings(as.numeric(names(forageLIBS)))
#' spectra <- as.matrix(forageLIBS[which(wl > 380 & wl < 430)])
#' domain <- applicability_domain(spectra[1:300, ])
#' flags <- predict(domain, spectra[301:368, ])
#' flags[!flags$inside, ]
predict.specproc_domain <- function(object, newdata, ...) {
  out <- q_residuals(object$pca, object$k, newdata = newdata, conf_level = object$conf_level)
  out$inside <- out$outlier == "regular"
  out
}

#' @export
print.specproc_domain <- function(x, ...) {
  cat("Applicability domain: PCA of ", x$n, " calibration spectra\n\n", sep = "")
  cat("Components:  ", x$k, " (", format(round(100 * x$explained, 2)), "% of the variance)\n",
      sep = "")
  cat("Limits:       T2 = ", format(signif(x$limits[["t2"]], 4)), ", Q = ",
      format(signif(x$limits[["q"]], 4)), " (", format(100 * x$conf_level), "%)\n", sep = "")
  invisible(x)
}
