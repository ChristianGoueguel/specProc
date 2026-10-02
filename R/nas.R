#' @title Net Analyte Signal and Figures of Merit
#'
#' @author Christian L. Goueguel
#'
#' @description
#' This function computes the net analyte signal (NAS) of each sample for a
#' multivariate inverse calibration model (PLS or PCR), following Lorber
#' (1997) and Faber (1998), and the figures of merit derived from it:
#' sensitivity, selectivity and, when the instrumental noise is known,
#' analytical sensitivity, limits of detection and quantification, and
#' signal-to-noise ratios.
#'
#' @details
#' The net analyte signal is the part of a spectrum that is unique to the
#' analyte, i.e. orthogonal to the spectral contributions of all other
#' constituents (the interferents). In an inverse calibration model
#' \eqn{\hat{y} = \bar{y} + (\textbf{x} - \bar{\textbf{x}})^T\textbf{b}} with
#' \eqn{A} latent variables, the interferent space is spanned by the
#' calibration spectra reconstructed from the model, from which the part
#' explained by \eqn{\hat{\textbf{y}}} has been removed. The regression vector
#' \eqn{\textbf{b}} is orthogonal to this space, so the NAS space within the
#' model is one-dimensional and spanned by \eqn{\textbf{b}} (Faber, 1998;
#' Bro and Andersen, 2003). The NAS vector of sample \eqn{i} is therefore
#' \deqn{\textbf{r}_i^* = \frac{(\textbf{x}_i - \bar{\textbf{x}})^T\textbf{b}}{\|\textbf{b}\|^2}\,\textbf{b}}
#' and its signed length, the scalar NAS, is
#' \deqn{\mathrm{NAS}_i = \frac{(\textbf{x}_i - \bar{\textbf{x}})^T\textbf{b}}{\|\textbf{b}\|} = \frac{\hat{y}_i - \bar{y}}{\|\textbf{b}\|}}
#'
#' The figures of merit follow Olivieri *et al.* (2006):
#'  - **Sensitivity**, \eqn{\mathrm{SEN} = 1 / \|\textbf{b}\|}: the NAS
#'    produced by a unit change in concentration, in signal units per
#'    concentration unit.
#'  - **Selectivity** of sample \eqn{i},
#'    \eqn{\mathrm{SEL}_i = |\mathrm{NAS}_i| / \|\textbf{x}_i - \bar{\textbf{x}}\|}:
#'    the fraction of the (centered) signal that is used for prediction,
#'    between 0 and 1.
#'  - With the standard deviation of the instrumental noise \eqn{\sigma_x}
#'    (`noise`): the **analytical sensitivity**
#'    \eqn{\gamma = \mathrm{SEN} / \sigma_x}, whose inverse is the smallest
#'    concentration difference that can be distinguished; the **limit of
#'    detection** \eqn{\mathrm{LOD} = 3.3\,\sigma_x / \mathrm{SEN}}; the
#'    **limit of quantification** \eqn{\mathrm{LOQ} = 10\,\sigma_x / \mathrm{SEN}};
#'    and the **signal-to-noise ratio** of each sample,
#'    \eqn{\mathrm{NAS}_i / \sigma_x}.
#'
#' The LOD and LOQ above account for the instrumental noise only, not for
#' the uncertainty of the calibration model or of the reference values, so
#' they are lower bounds. The noise level must be estimated independently,
#' for example as the standard deviation of the differences between repeated
#' spectra of the same sample divided by \eqn{\sqrt{2}}, in the same units
#' (and after the same preprocessing) as `x`.
#'
#' All quantities depend on the number of latent variables `ncomp`, which
#' should be chosen by validation beforehand.
#'
#' Before specProc 0.4.0, `nas()` returned spectra filtered by direct
#' orthogonalization; that correction is available from
#' [direct_orthogonal()].
#'
#' @param x A numeric matrix or data frame of calibration spectra (one per
#'   row).
#' @param y A numeric vector (or one-column matrix or data frame) of analyte
#'   concentrations.
#' @param ncomp A positive integer giving the number of latent variables of
#'   the calibration model. Default is 5.
#' @param method The inverse calibration model: `"pls"` (default, SIMPLS)
#'   or `"pcr"`.
#' @param center A logical value indicating whether to mean-center `x` and
#'   `y`. Default is `TRUE`.
#' @param scale A logical value indicating whether to scale the columns of
#'   `x` to unit variance. Default is `FALSE`. The figures of merit are then
#'   expressed in scaled units.
#' @param noise An optional positive number: the standard deviation of the
#'   instrumental noise, in the units of `x`. Needed for the analytical
#'   sensitivity, LOD, LOQ and signal-to-noise ratios.
#'
#' @return An object of class `specproc_nas`, a list with:
#'  - `nas`: A tibble with one row per calibration sample: the scalar
#'    `nas`, the `selectivity`, the fitted concentration `fitted` and, if
#'    `noise` is given, the signal-to-noise ratio `snr`.
#'  - `figures_of_merit`: A named vector with the `sensitivity`, the mean
#'    selectivity (`selectivity`) and, if `noise` is given,
#'    `analytical_sensitivity`, `lod` and `loq`.
#'  - `nas_vectors`: A tibble of the NAS vectors \eqn{\textbf{r}_i^*} of the
#'    calibration samples.
#'  - `coefficients`: The regression vector \eqn{\textbf{b}}.
#'  - `ncomp`, `method`, `noise`, `center`, `scale` and `y_center`: the model
#'    settings and preprocessing parameters.
#'
#' Use [predict()][predict.specproc_nas] to compute the NAS, selectivity and
#' predicted concentration of new samples.
#'
#' @references
#'  - Lorber, A. (1986). Error propagation and figures of merit for
#'    quantification by solving matrix equations. Analytical Chemistry,
#'    58(6):1167-1172.
#'  - Lorber, A., Faber, K., Kowalski, B.R. (1997). Net analyte signal
#'    calculation in multivariate calibration. Analytical Chemistry,
#'    69(8):1620-1626.
#'  - Faber, N.M. (1998). Efficient computation of net analyte signal vector
#'    in inverse multivariate calibration models. Analytical Chemistry,
#'    70(23):5108-5110.
#'  - Bro, R., Andersen, C.M. (2003). Theory of net analyte signal vectors in
#'    inverse regression. Journal of Chemometrics, 17(12):646-652.
#'  - Olivieri, A.C., Faber, N.M., Ferré, J., Boqué, R., Kalivas, J.H.,
#'    Mark, H. (2006). Uncertainty estimation and figures of merit for
#'    multivariate calibration (IUPAC Technical Report). Pure and Applied
#'    Chemistry, 78(3):633-661.
#'
#' @seealso [direct_orthogonal()] for the filter formerly returned by
#'   `nas()`.
#'
#' @export nas
#'
#' @examples
#' data(forageLIBS)
#' spectra <- forageLIBS[-(1:14)]  # the spectral channels
#' # the three samples measured twice
#' twice <- forageLIBS$Sample[duplicated(forageLIBS$Sample)]
#' first <- match(twice, forageLIBS$Sample)
#' second <- vapply(twice, function(s) max(which(forageLIBS$Sample == s)), integer(1))
#' # the noise of the spectra, from the differences between repeated measurements
#' noise <- stats::sd(as.matrix(spectra[second, ]) - as.matrix(spectra[first, ])) / sqrt(2)
#'
#' cal <- 1:300
#' fit <- nas(spectra[cal, ], forageLIBS$K[cal], ncomp = 7, noise = noise)
#' fit
#' head(fit$nas)
#' # the net analyte signal and selectivity of new spectra
#' head(predict(fit, spectra[-cal, ]))
nas <- function(x, y, ncomp = 5, method = "pls", center = TRUE, scale = FALSE, noise = NULL) {
  if (missing(x) || missing(y)) {
    stop("Both 'x' and 'y' must be provided.")
  }
  method <- match.arg(method, c("pls", "pcr"))
  check_count(ncomp, "ncomp")
  if (!is.null(noise)) {
    check_number(noise, "noise", lower = 0, lower_open = TRUE)
  }

  x_in <- as_numeric_matrix(x, "x")
  y_in <- as_response_matrix(y, nrow(x_in), "y")
  if (ncol(y_in) != 1) {
    stop("'y' must be a single response variable.")
  }
  if (anyNA(x_in) || anyNA(y_in)) {
    stop("'x' and 'y' cannot contain missing values.")
  }
  if (nrow(x_in) < 3) {
    stop("At least 3 observations are required.")
  }
  px <- preprocess(x_in, center, scale)
  py <- preprocess(y_in, center, FALSE)
  xs <- px$x
  ys <- drop(py$x)

  max_comp <- min(nrow(xs) - as.integer(center), ncol(xs))
  if (ncomp > max_comp) {
    warning("'ncomp' reduced to ", max_comp, ".", call. = FALSE)
    ncomp <- max_comp
  }

  b <- switch(
    method,
    pls = drop(pls::simpls.fit(xs, ys, ncomp = ncomp, center = FALSE)$coefficients[, , ncomp]),
    pcr = {
      s <- svd(xs, nu = ncomp, nv = ncomp)
      drop(s$v %*% (crossprod(s$u, ys) / s$d[seq_len(ncomp)]))
    }
  )
  b_norm <- sqrt(sum(b^2))
  if (b_norm == 0) {
    stop("The regression vector is zero; the response is not related to 'x'.")
  }

  sample_nas <- nas_samples(list(coefficients = b, y_center = py$center, noise = noise), xs)
  fom <- c(
    sensitivity = 1 / b_norm,
    selectivity = mean(sample_nas$selectivity, na.rm = TRUE)
  )
  if (!is.null(noise)) {
    fom <- c(
      fom,
      analytical_sensitivity = 1 / (b_norm * noise),
      lod = 3.3 * noise * b_norm,
      loq = 10 * noise * b_norm
    )
  }

  res <- list(
    nas = sample_nas,
    figures_of_merit = fom,
    nas_vectors = as_tbl(outer(sample_nas$nas, b / b_norm), colnames(x_in)),
    coefficients = b,
    ncomp = as.integer(ncomp),
    method = method,
    noise = noise,
    center = px$center,
    scale = px$scale,
    y_center = unname(py$center)
  )
  structure(res, variables = colnames(x_in), nvar = ncol(x_in), class = "specproc_nas")
}

# NAS, selectivity, predicted concentration (and SNR) of preprocessed spectra.
nas_samples <- function(object, xs) {
  b <- object$coefficients
  b_norm <- sqrt(sum(b^2))
  score <- drop(xs %*% b)
  scalar <- score / b_norm
  signal <- sqrt(rowSums(xs^2))
  out <- tibble::tibble(
    nas = scalar,
    selectivity = ifelse(signal > 0, abs(scalar) / signal, NA_real_),
    fitted = drop(object$y_center) + score
  )
  if (!is.null(object$noise)) {
    out$snr <- scalar / object$noise
  }
  out
}

#' @title Net Analyte Signal of New Samples
#'
#' @description
#' Computes the net analyte signal, selectivity and predicted concentration
#' of new samples with a model fitted by [nas()].
#'
#' @param object An object returned by [nas()].
#' @param newdata A numeric matrix or data frame of new spectra, with the same
#'   variables as the calibration spectra.
#' @param ... Not used.
#'
#' @return A tibble with one row per new sample and columns `nas`,
#'   `selectivity`, `predicted` and, if the model has a `noise` level, `snr`.
#'
#' @seealso [nas()]
#' @export
#'
#' @examples
#' data(forageLIBS)
#' wl <- suppressWarnings(as.numeric(names(forageLIBS)))
#' spectra <- forageLIBS[which(wl > 760 & wl < 780)]  # the K I resonance lines
#' cal <- 1:300
#' fit <- nas(spectra[cal, ], forageLIBS$K[cal], ncomp = 3)
#' head(predict(fit, spectra[-cal, ]))
predict.specproc_nas <- function(object, newdata, ...) {
  x <- filter_newdata(object, newdata)
  if (anyNA(x)) {
    stop("'newdata' contains missing values.", call. = FALSE)
  }
  out <- nas_samples(object, apply_preprocess(x, list(center = object$center, scale = object$scale)))
  names(out)[names(out) == "fitted"] <- "predicted"
  out
}

#' @export
print.specproc_nas <- function(x, ...) {
  fom <- x$figures_of_merit
  cat("Net analyte signal (", toupper(x$method), " model, ", x$ncomp, " components)\n\n", sep = "")
  cat("Calibration samples:     ", nrow(x$nas), "\n", sep = "")
  cat("Sensitivity:             ", format(fom[["sensitivity"]], digits = 4), "\n", sep = "")
  cat("Mean selectivity:        ", format(fom[["selectivity"]], digits = 3), "\n", sep = "")
  if (!is.null(x$noise)) {
    cat("Noise (sd):              ", format(x$noise, digits = 4), "\n", sep = "")
    cat("Analytical sensitivity:  ", format(fom[["analytical_sensitivity"]], digits = 4), "\n", sep = "")
    cat("LOD:                     ", format(fom[["lod"]], digits = 4), "\n", sep = "")
    cat("LOQ:                     ", format(fom[["loq"]], digits = 4), "\n", sep = "")
  } else {
    cat("\nGive 'noise' for the analytical sensitivity, LOD, LOQ and signal-to-noise ratios.\n")
  }
  invisible(x)
}
