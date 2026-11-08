#' @title Robust Yeo-Johnson Transformation
#'
#' @description
#' Transforms each variable toward central normality with the Yeo-Johnson
#' transformation, whose parameter \eqn{\lambda} is fitted robustly
#' (Raymaekers and Rousseeuw, 2021), so that outlying observations do not
#' drive it. Unlike the Box-Cox transformation ([robust_boxcox()]), it
#' accepts zero and negative values.
#'
#' @details
#' The Yeo-Johnson transformation (Yeo and Johnson, 2000) of a value
#' \eqn{x \ge 0} is
#' \deqn{\psi_\lambda(x) = \frac{(1 + x)^\lambda - 1}{\lambda}}
#' (\eqn{\log(1 + x)} for \eqn{\lambda = 0}), and of a value \eqn{x < 0}
#' \deqn{\psi_\lambda(x) = -\frac{(1 - x)^{2 - \lambda} - 1}{2 - \lambda}}
#' (\eqn{-\log(1 - x)} for \eqn{\lambda = 2}). \eqn{\lambda = 1} leaves the
#' values unchanged; a \eqn{\lambda} below 1 compresses the upper tail, for
#' right-skewed variables, and a \eqn{\lambda} above 1 compresses the lower
#' tail, for left-skewed ones.
#'
#' Each variable is first centered by its median and divided by its MAD, so
#' that \eqn{\lambda} does not depend on its location or units. It describes
#' the shape of the distribution on this robust standard scale, and cannot
#' be compared with the \eqn{\lambda} of [robust_boxcox()]: on the same data,
#' it is often much closer to 1.
#'
#' It suits the variables with zero or negative values, such as
#' baseline-corrected, derivative or SNV-scaled spectra. Strictly positive
#' variables, such as raw intensities or contents, can be transformed with
#' either transformation.
#'
#' @inheritSection robust_boxcox Robust fit
#' @inheritParams robust_boxcox
#'
#' @return An object of class `specproc_yeojohnson` (a list), which
#'   [predict()][predict.specproc_transformation] applies to new data, or
#'   with `inverse = TRUE` uses to transform values back to the original
#'   scale (as a median or a mean), with the following components:
#'  - `transformation`: A tibble of the transformed variables.
#'  - `summary`: A tibble with one row per variable: `variable`, `lambda`
#'    (`NA` for the variables left unchanged) and `objective` (the robust
#'    distance between the transformed values and the normal quantiles of
#'    step 2; lower is closer to normality).
#'  - `fits`: The fitted parameters of each variable, used by `predict()`.
#'  - `standardize`: The `standardize` argument.
#'
#' @references
#'  - Raymaekers, J., Rousseeuw, P.J. (2021). Transforming variables to
#'    central normality. Machine Learning,
#'    https://doi.org/10.1007/s10994-021-05960-5.
#'  - Yeo, I.-K., Johnson, R.A. (2000). A new family of power transformations
#'    to improve normality or symmetry. Biometrika, 87(4):954-959.
#'
#' @author Christian L. Goueguel
#'
#' @seealso [robust_boxcox()], [step_robust_yeojohnson()] to use it in a
#'   tidymodels recipe, [medcouple()] to measure the skewness before and
#'   after.
#'
#' @export robust_yeojohnson
#'
#' @examples
#' data(forageLIBS)
#' spectra <- snv(forageLIBS[-(1:14)])$correction
#' wl <- as.numeric(names(spectra))
#' # the Mg II lines at 279.55 and 280.27 nm: after SNV, most channels have
#' # negative values in some spectra, which Box-Cox cannot transform
#' mg <- spectra[which(wl > 279 & wl < 281)]
#' sum(vapply(mg, min, numeric(1)) < 0)
#' fit <- robust_yeojohnson(mg[1:300, ])
#' summary(fit$summary$lambda)
#' # the skewness of the channels, before and after
#' summary(sapply(mg[1:300, ], medcouple))
#' summary(sapply(fit$transformation, medcouple))
#' # new spectra, with the transformation of the first 300
#' dim(predict(fit, mg[301:368, ]))
robust_yeojohnson <- function(x, quantile = 0.99, nbsteps = 2, standardize = TRUE) {
  if (missing(x)) {
    stop("Missing 'x' argument.")
  }
  robust_power(x, "YJ", quantile, nbsteps, standardize)
}
