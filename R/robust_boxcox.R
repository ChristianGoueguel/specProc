#' @title Robust Box-Cox Transformation
#'
#' @description
#' Transforms each variable toward central normality with the Box-Cox
#' transformation, whose parameter \eqn{\lambda} is fitted robustly
#' (Raymaekers and Rousseeuw, 2021), so that outlying observations do not
#' drive it.
#'
#' @details
#' The Box-Cox transformation (Box and Cox, 1964) of a strictly positive
#' value \eqn{x} is
#' \deqn{x^{(\lambda)} = \frac{x^\lambda - 1}{\lambda}}
#' for \eqn{\lambda \ne 0}, and \eqn{x^{(\lambda)} = \log x} for
#' \eqn{\lambda = 0}. A \eqn{\lambda} below 1 compresses the upper tail, for
#' right-skewed variables (\eqn{\lambda = 0} for log-normal ones), and a
#' \eqn{\lambda} above 1 compresses the lower tail, for left-skewed ones.
#'
#' Each variable is first divided by its median, so that its transformed
#' median is 0 and \eqn{\lambda} does not depend on its units. Variables with
#' zero or negative values are left unchanged, with a warning: use
#' [robust_yeojohnson()] for them, as for baseline-corrected or SNV-scaled
#' spectra. When [predict()][predict.specproc_transformation] is applied to
#' new data, values that are zero or negative become `NA`.
#'
#' @section Robust fit:
#' The parameter \eqn{\lambda} of each variable is fitted as follows:
#'  1. The variable is pre-standardized (see Details).
#'  2. An initial \eqn{\lambda} (between -4 and 6) minimizes a robust
#'     distance (Tukey's biweight) between the sorted, robustly standardized
#'     transformed values and the quantiles of the normal distribution. For
#'     this step, the transformation is continued linearly on the side that
#'     it compresses (the upper side for \eqn{\lambda < 1}, the lower side
#'     for \eqn{\lambda > 1}), beyond the point where the transformed value
#'     is 1.5 times that of the quartile, so that outliers on that side
#'     cannot dictate \eqn{\lambda}.
#'  3. The transformed values are standardized with Huber's M-estimates of
#'     location and scale, and those farther from the center than the
#'     `quantile` of the absolute value of a standard normal variable (2.58
#'     for the default 0.99) are given weight zero (first with the rectified
#'     transformation of step 2). \eqn{\lambda} is then re-estimated by
#'     maximum likelihood on the others. This is repeated `nbsteps` times.
#'  4. With `standardize = TRUE`, the transformed variable is standardized by
#'     the mean and standard deviation of these inliers.
#'
#' Variables with fewer than 5 values or no spread (zero MAD) are left
#' unchanged. Missing values are ignored by the fit, and stay missing.
#'
#' @param x A numeric vector, matrix, data frame or tibble, with one variable
#'   per column (such as line intensities, contents or spectral channels).
#' @param quantile The quantile of the absolute value of a standard normal
#'   variable beyond which the standardized transformed values are given
#'   weight zero in the re-weighting steps, strictly between 0 and 1. Default
#'   is 0.99.
#' @param nbsteps The number of re-weighting steps, a positive integer.
#'   Default is 2.
#' @param standardize A logical value: standardize the transformed variables
#'   by the mean and standard deviation of the inliers (`TRUE`, default).
#'
#' @return An object of class `specproc_boxcox` (a list), which
#'   [predict()][predict.specproc_transformation] applies to new data, with
#'   the following components:
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
#'  - Box, G.E.P., Cox, D.R. (1964). An analysis of transformations.
#'    Journal of the Royal Statistical Society, Series B, 26(2):211-252.
#'
#' @author Christian L. Goueguel
#'
#' @seealso [robust_yeojohnson()] for variables with zero or negative values,
#'   [step_robust_boxcox()] to use it in a tidymodels recipe, [medcouple()] to
#'   measure the skewness before and after.
#'
#' @export robust_boxcox
#'
#' @examples
#' data(forageLIBS)
#' contents <- forageLIBS[c("Ca", "K", "P", "Mn")]  # mg/kg
#' fit <- robust_boxcox(contents[1:300, ])
#' fit$summary
#' # the skewness of the contents, before and after
#' round(medcouple(contents[1:300, ]), 2)
#' round(medcouple(fit$transformation), 2)
#' # new samples, with the transformation of the first 300
#' head(predict(fit, contents[301:368, ]))
robust_boxcox <- function(x, quantile = 0.99, nbsteps = 2, standardize = TRUE) {
  if (missing(x)) {
    stop("Missing 'x' argument.")
  }
  robust_power(x, "BC", quantile, nbsteps, standardize)
}
