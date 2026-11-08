#' @title Medcouple
#'
#' @description
#' This function calculates the medcouple (MC), a robust measure of the
#' skewness of a distribution (Brys *et al.* 2004), for a numeric vector.
#'
#' @details
#' The medcouple is the median of the kernel
#' \deqn{h(x_i, x_j) = \frac{(x_j - m) - (m - x_i)}{x_j - x_i}}
#' over all the pairs of observations \eqn{x_i \le m \le x_j} with
#' \eqn{x_i \ne x_j}, where \eqn{m} is the median of the data (with a
#' special definition of \eqn{h} for the ties at the median). It compares
#' the distances to the median of the observations above and below it,
#' scaled by their distance from each other, so it is unchanged by a shift
#' or a rescaling of the data.
#'
#' The interpretation of the medcouple is as follows:
#'  - It lies between -1 and 1, and is 0 for a symmetric distribution.
#'  - Positive values indicate a right-skewed distribution (a longer upper
#'    tail), and negative values a left-skewed one.
#'
#' Unlike the classical skewness, which is based on the third moment and can
#' be driven by a single extreme value, the medcouple has a breakdown value
#' of 25%. It is computed by [robustbase::mc()], in \eqn{O(n \log n)} time.
#' The medcouple is used by the adjusted boxplot ([adjusted_boxplot()]) and
#' the skewness-adjusted fences of [iqr_outliers()]; [medcouple_weight()]
#' applies it to each half of the data to measure the weight of the tails.
#'
#' For spectra, one medcouple per wavelength (`sapply(spectra, medcouple)`
#' for a data frame, `apply(spectra, 2, medcouple)` for a matrix) describes
#' the shape of the distribution of the intensities at each channel, and
#' [plot_medcouple()] draws it along the wavelength axis. Channels that
#' saturate the detector in many spectra have a strongly negative
#' medcouple, since their intensities pile up at the top of the range,
#' while the lines of an element whose contents are skewed, with a few
#' samples far above the others, have a positive one.
#'
#' @references
#'  - Brys, G., Hubert, M., and Struyf, A. (2004).
#'    A robust measure of skewness.
#'    Journal of Computational and Graphical Statistics, 13(4):996-1017
#'  - Hubert, M., and Vandervieren, E. (2008).
#'    An adjusted boxplot for skewed distributions.
#'    Computational Statistics & Data Analysis, 52(12):5186-5201
#'
#' @author Christian L. Goueguel
#' @param x A numeric vector.
#' @param drop_na Logical value indicating whether to remove missing values
#'   (NA). If `FALSE` (default) and `x` contains missing values, `NA` is
#'   returned.
#'
#' @return The medcouple of `x`, or `NA` when `x` has no values.
#'
#' @seealso [plot_medcouple()], [medcouple_weight()], [adjusted_boxplot()],
#'   [iqr_outliers()], [summary_stats()]
#'
#' @examples
#' data(forageLIBS)
#' # iron contents (mg/kg): a few samples are far above the others
#' fe <- forageLIBS$Fe[!is.na(forageLIBS$Fe)]
#' moments::skewness(fe)   # classical, driven by the highest contents
#' medcouple(fe)           # robust
#'
#' # one medcouple per column; Fe and Zn have a few missing values
#' contents <- forageLIBS[c("Ca", "K", "Mg", "P", "Fe", "Mn", "Zn")]
#' round(sapply(contents, medcouple, drop_na = TRUE), 2)
#'
#' # one medcouple per wavelength, from 380 to 430 nm
#' wl <- suppressWarnings(as.numeric(names(forageLIBS)))
#' mc <- sapply(forageLIBS[which(wl > 380 & wl < 430)], medcouple)
#' summary(mc)
#' # the Ca II line at 393.37 nm saturates the detector (65535 counts) in
#' # most spectra: a negative medcouple; the Fe I line at 404.58 nm follows
#' # the skewed iron contents: a positive one
#' wl <- as.numeric(names(mc))
#' round(mc[c(which.min(abs(wl - 393.37)), which.min(abs(wl - 404.58)))], 2)
#'
#' @export medcouple
#'
medcouple <- function(x, drop_na = FALSE) {
  if (missing(x)) {
    stop("Missing 'x' argument.")
  }
  if (!is.numeric(x) || length(dim(x)) > 1) {
    stop("The input 'x' must be a numeric vector; for the columns of a data ",
         "frame or matrix, use sapply(x, medcouple) or apply(x, 2, medcouple).")
  }
  check_flag(drop_na, "drop_na")

  # 1-d arrays (e.g. the result of tapply()) become plain vectors
  x <- as.vector(x)
  if (anyNA(x)) {
    if (!drop_na) return(NA_real_)
    x <- x[!is.na(x)]
  }
  if (length(x) == 0) {
    return(NA_real_)
  }
  # doScale is given so that robustbase does not print its message about the
  # default of 'doScale'
  robustbase::mc(x, doScale = FALSE)
}
