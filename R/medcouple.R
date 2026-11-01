#' @title Medcouple
#'
#' @description
#' This function calculates the medcouple (MC), a robust measure of the
#' skewness of a distribution (Brys *et al.* 2004), for a numeric vector or
#' for each column of a matrix or data frame, such as one medcouple per
#' wavelength of a set of spectra.
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
#' For spectra, one medcouple per wavelength describes the shape of the
#' distribution of the intensities at each channel. Channels that saturate
#' the detector in many spectra have a strongly negative medcouple, since
#' their intensities pile up at the top of the range, while the lines of an
#' element whose contents are skewed, with a few samples far above the
#' others, have a positive one.
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
#' @param x A numeric vector, or a numeric matrix, data frame or tibble (one
#'   medcouple per column).
#' @param drop_na Logical value indicating whether to remove missing values
#'   (NA). If `FALSE` (default), the medcouple of a vector or column that
#'   contains missing values is `NA`.
#'
#' @return For a vector, the medcouple of `x`. For a matrix or data frame,
#'   a numeric vector with the medcouple of each column, named by the
#'   columns. The medcouple of a vector without values is `NA`.
#'
#' @seealso [medcouple_weight()], [adjusted_boxplot()], [iqr_outliers()],
#'   [summary_stats()]
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
#' round(medcouple(contents, drop_na = TRUE), 2)
#'
#' # one medcouple per wavelength, from 380 to 430 nm
#' wl <- suppressWarnings(as.numeric(names(forageLIBS)))
#' mc <- medcouple(forageLIBS[which(wl > 380 & wl < 430)])
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
  if (!is.numeric(x) && !is.data.frame(x)) {
    stop("The input 'x' must be a numeric vector, matrix or data frame.")
  }
  check_flag(drop_na, "drop_na")

  if (is.numeric(x) && length(dim(x)) < 2) {
    return(medcouple_vector(as.vector(x), drop_na))
  }
  x <- as_numeric_matrix(x)
  mc <- vapply(seq_len(ncol(x)), function(j) medcouple_vector(x[, j], drop_na), numeric(1))
  stats::setNames(mc, colnames(x))
}

# Medcouple of a numeric vector; doScale is given so that robustbase does not
# print its message about the default of 'doScale'.
medcouple_vector <- function(v, drop_na) {
  if (anyNA(v)) {
    if (!drop_na) return(NA_real_)
    v <- v[!is.na(v)]
  }
  if (length(v) == 0) {
    return(NA_real_)
  }
  robustbase::mc(v, doScale = FALSE)
}
