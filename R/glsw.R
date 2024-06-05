#' @title Generalized Least Squares Weighting
#'
#' @author Christian L. Goueguel
#'
#' @description
#' The Generalized Least Squares Weighting (GLSW) algorithm, proposed by Martens *et al.* (2003),
#' is a technique used to mitigate the effects of external interferences in datasets.
#' It constructs a filter to remove these interferences, allowing for more
#' accurate data analysis and processing.
#'
#' @details
#' The algorithm works by first calculating a covariance matrix from the differences
#' between two spectral datasets that should ideally be similar. These differences are
#' considered to be the interferences or clutter present in the data.
#' For example, if two sets of measurements have been taken under similar conditions,
#' the differences between them could be attributed to external factors such as
#' sensor noise, environmental conditions, or other sources of interference.
#' Once the covariance matrix is calculated, GLSW applies a filtering matrix to
#' down-weight the contributions of the identified interferences or clutter. This
#' filtering matrix is constructed using a regularization parameter, denoted as
#' alpha (\eqn{\alpha}).
#'
#'
#' The value of \eqn{\alpha} determines how strongly the algorithm down-weights
#' the clutter components in the data. In cases where the interferences are
#' well-characterized and distinct from the desired signal, a small \eqn{\alpha} value
#' may be appropriate to achieve effective clutter removal. However, if the
#' interferences are more subtle or intertwined with the desired signal, a larger
#' \eqn{\alpha} value may be preferred to avoid over-suppression of the signal itself.
#'
#' Let \eqn{\textbf{X}} be a data matrix. The GLSW filter is applied as follows:
#'
#' \deqn{\textbf{X}_{new} = \textbf{X} \cdot \textbf{G}}
#'
#' where, \eqn{\textbf{X}_{new}} is the filtered matrix and \eqn{\textbf{G}} the filtering matrix.
#'
#' @param data1 A numeric matrix, data frame or tibble representing the first set of data.
#' @param data2 A numeric matrix, data frame or tibble representing the second set of data.
#' @param alpha A numeric value specifying the weighting parameter. Typical values
#' range from 1 to 0.0001. Default is 0.01.
#'
#' @return A tibble containing the filtering matrix.
#' @references
#'    - Martens, H., Hoy, M., Wise, B.M., Bro, R., Brockhoff, P.B., (2003).
#'      Pre-whitening of data by covariance-weighted preprocessing.
#'      Journal of Chemometrics, 17(3):153-165
#'
#' @export glsw
#'
glsw <- function(data1, data2, alpha = 0.01) {

  if (missing(data1)) {
    stop("Missing 'data1' argument.")
  }
  if (missing(data2)) {
    stop("Missing 'data2' argument.")
  }
  if (nrow(data1) != nrow(data2)) {
    stop("Both data must have the same number of rows.")
  }
  if (ncol(data1) != ncol(data2)) {
    stop("Both data must have the same number of columns.")
  }
  if (alpha < 1e-4 || alpha > 1) {
    stop("'alpha' must be between 1 to 0.0001.")
  }

  centering <- function(x) {
    xm <- x %>% tibble::as_tibble() %>% average()
    xc <- x - xm
    return(xc)
  }

  if (is.data.frame(data1) || tibble::is_tibble(data1)) {
    X1 <- as.matrix(data1)
  } else {
    X1 <- data1
  }

  if (is.data.frame(data2) || tibble::is_tibble(data2)) {
    X2 <- as.matrix(data2)
  } else {
    X2 <- data2
  }

  X1c <- centering(X1)
  X2c <- centering(X2)
  .diff <- X2c - X1c

  .filter <- glsw_cpp(as.matrix(.diff), alpha)
  colnames(.filter) <- colnames(X1)

  return(tibble::as_tibble(.filter))
}


