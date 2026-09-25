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
#' With \eqn{\textbf{X}_d} the difference between the mean-centered datasets,
#' \eqn{\textbf{C} = \textbf{X}_d^T\textbf{X}_d = \textbf{V}\textbf{S}^2\textbf{V}^T},
#' the filter is
#' \deqn{\textbf{G} = \textbf{V}\textbf{D}^{-1}\textbf{V}^T, \quad \textbf{D} = \sqrt{\textbf{S}^2/\alpha + \textbf{I}}}
#' where \eqn{\textbf{S}^2} holds the eigenvalues of \eqn{\textbf{C}}.
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
#' @param x1 A numeric matrix, data frame or tibble representing the first set of data.
#' @param x2 A numeric matrix, data frame or tibble representing the second set of data.
#' @param alpha A positive numeric value specifying the weighting parameter. Typical values range from 1 to 0.0001. Default is 0.01.
#'
#' @return A tibble containing the \eqn{p \times p} filtering matrix \eqn{\textbf{G}}.
#' @references
#'    - Martens, H., Hoy, M., Wise, B.M., Bro, R., Brockhoff, P.B., (2003).
#'      Pre-whitening of data by covariance-weighted preprocessing.
#'      Journal of Chemometrics, 17(3):153-165
#'
#' @export glsw
#'
#' @examples
#' set.seed(1)
#' x1 <- matrix(rnorm(20 * 30), 20, 30)
#' x2 <- x1 + outer(rnorm(20), cos(seq(0, pi, length.out = 30)))
#' G <- glsw(x1, x2, alpha = 0.01)
#' x1_filtered <- x1 %*% as.matrix(G)
#'
glsw <- function(x1, x2, alpha = 0.01) {

  if (missing(x1)) {
    stop("Missing 'x1' argument.")
  }
  if (missing(x2)) {
    stop("Missing 'x2' argument.")
  }
  x1 <- as_numeric_matrix(x1, "x1")
  x2 <- as_numeric_matrix(x2, "x2")
  if (nrow(x1) != nrow(x2)) {
    stop("Both data must have the same number of rows.")
  }
  if (ncol(x1) != ncol(x2)) {
    stop("Both data must have the same number of columns.")
  }
  if (!is.numeric(alpha) || length(alpha) != 1 || is.na(alpha) || alpha <= 0) {
    stop("'alpha' must be a single positive numeric value.")
  }
  if (anyNA(x1) || anyNA(x2)) {
    stop("'x1' and 'x2' cannot contain missing values.")
  }

  x_diff <- scale(x2, scale = FALSE) - scale(x1, scale = FALSE)
  .filter <- glsw_cpp(unname(x_diff), alpha)

  return(as_tbl(.filter, colnames(x1)))
}
