#' @title Generalized Least Squares Weighting
#'
#' @author Christian L. Goueguel
#'
#' @description
#' This function implements the Generalized Least Squares Weighting (GLSW) algorithm
#' proposed by Geladi and Kowalski (1986). The GLSW algorithm calculates a covariance
#' matrix from the differences between similar samples and applies a filtering matrix
#' to down-weight correlations present in the original covariance matrix. A weighting parameter,
#' \eqn{\alpha}, defines how strongly GLSW down weights interferences. The choice of \eqn{\alpha}
#' depends on the scale of the original values and the similarity between the interferences
#' and the net analyte signal.
#'
#' @details
#' The filtering matrix down-weights correlations in the original
#' covariance matrix, effectively reducing the impact of interferences on the data.
#'
#' @param data1 A numeric matrix, data frame or tibble representing the first set of data.
#' @param data2 A numeric matrix, data frame or tibble representing the second set of data (`NULL` by default).
#' @param alpha A numeric value specifying the weighting parameter. Typical values
#' range from 1 to 0.0001. Default is 0.01.
#'
#' @return The filtering matrix.
#' @references
#'    - Geladi, P., & Kowalski, B. R. (1986). Partial least-squares regression: a tutorial.
#'      Analytica Chimica Acta, 185, 1-17.
#'
#' @export glsw
glsw <- function(data1, data2 = NULL, alpha = 0.01) {
  if (nrow(data1) != nrow(data2)) {
    stop("'X1' and 'X2' must have the same number of rows.")
  }
  if (alpha <= 0) {
    stop("'alpha' must be a positive value.")
  }

  center_colmeans <- function(x) {
    xcenter <- x %>% tibble::as_tibble() %>% average()
    res <- x - xcenter
    return(res)
  }

  if (is.data.frame(data1) || tibble::is_tibble(data1)) {
    X1 <- as.matrix(data1)
  } else {
    X1 <- data1
  }
  X1c <- center_colmeans(X1)

  if (!is.null(data2)) {
    if (is.data.frame(data2) || tibble::is_tibble(data2)) {
      X2 <- as.matrix(data2)
    } else {
      X2 <- data2
    }
    X2c <- center_colmeans(X2)
    X_diff <- X2c - X1c
  } else {
    X_diff <- X1c
  }

  G <- glsw_cpp(as.matrix(X_diff), alpha)

  colnames(G) <- colnames(X1)
  return(tibble::as_tibble(G))
}


