#' @title Piecewise Direct Standardization
#'
#' @description
#' The `pds` function performs Piecewise Direct Standardization (PDS), a method
#' proposed by Bouveresse & Massart (1996) to transfer spectra from
#' one instrument to another. The main improvement over the original PDS method,
#' Wang *et al.*, (1991), is the use of a global PLSR model in addition to the local
#' PLSR models, which can lead to better transfer performance.
#'
#' @param x1 A matrix or data frame containing spectra acquired with the standard instrument.
#' @param x2 A matrix or data frame containing spectra acquired with the instrument to be standardized.
#' @param win An integer specifying the half size of the moving window used for PLSR. A larger value may improve the transfer but will increase computational time.
#' @param ncomp An integer specifying the number of components to be used in PLSR. Typically, a small number (e.g., 2-5) is sufficient.
#' @param alpha A numeric value between 0 and 1 specifying the weight for the global PLSR model. A value of 0 corresponds to the original PDS method, while a value of 1 corresponds to using only the global PLSR model.
#'
#' @return A list with two components:
#'   \item{`transfert_matrix`:}{A matrix containing the transfer coefficients.}
#'   \item{i`ntercept`:}{A vector containing the intercepts for each local PLSR
#'     model.}
#'
#' @references
#'   - Wang, Y., Veltkamp, D.J., Kowalski, B.R., (1991).
#'     Multivariate instrument standardization.
#'     Analytical Chemistry, 63(23):2750-2756.
#'   - Bouveresse, E., Massart, D.L., (1996).
#'     Improvement of the piecewise direct standardization procedure for the
#'     transfer of NIR spectra for multivariate calibration.
#'     Chemometrics and Intelligent Laboratory Systems, (32)2:201-213.
#'
#' @export pds
#'
pds <- function(x1, x2, win, ncomp, alpha) {
  if (ncol(x1) != ncol(x2)) {
    stop("Input matrices x1 and x2 must have the same number of columns.")
  }

  n_wavelengths <- ncol(x1)
  P <- matrix(0, nrow = n_wavelengths, ncol = n_wavelengths - (2 * win) + 2)
  interceptReg <- numeric(n_wavelengths - (2 * win) + 3)

  # Perform global PLSR
  global_plsfit <- pls::plsr(x1 ~ as.matrix(x2), ncomp = ncomp, scale = FALSE, method = "oscorespls")
  global_coefReg <- as.numeric(stats::coef(global_plsfit, ncomp = ncomp, intercept = TRUE))
  interceptReg[n_wavelengths - (2 * win) + 3] <- global_coefReg[1]
  global_coefReg <- global_coefReg[-1]

  # Loop over moving windows
  for (i in seq(win, n_wavelengths - win + 1)) {
    # Index range for the current window
    window_range <- (i - win):(i + win)

    # Perform local PLSR
    local_plsfit <- pls::plsr(x1[, i] ~ as.matrix(x2[, window_range]), ncomp = ncomp, scale = FALSE, method = "oscorespls")

    # Extract regression coefficients and intercept
    local_coefReg <- as.numeric(stats::coef(local_plsfit, ncomp = ncomp, intercept = TRUE))
    interceptReg[i - win + 1] <- local_coefReg[1]
    local_coefReg <- local_coefReg[-1]

    # Combine local and global coefficients
    combined_coefReg <- (1 - alpha) * local_coefReg + alpha * global_coefReg

    # Add coefficients to the transfer matrix
    P[window_range, i - win + 1] <- combined_coefReg

    # Display progression
    cat("\r", paste0(round(i / n_wavelengths * 100), " %"), sep = "")
  }

  # Pad the transfer matrix and intercept vector with zeros
  P <- cbind(matrix(0, nrow = n_wavelengths, ncol = win),
             P,
             matrix(0, nrow = n_wavelengths, ncol = win))
  interceptReg <- c(rep(0, win), interceptReg, rep(0, win))

  # Return the transfer matrix and intercept vector
  list(transfert_matrix = P, intercept = interceptReg)
}
