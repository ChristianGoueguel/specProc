#' @title Unbiased Median Absolute Deviation
#'
#' @author Christian L. Goueguel
#'
#' @description
#' This function calculates the Median Absolute Deviation (MAD) scale
#' estimator for a numeric vector, using the Park-Kim-Wang approach. The method
#' adds a small sample correction factor to make MAD unbiased at the normal distribution.
#'
#' @details
#' The correction factor, \eqn{C}, is calculated differently based on the sample size \eqn{n}:
#'    - For \eqn{n > 100}, \eqn{C} is calculated using an analytical approximation
#'      proposed by either Hayes (2014) or Williams (2011).
#'    - For \eqn{n \leq 100}, \eqn{C} is obtained from a pre-computed table of
#'      values proposed by Park *et al.* (2020).
#'
#' @references
#'    - Park, C., Kim, H., Wang, M., (2020).
#'      Investigation of finite-sample properties of robust location and scale
#'      estimators.
#'      Communications in Statistics - Simulation and Computation, 51(5):2619–2645.
#'    - Hayes, K., (2014).
#'      Finite-Sample Bias-Correction Factors for the Median
#'      Absolute Deviation.Communications in Statistics - Simulation and Computation, 43(10):2205–2212.
#'    - Williams, D.C., (2011).
#'      Finite sample correction factors for several simple robust estimators of
#'      normal standard deviation.
#'      Journal of Statistical Computation and Simulation, 81(11):1697–1702.
#'
#' @param x A numeric vector.
#' @param method A character string specifying the method to use for calculating the correction factor when the number of sample is more than 100. The available options are "hayes" (default) and "williams".
#' @param drop.na A logical value indicating whether to remove missing values (NA) from the calculations. If `TRUE` (the default), missing values will be removed. If `FALSE`, missing values will be included.
#'
#' @return The MAD scale estimate for the input vector `x`.
#'
#' @export umad
#'
umad <- function(x, method = "hayes", drop.na = TRUE) {
  if (missing(x)) {
    stop("Missing 'x' argument.")
  }
  if (!is.numeric(x)) {
    stop("'x' must be a numeric vector.")
  }
  if (!is.logical(drop.na) || length(drop.na) != 1) {
    stop("'drop.na' must be a single logical value (TRUE or FALSE).")
  }
  if (!is.character(method) || !(method %in% c("hayes", "williams"))) {
    stop("'method' must be either 'hayes' or 'williams'.")
  }

  if (drop.na) {
    x <- x[!is.na(x)]
  }

  if (length(x) < 2) {
    stop("The size of 'x' must be greater than 1")
  } else {
    n <- length(x)
  }

  if (all(x == x[1])) {
    stop("All values in the vector 'x' are the same (constant value).")
  }

  med_x <- stats::median(x)
  mad_x <- stats::median(abs(x - med_x))

  if (n > 100) {
    if (method == "hayes") {
      a <- -(0.76213 / n) - (0.86413 / n^2)
    } else {
      a <- -0.804168866 * n^(-1.008922)
    }
    c_n <- 1 / (stats::qnorm(3/4) * (1 + a))
  } else {
    factors <- c(
      NA, 1.772150, 2.204907, 2.016673, 1.803927, 1.763788, 1.686813,
      1.671843, 1.632940, 1.624681, 1.601308, 1.596155, 1.580754, 1.577272,
      1.566339, 1.563769, 1.555284, 1.553370, 1.547206, 1.545705, 1.540681,
      1.539302, 1.535165, 1.534053, 1.530517, 1.529996, 1.526916, 1.526422,
      1.523608, 1.523031, 1.520732, 1.520333, 1.518509, 1.517941, 1.516279,
      1.516070, 1.514425, 1.513989, 1.512747, 1.512418, 1.511078, 1.511041,
      1.509858, 1.509499, 1.508529, 1.508365, 1.507535, 1.507247, 1.506382,
      1.506307, 1.505611, 1.505172, 1.504575, 1.504417, 1.503713, 1.503604,
      1.503095, 1.502864, 1.502253, 1.502085, 1.501611, 1.501460, 1.501019,
      1.500841, 1.500331, 1.500343, 1.499877, 1.499772, 1.499291, 1.499216,
      1.498922, 1.498838, 1.498491, 1.498399, 1.497917, 1.497901, 1.497489,
      1.497544, 1.497248, 1.497185, 1.496797, 1.496779, 1.496428, 1.496501,
      1.496295, 1.496089, 1.495794, 1.495796, 1.495557, 1.495420, 1.495270,
      1.495141, 1.494944, 1.494958, 1.494706, 1.494665, 1.494379, 1.494331,
      1.494113, 1.494199
    )
    c_n <- factors[n]
  }

  mad_n <- c_n * mad_x
  return(mad_n)
}

