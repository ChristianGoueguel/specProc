#' @title Tukey g-and-h Distribution
#'
#' @description
#' Generates random numbers from the Tukey's *g*-and-*h* distribution.
#'
#' @param n The number of random numbers to generate.
#' @param g Parameter controlling the kurtosis (peakedness) of the distribution.
#' @param h Parameter controlling the skewness (asymmetry) of the distribution.
#'
#' @return A vector of length `n`.
#'
#' @examples
#' # Example 1:
#' y1 = tukeyGH(n = 100, g = 0.001, h = 0.001)
#' y2 = tukeyGH(n = 100, g = 0.001, h = 0.5)
#' y3 = tukeyGH(n = 100, g = 0.001, h = 1)
#'
#' plot(1:100, y1, type = "l", col = "red", main = "h ~ 0")
#' lines(1:100, y2, col = "blue")
#' lines(1:100, y3, col = "green")
#' legend("topright", legend = c("h ~ 0", "h = 0.5", "h = 1"),
#' col = c("red", "blue", "green"), lty = 1)
#'
#'
#' @export
tukeyGH <- function(n, g, h) {
  if (g <= 0 || h <= 0) {
    stop("The 'g' and 'h' parameters must be greater than 0.")
  }

  u <- stats::runif(n)
  z <- stats::qnorm(u)
  y <- (1 / g) * (exp(g * z) - exp(-g * z)) * exp((h^2 * z^2) / 2)

  return(y)
}
