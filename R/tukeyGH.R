#' @title Tukey g-and-h Parametric Distribution
#'
#' @author Christian L. Goueguel
#'
#' @description
#' The Tukey g-and-h (TGH) distribution is a flexible family of parametric distributions
#' that can accommodate a wide range of skewness and kurtosis values, making it
#' useful for modeling non-normal data.
#'
#' @details
#' The Tukey g-and-h distribution, introduced by John W. Tukey in 1977, is
#' defined by two transformation functions, g and h, which are applied to a
#' standard normal distribution. The g transformation controls the skewness of
#' the distribution, while the h transformation controls the kurtosis
#' (or heaviness of the tails). The probability density function (PDF) of the
#' TGH distribution is given by:
#'
#' \deqn{f(x; \mu, \sigma, g, h) = \frac{1}{\sigma} \phi(\frac{x - \mu}{\sigma T_g(z) T_h(z)})  \frac{dT_g(z)}{dz}  \frac{dT_h(z)}{dz}}
#'
#' where:
#'  - \eqn{\mu} is the location parameter (shifting the distribution horizontally)
#'  - \eqn{\sigma} is the scale parameter (stretching or shrinking the distribution)
#'  - \eqn{g} is the skewness parameter
#'  - \eqn{h} is the kurtosis parameter
#'  - \eqn{\phi(z)} is the standard normal PDF
#'  - \eqn{T_g(z)} is the Tukey g-transformation function: \eqn{\frac{(e^{gz} - 1)}{g}} if \eqn{g != 0}, or \eqn{z} if \eqn{g = 0}
#'  - \eqn{T_h(z)} is the Tukey h-transformation function: \eqn{e^{\frac{h z^2}{2}}}
#'
#' @references
#'  - Tukey, J.W., (1977).
#'    Modern techniques in data analysis.
#'    NSF‐sponsored regional research conference at Southeastern Massachusetts
#'    University, North Dartmouth, MA.
#'  - Martinez, J., Iglewicz, B., (1984).
#'    Some properties of the Tukey g and h family of distributions.
#'    Communications in Statistics:Theory and Methods, 13(3):353–369.
#'  - Hoaglin, D.C., (1985).
#'    Summarizing shape numerically: The g-and-h distributions.
#'    In: Hoaglin, D.C., Mosteller, F., Tukey, J.W., (eds),
#'    Data Analysis for Tables, Trends, and Shapes. New York:Wiley.
#'
#' @param x A numeric vector or a single value, depending on the function being called.
#' @param type A character string specifying the function to be called (`d` for density, `p` for cumulative distribution, `q` for quantile, or `r` for random number generation).
#' @param location The location parameter of the TGH distribution.
#' @param scale The scale parameter of the TGH distribution.
#' @param g The skewness parameter of the TGH distribution.
#' @param h The kurtosis parameter of the TGH distribution.
#' @param log If `TRUE`, the log-density is returned for the density function. Default is `FALSE`.
#' @param log.p If `TRUE`, the log-probability is returned for the cumulative distribution and quantile functions. Default is `FALSE`.
#' @param n For random number generation, the number of random values to be generated. Default is `NULL`.
#'
#' @return A numeric vector.
#' @export tukeyGH
#'
#' @examples
#' # Example 1: g = 0 and h = 0 (Gaussian distribution)
#' curve(tukeyGH(x, type = "d", location = 0, scale = 1, g = 0, h = 0),
#' -5, 5, col = "blue", ylab = "y", add = TRUE);
#' curve(gaussian(x, 0, 0, 2.3, 1), -5, 5, col = "red", add = TRUE)
#'
#' # Example 2: g = 1 and h = 0 (skewed distribution)
#' curve(tukeyGH(x, type = "d", location = 0, scale = 1, g = 1, h = 0.1),
#' -3, 3, col = "blue", ylab = "y", add = TRUE);
#' curve(gaussian(x, 0, 0, 2.5, 1.5), -3, 3, col = "red", add = TRUE)
#'
#' # Example 3: g = 0 and h = 1 (heavy-tail distribution)
#' curve(tukeyGH(x, type = "d", location = 0, scale = 1, g = 0.1, h = 1),
#' -5, 5, col = "blue", ylab = "y", add = TRUE);
#' curve(gaussian(x, 0, 0, 2.3, 1), -5, 5, col = "red", add = TRUE)
#'
tukeyGH <- function(x, type = "d", location = 0, scale = 1, g = 0, h = 0, log = FALSE, log.p = FALSE, n = NULL) {
  if (type == "d") {
    dgh(x, location, scale, g, h, log)
  } else if (type == "p") {
    pgh(x, location, scale, g, h, log.p)
  } else if (type == "q") {
    qgh(x, location, scale, g, h, log.p)
  } else if (type == "r") {
    if (is.null(n)) {
      stop("'n' is required for random number generation.")
    }
    rgh(n, location, scale, g, h)
  } else {
    stop("Invalid 'type'. Must be one of 'd', 'p', 'q', or 'r'.")
  }
}


tg <- function(x, g) {
  if (g == 0) x
  else (exp(g * x) - 1) / g
}

th <- function(x, h) {
  exp((x^2 * h) / 2)
}

deriv_gh <- function(z, g, h) {
  if (g == 0) exp(h * z^2 * 0.5) * (1 + h * z^2)
  else exp(h * z^2 * 0.5) * (exp(g * z) + h * z * (exp(g * z) - 1) / g)
}

gh_inv <- function(x, g, h) {
  purrr::map_dbl(
    x, ~ {
      if (h > 0) {
        f1 <- function(z, x, g, h) {
          tg(z, g) * th(z, h) - x
        }
        tryCatch(
          stats::uniroot(f1, x = .x, g = g, h = h, interval = c(-10, 10), extendInt = "yes")$root,
          error = function(err) .x * Inf
        )
      }
    }
  )
}

dgh <- function(x, location, scale, g, h, log = FALSE) {
  purrr::map_dbl(
    x, ~ {
      x_std <- (.x - location) / scale
      if (g == 0 && h == 0) {
        v <- stats::dnorm(x_std) / scale
      } else if (h == 0) {
        if ((x_std <= -1 / g && g > 0) || (x_std >= -1 / g && g < 0)) y <- NaN
        else y <- log(1 + g * x_std) / g
        v <- stats::dnorm(y) / (scale * exp(g * y))
      } else if (h > 0) {
        y <- gh_inv(x_std, g, h)
        v <- stats::dnorm(y) / (scale * deriv_gh(y, g, h))
      } else {
        stop("Negative kurtosis parameter")
      }
      if (log == FALSE) v else log(v)
    }
  )
}

pgh <- function(q, location, scale, g, h, log.p = FALSE) {
  purrr::map_dbl(
    q, ~ {
      if (g == 0 && h == 0) {
        l <- stats::pnorm(.x, location, scale)
      } else if (h == 0) {
        q_std <- (.x - location) / scale
        if ((q_std <= -1 / g && g > 0) || (q_std >= -1 / g && g < 0)) l <- NaN
        else {
          l <- stats::pnorm(log(1 + g * q_std) / g)
        }
      } else if (h > 0) {
        q_std <- (.x - location) / scale
        z <- gh_inv(q_std, g, h)
        l <- stats::pnorm(z)
      } else {
        stop("Negative kurtosis parameter")
      }
      if (log.p == FALSE) l else log(l)
    }
  )
}

qgh <- function(q, location, scale, g, h, log.p = FALSE) {
  purrr::map_dbl(
    q, ~ {
      if (h >= 0) {
        u <- stats::qnorm(.x, log.p = log.p)
        location + scale * tg(u, g) * th(u, h)
      } else {
        stop("Negative kurtosis parameter")
      }
    }
  )
}

rgh <- function(n, location, scale, g, h) {
  if (h >= 0) {
    z <- stats::rnorm(n)
    location + scale * tg(z, g) * th(z, h)
  } else {
    stop("Negative kurtosis parameter")
  }
}
