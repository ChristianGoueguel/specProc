#' @title Directional Outlyingness for Skewed Distribution
#'
#' @author Christian L. Goueguel
#'
#' @description
#' This function computes the directional outlyingness of a numeric vector, as
#' proposed by Rousseeuw *et al.* (2018), which is a measure of outlyingness of
#' data point that takes the skewness of the underlying distribution into account.
#'
#' @details
#' Directional outlyingness takes the potential skewness of the underlying
#' distribution into account, by the splitting the univariate dataset in two half
#' samples around the median. And then apply one-step M-estimator with Huber
#' \eqn{\rho}-function for scaling each part.
#'
#' @references
#'  - Rousseeuw, P.J., Raymaekers, J., Hubert, M., (2018).
#'    A Measure of Directional Outlyingness With Applications to Image Data and Video.
#'    Journal of Computational and Graphical Statistics, 27(2):345–359.
#'
#' @param x A numeric vector
#' @param cutoff.quantile A numeric value between 0 and 1 specifying the quantile for outlier detection (default: 0.995).
#' @param rmZeroes A logical value. If `TRUE`, removes values close to zero (default: `FALSE`).
#' @param maxRatio A numeric value greater than 2. If provided, constrains the ratio between positive and negative scales (default: `NULL`).
#' @param precScale A numeric value specifying the precision scale for near-zero comparisons (default: 1e-10).
#'
#' @return A tibble with columns:
#'   - `data`: The original numeric values.
#'   - `score`: The calculated outlyingness score.
#'   - `flag`: A logical vector indicating whether each value is a potential outlier or not.
#'
#' @export directOutlyingness
#'
#' @examples
#' x <- c(1, 5, 3, 9, 2, 6, 4, 8, 7, 1e3)
#' directOutlyingness(x)
#'
directOutlyingness <- function(x, cutoff.quantile = 0.995, rmZeroes = FALSE, maxRatio = NULL, precScale = 1e-10) {
  if (missing(x)) {
    stop("Missing 'x' argument.")
  }
  if (!is.numeric(x)) {
    stop("The input 'x' must be a numeric vector.")
  }
  if (!is.numeric(cutoff.quantile) || cutoff.quantile < 0 || cutoff.quantile > 1) {
    stop("The input 'cutoff.quantile' must be a numeric value between 0 and 1.")
  }
  if (!is.logical(rmZeroes)) {
    stop("'rmZeroes' must be of type boolean (TRUE or FALSE)")
  }
  if (!is.numeric(precScale) || precScale < 0) {
    stop("'precScale' must be a positive numeric value.")
  }

  x <- x[!is.na(x)]
  med <- stats::median(x)
  xc <- x - med
  n <- length(xc)
  h <- n %/% 2
  xa <- xc[xc > 0]
  xb <- xc[xc < 0]
  xa <- c(rep(0, (n - h - length(xa))), xa)
  xb <- c(rep(0, (n - h - length(xb))), abs(xb))

  if (rmZeroes){
    xa <- xa[xa > precScale]
    xb <- xb[xb > precScale]
  }

  if (!is.null(maxRatio)) {
    if (maxRatio < 2) {
      stop("maxRatio must be at least 2")
    } else {
      sall <- scale1StepM(x = xc, precScale = precScale)
      sa <- min(c(max(sa, sall / maxRatio, na.rm = TRUE), sall * maxRatio), na.rm = TRUE)
      sb <- min(c(max(sb, sall / maxRatio, na.rm = TRUE), sall * maxRatio), na.rm = TRUE)
    }
  } else {
    sa <- scale1StepM(x = xa, precScale = precScale)
    sb <- scale1StepM(x = xb, precScale = precScale)
  }

  res <- dplyr::if_else(x >= med, (x - med) / sa, (med - x) / sb)
  cutoff <- computeCutoff(res, cutoff.quantile)

  tbl <- tibble::tibble(
    data = x,
    score = res,
    flag = dplyr::if_else(res > cutoff, TRUE, FALSE)
  ) %>%
    dplyr::arrange(dplyr::desc(score))

  return(tbl)
}

rhoHuber <- function(x, c = 2.1){
  rho <- (x / c)^2
  rho[rho > 1] <- 1
  r <- 1.54^2 * rho
  return(r)
}

loc1StepM <- function(x, c1 = 3, precScale) {
  x <- x[!is.na(x)]
  medx <- stats::median(x)
  ax <- abs(x - medx)
  denom <- c1 * stats::median(ax)
  mu <- if (denom > precScale) {
    ax = ax/denom
    w = 1 - ax * ax
    w = ((abs(w) + w)/2)^2
    sum(x * w ) / sum(w)
  }
  else {
    medx
    }
  return(mu)
}

scale1StepM <- function(x, precScale) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n == 0) {
    return(0.0)
  } else {
    sigma0 <- 1.4826 * stats::median(abs(x))
    if(sigma0 < precScale) {
      return(0.0)
    } else {
      rho <- rhoHuber(x / sigma0)
      return(sigma0 * sqrt(sum(rho) * 2 / n))
    }
  }
}

computeCutoff <- function(outl, quant) {
  Ltemp <- log(0.1 + outl)
  cutoff <- exp(stats::qnorm(quant) * stats::mad(Ltemp) + stats::median(Ltemp)) - 0.1
  return(cutoff)
}
