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
#' distribution into account, by splitting the univariate dataset in two half
#' samples around the median. And then apply one-step M-estimator with Huber
#' \eqn{\rho}-function for scaling each part.
#'
#' @references
#'  - Rousseeuw, P.J., Raymaekers, J., Hubert, M., (2018).
#'    A Measure of Directional Outlyingness With Applications to Image Data and Video.
#'    Journal of Computational and Graphical Statistics, 27(2):345–359.
#'
#' @param x A numeric vector
#' @param cutoff_quantile A numeric value between 0 and 1 specifying the quantile for outlier detection (default: 0.995).
#' @param rm_zeroes A logical value. If `TRUE`, removes values close to zero (default: `FALSE`).
#' @param max_ratio A numeric value greater than 2. If provided, constrains the ratio between positive and negative scales (default: `NULL`).
#' @param prec_scale A numeric value specifying the precision scale for near-zero comparisons (default: 1e-10).
#' @param cutoff.quantile,rmZeroes,maxRatio,precScale `r lifecycle::badge("deprecated")` Use
#'   `cutoff_quantile`, `rm_zeroes`, `max_ratio` and `prec_scale`.
#'
#' @return A tibble with columns:
#'   - `data`: The original numeric values.
#'   - `score`: The calculated outlyingness score.
#'   - `flag`: A logical vector indicating whether each value is a potential outlier or not.
#'
#' @export directional_outlyingness
#'
#' @examples
#' data(forageLIBS)
#' # iron contents (mg/kg): a few samples are far above the others
#' fe <- forageLIBS$Fe[!is.na(forageLIBS$Fe)]
#' head(directional_outlyingness(fe))
directional_outlyingness <- function(x, cutoff_quantile = 0.995, rm_zeroes = FALSE, max_ratio = NULL,
                                     prec_scale = 1e-10, cutoff.quantile = deprecated(),
                                     rmZeroes = deprecated(), maxRatio = deprecated(),
                                     precScale = deprecated()) {
  fn <- "directional_outlyingness"
  cutoff_quantile <- renamed_arg(cutoff.quantile, cutoff_quantile, fn, "cutoff.quantile", "cutoff_quantile")
  rm_zeroes <- renamed_arg(rmZeroes, rm_zeroes, fn, "rmZeroes", "rm_zeroes")
  max_ratio <- renamed_arg(maxRatio, max_ratio, fn, "maxRatio", "max_ratio")
  prec_scale <- renamed_arg(precScale, prec_scale, fn, "precScale", "prec_scale")
  if (missing(x)) {
    stop("Missing 'x' argument.")
  }
  check_numeric_vector(x, "directional_outlyingness")
  if (!is.numeric(cutoff_quantile) || cutoff_quantile < 0 || cutoff_quantile > 1) {
    stop("The input 'cutoff_quantile' must be a numeric value between 0 and 1.")
  }
  if (!is.logical(rm_zeroes)) {
    stop("'rm_zeroes' must be of type boolean (TRUE or FALSE)")
  }
  if (!is.numeric(prec_scale) || prec_scale < 0) {
    stop("'prec_scale' must be a positive numeric value.")
  }

  x <- x[!is.na(x)]
  if (length(x) < 3) {
    stop("'x' must contain at least 3 non-missing values.")
  }
  score <- NULL
  med <- stats::median(x)
  xc <- x - med
  n <- length(xc)
  h <- n %/% 2
  xa <- xc[xc > 0]
  xb <- xc[xc < 0]
  xa <- c(rep(0, (n - h - length(xa))), xa)
  xb <- c(rep(0, (n - h - length(xb))), abs(xb))

  if (rm_zeroes){
    xa <- xa[xa > prec_scale]
    xb <- xb[xb > prec_scale]
  }

  sa <- scale1StepM(x = xa, precScale = prec_scale)
  sb <- scale1StepM(x = xb, precScale = prec_scale)

  if (!is.null(max_ratio)) {
    if (!is.numeric(max_ratio) || length(max_ratio) != 1 || max_ratio < 2) {
      stop("max_ratio must be at least 2")
    }
    sall <- scale1StepM(x = xc, precScale = prec_scale)
    sa <- min(max(sa, sall / max_ratio), sall * max_ratio)
    sb <- min(max(sb, sall / max_ratio), sall * max_ratio)
  }

  res <- ifelse(x >= med, (x - med) / sa, (med - x) / sb)
  res[x == med] <- 0
  cutoff <- computeCutoff(res, cutoff_quantile)

  tbl <- tibble::tibble(
    data = x,
    score = res,
    flag = res > cutoff
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
