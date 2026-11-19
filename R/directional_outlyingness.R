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
#' samples around the median, each with
#' \eqn{h = \lfloor (n+1)/2 \rfloor}{h = floor((n + 1)/2)}
#' observations (they share the median when \eqn{n} is odd), and scaling each
#' half separately (Rousseeuw *et al.*, 2018, Section 2.1). For the distances
#' \eqn{z_i} of a half sample to the median, the scale is the one-step
#' M-estimator
#' \deqn{s = s_0 \sqrt{\frac{1}{2 \alpha h} \sum_i \rho_c\left(\frac{z_i}{s_0}\right)},}{s = s0 sqrt(sum(rho_c(z_i / s0)) / (2 alpha h)),}
#' from the initial scale
#' \eqn{s_0 = \mathrm{med}_i(z_i) / \Phi^{-1}(0.75)}{s0 = med(z_i) / qnorm(0.75)},
#' with Huber's \eqn{\rho}-function for scale,
#' \eqn{\rho_c(t) = \min\{(t/c)^2, 1\}}{rho_c(t) = min((t/c)^2, 1)},
#' \eqn{c = 2.1} (85% efficiency at
#' the normal distribution), and
#' \eqn{\alpha = \int_0^\infty \rho_c(x) \, d\Phi(x)}{alpha = integral of rho_c(x) dPhi(x) over (0, Inf)}, which makes the
#' scale consistent at the normal distribution.
#'
#' The score of an observation is its distance to the median divided by the
#' scale of its own half: on the short side of a skewed distribution, an
#' observation gets a higher score than one as far from the median on the
#' long side. For a normal distribution, both scales estimate the standard
#' deviation, and the score is close to the absolute robust z-score of
#' [zscore()].
#'
#' An observation is flagged when its score exceeds the
#' \eqn{(1 + q) / 2} quantile of the standard normal distribution, where
#' \eqn{q} is `cutoff_quantile` (2.81 for the default 0.995): about
#' \eqn{1 - q} of the observations of a normal distribution (0.5%) are
#' flagged.
#'
#' @references
#'  - Rousseeuw, P.J., Raymaekers, J., Hubert, M., (2018).
#'    A Measure of Directional Outlyingness With Applications to Image Data and Video.
#'    Journal of Computational and Graphical Statistics, 27(2):345–359.
#'
#' @param x A numeric vector
#' @param cutoff_quantile A numeric value in (0, 1): the proportion of the
#'   observations of a normal distribution that are not flagged. The
#'   cutoff of the scores is the `(1 + cutoff_quantile) / 2` quantile of
#'   the standard normal distribution. Default is 0.995 (a cutoff of 2.81).
#' @param rm_zeroes A logical value. If `TRUE`, the zero distances (the
#'   observations equal to the median, within `prec_scale`) are left out of
#'   the half samples before their scales are computed, which matters when
#'   many observations are tied at the median. Default is `FALSE`, as in the
#'   paper.
#' @param max_ratio `NULL` (default), or a number of at least 2: each scale
#'   is then kept between the scale of all the distances to the median divided
#'   and multiplied by `max_ratio`, so that a very short or very long half
#'   cannot give extreme scores. Not in the paper.
#' @param prec_scale A non-negative number: an initial scale below it is taken
#'   as zero (default: 1e-10).
#' @param cutoff.quantile,rmZeroes,maxRatio,precScale `r lifecycle::badge("deprecated")` Use
#'   `cutoff_quantile`, `rm_zeroes`, `max_ratio` and `prec_scale`.
#'
#' @return A tibble with columns:
#'   - `data`: The original numeric values.
#'   - `score`: The calculated outlyingness score.
#'   - `flag`: A logical vector indicating whether each value is a potential outlier or not.
#'
#' @seealso [zscore()], [iqr_outliers()]
#'
#' @export directional_outlyingness
#'
#' @examples
#' # 200 right-skewed values (a gamma distribution shifted to start at 100),
#' # 3 low outliers below them and 3 high outliers
#' set.seed(1)
#' x <- c(100 + rgamma(200, shape = 2, scale = 10), 85, 87, 89, 220, 230, 240)
#' group <- rep(c("regular", "outlier"), c(200, 6))
#' ggplot2::ggplot(data.frame(x, group), ggplot2::aes(x)) +
#'   ggplot2::geom_density(fill = "#2166ac", alpha = 0.3) +
#'   ggplot2::geom_rug(ggplot2::aes(colour = group)) +
#'   ggplot2::scale_colour_manual(values = c(outlier = "#b2182b", regular = "#2166ac"))
#'
#' # the robust z-score scales both sides alike: regular values of the long
#' # right tail are flagged, but not the low outliers (the last three rows)
#' rob <- zscore(x, robust = TRUE)
#' subset(rob, flag)
#' tail(rob, 3)
#' # directional outlyingness scales each side separately: the low outliers
#' # score above the regular values of the long tail, and the 6 outliers are
#' # flagged
#' head(directional_outlyingness(x), 8)
#'
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
  check_number(cutoff_quantile, "cutoff_quantile", lower = 0, upper = 1,
               lower_open = TRUE, upper_open = TRUE)
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
  # the two half samples of Section 2.1 of the paper, as distances to the
  # median: the h largest observations (above) and the h smallest (below)
  med <- stats::median(x)
  n <- length(x)
  h <- (n + 1) %/% 2
  sorted <- sort(x)
  above <- sorted[(n - h + 1):n] - med
  below <- med - sorted[seq_len(h)]
  if (rm_zeroes) {
    above <- above[above > prec_scale]
    below <- below[below > prec_scale]
  }
  sa <- scale_1step_m(above, prec_scale)
  sb <- scale_1step_m(below, prec_scale)

  if (!is.null(max_ratio)) {
    if (!is.numeric(max_ratio) || length(max_ratio) != 1 || max_ratio < 2) {
      stop("max_ratio must be at least 2")
    }
    s_all <- scale_1step_m(abs(x - med), prec_scale)
    sa <- min(max(sa, s_all / max_ratio), s_all * max_ratio)
    sb <- min(max(sb, s_all / max_ratio), s_all * max_ratio)
  }

  res <- ifelse(x >= med, (x - med) / sa, (med - x) / sb)
  res[x == med] <- 0
  cutoff <- stats::qnorm((1 + cutoff_quantile) / 2)

  tbl <- tibble::tibble(
    data = x,
    score = res,
    flag = res > cutoff
  ) %>%
    dplyr::arrange(dplyr::desc(score))

  return(tbl)
}

# Huber's rho function for scale (Rousseeuw, Raymaekers and Hubert, 2018,
# Section 2.1): (t / c)^2 within [-c, c] and 1 outside.
huber_rho <- function(t, c = 2.1) {
  pmin((t / c)^2, 1)
}

# alpha = integral of huber_rho() over (0, Inf) with respect to the standard
# normal distribution: int_0^c (x / c)^2 dPhi(x) + P(Z > c), where
# int_0^c x^2 dPhi(x) = Phi(c) - 1/2 - c phi(c). It makes the scale of
# scale_1step_m() consistent at the normal distribution (0.10625 for c = 2.1).
huber_rho_alpha <- function(c = 2.1) {
  (stats::pnorm(c) - 0.5 - c * stats::dnorm(c)) / c^2 + stats::pnorm(c, lower.tail = FALSE)
}

# One-step M-estimator of the scale of the distances z >= 0 of a half sample
# to the median (equation 2 of the paper): from the initial scale
# s0 = med(z) / qnorm(0.75),
#   s = s0 * sqrt(sum(huber_rho(z / s0)) / (2 * alpha * h)),
# with h the number of distances. Zero for an empty half sample, or when s0
# is below prec_scale.
scale_1step_m <- function(z, prec_scale, c = 2.1) {
  z <- z[!is.na(z)]
  h <- length(z)
  if (h == 0) {
    return(0)
  }
  s0 <- stats::median(z) / stats::qnorm(0.75)
  if (s0 < prec_scale) {
    return(0)
  }
  s0 * sqrt(sum(huber_rho(z / s0, c)) / (2 * huber_rho_alpha(c) * h))
}
