#' @title Quantile Tail Weight Measure
#'
#' @description
#' This function calculates the left quantile weight (LQW) and the right
#' quantile weight (RQW) for a given numeric vector. These weights serve as
#' robust measures of tail heaviness, providing insights into the distribution's
#' behavior in the left and right tails, respectively.
#'
#' @details
#' The left quantile weight (LQW) and the right quantile weight (RQW) are
#' robust measures of tail weight (Brys *et al.* 2006). They apply the
#' quantile skewness of Hinkley (1975),
#' \deqn{SK(p) = \frac{Q(1-p) + Q(p) - 2Q(0.5)}{Q(1-p) - Q(p)},}
#' to each half of the distribution, divided at the median: the left half,
#' whose median is the first quartile \eqn{Q(0.25)}, and the right half, whose
#' median is the third quartile \eqn{Q(0.75)}:
#' \deqn{LQW(p) = -\frac{Q((1-p)/2) + Q(p/2) - 2Q(0.25)}{Q((1-p)/2) - Q(p/2)}}
#' \deqn{RQW(q) = \frac{Q((1+q)/2) + Q(1-q/2) - 2Q(0.75)}{Q((1+q)/2) - Q(1-q/2)}}
#' where \eqn{Q} is the quantile function of the data. `p` and `q` are
#' probabilities that choose the quantiles compared: LQW compares
#' \eqn{Q(p/2)} and \eqn{Q((1-p)/2)}, the \eqn{p} and \eqn{1 - p} quantiles
#' of the left half, around \eqn{Q(0.25)}; RQW compares \eqn{Q(1-q/2)} and
#' \eqn{Q((1+q)/2)}, the \eqn{1 - q} and \eqn{q} quantiles of the right
#' half, around \eqn{Q(0.75)}. A heavier tail stretches the outer quantile
#' further than the inner one, which gives a larger weight. With
#' \eqn{q = 1 - p}, both weights look equally far into their tails. The
#' defaults, `p = 0.125` and `q = 0.875`, compare \eqn{Q(0.0625)} and
#' \eqn{Q(0.4375)} for LQW, and \eqn{Q(0.5625)} and \eqn{Q(0.9375)} for
#' RQW.
#'
#' The breakdown value of LQW is \eqn{p/2} and that of RQW is
#' \eqn{(1 - q)/2}: 6.25% for the defaults, which resist up to 6.25% of
#' outliers in a tail. A smaller `p` (or a `q` closer to 1) looks further into
#' the tails, which makes the weights more sensitive to the tail weight but
#' less robust; `p = 0.25` and `q = 0.75` give a breakdown value of 12.5%, as
#' [medcouple_weight()].
#'
#' Interpretation of the quantile weights:
#'  - At the normal distribution, LQW = RQW = 0.249 with the defaults (0.144
#'    with `p = 0.25` and `q = 0.75`).
#'  - Larger values indicate heavier tails than the normal distribution, and
#'    smaller values lighter tails.
#'
#' @references
#'  - Brys, G., Hubert, M., and Struyf, A. (2006). Robust measures of tail weight.
#'    Computational Statistics & Data Analysis, 50(3):733-759
#'  - Hinkley, D.V., (1975). On power transformations to symmetry.
#'    Biometrika, 62(1):101–111.
#'
#' @author Christian L. Goueguel
#'
#' @param x A numeric vector.
#' @param p A probability between 0 and 0.5 that chooses the quantiles of
#'   the left half compared by LQW: \eqn{Q(p/2)} and \eqn{Q((1-p)/2)} of the
#'   data. A smaller value looks further into the left tail. Default is 0.125.
#' @param q A probability between 0.5 and 1 that chooses the quantiles of
#'   the right half compared by RQW: \eqn{Q(1-q/2)} and \eqn{Q((1+q)/2)} of
#'   the data. A larger value looks further into the right tail. Default is
#'   0.875.
#' @param drop_na Logical value indicating whether to remove missing values (NA). If `FALSE` (default) and `x` contains missing values, `NA` is returned.
#' @param drop.na `r lifecycle::badge("deprecated")` Use `drop_na`.
#'
#' @return A tibble with two numeric columns:
#'  - `LQW`: Left quantile weight.
#'  - `RQW`: Right quantile weight.
#'
#' @seealso [medcouple_weight()], [medcouple()]
#'
#' @examples
#' # 10000 values from three symmetric distributions: uniform (light tails),
#' # normal, and Laplace (double exponential: heavy tails)
#' set.seed(1)
#' n <- 10000
#' tails <- data.frame(
#'   distribution = factor(rep(c("uniform", "normal", "Laplace"), each = n),
#'                         levels = c("uniform", "normal", "Laplace")),
#'   value = c(runif(n, -2, 2), rnorm(n),
#'             rexp(n) * sample(c(-1, 1), n, replace = TRUE))
#' )
#' ggplot2::ggplot(tails, ggplot2::aes(value, fill = distribution)) +
#'   ggplot2::geom_density(alpha = 0.4) +
#'   ggplot2::coord_cartesian(xlim = c(-5, 5)) +
#'   ggplot2::scale_fill_manual(values = c("#fdb863", "#2166ac", "#b2182b"))
#'
#' # the weights grow with the tail weight, in both tails: about 0 for the
#' # uniform distribution, 0.25 for the normal one and 0.43 for the Laplace one
#' weights <- function(v) c(kurtosis = moments::kurtosis(v), unlist(quantile_weight(v)))
#' round(t(sapply(split(tails$value, tails$distribution), weights)), 3)
#'
#' # 1% of outliers: 9900 normal values and 100 near 10. The kurtosis jumps
#' # from 3 to 25, while the right quantile weight hardly moves and the left
#' # one does not see the outliers
#' set.seed(2)
#' x <- c(rnorm(9900), rnorm(100, mean = 10, sd = 0.5))
#' group <- rep(c("regular", "outlier"), c(9900, 100))
#' ggplot2::ggplot(data.frame(x, group), ggplot2::aes(x)) +
#'   ggplot2::geom_density(fill = "#2166ac", alpha = 0.3) +
#'   ggplot2::geom_rug(ggplot2::aes(colour = group)) +
#'   ggplot2::scale_colour_manual(values = c(outlier = "#b2182b", regular = "#2166ac"))
#' round(weights(x), 3)
#'
#' data(forageLIBS)
#' # iron contents (mg/kg): a few samples are far above the others
#' fe <- forageLIBS$Fe[!is.na(forageLIBS$Fe)]
#' moments::kurtosis(fe)   # classical
#' quantile_weight(fe)     # robust, left and right tails
#'
#' @export quantile_weight
#'
quantile_weight <- function(x, p = 0.125, q = 0.875, drop_na = FALSE,
                            drop.na = deprecated()) {
  drop_na <- renamed_arg(drop.na, drop_na, "quantile_weight", "drop.na", "drop_na")
  if (missing(x)) {
    stop("Missing 'x' argument.")
  }
  check_numeric_vector(x, "quantile_weight")
  if (!is.logical(drop_na)) {
    stop("The input 'drop_na' must be a logical value (TRUE or FALSE).")
  }
  check_number(p, "p", lower = 0, upper = 0.5, lower_open = TRUE, upper_open = TRUE)
  check_number(q, "q", lower = 0.5, upper = 1, lower_open = TRUE, upper_open = TRUE)

  if (drop_na) {
    x <- x[!is.na(x)]
  } else if (anyNA(x)) {
    return(tibble::tibble(LQW = NA_real_, RQW = NA_real_))
  }

  Q <- function(prob) unname(stats::quantile(x, prob, type = 7))

  ql1 <- Q((1 - p) / 2)
  ql2 <- Q(p / 2)
  qr1 <- Q((1 + q) / 2)
  qr2 <- Q(1 - q / 2)

  w_tbl <- tibble::tibble(
    LQW = -(ql1 + ql2 - 2 * Q(0.25)) / (ql1 - ql2),
    RQW = (qr1 + qr2 - 2 * Q(0.75)) / (qr1 - qr2)
  )

  return(w_tbl)
}
