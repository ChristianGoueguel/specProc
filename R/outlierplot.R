#' @title Univariate Representation of Multivariate Outliers
#'
#' @author Christian L. Goueguel
#'
#' @description
#' This function creates a visual representation of multivariate outliers using
#' a univariate plot.It uses robust covariance estimation methods to identify
#' outliers and provides options for displaying the results through various
#' plotting styles.
#'
#' @details
#' The function uses the Minimum Covariance Determinant (MCD) method to compute robust estimates
#' of location and scatter. It then applies an adaptive reweighting step to further improve
#' the outlier detection. The results are visualized using `ggplot2`, with options to highlight
#' outliers and/or color-code points based on their Mahalanobis distances.
#'
#' @param x A matrix or data frame.
#' @param quan A numeric value, between 0.5 and 1, that specifies the amount of observations which are used for MCD estimations. Default is 0.5.
#' @param alpha A numeric value specifying the amount of observations used for calculating the adjusted quantile. Default is 0.025.
#' @param show.outlier A logical value, if `TRUE` (default), outliers are highlighted in the plot.
#' @param show.mahal A logical value, if `FALSE` (default), robust Mahalanobis distances are not color-coded in the plot.
#'
#' @return Depending on the combination of `show.outlier` and `show.mahal`:
#'  - A `ggplot` object with outliers highlighted (if `show.outlier = TRUE`)
#'  - A `ggplot` object with Mahalanobis distances color-coded (if `show.mahal = TRUE`)
#'  - A `ggplot` object combining both outlier highlighting and Mahalanobis distance color-coding
#'   (if both `show.outlier` and `show.mahal` are `TRUE`)
#'  - A tibble containing standardized scores, outlier flags, and robust multivariate Mahalanobis distances
#'   (if both `show.outlier` and `show.mahal` are `FALSE`)
#'
#' @export outlierplot
#'
#' @examples
#' # Load the glass dataset from the chemometrics package
#' data(glass, package = "chemometrics")
#'
#' # Basic usage with default parameters
#' outlierplot(glass)
#'
#' # Adjust the proportion of observations used for MCD estimation
#' outlierplot(glass, quan = 0.75)
#'
#' # Show Mahalanobis distances instead of outlier highlighting
#' outlierplot(glass, show.outlier = FALSE, show.mahal = TRUE)
#'
#' # Combine outlier highlighting and Mahalanobis distance color-coding
#' outlierplot(glass, show.outlier = TRUE, show.mahal = TRUE)
#'
#' # Return data frame instead of plot
#' result_df <- outlierplot(glass, show.outlier = FALSE, show.mahal = FALSE)
#' head(result_df)
#'
outlierplot <- function(x, quan = 1/2, alpha = 0.025, show.outlier = TRUE, show.mahal = FALSE) {
  if (!is.matrix(x) && !is.data.frame(x) && !tibble::is_tibble(x)) {
    stop("'x' must be matrix or data.frame")
  }
  if (ncol(x) < 2) {
    stop("'x' must be at least two-dimensional")
  }
  if (!is.numeric(quan) || quan < 0.5 || quan > 1) {
    stop("'quan' must be a numeric value between 0.5 and 1")
  }
  if (!is.logical(show.outlier)) {
    stop("'show.outlier' must be of type boolean (TRUE or FALSE)")
  }
  if (!is.logical(show.mahal)) {
    stop("'show.mahal' must be of type boolean (TRUE or FALSE)")
  }

  if (is.data.frame(x) || tibble::is_tibble(x)) {
    x <- as.matrix(x)
  }

  rob <- robustbase::covMcd(x, alpha = quan)
  xarw <- covARW(x, rob$center, rob$cov, alpha = alpha)

  if (xarw$cn != Inf) {
    alpha <- sqrt(c(xarw$cn, stats::qchisq(c(0.75, 0.5, 0.25), ncol(x))))
  } else {
      alpha <- sqrt(stats::qchisq(c(0.975, 0.75, 0.5, 0.25), ncol(x)))
  }

  dist <- stats::mahalanobis(x, center = rob$center, cov = rob$cov)
  sx <- matrix(NA, nrow = nrow(x), ncol = ncol(x))

  for (i in 1:ncol(x)) {
    sx[,i] <- (x[,i] - xarw$m[i]) / sqrt(xarw$c[i,i])
    }
  r <- range(sx)
  out <- sqrt(dist) > min(sqrt(xarw$cn), sqrt(stats::qchisq(0.975, dim(x)[2])))

  s_df <- as.data.frame(sx)
  colnames(s_df) <- colnames(x)

  set.seed(123)
  s_df <- s_df %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      outlier = out,
      mahalanobis = sqrt(dist),
      x = runif(nrow(x), min = -1, max = 1)
    )

  df_long <- s_df %>%
    tidyr::pivot_longer(
      cols = !c(outlier, mahalanobis, x),
      names_to = "variable",
      values_to = "score"
      )

  p <- ggplot2::ggplot(df_long) +
    ggplot2::aes(x = x, y = score) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.1, linetype = "dotdash") +
    ggplot2::ylim(r[1], r[2]) +
    ggplot2::facet_grid(cols = ggplot2::vars(variable), scales = "fixed", space = "fixed") +
    ggplot2::theme_light() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(size = 5),
      panel.grid = ggplot2::element_blank(),
      legend.position = "bottom"
      )

  p_outl <- p +
    ggplot2::geom_point(ggplot2::aes(color = outlier, shape = outlier)) +
    ggplot2::scale_shape(solid = TRUE) +
    ggplot2::scale_color_manual(values = c("#efb61f", "#011f4b"))

  p_mahal <- p +
    ggplot2::geom_point(ggplot2::aes(color = mahalanobis)) +
    ggplot2::scale_color_gradient2(
      limits = c(round(r[1], 0), round(r[2], 0)),
      breaks = c(round(r[1], 0), 0, round(r[2], 0)),
      low = "red",
      mid = "green",
      high = "blue",
      midpoint = 0) +
    ggplot2::guides(color = ggplot2::guide_colorbar()) +
    ggplot2::labs(color = "Robust\nMahalanobis")

  p_all <- p +
    ggplot2::geom_point(ggplot2::aes(color = mahalanobis, shape = outlier)) +
    ggplot2::scale_color_viridis_c(option = "plasma") +
    ggplot2::scale_shape_manual(values = c("FALSE" = 1, "TRUE" = 17)) +
    ggplot2::labs(color = "Robust\nMahalanobis", shape = "Outlier")

  if (show.outlier == TRUE && show.mahal == FALSE) {
    return(p_outl)
  }

  if (show.outlier == FALSE && show.mahal == TRUE) {
    return(p_mahal)
  }

  if (show.outlier == TRUE && show.mahal == TRUE) {
    return(p_all)
  }

  if (show.outlier == FALSE && show.mahal == FALSE) {
    return(s_df %>% dplyr::select(-x))
  }

}


covARW <- function(x, m0, c0, alpha, pcrit){
  n <- nrow(x)
  p <- ncol(x)

  if (missing(pcrit)){
    if (p <= 10) pcrit <- (0.24 - 0.003 * p) / sqrt(n)
    if (p > 10) pcrit <- (0.252 - 0.0018 * p) / sqrt(n)
  }

  if (missing(alpha)) {
    delta <- stats::qchisq(0.975, p)
    } else {
    delta <- stats::qchisq(1 - alpha, p)
    }

  d2 <- stats::mahalanobis(x, m0, c0)
  d2ord <- sort(d2)
  dif <- stats::pchisq(d2ord, p) - (0.5:n) / n
  i <- (d2ord >= delta) & (dif > 0)

  if (sum(i) == 0) {
    alfan <- 0
    } else {
      alfan <- max(dif[i])
      }

  if (alfan < pcrit) {
    alfan <- 0
    }
  if (alfan > 0) {
    cn <- max(d2ord[n - ceiling(n * alfan)], delta)
    } else {
      cn <- Inf
      }

  w <- d2 < cn

  if(sum(w) == 0) {
    m <- m0
    c <- c0
    } else {
    m <- apply(x[w,], 2, mean)
    c1 <- as.matrix(x - tcrossprod(rep(1, n), m))
    c <- crossprod(c1 * w, c1) / sum(w)
    }

  list(
    m = m,
    c = c,
    cn = cn,
    w = w
    )
}
