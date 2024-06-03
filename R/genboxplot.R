#' @title Generalized Boxplot for Skewed and Heavy-Tailed Distributions
#'
#' @author Christian L. Goueguel
#'
#' @description
#' This function computes the Generalized Boxplot, a robust data visualization technique
#' for detecting outliers and skewed and heavy-tailed distribution, as proposed by
#' Bruffaerts *et al*. (2014). It uses the Tukey g-and-h distribution to model the data.
#'
#' @param data A numeric data frame or tibble.
#' @param alpha A scalar, between 0 and 1 that specifies the desired detection rate of atypical values.
#' @param p A scalar, between 0.5 and 1 that specifies the quantile order for estimating g and h.
#' @param plot Logical value indicating whether to plot the boxplot or return the boxplot statistics.
#' @return If `plot` is `FALSE`, returns a list containing the following components:
#'   \describe{
#'     \item{lower.fence}{The lower fence value.}
#'     \item{lower.quartile}{The lower quartile value.}
#'     \item{median}{The median value.}
#'     \item{upper.quartile}{The upper quartile value.}
#'     \item{upper.fence}{The upper fence value.}
#'     \item{lower.outliers}{A vector containing the lower outliers.}
#'     \item{upper.outliers}{A vector containing the upper outliers.}
#'   }
#'   If `plot` is `TRUE`, returns a `ggplot` object representing the boxplot.
#'
#' @references
#'  - Bruffaerts, C., Verardi, V., Vermandele, C. (2014). A generalized boxplot for
#'    skewed and heavy-tailed distributions. Statistics and Probability Letters 95(C):110–117
#'
#' @export genboxplot
genboxplot <- function(data, alpha = 0.05, p = 0.9, plot = FALSE) {
  if (missing(data)) {
    stop("Missing 'data' argument.")
  }
  if (!is.numeric(alpha) || alpha < 0 || alpha > 1) {
    stop("Argument 'alpha' must be a numeric value between 0 and 1.")
  }
  if (!is.numeric(p) || p < 0.5 || p > 1) {
    stop("Argument 'p' must be a numeric value between 0.5 and 1.")
  }

  genboxStats <- function(x) {
    n <- length(x)
    order_x <- sort(x)

    # Standardize the data
    x_star <- (x - stats::median(x)) / stats::IQR(x)

    # Shift the data to positive values
    r <- x_star - min(x_star) + 0.1

    # Standardize to (0, 1) interval
    r_tilde <- r / (min(r) + max(r))

    # Inverse normal transformation
    w <- stats::qnorm((rank(r_tilde, na.last = "keep") - 0.5) / sum(!is.na(r_tilde)))

    # Standardize w
    w_star <- (w - stats::median(w)) / (stats::IQR(w) / 1.3426)

    # Estimate g and h parameters
    A <- stats::quantile(w_star, p) / stats::quantile(w_star, 1 - p)
    B <- (stats::quantile(w_star, p) * stats::quantile(w_star, 1 - p)) / (stats::quantile(w_star, p) + stats::quantile(w_star, 1 - p))
    z <- stats::qnorm(p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
    g <- (1 / z) * log(-A)
    h <- (2 / z^2) * log(-g * B)

    # Calculate quantiles of the Tukey g-and-h distribution
    xi_alpha_2 <- gk::qgh(alpha / 2, A = stats::median(w_star), B = stats::mad(w_star), g = g, h = h)
    xi_1_alpha_2 <- gk::qgh(1 - alpha / 2, A = stats::median(w_star), B = stats::mad(w_star), g = g, h = h)

    # Identify atypical observations
    atypical_indices <- which(w_star < xi_alpha_2 | w_star > xi_1_alpha_2)

    # Calculate fences and hinges
    L_star_minus <- xi_alpha_2
    L_star_plus <- xi_1_alpha_2
    C_low <- stats::median(w) + (stats::IQR(w) / 1.3426) * L_star_minus
    C_up <- stats::median(w) + (stats::IQR(w) / 1.3426) * L_star_plus
    f_low <- stats::pnorm(C_low, mean = 0, sd = 1) * (min(r) + max(r)) + min(x_star) - 0.1
    f_up <- stats::pnorm(C_up, mean = 0, sd = 1) * (min(r) + max(r)) + min(x_star) - 0.1

    # Calculate quartiles
    q1 <- stats::quantile(x, 0.25)
    q3 <- stats::quantile(x, 0.75)

    # Construct the boxplot data structure
    boxplot_data <- list(
      lower.fence = f_low * stats::IQR(x) + stats::median(x),
      lower.quartile = q1,
      median = stats::median(x),
      upper.quartile = q3,
      upper.fence = f_up * stats::IQR(x) + stats::median(x),
      lower.outliers = order_x[order_x < f_low * stats::IQR(x) + stats::median(x)],
      upper.outliers = order_x[order_x > f_up * stats::IQR(x) + stats::median(x)]
    )

    return(boxplot_data)
  }

  if (plot) {
    boxplot_stats <- data %>%
      purrr::map_dfr(
        function(.x) {
          gen_box <- genboxStats(.x)
          tibble::tibble(
            variable = deparse(substitute(.x)),
            lower = gen_box$lower.fence,
            q1 = gen_box$lower.quartile,
            median = gen_box$median,
            q3 = gen_box$upper.quartile,
            upper = gen_box$upper.fence
          )
        },
        .id = "variable"
      ) %>%
      dplyr::mutate(variable = forcats::as_factor(variable))

    outlier_tbl <- boxplot_stats %>%
      tidyr::pivot_longer(cols = c("lower", "q1", "median", "q3", "upper"), names_to = "value_type", values_to = "value") %>%
      dplyr::filter(value_type %in% c("lower", "upper")) %>%
      dplyr::group_by(variable, value_type) %>%
      dplyr::mutate(outlier_value = list(data[[as.character(variable)]][data[[as.character(variable)]] < value | data[[as.character(variable)]] > value])) %>%
      dplyr::unnest(outlier_value) %>%
      dplyr::select(variable, value, outlier_value)

    ggplot2::ggplot() +
      ggplot2::geom_boxplot(
        data = boxplot_stats,
        ggplot2::aes(
          x = variable,
          ymin = lower,
          lower = q1,
          middle = median,
          upper = q3,
          ymax = upper,
          group = variable,
          fill = variable
        ),
        stat = "identity",
        width = 0.5, # Adjust the width of the boxplots
        colour = "black",
        outlier.colour = NA,
        outlier.shape = NA,
        notch = FALSE, # Set to TRUE if you want notched boxplots
        notchwidth = 0.5, # Adjust the notch width
        staplewidth = 0.5 # Adjust the staple width
      ) +
      ggplot2::geom_point(
        data = outlier_tbl,
        ggplot2::aes(
          x = variable,
          y = outlier_value,
          fill = variable,
          group = variable
        ),
        shape = 21,
        size = 2,
        alpha = 1/3
      ) +
      ggplot2::geom_jitter(size = 1.5) +
      ggsci::scale_fill_d3(palette = "category20") +
      ggplot2::theme_bw() +
      ggplot2::theme(
        legend.position = "none",
        panel.grid = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1)
      ) +
      ggplot2::labs(x = " ", y = " ")
  }
}


