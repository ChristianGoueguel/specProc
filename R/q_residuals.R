#' @title Hotelling's T-squared and Q Residuals of a PCA Model
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Computes, for each sample, the two distances used to monitor a principal
#' component analysis (PCA) model: Hotelling's \eqn{T^2}, the distance
#' within the model plane of the first `k` components, and the Q residual
#' (squared prediction error, SPE), the squared distance to that plane, with
#' their 95% and 99% limits.
#'
#' @details
#' With scores \eqn{t_{ia}} and eigenvalues \eqn{\lambda_a},
#' \deqn{T^2_i = \sum_{a=1}^{k} \frac{t_{ia}^2}{\lambda_a}, \qquad
#'   Q_i = \lVert x_i - \hat{x}_i \rVert^2}
#' where \eqn{\hat{x}_i} is the reconstruction of the (centered and scaled)
#' sample from the `k` components. A high \eqn{T^2} is an extreme but
#' well-modeled sample (for example, a high concentration); a high Q is a
#' sample the model does not describe (another matrix, a contamination, an
#' instrumental problem). The limit of \eqn{T^2} is
#' \eqn{k(n-1)/(n-k)\,F(k, n-k)} for the calibration samples and
#' \eqn{k(n+1)(n-1)/(n(n-k))\,F(k, n-k)} for new samples. The limit of Q
#' is that of Jackson and Mudholkar (1979), from the eigenvalues of the
#' components left out, or Box's (1954) scaled chi-square approximation
#' (`method = "box"`). Both approximate the distribution of Q from the
#' eigenvalues of the calibration samples, and tend to be slightly
#' conservative (fewer false alarms than the nominal level).
#'
#' These are classical estimates, themselves affected by outliers: see
#' [robpca()] and [plot_outlier_map()] for robust score and orthogonal
#' distances.
#'
#' @param model A [stats::prcomp()] fit that kept all its components (the
#'   default of `prcomp()`), or a numeric matrix or data frame, on which a
#'   PCA is fitted with `center` and `scale`.
#' @param k The number of components of the model.
#' @param newdata Optional new samples (a matrix or data frame with the
#'   variables of the model), whose distances are computed instead of those
#'   of the calibration samples.
#' @param method The limit of Q: `"jackson"` (default, Jackson-Mudholkar) or
#'   `"box"`.
#' @param center,scale Passed to [stats::prcomp()] when `model` is data.
#'   Default is `TRUE` and `FALSE`.
#'
#' @return A tibble of class `specproc_influence`, with one row per sample:
#'   `sample` (row number), `t2`, `q`, their limits (`t2_limit_95`,
#'   `t2_limit_99`, `q_limit_95`, `q_limit_99`) and `outlier`, the type of
#'   the sample at the 99% limits: `"regular"`, `"extreme"` (high \eqn{T^2}
#'   only), `"residual"` (high Q only) or `"both"`. Draw it with
#'   [plot_influence()].
#'
#' @references
#'  - Jackson, J.E., Mudholkar, G.S. (1979). Control procedures for
#'    residuals associated with principal component analysis.
#'    Technometrics, 21(3):341-349.
#'  - Box, G.E.P. (1954). Some theorems on quadratic forms applied in the
#'    study of analysis of variance problems, I. Annals of Mathematical
#'    Statistics, 25(2):290-302.
#'  - Nomikos, P., MacGregor, J.F. (1995). Multivariate SPC charts for
#'    monitoring batch processes. Technometrics, 37(1):41-59.
#'
#' @seealso [plot_influence()], [hotelling_t2()], [robpca()]
#' @export q_residuals
#'
#' @examples
#' data(soilLIBS)
#' spectra <- average(soilLIBS[-(2:8)], Sample)
#' pca <- stats::prcomp(spectra[-1], scale. = TRUE)
#' influence <- q_residuals(pca, k = 3)
#' influence[influence$outlier != "regular", ]
#' plot_influence(influence, label = spectra$Sample)
q_residuals <- function(model, k, newdata = NULL, method = "jackson", center = TRUE,
                        scale = FALSE) {
  method <- match.arg(method, c("jackson", "box"))
  if (!inherits(model, "prcomp")) {
    model <- stats::prcomp(as_numeric_matrix(model, "model"), center = center, scale. = scale)
  }
  rank <- ncol(model$x)
  if (length(model$sdev) > rank) {
    stop("'model' must keep all its components (do not set `rank.` in prcomp()).", call. = FALSE)
  }
  check_count(k, "k")
  if (k >= rank) {
    stop("'k' must be smaller than the number of components (", rank, ").", call. = FALSE)
  }
  n <- nrow(model$x)
  lambda <- model$sdev^2
  p <- model$rotation[, seq_len(k), drop = FALSE]
  if (is.null(newdata)) {
    x <- model$x %*% t(model$rotation)
  } else {
    x <- as_numeric_matrix(newdata, "newdata")
    if (!is.null(rownames(model$rotation)) && !is.null(colnames(x))) {
      missing_cols <- setdiff(rownames(model$rotation), colnames(x))
      if (length(missing_cols) > 0) {
        stop("'newdata' lacks ", length(missing_cols), " variable(s) of the model, e.g. `",
             missing_cols[1], "`.", call. = FALSE)
      }
      x <- x[, rownames(model$rotation), drop = FALSE]
    }
    x <- scale(x, center = if (isFALSE(model$center)) FALSE else model$center,
               scale = if (isFALSE(model$scale)) FALSE else model$scale)
  }
  scores <- x %*% p
  t2 <- rowSums(sweep(scores^2, 2, lambda[seq_len(k)], "/"))
  q <- rowSums((x - scores %*% t(p))^2)

  t2_factor <- if (is.null(newdata)) k * (n - 1) / (n - k) else k * (n + 1) * (n - 1) / (n * (n - k))
  t2_lim <- function(level) t2_factor * stats::qf(level, k, n - k)
  rest <- lambda[-seq_len(k)]
  q_lim <- function(level) q_limit(rest, level, method)
  out <- tibble::tibble(
    sample = seq_len(nrow(x)), t2 = unname(t2), q = unname(q),
    t2_limit_95 = t2_lim(0.95), t2_limit_99 = t2_lim(0.99),
    q_limit_95 = q_lim(0.95), q_limit_99 = q_lim(0.99)
  )
  high_t2 <- out$t2 > out$t2_limit_99
  high_q <- out$q > out$q_limit_99
  out$outlier <- factor(ifelse(high_t2 & high_q, "both", ifelse(high_t2, "extreme",
                                                                 ifelse(high_q, "residual", "regular"))),
                        levels = c("regular", "extreme", "residual", "both"))
  attr(out, "k") <- k
  attr(out, "new") <- !is.null(newdata)
  class(out) <- c("specproc_influence", class(out))
  out
}

#' @title Influence Plot of a PCA Model
#'
#' @description
#' Plots the Q residual of each sample against its Hotelling's \eqn{T^2},
#' with their 95% (dashed) and 99% (solid) limits, from [q_residuals()].
#' The samples beyond a 99% limit are colored by type and labeled.
#'
#' @param x The result of [q_residuals()].
#' @param label The labels of the samples: a vector with one value per
#'   sample. By default, their row numbers.
#' @param log A logical: logarithmic axes (`FALSE`, default), useful when a
#'   few samples are far beyond the limits.
#' @param title The plot title.
#'
#' @return A ggplot object.
#' @seealso [q_residuals()], [plot_outlier_map()]
#' @export plot_influence
plot_influence <- function(x, label = NULL, log = FALSE, title = NULL) {
  if (!inherits(x, "specproc_influence")) {
    stop("'x' must be returned by q_residuals().", call. = FALSE)
  }
  check_flag(log, "log")
  if (!is.null(label) && length(label) != nrow(x)) {
    stop("'label' must have one value per sample (", nrow(x), ").", call. = FALSE)
  }
  df <- as.data.frame(x)
  df$label <- if (is.null(label)) df$sample else label
  flagged <- df[df$outlier != "regular", , drop = FALSE]
  lines <- data.frame(level = c("95%", "99%"), t2 = c(x$t2_limit_95[1], x$t2_limit_99[1]),
                      q = c(x$q_limit_95[1], x$q_limit_99[1]))
  if (is.null(title)) {
    title <- sprintf("Influence plot (%d components)%s", attr(x, "k"),
                     if (isTRUE(attr(x, "new"))) ", new samples" else "")
  }
  colours <- c(regular = "grey45", extreme = "#1f4e79", residual = "#c0392b", both = "#7b3294")
  p <- ggplot2::ggplot(df, ggplot2::aes(.data$t2, .data$q)) +
    ggplot2::geom_vline(data = lines, ggplot2::aes(xintercept = .data$t2, linetype = .data$level),
                        colour = "grey40") +
    ggplot2::geom_hline(data = lines, ggplot2::aes(yintercept = .data$q, linetype = .data$level),
                        colour = "grey40") +
    ggplot2::geom_point(ggplot2::aes(colour = .data$outlier), size = 2, alpha = 0.85) +
    ggplot2::scale_colour_manual(values = colours, drop = TRUE, name = NULL) +
    ggplot2::scale_linetype_manual(values = c(`95%` = "dashed", `99%` = "solid"), name = "Limits")
  if (nrow(flagged) > 0) {
    p <- p + ggplot2::geom_text(data = flagged, ggplot2::aes(label = .data$label, colour = .data$outlier),
                                vjust = -0.9, size = 3, show.legend = FALSE)
  }
  # room for the labels of the samples at the edges
  room <- ggplot2::expansion(mult = c(0.05, 0.12))
  p <- p + if (log) {
    list(ggplot2::scale_x_log10(expand = room), ggplot2::scale_y_log10(expand = room))
  } else {
    list(ggplot2::scale_x_continuous(expand = room), ggplot2::scale_y_continuous(expand = room))
  }
  p +
    ggplot2::labs(x = expression("Hotelling's" ~ T^2), y = "Q residual (SPE)", title = title) +
    ggplot2::theme_bw()
}

# ---- internals ---------------------------------------------------------------

# Upper limit of Q from the eigenvalues of the components left out.
q_limit <- function(rest, level, method) {
  rest <- rest[rest > 0]
  if (length(rest) == 0) return(0)
  theta <- vapply(1:3, function(j) sum(rest^j), numeric(1))
  if (method == "box") {
    g <- theta[2] / theta[1]
    h <- theta[1]^2 / theta[2]
    return(g * stats::qchisq(level, h))
  }
  h0 <- 1 - 2 * theta[1] * theta[3] / (3 * theta[2]^2)
  z <- stats::qnorm(level)
  theta[1] * (z * sqrt(2 * theta[2] * h0^2) / theta[1] + 1 +
                theta[2] * h0 * (h0 - 1) / theta[1]^2)^(1 / h0)
}
