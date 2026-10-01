#' @title Hotelling's T-squared and Q Residuals of a PCA Model
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Computes, for each sample, the two distances used to monitor a principal
#' component analysis (PCA) model: Hotelling's \eqn{T^2}, the distance
#' within the model plane of the first `k` components, and the Q residual
#' (squared prediction error, SPE), the squared distance to that plane, with
#' their limits at one or more confidence levels.
#'
#' @details
#' With scores \eqn{t_{ia}} and eigenvalues \eqn{\lambda_a},
#' \deqn{T^2_i = \sum_{a=1}^{k} \frac{t_{ia}^2}{\lambda_a}, \qquad
#'   Q_i = \lVert x_i - \hat{x}_i \rVert^2}
#' where \eqn{\hat{x}_i} is the reconstruction of the (centered and scaled)
#' sample from the `k` components. A high \eqn{T^2} is an extreme but
#' well-modeled sample (for example, a high concentration); a high Q is a
#' sample the model does not describe (another matrix, a contamination, an
#' instrumental problem).
#'
#' For the samples of the model, \eqn{T^2} and its limits come from
#' [hotelling_t2()] (F or Beta distribution, `t2_method`). For new samples,
#' the limit is that of a new observation,
#' \eqn{k(n+1)(n-1)/(n(n-k))\,F(k, n-k)}. The limit of Q is that of Jackson
#' and Mudholkar (1979), from the eigenvalues of the components left out, or
#' Box's (1954) scaled chi-square approximation (`method = "box"`); both
#' tend to be slightly conservative. Samples are classified at the highest
#' confidence level.
#'
#' These are classical estimates, themselves affected by outliers: see
#' [robpca()] and [plot_outlier_map()] for robust score and orthogonal
#' distances. [dmodx()] gives the residual distance in SIMCA's form.
#'
#' @param model A [stats::prcomp()] fit that kept all its components (the
#'   default of `prcomp()`), or a numeric matrix or data frame, on which a
#'   PCA is fitted with `center` and `scale`.
#' @param k The number of components of the model.
#' @param newdata Optional new samples (a matrix or data frame with the
#'   variables of the model), whose distances are computed instead of those
#'   of the calibration samples.
#' @param conf_level The confidence level(s) of the limits: one or more
#'   values between 0 and 1. Default is 0.975.
#' @param method The limit of Q: `"jackson"` (default, Jackson-Mudholkar) or
#'   `"box"`.
#' @param t2_method The distribution of the \eqn{T^2} limit of the samples
#'   of the model: `"f"` (default) or `"beta"` (see [hotelling_t2()]).
#' @param center,scale Passed to [stats::prcomp()] when `model` is data.
#'   Default is `TRUE` and `FALSE`.
#'
#' @return A tibble of class `specproc_influence`, with one row per sample:
#'   `sample` (row number), `t2`, its limits at each confidence level (in %,
#'   e.g. `t2_limit_97.5`), `q`, its limits (`q_limit_97.5`, ...) and `outlier`,
#'   the type of the sample at the highest confidence level: `"regular"`,
#'   `"extreme"` (high \eqn{T^2} only), `"residual"` (high Q only) or
#'   `"both"`. Draw it with [plot_influence()].
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
#' @seealso [dmodx()], [plot_influence()], [hotelling_t2()], [robpca()]
#' @export q_residuals
#'
#' @examples
#' if (rlang::is_installed("HotellingEllipse", version = "1.3.0")) {
#'   data(forageLIBS)
#'   spectra <- forageLIBS[-(1:14)]
#'   pca <- stats::prcomp(spectra)
#'   influence <- q_residuals(pca, k = 3)
#'   influence[influence$outlier != "regular", ]
#'   plot_influence(influence, label = forageLIBS$Measurement)
#'   # several limits
#'   plot_influence(q_residuals(pca, k = 3, conf_level = c(0.95, 0.99)))
#' }
q_residuals <- function(model, k, newdata = NULL, conf_level = 0.975, method = "jackson",
                        t2_method = "f", center = TRUE, scale = FALSE) {
  method <- match.arg(method, c("jackson", "box"))
  parts <- pca_parts(model, k, newdata, conf_level, t2_method, center, scale)
  q <- parts$sq_residuals
  rest <- parts$lambda[-seq_len(parts$k)]
  q_limits <- stats::setNames(vapply(parts$conf_level, function(l) q_limit(rest, l, method),
                                     numeric(1)), parts$labels)
  influence_table(parts, "q", q, q_limits)
}

#' @title Distance to the Model (DModX) of a PCA Model
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Computes the distance of each sample to a principal component analysis
#' (PCA) model in the space of the variables (DModX, as in SIMCA): the
#' residual standard deviation of the sample, normalized by that of the
#' calibration samples, with its limits at one or more confidence levels,
#' and Hotelling's \eqn{T^2} for the influence plot.
#'
#' @details
#' With the residuals \eqn{e_{ij}} of the \eqn{K} variables after `k`
#' components, for \eqn{N} calibration samples (and \eqn{A_0 = 1} if the
#' data are centered),
#' \deqn{s_i = \sqrt{\frac{\sum_j e_{ij}^2}{K - k}}\,\sqrt{\frac{N}{N - k -
#'   A_0}}, \qquad s_0 = \sqrt{\frac{\sum_i \sum_j e_{ij}^2}{(N - k -
#'   A_0)(K - k)}}}
#' (the correction factor \eqn{\sqrt{N / (N - k - A_0)}} applies to the
#' calibration samples only). DModX is \eqn{s_i / s_0} (normalized, default)
#' or \eqn{s_i}. Its squared ratio follows an F distribution with
#' \eqn{\nu} and \eqn{(N - k - A_0)\nu} degrees of freedom, so the limit of
#' the normalized DModX is \eqn{\sqrt{F_{1-\alpha}}} (times \eqn{s_0} for the
#' absolute DModX).
#'
#' SIMCA takes \eqn{\nu = K - k} (`df = "simca"`), as if the residuals of the
#' variables were independent. For spectra, with far more (correlated)
#' channels than samples, this gives a limit close to 1 that flags a large
#' share of ordinary samples. With `df = "effective"` (default), \eqn{\nu} is
#' the effective number of residual dimensions,
#' \eqn{\theta_1^2 / \theta_2} from the eigenvalues \eqn{\lambda_a} of the
#' components left out (\eqn{\theta_j = \sum_{a > k} \lambda_a^j}, as in
#' Box's approximation of Q), which is \eqn{K - k} when the residuals are
#' independent with equal variances. DModX is then consistent with the Q
#' residuals of [q_residuals()], of which it is a scaled square root.
#'
#' @inheritParams q_residuals
#' @param normalized A logical: DModX relative to the residual standard
#'   deviation of the calibration samples (`TRUE`, default), or absolute.
#' @param df The degrees of freedom of the limit: `"effective"` (default) or
#'   `"simca"` (see details).
#'
#' @return A tibble of class `specproc_influence`, with one row per sample:
#'   `sample`, `t2` and its limits (as for [q_residuals()]), `dmodx`, its
#'   limits at each confidence level (`dmodx_limit_97.5`, ...), and `outlier`,
#'   the type of the sample at the highest confidence level. Draw it with
#'   [plot_influence()].
#'
#' @references
#'  - Wold, S., Sjöström, M. (1977). SIMCA: a method for analyzing chemical
#'    data in terms of similarity and analogy. In Kowalski, B.R. (ed.),
#'    Chemometrics: Theory and Application, ACS Symposium Series 52,
#'    American Chemical Society, Washington, pp. 243-282.
#'  - Eriksson, L., Johansson, E., Kettaneh-Wold, N., Trygg, J., Wikström,
#'    C., Wold, S. (2006). Multi- and Megavariate Data Analysis, Part I, 2nd
#'    ed. Umetrics Academy, Umeå.
#'  - Box, G.E.P. (1954). Some theorems on quadratic forms applied in the
#'    study of analysis of variance problems, I. Annals of Mathematical
#'    Statistics, 25(2):290-302.
#'
#' @seealso [q_residuals()], [plot_influence()], [hotelling_t2()]
#' @export dmodx
#'
#' @examples
#' if (rlang::is_installed("HotellingEllipse", version = "1.3.0")) {
#'   data(forageLIBS)
#'   spectra <- forageLIBS[-(1:14)]
#'   pca <- stats::prcomp(spectra)
#'   d <- dmodx(pca, k = 3)
#'   d[d$outlier != "regular", ]
#'   plot_influence(d, label = forageLIBS$Measurement)
#' }
dmodx <- function(model, k, newdata = NULL, conf_level = 0.975, normalized = TRUE,
                  df = "effective", t2_method = "f", center = TRUE, scale = FALSE) {
  check_flag(normalized, "normalized")
  df <- match.arg(df, c("effective", "simca"))
  parts <- pca_parts(model, k, newdata, conf_level, t2_method, center, scale)
  a <- parts$k
  n <- parts$n
  n_var <- parts$n_var
  a0 <- if (parts$centered) 1 else 0
  if (n - a - a0 < 1 || n_var <= a) {
    stop("Too few samples or variables for DModX with ", a, " components.", call. = FALSE)
  }
  s0 <- sqrt(parts$calibration_ss / ((n - a - a0) * (n_var - a)))
  correction <- if (parts$new) 1 else sqrt(n / (n - a - a0))
  s <- sqrt(parts$sq_residuals / (n_var - a)) * correction
  rest <- parts$lambda[-seq_len(a)]
  rest <- rest[rest > 0]
  nu <- if (df == "simca") n_var - a else sum(rest)^2 / sum(rest^2)
  crit <- sqrt(stats::qf(parts$conf_level, nu, (n - a - a0) * nu))
  value <- if (normalized) s / s0 else s
  limits <- stats::setNames(if (normalized) crit else crit * s0, parts$labels)
  out <- influence_table(parts, "dmodx", value, limits)
  attr(out, "normalized") <- normalized
  attr(out, "df") <- df
  out
}

#' @title Influence Plot of a PCA Model
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Plots the residual distance of each sample to a PCA model (Q residual or
#' DModX) against its Hotelling's \eqn{T^2}, with their limits at each
#' confidence level, from [q_residuals()] or [dmodx()]. It is drawn like the
#' outlier map of a robust PCA, [plot_outlier_map()].
#'
#' @details
#' The limits at the highest confidence level (dashed lines) divide the plot
#' into four types of samples:
#'  - **regular** samples (bottom left);
#'  - **extreme** samples (bottom right): a high \eqn{T^2} but well
#'    described by the model, for example a high concentration;
#'  - **residual** samples (top left): not described by the model (another
#'    matrix, a contamination, an instrumental problem);
#'  - samples beyond **both** limits, the most harmful.
#'
#' The limits at lower confidence levels are dotted, dot-dashed, ... lines.
#'
#' With `relative = TRUE`, each distance is divided by its limit at the
#' highest confidence level, so both limits are at 1 whatever the model,
#' which puts plots of different models on the same scale.
#'
#' @param x The result of [q_residuals()] or [dmodx()].
#' @param label The names of the samples: a vector with one value per
#'   sample. By default, their row numbers.
#' @param labels The number of most outlying samples to label (beyond a
#'   limit at the highest confidence level). Default is 3; use 0 for no
#'   labels.
#' @param relative If `TRUE`, plot the distances divided by their limits at
#'   the highest confidence level. Default is `FALSE`.
#' @param shade If `TRUE`, shade the three outlying regions in grey, darker
#'   for more harmful samples (extreme, residual, both), and name them in
#'   their corners. Default is `FALSE`.
#' @param log If `TRUE`, use logarithmic axes, which spread out the regular
#'   samples when a few are far away. Zero distances are drawn at the
#'   smallest positive distance. Default is `FALSE`.
#' @param colour_by The colors of the points: `"type"` (default), by type,
#'   or `"distance"`, by their distance relative to the limits,
#'   \eqn{\max(T^2/T^2_{lim}, Q/Q_{lim})}, on a rainbow scale from dark red to
#'   blue, as in [plot_outlier_map()].
#' @param title The plot title.
#' @param ... Further arguments passed to [ggplot2::geom_point()] to style
#'   the points, such as `alpha` (default 0.85), `size` (2.2), `stroke`
#'   (0.4) or `colour` (the outline, `"black"`). `size` can also be a numeric
#'   vector with one value per sample, such as the concentration of an
#'   element, to vary the size of the points, with a legend.
#'
#' @return A ggplot object.
#' @seealso [q_residuals()], [dmodx()], [plot_outlier_map()]
#' @export plot_influence
plot_influence <- function(x, label = NULL, labels = 3, relative = FALSE, shade = FALSE,
                           log = FALSE, colour_by = c("type", "distance"), title = NULL, ...) {
  if (!inherits(x, "specproc_influence")) {
    stop("'x' must be returned by q_residuals() or dmodx().", call. = FALSE)
  }
  if (!is.null(label) && length(label) != nrow(x)) {
    stop("'label' must have one value per sample (", nrow(x), ").", call. = FALSE)
  }
  check_count(labels, "labels", lower = 0)
  colour_by <- match.arg(colour_by)
  check_map_flags(relative, shade, log)
  distance <- attr(x, "distance") %||% "q"
  levels <- attr(x, "conf_labels")
  new <- isTRUE(attr(x, "new"))
  df <- data.frame(
    id = as.character(if (is.null(label)) x$sample else label), x = x$t2, y = x[[distance]],
    type = x$outlier, set = if (new) "new" else "calibration", stringsAsFactors = FALSE
  )
  cuts <- data.frame(
    level = paste0(levels, "%"),
    x = vapply(levels, function(l) x[[paste0("t2_limit_", l)]][1], numeric(1)),
    y = vapply(levels, function(l) x[[paste0(distance, "_limit_", l)]][1], numeric(1))
  )
  if (is.null(title)) {
    title <- sprintf("Influence plot (%d components)%s", attr(x, "k"), if (new) ", new samples" else "")
  }
  y_lab <- if (distance == "q") "Q residual (SPE)" else if (isTRUE(attr(x, "normalized"))) {
    "DModX (normalized)"
  } else {
    "DModX"
  }
  distance_map(
    df, cuts = cuts, labels = labels, relative = relative, shade = shade, log = log,
    colour_by = colour_by,
    palette = c(regular = "grey55", extreme = "#1b9e77", residual = "#d95f02", both = "#e7298a"),
    regions = c("extreme", "residual", "both"),
    x_lab = list(expression("Hotelling's" ~ T^2), expression("Reduced Hotelling's" ~ T^2)),
    y_lab = c(y_lab, paste("Reduced", y_lab)),
    title = title, point = map_point_args(rlang::enquos(...), nrow(df)), show_set = FALSE
  )
}

# ---- internals ---------------------------------------------------------------

# T-squared and squared residuals of the calibration samples or of new ones.
# The model keeps all its components, so the squared residual of a
# calibration sample is the sum of its squared scores on the components left
# out: the data need not be reconstructed.
pca_parts <- function(model, k, newdata, conf_level, t2_method, center, scale) {
  conf_level <- check_conf_level(conf_level)
  t2_method <- match.arg(t2_method, c("f", "beta"))
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
  calibration_sq <- rowSums(model$x[, -seq_len(k), drop = FALSE]^2)
  labels <- conf_label(conf_level)
  if (is.null(newdata)) {
    sq_residuals <- calibration_sq
    t2 <- hotelling_t2(as.data.frame(model$x[, seq_len(k), drop = FALSE]),
                       columns = colnames(model$x)[seq_len(k)], conf_level = conf_level,
                       method = t2_method)
    t2_values <- t2$t2
    t2_limits <- vapply(labels, function(l) t2[[paste0("limit_", l)]][1], numeric(1))
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
    scores <- x %*% p
    t2_values <- unname(rowSums(sweep(scores^2, 2, lambda[seq_len(k)], "/")))
    t2_limits <- vapply(conf_level, function(l) t2_limit(l, k, n, "new"), numeric(1))
    sq_residuals <- rowSums((x - scores %*% t(p))^2)
  }
  list(k = k, n = n, n_var = nrow(model$rotation), lambda = lambda,
       sq_residuals = unname(sq_residuals), calibration_ss = sum(calibration_sq), t2 = t2_values,
       t2_limits = stats::setNames(unname(t2_limits), labels), conf_level = conf_level,
       labels = labels, new = !is.null(newdata), centered = !isFALSE(model$center))
}

# The tibble of an influence analysis, classified at the highest level.
influence_table <- function(parts, distance, value, limits) {
  out <- tibble::tibble(sample = seq_along(value), t2 = parts$t2)
  for (l in parts$labels) out[[paste0("t2_limit_", l)]] <- parts$t2_limits[[l]]
  out[[distance]] <- unname(value)
  for (l in parts$labels) out[[paste0(distance, "_limit_", l)]] <- unname(limits[[l]])
  top <- parts$labels[length(parts$labels)]
  high_t2 <- out$t2 > parts$t2_limits[[top]]
  high_d <- out[[distance]] > limits[[top]]
  out$outlier <- factor(ifelse(high_t2 & high_d, "both", ifelse(high_t2, "extreme",
                                                                 ifelse(high_d, "residual", "regular"))),
                        levels = c("regular", "extreme", "residual", "both"))
  attr(out, "k") <- parts$k
  attr(out, "new") <- parts$new
  attr(out, "conf_level") <- parts$conf_level
  attr(out, "conf_labels") <- parts$labels
  attr(out, "distance") <- distance
  class(out) <- c("specproc_influence", class(out))
  out
}

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
