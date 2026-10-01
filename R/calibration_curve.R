#' @title Univariate Calibration Curve
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Fits a univariate calibration curve (signal against concentration), with
#' its figures of merit, limits of detection and quantification, and tests
#' of linearity. The curve predicts the concentration of new samples from
#' their signal, with confidence intervals.
#'
#' @details
#' The signal, such as a line intensity from [line_intensities()] or a line
#' ratio, is modeled as a straight line or a second-degree polynomial of the
#' concentration, fitted by (weighted) least squares. Weights `"1/x"` or
#' `"1/x2"` suit signals whose variance grows with concentration, which is
#' common in LIBS.
#'
#' **Figures of merit.** The sensitivity is the slope of the curve at zero
#' concentration. The limits of detection and quantification are
#' \eqn{LOD = 3.3\,\sigma / S} and \eqn{LOQ = 10\,\sigma / S} (ICH, 2005),
#' where \eqn{S} is the sensitivity and \eqn{\sigma} the standard deviation
#' of the signal of a blank, estimated from
#'  - `"residual"`: the residual standard deviation of the fit (default
#'    without `blank`);
#'  - `"intercept"`: the standard error of the intercept;
#'  - `"blank"`: the standard deviation of replicate `blank` signals
#'    (default when given).
#'
#' With weights, the residual standard deviation depends on the scale of the
#' weights, so prefer `"blank"` or `"intercept"`.
#'
#' **Linearity.** For a straight line, Mandel's test compares its residual
#' variance with that of a quadratic fit; a small p-value means that the
#' curvature is significant (as with self-absorption or detector
#' saturation). When concentrations are replicated, the lack-of-fit test
#' compares the residuals with the pure error of the replicates.
#'
#' **Intervals.** The confidence band of the curve shows where the mean
#' signal lies at each concentration; the prediction band, wider, where a
#' single new measurement is expected to fall, since it adds the noise of
#' the measurement. [plot_calibration()] draws either or both, and
#' [predict()][predict.specproc_calibration] computes them for given
#' concentrations (`type = "signal"`).
#'
#' **Inverse prediction.** The concentration of a sample is the solution of
#' the calibration equation for its signal (for a quadratic curve, the root
#' within or nearest to the calibration range). Its standard error, by the
#' delta method, combines the uncertainty of the curve with that of the new
#' signal, the mean of `replicates` measurements.
#'
#' @param data A data frame with the calibration samples.
#' @param signal,concentration The columns of `data` holding the signal and
#'   the reference concentration, unquoted or as strings.
#' @param model `"linear"` (default) or `"quadratic"`.
#' @param weights `NULL` (default, unweighted), `"1/x"`, `"1/x2"`, or a
#'   numeric vector of weights, one per row of `data`.
#' @param blank An optional numeric vector of replicate signals of a blank
#'   sample.
#' @param level The confidence level of the intervals of the coefficients.
#'   Default is 0.95.
#' @param lod_method The estimate of the blank standard deviation:
#'   `"residual"`, `"intercept"` or `"blank"` (see details). By default,
#'   `"blank"` when `blank` is given, `"residual"` otherwise.
#'
#' @return An object of class `specproc_calibration`, a list with
#'  - `coefficients`: a tibble of the estimates, standard errors and
#'    confidence intervals (`lower`, `upper`) at `level`;
#'  - `figures_of_merit`: a tibble with the number of standards `n`, the
#'    `sensitivity`, `r_squared`, the residual standard deviation `sigma`,
#'    the blank standard deviation used (`sigma_blank`), `lod` and `loq`
#'    (in concentration units);
#'  - `linearity`: a tibble with the statistic, degrees of freedom and
#'    p-value of Mandel's test and of the lack-of-fit test (when they apply);
#'  - `fit`: the `lm` fit, and `data`, the calibration data.
#'
#' Use [predict()][predict.specproc_calibration] for new samples and
#' [plot_calibration()] to draw the curve.
#'
#' @references
#'  - ICH (2005). Validation of analytical procedures: text and methodology
#'    Q2(R1). International Conference on Harmonisation.
#'  - Mandel, J. (1964). The Statistical Analysis of Experimental Data.
#'    Interscience, New York.
#'  - Miller, J.N., Miller, J.C. (2018). Statistics and Chemometrics for
#'    Analytical Chemistry, 7th ed. Pearson, Harlow.
#'
#' @seealso [predict.specproc_calibration()], [plot_calibration()],
#'   [line_intensities()], [nas()]
#' @export calibration_curve
#'
#' @examples
#' # standards with a slightly curved response
#' set.seed(1)
#' standards <- data.frame(concentration = rep(c(0, 0.5, 1, 2, 4, 8), each = 3))
#' standards$intensity <- with(standards, 50 + 1000 * concentration - 15 * concentration^2 +
#'                               rnorm(18, sd = 20))
#' cal <- calibration_curve(standards, intensity, concentration)
#' cal
#' cal2 <- calibration_curve(standards, intensity, concentration, model = "quadratic")
#' predict(cal2, c(1500, 5000))
#' plot_calibration(cal2)
calibration_curve <- function(data, signal, concentration, model = "linear", weights = NULL,
                              blank = NULL, lod_method = NULL, level = 0.95) {
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame.", call. = FALSE)
  }
  signal <- rlang::as_name(rlang::enquo(signal))
  concentration <- rlang::as_name(rlang::enquo(concentration))
  for (col in c(signal, concentration)) {
    if (!col %in% names(data)) stop("Column `", col, "` not found in 'data'.", call. = FALSE)
    if (!is.numeric(data[[col]])) stop("Column `", col, "` must be numeric.", call. = FALSE)
  }
  model <- match.arg(model, c("linear", "quadratic"))
  check_number(level, "level", lower = 0, upper = 1, lower_open = TRUE, upper_open = TRUE)
  if (is.null(lod_method)) lod_method <- if (is.null(blank)) "residual" else "blank"
  lod_method <- match.arg(lod_method, c("residual", "intercept", "blank"))
  if (lod_method == "blank" && (is.null(blank) || !is.numeric(blank) || sum(!is.na(blank)) < 2)) {
    stop("The \"blank\" method needs at least 2 replicate `blank` signals.", call. = FALSE)
  }

  d <- tibble::tibble(concentration = data[[concentration]], signal = data[[signal]])
  w <- calibration_weights(weights, d$concentration)
  keep <- stats::complete.cases(d) & !is.na(w)
  d <- d[keep, , drop = FALSE]
  w <- w[keep]
  p <- if (model == "linear") 2 else 3
  if (length(unique(d$concentration)) < p + 1) {
    stop("A ", model, " calibration needs at least ", p + 1, " distinct concentrations.",
         call. = FALSE)
  }
  formula <- if (model == "linear") signal ~ concentration else signal ~ concentration + I(concentration^2)
  fit <- if (is.null(weights)) stats::lm(formula, data = d) else stats::lm(formula, data = d, weights = w)
  s <- suppressWarnings(summary(fit))
  coefs <- s$coefficients
  sensitivity <- coefs[2, "Estimate"]
  if (!is.finite(sensitivity) || sensitivity == 0) {
    stop("The calibration curve has no slope at zero concentration.", call. = FALSE)
  }
  sigma_blank <- switch(
    lod_method,
    residual = s$sigma,
    intercept = coefs[1, "Std. Error"],
    blank = stats::sd(blank, na.rm = TRUE)
  )

  ci <- suppressWarnings(stats::confint(fit, level = level))
  structure(
    list(
      coefficients = tibble::tibble(term = c("intercept", "slope", "quadratic")[seq_len(p)],
                                    estimate = unname(coefs[, "Estimate"]),
                                    std_error = unname(coefs[, "Std. Error"]),
                                    lower = unname(ci[, 1]), upper = unname(ci[, 2])),
      figures_of_merit = tibble::tibble(
        n = nrow(d), sensitivity = sensitivity, r_squared = s$r.squared, sigma = s$sigma,
        sigma_blank = sigma_blank, lod = 3.3 * sigma_blank / abs(sensitivity),
        loq = 10 * sigma_blank / abs(sensitivity)
      ),
      linearity = calibration_linearity(d, w, fit, model),
      fit = fit, data = d, model = model, weights = weights, lod_method = lod_method,
      level = level,
      names = c(signal = signal, concentration = concentration)
    ),
    class = "specproc_calibration"
  )
}

#' @title Predict from a Calibration Curve
#'
#' @description
#' Computes the concentrations of new samples from their signal, by inverse
#' prediction from a curve fitted with [calibration_curve()], or the
#' expected signal at given concentrations, with confidence or prediction
#' intervals.
#'
#' @details
#' With `type = "concentration"` (default), the interval of each
#' concentration combines the uncertainty of the curve with the noise of the
#' new signal, the mean of `replicates` measurements (see
#' [calibration_curve()]).
#'
#' With `type = "signal"`, `newdata` holds concentrations, and the interval
#' is either the confidence interval of the mean signal (`interval =
#' "confidence"`) or the prediction interval of a new signal, the mean of
#' `replicates` measurements (`interval = "prediction"`, default). For a
#' weighted curve, the noise of a new signal follows the weights at its
#' concentration (numeric weights give new samples a weight of 1).
#'
#' @param object A curve fitted with [calibration_curve()].
#' @param newdata For `type = "concentration"`, the signals of the new
#'   samples: a numeric vector, or a data frame with the signal column used
#'   for the calibration. For `type = "signal"`, their concentrations: a
#'   numeric vector or a data frame with the concentration column.
#' @param replicates The number of replicate measurements averaged in each
#'   signal. Default is 1.
#' @param level The confidence level of the intervals. Default is 0.95.
#' @param type `"concentration"` (default, inverse prediction) or `"signal"`.
#' @param interval For `type = "signal"`: `"prediction"` (default) or
#'   `"confidence"`.
#' @param ... Not used.
#'
#' @return For `type = "concentration"`, a tibble with the `signal`, the
#'   predicted `concentration`, its standard error `se`, the interval
#'   (`lower`, `upper`) and whether it is below the limit of detection
#'   (`below_lod`) or of quantification (`below_loq`). For `type =
#'   "signal"`, a tibble with the `concentration`, the predicted `signal`,
#'   its standard error `se` and the interval (`lower`, `upper`).
#'
#' @seealso [calibration_curve()], [plot_calibration()]
#' @export
#'
#' @examples
#' set.seed(1)
#' standards <- data.frame(concentration = rep(c(0, 0.5, 1, 2, 4, 8), each = 3))
#' standards$intensity <- 50 + 1000 * standards$concentration + rnorm(18, sd = 30)
#' cal <- calibration_curve(standards, intensity, concentration)
#' predict(cal, c(800, 3000), replicates = 3)
#' predict(cal, c(1, 5), type = "signal")
#' predict(cal, c(1, 5), type = "signal", interval = "confidence")
predict.specproc_calibration <- function(object, newdata, replicates = 1, level = 0.95,
                                         type = "concentration", interval = "prediction", ...) {
  type <- match.arg(type, c("concentration", "signal"))
  interval <- match.arg(interval, c("prediction", "confidence"))
  if (type == "signal") {
    x0 <- if (is.data.frame(newdata)) newdata[[object$names[["concentration"]]]] else newdata
    if (!is.numeric(x0)) {
      stop("'newdata' must be numeric concentrations, or a data frame with the column `",
           object$names[["concentration"]], "`.", call. = FALSE)
    }
    check_count(replicates, "replicates")
    check_number(level, "level", lower = 0, upper = 1, lower_open = TRUE, upper_open = TRUE)
    band <- calibration_band(object, x0, interval, level, replicates)
    return(tibble::tibble(concentration = x0, signal = band$fit, se = band$se,
                          lower = band$lower, upper = band$upper))
  }
  y0 <- if (is.data.frame(newdata)) newdata[[object$names[["signal"]]]] else newdata
  if (!is.numeric(y0)) {
    stop("'newdata' must be numeric signals, or a data frame with the column `",
         object$names[["signal"]], "`.", call. = FALSE)
  }
  check_count(replicates, "replicates")
  check_number(level, "level", lower = 0, upper = 1, lower_open = TRUE, upper_open = TRUE)
  beta <- stats::coef(object$fit)
  vcov <- stats::vcov(object$fit)
  sigma <- suppressWarnings(summary(object$fit))$sigma
  range_x <- range(object$data$concentration)
  x0 <- vapply(y0, function(y) inverse_calibration(beta, y, range_x), numeric(1))
  se <- vapply(seq_along(y0), function(i) {
    if (is.na(x0[i])) return(NA_real_)
    # gradient of x0 with respect to the coefficients and to the signal
    h <- 1e-6 * pmax(abs(beta), 1e-8)
    grad <- vapply(seq_along(beta), function(k) {
      b <- beta
      b[k] <- b[k] + h[k]
      (inverse_calibration(b, y0[i], range_x) - x0[i]) / h[k]
    }, numeric(1))
    dy <- 1 / calibration_slope(beta, x0[i])
    w0 <- calibration_weights(object$weights, x0[i], strict = FALSE)
    sqrt(max(0, drop(t(grad) %*% vcov %*% grad)) + dy^2 * sigma^2 / (replicates * w0))
  }, numeric(1))
  q <- stats::qt(1 - (1 - level) / 2, df = stats::df.residual(object$fit))
  fom <- object$figures_of_merit
  tibble::tibble(signal = y0, concentration = x0, se = se, lower = x0 - q * se,
                 upper = x0 + q * se, below_lod = x0 < fom$lod, below_loq = x0 < fom$loq)
}

#' @title Plot a Calibration Curve
#'
#' @description
#' Draws the calibration standards, the fitted curve with its confidence
#' and prediction bands, the limits of detection and quantification, and
#' optionally the concentrations predicted for new samples.
#'
#' @details
#' The confidence band shows where the mean signal lies; the prediction
#' band, where a single new measurement (or the mean of `replicates`) is
#' expected to fall (see [calibration_curve()]). With `newdata`, each new
#' signal is drawn on the curve at its predicted concentration, with guide
#' lines to the axes and a horizontal bar for the interval of the
#' concentration from [predict()][predict.specproc_calibration].
#'
#' @param object A curve fitted with [calibration_curve()].
#' @param interval The bands to draw: `"both"` (default), `"confidence"`,
#'   `"prediction"` or `"none"`.
#' @param level The confidence level of the bands and intervals. Default is
#'   0.95.
#' @param newdata Optional signals of new samples (a numeric vector, or a
#'   data frame with the signal column) to show with their predicted
#'   concentrations.
#' @param replicates The number of replicate measurements averaged in each
#'   new signal. Default is 1.
#' @param title The plot title. By default, the model and the figures of
#'   merit.
#'
#' @return A ggplot object.
#' @seealso [calibration_curve()], [predict.specproc_calibration()]
#' @export plot_calibration
#'
#' @examples
#' set.seed(1)
#' standards <- data.frame(concentration = rep(c(0, 0.5, 1, 2, 4, 8), each = 3))
#' standards$intensity <- 50 + 1000 * standards$concentration + rnorm(18, sd = 300)
#' cal <- calibration_curve(standards, intensity, concentration)
#' plot_calibration(cal)
#' plot_calibration(cal, newdata = c(800, 3000, 6500))
plot_calibration <- function(object, interval = "both", level = 0.95, newdata = NULL,
                             replicates = 1, title = NULL) {
  if (!inherits(object, "specproc_calibration")) {
    stop("'object' must be returned by calibration_curve().", call. = FALSE)
  }
  interval <- match.arg(interval, c("both", "confidence", "prediction", "none"))
  check_number(level, "level", lower = 0, upper = 1, lower_open = TRUE, upper_open = TRUE)
  check_count(replicates, "replicates")
  fom <- object$figures_of_merit
  x_grid <- seq(min(0, min(object$data$concentration)), max(object$data$concentration),
                length.out = 200)
  curve <- data.frame(concentration = x_grid,
                      fit = calibration_band(object, x_grid, "confidence", level, 1)$fit)
  pct <- paste0(format(100 * level), "%")
  band_labels <- c(confidence = paste(pct, "confidence band"),
                   prediction = paste(pct, "prediction band"))
  bands <- switch(interval, both = c("prediction", "confidence"), none = character(),
                  interval)
  limits <- data.frame(value = c(fom$lod, fom$loq), limit = c("LOD", "LOQ"))
  if (is.null(title)) {
    title <- sprintf("%s calibration: R2 = %.4f, LOD = %s, LOQ = %s",
                     if (object$model == "linear") "Linear" else "Quadratic", fom$r_squared,
                     format(signif(fom$lod, 3)), format(signif(fom$loq, 3)))
  }
  p <- ggplot2::ggplot()
  for (b in bands) {
    band <- calibration_band(object, x_grid, b, level, replicates)
    band_df <- data.frame(concentration = x_grid, lower = band$lower, upper = band$upper,
                          band = band_labels[[b]])
    p <- p + ggplot2::geom_ribbon(data = band_df,
                                  ggplot2::aes(x = .data$concentration, ymin = .data$lower,
                                               ymax = .data$upper, fill = .data$band))
  }
  if (length(bands) > 0) {
    p <- p + ggplot2::scale_fill_manual(values = stats::setNames(c("grey70", "grey90"), band_labels),
                                        breaks = unname(band_labels[bands]), name = NULL)
  }
  p <- p +
    ggplot2::geom_line(data = curve, ggplot2::aes(.data$concentration, .data$fit), colour = "#1f4e79") +
    ggplot2::geom_vline(data = limits, ggplot2::aes(xintercept = .data$value, linetype = .data$limit),
                        colour = "grey40") +
    ggplot2::geom_point(data = object$data, ggplot2::aes(.data$concentration, .data$signal), size = 2)
  if (!is.null(newdata)) {
    unknown <- stats::predict(object, newdata, replicates = replicates, level = level)
    unknown <- unknown[!is.na(unknown$concentration), , drop = FALSE]
    p <- p +
      ggplot2::geom_segment(data = unknown, ggplot2::aes(x = -Inf, xend = .data$concentration,
                                                         y = .data$signal, yend = .data$signal),
                            colour = "#c0392b", linetype = "dotted") +
      ggplot2::geom_segment(data = unknown, ggplot2::aes(x = .data$concentration, xend = .data$concentration,
                                                         y = .data$signal, yend = -Inf),
                            colour = "#c0392b", linetype = "dotted") +
      ggplot2::geom_errorbar(data = unknown, ggplot2::aes(y = .data$signal, xmin = .data$lower,
                                                          xmax = .data$upper),
                             orientation = "y", width = 0, colour = "#c0392b", linewidth = 0.8) +
      ggplot2::geom_point(data = unknown, ggplot2::aes(.data$concentration, .data$signal),
                          colour = "#c0392b", shape = 18, size = 3.5)
  }
  p <- p +
    ggplot2::scale_linetype_manual(values = c(LOD = "dashed", LOQ = "dotted"), name = NULL) +
    ggplot2::labs(x = object$names[["concentration"]], y = object$names[["signal"]], title = title,
                  subtitle = if (!is.null(newdata)) paste0("Red: new samples, with the ", pct,
                                                           " interval of their concentration")) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")
  finish_title(p)
}

#' @export
print.specproc_calibration <- function(x, ...) {
  fom <- x$figures_of_merit
  cat(if (x$model == "linear") "Linear" else "Quadratic", " calibration curve (", fom$n,
      " standards", if (!is.null(x$weights)) ", weighted", ")\n\n", sep = "")
  print(as.data.frame(x$coefficients), row.names = FALSE, digits = 4)
  cat("\nR-squared:    ", format(fom$r_squared, digits = 5), "\n", sep = "")
  cat("Sensitivity:  ", format(fom$sensitivity, digits = 4), "\n", sep = "")
  cat("LOD:          ", format(fom$lod, digits = 3), "  (", x$lod_method, ")\n", sep = "")
  cat("LOQ:          ", format(fom$loq, digits = 3), "\n", sep = "")
  for (i in seq_len(nrow(x$linearity))) {
    cat(sprintf("%-14s F = %.2f, p = %s\n", paste0(x$linearity$test[i], ":"),
                x$linearity$statistic[i], format.pval(x$linearity$p_value[i], digits = 3)))
  }
  invisible(x)
}

# ---- internals ---------------------------------------------------------------

calibration_weights <- function(weights, x, strict = TRUE) {
  if (is.null(weights)) return(rep(1, length(x)))
  if (is.character(weights)) {
    weights <- match.arg(weights, c("1/x", "1/x2"))
    if (strict && any(x <= 0, na.rm = TRUE)) {
      stop("Weights \"", weights, "\" need positive concentrations.", call. = FALSE)
    }
    x <- pmax(x, .Machine$double.eps)
    return(if (weights == "1/x") 1 / x else 1 / x^2)
  }
  if (!strict) return(rep(1, length(x)))   # numeric weights: new samples get weight 1
  if (!is.numeric(weights) || length(weights) != length(x) || any(weights < 0, na.rm = TRUE)) {
    stop("'weights' must be \"1/x\", \"1/x2\" or non-negative numbers, one per row.",
         call. = FALSE)
  }
  weights
}

calibration_linearity <- function(d, w, fit, model) {
  out <- list()
  n <- nrow(d)
  if (model == "linear" && n > 3) {
    quad <- stats::lm(signal ~ concentration + I(concentration^2), data = d, weights = w)
    rss1 <- sum(w * stats::residuals(fit)^2)
    rss2 <- sum(w * stats::residuals(quad)^2)
    f <- (rss1 - rss2) / (rss2 / (n - 3))
    out$mandel <- c(statistic = f, df1 = 1, df2 = n - 3,
                    p_value = stats::pf(f, 1, n - 3, lower.tail = FALSE))
  }
  levels <- length(unique(d$concentration))
  p <- length(stats::coef(fit))
  if (levels < n && levels > p) {
    pure <- sum(w * (d$signal - stats::ave(d$signal, d$concentration,
                                           FUN = function(v) mean(v)))^2)
    rss <- sum(w * stats::residuals(fit)^2)
    df1 <- levels - p
    df2 <- n - levels
    f <- ((rss - pure) / df1) / (pure / df2)
    out$lack_of_fit <- c(statistic = f, df1 = df1, df2 = df2,
                         p_value = stats::pf(f, df1, df2, lower.tail = FALSE))
  }
  if (length(out) == 0) {
    return(tibble::tibble(test = character(), statistic = numeric(), df1 = numeric(),
                          df2 = numeric(), p_value = numeric()))
  }
  tibble::tibble(test = unname(c(mandel = "Mandel", lack_of_fit = "Lack of fit")[names(out)]),
                 statistic = vapply(out, `[[`, numeric(1), "statistic"),
                 df1 = vapply(out, `[[`, numeric(1), "df1"),
                 df2 = vapply(out, `[[`, numeric(1), "df2"),
                 p_value = vapply(out, `[[`, numeric(1), "p_value"))
}

# Fitted signal at concentrations x, with a confidence or prediction interval
# (a new signal being the mean of `replicates` measurements).
calibration_band <- function(object, x, interval, level, replicates) {
  pred <- stats::predict(object$fit, newdata = data.frame(concentration = x), se.fit = TRUE)
  sigma <- suppressWarnings(summary(object$fit))$sigma
  se <- pred$se.fit
  if (interval == "prediction") {
    w <- calibration_weights(object$weights, x, strict = FALSE)
    se <- sqrt(se^2 + sigma^2 / (replicates * w))
  }
  q <- stats::qt(1 - (1 - level) / 2, df = stats::df.residual(object$fit))
  list(fit = unname(pred$fit), se = unname(se), lower = unname(pred$fit - q * se),
       upper = unname(pred$fit + q * se))
}

calibration_slope <- function(beta, x) {
  beta[2] + if (length(beta) == 3) 2 * beta[3] * x else 0
}

# Concentration giving the signal y: the root within or nearest the range.
inverse_calibration <- function(beta, y, range_x) {
  if (is.na(y)) return(NA_real_)
  if (length(beta) == 2) return(unname((y - beta[1]) / beta[2]))
  a <- beta[3]
  b <- beta[2]
  c0 <- beta[1] - y
  if (abs(a) < .Machine$double.eps * max(1, abs(b))) return(unname(-c0 / b))
  disc <- b^2 - 4 * a * c0
  if (disc < 0) return(NA_real_)
  roots <- (-b + c(-1, 1) * sqrt(disc)) / (2 * a)
  # distance to the calibration range, then the root on the rising or
  # falling branch of the standards
  dist <- pmax(range_x[1] - roots, roots - range_x[2], 0)
  unname(roots[order(dist, abs(roots - mean(range_x)))[1]])
}
