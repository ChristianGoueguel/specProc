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
#' With `lod_convention = "iso11843"`, the limits follow ISO 11843-2 and
#' IUPAC (Currie, 1999), and account for the uncertainty of the curve and
#' for the false positive and false negative risks \eqn{\alpha} and
#' \eqn{\beta}. The standard deviation of a net signal near zero is
#' \eqn{\sigma_0 = \sqrt{\sigma^2 / K + s_a^2}}, where \eqn{\sigma} is the
#' standard deviation of the signal (`"residual"` or `"blank"`), \eqn{K} the
#' number of `replicates` averaged for a sample and \eqn{s_a} the standard
#' error of the intercept. The critical value (decision limit), above which
#' an element is detected, is \eqn{x_C = t_{1-\alpha,\nu}\,\sigma_0 / S}; the
#' limit of detection, the content detected with probability \eqn{1 -
#' \beta}, is \eqn{x_D = \delta_{\nu,\alpha,\beta}\,\sigma_0 / S}, where
#' \eqn{\delta} is the noncentrality parameter of the noncentral t
#' distribution (about \eqn{2 t_{1-\alpha,\nu}} for \eqn{\alpha = \beta}) and
#' \eqn{\nu} the degrees of freedom of \eqn{\sigma}; and the limit of
#' quantification (IUPAC) is \eqn{x_Q = 10\,\sigma_0 / S}. For a weighted
#' curve, the standard deviation must come from `blank` signals.
#'
#' **Errors in the concentrations.** Least squares assume exact
#' concentrations. When those of the standards are uncertain, such as the
#' certified values of reference materials, the slope is biased toward zero
#' and the intervals too narrow. `regression = "york"` fits the line by the
#' method of York et al. (2004), with the standard uncertainties of the
#' concentration (`concentration_se`) and of the signal (`signal_se`) of
#' each standard; its standard errors are multiplied by the square root of
#' the reduced chi-squared when it exceeds 1 (the points scatter more than
#' their uncertainties explain). `regression = "deming"` (Deming
#' regression) assumes constant uncertainties, whose ratio
#' `variance_ratio` (variance of the signal over variance of the
#' concentration, in their units) is given, or computed from the mean
#' squared `signal_se` and `concentration_se`; its standard errors come from
#' the scatter of the points. Both fit straight lines without weights; the
#' tests of linearity are those of the least-squares fit.
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
#' @param regression `"ols"` (default, least squares), `"deming"` or
#'   `"york"` (see details).
#' @param concentration_se,signal_se For `regression = "york"` or
#'   `"deming"`: the columns of `data` with the standard uncertainties of the
#'   concentration and of the signal of each standard, unquoted or as
#'   strings.
#' @param variance_ratio For `regression = "deming"`: the ratio of the error
#'   variance of the signal to that of the concentration. By default, it is
#'   computed from `signal_se` and `concentration_se`.
#' @param lod_convention The convention of the limits of detection and
#'   quantification: `"ich"` (default, \eqn{3.3\,\sigma/S} and
#'   \eqn{10\,\sigma/S}) or `"iso11843"` (ISO 11843-2 and IUPAC; see
#'   details).
#' @param alpha,beta For `lod_convention = "iso11843"`: the probabilities of
#'   false positives and of false negatives. Default is 0.05.
#' @param replicates For `lod_convention = "iso11843"`: the number of
#'   replicate measurements averaged for a sample. Default is 1.
#'
#' @return An object of class `specproc_calibration`, a list with
#'  - `coefficients`: a tibble of the estimates, standard errors and
#'    confidence intervals (`lower`, `upper`) at `level`;
#'  - `figures_of_merit`: a tibble with the number of standards `n`, the
#'    `sensitivity`, `r_squared`, the residual standard deviation `sigma`,
#'    the blank standard deviation used (`sigma_blank`), the
#'    `critical_value` (ISO 11843 only, `NA` otherwise), `lod` and `loq` (in
#'    concentration units);
#'  - `linearity`: a tibble with the statistic, degrees of freedom and
#'    p-value of Mandel's test and of the lack-of-fit test (when they apply);
#'  - `fit`: the least-squares `lm` fit, `eiv`, the Deming or York fit
#'    (`NULL` for least squares), with its `chi_squared`, and `data`, the
#'    calibration data.
#'
#' Use [predict()][predict.specproc_calibration] for new samples and
#' [plot_calibration()] to draw the curve.
#'
#' @references
#'  - Currie, L.A. (1999). Detection and quantification limits: origins and
#'    historical overview. Analytica Chimica Acta, 391(2):127-134.
#'  - ICH (2005). Validation of analytical procedures: text and methodology
#'    Q2(R1). International Conference on Harmonisation.
#'  - ISO 11843-2 (2000). Capability of detection, Part 2: Methodology in
#'    the linear calibration case. International Organization for
#'    Standardization, Geneva.
#'  - Mandel, J. (1964). The Statistical Analysis of Experimental Data.
#'    Interscience, New York.
#'  - Miller, J.N., Miller, J.C. (2018). Statistics and Chemometrics for
#'    Analytical Chemistry, 7th ed. Pearson, Harlow.
#'  - York, D., Evensen, N.M., López Martínez, M., De Basabe Delgado, J.
#'    (2004). Unified equations for the slope, intercept, and standard errors
#'    of the best straight line. American Journal of Physics, 72(3):367-375.
#'
#' @seealso [predict.specproc_calibration()], [plot_calibration()],
#'   [line_intensities()], [nas()]
#' @export calibration_curve
#'
#' @examples
#' data(forageLIBS)
#' # the K I 769.90 nm line, normalized to the C I 247.86 nm line of the matrix
#' lines <- line_intensities(forageLIBS[-(1:14)], c(C = 247.856, K = 769.896), baseline = TRUE)
#' standards <- data.frame(
#'   K = forageLIBS$K,
#'   signal = lines$intensity[lines$line == "K"] / lines$intensity[lines$line == "C"]
#' )
#' cal <- calibration_curve(standards[1:300, ], signal, K)
#' cal
#' # the concentrations of new samples, from their signals
#' head(predict(cal, standards$signal[301:368]))
#'
#' # limits of detection by ISO 11843-2
#' calibration_curve(standards[1:300, ], signal, K, lod_convention = "iso11843")
#'
#' # reference materials with certified values known within 3%, and
#' # signals within 2% (simulated)
#' set.seed(1)
#' crm <- data.frame(certified = c(0.5, 1, 2, 4, 6, 8))
#' crm$signal <- 0.05 + 0.2 * crm$certified * (1 + rnorm(6, sd = 0.02)) +
#'   0.2 * rnorm(6, sd = 0.03 * crm$certified)
#' crm$certified_se <- 0.03 * crm$certified
#' crm$signal_se <- 0.02 * crm$signal
#' calibration_curve(crm, signal, certified, regression = "york",
#'                   concentration_se = certified_se, signal_se = signal_se)
#'
calibration_curve <- function(data, signal, concentration, model = "linear", weights = NULL,
                              blank = NULL, lod_method = NULL, level = 0.95, regression = "ols",
                              concentration_se = NULL, signal_se = NULL, variance_ratio = NULL,
                              lod_convention = "ich", alpha = 0.05, beta = 0.05,
                              replicates = 1) {
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
  regression <- match.arg(regression, c("ols", "deming", "york"))
  lod_convention <- match.arg(lod_convention, c("ich", "iso11843"))
  check_number(level, "level", lower = 0, upper = 1, lower_open = TRUE, upper_open = TRUE)
  check_number(alpha, "alpha", lower = 0, upper = 0.5, lower_open = TRUE)
  check_number(beta, "beta", lower = 0, upper = 0.5, lower_open = TRUE)
  replicates <- check_count(replicates, "replicates")
  if (is.null(lod_method)) lod_method <- if (is.null(blank)) "residual" else "blank"
  lod_method <- match.arg(lod_method, c("residual", "intercept", "blank"))
  if (lod_method == "blank" && (is.null(blank) || !is.numeric(blank) || sum(!is.na(blank)) < 2)) {
    stop("The \"blank\" method needs at least 2 replicate `blank` signals.", call. = FALSE)
  }
  errors <- list(concentration = calibration_column(data, rlang::enquo(concentration_se)),
                 signal = calibration_column(data, rlang::enquo(signal_se)))
  if (regression != "ols") {
    if (model != "linear") {
      stop("Deming and York regressions fit straight lines (`model = \"linear\"`).", call. = FALSE)
    }
    if (!is.null(weights)) {
      stop("Deming and York regressions weight the standards by their uncertainties: ",
           "leave `weights` NULL.", call. = FALSE)
    }
    if ((regression == "york" || is.null(variance_ratio)) &&
        (is.null(errors$concentration) || is.null(errors$signal))) {
      stop(if (regression == "york") "York regression needs `concentration_se` and `signal_se`."
           else "Deming regression needs `variance_ratio`, or `concentration_se` and `signal_se`.",
           call. = FALSE)
    }
  }

  d <- tibble::tibble(concentration = data[[concentration]], signal = data[[signal]],
                      concentration_se = errors$concentration %||% NA_real_,
                      signal_se = errors$signal %||% NA_real_)
  w <- calibration_weights(weights, d$concentration)
  keep <- stats::complete.cases(d[c("concentration", "signal")]) & !is.na(w)
  if (regression == "york" || (regression == "deming" && is.null(variance_ratio))) {
    keep <- keep & !is.na(d$concentration_se) & !is.na(d$signal_se)
  }
  d <- d[keep, , drop = FALSE]
  w <- w[keep]
  if (is.null(errors$concentration) && is.null(errors$signal)) {
    d$concentration_se <- NULL
    d$signal_se <- NULL
  }
  p <- if (model == "linear") 2 else 3
  if (length(unique(d$concentration)) < p + 1) {
    stop("A ", model, " calibration needs at least ", p + 1, " distinct concentrations.",
         call. = FALSE)
  }
  formula <- if (model == "linear") signal ~ concentration else signal ~ concentration + I(concentration^2)
  fit <- if (is.null(weights)) stats::lm(formula, data = d) else stats::lm(formula, data = d, weights = w)
  s <- suppressWarnings(summary(fit))
  object <- list(fit = fit, data = d, model = model, weights = weights, regression = regression,
                 eiv = if (regression != "ols") eiv_fit(d, regression, variance_ratio))
  beta_hat <- calibration_coef(object)
  vcov <- calibration_vcov(object)
  sigma <- calibration_sigma(object)
  df <- calibration_df(object)
  sensitivity <- beta_hat[[2]]
  if (!is.finite(sensitivity) || sensitivity == 0) {
    stop("The calibration curve has no slope at zero concentration.", call. = FALSE)
  }
  sigma_blank <- switch(
    lod_method,
    residual = sigma,
    intercept = sqrt(vcov[1, 1]),
    blank = stats::sd(blank, na.rm = TRUE)
  )
  limits <- detection_limits(lod_convention, lod_method, sigma_blank, sensitivity, vcov,
                             df = if (lod_method == "blank") sum(!is.na(blank)) - 1 else df,
                             weighted = !is.null(weights), alpha, beta, replicates)

  se <- sqrt(diag(vcov))
  q <- stats::qt(1 - (1 - level) / 2, df = df)
  structure(
    c(object, list(
      coefficients = tibble::tibble(term = c("intercept", "slope", "quadratic")[seq_len(p)],
                                    estimate = unname(beta_hat), std_error = unname(se),
                                    lower = unname(beta_hat - q * se),
                                    upper = unname(beta_hat + q * se)),
      figures_of_merit = tibble::tibble(
        n = nrow(d), sensitivity = sensitivity, r_squared = s$r.squared, sigma = sigma,
        sigma_blank = sigma_blank, critical_value = limits$critical_value, lod = limits$lod,
        loq = limits$loq
      ),
      linearity = calibration_linearity(d, w, fit, model),
      lod_method = lod_method, lod_convention = lod_convention,
      detection = list(alpha = alpha, beta = beta, replicates = replicates), level = level,
      names = c(signal = signal, concentration = concentration)
    )),
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
#'   (`lower`, `upper`), whether it is below the limit of detection
#'   (`below_lod`) or of quantification (`below_loq`), whether it lies
#'   outside the range of the standards (`outside_range`, an
#'   extrapolation), and, with the ISO 11843 limits, whether the element is
#'   `detected` (above the critical value). For `type =
#'   "signal"`, a tibble with the `concentration`, the predicted `signal`,
#'   its standard error `se` and the interval (`lower`, `upper`).
#'
#' @seealso [calibration_curve()], [plot_calibration()]
#' @export
#'
#' @examples
#' data(forageLIBS)
#' # the K I 769.90 nm line, normalized to the C I 247.86 nm line of the matrix
#' lines <- line_intensities(forageLIBS[-(1:14)], c(C = 247.856, K = 769.896), baseline = TRUE)
#' standards <- data.frame(
#'   K = forageLIBS$K,
#'   signal = lines$intensity[lines$line == "K"] / lines$intensity[lines$line == "C"]
#' )
#' cal <- calibration_curve(standards[1:300, ], signal, K)
#' predict(cal, standards$signal[301:305])
#' # the signal expected at given contents (% K)
#' predict(cal, c(1, 2, 3), type = "signal")
#' predict(cal, c(1, 2, 3), type = "signal", interval = "confidence")
#'
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
  beta <- calibration_coef(object)
  vcov <- calibration_vcov(object)
  sigma <- calibration_sigma(object)
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
  q <- stats::qt(1 - (1 - level) / 2, df = calibration_df(object))
  fom <- object$figures_of_merit
  out <- tibble::tibble(signal = y0, concentration = x0, se = se, lower = x0 - q * se,
                        upper = x0 + q * se, below_lod = x0 < fom$lod, below_loq = x0 < fom$loq,
                        outside_range = x0 < range_x[1] | x0 > range_x[2])
  if (!is.na(fom$critical_value %||% NA)) {
    out$detected <- x0 > fom$critical_value
  }
  out
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
#' @param xlab,ylab The axis titles. Default are the names of the
#'   concentration and signal columns.
#' @param base_size The size of the text, in points. Default is 11.
#' @param point_size The size of the points of the standards. Default is 2.
#' @param point_alpha The opacity of the points of the standards, between 0
#'   and 1. Default is 1; lower it when replicates overlap.
#'
#' @return A ggplot object.
#' @seealso [calibration_curve()], [predict.specproc_calibration()]
#' @export plot_calibration
#'
#' @examples
#' # the calibration curve of potassium of vignette("calibration")
#' data(forageLIBS)
#' spectra_id <- names(forageLIBS)[1:2]
#' minerals <- names(forageLIBS)[3:14]
#' spectra <- forageLIBS[setdiff(names(forageLIBS), c(spectra_id, minerals))]
#' corrected <- baseline_arpls(spectra, lambda = 1e5, max_iter = 20)$correction
#'
#' # the K I 769.90 nm line, normalized to the C I 247.86 nm line of the matrix
#' lines <- line_intensities(corrected, c(C = 247.856, K = 769.896))
#' data <- forageLIBS[c(spectra_id, "K")]
#' data$K_signal <- lines$intensity[lines$line == "K"] / lines$intensity[lines$line == "C"]
#'
#' # a quarter of the samples are set aside to test the calibration
#' set.seed(1)
#' test_samples <- sample(unique(data$Sample), round(0.25 * length(unique(data$Sample))))
#' calibration <- data[!data$Sample %in% test_samples, ]
#'
#' k_curve <- calibration_curve(calibration, K_signal, K)
#' plot_calibration(k_curve, point_alpha = 0.4, xlab = "K (%)",
#'                  ylab = "K I 769.90 nm / C I 247.86 nm")
plot_calibration <- function(object, interval = "both", level = 0.95, newdata = NULL,
                             replicates = 1, title = NULL, xlab = NULL, ylab = NULL,
                             base_size = 11, point_size = 2, point_alpha = 1) {
  if (!inherits(object, "specproc_calibration")) {
    stop("'object' must be returned by calibration_curve().", call. = FALSE)
  }
  check_plot_text(title, xlab, ylab, base_size)
  check_size(point_size, "point_size")
  check_alpha(point_alpha, "point_alpha")
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
  if (!is.na(fom$critical_value %||% NA)) {
    limits <- rbind(data.frame(value = fom$critical_value, limit = "Critical value"), limits)
  }
  if (is.null(title)) {
    title <- sprintf("%s calibration: R2 = %.4f, LOD = %s, LOQ = %s",
                     switch(object$regression %||% "ols",
                            ols = if (object$model == "linear") "Linear" else "Quadratic",
                            deming = "Deming", york = "York"),
                     fom$r_squared, format(signif(fom$lod, 3)), format(signif(fom$loq, 3)))
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
    ggplot2::geom_point(data = object$data, ggplot2::aes(.data$concentration, .data$signal),
                        size = point_size, alpha = point_alpha)
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
    ggplot2::scale_linetype_manual(values = c(`Critical value` = "dotdash", LOD = "dashed",
                                              LOQ = "dotted"),
                                   breaks = limits$limit, name = NULL) +
    ggplot2::labs(x = xlab %||% object$names[["concentration"]],
                  y = ylab %||% object$names[["signal"]], title = title,
                  subtitle = if (!is.null(newdata)) paste0("Red: new samples, with the ", pct,
                                                           " interval of their concentration")) +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(legend.position = "bottom")
  finish_title(p)
}

#' @export
print.specproc_calibration <- function(x, ...) {
  fom <- x$figures_of_merit
  regression <- x$regression %||% "ols"
  cat(if (x$model == "linear") "Linear" else "Quadratic", " calibration curve (", fom$n,
      " standards", if (!is.null(x$weights)) ", weighted",
      switch(regression, ols = "", deming = ", Deming regression", york = ", York regression"),
      ")\n\n", sep = "")
  print(as.data.frame(x$coefficients), row.names = FALSE, digits = 4)
  cat("\nR-squared:    ", format(fom$r_squared, digits = 5), "\n", sep = "")
  if (regression == "york") {
    cat("Chi-squared:  ", format(x$eiv$chi_squared, digits = 3), " (reduced)\n", sep = "")
  } else if (regression == "deming") {
    cat("Error ratio:  ", format(x$eiv$variance_ratio, digits = 3),
        " (variance of the signal / variance of the concentration)\n", sep = "")
  }
  cat("Sensitivity:  ", format(fom$sensitivity, digits = 4), "\n", sep = "")
  if (identical(x$lod_convention, "iso11843")) {
    d <- x$detection
    cat("Critical:     ", format(fom$critical_value, digits = 3), "  (ISO 11843, alpha = ",
        format(d$alpha), ")\n", sep = "")
    cat("LOD:          ", format(fom$lod, digits = 3), "  (beta = ", format(d$beta), "; ",
        x$lod_method, if (d$replicates > 1) paste0("; ", d$replicates, " replicates"), ")\n",
        sep = "")
    cat("LOQ:          ", format(fom$loq, digits = 3), "  (IUPAC, 10 sigma)\n", sep = "")
  } else {
    cat("LOD:          ", format(fom$lod, digits = 3), "  (", x$lod_method, ")\n", sep = "")
    cat("LOQ:          ", format(fom$loq, digits = 3), "\n", sep = "")
  }
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
  beta <- calibration_coef(object)
  design <- outer(x, seq_along(beta) - 1, `^`)
  fit <- drop(design %*% beta)
  se <- sqrt(pmax(rowSums((design %*% calibration_vcov(object)) * design), 0))
  if (interval == "prediction") {
    w <- calibration_weights(object$weights, x, strict = FALSE)
    se <- sqrt(se^2 + calibration_sigma(object)^2 / (replicates * w))
  }
  q <- stats::qt(1 - (1 - level) / 2, df = calibration_df(object))
  list(fit = unname(fit), se = unname(se), lower = unname(fit - q * se),
       upper = unname(fit + q * se))
}

# Coefficients (intercept, slope[, quadratic]), their covariance, the
# residual standard deviation of the signal and its degrees of freedom, of
# the least-squares or errors-in-variables fit.
calibration_coef <- function(object) {
  if (is.null(object$eiv)) unname(stats::coef(object$fit)) else object$eiv$coefficients
}

calibration_vcov <- function(object) {
  if (is.null(object$eiv)) unname(stats::vcov(object$fit)) else object$eiv$vcov
}

calibration_sigma <- function(object) {
  if (is.null(object$eiv)) suppressWarnings(summary(object$fit))$sigma else object$eiv$sigma
}

calibration_df <- function(object) {
  if (is.null(object$eiv)) stats::df.residual(object$fit) else nrow(object$data) - 2
}

# A column of `data` named by a quosure (NULL when not given).
calibration_column <- function(data, quo) {
  if (rlang::quo_is_null(quo)) return(NULL)
  col <- rlang::as_name(quo)
  if (!col %in% names(data)) stop("Column `", col, "` not found in 'data'.", call. = FALSE)
  v <- data[[col]]
  if (!is.numeric(v) || any(v < 0, na.rm = TRUE)) {
    stop("Column `", col, "` must contain non-negative standard uncertainties.", call. = FALSE)
  }
  v
}

# Errors-in-variables fit of a straight line: York's regression with the
# standard uncertainties of each standard, or Deming's with a constant ratio
# of the error variances (a York regression with constant uncertainties, whose
# scale is estimated from the residuals).
eiv_fit <- function(d, regression, variance_ratio) {
  if (regression == "deming") {
    if (is.null(variance_ratio)) {
      if (is.null(d$concentration_se) || is.null(d$signal_se)) {
        stop("Deming regression needs `variance_ratio`, or `concentration_se` and `signal_se`.",
             call. = FALSE)
      }
      variance_ratio <- mean(d$signal_se^2) / mean(d$concentration_se^2)
    }
    check_number(variance_ratio, "variance_ratio", lower = 0, lower_open = TRUE)
    n <- nrow(d)
    fit <- york_fit(d$concentration, d$signal, rep(1, n), rep(sqrt(variance_ratio), n))
    scale <- fit$chi_squared
  } else {
    if (is.null(d$concentration_se) || is.null(d$signal_se)) {
      stop("York regression needs `concentration_se` and `signal_se`.", call. = FALSE)
    }
    if (any(d$concentration_se == 0 & d$signal_se == 0)) {
      stop("Each standard needs a positive uncertainty of its concentration or signal.",
           call. = FALSE)
    }
    fit <- york_fit(d$concentration, d$signal, d$concentration_se, d$signal_se)
    scale <- max(1, fit$chi_squared)
    variance_ratio <- NULL
  }
  residuals <- d$signal - fit$coefficients[1] - fit$coefficients[2] * d$concentration
  list(coefficients = fit$coefficients, vcov = fit$vcov * scale,
       sigma = sqrt(sum(residuals^2) / (nrow(d) - 2)), chi_squared = fit$chi_squared,
       variance_ratio = variance_ratio, iterations = fit$iterations)
}

# York et al. (2004): the straight line through points with standard
# uncertainties sx and sy (correlation r), with the covariance of its
# intercept and slope and the reduced chi-squared of the fit.
york_fit <- function(x, y, sx, sy, r = 0, tol = 1e-12, max_iter = 100) {
  n <- length(x)
  b <- stats::coef(stats::lm(y ~ x))[[2]]
  weights <- function(b) 1 / (sy^2 + b^2 * sx^2 - 2 * b * r * sx * sy)
  for (iter in seq_len(max_iter)) {
    w <- weights(b)
    u <- x - sum(w * x) / sum(w)
    v <- y - sum(w * y) / sum(w)
    beta <- w * (u * sy^2 + b * v * sx^2 - (b * u + v) * r * sx * sy)
    b_new <- sum(w * beta * v) / sum(w * beta * u)
    converged <- abs(b_new - b) <= tol * abs(b_new)
    b <- b_new
    if (converged) break
  }
  if (!converged) {
    warning("York regression did not converge in ", max_iter, " iterations.", call. = FALSE)
  }
  w <- weights(b)
  x_mean <- sum(w * x) / sum(w)
  y_mean <- sum(w * y) / sum(w)
  u <- x - x_mean
  v <- y - y_mean
  beta <- w * (u * sy^2 + b * v * sx^2 - (b * u + v) * r * sx * sy)
  a <- y_mean - b * x_mean
  adjusted <- x_mean + beta                  # the adjusted abscissas
  adjusted_mean <- sum(w * adjusted) / sum(w)
  var_b <- 1 / sum(w * (adjusted - adjusted_mean)^2)
  var_a <- 1 / sum(w) + adjusted_mean^2 * var_b
  cov_ab <- -adjusted_mean * var_b
  list(coefficients = c(a, b), vcov = matrix(c(var_a, cov_ab, cov_ab, var_b), 2),
       chi_squared = sum(w * (y - a - b * x)^2) / (n - 2), iterations = iter)
}

# Critical value, limits of detection and quantification (concentration
# units): ICH (3.3 and 10 sigma / S), or ISO 11843-2 and IUPAC (Currie) with
# the uncertainty of the intercept, the risks alpha and beta, and the mean of
# `replicates` measurements of the sample.
detection_limits <- function(convention, lod_method, sigma_blank, sensitivity, vcov, df,
                             weighted, alpha, beta, replicates) {
  if (convention == "ich") {
    return(list(critical_value = NA_real_, lod = 3.3 * sigma_blank / abs(sensitivity),
                loq = 10 * sigma_blank / abs(sensitivity)))
  }
  if (lod_method == "intercept") {
    stop("ISO 11843 limits need the standard deviation of the signal (`lod_method` ",
         "\"residual\" or \"blank\").", call. = FALSE)
  }
  if (weighted && lod_method == "residual") {
    stop("With weights, the residual standard deviation depends on their scale: give ",
         "`blank` signals for ISO 11843 limits.", call. = FALSE)
  }
  if (df < 1) {
    stop("ISO 11843 limits need at least one degree of freedom for the standard deviation.",
         call. = FALSE)
  }
  # the standard deviation of a net signal near zero: new measurement and intercept
  sd_net <- sqrt(sigma_blank^2 / replicates + vcov[1, 1])
  t_crit <- stats::qt(1 - alpha, df)
  delta <- stats::uniroot(function(d) stats::pt(t_crit, df, ncp = d) - beta,
                          c(0, 2 * t_crit + 10 * stats::qnorm(1 - beta)), tol = 1e-10)$root
  list(critical_value = t_crit * sd_net / abs(sensitivity), lod = delta * sd_net / abs(sensitivity),
       loq = 10 * sd_net / abs(sensitivity))
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
