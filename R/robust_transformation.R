# Robust Box-Cox and Yeo-Johnson transformations to central normality, after
# Raymaekers and Rousseeuw (2021). Used by robust_boxcox(), robust_yeojohnson()
# and their recipe steps (and by the deprecated robust_bcyj()).
#
# For each variable:
#  1. The variable is pre-standardized: divided by its median for Box-Cox
#     (which needs positive values and is not scale-equivariant), centered
#     by its median and divided by its MAD for Yeo-Johnson.
#  2. An initial lambda minimizes a robust distance between the sorted,
#     robustly standardized transformed values and the normal quantiles
#     (sum of Tukey's biweight of the differences). The transformation is
#     rectified for this step: on the side it compresses (the upper side
#     for lambda < 1, the lower side for lambda > 1), beyond the point where
#     the transformed value is 1.5 times that of the quartile, it is
#     continued linearly, so that outliers on that side cannot dictate
#     lambda.
#  3. Reweighting: the values whose transformed value, standardized by
#     Huber's M-estimates of location and scale, exceeds
#     the `quantile` of the normal distribution get weight zero (first with
#     the rectified transformation), and lambda is the maximum likelihood
#     estimate on the others. This is repeated `nbsteps` times.
#  4. The transformed values are standardized by the mean and standard
#     deviation of the inliers.

transformation_range <- c(-4, 6)

bc_transform <- function(x, lambda) {
  if (abs(lambda) < 1e-8) log(x) else (x^lambda - 1) / lambda
}

yj_transform <- function(x, lambda) {
  out <- numeric(length(x))
  pos <- !is.na(x) & x >= 0
  neg <- !is.na(x) & x < 0
  out[is.na(x)] <- NA_real_
  out[pos] <- if (abs(lambda) < 1e-8) log1p(x[pos]) else ((x[pos] + 1)^lambda - 1) / lambda
  out[neg] <- if (abs(lambda - 2) < 1e-8) -log1p(-x[neg]) else
    -((1 - x[neg])^(2 - lambda) - 1) / (2 - lambda)
  out
}

# Derivative of the transformation with respect to x, for the rectification.
transform_slope <- function(x, lambda, type) {
  if (type == "BC") {
    x^(lambda - 1)
  } else if (x >= 0) {
    (x + 1)^(lambda - 1)
  } else {
    (1 - x)^(1 - lambda)
  }
}

transform_values <- function(x, lambda, type) {
  if (type == "BC") bc_transform(x, lambda) else yj_transform(x, lambda)
}

# Inverse transformations: NA beyond the range of the transformation (the
# Box-Cox transformation is bounded above for lambda < 0; the Yeo-Johnson
# transformation above for lambda < 0 and below for lambda > 2).
bc_inverse <- function(y, lambda) {
  if (abs(lambda) < 1e-8) {
    return(exp(y))
  }
  out <- rep(NA_real_, length(y))
  base <- lambda * y + 1
  ok <- !is.na(base) & base > 0
  out[ok] <- base[ok]^(1 / lambda)
  out
}

yj_inverse <- function(y, lambda) {
  out <- rep(NA_real_, length(y))
  pos <- which(!is.na(y) & y >= 0)
  neg <- which(!is.na(y) & y < 0)
  if (abs(lambda) < 1e-8) {
    out[pos] <- expm1(y[pos])
  } else {
    base <- lambda * y[pos] + 1
    ok <- base > 0
    out[pos[ok]] <- base[ok]^(1 / lambda) - 1
  }
  if (abs(lambda - 2) < 1e-8) {
    out[neg] <- -expm1(-y[neg])
  } else {
    base <- 1 - (2 - lambda) * y[neg]
    ok <- base > 0
    out[neg[ok]] <- 1 - base[ok]^(1 / (2 - lambda))
  }
  out
}

# The point beyond which the transformation is rectified: on the side that it
# compresses (the upper side for lambda < 1, the lower side for lambda > 1),
# where the transformed value is 1.5 times that of the quartile (measured
# from the transformed median, zero after pre-standardization), so that the
# bulk of the data keep their exact transformation.
rectification_point <- function(x, lambda, type, quartiles) {
  q <- if (lambda < 1) quartiles[2] else quartiles[1]
  target <- 1.5 * transform_values(q, lambda, type)
  # stay inside the range of the transformation
  if (type == "BC" || target >= 0) {
    if (lambda < 0) target <- min(target, -1 / lambda - 1e-5)
    if (lambda > 0 && type == "BC") target <- max(target, -1 / lambda + 1e-5)
  } else if (lambda > 2) {
    target <- max(target, -1 / (lambda - 2) + 1e-5)
  }
  point <- if (type == "BC") bc_inverse(target, lambda) else yj_inverse(target, lambda)
  min(max(point, min(x)), max(x))
}

# The rectified transformation: continued linearly beyond the rectification
# point, so that outliers on the side that the transformation compresses
# cannot be pulled in by the choice of lambda.
rectified_transform <- function(x, lambda, type, quartiles) {
  y <- transform_values(x, lambda, type)
  if (lambda == 1) {
    return(y)
  }
  point <- rectification_point(x, lambda, type, quartiles)
  beyond <- if (lambda < 1) x > point else x < point
  if (any(beyond)) {
    y[beyond] <- transform_values(point, lambda, type) +
      transform_slope(point, lambda, type) * (x[beyond] - point)
  }
  y
}

# Standardizes with Huber's M-estimates of location and scale (Proposal 2),
# or the median and MAD when they do not converge.
huber_standardize <- function(y) {
  est <- tryCatch(MASS::hubers(y), error = function(e) NULL, warning = function(w) NULL)
  if (is.null(est) || !is.finite(est$s) || est$s <= 0) {
    est <- list(mu = stats::median(y), s = stats::mad(y))
  }
  (y - est$mu) / est$s
}

tukey_rho <- function(r, c) {
  u <- pmin(abs(r) / c, 1)
  c^2 / 6 * (1 - (1 - u^2)^3)
}

# Robust distance between the standardized sorted values and the normal
# quantiles.
normality_objective <- function(y) {
  s <- stats::mad(y)
  if (!is.finite(s) || s <= 0) {
    return(Inf)
  }
  z <- sort((y - stats::median(y)) / s)
  n <- length(z)
  q <- stats::qnorm((seq_len(n) - 1 / 3) / (n + 1 / 3))
  sum(tukey_rho(z - q, 0.5))
}

# Weighted log-likelihood of lambda under normality of the transformed values.
transform_loglik <- function(x, lambda, type, w) {
  y <- transform_values(x, lambda, type)
  m <- sum(w * y) / sum(w)
  v <- sum(w * (y - m)^2) / sum(w)
  if (!is.finite(v) || v <= 0) {
    return(-Inf)
  }
  jacobian <- if (type == "BC") log(x) else sign(x) * log1p(abs(x))
  -sum(w) / 2 * log(v) + (lambda - 1) * sum(w * jacobian)
}

# Fits one transformation (BC or YJ) to a pre-standardized variable.
fit_one_transformation <- function(x, type, quantile, nbsteps) {
  quartiles <- stats::quantile(x, c(0.25, 0.75), names = FALSE)
  initial <- function(lambda) normality_objective(rectified_transform(x, lambda, type, quartiles))
  grid <- seq(transformation_range[1], transformation_range[2], by = 0.25)
  values <- vapply(grid, initial, numeric(1))
  best <- grid[which.min(values)]
  lambda <- stats::optimize(initial, c(max(transformation_range[1], best - 0.25),
                                       min(transformation_range[2], best + 0.25)))$minimum
  cutoff <- sqrt(stats::qchisq(quantile, 1))
  # the first weights come from the rectified transformation, which keeps
  # the outliers on the compressed side far out
  y <- rectified_transform(x, lambda, type, quartiles)
  for (step in seq_len(nbsteps)) {
    w <- as.numeric(abs(huber_standardize(y)) <= cutoff)
    lambda <- stats::optimize(function(l) transform_loglik(x, l, type, w), transformation_range,
                              maximum = TRUE)$maximum
    y <- transform_values(x, lambda, type)
  }
  w <- abs(huber_standardize(y)) <= cutoff
  list(lambda = lambda, type = type, mean = mean(y[w]), sd = stats::sd(y[w]),
       objective = normality_objective(y))
}

# Fits the transformation of each column of a numeric matrix.
robust_transformation <- function(x, type = "bestObj", quantile = 0.99, nbsteps = 2,
                                  standardize = TRUE) {
  x <- as_numeric_matrix(x, "x")
  fits <- lapply(seq_len(ncol(x)), function(j) {
    v <- x[, j]
    v <- v[!is.na(v)]
    none <- list(lambda = 1, type = "none", center = 0, scale = 1, mean = 0, sd = 1,
                 objective = NA_real_)
    if (length(v) < 5 || stats::mad(v) <= 0) {
      return(none)
    }
    positive <- all(v > 0)
    types <- switch(type, BC = if (positive) "BC" else character(), YJ = "YJ",
                    bestObj = if (positive) c("BC", "YJ") else "YJ")
    if (length(types) == 0) {
      return(none)
    }
    candidates <- lapply(types, function(t) {
      center <- if (t == "BC") 0 else stats::median(v)
      scale <- if (t == "BC") stats::median(v) else stats::mad(v)
      fit <- fit_one_transformation((v - center) / scale, t, quantile, nbsteps)
      c(fit, list(center = center, scale = scale))
    })
    candidates[[which.min(vapply(candidates, `[[`, numeric(1), "objective"))]]
  })
  names(fits) <- colnames(x)
  if (type == "BC") {
    warn_nonpositive(x)
  }
  structure(list(fits = fits, standardize = standardize), variables = colnames(x),
            nvar = ncol(x), class = "specproc_transformation")
}

# Box-Cox leaves the variables with zero or negative values unchanged.
warn_nonpositive <- function(x) {
  nonpositive <- which(apply(x, 2, function(v) any(v <= 0, na.rm = TRUE)))
  if (length(nonpositive) == 0) {
    return(invisible())
  }
  nms <- colnames(x)[nonpositive] %||% paste0("V", nonpositive)
  shown <- paste(utils::head(nms, 5), collapse = ", ")
  if (length(nms) > 5) shown <- paste0(shown, ", ...")
  warning("The Box-Cox transformation needs strictly positive values; ", length(nms),
          " variable(s) with zero or negative values are left unchanged (", shown,
          "). Use the Yeo-Johnson transformation for them.", call. = FALSE)
}

# Applies fitted transformations to the columns of a numeric matrix.
apply_transformation <- function(x, transformation) {
  x <- as_numeric_matrix(x, "x")
  fits <- transformation$fits
  out <- x
  for (j in seq_along(fits)) {
    f <- fits[[j]]
    if (f$type == "none") next
    v <- (x[, j] - f$center) / f$scale
    if (f$type == "BC") v[!is.na(v) & v <= 0] <- NA_real_
    y <- transform_values(v, f$lambda, f$type)
    out[, j] <- if (transformation$standardize) (y - f$mean) / f$sd else y
  }
  out
}

# Fits robust_boxcox() (type "BC") or robust_yeojohnson() (type "YJ") and
# applies the transformation to the data.
robust_power <- function(x, type, quantile, nbsteps, standardize) {
  x <- as_numeric_matrix(x, "x")
  check_transformation_args(quantile, nbsteps, standardize)
  if (is.null(colnames(x))) {
    colnames(x) <- paste0("V", seq_len(ncol(x)))
  }
  fit <- robust_transformation(x, type = type, quantile = quantile, nbsteps = nbsteps,
                               standardize = standardize)
  res <- list(
    transformation = as_tbl(apply_transformation(x, fit)),
    summary = transformation_summary(fit),
    fits = fit$fits,
    standardize = standardize
  )
  structure(res, variables = colnames(x), nvar = ncol(x),
            class = c(if (type == "BC") "specproc_boxcox" else "specproc_yeojohnson",
                      "specproc_transformation"))
}

check_transformation_args <- function(quantile, nbsteps, standardize) {
  check_number(quantile, "quantile", 0, 1, lower_open = TRUE, upper_open = TRUE)
  check_count(nbsteps, "nbsteps")
  check_flag(standardize, "standardize")
}

# The lambda of each variable (NA for those left unchanged) and the objective.
transformation_lambda <- function(fits) {
  unname(vapply(fits, function(f) if (f$type == "none") NA_real_ else f$lambda, numeric(1)))
}

transformation_summary <- function(fit) {
  tibble::tibble(
    variable = names(fit$fits),
    lambda = transformation_lambda(fit$fits),
    objective = unname(vapply(fit$fits, `[[`, numeric(1), "objective"))
  )
}

#' @export
print.specproc_transformation <- function(x, ...) {
  title <- if (inherits(x, "specproc_boxcox")) {
    "Robust Box-Cox transformation"
  } else if (inherits(x, "specproc_yeojohnson")) {
    "Robust Yeo-Johnson transformation"
  } else {
    "Robust Box-Cox/Yeo-Johnson transformation"
  }
  lambda <- transformation_lambda(x$fits)
  done <- !is.na(lambda)
  cat(title, "\n\n", sep = "")
  cat("Variables:     ", length(lambda), "\n", sep = "")
  cat("Transformed:   ", sum(done),
      if (any(!done)) paste0(" (", sum(!done), " left unchanged)"), "\n", sep = "")
  if (any(done)) {
    rng <- vapply(range(lambda[done]), format, character(1), digits = 3)
    cat("Lambda:        ", if (sum(done) == 1) rng[1] else paste(rng, collapse = " to "),
        "\n", sep = "")
  }
  cat("Standardized:  ", if (isTRUE(x$standardize)) "yes" else "no", "\n", sep = "")
  cat("\nUse predict(<transformation>, newdata) to transform new data.\n")
  invisible(x)
}

#' @title Apply a Robust Transformation to New Data, or Back-Transform
#'
#' @description
#' Transforms new data with the \eqn{\lambda} (and, with `standardize =
#' TRUE`, the mean and standard deviation of the inliers) estimated by
#' [robust_boxcox()] or [robust_yeojohnson()]. With `inverse = TRUE`, it
#' transforms values back to the original scale, such as the predictions of
#' a model fitted to the transformed variables.
#'
#' @details
#' `newdata` can hold any of the variables of the fit, matched by name. A
#' matrix or vector without column names must have all of them, in their
#' order.
#'
#' The back-transformation undoes, in reverse order, the standardization,
#' the transformation and the pre-standardization of each variable. When a
#' model predicts \eqn{\hat{z}} on the transformed scale with symmetric
#' errors, the back-transformed value \eqn{g^{-1}(\hat{z})} estimates the
#' median of the variable on the original scale (`estimate = "median"`, the
#' default). It needs no other assumption, and suits the prediction of one
#' sample.
#'
#' On the original scale, the mean is higher than the median when
#' \eqn{\lambda < 1} (the transformation compresses the upper tail), and
#' lower when \eqn{\lambda > 1}. `estimate = "mean"` gives the smearing
#' estimate of Duan (1983),
#' \deqn{\hat{x} = \frac{1}{n} \sum_{k=1}^{n} g^{-1}(\hat{z} + e_k),}
#' where \eqn{e_1, \dots, e_n} are the `residuals` of the model on the
#' transformed scale (preferably cross-validated, since the residuals of the
#' fit underestimate the errors of prediction). It assumes no distribution
#' of the errors, only that they do not depend on \eqn{\hat{z}}. The mean
#' matters when the predictions are averaged or summed, such as for the mean
#' content of a lot or a mass balance. It is not robust: an outlying
#' residual pulls it, so pass the residuals of the inliers of a robust model
#' (for example, of the spectra that [rsimpls()] does not flag).
#'
#' The back-transformation is not defined everywhere, because some
#' transformations are bounded: the Box-Cox transformation above for
#' \eqn{\lambda < 0}, and the Yeo-Johnson transformation above for
#' \eqn{\lambda < 0} and below for \eqn{\lambda > 2}. Values beyond the
#' bound (with `estimate = "mean"`, values that pass it with one of the
#' residuals) become `NA`, with a warning.
#'
#' @param object An object returned by [robust_boxcox()] or
#'   [robust_yeojohnson()].
#' @param newdata A numeric vector, matrix or data frame with some or all of
#'   the variables of the fit: new data, or with `inverse = TRUE`, values on
#'   the transformed scale.
#' @param inverse A logical value: transform the values back to the
#'   original scale (`TRUE`), or transform new data (`FALSE`, default).
#' @param estimate With `inverse = TRUE`, the estimate on the original scale:
#'   `"median"` (default) or `"mean"`, which needs `residuals`.
#' @param residuals With `estimate = "mean"`, the residuals of the model on
#'   the transformed scale: a numeric vector, used for every variable of
#'   `newdata`, or a matrix or data frame with a column for each of them,
#'   matched by name (without column names, in the order of the columns of
#'   `newdata`). Missing values are ignored.
#' @param ... Not used.
#'
#' @return A tibble with the variables of `newdata`, in the order of the
#'   fit. Missing values stay missing, and with the Box-Cox transformation,
#'   new values that are zero or negative become `NA`.
#'
#' @references
#'  - Duan, N. (1983). Smearing estimate: a nonparametric retransformation
#'    method. Journal of the American Statistical Association,
#'    78(383):605-610.
#'
#' @seealso [robust_boxcox()], [robust_yeojohnson()]
#' @export
#'
#' @examples
#' data(forageLIBS)
#' contents <- forageLIBS[c("Ca", "K", "P", "Mn")]
#' fit <- robust_boxcox(contents[1:300, ])
#' new <- predict(fit, contents[301:368, ])
#' # back to mg/kg
#' head(predict(fit, new, inverse = TRUE))
#' # Box-Cox is not defined for zero or negative values
#' x <- contents[301:302, ]
#' x$Ca[2] <- 0
#' predict(fit, x)
#'
#' # a calibration of the transformed Mn contents on the Mn II line at
#' # 257.61 nm, back to mg/kg
#' wl <- suppressWarnings(as.numeric(names(forageLIBS)))
#' line <- forageLIBS[[which.min(abs(wl - 257.61))]]
#' fit <- robust_boxcox(forageLIBS[1:300, "Mn", drop = FALSE])
#' model <- lm(Mn ~ log(line), data = data.frame(Mn = fit$transformation$Mn,
#'                                                line = line[1:300]))
#' pred <- data.frame(Mn = predict(model, data.frame(line = line[301:368])))
#' med <- predict(fit, pred, inverse = TRUE)
#' avg <- predict(fit, pred, inverse = TRUE, estimate = "mean",
#'                residuals = residuals(model))
#' summary(avg$Mn / med$Mn)
#' # the mean content of the 68 samples
#' c(median = mean(med$Mn), mean = mean(avg$Mn),
#'   reference = mean(forageLIBS$Mn[301:368]))
predict.specproc_transformation <- function(object, newdata, inverse = FALSE,
                                            estimate = c("median", "mean"), residuals = NULL,
                                            ...) {
  if (missing(newdata)) {
    stop("Missing 'newdata' argument.")
  }
  check_flag(inverse, "inverse")
  estimate <- match.arg(estimate)
  if (!inverse && (estimate == "mean" || !is.null(residuals))) {
    stop("'estimate' and 'residuals' are used only with inverse = TRUE.")
  }
  if (estimate == "mean" && is.null(residuals)) {
    stop("estimate = \"mean\" needs the 'residuals' of the model, on the transformed scale.")
  }
  if (estimate == "median" && !is.null(residuals)) {
    stop("'residuals' are used only with estimate = \"mean\".")
  }
  x <- transformation_newdata(object, newdata)
  fits <- object$fits[colnames(x)]
  if (!inverse) {
    return(as_tbl(apply_transformation(x, list(fits = fits, standardize = object$standardize))))
  }
  if (estimate == "mean") {
    # unnamed residuals follow the columns of newdata, as given
    given <- colnames(as_numeric_matrix(newdata, "newdata")) %||% colnames(x)
    res <- transformation_residuals(residuals, given)
  }
  out <- x
  for (v in colnames(x)) {
    out[, v] <- if (estimate == "median") {
      invert_values(x[, v], fits[[v]], object$standardize)
    } else {
      smear_values(x[, v], res[[v]], fits[[v]], object$standardize)
    }
  }
  undefined <- sum(is.na(out) & !is.na(x))
  if (undefined > 0) {
    warning(undefined, if (estimate == "median") {
      " value(s) are beyond the range of the transformation and have no back-transformation"
    } else {
      " value(s) have no mean: with some of the residuals, they are beyond the range of the transformation"
    }, "; they are NA.", call. = FALSE)
  }
  as_tbl(out)
}

# The columns of new data, a subset of the variables of the fit (all of them,
# in order, without column names), in the order of the fit.
transformation_newdata <- function(object, newdata) {
  x <- as_numeric_matrix(newdata, "newdata")
  vars <- attr(object, "variables")
  nms <- colnames(x)
  if (is.null(nms)) {
    if (ncol(x) != length(vars)) {
      stop("'newdata' without column names must have ", length(vars),
           " column(s), like the data of the fit.", call. = FALSE)
    }
    colnames(x) <- vars
    return(x)
  }
  if (anyDuplicated(nms)) {
    stop("'newdata' has duplicated column names.", call. = FALSE)
  }
  unknown <- setdiff(nms, vars)
  if (length(unknown) > 0) {
    stop("The columns of 'newdata' must be variables of the fit; not found: ",
         paste(utils::head(unknown, 5), collapse = ", "), if (length(unknown) > 5) ", ...",
         ".", call. = FALSE)
  }
  x[, intersect(vars, nms), drop = FALSE]
}

# The residuals of each variable, from a vector (the same for all) or a
# matrix or data frame with a column per variable.
transformation_residuals <- function(residuals, vars) {
  if (is.numeric(residuals) && length(dim(residuals)) < 2) {
    res <- rep(list(as.vector(residuals)), length(vars))
  } else {
    r <- as_numeric_matrix(residuals, "residuals")
    if (is.null(colnames(r))) {
      if (ncol(r) != length(vars)) {
        stop("'residuals' without column names must have one column per variable of 'newdata'.",
             call. = FALSE)
      }
      colnames(r) <- vars
    }
    missing_vars <- setdiff(vars, colnames(r))
    if (length(missing_vars) > 0) {
      stop("'residuals' has no column for: ", paste(missing_vars, collapse = ", "), ".",
           call. = FALSE)
    }
    res <- lapply(vars, function(v) r[, v])
  }
  res <- lapply(res, function(e) e[!is.na(e)])
  if (any(lengths(res) == 0)) {
    stop("'residuals' has no values.", call. = FALSE)
  }
  stats::setNames(res, vars)
}

# Back-transforms the values of one variable: undoes the standardization,
# the transformation and the pre-standardization.
invert_values <- function(z, f, standardize) {
  if (f$type == "none") {
    return(z)
  }
  y <- if (standardize) z * f$sd + f$mean else z
  v <- if (f$type == "BC") bc_inverse(y, f$lambda) else yj_inverse(y, f$lambda)
  v * f$scale + f$center
}

# Smearing estimate of the mean (Duan, 1983): the mean of the
# back-transformed values plus each residual; NA when one of them is not
# defined.
smear_values <- function(z, e, f, standardize) {
  grid <- outer(z, e, `+`)
  rowMeans(matrix(invert_values(as.vector(grid), f, standardize), nrow = length(z)))
}
