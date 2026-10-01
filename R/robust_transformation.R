# Robust Box-Cox and Yeo-Johnson transformations to central normality, after
# Raymaekers and Rousseeuw (2021). Used by robust_bcyj() and
# step_robust_bcyj(). Not exported.
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

# Inverse transformations (on the domain of the transformation).
bc_inverse <- function(y, lambda) {
  if (abs(lambda) < 1e-8) exp(y) else (lambda * y + 1)^(1 / lambda)
}

yj_inverse <- function(y, lambda) {
  if (y >= 0) {
    if (abs(lambda) < 1e-8) expm1(y) else (lambda * y + 1)^(1 / lambda) - 1
  } else {
    if (abs(lambda - 2) < 1e-8) -expm1(-y) else 1 - (1 - (2 - lambda) * y)^(1 / (2 - lambda))
  }
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
  structure(list(fits = fits, standardize = standardize), class = "specproc_transformation")
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
