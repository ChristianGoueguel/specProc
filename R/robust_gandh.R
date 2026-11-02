#' @title Robust Tukey g-and-h Transformation
#'
#' @description
#' Transforms each variable toward central normality by inverting a Tukey
#' g-and-h distribution fitted by robust quantile least squares (rQLS;
#' Xu, Iglewicz and Chervoneva, 2014). Accepts positive, zero and negative
#' values, and fits skewness and tail heaviness separately.
#'
#' @details
#' The fitted distribution maps a standard normal value \eqn{z} to
#' \deqn{x = A + B \frac{\exp(gz)-1}{g}\exp(hz^2/2),}
#' with \eqn{B > 0} and \eqn{h \ge 0}. At \eqn{g = 0}, the fraction is
#' \eqn{z}. The transformation solves this equation for \eqn{z} directly,
#' without rounding extreme cumulative probabilities to zero or one.
#' The inverse transformation evaluates the equation.
#'
#' Each variable is first centered by its median and divided by its MAD.
#' QLS fits model quantiles to sample order statistics at probabilities
#' \eqn{p_j = (j-1/3)/(m+1/3)}. Initialization compares this QLS fit with
#' a second QLS fit using equally spaced probabilities from 0.15 to 0.85,
#' selecting the smaller median absolute order-statistic discrepancy. This
#' additional start reduces masking when contamination reaches an outer
#' fitting quantile. The rQLS iterations compare all order
#' statistics with their fitted quantiles, calculate Tukey biweight weights,
#' and refit QLS to quantiles of the observations with nonzero weights.
#' The positive weights are used for selection, not as quantile weights.
#' All original observations, including the trimmed ones, are transformed.
#'
#' With `cutoff = NULL`, a bounded numerical version of the paper's adaptive
#' search is used: starting at least at 10 on the standardized input scale,
#' a geometric pilot search brackets the cutoffs retaining
#' more than half and all observations, followed by a descending grid with
#' the paper's grading interval and tail-weight ordering checks. At most
#' 25 pilot and 51 grading fits are made. If no candidate meets the ordering
#' checks, the largest converged candidate retaining more than half the
#' observations is used, with a warning and `tuning_met = FALSE`. A supplied
#' cutoff bypasses this search. This numerical search need not give exactly
#' the same cutoff as the authors' implementation.
#'
#' With `standardize = TRUE`, latent normal scores are additionally centered
#' and scaled by the mean and standard deviation of the fitting inliers.
#' With `FALSE`, the fitted latent scores are returned directly; they already
#' account for the distribution's location and scale.
#'
#' Variables with fewer than `max(10, n_quantiles + 1)` observed values or
#' zero MAD are left unchanged. Failed fits are left unchanged with a
#' warning. Missing values are ignored by fitting and stay missing. Infinite
#' input values are rejected. This is a continuous-distribution model;
#' substantial ties, mixtures and censoring need separate consideration.
#' For \eqn{h = 0, g \ne 0}, new values outside the fitted distribution's
#' support become `NA` with a warning. Inverse values exceeding floating
#' point range also become `NA` with a warning.
#'
#' @param x A numeric vector, matrix, data frame or tibble, one variable per
#'   column.
#' @param n_quantiles Number of fitting quantiles, an integer from 4 to 20.
#'   Default is 10, as recommended for rQLS in the paper.
#' @param cutoff Positive, finite Tukey biweight cutoff on the median/MAD
#'   standardized input scale. `NULL` (default) selects it adaptively for
#'   each variable. It is an order-statistic fitting tolerance, not a normal
#'   score cutoff or a contamination percentage.
#' @param max_iter Maximum number of trimming/refitting iterations per
#'   cutoff. Default is 50.
#' @param tol Positive, finite relative tolerance for convergence. Default
#'   is 1e-6.
#' @param standardize Logical: additionally standardize latent scores using
#'   the fitting inliers (`TRUE`, default).
#'
#' @return An object of class `specproc_gandh`, inheriting from
#'   `specproc_transformation`, with the same components as [robust_boxcox()]:
#'  - `transformation`: A tibble of transformed values.
#'  - `summary`: A tibble with `variable`, `location`, `scale` (both on the
#'    original input scale), `g`, `h`, `objective` (quantile mean squared
#'    error on the median/MAD input scale), `n_trimmed`, `cutoff` (on the
#'    standardized input scale), `converged` and `tuning_met`. Parameters
#'    and diagnostics are `NA` for variables left unchanged. `tuning_met`
#'    is `NA` when a cutoff was supplied.
#'  - `fits`: Parameters and fitting diagnostics used by `predict()`.
#'  - `standardize`: The `standardize` argument.
#'
#' @references
#' Xu, Y., Iglewicz, B., Chervoneva, I. (2014). Robust estimation of the
#' parameters of g-and-h distributions, with applications to outlier
#' detection. Computational Statistics & Data Analysis, 75:66-80.
#' \doi{10.1016/j.csda.2014.01.003}.
#'
#' @seealso [robust_boxcox()], [robust_yeojohnson()], [tukey_gh()],
#'   [predict.specproc_transformation()], [step_robust_gandh()]
#' @author Christian L. Goueguel
#' @export
#'
#' @examples
#' data(forageLIBS)
#' contents <- forageLIBS[c("Ca", "K", "P", "Mn")]
#' fit <- robust_gandh(contents[1:300, ])
#' fit$summary
#' new <- predict(fit, contents[301:368, ])
#' head(predict(fit, new, inverse = TRUE))
robust_gandh <- function(x, n_quantiles = 10, cutoff = NULL, max_iter = 50,
                         tol = 1e-6, standardize = TRUE) {
  if (missing(x)) stop("Missing 'x' argument.")
  check_gandh_args(n_quantiles, cutoff, max_iter, tol, standardize)
  x <- as_numeric_matrix(x, "x")
  if (any(is.infinite(x))) stop("'x' must not contain infinite values.", call. = FALSE)
  if (is.null(colnames(x))) colnames(x) <- paste0("V", seq_len(ncol(x)))
  if (anyDuplicated(colnames(x))) stop("'x' has duplicated column names.", call. = FALSE)
  fits <- lapply(seq_len(ncol(x)), function(j) {
    v <- x[, j]
    v <- v[!is.na(v)]
    none <- list(type = "none")
    if (length(v) < max(10, n_quantiles + 1) || stats::mad(v) <= 0) return(none)
    center <- stats::median(v)
    scale <- stats::mad(v)
    fit <- fit_gandh((v - center) / scale, n_quantiles, cutoff, max_iter, tol)
    if (is.null(fit)) {
      warning("g-and-h fit failed for '", colnames(x)[j], "'; left unchanged.", call. = FALSE)
      return(none)
    }
    if (isFALSE(fit$tuning_met)) {
      warning("Adaptive g-and-h cutoff for '", colnames(x)[j],
              "' did not meet the tail-weight checks; inspect 'summary' or supply 'cutoff'.",
              call. = FALSE)
    }
    c(list(type = "GH", center = center, scale = scale), fit)
  })
  names(fits) <- colnames(x)
  fit <- list(fits = fits, standardize = standardize)
  res <- list(transformation = as_tbl(apply_transformation(x, fit)),
              summary = gandh_summary(fits), fits = fits, standardize = standardize)
  structure(res, variables = colnames(x), nvar = ncol(x),
            class = c("specproc_gandh", "specproc_transformation"))
}

check_gandh_args <- function(n_quantiles, cutoff, max_iter, tol, standardize) {
  if (is.numeric(n_quantiles) && any(!is.finite(n_quantiles))) {
    stop("'n_quantiles' must be a finite integer from 4 to 20.", call. = FALSE)
  }
  check_count(n_quantiles, "n_quantiles", 4)
  if (n_quantiles > 20) stop("'n_quantiles' must be <= 20.", call. = FALSE)
  if (!is.null(cutoff)) check_number(cutoff, "cutoff", 0, Inf, TRUE, TRUE)
  if (is.numeric(max_iter) && any(!is.finite(max_iter))) {
    stop("'max_iter' must be a finite positive integer.", call. = FALSE)
  }
  check_count(max_iter, "max_iter")
  check_number(tol, "tol", 0, Inf, TRUE, TRUE)
  check_flag(standardize, "standardize")
}

# Normal scores -> input values. expm1 and the g=0 limit preserve accuracy
# near the normal submodel. Used by both QLS and inverse prediction.
gandh_values <- function(z, A, B, g, h) {
  v <- if (g == 0) z else expm1(g * z) / g
  A + B * v * exp(h * z^2 / 2)
}

# Invert on the log absolute scale. This avoids overflow while bracketing
# roots and avoids the precision loss of qnorm(pnorm(z)) in extreme tails.
gandh_scores <- function(x, A, B, g, h) {
  y <- (x - A) / B
  out <- rep(NA_real_, length(y))
  ok <- which(!is.na(y) & is.finite(y))
  if (h == 0) {
    if (g == 0) out[ok] <- y[ok] else {
      good <- ok[1 + g * y[ok] > 0]
      out[good] <- log1p(g * y[good]) / g
      overflow <- good[is.infinite(g * y[good])]
      out[overflow] <- (log(abs(g)) + log(abs(y[overflow]))) / g
    }
    return(out)
  }
  out[ok[y[ok] == 0]] <- 0
  ok <- ok[y[ok] != 0]
  side <- sign(y[ok])
  target <- log(abs(y[ok]))
  # log|value| - log|y| at |z| = t, increasing in t for h > 0.
  f <- function(t) {
    first <- if (g == 0) log(t) else {
      u <- g * side * t
      v <- log(abs(expm1(u)))
      big <- u > 50
      v[big] <- u[big] + log1p(-exp(-u[big]))
      v - log(abs(g))
    }
    first + h * t^2 / 2 - target
  }
  # Bracket and bisect all roots together, instead of one uniroot() per value.
  lower <- numeric(length(ok))
  upper <- rep(1, length(ok))
  while (any(low <- f(upper) < 0)) {
    lower[low] <- upper[low]
    upper[low] <- 2 * upper[low]
  }
  repeat {
    mid <- (lower + upper) / 2
    below <- f(mid) < 0
    lower[below] <- mid[below]
    upper[!below] <- mid[!below]
    if (all(upper - lower <= 1e-12 * pmax(1, upper))) break
  }
  out[ok] <- side * (lower + upper) / 2
  out
}

gandh_probs <- function(n) (seq_len(n) - 1 / 3) / (n + 1 / 3)

# The paper uses x_(ceiling(n*p)), i.e. empirical inverse CDF (type 1).
gandh_quantiles <- function(sorted, p) sorted[pmax(1L, ceiling(length(sorted) * p))]

gandh_qls <- function(sorted, p, start = NULL) {
  q <- gandh_quantiles(sorted, p)
  if (length(unique(q)) < 4) return(NULL)
  z <- stats::qnorm(p)
  if (is.null(start)) {
    mid <- stats::median(sorted)
    qq <- stats::quantile(sorted, c(0.1, 0.9), names = FALSE)
    g <- if (qq[1] < mid && mid < qq[2]) log((qq[2] - mid) / (mid - qq[1])) / stats::qnorm(0.9) else 0
    start <- c(mid, log(stats::mad(sorted)), g, 0.05)
  }
  residual <- function(par) {
    pred <- suppressWarnings(gandh_values(z, par[1], exp(par[2]), par[3], par[4]))
    r <- q - pred
    if (any(!is.finite(r)) || any(abs(r) > 1e50)) rep(1e50, length(q)) else r
  }
  # Analytic derivatives are important after median/MAD scaling: A and
  # log(B) can be almost zero, where relative finite differences vanish.
  jacobian <- function(par) {
    g <- par[3]
    u <- g * z
    v <- if (g == 0) z else expm1(u) / g
    dg <- if (max(abs(u)) < 1e-4) {
      z^2 / 2 + g * z^3 / 3 + g^2 * z^4 / 8 + g^3 * z^5 / 30
    } else (u * exp(u) - expm1(u)) / g^2
    factor <- exp(par[2] + par[4] * z^2 / 2)
    -cbind(1, factor * v, factor * dg, factor * v * z^2 / 2)
  }
  fit <- tryCatch(minpack.lm::nls.lm(
    par = start, lower = c(-Inf, -Inf, -Inf, 0), fn = residual, jac = jacobian,
    control = minpack.lm::nls.lm.control(maxiter = 200, ftol = 1e-10, ptol = 1e-10)
  ), error = function(e) NULL, warning = function(w) NULL)
  if (is.null(fit) || !fit$info %in% 1:4 || any(!is.finite(fit$par)) ||
      !is.finite(exp(fit$par[2])) || exp(fit$par[2]) <= 0) return(NULL)
  # On the h=0 boundary, clipping a four-parameter LM step can stop before
  # the other three parameters have reached their optimum. Solve that
  # submodel explicitly, instead of slowly drifting on successive refits.
  if (fit$par[4] < 1e-6) {
    boundary <- tryCatch(minpack.lm::nls.lm(
      par = fit$par[1:3], fn = function(par) residual(c(par, 0)),
      jac = function(par) jacobian(c(par, 0))[, 1:3, drop = FALSE],
      control = minpack.lm::nls.lm.control(maxiter = 200, ftol = 1e-10, ptol = 1e-10)
    ), error = function(e) NULL, warning = function(w) NULL)
    if (!is.null(boundary) && boundary$info %in% 1:4 &&
        sum(residual(c(boundary$par, 0))^2) <= sum(residual(fit$par)^2)) {
      fit$par <- c(boundary$par, 0)
    }
  }
  list(par = fit$par, objective = mean(residual(fit$par)^2))
}

gandh_weights <- function(sorted, z, par, cutoff) {
  r <- sorted - gandh_values(z, par[1], exp(par[2]), par[3], par[4])
  u <- pmin(abs(r) / cutoff, 1)
  (1 - u^2)^2
}

# One complete rQLS fit at a fixed cutoff. More than half the observations
# must remain, and convergence includes a stable retained subset.
gandh_rqls <- function(sorted, p, z, initial, cutoff, max_iter, tol) {
  fit <- initial
  previous <- rep(TRUE, length(sorted))
  for (i in seq_len(max_iter)) {
    weights <- gandh_weights(sorted, z, fit$par, cutoff)
    keep <- is.finite(weights) & weights > 0
    if (sum(keep) <= max(length(sorted) / 2, length(p))) return(NULL)
    next_fit <- gandh_qls(sorted[keep], p, fit$par)
    if (is.null(next_fit)) return(NULL)
    change <- max(abs(next_fit$par - fit$par) / (1 + abs(fit$par)))
    stable <- identical(keep, previous)
    fit <- next_fit
    previous <- keep
    final_weights <- gandh_weights(sorted, z, fit$par, cutoff)
    if (stable && change <= tol && identical(keep, is.finite(final_weights) & final_weights > 0)) {
      return(c(fit, list(weights = final_weights, cutoff = cutoff, iterations = i)))
    }
  }
  NULL
}

# Tail ordering of Section 2.2. An entirely high-weight half has no weak
# tail to order, and is accepted explicitly (empty-index case).
gandh_tail_weights <- function(w) {
  if (all(w >= 0.8)) return(TRUE)
  n <- length(w)
  left <- w[seq_len(floor(n / 2))]
  right <- rev(w[seq.int(floor(n / 2) + 1, n)])
  side_ok <- function(v) {
    if (all(v >= 0.8)) return(TRUE)
    weak <- which(v < 0.7)
    strong <- which(v > 0.8)
    length(weak) > 0 && length(strong) > 0 && max(weak) < min(strong) && v[1] == min(v)
  }
  side_ok(left) && side_ok(right)
}

fit_gandh <- function(x, n_quantiles, cutoff, max_iter, tol) {
  if (any(!is.finite(x))) return(NULL)
  ord <- order(x)
  sorted <- x[ord]
  p <- gandh_probs(n_quantiles)
  z <- stats::qnorm(gandh_probs(length(sorted)))
  full <- gandh_qls(sorted, p)
  central <- gandh_qls(sorted, seq(0.15, 0.85, length.out = n_quantiles))
  candidates <- Filter(Negate(is.null), list(full, central))
  if (length(candidates) == 0) return(NULL)
  discrepancy <- function(fit) {
    stats::median(abs(sorted - gandh_values(z, fit$par[1], exp(fit$par[2]),
                                            fit$par[3], fit$par[4])))
  }
  initial <- candidates[[which.min(vapply(candidates, discrepancy, numeric(1)))]]
  tuning_met <- NA
  if (!is.null(cutoff)) {
    fit <- gandh_rqls(sorted, p, z, initial, cutoff, max_iter, tol)
  } else {
    error <- abs(sorted - gandh_values(z, initial$par[1], exp(initial$par[2]),
                                       initial$par[3], initial$par[4]))
    if (any(!is.finite(error))) return(NULL)
    # A coarse all-retained pilot, as allowed in the paper, is preferable
    # to max(error) exactly: halving an exact maximum forces trimming even
    # in a clean sample. The scale here is already median/MAD standardized.
    b <- max(10, max(error) * (1 + 1e-8))
    fit <- NULL
    fallback <- NULL
    # Exact/near-exact quantile data need no robustification.
    tuning_met <- max(error) <= tol
    if (tuning_met) fit <- gandh_rqls(sorted, p, z, initial, b, max_iter, tol)
    if (!tuning_met) {
      a <- b
      for (trial in b * 0.7^seq_len(25)) {
        candidate <- gandh_rqls(sorted, p, z, initial, trial, max_iter, tol)
        if (is.null(candidate)) next
        if (is.null(fallback)) fallback <- candidate
        a <- trial
      }
      # A finite pilot search replaces the paper's manually chosen trials.
      if (a < b / 2) {
        interval <- 10^(floor(log10(b / 2)) - 1)
        grid <- b / 2 - seq.int(0, min(50, floor((b / 2 - a) / interval))) * interval
        for (trial in grid) {
          candidate <- gandh_rqls(sorted, p, z, initial, trial, max_iter, tol)
          if (is.null(candidate)) next
          if (is.null(fallback)) fallback <- candidate
          if (gandh_tail_weights(candidate$weights)) {
            fit <- candidate
            tuning_met <- TRUE
            break
          }
        }
      }
      if (!tuning_met) fit <- fallback
      # If all trimming candidates cycle or fail, a converged all-retained
      # fit is still reported explicitly as failing the adaptive checks.
      if (is.null(fit) && !is.null(full)) {
        err <- abs(sorted - gandh_values(z, full$par[1], exp(full$par[2]),
                                         full$par[3], full$par[4]))
        if (all(is.finite(err))) {
          fit <- gandh_rqls(sorted, p, z, full, max(10, 1.01 * max(err)), max_iter, tol)
        }
      }
    }
  }
  if (is.null(fit)) return(NULL)
  par <- fit$par
  scores <- gandh_scores(sorted, par[1], exp(par[2]), par[3], par[4])
  keep <- fit$weights > 0
  if (any(!is.finite(scores[keep]))) return(NULL)
  mu <- mean(scores[keep])
  sd <- stats::sd(scores[keep])
  if (!is.finite(sd) || sd <= 0) return(NULL)
  weights <- fit$weights[order(ord)]
  list(A = unname(par[1]), B = unname(exp(par[2])), g = unname(par[3]), h = unname(par[4]),
       mean = mu, sd = sd, objective = fit$objective, n_trimmed = sum(!keep),
       cutoff = fit$cutoff, iterations = fit$iterations, converged = TRUE,
       tuning_met = tuning_met, weights = weights)
}

gandh_summary <- function(fits) {
  value <- function(fun, missing = NA_real_) unname(vapply(fits, function(f) {
    if (f$type == "none") missing else fun(f)
  }, missing))
  tibble::tibble(
    variable = names(fits), location = value(function(f) f$center + f$scale * f$A),
    scale = value(function(f) f$scale * f$B), g = value(function(f) f$g), h = value(function(f) f$h),
    objective = value(function(f) f$objective), n_trimmed = value(function(f) f$n_trimmed),
    cutoff = value(function(f) f$cutoff), converged = value(function(f) f$converged, NA),
    tuning_met = value(function(f) f$tuning_met, NA)
  )
}

print_gandh <- function(x) {
  done <- vapply(x$fits, function(f) f$type != "none", logical(1))
  cat("Robust Tukey g-and-h transformation (rQLS)\n\n")
  cat("Variables:     ", length(done), "\n", sep = "")
  cat("Transformed:   ", sum(done), "\n", sep = "")
  cat("Standardized:  ", if (x$standardize) "yes" else "no", "\n", sep = "")
  cat("\nUse predict(<transformation>, newdata) to transform new data.\n")
  invisible(x)
}
