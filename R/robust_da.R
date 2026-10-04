#' @title Robust Linear and Quadratic Discriminant Analysis
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Robust linear (LDA) and quadratic (QDA) discriminant analysis of Hubert
#' and Van Driessen (2004): the class centers and covariance matrices are
#' estimated by the minimum covariance determinant (MCD) estimator, so that
#' outlying observations of the training data have little influence on the
#' discriminant rules.
#'
#' @details
#' **Quadratic rule.** The center \eqn{\hat\mu_j} and covariance
#' \eqn{\hat\Sigma_j} of each class are its reweighted MCD estimates. An
#' observation \eqn{x} is assigned to the class with the largest discriminant
#' score
#' \deqn{d_j(x) = -\frac{1}{2} \log|\hat\Sigma_j| - \frac{1}{2}
#'   (x - \hat\mu_j)^T \hat\Sigma_j^{-1} (x - \hat\mu_j) + \log p_j.}
#'
#' **Linear rule.** The classes share one covariance matrix: the reweighted
#' MCD of the observations centered by the MCD center of their class, whose
#' own center shifts the class centers. The scores are those of the
#' quadratic rule with this common covariance.
#'
#' The membership (prior) probabilities \eqn{p_j} are, by default, the
#' proportions of the regular observations of the training data in each
#' class: those within the cut-off \eqn{\sqrt{\chi^2_{p, 0.975}}} of the
#' robust distance to the center of their class. The posterior probabilities
#' are proportional to \eqn{p_j} times the normal density of each class.
#'
#' The MCD needs more observations than variables in each class, so robust
#' discriminant analysis applies to low-dimensional data: line intensities
#' or ratios, or the robust principal component scores of spectra
#' ([robpca()], [step_robpca()]). For whole spectra, see [rsimca()].
#'
#' The misclassification rates are estimated on the regular observations of
#' the training data (`misclassification`), which tends to underestimate
#' them: estimate them on test data or by cross-validation (for example with
#' the `"mcd"` engine of [parsnip::discrim_linear()] and
#' [parsnip::discrim_quad()] in tidymodels).
#'
#' This is an independent implementation of the published description. The
#' MCD uses random subsets, so use [set.seed()] for reproducible results.
#'
#' # tidymodels
#'
#' specProc adds the `"mcd"` engine to [parsnip::discrim_linear()] (linear
#' rule) and [parsnip::discrim_quad()] (quadratic rule); `alpha`, `prior` and
#' `nsamp` are engine arguments. The engines are available once parsnip and
#' specProc are both loaded, in either order.
#'
#' @param x A numeric matrix or data frame of the predictors, one
#'   observation per row.
#' @param group The classes of the observations: a factor, or a vector
#'   converted to one.
#' @param method The discriminant rule: `"linear"` (default) or
#'   `"quadratic"`.
#' @param alpha The fraction of observations of each class assumed to be
#'   regular (the MCD subset size), between 0.5 and 1. Default is 0.75.
#' @param prior The membership (prior) probabilities of the classes, in the
#'   order of the levels of `group` (or named by them). Default is the
#'   proportions of the regular training observations in each class.
#' @param nsamp The number of random subsets of the MCD. Default is 500.
#'
#' @return An object of class `specproc_robust_da`, a list with:
#'  - `center`: the robust centers of the classes (one row per class).
#'  - `cov`: the common covariance matrix (linear rule), or a list of the
#'    covariance matrices of the classes (quadratic rule).
#'  - `prior`: the membership probabilities.
#'  - `rd`: the robust distance of each training observation to the center
#'    of its class, and `cutoff_rd` its cut-off.
#'  - `weights`: 1 for the regular training observations, 0 for the
#'    outliers.
#'  - `fitted`: the classes assigned to the training observations.
#'  - `misclassification`: a tibble with the misclassification rate of each
#'    class on its regular training observations, and the overall rate
#'    weighted by the membership probabilities.
#'  - `method`, `levels`, `alpha`.
#'
#' Use [predict()][predict.specproc_robust_da] for the classes and
#' posterior probabilities of new observations.
#'
#' @references
#'  - Hubert, M., Van Driessen, K. (2004). Fast and robust discriminant
#'    analysis. Computational Statistics and Data Analysis, 45(2):301-320.
#'  - Rousseeuw, P.J., Van Driessen, K. (1999). A fast algorithm for the
#'    minimum covariance determinant estimator. Technometrics,
#'    41(3):212-223.
#'
#' @seealso [predict.specproc_robust_da()], [rsimca()]
#' @export
#'
#' @examples
#' # iris: train on 100 flowers, predict the 50 others
#' set.seed(1)
#' train <- sample(nrow(iris), 100)
#' fit <- robust_da(iris[train, 1:4], iris$Species[train])
#' fit
#' table(predicted = predict(fit, iris[-train, 1:4]), true = iris$Species[-train])
#' head(predict(fit, iris[-train, 1:4], type = "prob"))
robust_da <- function(x, group, method = c("linear", "quadratic"), alpha = 0.75, prior = NULL,
                      nsamp = 500) {
  method <- match.arg(method)
  if (missing(x) || missing(group)) {
    stop("Both 'x' and 'group' must be provided.", call. = FALSE)
  }
  x <- as_numeric_matrix(x, "x")
  if (is.null(colnames(x))) colnames(x) <- paste0("x", seq_len(ncol(x)))
  group <- class_factor(group, nrow(x))
  if (anyNA(x)) {
    stop("'x' contains missing values.", call. = FALSE)
  }
  check_number(alpha, "alpha", lower = 0.5, upper = 1)
  check_count(nsamp, "nsamp")
  lev <- levels(group)
  p <- ncol(x)
  sizes <- table(group)
  if (any(sizes <= p)) {
    stop("Each class needs more observations than variables (", p, "): class `",
         names(sizes)[which.min(sizes)], "` has ", min(sizes), ". Use robust principal ",
         "component scores (robpca()) or rsimca() for high-dimensional data.", call. = FALSE)
  }

  mcd <- lapply(lev, function(g) scaled_mcd(x[group == g, , drop = FALSE], alpha, nsamp))
  center <- do.call(rbind, lapply(mcd, `[[`, "center"))
  dimnames(center) <- list(lev, colnames(x))
  idx <- as.integer(group)
  if (method == "linear") {
    pooled <- scaled_mcd(x - center[idx, , drop = FALSE], alpha, nsamp)
    center <- sweep(center, 2, pooled$center, "+")
    cov <- pooled$cov
    dimnames(cov) <- list(colnames(x), colnames(x))
    rd2 <- numeric(nrow(x))
    for (j in seq_along(lev)) {
      rd2[idx == j] <- stats::mahalanobis(x[idx == j, , drop = FALSE], center[j, ], cov)
    }
    weights <- rd2 <= stats::qchisq(0.975, p)
  } else {
    cov <- stats::setNames(lapply(mcd, function(m) {
      dimnames(m$cov) <- list(colnames(x), colnames(x))
      m$cov
    }), lev)
    rd2 <- numeric(nrow(x))
    weights <- logical(nrow(x))
    for (j in seq_along(lev)) {
      in_j <- idx == j
      rd2[in_j] <- stats::mahalanobis(x[in_j, , drop = FALSE], center[j, ], cov[[j]])
      weights[in_j] <- mcd[[j]]$weights
    }
  }
  prior <- class_prior(prior, group, weights)

  res <- list(center = center, cov = cov, prior = prior, rd = sqrt(rd2),
              cutoff_rd = sqrt(stats::qchisq(0.975, p)), weights = as.numeric(weights),
              fitted = NULL, misclassification = NULL, method = method, levels = lev,
              alpha = alpha)
  res <- structure(res, variables = colnames(x), nvar = p, class = "specproc_robust_da")
  res$fitted <- da_classes(res, x)
  res$misclassification <- misclassification_table(res$fitted, group, weights, prior)
  res
}

# The classes of a classification: a factor without missing values, with
# at least two observed levels.
class_factor <- function(group, n) {
  group <- droplevels(as.factor(group))
  if (length(group) != n) {
    stop("'x' and 'group' must have the same number of observations.", call. = FALSE)
  }
  if (anyNA(group)) {
    stop("'group' contains missing values.", call. = FALSE)
  }
  if (nlevels(group) < 2) {
    stop("'group' must have at least two classes.", call. = FALSE)
  }
  group
}

# Membership (prior) probabilities: the given ones (in the order of the
# levels, or named by them), or the proportions of the regular
# observations in each class.
class_prior <- function(prior, group, weights) {
  lev <- levels(group)
  if (is.null(prior)) {
    counts <- table(factor(group[weights], levels = lev))
    if (any(counts == 0)) {
      stop("Class `", names(counts)[counts == 0][1], "` has no regular observation.",
           call. = FALSE)
    }
    return(stats::setNames(as.numeric(counts) / sum(counts), lev))
  }
  if (!is.numeric(prior) || length(prior) != length(lev) || anyNA(prior) || any(prior <= 0)) {
    stop("'prior' must hold one positive probability per class (", length(lev), ").",
         call. = FALSE)
  }
  if (!is.null(names(prior))) {
    if (!setequal(names(prior), lev)) {
      stop("The names of 'prior' must be the classes.", call. = FALSE)
    }
    prior <- prior[lev]
  }
  stats::setNames(as.numeric(prior) / sum(prior), lev)
}

# Discriminant scores (log of prior times normal density, up to a common
# constant) of the rows of x for each class.
da_scores <- function(object, x) {
  lev <- object$levels
  out <- vapply(seq_along(lev), function(j) {
    cov <- if (object$method == "linear") object$cov else object$cov[[j]]
    logdet <- if (object$method == "linear") 0 else {
      determinant(cov, logarithm = TRUE)$modulus[1]
    }
    -0.5 * logdet - 0.5 * stats::mahalanobis(x, object$center[j, ], cov) + log(object$prior[[j]])
  }, numeric(nrow(x)))
  matrix(out, nrow(x), dimnames = list(NULL, lev))
}

da_classes <- function(object, x) {
  factor(object$levels[max.col(da_scores(object, x), ties.method = "first")],
         levels = object$levels)
}

# Misclassification rates of each class on its regular observations, and
# overall, weighted by the membership probabilities.
misclassification_table <- function(fitted, group, weights, prior) {
  lev <- levels(group)
  rate <- vapply(lev, function(g) {
    in_g <- group == g & weights
    mean(fitted[in_g] != g)
  }, numeric(1))
  tibble::tibble(class = c(lev, "overall"),
                 n = c(as.integer(table(factor(group[weights], levels = lev))), sum(weights)),
                 error = c(rate, sum(rate * prior)))
}

#' @title Predictions of a Robust Discriminant Analysis
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Predicts the classes, or the posterior probabilities of the classes, of
#' new observations with a model fitted by [robust_da()].
#'
#' @param object An object returned by [robust_da()].
#' @param newdata A numeric matrix or data frame with the same variables as
#'   the training data.
#' @param type `"class"` (default) for the predicted classes, or `"prob"`
#'   for the posterior probabilities.
#' @param ... Not used.
#'
#' @return With `type = "class"`, a factor of the predicted classes. With
#'   `type = "prob"`, a tibble with one column of posterior probabilities
#'   per class.
#'
#' @seealso [robust_da()]
#' @export
#'
#' @examples
#' # iris: train on 100 flowers, predict the 50 others
#' set.seed(1)
#' train <- sample(nrow(iris), 100)
#' fit <- robust_da(iris[train, 1:4], iris$Species[train], method = "quadratic")
#' head(predict(fit, iris[-train, 1:4]))
#' head(predict(fit, iris[-train, 1:4], type = "prob"))
predict.specproc_robust_da <- function(object, newdata, type = c("class", "prob"), ...) {
  type <- match.arg(type)
  x <- filter_newdata(object, newdata)
  if (anyNA(x)) {
    stop("'newdata' contains missing values.", call. = FALSE)
  }
  if (type == "class") {
    return(da_classes(object, x))
  }
  scores <- da_scores(object, x)
  prob <- exp(scores - apply(scores, 1, max))
  tibble::as_tibble(prob / rowSums(prob))
}

#' @export
print.specproc_robust_da <- function(x, ...) {
  cat("Robust ", if (x$method == "linear") "linear" else "quadratic",
      " discriminant analysis (MCD)\n\n", sep = "")
  cat("Observations:   ", length(x$rd), " (", sum(x$weights == 0), " outliers)\n", sep = "")
  cat("Variables:      ", attr(x, "nvar"), "\n", sep = "")
  cat("Classes:        ", paste(x$levels, collapse = ", "), "\n", sep = "")
  cat("Prior:          ", paste(format(x$prior, digits = 3), collapse = " "), "\n", sep = "")
  cat("\nMisclassification of the regular training observations:\n")
  print(x$misclassification)
  invisible(x)
}

# Registers the "mcd" engines of parsnip::discrim_linear() and
# parsnip::discrim_quad(), once (see register_pls_rsimpls()).
register_discrim_mcd <- function() {
  env <- parsnip::get_model_env()
  for (model in c("discrim_linear", "discrim_quad")) {
    if (!model %in% env$models || "mcd" %in% env[[model]]$engine) next
    parsnip::set_model_engine(model, mode = "classification", eng = "mcd")
    parsnip::set_dependency(model, eng = "mcd", pkg = "specProc", mode = "classification")
    parsnip::set_fit(
      model = model, eng = "mcd", mode = "classification",
      value = list(interface = "matrix", data = c(x = "x", y = "group"),
                   protect = c("x", "group"), func = c(pkg = "specProc", fun = "robust_da"),
                   defaults = list(method = if (model == "discrim_linear") "linear" else "quadratic"))
    )
    parsnip::set_encoding(
      model = model, eng = "mcd", mode = "classification",
      options = list(predictor_indicators = "traditional", compute_intercept = TRUE,
                     remove_intercept = TRUE, allow_sparse_x = FALSE)
    )
    for (type in c("class", "prob")) {
      parsnip::set_pred(
        model = model, eng = "mcd", mode = "classification", type = type,
        value = list(pre = NULL, post = NULL, func = c(fun = "predict"),
                     args = list(object = quote(object$fit), newdata = quote(new_data), type = type))
      )
    }
  }
  invisible(TRUE)
}
