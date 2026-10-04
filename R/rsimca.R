#' @title Robust SIMCA Classification (RSIMCA)
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Robust soft independent modeling of class analogy (RSIMCA) of Vanden
#' Branden and Hubert (2005): a robust PCA model ([robpca()]) of each class,
#' and the assignment of each observation to the class whose model is the
#' closest, from its score and orthogonal distances. It applies to
#' high-dimensional data such as whole spectra, and outlying spectra of the
#' training data have little influence on the class models.
#'
#' @details
#' Each class is modeled by [robpca()] with `ncomp` components (or the
#' number chosen by [robpca()] from `var_explained`). For an observation
#' \eqn{x} and the model of class \eqn{j}, the score distance
#' \eqn{SD_j(x)} and the orthogonal distance \eqn{OD_j(x)} are divided by
#' their cut-offs, so that 1 marks the boundary of the class. The
#' observation is assigned to the class with the smallest combined distance
#' \deqn{D_j(x) = \gamma \left(\frac{OD_j(x)}{c_{OD,j}}\right)^2 +
#'   (1 - \gamma) \left(\frac{SD_j(x)}{c_{SD,j}}\right)^2}
#' (rule R2 of the paper), or with the distances instead of their squares
#' when `squared = FALSE` (rule R1). `gamma` weights the orthogonal
#' distances against the score distances.
#'
#' An observation whose two scaled distances exceed 1 for every class is an
#' outlier for all of them (`outlying`): it is still assigned to the closest
#' class, but probably belongs to none.
#'
#' The misclassification rates are estimated on the regular observations of
#' the training data (those regular in the robust PCA of their class), and
#' the overall rate is weighted by the membership probabilities, by default
#' the proportions of these observations in each class. They tend to
#' underestimate the error: estimate it on test data or by
#' cross-validation.
#'
#' **Differences from the paper.** The number of components of each class
#' is given (`ncomp`) or chosen by [robpca()] from the proportion of
#' variance explained, instead of by robust cross-validation (PRESS).
#'
#' This is an independent implementation of the published description.
#' [robpca()] uses random directions and subsets, so use [set.seed()] for
#' reproducible results.
#'
#' @param x A numeric matrix or data frame of the predictors (spectra), one
#'   observation per row.
#' @param group The classes of the observations: a factor, or a vector
#'   converted to one. Each class needs at least 5 observations.
#' @param ncomp The number of components of the robust PCA of each class: a
#'   single number for all the classes, one per class (in the order of the
#'   levels of `group`, or named by them), or `NULL` (default) to let
#'   [robpca()] choose them from `var_explained`.
#' @param kmax The largest number of components of the robust PCA. Default
#'   is 10.
#' @param alpha The robustness parameter of [robpca()]: the fraction of
#'   observations of each class assumed to be regular, between 0.5 and 1.
#'   Default is 0.75.
#' @param gamma The weight of the orthogonal distances in the classification
#'   rule, between 0 and 1. Default is 0.5.
#' @param squared If `TRUE` (default), the rule combines the squared scaled
#'   distances (R2 of the paper); otherwise the scaled distances (R1).
#' @param var_explained The proportion of variance explained used by
#'   [robpca()] to choose the number of components when `ncomp` is `NULL`.
#'   Default is 0.8.
#' @param prior The membership (prior) probabilities of the classes, used to
#'   weight the overall misclassification rate, in the order of the levels
#'   of `group` (or named by them). Default is the proportions of the
#'   regular training observations in each class.
#' @param ndir,nsamp The number of random directions of the outlyingness and
#'   of random subsets of FAST-MCD in [robpca()]. Defaults are 250 and 500.
#'
#' @return An object of class `specproc_rsimca`, a list with:
#'  - `models`: the [robpca()] model of each class.
#'  - `ncomp`: the number of components of each class model.
#'  - `distances`: a tibble with the combined distance \eqn{D_j} of each
#'    training observation to each class.
#'  - `fitted`: the classes assigned to the training observations.
#'  - `weights`: 1 for the observations regular in the model of their class,
#'    0 for the others.
#'  - `outlying`: `TRUE` for the observations outlying for every class.
#'  - `misclassification`: a tibble with the misclassification rate of each
#'    class on its regular training observations, and the overall rate.
#'  - `prior`, `gamma`, `squared`, `levels`, `alpha`.
#'
#' Use [predict()][predict.specproc_rsimca] for the classes and distances
#' of new observations.
#'
#' @references
#'  - Vanden Branden, K., Hubert, M. (2005). Robust classification in high
#'    dimensions based on the SIMCA method. Chemometrics and Intelligent
#'    Laboratory Systems, 79(1-2):10-21.
#'  - Hubert, M., Rousseeuw, P.J., Vanden Branden, K. (2005). ROBPCA: a new
#'    approach to robust principal component analysis. Technometrics,
#'    47(1):64-79.
#'
#' @seealso [predict.specproc_rsimca()], [robpca()], [robust_da()]
#' @export
#'
#' @examples
#' data(forageLIBS)
#' wl <- suppressWarnings(as.numeric(names(forageLIBS)))
#' spectra <- forageLIBS[which(wl > 380 & wl < 430)]
#' # forage samples with low and high calcium
#' level <- cut(forageLIBS$Ca, c(-Inf, 0.6, Inf), labels = c("low", "high"))
#' set.seed(1)
#' fit <- rsimca(spectra[1:300, ], level[1:300], ncomp = 3)
#' fit
#' table(predict(fit, spectra[301:368, ]), level[301:368])
rsimca <- function(x, group, ncomp = NULL, kmax = 10, alpha = 0.75, gamma = 0.5, squared = TRUE,
                   var_explained = 0.8, prior = NULL, ndir = 250, nsamp = 500) {
  if (missing(x) || missing(group)) {
    stop("Both 'x' and 'group' must be provided.", call. = FALSE)
  }
  x <- robust_pca_input(x)
  group <- class_factor(group, nrow(x))
  lev <- levels(group)
  check_number(gamma, "gamma", lower = 0, upper = 1)
  check_flag(squared, "squared")
  sizes <- table(group)
  if (any(sizes < 5)) {
    stop("Each class needs at least 5 observations: class `", names(sizes)[which.min(sizes)],
         "` has ", min(sizes), ".", call. = FALSE)
  }
  ncomp <- class_ncomp(ncomp, lev)

  models <- lapply(seq_along(lev), function(j) {
    robpca(x[group == lev[j], , drop = FALSE], k = ncomp[[j]], kmax = kmax, alpha = alpha,
           ndir = ndir, var_explained = var_explained, nsamp = nsamp)
  })
  names(models) <- lev
  weights <- logical(nrow(x))
  for (j in seq_along(lev)) {
    weights[group == lev[j]] <- models[[j]]$outlier_type == "regular"
  }
  prior <- class_prior(prior, group, weights)

  res <- list(models = models, ncomp = vapply(models, function(m) m$k, integer(1)),
              distances = NULL, fitted = NULL, weights = as.numeric(weights), outlying = NULL,
              misclassification = NULL, prior = prior, gamma = gamma, squared = squared,
              levels = lev, alpha = alpha)
  res <- structure(res, variables = colnames(x), nvar = ncol(x), class = "specproc_rsimca")
  dist <- simca_distances(res, x)
  res$distances <- tibble::as_tibble(dist$combined)
  res$fitted <- simca_classes(dist$combined, lev)
  res$outlying <- dist$outlying
  res$misclassification <- misclassification_table(res$fitted, group, weights, prior)
  res
}

# Number of components of each class model: a list with one element per
# class (NULL to let robpca() choose).
class_ncomp <- function(ncomp, lev) {
  if (is.null(ncomp)) {
    return(rep(list(NULL), length(lev)))
  }
  if (!is.numeric(ncomp) || !length(ncomp) %in% c(1, length(lev))) {
    stop("'ncomp' must be NULL, a single number, or one number per class (", length(lev), ").",
         call. = FALSE)
  }
  for (k in ncomp) check_count(k, "ncomp")
  if (length(ncomp) == 1) {
    return(rep(list(as.integer(ncomp)), length(lev)))
  }
  if (!is.null(names(ncomp))) {
    if (!setequal(names(ncomp), lev)) {
      stop("The names of 'ncomp' must be the classes.", call. = FALSE)
    }
    ncomp <- ncomp[lev]
  }
  as.list(as.integer(ncomp))
}

# Scaled score and orthogonal distances of the rows of x to each class model,
# their combination by the classification rule, and whether each row is
# outlying for every class.
simca_distances <- function(object, x) {
  lev <- object$levels
  power <- if (object$squared) 2 else 1
  sd <- od <- matrix(0, nrow(x), length(lev), dimnames = list(NULL, lev))
  for (j in seq_along(lev)) {
    m <- object$models[[j]]
    pred <- stats::predict(m, x)
    sd[, j] <- pred$sd / m$cutoff_sd
    od[, j] <- if (m$cutoff_od > 0) pred$od / m$cutoff_od else 0
  }
  list(sd = sd, od = od,
       combined = object$gamma * od^power + (1 - object$gamma) * sd^power,
       outlying = !apply(sd <= 1 & od <= 1, 1, any))
}

simca_classes <- function(combined, lev) {
  factor(lev[max.col(-combined, ties.method = "first")], levels = lev)
}

#' @title Predictions of a Robust SIMCA Model
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Predicts the classes of new observations with a model fitted by
#' [rsimca()], or their distances to the classes.
#'
#' @param object An object returned by [rsimca()].
#' @param newdata A numeric matrix or data frame with the same variables as
#'   the training data.
#' @param type `"class"` (default) for the predicted classes, or
#'   `"distances"` for the combined distances to the classes.
#' @param ... Not used.
#'
#' @return With `type = "class"`, a factor of the predicted classes. With
#'   `type = "distances"`, a tibble with the combined distance of each
#'   observation to each class (one column per class), and `outlying`,
#'   `TRUE` for the observations outlying for every class.
#'
#' @seealso [rsimca()]
#' @export
#'
#' @examples
#' data(forageLIBS)
#' wl <- suppressWarnings(as.numeric(names(forageLIBS)))
#' spectra <- forageLIBS[which(wl > 380 & wl < 430)]
#' level <- cut(forageLIBS$Ca, c(-Inf, 0.6, Inf), labels = c("low", "high"))
#' set.seed(1)
#' fit <- rsimca(spectra[1:300, ], level[1:300], ncomp = 3)
#' head(predict(fit, spectra[301:368, ], type = "distances"))
predict.specproc_rsimca <- function(object, newdata, type = c("class", "distances"), ...) {
  type <- match.arg(type)
  x <- filter_newdata(object, newdata)
  if (anyNA(x)) {
    stop("'newdata' contains missing values.", call. = FALSE)
  }
  dist <- simca_distances(object, x)
  if (type == "class") {
    return(simca_classes(dist$combined, object$levels))
  }
  out <- tibble::as_tibble(dist$combined)
  out$outlying <- dist$outlying
  out
}

#' @export
print.specproc_rsimca <- function(x, ...) {
  cat("Robust SIMCA (RSIMCA)\n\n")
  cat("Observations:   ", length(x$weights), " (", sum(x$weights == 0),
      " outliers in their class, ", sum(x$outlying), " in all classes)\n", sep = "")
  cat("Variables:      ", attr(x, "nvar"), "\n", sep = "")
  cat("Classes:        ", paste0(x$levels, " (", x$ncomp, " comp.)", collapse = ", "), "\n",
      sep = "")
  cat("Rule:           gamma = ", x$gamma, ", ", if (x$squared) "squared" else "unsquared",
      " scaled distances\n", sep = "")
  cat("\nMisclassification of the regular training observations:\n")
  print(x$misclassification)
  invisible(x)
}
