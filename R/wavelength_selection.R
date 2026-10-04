#' @title Wavelength Selection for PLS Regression
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Selects the spectral variables (wavelengths) that are informative for a
#' response, from a PLS regression model:
#'  - `"vip"`: the variable importance in projection (VIP) of each variable;
#'  - `"sr"`: the selectivity ratio (SR) of each variable;
#'  - `"ipls"`: forward interval PLS (iPLS), which selects contiguous
#'    spectral intervals.
#'
#' With `robust = TRUE`, the PLS model is the robust model of [rsimpls()],
#' so that outlying spectra and wrong reference values do not drive the
#' selection. [step_select_wavelengths()] performs the selection in a
#' tidymodels recipe, where it is repeated on every resample.
#'
#' @details
#' **VIP** (Wold, Johansson and Cocchi, 1993) sums the squared PLS weights of
#' each variable over the components, weighted by the variance of the
#' response that each component explains:
#' \deqn{VIP_j = \sqrt{p \sum_a SSY_a w_{ja}^2 / \sum_a SSY_a}.}
#' The weights are the orthonormal PLS weights of the NIPALS algorithm (an
#' orthonormal basis of the SIMPLS weights, which span the same nested
#' subspaces). The squared VIP values average 1, so the usual rule
#' \eqn{VIP > 1} keeps the variables of above-average importance. The PLS
#' weights favor variables of large variance, so on unscaled spectra a
#' strong line unrelated to the response can get a large VIP through the
#' later components.
#'
#' **SR** (Rajalahti et al., 2009) splits each variable, by target
#' projection on the normalized regression vector
#' \eqn{w_{TP} = b/\|b\|}, into a part explained by the predictive
#' component \eqn{t_{TP} = X w_{TP}} and a residual, and divides their
#' variances. Unlike VIP, it does not depend on the intensity of the
#' variable: a weak emission line that follows the response as closely as a
#' strong one gets the same ratio, which matters for unscaled spectra. The
#' ratios depend on how much of the variance of each variable is unrelated
#' to the response (in LIBS, shot-to-shot fluctuations), so there is no
#' general threshold: in the `forageLIBS` spectra, the largest ratio for
#' calcium, at the Ca II 317.9 nm line, is about 0.5.
#'
#' Both keep the `num_terms` variables with the largest values or, if
#' `num_terms = NULL`, those above `threshold` (by default 1 for VIP; SR
#' needs `num_terms` or `threshold`). With `recursive = TRUE`
#' (backward variable elimination), the model is refitted on the remaining
#' variables and the fraction `prop_drop` of the least important ones is
#' removed, until `num_terms` variables are left, so that the importance is
#' recomputed without the variables already removed.
#'
#' **iPLS** (Nørgaard et al., 2000) splits the variables into `intervals`
#' contiguous intervals of nearly equal sizes (in the order of the columns,
#' which should be sorted by wavelength). Starting from none, it adds at
#' each step the interval that gives the lowest cross-validated error
#' (RMSECV) of a PLS model on the intervals selected so far, with the best
#' number of components up to `ncomp`. It stops after `num_intervals`
#' intervals or, if `num_intervals = NULL`, when no interval lowers the
#' RMSECV. The observations are split into `folds` interleaved groups
#' (observation `i` in group `(i - 1) %% folds + 1`); average replicate
#' spectra of a sample first, or they will fall in different groups. When
#' a parallel plan is set with `future::plan()` (and the future.apply
#' package is installed), the candidate intervals of each step are
#' evaluated in parallel; within [tune::tune_grid()], which runs the
#' resamples in parallel, they are evaluated sequentially.
#'
#' **Robust selection.** With `robust = TRUE`, VIP and SR are computed from
#' an [rsimpls()] model, on the observations of its final regression (those
#' that are not regression outliers; good leverage points are kept). For
#' iPLS, an [rsimpls()] model of all the variables identifies these
#' observations once, and the intervals are selected by classical PLS on
#' them, so that outliers affect neither the models nor the RMSECV. Refitting a
#' robust model for every candidate interval would be about 35 times
#' slower. Use [set.seed()] for reproducible results.
#'
#' The selection uses the response, so it must be estimated on training
#' data only: to assess a model built on the selected variables, select
#' within each resample with [step_select_wavelengths()].
#'
#' @param x A numeric matrix or data frame of the spectra, one observation
#'   per row and the variables in wavelength order.
#' @param y A numeric vector of the response.
#' @param method The selection method: `"vip"` (default), `"sr"` or
#'   `"ipls"`.
#' @param ncomp The number of PLS components of the model that ranks the
#'   variables (`"vip"`, `"sr"`), or the largest number of components of the
#'   interval models (`"ipls"`). Default is 5.
#' @param num_terms The number of variables to keep (`"vip"`, `"sr"`). If
#'   `NULL` (default), the variables above `threshold` are kept.
#' @param threshold The importance above which variables are kept when
#'   `num_terms = NULL`. If `NULL` (default), 1 for VIP; SR has no default
#'   threshold.
#' @param recursive If `TRUE`, remove the variables by backward elimination
#'   (`"vip"`, `"sr"`; requires `num_terms`). Default is `FALSE`.
#' @param prop_drop The fraction of the remaining variables removed at each
#'   round of backward elimination. Default is 0.25.
#' @param intervals The number of contiguous intervals (`"ipls"`). Default
#'   is 40.
#' @param num_intervals The number of intervals to select (`"ipls"`). If
#'   `NULL` (default), intervals are added while the RMSECV decreases.
#' @param folds The number of cross-validation groups (`"ipls"`). Default
#'   is 5.
#' @param robust If `TRUE`, use the robust PLS model of [rsimpls()].
#'   Default is `FALSE`.
#' @param ... Further arguments of [rsimpls()] when `robust = TRUE`:
#'   `alpha`, `ndir` and `nsamp`.
#'
#' @return An object of class `specproc_wavelength_selection`, a list with:
#'  - `selected`: the names of the selected variables, in column order.
#'  - `importance`: the VIP or SR of every variable (from the model of all
#'    the variables), or for iPLS the RMSECV of the interval of each
#'    variable alone.
#'  - `intervals`: for iPLS, a tibble with one row per interval: its first
#'    and last variables, size, RMSECV alone, and `step`, the step at which
#'    it was selected (`NA` if not selected).
#'  - `path`: a tibble of the selection path: the number of variables left
#'    after each round of backward elimination, or for iPLS the interval
#'    added at each step, with the RMSECV and number of components.
#'  - `method`, `ncomp`, `robust`, `threshold` (when used), `observations`
#'    (the number of observations used; with `robust = TRUE`, those of the
#'    robust regression) and `mean_spectrum` (for
#'    [plot_wavelength_selection()]).
#'
#' @references
#'  - Wold, S., Johansson, E., Cocchi, M. (1993). PLS: partial least squares
#'    projections to latent structures. In Kubinyi, H. (ed.), 3D QSAR in
#'    Drug Design: Theory, Methods and Applications, 523-550. ESCOM, Leiden.
#'  - Rajalahti, T., Arneberg, R., Berven, F.S., Myhr, K.-M., Ulvik, R.J.,
#'    Kvalheim, O.M. (2009). Biomarker discovery in mass spectral profiles by
#'    means of selectivity ratio plot. Chemometrics and Intelligent
#'    Laboratory Systems, 95(1):35-48.
#'  - Nørgaard, L., Saudland, A., Wagner, J., Nielsen, J.P., Munck, L.,
#'    Engelsen, S.B. (2000). Interval partial least-squares regression
#'    (iPLS): a comparative chemometric study with an example from
#'    near-infrared spectroscopy. Applied Spectroscopy, 54(3):413-419.
#'  - Mehmood, T., Liland, K.H., Snipen, L., Sæbø, S. (2012). A review of
#'    variable selection methods in partial least squares regression.
#'    Chemometrics and Intelligent Laboratory Systems, 118:62-69.
#'
#' @seealso [step_select_wavelengths()], [plot_wavelength_selection()],
#'   [rsimpls()]
#'
#' @export select_wavelengths
#'
#' @examples
#' data(forageLIBS)
#' wl <- suppressWarnings(as.numeric(names(forageLIBS)))
#' spectra <- forageLIBS[which(wl > 380 & wl < 430)]  # Ca II and Ca I lines
#' sel <- select_wavelengths(spectra, forageLIBS$Ca, method = "sr", num_terms = 50)
#' sel
#' head(sel$selected)
#' select_wavelengths(spectra, forageLIBS$Ca, method = "ipls", intervals = 10)
select_wavelengths <- function(x, y, method = c("vip", "sr", "ipls"), ncomp = 5, num_terms = NULL,
                               threshold = NULL, recursive = FALSE, prop_drop = 0.25, intervals = 40,
                               num_intervals = NULL, folds = 5, robust = FALSE, ...) {
  if (missing(x) || missing(y) || is.null(x) || is.null(y)) {
    stop("Both 'x' and 'y' must be provided.", call. = FALSE)
  }
  method <- match.arg(method)
  x <- as_numeric_matrix(x, "x")
  if (is.null(colnames(x))) colnames(x) <- paste0("V", seq_len(ncol(x)))
  y <- as_response_matrix(y, nrow(x))
  if (ncol(y) != 1) {
    stop("'y' must be a single response.", call. = FALSE)
  }
  y <- y[, 1]
  if (anyNA(x) || anyNA(y)) {
    stop("'x' and 'y' must not contain missing values.", call. = FALSE)
  }
  if (nrow(x) < 5 || ncol(x) < 2) {
    stop("At least 5 observations and 2 variables are needed.", call. = FALSE)
  }
  check_count(ncomp, "ncomp")
  if (!is.null(num_terms)) check_count(num_terms, "num_terms")
  if (!is.null(threshold)) check_number(threshold, "threshold", lower = 0)
  check_flag(recursive, "recursive")
  check_number(prop_drop, "prop_drop", lower = 0, upper = 1, lower_open = TRUE, upper_open = TRUE)
  check_count(intervals, "intervals", lower = 2)
  if (!is.null(num_intervals)) check_count(num_intervals, "num_intervals")
  check_count(folds, "folds", lower = 2)
  check_flag(robust, "robust")
  if (recursive && is.null(num_terms)) {
    stop("Backward elimination (`recursive = TRUE`) needs `num_terms`.", call. = FALSE)
  }
  if (method == "sr" && is.null(num_terms) && is.null(threshold)) {
    stop("The selectivity ratio has no general threshold: give `num_terms` or `threshold`.",
         call. = FALSE)
  }
  if (is.null(threshold)) threshold <- 1
  robust_args <- robust_selection_args(list(...))

  res <- if (method == "ipls") {
    ipls_selection(x, y, ncomp, intervals, num_intervals, folds, robust, robust_args)
  } else {
    filter_selection(x, y, method, ncomp, num_terms, threshold, recursive, prop_drop, robust,
                     robust_args)
  }
  res$method <- method
  res$ncomp <- as.integer(ncomp)
  res$robust <- robust
  res$mean_spectrum <- colMeans(x)
  structure(res, nvar = ncol(x), class = "specproc_wavelength_selection")
}

# The arguments of rsimpls() passed through `...`, with their defaults.
robust_selection_args <- function(args) {
  if (length(args) && (is.null(names(args)) || any(!names(args) %in% c("alpha", "ndir", "nsamp")))) {
    stop("The arguments in '...' must be `alpha`, `ndir` or `nsamp` (see rsimpls()).", call. = FALSE)
  }
  args <- utils::modifyList(list(alpha = 0.75, ndir = 250, nsamp = 500), args)
  check_number(args$alpha, "alpha", lower = 0.5, upper = 1)
  check_count(args$nsamp, "nsamp")
  args
}

# PLS model that ranks the variables: SIMPLS, or RSIMPLS with robust = TRUE.
# Returns the SIMPLS weight vectors, the regression vector and the
# observations to use (all, or those of the final robust regression).
importance_model <- function(x, y, ncomp, robust, robust_args) {
  if (robust) {
    fit <- rsimpls_fit(x, matrix(y, dimnames = list(NULL, "y")), ncomp, robust_args$alpha,
                       robust_args$ndir, robust_args$nsamp)
    k <- min(ncomp, fit$kmax)
    model <- fit$models[[k]]
    list(weights = fit$weights[, seq_len(k), drop = FALSE], b = drop(model$coefficients),
         rows = model$weights == 1)
  } else {
    k <- min(ncomp, nrow(x) - 2, ncol(x))
    fit <- pls::simpls.fit(x, y, k, center = TRUE, stripped = FALSE)
    list(weights = fit$projection, b = fit$coefficients[, 1, k], rows = rep(TRUE, nrow(x)))
  }
}

# VIP from the (centered) data, response and SIMPLS weight vectors. The
# variance of the response explained by each component is the sequential
# sum of squares of the scores, and the weights are orthonormalized in
# order (the NIPALS weights).
vip_scores <- function(xc, yc, weights) {
  scores <- xc %*% weights
  ssy <- drop(crossprod(qr.Q(qr(scores)), yc))^2
  w <- qr.Q(qr(weights))
  sqrt(ncol(xc) * drop(w^2 %*% ssy) / sum(ssy))
}

# Selectivity ratio: explained over residual variance of each (centered)
# variable after target projection on the regression vector b.
sr_scores <- function(xc, b) {
  t <- drop(xc %*% b) / sqrt(sum(b^2))
  tt <- sum(t^2)
  explained <- tt * (drop(crossprod(xc, t)) / tt)^2
  total <- colSums(xc^2)
  residual <- pmax(total - explained, total * 1e-12)
  ifelse(total > 0, explained / residual, 0)
}

filter_selection <- function(x, y, method, ncomp, num_terms, threshold, recursive, prop_drop,
                             robust, robust_args) {
  score <- function(cols) {
    model <- importance_model(x[, cols, drop = FALSE], y, ncomp, robust, robust_args)
    xr <- x[model$rows, cols, drop = FALSE]
    xc <- sweep(xr, 2, colMeans(xr))
    s <- if (method == "vip") {
      vip_scores(xc, y[model$rows] - mean(y[model$rows]), model$weights)
    } else {
      sr_scores(xc, model$b)
    }
    list(score = stats::setNames(s, colnames(x)[cols]), rows = model$rows)
  }
  p <- ncol(x)
  first <- score(seq_len(p))
  importance <- first$score
  if (!is.null(num_terms) && num_terms >= p) {
    keep <- seq_len(p)
    path <- tibble::tibble(round = 0L, variables = p)
  } else if (!recursive) {
    keep <- if (is.null(num_terms)) {
      which(importance > threshold)
    } else {
      sort(order(importance, decreasing = TRUE)[seq_len(num_terms)])
    }
    path <- tibble::tibble(round = 0:1, variables = c(p, length(keep)))
  } else {
    keep <- seq_len(p)
    current <- importance
    sizes <- p
    while (length(keep) > num_terms) {
      n_keep <- max(num_terms, floor(length(keep) * (1 - prop_drop)))
      keep <- sort(keep[order(current, decreasing = TRUE)[seq_len(n_keep)]])
      sizes <- c(sizes, length(keep))
      if (length(keep) > num_terms) current <- score(keep)$score
    }
    path <- tibble::tibble(round = seq_along(sizes) - 1L, variables = sizes)
  }
  if (length(keep) == 0) {
    stop("No variable has an importance above `threshold` (", threshold, "); lower it or use `num_terms`.",
         call. = FALSE)
  }
  list(selected = colnames(x)[keep], importance = importance,
       threshold = if (is.null(num_terms)) threshold else NULL, path = path,
       observations = sum(first$rows))
}

ipls_selection <- function(x, y, ncomp, intervals, num_intervals, folds, robust, robust_args) {
  p <- ncol(x)
  rows <- if (robust) importance_model(x, y, ncomp, TRUE, robust_args)$rows else rep(TRUE, nrow(x))
  xr <- if (all(rows)) x else x[rows, , drop = FALSE]
  yr <- y[rows]
  if (nrow(xr) < 2 * folds) {
    stop("Too few observations for ", folds, "-fold cross-validation.", call. = FALSE)
  }
  intervals <- min(intervals, p)
  group <- ceiling(seq_len(p) * intervals / p)
  fold <- (seq_len(nrow(xr)) - 1) %% folds + 1

  selected <- integer(0)
  best <- Inf
  alone <- NULL
  path <- list()
  repeat {
    candidates <- setdiff(seq_len(intervals), selected)
    if (length(candidates) == 0) break
    errs <- parallel_lapply(candidates, ipls_candidate(xr, yr, fold, group, ncomp, selected))
    rmse <- vapply(errs, `[[`, numeric(1), "rmse")
    if (is.null(alone)) alone <- rmse
    i <- which.min(rmse)
    if (is.null(num_intervals) && rmse[i] >= best) break
    selected <- c(selected, candidates[i])
    best <- rmse[i]
    path[[length(path) + 1]] <- tibble::tibble(step = length(selected), interval = candidates[i],
                                               RMSECV = rmse[i], ncomp = errs[[i]]$ncomp)
    if (!is.null(num_intervals) && length(selected) >= num_intervals) break
  }
  bounds <- split(seq_len(p), group)
  info <- tibble::tibble(
    interval = seq_len(intervals),
    first = colnames(x)[vapply(bounds, min, integer(1))],
    last = colnames(x)[vapply(bounds, max, integer(1))],
    size = lengths(bounds),
    RMSECV = alone,
    step = match(seq_len(intervals), selected)
  )
  list(selected = colnames(x)[group %in% selected],
       importance = stats::setNames(alone[group], colnames(x)),
       intervals = info, path = dplyr::bind_rows(path), observations = sum(rows))
}

# The RMSECV of the selected intervals plus one candidate interval, as a
# function of the candidate. Built here so that its environment holds only
# what parallel workers need.
ipls_candidate <- function(x, y, fold, group, ncomp, selected) {
  force(x)
  force(y)
  force(fold)
  force(group)
  force(ncomp)
  force(selected)
  function(g) ipls_cv(x[, group %in% c(selected, g), drop = FALSE], y, fold, ncomp)
}

# RMSECV of PLS models with 1 to ncomp components, over the given
# cross-validation groups; returns the smallest and its number of components.
ipls_cv <- function(x, y, fold, ncomp) {
  k <- min(ncomp, ncol(x), sum(fold != fold[1]) - 1)
  press <- numeric(k)
  for (f in unique(fold)) {
    train <- fold != f
    fit <- pls::simpls.fit(x[train, , drop = FALSE], y[train], k, center = TRUE, stripped = TRUE)
    test <- sweep(x[!train, , drop = FALSE], 2, fit$Xmeans)
    pred <- test %*% matrix(fit$coefficients, ncol(x), k) + fit$Ymeans
    press <- press + colSums((y[!train] - pred)^2)
  }
  rmse <- sqrt(press / length(y))
  list(rmse = min(rmse), ncomp = which.min(rmse))
}

# lapply(), in parallel with future.apply when a parallel plan is set
# (future::plan() with more than one worker), sequentially otherwise. With
# `seed = TRUE`, each element gets its own stream of random numbers (for
# FUN using random numbers, reproducible with set.seed()).
parallel_lapply <- function(X, FUN, seed = FALSE) {
  if (length(X) > 1 && rlang::is_installed(c("future", "future.apply")) && future::nbrOfWorkers() > 1) {
    future.apply::future_lapply(X, FUN, future.seed = seed)
  } else {
    lapply(X, FUN)
  }
}

#' @export
print.specproc_wavelength_selection <- function(x, ...) {
  label <- switch(x$method, vip = "VIP", sr = "selectivity ratio", ipls = "forward interval PLS")
  cat("Wavelength selection (", label, if (x$robust) ", robust", ")\n\n", sep = "")
  p <- attr(x, "nvar")
  cat("Variables:      ", p, "\n", sep = "")
  cat("Selected:       ", length(x$selected), " (", format(100 * length(x$selected) / p, digits = 3),
      "%)\n", sep = "")
  cat("Observations:   ", x$observations, if (x$robust) " (without the regression outliers)", "\n", sep = "")
  if (x$method == "ipls") {
    cat("Components:     up to ", x$ncomp, "\n", sep = "")
    if (nrow(x$path)) {
      last <- x$path[nrow(x$path), ]
      cat("Intervals:      ", nrow(x$path), " of ", nrow(x$intervals), " (RMSECV ",
          format(last$RMSECV, digits = 4), ", ", last$ncomp, " components)\n", sep = "")
    }
  } else {
    cat("Components:     ", x$ncomp, "\n", sep = "")
    rule <- if (!is.null(x$threshold)) {
      paste0(toupper(x$method), " > ", format(x$threshold))
    } else if (nrow(x$path) > 2) {
      paste0("backward elimination in ", nrow(x$path) - 1, " rounds")
    } else {
      "the most important variables"
    }
    cat("Rule:           ", rule, "\n", sep = "")
  }
  invisible(x)
}

#' @title Plot of a Wavelength Selection
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Shows the variables selected by [select_wavelengths()]: the mean
#' spectrum, with the selected regions shaded, above the importance of each
#' variable (VIP or selectivity ratio, with the threshold when one was
#' used) or, for iPLS, the RMSECV of each interval alone, with the selected
#' intervals colored.
#'
#' @param object An object returned by [select_wavelengths()].
#' @param title The plot title.
#'
#' @return A ggplot object.
#'
#' @seealso [select_wavelengths()]
#' @export
#'
#' @examples
#' data(forageLIBS)
#' wl <- suppressWarnings(as.numeric(names(forageLIBS)))
#' spectra <- forageLIBS[which(wl > 380 & wl < 430)]
#' sel <- select_wavelengths(spectra, forageLIBS$Ca, method = "sr", num_terms = 50)
#' plot_wavelength_selection(sel)
plot_wavelength_selection <- function(object, title = NULL) {
  if (!inherits(object, "specproc_wavelength_selection")) {
    stop("'object' must be returned by select_wavelengths().", call. = FALSE)
  }
  vars <- names(object$mean_spectrum)
  wl <- names_to_wavelength(vars)
  x_lab <- if (is.null(wl)) "Variable" else "Wavelength (nm)"
  if (is.null(wl)) wl <- seq_along(vars)
  selected <- vars %in% object$selected
  importance_label <- switch(object$method, vip = "VIP", sr = "Selectivity ratio",
                             ipls = "RMSECV of the interval")
  panels <- c("Mean spectrum", importance_label)
  curves <- data.frame(
    wl = rep(wl, 2),
    value = c(object$mean_spectrum, object$importance),
    panel = factor(rep(panels, each = length(wl)), levels = panels)
  )
  if (object$method == "ipls") curves <- curves[curves$panel == panels[1], ]

  # shaded runs of contiguous selected variables, half a step beyond them
  step <- if (length(wl) > 1) stats::median(diff(wl)) else 1
  runs <- rle(selected)
  ends <- cumsum(runs$lengths)
  starts <- ends - runs$lengths + 1
  shade <- data.frame(xmin = wl[starts[runs$values]] - step / 2, xmax = wl[ends[runs$values]] + step / 2)

  p <- ggplot2::ggplot() +
    ggplot2::geom_rect(data = shade, ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax),
                       ymin = -Inf, ymax = Inf, fill = "#1b9e77", alpha = 0.2)
  if (object$method == "ipls") {
    info <- object$intervals
    bars <- data.frame(
      xmin = wl[match(info$first, vars)] - step / 2, xmax = wl[match(info$last, vars)] + step / 2,
      value = info$RMSECV, chosen = ifelse(is.na(info$step), "not selected", "selected"),
      panel = factor(panels[2], levels = panels)
    )
    p <- p + ggplot2::geom_rect(data = bars, ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax,
                                                         ymin = 0, ymax = .data$value, fill = .data$chosen),
                                colour = "white", linewidth = 0.2) +
      ggplot2::scale_fill_manual(values = c(selected = "#1b9e77", `not selected` = "grey70"), name = NULL)
  }
  p <- p + ggplot2::geom_line(data = curves, ggplot2::aes(.data$wl, .data$value), linewidth = 0.3)
  if (!is.null(object$threshold)) {
    p <- p + ggplot2::geom_hline(data = data.frame(panel = factor(panels[2], levels = panels),
                                                   y = object$threshold),
                                 ggplot2::aes(yintercept = .data$y), linetype = "dashed", colour = "grey30")
  }
  if (is.null(title)) {
    method <- switch(object$method, vip = "VIP", sr = "Selectivity ratio", ipls = "Interval PLS")
    title <- paste0(method, " wavelength selection: ", length(object$selected), " of ",
                    length(vars), " variables")
  }
  finish_title(p +
    ggplot2::facet_wrap(~ panel, ncol = 1, scales = "free_y", strip.position = "left") +
    ggplot2::labs(x = x_lab, y = NULL, title = title) +
    ggplot2::theme_bw() +
    ggplot2::theme(strip.placement = "outside", strip.background = ggplot2::element_blank(),
                   legend.position = "bottom", panel.grid.minor = ggplot2::element_blank()))
}

#' @title Wavelength Selection Recipe Step
#'
#' @author Christian L. Goueguel
#'
#' @description
#' `step_select_wavelengths()` creates a *specification* of a recipe step
#' that keeps the predictors (wavelengths) selected by
#' [select_wavelengths()] and removes the others. The selection is made on
#' the training data when the recipe is prepped, so that within
#' [tune::tune_grid()] it is repeated on the analysis set of every
#' resample, as it must be: a selection made on all the data would make the
#' cross-validated error too optimistic.
#'
#' @details
#' See [select_wavelengths()] for the methods: `"vip"` and `"sr"` keep the
#' `num_terms` most important predictors (or those above `threshold`), and
#' `"ipls"` selects `num_intervals` contiguous intervals. The outcome is not
#' needed when new data are baked.
#'
#' # Tuning
#'
#' `num_terms`, `num_intervals` and `num_comp` (the components of the
#' selection model) can be tuned with [tune::tune()]. Their default ranges
#' are [dials::num_terms()] with 20 to 1000 predictors, [num_intervals()]
#' with 1 to 10 intervals, and [dials::num_comp()] with 1 to 10 components.
#' When the model also has a `num_comp` argument, give the step's parameter
#' its own id, such as `num_comp = tune("select_comp")`.
#'
#' # Tidying
#'
#' [tidy()][recipes::tidy.recipe] returns a tibble with columns `terms` (the
#' selected predictors), `selected`, `importance` and `id`.
#'
#' @inherit step_osc return
#' @inheritParams step_osc
#' @inheritParams select_wavelengths
#' @param num_comp The number of components of the selection model (`ncomp`
#'   of [select_wavelengths()]). Default is 5.
#' @param options A list of further arguments of [select_wavelengths()]:
#'   `prop_drop`, `folds`, and `alpha`, `ndir`, `nsamp` with
#'   `robust = TRUE`.
#' @param res The selection, stored once the step has been trained.
#'
#' @seealso [select_wavelengths()], [plot_wavelength_selection()]
#' @export
#'
#' @examplesIf rlang::is_installed("recipes")
#' library(recipes)
#' data(forageLIBS)
#' wl <- suppressWarnings(as.numeric(names(forageLIBS)))
#' dat <- forageLIBS[c(which(names(forageLIBS) == "Ca"), which(wl > 380 & wl < 430))]
#' rec <- recipe(Ca ~ ., data = dat[1:300, ]) |>
#'   step_select_wavelengths(all_predictors(), method = "sr", num_terms = 50)
#' prepped <- prep(rec)
#' ncol(bake(prepped, new_data = dat[301:368, -1]))
#' tidy(prepped, number = 1)
step_select_wavelengths <- function(recipe, ..., role = NA, trained = FALSE, outcome = NULL,
                                    method = "vip", num_terms = NULL, threshold = NULL,
                                    num_comp = 5, recursive = FALSE, intervals = 40,
                                    num_intervals = NULL, robust = FALSE, options = list(),
                                    res = NULL, columns = NULL, skip = FALSE,
                                    id = recipes::rand_id("select_wavelengths")) {
  rlang::check_installed("recipes")
  method <- match.arg(method, c("vip", "sr", "ipls"))
  recipes::add_step(recipe, specproc_step_new(
    "select_wavelengths", terms = rlang::enquos(...), role = role, trained = trained,
    outcome = rlang::enquos(outcome), method = method, num_terms = num_terms,
    threshold = threshold, num_comp = num_comp, recursive = recursive, intervals = intervals,
    num_intervals = num_intervals, robust = robust, options = options, res = res,
    columns = columns, skip = skip, id = id
  ))
}

#' @title Number of Intervals
#'
#' @description
#' The number of spectral intervals selected by interval PLS, a tuning
#' parameter of [step_select_wavelengths()] with `method = "ipls"`.
#'
#' @param range A two-element vector with the smallest and largest numbers
#'   of intervals. Default is 1 to 10.
#' @param trans A transformation object from the scales package, or `NULL`.
#'
#' @return A `dials` quantitative parameter.
#' @export
#'
#' @examplesIf rlang::is_installed("dials")
#' num_intervals()
num_intervals <- function(range = c(1L, 10L), trans = NULL) {
  rlang::check_installed("dials")
  dials::new_quant_param(type = "integer", range = range, inclusive = c(TRUE, TRUE),
                         trans = trans, label = c(num_intervals = "# Intervals"),
                         finalize = NULL)
}

#' @exportS3Method recipes::prep
prep.step_select_wavelengths <- function(x, training, info = NULL, ...) {
  cols <- step_predictors(x, training, info)
  y_name <- step_outcome(x, training, info)
  if (length(cols) > 0) {
    xmat <- step_matrix(training, cols)
    y <- training[[y_name]]
    if (anyNA(xmat) || anyNA(y)) {
      stop("`step_select_wavelengths()` does not handle missing values; impute or remove them first.",
           call. = FALSE)
    }
    args <- c(list(xmat, y, method = x$method, ncomp = x$num_comp, num_terms = x$num_terms,
                   threshold = x$threshold, recursive = x$recursive, intervals = x$intervals,
                   num_intervals = x$num_intervals, robust = x$robust),
              x$options %||% list())
    fit <- do.call(select_wavelengths, args)
    fit$mean_spectrum <- NULL
    x$res <- fit
  }
  x$columns <- cols
  x$outcome <- y_name
  x$trained <- TRUE
  x
}

#' @exportS3Method recipes::bake
bake.step_select_wavelengths <- function(object, new_data, ...) {
  cols <- object$columns
  recipes::check_new_data(cols, object, new_data)
  removed <- setdiff(cols, object$res$selected)
  if (length(removed) > 0) new_data[removed] <- NULL
  new_data
}

#' @export
print.step_select_wavelengths <- function(x, width = max(20, options()$width - 30), ...) {
  title <- paste0("Wavelength selection (", x$method, ") on ")
  recipes::print_step(x$columns, x$terms, x$trained, title, width)
  invisible(x)
}

#' @exportS3Method generics::tidy
tidy.step_select_wavelengths <- function(x, ...) {
  if (recipes::is_trained(x) && !is.null(x$res)) {
    tibble::tibble(terms = x$columns, selected = x$columns %in% x$res$selected,
                   importance = unname(x$res$importance[x$columns]), id = x$id)
  } else {
    terms <- if (recipes::is_trained(x)) x$columns else recipes::sel2char(x$terms)
    tibble::tibble(terms = terms, selected = NA, importance = NA_real_, id = x$id)
  }
}

#' @exportS3Method generics::tunable
tunable.step_select_wavelengths <- function(x, ...) {
  tibble::tibble(
    name = c("num_terms", "num_intervals", "num_comp"),
    call_info = list(list(pkg = "dials", fun = "num_terms", range = c(20L, 1000L)),
                     list(pkg = "specProc", fun = "num_intervals"),
                     list(pkg = "dials", fun = "num_comp", range = c(1L, 10L))),
    source = "recipe", component = "step_select_wavelengths", component_id = x$id
  )
}

#' @exportS3Method generics::required_pkgs
required_pkgs.step_select_wavelengths <- function(x, ...) {
  c("specProc")
}
