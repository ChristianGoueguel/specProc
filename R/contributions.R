#' @title Q and T-squared Contributions of a PCA Model
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Computes how much each variable (wavelength) contributes to the Q
#' residual or to Hotelling's \eqn{T^2} of each sample, keeping the sign of
#' the deviation, to find the variables that make a sample an outlier.
#' Relative contributions, computed against reference samples, show what
#' differs between a sample and the reference ones.
#'
#' @details
#' For a sample \eqn{x} (centered and scaled as in the model), with scores
#' \eqn{t = xP} on the first \eqn{k} components (loadings \eqn{P},
#' variances of the scores \eqn{\lambda}):
#'  - the **Q contributions** are the residuals \eqn{e = x - tP^T}, whose
#'    sum of squares is Q (the squared orthogonal distance of robust fits);
#'  - the **\eqn{T^2} contributions** are \eqn{t \Lambda^{-1/2} P^T}, the
#'    scaled scores projected back onto the variables, whose sum of squares
#'    is \eqn{T^2} (the squared score distance of robust fits). This is the
#'    definition of the PLS_Toolbox (Eigenvector Research). For [rospca()]
#'    fits, whose loadings are not exactly orthogonal, the sum of squares is
#'    close to, but not exactly, \eqn{T^2}.
#'
#' Q contributions show which variables the model does not describe for a
#' sample: large contributions grouped on a few emission lines point to a
#' systematic deviation (a contamination, a matrix effect, a saturated or
#' shifted line), small ones spread over all channels to random noise.
#' \eqn{T^2} contributions show which variables place a sample far from the
#' center in the score space.
#'
#' **Relative contributions.** Normally, contributions are relative to the
#' model (its center, for \eqn{T^2}). With `reference`, the mean
#' contribution of the reference samples is subtracted from the
#' contribution of each sample, which shows what differs between them: for
#' example, whether two samples have large Q residuals for the same reason,
#' or what moves a sample from the regular samples to where it lies in the
#' score space. `reference = "regular"` uses all the regular samples: of
#' the robust fit, or at the highest confidence level of [q_residuals()] for
#' a [stats::prcomp()] fit.
#'
#' The contributions are in the units of the preprocessed data of the
#' model: centered (and scaled, if the model scaled the variables).
#'
#' @param model A [stats::prcomp()] fit, or an object returned by [robpca()],
#'   [rospca()] or [macropca()].
#' @param k The number of components. Required for a [stats::prcomp()] fit;
#'   for a robust fit, at most its number of components (the default).
#' @param statistic `"q"` (default) for the Q contributions, or `"t2"` for
#'   the \eqn{T^2} contributions.
#' @param data The samples whose contributions are computed: a numeric
#'   matrix or data frame with the variables of the model (other columns
#'   are ignored when the columns are named), preprocessed like the data the
#'   model was fitted on (for example, with [center()] if the model was
#'   fitted on `center(spectra)`). Default is the calibration
#'   data of a [stats::prcomp()] fit (which must keep all its components),
#'   or the imputed data of a [macropca()] fit; it is required for
#'   [robpca()] and [rospca()] fits, which do not keep their data.
#' @param samples Optional indices or names of the rows of `data` to return.
#'   Default is all.
#' @param reference Optional indices or names of rows of `data` whose mean
#'   contribution is subtracted (relative contributions), or `"regular"`
#'   for all the regular samples.
#'
#' @return A tibble of class `specproc_contributions` with one row per
#'   sample: `sample` (the row name, or number) and one column per variable.
#'   The attribute `total` holds the statistic of each sample (Q or
#'   \eqn{T^2}, before any reference is subtracted). Draw it with
#'   [plot_contributions()].
#'
#' @references
#'  - Wise, B.M., Gallagher, N.B., Bro, R., Shaver, J.M., Windig, W.,
#'    Koch, R.S. (2006). PLS_Toolbox 4.0 for use with MATLAB. Eigenvector
#'    Research, Wenatchee, WA.
#'  - Westerhuis, J.A., Gurden, S.P., Smilde, A.K. (2000). Generalized
#'    contribution plots in multivariate statistical process monitoring.
#'    Chemometrics and Intelligent Laboratory Systems, 51(1):95-114.
#'
#' @seealso [plot_contributions()], [q_residuals()], [plot_outlier_map()]
#' @export
#'
#' @examples
#' minerals <- c("Ca", "Cl", "Cu", "Fe", "Mg", "Mn", "Mo", "P", "K", "Na", "S", "Zn")
#' spectra <- dplyr::select(forageLIBS, -Measurement, -Sample, -dplyr::all_of(minerals))
#' set.seed(1)
#' fit <- robpca(spectra, k = 3)
#' # Q contributions of two outlying samples, relative to the regular ones
#' q <- contributions(fit, data = spectra, samples = c(49, 127), reference = "regular")
#' q[, 1:5]
#'
contributions <- function(model, k = NULL, statistic = c("q", "t2"), data = NULL, samples = NULL,
                          reference = NULL) {
  statistic <- match.arg(statistic)
  parts <- contribution_parts(model, k, data)
  n <- nrow(parts$x)
  ids <- rownames(parts$x) %||% as.character(seq_len(n))
  keep <- if (is.null(samples)) seq_len(n) else contribution_rows(samples, ids, "samples")
  ref <- if (is.null(reference)) {
    NULL
  } else if (identical(reference, "regular")) {
    which(contribution_regular(model, parts))
  } else {
    contribution_rows(reference, ids, "reference")
  }
  if (!is.null(reference) && length(ref) == 0) {
    stop("None of the samples is regular for the model, so there is no reference. ",
         "Are the samples in 'data' preprocessed like the data the model was fitted on ",
         "(for example with center())?", call. = FALSE)
  }
  rows <- sort(unique(c(keep, ref)))
  values <- contribution_values(parts, statistic, rows)
  total <- rowSums(values^2)
  if (!is.null(ref)) {
    values <- sweep(values, 2, colMeans(values[match(ref, rows), , drop = FALSE]))
  }
  values <- values[match(keep, rows), , drop = FALSE]
  colnames(values) <- parts$variables
  out <- tibble::as_tibble(values, .name_repair = "minimal")
  out <- tibble::add_column(out, sample = ids[keep], .before = 1)
  structure(out, total = unname(total[match(keep, rows)]), statistic = statistic, k = parts$k,
            reference = if (is.null(reference)) NULL else if (identical(reference, "regular")) "regular" else ids[ref],
            model = model_label(model),
            class = c("specproc_contributions", class(out)))
}

#' @title Plot Q and T-squared Contributions
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Plots the contributions of the variables to the Q residual or to the
#' \eqn{T^2} of samples (see [contributions()]) against wavelength, one
#' panel per sample, and labels the wavelengths that contribute most, with
#' the emission lines they match when a line list is given.
#'
#' @details
#' Each contribution is drawn red above zero (the sample is higher than the
#' model, or than the reference samples) and blue below. The `top` largest
#' positive and negative local extrema are labeled, as in [plot_loadings()]:
#' in bold with the nearest line of `lines` within `tol` nm. The mean
#' spectrum can be drawn in grey behind each panel, rescaled to it.
#'
#' @param x An object returned by [contributions()].
#' @param samples The samples to show (row numbers of `x`, or names in its
#'   `sample` column). Default is the three with the largest statistic.
#' @inheritParams plot_loadings
#'
#' @return A ggplot object, or a plotly object if `interactive = TRUE`.
#'
#' @seealso [contributions()], [plot_loadings()], [libs_lines()]
#' @export
#'
#' @examples
#' minerals <- c("Ca", "Cl", "Cu", "Fe", "Mg", "Mn", "Mo", "P", "K", "Na", "S", "Zn")
#' spectra <- dplyr::select(forageLIBS, -Measurement, -Sample, -dplyr::all_of(minerals))
#' set.seed(1)
#' fit <- robpca(spectra, k = 3)
#' q <- contributions(fit, data = spectra, samples = c(49, 127), reference = "regular")
#' plot_contributions(q, spectra = spectra)
#'
plot_contributions <- function(x, samples = NULL, top = 10, lines = NULL, tol = 0.1,
                               spectra = NULL, span = 5, interactive = FALSE, title = NULL) {
  if (!inherits(x, "specproc_contributions")) {
    stop("'x' must be returned by contributions().", call. = FALSE)
  }
  check_flag(interactive, "interactive")
  total <- attr(x, "total")
  if (is.null(samples)) {
    rows <- order(total, decreasing = TRUE)[seq_len(min(3, nrow(x)))]
  } else {
    rows <- contribution_rows(samples, x$sample, "samples")
  }
  values <- as.matrix(x[rows, -1, drop = FALSE])
  variables <- colnames(values)
  wavelength <- names_to_wavelength(variables)
  has_wavelength <- length(wavelength) == length(variables)
  if (!has_wavelength) wavelength <- seq_along(variables)
  parts <- list(variables = variables, wavelength = wavelength, has_wavelength = has_wavelength,
                segment = if (has_wavelength) wavelength_segments(wavelength) else rep(1L, length(variables)))
  check_peak_args(top, span, lines, tol, parts)

  statistic <- attr(x, "statistic")
  stat_label <- if (statistic == "q") "Q" else "T²"
  labels <- sprintf("Sample %s (%s = %s)", x$sample[rows], stat_label, signif(total[rows], 3))
  n <- ncol(values)
  curves <- data.frame(
    panel = factor(rep(labels, each = n), levels = labels),
    index = rep(seq_len(n), length(rows)),
    wavelength = rep(wavelength, length(rows)),
    value = as.vector(t(values)),
    segment = rep(parts$segment, length(rows)),
    type = "loadings",
    stringsAsFactors = FALSE
  )
  peaks <- find_loading_peaks(curves, parts, top, span, lines, tol)
  background <- if (is.null(spectra)) NULL else loading_background(spectra, parts, curves)
  if (is.null(title)) {
    reference <- attr(x, "reference")
    title <- paste0(attr(x, "model"), " ", stat_label, " contributions (", attr(x, "k"),
                    " components)",
                    if (is.null(reference)) "" else if (identical(reference, "regular"))
                      ", relative to the regular samples" else
                      paste0(", relative to sample", if (length(reference) > 1) "s", " ",
                             paste(utils::head(reference, 5), collapse = ", "),
                             if (length(reference) > 5) ", ..."))
  }
  x_lab <- if (has_wavelength) "Wavelength (nm)" else "Variable"
  y_lab <- "Contribution"
  if (interactive) {
    plot_loadings_plotly(curves, peaks, background, x_lab, y_lab, title)
  } else {
    plot_loadings_ggplot(curves, peaks, background, parts, x_lab, y_lab, title)
  }
}

#' @export
print.specproc_contributions <- function(x, ...) {
  stat_label <- if (attr(x, "statistic") == "q") "Q" else "T-squared"
  cat("# ", stat_label, " contributions of ", nrow(x), " sample(s) to ", ncol(x) - 1,
      " variables (", attr(x, "k"), " components)", sep = "")
  if (!is.null(attr(x, "reference"))) cat(", relative to reference samples")
  cat("\n")
  print(tibble::as_tibble(unclass_contributions(x)), ...)
  invisible(x)
}

# ---- internals ---------------------------------------------------------------

unclass_contributions <- function(x) {
  attributes(x)[c("total", "statistic", "k", "reference", "model")] <- NULL
  class(x) <- setdiff(class(x), "specproc_contributions")
  x
}

# Preprocessed data, loadings, score variances and variables of a model.
contribution_parts <- function(model, k, data) {
  if (inherits(model, "prcomp")) {
    rank <- ncol(model$x)
    if (is.null(k)) stop("'k' is required for a prcomp fit.", call. = FALSE)
    check_count(k, "k")
    if (k >= rank) {
      stop("'k' must be smaller than the number of components (", rank, ").", call. = FALSE)
    }
    variables <- rownames(model$rotation) %||% paste0("V", seq_len(nrow(model$rotation)))
    if (is.null(data)) {
      if (length(model$sdev) > rank) {
        stop("'model' must keep all its components (do not set `rank.` in prcomp()), ",
             "or give 'data'.", call. = FALSE)
      }
      # the calibration data, as the model sees them
      x <- model$x %*% t(model$rotation)
    } else {
      raw <- contribution_data(data, variables)
      x <- scale(raw, center = if (isFALSE(model$center)) FALSE else model$center,
                 scale = if (isFALSE(model$scale)) FALSE else model$scale)
      if (nrow(x) == nrow(model$x)) {
        check_calibration_match(x %*% model$rotation[, 1, drop = FALSE], model$x[, 1])
      }
    }
    loadings <- model$rotation[, seq_len(k), drop = FALSE]
    variance <- model$sdev[seq_len(k)]^2
  } else if (inherits(model, "specproc_robpca")) {
    if (is.null(k)) k <- model$k
    check_count(k, "k")
    if (k > model$k) stop("'k' must be at most ", model$k, ".", call. = FALSE)
    variables <- attr(model, "variables") %||% rownames(model$loadings)
    if (is.null(data)) {
      if (is.null(model$imputed)) {
        stop("'data' is required: robpca() and rospca() fits do not keep their data.", call. = FALSE)
      }
      x <- as.matrix(model$imputed)
    } else {
      x <- contribution_data(data, variables)
      if (anyNA(x)) stop("'data' contains missing values.", call. = FALSE)
    }
    x <- sweep(sweep(x, 2, model$center), 2, model$scale, "/")
    if (!is.null(data) && nrow(x) == length(model$od)) {
      check_calibration_match(orthogonal_distance(x, model$loadings), model$od)
    }
    loadings <- model$loadings[, seq_len(k), drop = FALSE]
    variance <- model$eigenvalues[seq_len(k)]
  } else {
    stop("'model' must be a prcomp fit or returned by robpca(), rospca() or macropca().",
         call. = FALSE)
  }
  list(x = x, raw = if (inherits(model, "prcomp") && !is.null(data)) raw else NULL,
       loadings = unname(loadings), variance = variance, k = as.integer(k),
       variables = variables, calibration = is.null(data))
}

# Warns when data with as many rows as the calibration data do not give the
# same values: most often, data not preprocessed like the calibration data.
check_calibration_match <- function(values, calibration) {
  values <- as.vector(values)
  calibration <- as.vector(calibration)
  scale <- max(abs(calibration), .Machine$double.eps)
  if (max(abs(values - calibration)) > 1e-6 * scale) {
    warning("'data' has as many samples as the calibration data but does not match them: ",
            "are they preprocessed like the data the model was fitted on ",
            "(for example with center())? Contributions are computed as for new samples.",
            call. = FALSE)
  }
  invisible(TRUE)
}

contribution_data <- function(data, variables) {
  x <- data
  if (!is.null(colnames(x)) && all(variables %in% colnames(x))) {
    x <- x[, variables, drop = FALSE]
  } else if (ncol(x) != length(variables)) {
    stop("'data' must have the ", length(variables), " variables of the model.", call. = FALSE)
  }
  as_numeric_matrix(x, "data")
}

contribution_values <- function(parts, statistic, rows) {
  x <- parts$x[rows, , drop = FALSE]
  p <- parts$loadings
  scores <- x %*% p
  if (statistic == "q") {
    x - scores %*% t(p)
  } else {
    sweep(scores, 2, sqrt(parts$variance), "/") %*% t(p)
  }
}

# Regular samples: of the robust fit, or at the highest confidence level of
# q_residuals() for a prcomp fit.
contribution_regular <- function(model, parts) {
  if (inherits(model, "prcomp")) {
    q_residuals(model, parts$k, newdata = parts$raw)$outlier == "regular"
  } else if (parts$calibration) {
    model$outlier_type == "regular"
  } else {
    # the preprocessed data: the distances of predict()
    scores <- parts$x %*% model$loadings
    od <- orthogonal_distance(parts$x, model$loadings)
    sd <- sqrt(rowSums(sweep(scores^2, 2, model$eigenvalues, "/")))
    outlier_type(sd, od, model$cutoff_sd, model$cutoff_od) == "regular"
  }
}

contribution_rows <- function(select, ids, arg) {
  idx <- if (is.character(select)) match(select, ids) else select
  if (!is.numeric(idx) || length(idx) == 0 || anyNA(idx) || any(idx < 1 | idx > length(ids)) ||
      any(idx %% 1 != 0)) {
    stop("'", arg, "' must be row numbers or names of the samples.", call. = FALSE)
  }
  as.integer(idx)
}
