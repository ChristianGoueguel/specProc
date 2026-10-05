#' @title Gap-Segment Derivatives
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Computes the first or second derivative of spectra by the gap-segment
#' (Norris-Williams) method: the spectrum is averaged over segments of
#' `segment` channels, and the derivative is the difference between segments
#' separated by `gap` channels.
#'
#' @details
#' The first derivative at a channel is the mean of a segment after it minus
#' the mean of a segment before it, the `gap` channels between the two
#' segments being centered on the channel, divided by the distance between
#' the centers of the segments (`segment + gap` channels). The second
#' derivative combines three segments separated by `gap` channels, the
#' middle one centered on the channel: the mean of the first, minus twice
#' the mean of the middle one, plus the mean of the last, divided by the
#' squared distance between the centers. The filter thus spans
#' `2 * segment + gap` channels (first derivative) or
#' `3 * segment + 2 * gap` channels (second derivative).
#'
#' Averaging over a segment smooths the spectrum, and the gap sets the
#' distance over which the difference is taken: unlike the window of
#' [savitzky_golay()], which sets both, the two can be chosen separately.
#' With `segment = 1`, there is no smoothing (Norris gap derivative). A
#' first derivative removes a constant offset, and a second derivative a
#' straight baseline; a curved background, such as the continuum of a LIBS
#' plasma, is reduced but not removed (see [baseline_als()]). Emission lines
#' are only a few channels wide in LIBS spectra: a gap or segment wider than
#' a line merges it with its neighbors, which matters for measuring lines
#' more than for multivariate models of whole spectra.
#'
#' The derivatives are exact for straight lines (first derivative) and
#' parabolas (second derivative), and per channel: for derivatives per nm,
#' divide the first derivative by the channel spacing (and the second by its
#' square). Programs count the gap in different ways: here it is the number
#' of channels between two segments, as the argument `w` of
#' `prospectr::gapDer()`. Norris and Williams (1984) gave the second
#' derivative the opposite sign.
#'
#' Every channel is kept: the first and last channels, where the filter does
#' not fit, take the derivative of the nearest channel where it fits. When
#' `segments = TRUE`, the spectrum is split between the detectors of a
#' multi-spectrometer system (where the wavelengths step back, or jump by
#' more than 5 times the median spacing), and each segment is filtered on
#' its own; segments shorter than the filter are returned as `NA`, with a
#' warning.
#'
#' @param x A numeric matrix or data frame of spectra, one per row, or a
#'   numeric vector (one spectrum). Their names are the wavelengths.
#' @param derivative The derivative: 1 (default) or 2.
#' @param gap The number of channels between two segments, an odd number.
#'   Default is 5.
#' @param segment The number of channels averaged in each segment, an odd
#'   number. Default is 3.
#' @param segments A logical: filter the segments between gaps of the
#'   wavelength axis separately (`TRUE`, default). Needs wavelengths as
#'   names.
#'
#' @return The derivatives, with the same shape and names as `x` (a tibble
#'   for a data frame).
#'
#' @references
#'  - Norris, K.H., Williams, P.C. (1984). Optimization of mathematical
#'    treatments of raw near-infrared signal in the measurement of protein
#'    in hard red spring wheat. I. Influence of particle size. Cereal
#'    Chemistry, 61(2):158-165.
#'  - Rinnan, A., van den Berg, F., Engelsen, S.B. (2009). Review of the
#'    most common pre-processing techniques for near-infrared spectra.
#'    Trends in Analytical Chemistry, 28(10):1201-1222.
#'  - Vrábel, J., Képeš, E., Duponchel, L., et al. (2020). Classification of
#'    challenging laser-induced breakdown spectroscopy soil sample data -
#'    EMSLIBS contest. Spectrochimica Acta Part B, 169:105872.
#'
#' @seealso [step_gap_derivative()], [savitzky_golay()], [bin_spectra()]
#' @export gap_derivative
#'
#' @examples
#' data(forageLIBS)
#' spectrum <- unlist(forageLIBS[1, -(1:14)])
#' wl <- as.numeric(names(spectrum))
#' keep <- wl > 400 & wl < 410
#' norris <- gap_derivative(spectrum[keep], gap = 5, segment = 3)
#' savgol <- savitzky_golay(spectrum[keep], window = 11, derivative = 1)
#' plot(wl[keep], norris, type = "l", xlab = "Wavelength (nm)",
#'      ylab = "First derivative")
#' lines(wl[keep], savgol, col = "blue")
gap_derivative <- function(x, derivative = 1, gap = 5, segment = 3, segments = TRUE) {
  check_gap_derivative(derivative, gap, segment)
  check_flag(segments, "segments")
  vector_input <- is.numeric(x) && is.null(dim(x))
  df_input <- is.data.frame(x)
  mat <- if (vector_input) matrix(x, nrow = 1, dimnames = list(NULL, names(x))) else
    as_numeric_matrix(x, "x")
  out <- gap_derivative_matrix(mat, derivative, gap, segment, segments)
  if (vector_input) return(stats::setNames(out[1, ], names(x)))
  if (df_input) return(tibble::as_tibble(as.data.frame(out, check.names = FALSE)))
  out
}

#' @title Gap-Segment Derivative Recipe Step
#'
#' @description
#' `step_gap_derivative()` creates a *specification* of a recipe step that
#' computes the gap-segment (Norris-Williams) derivative of the spectra with
#' [gap_derivative()].
#'
#' @details
#' The selected columns form one spectrum per row, and are replaced by their
#' derivative. Nothing is estimated from the training data. The `derivative`,
#' `gap` and `segment` can be tuned with `tune::tune()` (dials parameters
#' `savgol_derivative()`, with values 1 and 2, and `window_size()`); an even
#' `gap` or `segment`, as a tuning grid may propose, is increased to the next
#' odd number. [tidy()][recipes::tidy.recipe] returns the `terms`,
#' `derivative`, `gap`, `segment` and `id`.
#'
#' @inherit step_baseline return
#' @inheritParams step_baseline
#' @inheritParams gap_derivative
#'
#' @seealso [gap_derivative()], [step_savgol()], [step_bin_spectra()]
#' @export
#'
#' @examples
#' if (rlang::is_installed("recipes")) {
#'   data(forageLIBS)
#'   rec <- recipes::recipe(K ~ ., data = forageLIBS[-c(1:10, 12:14)]) |>
#'     step_gap_derivative(recipes::all_predictors(), gap = 5, segment = 3) |>
#'     recipes::prep()
#'   recipes::tidy(rec, number = 1)
#' }
step_gap_derivative <- function(recipe, ..., derivative = 1, gap = 5, segment = 3,
                                segments = TRUE, role = NA, trained = FALSE, columns = NULL,
                                skip = FALSE, id = recipes::rand_id("gap_derivative")) {
  rlang::check_installed("recipes")
  recipes::add_step(recipe, specproc_step_new(
    "gap_derivative", terms = rlang::enquos(...), role = role, trained = trained,
    derivative = derivative, gap = gap, segment = segment, segments = segments,
    columns = columns, skip = skip, id = id
  ))
}

# ---- internals ---------------------------------------------------------------

check_gap_derivative <- function(derivative, gap, segment) {
  if (!is.numeric(derivative) || length(derivative) != 1 || !derivative %in% 1:2) {
    stop("'derivative' must be 1 or 2.", call. = FALSE)
  }
  values <- list(gap = gap, segment = segment)
  for (arg in names(values)) {
    v <- values[[arg]]
    if (!is.numeric(v) || length(v) != 1 || is.na(v) || v < 1 || v %% 2 != 1) {
      stop("'", arg, "' must be an odd number of channels, at least 1.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

# Weights of the gap-segment filter, exact for polynomials of degree
# `derivative`.
gap_weights <- function(derivative, gap, segment) {
  seg <- rep(1, segment)
  zero <- rep(0, gap)
  d <- segment + gap  # distance between the centers of two segments
  if (derivative == 1) {
    c(-seg, zero, seg) / (segment * d)
  } else {
    c(seg, zero, -2 * seg, zero, seg) / (segment * d^2)
  }
}

# Differentiates the rows of a matrix, segment by segment.
gap_derivative_matrix <- function(x, derivative, gap, segment, segments) {
  out <- matrix(NA_real_, nrow(x), ncol(x), dimnames = dimnames(x))
  weights <- gap_weights(derivative, gap, segment)
  len <- length(weights)
  h <- (len - 1) / 2
  short <- 0
  for (idx in segment_columns(colnames(x), segments, ncol(x))) {
    m <- length(idx)
    if (m < len) {
      short <- short + 1
      next
    }
    # stats::filter() reverses the weights
    y <- t(stats::filter(t(x[, idx, drop = FALSE]), rev(weights), sides = 2))
    if (h > 0) {
      y[, seq_len(h)] <- y[, h + 1]
      y[, (m - h + 1):m] <- y[, m - h]
    }
    out[, idx] <- y
  }
  if (short > 0) {
    warning(short, " segment(s) shorter than the filter are set to NA.", call. = FALSE)
  }
  out
}

# An even value from a tuning grid, increased to the next odd number.
next_odd <- function(v) {
  if (is.numeric(v) && length(v) == 1 && !is.na(v) && v %% 2 == 0) v + 1 else v
}

#' @exportS3Method recipes::prep
prep.step_gap_derivative <- function(x, training, info = NULL, ...) {
  x$gap <- next_odd(x$gap)
  x$segment <- next_odd(x$segment)
  check_gap_derivative(x$derivative, x$gap, x$segment)
  x$columns <- step_predictors(x, training, info)
  x$trained <- TRUE
  x
}

#' @exportS3Method recipes::bake
bake.step_gap_derivative <- function(object, new_data, ...) {
  cols <- object$columns
  recipes::check_new_data(cols, object, new_data)
  if (length(cols) == 0) return(new_data)
  xmat <- step_matrix(new_data, cols)
  new_data[cols] <- as.data.frame(gap_derivative_matrix(xmat, object$derivative, object$gap,
                                                        object$segment, object$segments))
  new_data
}

#' @export
print.step_gap_derivative <- function(x, width = max(20, options()$width - 30), ...) {
  what <- c("Gap-segment first derivative", "Gap-segment second derivative")[x$derivative]
  recipes::print_step(x$columns, x$terms, x$trained, paste0(what, " on "), width)
  invisible(x)
}

#' @exportS3Method generics::tidy
tidy.step_gap_derivative <- function(x, ...) {
  terms <- if (recipes::is_trained(x)) x$columns else recipes::sel2char(x$terms)
  tibble::tibble(terms = terms, derivative = x$derivative, gap = x$gap,
                 segment = x$segment, id = x$id)
}

#' @exportS3Method generics::tunable
tunable.step_gap_derivative <- function(x, ...) {
  tibble::tibble(
    name = c("derivative", "gap", "segment"),
    call_info = list(list(pkg = "specProc", fun = "savgol_derivative", range = c(1L, 2L)),
                     list(pkg = "dials", fun = "window_size", range = c(1L, 11L)),
                     list(pkg = "dials", fun = "window_size", range = c(1L, 7L))),
    source = "recipe", component = "step_gap_derivative", component_id = x$id
  )
}

#' @exportS3Method generics::required_pkgs
required_pkgs.step_gap_derivative <- function(x, ...) {
  c("specProc")
}
