#' @title Savitzky-Golay Smoothing and Derivatives
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Smooths spectra, or computes their first or second derivative, with the
#' Savitzky-Golay filter: a polynomial of degree `order` fitted by least
#' squares in a moving window of `window` channels.
#'
#' @details
#' Smoothing reduces noise, at the cost of broadening narrow lines when the
#' window is wide compared with the line width (a higher `order` preserves
#' the line shape better, and smooths less). Derivatives remove constant
#' (first derivative) or linear (second derivative) baselines and separate
#' overlapping features, but amplify noise, which the polynomial fit
#' counteracts.
#'
#' Every channel is kept: at the ends of a spectrum, the values come from
#' the polynomial fitted to the first (or last) `window` channels. When
#' `segments = TRUE`, the spectrum is split between the detectors of a
#' multi-spectrometer system (where the wavelengths step back, or jump by
#' more than 5 times the median spacing), and each segment is filtered on
#' its own; segments shorter than `window` are returned as `NA`, with a
#' warning.
#'
#' Derivatives are per channel. For derivatives per nm, divide the first
#' derivative by the channel spacing (and the second by its square).
#'
#' @param x A numeric matrix or data frame of spectra, one per row, or a
#'   numeric vector (one spectrum). Their names are the wavelengths.
#' @param window The width of the window, an odd number of channels.
#'   Default is 11.
#' @param order The degree of the polynomial, smaller than `window`.
#'   Default is 2.
#' @param derivative The derivative: 0 (smoothing, default), 1 or 2. It
#'   must not exceed `order`.
#' @param segments A logical: filter the segments between gaps of the
#'   wavelength axis separately (`TRUE`, default). Needs wavelengths as
#'   names.
#'
#' @return The filtered spectra, with the same shape and names as `x` (a
#'   tibble for a data frame).
#'
#' @references
#'  - Savitzky, A., Golay, M.J.E. (1964). Smoothing and differentiation of
#'    data by simplified least squares procedures. Analytical Chemistry,
#'    36(8):1627-1639.
#'  - Rinnan, A., van den Berg, F., Engelsen, S.B. (2009). Review of the
#'    most common pre-processing techniques for near-infrared spectra.
#'    Trends in Analytical Chemistry, 28(10):1201-1222.
#'
#' @seealso [step_savgol()]
#' @export savitzky_golay
#'
#' @examples
#' data(forageLIBS)
#' spectrum <- unlist(forageLIBS[1, -(1:14)])
#' smooth <- savitzky_golay(spectrum, window = 7)
#' first <- savitzky_golay(spectrum, window = 11, derivative = 1)
#' wl <- as.numeric(names(spectrum))
#' keep <- wl > 400 & wl < 410
#' plot(wl[keep], spectrum[keep], type = "l", col = "grey", xlab = "Wavelength (nm)",
#'      ylab = "Intensity")
#' lines(wl[keep], smooth[keep], col = "blue")
savitzky_golay <- function(x, window = 11, order = 2, derivative = 0, segments = TRUE) {
  check_savgol(window, order, derivative)
  check_flag(segments, "segments")
  vector_input <- is.numeric(x) && is.null(dim(x))
  df_input <- is.data.frame(x)
  mat <- if (vector_input) matrix(x, nrow = 1, dimnames = list(NULL, names(x))) else
    as_numeric_matrix(x, "x")
  out <- savgol_matrix(mat, window, order, derivative, segments)
  if (vector_input) return(stats::setNames(out[1, ], names(x)))
  if (df_input) return(tibble::as_tibble(as.data.frame(out, check.names = FALSE)))
  out
}

#' @title Savitzky-Golay Recipe Step
#'
#' @description
#' `step_savgol()` creates a *specification* of a recipe step that smooths
#' the spectra, or computes their derivative, with [savitzky_golay()].
#'
#' @details
#' The selected columns form one spectrum per row, and are replaced by the
#' filtered values. Nothing is estimated from the training data. The
#' `window`, `order` and `derivative` can be tuned with `tune::tune()`
#' (dials parameters `window_size()`, `degree_int()` and
#' `savgol_derivative()`); an even `window`, as a tuning grid may propose,
#' is increased to the next odd number. [tidy()][recipes::tidy.recipe] returns the
#' `terms`, `window`, `order`, `derivative` and `id`.
#'
#' @inherit step_baseline return
#' @inheritParams step_baseline
#' @inheritParams savitzky_golay
#'
#' @seealso [savitzky_golay()], [step_snv()]
#' @export
#'
#' @examples
#' if (rlang::is_installed("recipes")) {
#'   data(forageLIBS)
#'   rec <- recipes::recipe(K ~ ., data = forageLIBS[-c(1:10, 12:14)]) |>
#'     step_savgol(recipes::all_predictors(), window = 11, derivative = 1) |>
#'     recipes::prep()
#'   recipes::tidy(rec, number = 1)
#' }
step_savgol <- function(recipe, ..., window = 11, order = 2, derivative = 0, segments = TRUE,
                        role = NA, trained = FALSE, columns = NULL, skip = FALSE,
                        id = recipes::rand_id("savgol")) {
  rlang::check_installed("recipes")
  recipes::add_step(recipe, specproc_step_new(
    "savgol", terms = rlang::enquos(...), role = role, trained = trained, window = window,
    order = order, derivative = derivative, segments = segments, columns = columns,
    skip = skip, id = id
  ))
}

#' @title Derivative Order of a Savitzky-Golay Filter
#'
#' @description
#' A dials parameter for the `derivative` of [step_savgol()]: 0 (smoothing),
#' 1 or 2.
#'
#' @param range The range of derivative orders. Default is 0 to 2.
#' @param trans Not used.
#'
#' @return A dials `quant_param` object.
#' @seealso [step_savgol()]
#' @export
savgol_derivative <- function(range = c(0L, 2L), trans = NULL) {
  rlang::check_installed("dials")
  dials::new_quant_param(type = "integer", range = range, inclusive = c(TRUE, TRUE),
                         trans = trans, label = c(savgol_derivative = "Derivative order"),
                         finalize = NULL)
}

# ---- internals ---------------------------------------------------------------

check_savgol <- function(window, order, derivative) {
  if (!is.numeric(window) || length(window) != 1 || is.na(window) || window < 3 ||
      window %% 2 != 1) {
    stop("'window' must be an odd number of channels, at least 3.", call. = FALSE)
  }
  check_count(order, "order", lower = 0)
  if (order >= window) stop("'order' must be smaller than 'window'.", call. = FALSE)
  if (!derivative %in% 0:2) stop("'derivative' must be 0, 1 or 2.", call. = FALSE)
  if (derivative > order) stop("'derivative' must not exceed 'order'.", call. = FALSE)
  invisible(TRUE)
}

# Weights of the Savitzky-Golay filter: row i gives the value (or derivative)
# at position i of a window of `window` points.
savgol_weights <- function(window, order, derivative) {
  z <- seq_len(window) - (window + 1) / 2
  a <- outer(z, 0:order, `^`)
  ad <- outer(z, 0:order, function(zz, k) {
    ifelse(k >= derivative, factorial(k) / factorial(pmax(k - derivative, 0)) *
             zz^pmax(k - derivative, 0), 0)
  })
  ad %*% solve(crossprod(a), t(a))
}

# Filters the rows of a matrix, segment by segment.
savgol_matrix <- function(x, window, order, derivative, segments) {
  out <- matrix(NA_real_, nrow(x), ncol(x), dimnames = dimnames(x))
  bounds <- list(seq_len(ncol(x)))
  if (segments && !is.null(colnames(x))) {
    wl <- parse_wavelength(colnames(x))
    if (!anyNA(wl) && ncol(x) > 2) {
      bounds <- split(seq_len(ncol(x)), wavelength_segments(wl))
    }
  }
  weights <- savgol_weights(window, order, derivative)
  h <- (window - 1) / 2
  short <- 0
  for (idx in bounds) {
    m <- length(idx)
    if (m < window) {
      short <- short + 1
      next
    }
    xs <- x[, idx, drop = FALSE]
    # interior: convolution with the central weights (stats::filter reverses them)
    y <- t(stats::filter(t(xs), rev(weights[h + 1, ]), sides = 2))
    # edges: the polynomial fitted to the first and last windows
    y[, seq_len(h)] <- xs[, seq_len(window), drop = FALSE] %*% t(weights[seq_len(h), , drop = FALSE])
    last <- (m - window + 1):m
    y[, (m - h + 1):m] <- xs[, last, drop = FALSE] %*%
      t(weights[(h + 2):window, , drop = FALSE])
    out[, idx] <- y
  }
  if (short > 0) {
    warning(short, " segment(s) shorter than the window are set to NA.", call. = FALSE)
  }
  out
}

#' @exportS3Method recipes::prep
prep.step_savgol <- function(x, training, info = NULL, ...) {
  if (is.numeric(x$window) && length(x$window) == 1 && !is.na(x$window) && x$window %% 2 == 0) {
    x$window <- x$window + 1
  }
  check_savgol(x$window, x$order, x$derivative)
  x$columns <- step_predictors(x, training, info)
  x$trained <- TRUE
  x
}

#' @exportS3Method recipes::bake
bake.step_savgol <- function(object, new_data, ...) {
  cols <- object$columns
  recipes::check_new_data(cols, object, new_data)
  if (length(cols) == 0) return(new_data)
  xmat <- step_matrix(new_data, cols)
  new_data[cols] <- as.data.frame(savgol_matrix(xmat, object$window, object$order,
                                                object$derivative, object$segments))
  new_data
}

#' @export
print.step_savgol <- function(x, width = max(20, options()$width - 30), ...) {
  what <- c("Savitzky-Golay smoothing", "Savitzky-Golay first derivative",
            "Savitzky-Golay second derivative")[x$derivative + 1]
  recipes::print_step(x$columns, x$terms, x$trained, paste0(what, " on "), width)
  invisible(x)
}

#' @exportS3Method generics::tidy
tidy.step_savgol <- function(x, ...) {
  terms <- if (recipes::is_trained(x)) x$columns else recipes::sel2char(x$terms)
  tibble::tibble(terms = terms, window = x$window, order = x$order,
                 derivative = x$derivative, id = x$id)
}

#' @exportS3Method generics::tunable
tunable.step_savgol <- function(x, ...) {
  tibble::tibble(
    name = c("window", "order", "derivative"),
    call_info = list(list(pkg = "dials", fun = "window_size", range = c(5L, 21L)),
                     list(pkg = "dials", fun = "degree_int", range = c(1L, 4L)),
                     list(pkg = "specProc", fun = "savgol_derivative")),
    source = "recipe", component = "step_savgol", component_id = x$id
  )
}

#' @exportS3Method generics::required_pkgs
required_pkgs.step_savgol <- function(x, ...) {
  c("specProc")
}
