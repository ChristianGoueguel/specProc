#' @title Wavelet Denoising of Spectra
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Removes noise from spectra by thresholding their undecimated wavelet
#' coefficients (translation-invariant wavelet shrinkage). Unlike a
#' smoothing filter, it keeps the height and width of narrow lines.
#'
#' @details
#' The spectrum is decomposed by the maximal overlap discrete wavelet
#' transform (MODWT, Percival and Walden, 2000), the undecimated version of
#' the transform of [wavelet_features()]: every level has one detail
#' coefficient per channel. The details of the levels 1 to `level` are
#' thresholded, and the spectrum is rebuilt from them and the smooth part at
#' `level`, which keeps the continuum and the broad features. Thresholding
#' the undecimated transform is the translation-invariant denoising of
#' Coifman and Donoho (1995), the average of the denoised decimated
#' transforms of all the shifts of the spectrum: unlike thresholding the
#' decimated transform, it gives no artifacts that depend on where a line
#' falls on the channel grid.
#'
#' The noise \eqn{\sigma_j} of each level \eqn{j} is estimated in each
#' spectrum as the median absolute detail of that level divided by 0.6745,
#' which allows for noise correlated between neighboring channels
#' (Johnstone and Silverman, 1997). The median is not affected by the lines
#' as long as they cover less than half of the channels. The threshold of a
#' level is
#'  - `threshold = "sure"` (default): the SureShrink threshold, which
#'    minimizes Stein's unbiased estimate of the risk of soft thresholding
#'    at that level, or the universal threshold when the details of the
#'    level are sparse (Donoho and Johnstone, 1995). The undecimated
#'    details are correlated, so the risk estimate is approximate;
#'  - `threshold = "universal"`: \eqn{\sigma_j \sqrt{2 \log n}} for \eqn{n}
#'    channels (Donoho and Johnstone, 1994), about 4.2 times the noise for
#'    7000 channels. It removes nearly all the noise, but also the weak
#'    lines.
#'
#' With `rule = "hard"` (default), the details above the threshold are kept
#' unchanged, so the lines keep their height. With `rule = "soft"`, they are
#' shrunk by the threshold: a smoother result, but lower lines.
#'
#' The default settings suit spectra whose lines are a few channels wide. In
#' the single shots of [forageShots], whose lines are 2 to 3 channels wide
#' (FWHM), the noise is correlated between neighboring channels: more than
#' half of it is at level 2, the scale of the lines. Compared with the mean
#' of the other shots of each sample, the defaults lowered the error of the
#' continuum by about 10%, while the lines 3 to 15 times higher than the
#' noise lost about 4% of their height and the strong lines 0.2%; the
#' universal threshold lowered the weak lines by a third. A Savitzky-Golay
#' filter of 5 channels ([savitzky_golay()]) lowered the strong lines by
#' 10%. Because the noise has the scale of the lines, no filter separates
#' them well in such spectra, and the PLS calibrations of the averaged
#' spectra of [forageLIBS] hardly change after denoising. Spectra with
#' wider lines and noise at finer scales gain more, with a higher `level`.
#'
#' The ends of the spectrum are extended by reflection. With `segments =
#' TRUE`, the spectrum is split between the detectors of a multi-spectrometer
#' system, as in [savitzky_golay()], and each segment is denoised on its
#' own. A segment shorter than the wavelet filter at `level`,
#' \eqn{(2^{level} - 1)(L - 1) + 1} channels for a filter of \eqn{L}
#' coefficients, is denoised with fewer levels, and a segment shorter than
#' the filter itself is returned unchanged. A segment with missing values is
#' returned unchanged, with a warning.
#'
#' The threshold follows the typical noise of a spectrum, mostly that of the
#' continuum. In LIBS, the noise grows with the intensity, so the noise on
#' top of strong lines is mostly kept, and weak lines whose details do not
#' exceed the threshold are removed with the noise. Denoising lowers the
#' apparent noise of blank spectra: compute detection limits (as with
#' [calibration_curve()]) from the spectra before denoising.
#'
#' @param x A numeric matrix or data frame of spectra, one per row, or a
#'   numeric vector (one spectrum). Their names are the wavelengths.
#' @param wavelet The wavelet: `"la8"` (default; Daubechies' least
#'   asymmetric wavelet with 8 coefficients, whose nearly symmetric shape
#'   suits spectral lines), `"haar"`, `"d4"`, `"d6"` or `"d8"` (see
#'   [wavelet_features()]).
#' @param level The number of levels thresholded. Default is 2: the scales
#'   of 2 to 4 channels.
#' @param threshold The threshold: `"sure"` (default) or `"universal"`.
#' @param rule The thresholding rule: `"hard"` (default) or `"soft"`.
#' @param segments A logical: denoise the segments between gaps of the
#'   wavelength axis separately (`TRUE`, default). Needs wavelengths as
#'   names.
#'
#' @return The denoised spectra, with the same shape and names as `x` (a
#'   tibble for a data frame).
#'
#' @references
#'  - Donoho, D.L., Johnstone, I.M. (1994). Ideal spatial adaptation by
#'    wavelet shrinkage. Biometrika, 81(3):425-455.
#'  - Donoho, D.L., Johnstone, I.M. (1995). Adapting to unknown smoothness
#'    via wavelet shrinkage. Journal of the American Statistical
#'    Association, 90(432):1200-1224.
#'  - Coifman, R.R., Donoho, D.L. (1995). Translation-invariant de-noising.
#'    In: Antoniadis, A., Oppenheim, G. (eds), Wavelets and Statistics.
#'    Lecture Notes in Statistics, 103:125-150. Springer, New York.
#'  - Johnstone, I.M., Silverman, B.W. (1997). Wavelet threshold estimators
#'    for data with correlated noise. Journal of the Royal Statistical
#'    Society: Series B, 59(2):319-351.
#'  - Percival, D.B., Walden, A.T. (2000). Wavelet Methods for Time Series
#'    Analysis. Cambridge University Press, Cambridge.
#'
#' @seealso [step_wavelet_denoise()], [savitzky_golay()], [wavelet_features()]
#' @export wavelet_denoise
#'
#' @examples
#' data(forageShots)
#' # one laser shot, around the Mn I triplet at 403 nm
#' shot <- unlist(forageShots[1, -(1:5)])
#' clean <- wavelet_denoise(shot)
#' wl <- as.numeric(names(shot))
#' keep <- wl > 400 & wl < 408
#' plot(wl[keep], shot[keep], type = "l", col = "grey", xlab = "Wavelength (nm)",
#'      ylab = "Intensity")
#' lines(wl[keep], clean[keep], col = "blue")
wavelet_denoise <- function(x, wavelet = "la8", level = 2, threshold = "sure", rule = "hard",
                            segments = TRUE) {
  wavelet <- match.arg(wavelet, names(wavelet_filters))
  threshold <- match.arg(threshold, c("sure", "universal"))
  rule <- match.arg(rule, c("hard", "soft"))
  check_count(level, "level")
  check_flag(segments, "segments")
  vector_input <- is.numeric(x) && is.null(dim(x))
  df_input <- is.data.frame(x)
  mat <- if (vector_input) matrix(x, nrow = 1, dimnames = list(NULL, names(x))) else
    as_numeric_matrix(x, "x")
  out <- denoise_matrix(mat, wavelet, level, threshold, rule, segments)
  if (vector_input) return(stats::setNames(out[1, ], names(x)))
  if (df_input) return(tibble::as_tibble(as.data.frame(out, check.names = FALSE)))
  out
}

#' @title Wavelet Denoising Recipe Step
#'
#' @description
#' `step_wavelet_denoise()` creates a *specification* of a recipe step that
#' denoises the spectra with [wavelet_denoise()].
#'
#' @details
#' The selected columns form one spectrum per row, and are replaced by the
#' denoised values. Nothing is estimated from the training data: the noise
#' and thresholds are those of each spectrum. The `level` can be tuned with
#' `tune::tune()` (dials parameter `wavelet_level()`, from 1 to 4 by
#' default). [tidy()][recipes::tidy.recipe] returns the `terms`, `wavelet`,
#' `level`, `threshold`, `rule` and `id`.
#'
#' @inherit step_baseline return
#' @inheritParams step_baseline
#' @inheritParams wavelet_denoise
#'
#' @seealso [wavelet_denoise()], [step_savgol()], [step_wavelet()]
#' @export
#'
#' @examples
#' if (rlang::is_installed("recipes")) {
#'   data(forageShots)
#'   rec <- recipes::recipe(K ~ ., data = forageShots[-c(1:4)]) |>
#'     step_wavelet_denoise(recipes::all_predictors()) |>
#'     recipes::prep()
#'   recipes::tidy(rec, number = 1)
#' }
step_wavelet_denoise <- function(recipe, ..., wavelet = "la8", level = 2, threshold = "sure",
                                 rule = "hard", segments = TRUE, role = NA, trained = FALSE,
                                 columns = NULL, skip = FALSE,
                                 id = recipes::rand_id("wavelet_denoise")) {
  rlang::check_installed("recipes")
  wavelet <- match.arg(wavelet, names(wavelet_filters))
  threshold <- match.arg(threshold, c("sure", "universal"))
  rule <- match.arg(rule, c("hard", "soft"))
  check_flag(segments, "segments")
  recipes::add_step(recipe, specproc_step_new(
    "wavelet_denoise", terms = rlang::enquos(...), role = role, trained = trained,
    wavelet = wavelet, level = level, threshold = threshold, rule = rule, segments = segments,
    columns = columns, skip = skip, id = id
  ))
}

# ---- internals ---------------------------------------------------------------

# Denoises the rows of a matrix, segment by segment.
denoise_matrix <- function(x, wavelet, level, threshold, rule, segments) {
  h <- wavelet_filters[[wavelet]]
  out <- x
  missing_rows <- integer(0)
  for (idx in segment_columns(colnames(x), segments, ncol(x))) {
    # the levels whose filter, (2^j - 1)(L - 1) + 1 channels, fits in the segment
    max_level <- floor(log2((length(idx) - 1) / (length(h) - 1) + 1))
    if (max_level < 1) next
    complete <- rowSums(is.na(x[, idx, drop = FALSE])) == 0
    missing_rows <- union(missing_rows, which(!complete))
    if (!any(complete)) next
    out[complete, idx] <- denoise_rows(x[complete, idx, drop = FALSE], h, min(level, max_level),
                                       threshold, rule)
  }
  if (length(missing_rows) > 0) {
    warning("The segments with missing values of ", length(missing_rows),
            " spectra are returned unchanged.", call. = FALSE)
  }
  out
}

# Thresholds the undecimated transform of complete rows. The rows are
# extended by reflection on both sides by the width of the filter at `level`,
# so that the circular transform never wraps around inside the spectrum.
denoise_rows <- function(x, h, level, threshold, rule) {
  n <- ncol(x)
  pad <- (2^level - 1) * (length(h) - 1)
  ext <- cbind(x[, pad:1, drop = FALSE], x, x[, n:(n - pad + 1), drop = FALSE])
  inside <- pad + seq_len(n)
  dec <- modwt_matrix(ext, h, level)
  for (j in seq_len(level)) {
    w <- dec$details[[j]]
    sigma <- col_medians(t(abs(w[, inside, drop = FALSE]))) / stats::qnorm(0.75)
    lambda <- if (threshold == "universal") sigma * sqrt(2 * log(n)) else {
      sigma * vapply(seq_len(nrow(w)), function(i) {
        if (sigma[i] > 0) sure_threshold(w[i, inside] / sigma[i]) else 0
      }, numeric(1))
    }
    dec$details[[j]] <- shrink(w, unname(lambda), rule)
  }
  out <- imodwt_matrix(dec, h)[, inside, drop = FALSE]
  dimnames(out) <- dimnames(x)
  out
}

# MODWT of every row of x (circular): the details of each level and the
# smooth at `level`. Its filters are those of dwt_matrix() divided by
# sqrt(2), and spread by 2^(j - 1) at level j instead of downsampling.
modwt_matrix <- function(x, h, level) {
  g <- rev(h) * (-1)^(seq_along(h) - 1)
  h <- h / sqrt(2)
  g <- g / sqrt(2)
  n <- ncol(x)
  v <- x
  details <- vector("list", level)
  for (j in seq_len(level)) {
    step <- 2^(j - 1)
    smooth <- matrix(0, nrow(v), n)
    detail <- matrix(0, nrow(v), n)
    for (k in seq_along(h)) {
      block <- v[, (seq_len(n) - 1 + step * (k - 1)) %% n + 1, drop = FALSE]
      smooth <- smooth + h[k] * block
      detail <- detail + g[k] * block
    }
    details[[j]] <- detail
    v <- smooth
  }
  list(details = details, smooth = v)
}

# Inverse of modwt_matrix(): its adjoint, level by level.
imodwt_matrix <- function(dec, h) {
  g <- rev(h) * (-1)^(seq_along(h) - 1)
  h <- h / sqrt(2)
  g <- g / sqrt(2)
  v <- dec$smooth
  n <- ncol(v)
  for (j in rev(seq_along(dec$details))) {
    step <- 2^(j - 1)
    w <- dec$details[[j]]
    out <- matrix(0, nrow(v), n)
    for (k in seq_along(h)) {
      cols <- (seq_len(n) - 1 - step * (k - 1)) %% n + 1
      out <- out + h[k] * v[, cols, drop = FALSE] + g[k] * w[, cols, drop = FALSE]
    }
    v <- out
  }
  v
}

# Hard or soft thresholding of each row of w at its threshold lambda.
shrink <- function(w, lambda, rule) {
  lambda <- matrix(lambda, nrow(w), ncol(w))
  if (rule == "hard") w * (abs(w) > lambda) else sign(w) * pmax(abs(w) - lambda, 0)
}

# SureShrink threshold of standardized details u (unit noise): the minimum
# of Stein's unbiased risk estimate of soft thresholding,
# d - 2 #{|u| <= t} + sum(min(u^2, t^2)), over the |u|; the universal
# threshold when the details are sparse (Donoho and Johnstone, 1995).
sure_threshold <- function(u) {
  d <- length(u)
  universal <- sqrt(2 * log(d))
  if ((sum(u^2) - d) / d <= log2(d)^1.5 / sqrt(d)) return(universal)
  a <- sort(u^2)
  k <- seq_len(d)
  risk <- d - 2 * k + cumsum(a) + (d - k) * a
  min(sqrt(a[which.min(risk)]), universal)
}

#' @exportS3Method recipes::prep
prep.step_wavelet_denoise <- function(x, training, info = NULL, ...) {
  check_count(x$level, "level")
  x$columns <- step_predictors(x, training, info)
  x$trained <- TRUE
  x
}

#' @exportS3Method recipes::bake
bake.step_wavelet_denoise <- function(object, new_data, ...) {
  cols <- object$columns
  recipes::check_new_data(cols, object, new_data)
  if (length(cols) == 0) return(new_data)
  xmat <- step_matrix(new_data, cols)
  new_data[cols] <- as.data.frame(denoise_matrix(xmat, object$wavelet, object$level,
                                                 object$threshold, object$rule, object$segments))
  new_data
}

#' @export
print.step_wavelet_denoise <- function(x, width = max(20, options()$width - 30), ...) {
  title <- paste0("Wavelet denoising (", x$wavelet, ", level ", x$level, ") of ")
  recipes::print_step(x$columns, x$terms, x$trained, title, width)
  invisible(x)
}

#' @exportS3Method generics::tidy
tidy.step_wavelet_denoise <- function(x, ...) {
  terms <- if (recipes::is_trained(x)) x$columns else recipes::sel2char(x$terms)
  tibble::tibble(terms = terms, wavelet = x$wavelet, level = x$level, threshold = x$threshold,
                 rule = x$rule, id = x$id)
}

#' @exportS3Method generics::tunable
tunable.step_wavelet_denoise <- function(x, ...) {
  tibble::tibble(
    name = "level",
    call_info = list(list(pkg = "specProc", fun = "wavelet_level", range = c(1L, 4L))),
    source = "recipe", component = "step_wavelet_denoise", component_id = x$id
  )
}

#' @exportS3Method generics::required_pkgs
required_pkgs.step_wavelet_denoise <- function(x, ...) {
  c("specProc")
}
