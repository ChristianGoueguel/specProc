#' @title Intensities of Emission Lines
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Measures the intensity of emission lines in spectra: for each line, the
#' peak is searched near its tabulated wavelength, and its area or height is
#' measured. The result feeds [boltzmann()], [saha_boltzmann()],
#' [cf_libs()] and [calibration_curve()].
#'
#' @details
#' For each spectrum and line, the peak is the channel of highest intensity
#' within `search` nm of the tabulated wavelength, which absorbs small
#' errors of the wavelength calibration. The intensity is then measured on
#' the channels within `half_width` nm of the peak:
#'  - `method = "area"` (default): the integrated intensity (trapezoidal
#'    rule), in intensity units times nm;
#'  - `method = "height"`: the peak intensity;
#'  - `method = "voigt"`: the area of a Voigt profile fitted with
#'    [peak_fit()] to the channels within `fit_width` nm of the peak. It
#'    separates the line from a flat background and is less sensitive to the
#'    window, but it is slower and needs a line well sampled by the
#'    detector.
#'
#' With `baseline = TRUE`, a straight line through the intensities at both
#' ends of the window is subtracted before the area or height is measured
#' (the Voigt fit always includes a constant background).
#'
#' Each line is also checked:
#'  - `snr`: the height above the baseline divided by the noise of the
#'    spectrum, estimated robustly from its channel-to-channel differences;
#'    `detected` is `snr >= min_snr`.
#'  - `saturated`: with `limit`, whether a channel of the window reaches
#'    the saturation limit of the detector (see [saturation_summary()]).
#'
#' @param spectra The spectra: a numeric vector named by wavelength (one
#'   spectrum), or a matrix or data frame with one spectrum per row and the
#'   wavelengths as column names. Other columns of a data frame (such as
#'   sample names) are kept in the result.
#' @param lines The lines: a numeric vector of wavelengths (nm), whose names
#'   are used as labels, or a data frame with a `wavelength` column, such as
#'   returned by [nist_lines()], whose columns are kept in the result.
#' @param half_width The half-width of the integration window around the
#'   peak, in nm. Default is 0.15.
#' @param search The half-width of the window in which the peak is
#'   searched around the tabulated wavelength, in nm. Default is 0.2.
#' @param method `"area"` (default), `"height"` or `"voigt"`.
#' @param baseline A logical: subtract a linear baseline under each line
#'   (`FALSE`, default).
#' @param fit_width The half-width of the window of the Voigt fit, in nm.
#'   Default is 3 times `half_width`.
#' @param limit The saturation limit of the detector, in counts, or `NULL`
#'   (default) not to check saturation.
#' @param min_snr The signal-to-noise ratio above which a line is detected.
#'   Default is 3.
#'
#' @return A tibble with one row per spectrum and line: the other columns
#'   of `spectra` (or `spectrum`, the row number), the columns of `lines`
#'   (or `line` and `wavelength`), and `peak_wavelength`, `shift` (nm),
#'   `height`, `intensity`, `snr`, `detected` and `saturated` (which
#'   replace the columns of `lines` of the same name, such as the relative
#'   intensity listed by [nist_lines()]). For a single
#'   spectrum, it has one row per line, like `lines`, with the measured
#'   `intensity`.
#'
#' @seealso [step_line_intensities()], [nist_lines()], [cf_libs()],
#'   [calibration_curve()], [peak_fit()]
#' @export line_intensities
#'
#' @examples
#' data(forageLIBS)
#' spectra <- forageLIBS[1:8, -(1:14)]
#' k <- c(`K I 404.41` = 404.414, `K I 404.72` = 404.721, `Mg I 518.36` = 518.360)
#' line_intensities(spectra, k, baseline = TRUE)
#'
#' # one spectrum and a table of lines, ready for a Boltzmann plot
#' lines <- data.frame(wavelength = c(428.30, 430.25, 443.50, 445.48),
#'                     Aki = c(4.34e7, 1.36e8, 6.70e7, 8.70e7), gk = c(5, 5, 5, 7),
#'                     Ek = c(4.78, 4.78, 4.68, 4.68))
#' mean_spectrum <- colMeans(forageLIBS[-(1:14)])
#' line_intensities(mean_spectrum, lines, baseline = TRUE)
line_intensities <- function(spectra, lines, half_width = 0.15, search = 0.2, method = "area",
                             baseline = FALSE, fit_width = 3 * half_width, limit = NULL,
                             min_snr = 3) {
  method <- match.arg(method, c("area", "height", "voigt"))
  check_number(half_width, "half_width", lower = 0, lower_open = TRUE)
  check_number(search, "search", lower = 0)
  check_number(fit_width, "fit_width", lower = 0, lower_open = TRUE)
  check_flag(baseline, "baseline")
  check_number(min_snr, "min_snr", lower = 0)
  if (!is.null(limit)) check_number(limit, "limit")
  sp <- line_spectra(spectra)
  lines <- line_table(lines)

  x <- sp$x
  wl <- sp$wavelength
  noise <- apply(x, 1, spectrum_noise)
  results <- lapply(seq_len(nrow(lines)), function(j) {
    m <- measure_line(x, wl, lines$wavelength[j], half_width, search, method, baseline,
                      fit_width, limit)
    m$snr <- m$height / noise
    m$line_row <- j
    m$spectrum_row <- seq_len(nrow(x))
    m
  })
  res <- do.call(rbind, results)
  res$detected <- !is.na(res$snr) & res$snr >= min_snr
  measured <- c("peak_wavelength", "shift", "height", "intensity", "snr", "detected", "saturated")
  # measured columns replace those of the same name (such as the relative
  # intensity of nist_lines())
  lines <- lines[setdiff(names(lines), measured)]
  ids <- sp$ids[setdiff(names(sp$ids), c(measured, names(lines)))]
  out <- tibble::as_tibble(cbind(
    ids[res$spectrum_row, , drop = FALSE],
    lines[res$line_row, , drop = FALSE],
    res[measured]
  ))
  out <- out[order(res$spectrum_row, res$line_row), , drop = FALSE]
  if (sp$single) out$spectrum <- NULL
  out
}

#' @title Emission Line Intensities Recipe Step
#'
#' @description
#' `step_line_intensities()` creates a *specification* of a recipe step
#' that replaces the spectral columns by the intensities of selected
#' emission lines, measured with [line_intensities()].
#'
#' @details
#' The selected columns form one spectrum per row, and their names must be
#' their wavelengths (in nm). Each line becomes a new column, named by
#' `prefix` and the name of the line (or its wavelength). Nothing is
#' estimated from the training data. The Voigt method of
#' [line_intensities()] is not offered, as it is too slow for resampling.
#' [tidy()][recipes::tidy.recipe] returns the `line` names, their
#' `wavelength`, the `method` and `id`.
#'
#' @inheritParams step_baseline
#' @inheritParams line_intensities
#' @param lines The lines: a numeric vector of wavelengths (nm), preferably
#'   named, such as `c(Ca = 393.37, Mg = 279.55)`.
#' @param method `"area"` (default) or `"height"`.
#' @param prefix The prefix of the new column names. Default is `"line_"`.
#' @param keep_original_cols A logical: keep the spectral columns (`FALSE`,
#'   default).
#'
#' @inherit step_baseline return
#' @seealso [line_intensities()], [step_line_ratio()]
#' @export
#'
#' @examples
#' if (rlang::is_installed("recipes")) {
#'   data(forageLIBS)
#'   rec <- recipes::recipe(K ~ ., data = forageLIBS[-c(1:10, 12:14)]) |>
#'     step_line_intensities(recipes::all_predictors(),
#'                           lines = c(K = 769.90, Mg = 285.21, Ca = 317.93)) |>
#'     recipes::prep()
#'   head(recipes::bake(rec, new_data = NULL))
#' }
step_line_intensities <- function(recipe, ..., lines, half_width = 0.15, search = 0.2,
                                  method = "area", baseline = FALSE, prefix = "line_",
                                  keep_original_cols = FALSE, role = "predictor",
                                  trained = FALSE, columns = NULL, skip = FALSE,
                                  id = recipes::rand_id("line_intensities")) {
  rlang::check_installed("recipes")
  if (missing(lines) || !is.numeric(lines) || length(lines) == 0 || anyNA(lines)) {
    stop("'lines' must be a numeric vector of wavelengths (nm).", call. = FALSE)
  }
  method <- match.arg(method, c("area", "height"))
  check_number(half_width, "half_width", lower = 0, lower_open = TRUE)
  check_number(search, "search", lower = 0)
  check_flag(baseline, "baseline")
  check_flag(keep_original_cols, "keep_original_cols")
  recipes::add_step(recipe, specproc_step_new(
    "line_intensities", terms = rlang::enquos(...), role = role, trained = trained,
    lines = lines, half_width = half_width, search = search, method = method,
    baseline = baseline, prefix = prefix, keep_original_cols = keep_original_cols,
    columns = columns, skip = skip, id = id
  ))
}

# ---- internals ---------------------------------------------------------------

# Spectra as a matrix, their wavelengths and the id columns.
line_spectra <- function(spectra) {
  if (is.numeric(spectra) && is.null(dim(spectra))) {
    wl <- parse_wavelength(names(spectra) %||% character(length(spectra)))
    if (length(spectra) < 2 || anyNA(wl)) {
      stop("A single spectrum must be a numeric vector named by its wavelengths.", call. = FALSE)
    }
    x <- matrix(as.numeric(spectra), nrow = 1)
    return(list(x = x, wavelength = wl, ids = data.frame(spectrum = 1L), single = TRUE))
  }
  if (is.matrix(spectra)) spectra <- as.data.frame(spectra, check.names = FALSE)
  if (!is.data.frame(spectra) || nrow(spectra) == 0) {
    stop("'spectra' must be a named numeric vector, a matrix or a data frame.", call. = FALSE)
  }
  wl <- suppressWarnings(as.numeric(names(spectra)))
  channels <- !is.na(wl) & vapply(spectra, is.numeric, logical(1))
  if (sum(channels) < 2) {
    stop("'spectra' must have columns named by their wavelengths.", call. = FALSE)
  }
  x <- as.matrix(spectra[channels])
  storage.mode(x) <- "double"
  ids <- if (any(!channels)) as.data.frame(spectra[!channels]) else data.frame(spectrum = seq_len(nrow(x)))
  list(x = x, wavelength = wl[channels], ids = ids, single = FALSE)
}

# Lines as a data frame with a wavelength column.
line_table <- function(lines) {
  if (is.numeric(lines) && is.null(dim(lines))) {
    if (length(lines) == 0 || anyNA(lines)) {
      stop("'lines' must contain wavelengths (nm).", call. = FALSE)
    }
    labels <- names(lines) %||% format(lines)
    labels[is.na(labels) | labels == ""] <- format(lines)[is.na(labels) | labels == ""]
    return(data.frame(line = labels, wavelength = unname(lines), stringsAsFactors = FALSE))
  }
  if (!is.data.frame(lines) || !"wavelength" %in% names(lines) || nrow(lines) == 0) {
    stop("'lines' must be a numeric vector or a data frame with a `wavelength` column.",
         call. = FALSE)
  }
  if (!is.numeric(lines$wavelength) || anyNA(lines$wavelength)) {
    stop("The `wavelength` column of 'lines' must be numeric, without missing values.",
         call. = FALSE)
  }
  as.data.frame(lines)
}

# Noise of a spectrum, from its channel-to-channel differences.
spectrum_noise <- function(v) {
  d <- diff(v[!is.na(v)])
  s <- stats::mad(d) / sqrt(2)
  if (!is.finite(s) || s <= 0) s <- stats::sd(d) / sqrt(2)
  if (!is.finite(s) || s <= 0) NA_real_ else s
}

# Measures one line in every spectrum (rows of x). The spectra are grouped by
# the channel of their peak, which sets their integration window, and each
# group is measured with matrix operations.
measure_line <- function(x, wl, center, half_width, search, method, baseline, fit_width, limit) {
  n <- nrow(x)
  out <- data.frame(peak_wavelength = rep(NA_real_, n), shift = NA_real_, height = NA_real_,
                    intensity = NA_real_, saturated = NA)
  if (min(abs(wl - center)) > search + half_width) {
    return(out)   # outside the spectral range
  }
  near <- which(abs(wl - center) <= max(search, min(abs(wl - center))))
  seg <- x[, near, drop = FALSE]
  seg[is.na(seg)] <- -Inf
  peak <- near[max.col(seg, ties.method = "first")]
  peak[rowSums(is.finite(seg)) == 0] <- NA_integer_   # no value in the search window
  for (pk in unique(stats::na.omit(peak))) {
    rows <- which(peak == pk)
    idx <- which(abs(wl - wl[pk]) <= half_width)
    idx <- idx[order(wl[idx])]
    w <- wl[idx]
    v <- x[rows, idx, drop = FALSE]
    m <- length(idx)
    if (baseline && m > 1) {
      v <- v - (v[, 1] + outer(v[, m] - v[, 1], (w - w[1]) / (w[m] - w[1])))
    }
    height <- suppressWarnings(apply(v, 1, max, na.rm = TRUE))
    height[!is.finite(height)] <- NA_real_
    out$peak_wavelength[rows] <- wl[pk]
    out$height[rows] <- height
    out$saturated[rows] <- if (is.null(limit)) NA else
      rowSums(x[rows, idx, drop = FALSE] >= limit, na.rm = TRUE) > 0
    out$intensity[rows] <- switch(
      method,
      height = height,
      area = if (m > 1) as.vector((v[, -1, drop = FALSE] + v[, -m, drop = FALSE]) %*% diff(w)) / 2 else height,
      voigt = NA_real_
    )
  }
  out$shift <- out$peak_wavelength - center
  if (method == "voigt") {
    out$intensity <- voigt_areas(x, wl, out$peak_wavelength, fit_width)
  }
  out
}

# Areas of Voigt profiles fitted around the peak of each spectrum.
voigt_areas <- function(x, wl, peaks, fit_width) {
  too_few <- vapply(peaks, function(p) !is.na(p) && sum(abs(wl - p) <= fit_width) < 6, logical(1))
  if (any(too_few)) {
    warning("Fewer than 6 channels to fit the line at ", round(peaks[too_few][1], 3),
            " nm: widen `fit_width`.", call. = FALSE)
  }
  vapply(seq_len(nrow(x)), function(i) {
    if (is.na(peaks[i]) || too_few[i]) return(NA_real_)
    keep <- abs(wl - peaks[i]) <= fit_width
    window <- tibble::as_tibble(as.list(stats::setNames(x[i, keep], wl[keep])))
    fit <- suppressWarnings(peak_fit(window, profile = "voigt"))
    tidied <- fit$tidied[[1]]
    if (is.null(tidied)) return(NA_real_)
    tidied$estimate[tidied$term == "A"]
  }, numeric(1))
}

line_column_names <- function(lines, prefix) {
  labels <- names(lines) %||% rep("", length(lines))
  labels[is.na(labels) | labels == ""] <- format(lines)[is.na(labels) | labels == ""]
  make.unique(paste0(prefix, trimws(labels)), sep = "_")
}

#' @exportS3Method recipes::prep
prep.step_line_intensities <- function(x, training, info = NULL, ...) {
  cols <- step_predictors(x, training, info)
  wl <- parse_wavelength(cols)
  if (length(cols) > 0 && anyNA(wl)) {
    stop("`step_line_intensities()` needs spectral columns named by their wavelengths, e.g. `",
         cols[is.na(wl)][1], "` is not.", call. = FALSE)
  }
  x$columns <- cols
  x$trained <- TRUE
  x
}

#' @exportS3Method recipes::bake
bake.step_line_intensities <- function(object, new_data, ...) {
  cols <- object$columns
  recipes::check_new_data(cols, object, new_data)
  if (length(cols) == 0) {
    return(new_data)
  }
  xmat <- step_matrix(new_data, cols)
  wl <- parse_wavelength(cols)
  values <- vapply(object$lines, function(center) {
    measure_line(xmat, wl, center, object$half_width, object$search, object$method,
                 object$baseline, NA_real_, NULL)$intensity
  }, numeric(nrow(xmat)))
  values <- matrix(values, nrow = nrow(xmat))
  colnames(values) <- line_column_names(object$lines, object$prefix)
  values <- tibble::as_tibble(values)
  values <- recipes::check_name(values, new_data, object, newname = names(values))
  new_data <- dplyr::bind_cols(new_data, values)
  if (!object$keep_original_cols) {
    new_data <- new_data[setdiff(names(new_data), cols)]
  }
  new_data
}

#' @export
print.step_line_intensities <- function(x, width = max(20, options()$width - 30), ...) {
  title <- paste0("Intensities of ", length(x$lines), " emission lines from ")
  recipes::print_step(x$columns, x$terms, x$trained, title, width)
  invisible(x)
}

#' @exportS3Method generics::tidy
tidy.step_line_intensities <- function(x, ...) {
  tibble::tibble(line = line_column_names(x$lines, x$prefix), wavelength = unname(x$lines),
                 method = x$method, id = x$id)
}

#' @exportS3Method generics::tunable
tunable.step_line_intensities <- function(x, ...) {
  tibble::tibble(name = character(0), call_info = list(), source = character(0),
                 component = character(0), component_id = character(0))
}

#' @exportS3Method generics::required_pkgs
required_pkgs.step_line_intensities <- function(x, ...) {
  c("specProc")
}
