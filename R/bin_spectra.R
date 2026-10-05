#' @title Binning of Adjacent Channels
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Reduces the number of channels of spectra by replacing each group of
#' `width` adjacent channels by their mean.
#'
#' @details
#' Averaging keeps the intensities on their scale and divides the noise of
#' independent channels by about `sqrt(width)`, at the cost of spectral
#' resolution: keep `width` smaller than the width of the emission lines (a
#' few channels in LIBS spectra) to keep them resolved. Fewer channels also
#' make the models faster to fit.
#'
#' The channels are grouped from the first one; when their number is not a
#' multiple of `width`, the last group is smaller. When `segments = TRUE`, the
#' spectrum is split between the detectors of a multi-spectrometer system
#' (where the wavelengths step back, or jump by more than 5 times the median
#' spacing), and each detector is binned on its own, so that no group
#' straddles two detectors. The binned channels are named by their mean
#' wavelength (to 10 significant digits), or, when the names are not
#' wavelengths, by the name of their first channel; a channel left alone in
#' its group keeps its name.
#'
#' @param x A numeric matrix or data frame of spectra, one per row, or a
#'   numeric vector (one spectrum). Their names are the wavelengths.
#' @param width The number of adjacent channels averaged, a positive integer.
#'   Default is 3; `width = 1` returns the spectra unchanged.
#' @param segments A logical: bin the segments between gaps of the
#'   wavelength axis separately (`TRUE`, default). Needs wavelengths as
#'   names.
#'
#' @return The binned spectra, of the same type as `x` (a tibble for a data
#'   frame), with `ceiling(n / width)` channels for each detector segment of
#'   `n` channels.
#'
#' @references
#'  - Vrábel, J., Képeš, E., Duponchel, L., et al. (2020). Classification of
#'    challenging laser-induced breakdown spectroscopy soil sample data -
#'    EMSLIBS contest. Spectrochimica Acta Part B, 169:105872.
#'
#' @seealso [step_bin_spectra()], [gap_derivative()], [average()]
#' @export bin_spectra
#'
#' @examples
#' data(forageLIBS)
#' spectra <- forageLIBS[1:3, -(1:14)]
#' binned <- bin_spectra(spectra, width = 3)
#' ncol(spectra)
#' ncol(binned)
#' names(binned)[1:3]
bin_spectra <- function(x, width = 3, segments = TRUE) {
  check_count(width, "width")
  check_flag(segments, "segments")
  vector_input <- is.numeric(x) && is.null(dim(x))
  df_input <- is.data.frame(x)
  mat <- if (vector_input) matrix(x, nrow = 1, dimnames = list(NULL, names(x))) else
    as_numeric_matrix(x, "x")
  bins <- spectra_bins(colnames(mat), width, segments, ncol(mat))
  out <- bin_matrix(mat, bins)
  if (vector_input) return(stats::setNames(out[1, ], colnames(out)))
  if (df_input) return(tibble::as_tibble(as.data.frame(out, check.names = FALSE)))
  out
}

#' @title Binning Recipe Step
#'
#' @description
#' `step_bin_spectra()` creates a *specification* of a recipe step that
#' replaces each group of `width` adjacent channels of the spectra by their
#' mean, with [bin_spectra()].
#'
#' @details
#' The selected columns form one spectrum per row, and are replaced by the
#' fewer binned columns, named by their mean wavelength (see
#' [bin_spectra()]). The groups of channels are set from the column names
#' when the recipe is prepped; nothing is estimated from the training data.
#' The `width` can be tuned with `tune::tune()` (dials parameter
#' `window_size()`). [tidy()][recipes::tidy.recipe] returns the `terms`,
#' `width` and `id`.
#'
#' @inherit step_baseline return
#' @inheritParams step_baseline
#' @inheritParams bin_spectra
#' @param role The role of the binned columns. Default is `"predictor"`.
#' @param bins The groups of channels and the names of the binned columns,
#'   set when the recipe is prepped. Not to be set by the user.
#'
#' @seealso [bin_spectra()], [step_gap_derivative()], [step_spectral_norm()]
#' @export
#'
#' @examples
#' if (rlang::is_installed("recipes")) {
#'   data(forageLIBS)
#'   # binning, normalization to the total area and gap-segment derivative
#'   rec <- recipes::recipe(K ~ ., data = forageLIBS[-c(1:10, 12:14)]) |>
#'     step_bin_spectra(recipes::all_predictors(), width = 3) |>
#'     step_spectral_norm(recipes::all_predictors(), method = "area") |>
#'     step_gap_derivative(recipes::all_predictors(), gap = 3, segment = 1) |>
#'     recipes::prep()
#'   dim(recipes::bake(rec, new_data = NULL))
#' }
step_bin_spectra <- function(recipe, ..., width = 3, segments = TRUE, role = "predictor",
                             trained = FALSE, columns = NULL, bins = NULL, skip = FALSE,
                             id = recipes::rand_id("bin_spectra")) {
  rlang::check_installed("recipes")
  recipes::add_step(recipe, specproc_step_new(
    "bin_spectra", terms = rlang::enquos(...), role = role, trained = trained,
    width = width, segments = segments, columns = columns, bins = bins, skip = skip, id = id
  ))
}

# ---- internals ---------------------------------------------------------------

# Groups of `width` adjacent channels within each detector segment: the
# group of each channel and the names of the binned channels.
spectra_bins <- function(nms, width, segments, n = length(nms)) {
  group <- integer(n)
  start <- 0L
  for (idx in segment_columns(nms, segments, n)) {
    g <- (seq_along(idx) - 1L) %/% width
    group[idx] <- start + g + 1L
    start <- start + max(g) + 1L
  }
  size <- tabulate(group, start)
  first <- match(seq_len(start), group)
  out_names <- if (!is.null(nms)) {
    wl <- parse_wavelength(nms)
    if (anyNA(wl)) {
      nms[first]
    } else {
      binned <- as.character(signif(as.vector(rowsum(wl, group)) / size, 10))
      ifelse(size == 1, nms[first], binned)
    }
  }
  list(group = group, size = size, names = out_names)
}

# Means of the channels of each bin.
bin_matrix <- function(x, bins) {
  out <- t(rowsum(t(x), bins$group, reorder = FALSE)) / rep(bins$size, each = nrow(x))
  dimnames(out) <- list(rownames(x), bins$names)
  out
}

#' @exportS3Method recipes::prep
prep.step_bin_spectra <- function(x, training, info = NULL, ...) {
  check_count(x$width, "width")
  check_flag(x$segments, "segments")
  x$columns <- step_predictors(x, training, info)
  x$bins <- if (length(x$columns) > 0) spectra_bins(x$columns, x$width, x$segments)
  x$trained <- TRUE
  x
}

#' @exportS3Method recipes::bake
bake.step_bin_spectra <- function(object, new_data, ...) {
  cols <- object$columns
  recipes::check_new_data(cols, object, new_data)
  if (length(cols) == 0) return(new_data)
  binned <- tibble::as_tibble(as.data.frame(bin_matrix(step_matrix(new_data, cols), object$bins),
                                            check.names = FALSE))
  new_data <- new_data[setdiff(names(new_data), cols)]
  binned <- recipes::check_name(binned, new_data, object, newname = names(binned))
  dplyr::bind_cols(new_data, binned)
}

#' @export
print.step_bin_spectra <- function(x, width = max(20, options()$width - 30), ...) {
  title <- paste0("Binning of ", x$width, " adjacent channels on ")
  recipes::print_step(x$columns, x$terms, x$trained, title, width)
  invisible(x)
}

#' @exportS3Method generics::tidy
tidy.step_bin_spectra <- function(x, ...) {
  terms <- if (recipes::is_trained(x)) x$columns else recipes::sel2char(x$terms)
  tibble::tibble(terms = terms, width = x$width, id = x$id)
}

#' @exportS3Method generics::tunable
tunable.step_bin_spectra <- function(x, ...) {
  tibble::tibble(
    name = "width",
    call_info = list(list(pkg = "dials", fun = "window_size", range = c(1L, 5L))),
    source = "recipe", component = "step_bin_spectra", component_id = x$id
  )
}

#' @exportS3Method generics::required_pkgs
required_pkgs.step_bin_spectra <- function(x, ...) {
  c("specProc")
}
