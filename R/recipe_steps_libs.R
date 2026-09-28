#' @title Internal Standard Normalization Recipe Step
#'
#' @author Christian L. Goueguel
#'
#' @description
#' `step_line_ratio()` creates a *specification* of a recipe step that
#' divides each spectrum by the intensity of a reference emission line, such
#' as a line of a matrix element of constant concentration (internal
#' standard). This compensates for shot-to-shot changes of the ablated mass
#' and of the plasma conditions.
#'
#' @details
#' The selected columns form one spectrum per row, and their names must be
#' their wavelengths (in nm). The intensity of the reference line is
#' measured in each spectrum over the channels within `window` nm of
#' `reference`:
#'  - `method = "area"` (default): the integrated intensity (trapezoidal
#'    rule), more robust to noise and to small wavelength shifts;
#'  - `method = "height"`: the maximum intensity.
#'
#' With `baseline = TRUE`, a straight line through the intensities at both
#' ends of the window is subtracted first, so that the continuum under the
#' line does not contribute. When several reference wavelengths are given,
#' their intensities are summed.
#'
#' Nothing is estimated from the training data: the channels of the window
#' are found when the recipe is prepped, and each spectrum is normalized
#' independently when it is baked. The selected columns are replaced by the
#' normalized values. Spectra whose reference intensity is not positive are
#' set to `NA`, with a warning. [tidy()][recipes::tidy.recipe] returns the
#' `reference` wavelengths, the `window`, the `method` and `id`.
#'
#' Choose a line that is well resolved, not self-absorbed and not
#' saturated (see [saturation_summary()] and [self_absorption()]), and from
#' an upper level close to that of the analyte lines, so that the ratio
#' depends little on the plasma temperature.
#'
#' @inherit step_baseline return
#' @inheritParams step_baseline
#' @param reference The wavelength(s) of the reference line(s), in nm.
#' @param window The half-width of the window around each reference
#'   wavelength, in nm. Default is 0.1.
#' @param method The intensity of the reference line: `"area"` (default) or
#'   `"height"`.
#' @param baseline A logical: subtract a linear baseline under the reference
#'   line (`FALSE`, default).
#' @param channels The channels of each reference window, found when the
#'   recipe is prepped. Not to be set by the user.
#'
#' @references
#'  - Body, D., Chadwick, B.L., (2001). Optimization of the spectral data
#'    processing in a LIBS simultaneous elemental analysis system.
#'    Spectrochimica Acta Part B, 56(6):725-736.
#'
#' @seealso [normalize()], [step_reject_shots()]
#' @export
#'
#' @examples
#' if (rlang::is_installed("recipes")) {
#'   data(specLIBS)
#'   # normalize to the Ca II 393.37 nm line
#'   rec <- recipes::recipe(~ ., data = specLIBS[-(2:8)]) |>
#'     step_line_ratio(recipes::all_numeric(), reference = 393.37, window = 0.3) |>
#'     recipes::prep()
#'   recipes::tidy(rec, number = 1)
#' }
step_line_ratio <- function(recipe, ..., reference, window = 0.1, method = "area",
                            baseline = FALSE, role = NA, trained = FALSE, columns = NULL,
                            channels = NULL, skip = FALSE, id = recipes::rand_id("line_ratio")) {
  rlang::check_installed("recipes")
  if (missing(reference) || !is.numeric(reference) || length(reference) == 0 || anyNA(reference)) {
    stop("'reference' must give the wavelength(s) of the reference line(s), in nm.", call. = FALSE)
  }
  check_number(window, "window", lower = 0, lower_open = TRUE)
  method <- match.arg(method, c("area", "height"))
  check_flag(baseline, "baseline")
  recipes::add_step(recipe, specproc_step_new(
    "line_ratio", terms = rlang::enquos(...), role = role, trained = trained,
    reference = reference, window = window, method = method, baseline = baseline,
    columns = columns, channels = channels, skip = skip, id = id
  ))
}

#' @title Shot Rejection Recipe Step
#'
#' @description
#' `step_reject_shots()` creates a *specification* of a recipe step that
#' removes the outlying laser shots of each sample from the training data,
#' with [reject_shots()].
#'
#' @details
#' Each shot is compared with the other shots of the same sample (the
#' `sample` column), so nothing is estimated from the training data as a
#' whole. Because the step removes rows, `skip = TRUE` by default: shots are
#' rejected when the recipe is prepped, but new data are not filtered when
#' they are baked (predictions must be made for every row). Use a resampling
#' scheme grouped by sample, such as [rsample::group_vfold_cv()], so that
#' shots of the same sample are not split between analysis and assessment
#' sets.
#'
#' To model sample means instead of single shots, average the shots before
#' the recipe, with [reject_shots()] and [average()].
#'
#' [tidy()][recipes::tidy.recipe] returns the spectral `terms`, the `sample`
#' column, the criteria (`method`), the `cutoff` and `id`.
#'
#' @inheritParams step_baseline
#' @inheritParams reject_shots
#' @param method The criteria: one or more of `"intensity"`,
#'   `"correlation"` (both by default) and `"distance"` (see
#'   [reject_shots()]).
#' @param ... One or more selector functions to choose the spectral
#'   columns (named by their wavelengths, see [reject_shots()]).
#' @param sample The column identifying the sample of each shot, as a bare
#'   name or a selector. Give it the `"id"` role (see
#'   [recipes::update_role()]) so that it is not used as a predictor.
#' @param skip A logical: skip the step when new data are baked (`TRUE`,
#'   default). See details.
#'
#' @return An updated version of `recipe` with the new step added to the
#'   sequence of any existing operations.
#'
#' @seealso [reject_shots()], [average()], [step_line_ratio()]
#' @export
#'
#' @examples
#' if (rlang::is_installed("recipes")) {
#'   set.seed(1)
#'   wl <- seq(390, 400, by = 0.1)
#'   line <- exp(-(wl - 393.4)^2 / 0.02)
#'   shots <- t(sapply(rep(c(1, 2), each = 6), function(s) 1000 * s * line + rnorm(length(wl), 50, 5)))
#'   shots[3, ] <- shots[3, ] / 10     # a weak shot
#'   colnames(shots) <- wl
#'   df <- data.frame(Sample = rep(c("a", "b"), each = 6), shots, check.names = FALSE)
#'   rec <- recipes::recipe(~ ., data = df) |>
#'     recipes::update_role(Sample, new_role = "id") |>
#'     step_reject_shots(recipes::all_numeric(), sample = Sample) |>
#'     recipes::prep()
#'   nrow(recipes::bake(rec, new_data = NULL))   # 11 shots kept for training
#' }
step_reject_shots <- function(recipe, ..., sample, method = c("intensity", "correlation"),
                              cutoff = 3.5, role = NA, trained = FALSE, columns = NULL,
                              skip = TRUE, id = recipes::rand_id("reject_shots")) {
  rlang::check_installed("recipes")
  if (missing(sample)) {
    stop("'sample' must name the column identifying the sample of each shot.", call. = FALSE)
  }
  method <- match.arg(method, shot_criteria, several.ok = TRUE)
  check_number(cutoff, "cutoff", lower = 0, lower_open = TRUE)
  recipes::add_step(recipe, specproc_step_new(
    "reject_shots", terms = rlang::enquos(...), role = role, trained = trained,
    sample = rlang::enquos(sample), method = method, cutoff = cutoff, columns = columns,
    skip = skip, id = id
  ))
}

# ---- internals ---------------------------------------------------------------

# Channels of the window around each reference wavelength.
line_ratio_channels <- function(wavelength, reference, window) {
  lapply(reference, function(ref) {
    idx <- which(abs(wavelength - ref) <= window)
    if (length(idx) == 0) {
      stop("No channel within ", window, " nm of the reference wavelength ", ref,
           " nm: widen `window` or check `reference`.", call. = FALSE)
    }
    idx[order(wavelength[idx])]
  })
}

# Intensity of the reference line(s) in each spectrum (row of x).
line_ratio_intensity <- function(x, wavelength, channels, method, baseline) {
  total <- numeric(nrow(x))
  for (idx in channels) {
    wl <- wavelength[idx]
    seg <- x[, idx, drop = FALSE]
    if (baseline && length(idx) > 1) {
      slope <- (seg[, length(idx)] - seg[, 1]) / (wl[length(idx)] - wl[1])
      seg <- seg - (seg[, 1] + outer(slope, wl - wl[1]))
    }
    total <- total + if (method == "height" || length(idx) == 1) {
      apply(seg, 1, max)
    } else {
      dw <- diff(wl)
      as.vector((seg[, -1, drop = FALSE] + seg[, -length(idx), drop = FALSE]) %*% dw) / 2
    }
  }
  total
}

#' @exportS3Method recipes::prep
prep.step_line_ratio <- function(x, training, info = NULL, ...) {
  cols <- step_predictors(x, training, info)
  wl <- parse_wavelength(cols)
  if (length(cols) > 0 && anyNA(wl)) {
    stop("`step_line_ratio()` needs spectral columns named by their wavelengths, e.g. `",
         cols[is.na(wl)][1], "` is not.", call. = FALSE)
  }
  x$channels <- if (length(cols) > 0) line_ratio_channels(wl, x$reference, x$window)
  x$columns <- cols
  x$trained <- TRUE
  x
}

#' @exportS3Method recipes::bake
bake.step_line_ratio <- function(object, new_data, ...) {
  cols <- object$columns
  recipes::check_new_data(cols, object, new_data)
  if (length(cols) == 0) {
    return(new_data)
  }
  xmat <- step_matrix(new_data, cols)
  ref <- line_ratio_intensity(xmat, parse_wavelength(cols), object$channels, object$method,
                              object$baseline)
  bad <- !is.finite(ref) | ref <= 0
  if (any(bad)) {
    warning(sum(bad), " spectra with a non-positive reference line intensity are set to NA.",
            call. = FALSE)
    ref[bad] <- NA_real_
  }
  new_data[cols] <- as.data.frame(xmat / ref)
  new_data
}

#' @export
print.step_line_ratio <- function(x, width = max(20, options()$width - 30), ...) {
  title <- paste0("Normalization to the line at ", paste(x$reference, collapse = " + "), " nm on ")
  recipes::print_step(x$columns, x$terms, x$trained, title, width)
  invisible(x)
}

#' @exportS3Method generics::tidy
tidy.step_line_ratio <- function(x, ...) {
  tibble::tibble(reference = x$reference, window = x$window, method = x$method,
                 baseline = x$baseline, id = x$id)
}

#' @exportS3Method generics::tunable
tunable.step_line_ratio <- function(x, ...) {
  tibble::tibble(name = character(0), call_info = list(), source = character(0),
                 component = character(0), component_id = character(0))
}

#' @exportS3Method generics::required_pkgs
required_pkgs.step_line_ratio <- function(x, ...) {
  c("specProc")
}

#' @exportS3Method recipes::prep
prep.step_reject_shots <- function(x, training, info = NULL, ...) {
  cols <- step_predictors(x, training, info)
  sample <- if (is.character(x$sample)) x$sample else
    recipes::recipes_argument_select(x$sample, training, info, single = TRUE)
  x$sample <- sample
  x$columns <- cols
  x$trained <- TRUE
  x
}

#' @exportS3Method recipes::bake
bake.step_reject_shots <- function(object, new_data, ...) {
  cols <- object$columns
  recipes::check_new_data(c(cols, object$sample), object, new_data)
  if (length(cols) == 0) {
    return(new_data)
  }
  rejected <- shot_flags(step_matrix(new_data, cols), new_data[[object$sample]], object$method,
                         object$cutoff)$rejected
  new_data[!rejected, , drop = FALSE]
}

#' @export
print.step_reject_shots <- function(x, width = max(20, options()$width - 30), ...) {
  title <- paste0("Shot rejection (", paste(x$method, collapse = ", "), ") on ")
  recipes::print_step(x$columns, x$terms, x$trained, title, width)
  invisible(x)
}

#' @exportS3Method generics::tidy
tidy.step_reject_shots <- function(x, ...) {
  trained <- recipes::is_trained(x)
  tibble::tibble(
    terms = if (trained) x$columns else recipes::sel2char(x$terms),
    sample = if (trained) x$sample else paste(recipes::sel2char(x$sample), collapse = ", "),
    method = paste(x$method, collapse = ", "), cutoff = x$cutoff, id = x$id
  )
}

#' @exportS3Method generics::tunable
tunable.step_reject_shots <- function(x, ...) {
  tibble::tibble(name = character(0), call_info = list(), source = character(0),
                 component = character(0), component_id = character(0))
}

#' @exportS3Method generics::required_pkgs
required_pkgs.step_reject_shots <- function(x, ...) {
  c("specProc")
}
