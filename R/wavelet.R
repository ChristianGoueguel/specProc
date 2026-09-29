#' @title Wavelet Coefficients of Spectra
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Computes the discrete wavelet transform (DWT) of spectra and returns its
#' coefficients as features: a compressed representation of the spectra for
#' modeling, with far fewer variables than channels.
#'
#' @details
#' The DWT decomposes a spectrum, level by level, into approximation
#' coefficients (the smooth part, at half the resolution of the previous
#' level) and detail coefficients (the fine structure removed at that
#' level), with the pyramid algorithm of Mallat (1989) and an orthonormal
#' wavelet:
#'  - `"haar"`: the Haar wavelet (2 coefficients);
#'  - `"d4"`, `"d6"`, `"d8"`: Daubechies' extremal phase wavelets with 4, 6
#'    and 8 coefficients;
#'  - `"la8"`: Daubechies' least asymmetric wavelet with 8 coefficients
#'    (symlet 4), whose nearly symmetric shape suits spectral lines.
#'
#' The coefficients kept are
#'  - `"approximation"` (default): the approximation at `level`, a smoothed
#'    version of the spectrum with about \eqn{2^{-level}} times as many
#'    values;
#'  - `"all"`: the approximation at `level` and the details of every level,
#'    the complete transform (as many values as channels, after padding).
#'
#' The spectrum is extended by reflection at its end to a length divisible
#' by \eqn{2^{level}}, and the transform is periodic. The transform is
#' orthonormal, so the sum of squares of all the coefficients equals that of
#' the (padded) spectrum. As features for a model, the approximation keeps
#' the broad shape of the spectrum and drops the noise; to keep only the
#' most informative coefficients, [step_wavelet()] can select those with the
#' largest variance in the training data (Trygg and Wold, 1998).
#'
#' @param x A numeric matrix or data frame of spectra, one per row, or a
#'   numeric vector (one spectrum).
#' @param wavelet The wavelet: `"haar"`, `"d4"` (default), `"d6"`, `"d8"` or
#'   `"la8"`.
#' @param level The number of levels of the decomposition. Default is 3.
#' @param coefficients The coefficients returned: `"approximation"`
#'   (default) or `"all"`.
#'
#' @return A matrix with one row per spectrum and one column per coefficient,
#'   named `A<level>_<i>` for the approximation and `D<j>_<i>` for the
#'   details of level `j`.
#'
#' @references
#'  - Mallat, S.G. (1989). A theory for multiresolution signal
#'    decomposition: the wavelet representation. IEEE Transactions on
#'    Pattern Analysis and Machine Intelligence, 11(7):674-693.
#'  - Daubechies, I. (1992). Ten Lectures on Wavelets. SIAM, Philadelphia.
#'  - Trygg, J., Wold, S. (1998). PLS regression on wavelet compressed NIR
#'    spectra. Chemometrics and Intelligent Laboratory Systems,
#'    42(1-2):209-220.
#'
#' @seealso [step_wavelet()], [savitzky_golay()]
#' @export wavelet_features
#'
#' @examples
#' data(soilLIBS)
#' spectra <- soilLIBS[1:4, -(1:8)]
#' approx <- wavelet_features(spectra, wavelet = "la8", level = 4)
#' dim(approx)   # 7152 channels -> 447 coefficients
wavelet_features <- function(x, wavelet = "d4", level = 3, coefficients = "approximation") {
  wavelet <- match.arg(wavelet, names(wavelet_filters))
  coefficients <- match.arg(coefficients, c("approximation", "all"))
  mat <- if (is.numeric(x) && is.null(dim(x))) matrix(x, nrow = 1) else as_numeric_matrix(x, "x")
  if (anyNA(mat)) {
    stop("The spectra have missing values; impute or remove them first.", call. = FALSE)
  }
  check_wavelet_level(level, ncol(mat), wavelet)
  dwt <- dwt_matrix(mat, wavelet_filters[[wavelet]], level)
  if (coefficients == "approximation") return(dwt$approximation)
  do.call(cbind, c(list(dwt$approximation), rev(dwt$details)))
}

#' @title Wavelet Features Recipe Step
#'
#' @description
#' `step_wavelet()` creates a *specification* of a recipe step that replaces
#' the spectral columns by their wavelet coefficients, computed with
#' [wavelet_features()], optionally keeping only the coefficients of largest
#' variance in the training data.
#'
#' @details
#' The selected columns form one spectrum per row. With `num_coef`, the
#' variance of each coefficient is computed on the training data when the
#' recipe is prepped, and the `num_coef` coefficients of largest variance are
#' kept (Trygg and Wold, 1998); new data get the same coefficients. The
#' `level` and `num_coef` can be tuned with `tune::tune()` (dials parameters
#' `wavelet_level()` and `num_terms()`). [tidy()][recipes::tidy.recipe]
#' returns the names of the kept coefficients (`terms`), their `variance` in
#' the training data (`NA` before the step is trained) and `id`.
#'
#' @inheritParams step_baseline
#' @inheritParams wavelet_features
#' @param num_coef The number of coefficients to keep, those of largest
#'   variance in the training data, or `NULL` (default) to keep them all.
#' @param prefix The prefix of the new column names. Default is `"wav_"`.
#' @param keep_original_cols A logical: keep the spectral columns (`FALSE`,
#'   default).
#' @param res The coefficients kept, stored once the step has been trained.
#'   Not to be set by the user.
#'
#' @inherit step_baseline return
#' @seealso [wavelet_features()], [step_savgol()]
#' @export
#'
#' @examples
#' if (rlang::is_installed("recipes")) {
#'   data(soilLIBS)
#'   rec <- recipes::recipe(Clay ~ ., data = soilLIBS[-c(1:2, 4:8)]) |>
#'     step_wavelet(recipes::all_predictors(), wavelet = "la8", level = 4, num_coef = 50) |>
#'     recipes::prep()
#'   dim(recipes::bake(rec, new_data = NULL))
#' }
step_wavelet <- function(recipe, ..., wavelet = "d4", level = 3, coefficients = "approximation",
                         num_coef = NULL, prefix = "wav_", keep_original_cols = FALSE,
                         role = "predictor", trained = FALSE, columns = NULL, res = NULL,
                         skip = FALSE, id = recipes::rand_id("wavelet")) {
  rlang::check_installed("recipes")
  wavelet <- match.arg(wavelet, names(wavelet_filters))
  coefficients <- match.arg(coefficients, c("approximation", "all"))
  check_flag(keep_original_cols, "keep_original_cols")
  recipes::add_step(recipe, specproc_step_new(
    "wavelet", terms = rlang::enquos(...), role = role, trained = trained, wavelet = wavelet,
    level = level, coefficients = coefficients, num_coef = num_coef, prefix = prefix,
    keep_original_cols = keep_original_cols, columns = columns, res = res, skip = skip, id = id
  ))
}

#' @title Number of Levels of a Wavelet Decomposition
#'
#' @description
#' A dials parameter for the `level` of [step_wavelet()].
#'
#' @param range The range of levels. Default is 1 to 6.
#' @param trans Not used.
#'
#' @return A dials `quant_param` object.
#' @seealso [step_wavelet()]
#' @export
wavelet_level <- function(range = c(1L, 6L), trans = NULL) {
  rlang::check_installed("dials")
  dials::new_quant_param(type = "integer", range = range, inclusive = c(TRUE, TRUE),
                         trans = trans, label = c(wavelet_level = "Wavelet levels"),
                         finalize = NULL)
}

# ---- internals ---------------------------------------------------------------

# Scaling (low-pass) filters of orthonormal wavelets.
wavelet_filters <- list(
  haar = c(1, 1) / sqrt(2),
  d4 = c(1 + sqrt(3), 3 + sqrt(3), 3 - sqrt(3), 1 - sqrt(3)) / (4 * sqrt(2)),
  d6 = c(0.3326705529509569, 0.8068915093133388, 0.4598775021193313,
         -0.1350110200103908, -0.0854412738822415, 0.0352262918821007),
  d8 = c(0.2303778133088964, 0.7148465705529154, 0.6308807679298587,
         -0.0279837694168599, -0.1870348117190931, 0.0308413818355607,
         0.0328830116668852, -0.0105974017850690),
  la8 = c(-0.0757657147893407, -0.0296355276459541, 0.4976186676324578,
          0.8037387518052163, 0.2978577956055422, -0.0992195435769354,
          -0.0126039672622612, 0.0322231006040713)
)

check_wavelet_level <- function(level, n, wavelet) {
  check_count(level, "level")
  if (is.null(n) || n == 0) return(invisible(TRUE))
  max_level <- floor(log2(n / (length(wavelet_filters[[wavelet]]) - 1)))
  if (level > max_level) {
    stop("'level' must be at most ", max_level, " for spectra of ", n, " channels with the ",
         wavelet, " wavelet.", call. = FALSE)
  }
  invisible(TRUE)
}

# Extends the rows by reflection at their end to a multiple of 2^level.
wavelet_pad <- function(x, level) {
  n <- ncol(x)
  m <- ceiling(n / 2^level) * 2^level
  if (m == n) return(x)
  extra <- m - n
  # x[n - 1], x[n - 2], ...: the mirror image without repeating the last value
  reflect <- n - ((seq_len(extra) - 1) %% (n - 1)) - 1
  cbind(x, x[, reflect, drop = FALSE])
}

# Pyramid algorithm: approximation at `level` and details of each level
# (details[[j]] for level j), for every row of x.
dwt_matrix <- function(x, h, level) {
  g <- rev(h) * (-1)^(seq_along(h) - 1)
  a <- wavelet_pad(x, level)
  details <- vector("list", level)
  for (j in seq_len(level)) {
    n <- ncol(a)
    half <- n / 2
    base <- 2 * (seq_len(half) - 1)
    approx_next <- matrix(0, nrow(a), half)
    detail <- matrix(0, nrow(a), half)
    for (k in seq_along(h)) {
      cols <- (base + k - 1) %% n + 1
      block <- a[, cols, drop = FALSE]
      approx_next <- approx_next + h[k] * block
      detail <- detail + g[k] * block
    }
    colnames(detail) <- paste0("D", j, "_", seq_len(half))
    details[[j]] <- detail
    a <- approx_next
  }
  colnames(a) <- paste0("A", level, "_", seq_len(ncol(a)))
  list(approximation = a, details = details)
}

# Inverse of dwt_matrix (used in the tests).
idwt_matrix <- function(dwt, h) {
  g <- rev(h) * (-1)^(seq_along(h) - 1)
  a <- dwt$approximation
  for (j in rev(seq_along(dwt$details))) {
    d <- dwt$details[[j]]
    half <- ncol(a)
    n <- 2 * half
    out <- matrix(0, nrow(a), n)
    base <- 2 * (seq_len(half) - 1)
    for (k in seq_along(h)) {
      cols <- (base + k - 1) %% n + 1
      for (i in seq_len(half)) {
        out[, cols[i]] <- out[, cols[i]] + h[k] * a[, i] + g[k] * d[, i]
      }
    }
    a <- out
  }
  a
}

#' @exportS3Method recipes::prep
prep.step_wavelet <- function(x, training, info = NULL, ...) {
  cols <- step_predictors(x, training, info)
  check_wavelet_level(x$level, length(cols), x$wavelet)
  if (length(cols) > 0) {
    coefs <- wavelet_features(step_matrix(training, cols), x$wavelet, x$level, x$coefficients)
    variance <- apply(coefs, 2, stats::var)
    keep <- if (is.null(x$num_coef)) seq_along(variance) else {
      check_count(x$num_coef, "num_coef")
      sort(order(variance, decreasing = TRUE)[seq_len(min(x$num_coef, length(variance)))])
    }
    x$res <- list(keep = colnames(coefs)[keep], variance = unname(variance[keep]))
  }
  x$columns <- cols
  x$trained <- TRUE
  x
}

#' @exportS3Method recipes::bake
bake.step_wavelet <- function(object, new_data, ...) {
  cols <- object$columns
  recipes::check_new_data(cols, object, new_data)
  if (length(cols) == 0) return(new_data)
  coefs <- wavelet_features(step_matrix(new_data, cols), object$wavelet, object$level,
                            object$coefficients)[, object$res$keep, drop = FALSE]
  colnames(coefs) <- paste0(object$prefix, colnames(coefs))
  coefs <- tibble::as_tibble(coefs)
  coefs <- recipes::check_name(coefs, new_data, object, newname = names(coefs))
  new_data <- dplyr::bind_cols(new_data, coefs)
  if (!object$keep_original_cols) {
    new_data <- new_data[setdiff(names(new_data), cols)]
  }
  new_data
}

#' @export
print.step_wavelet <- function(x, width = max(20, options()$width - 30), ...) {
  title <- paste0("Wavelet (", x$wavelet, ", level ", x$level, ") coefficients of ")
  recipes::print_step(x$columns, x$terms, x$trained, title, width)
  invisible(x)
}

#' @exportS3Method generics::tidy
tidy.step_wavelet <- function(x, ...) {
  if (recipes::is_trained(x) && !is.null(x$res)) {
    tibble::tibble(terms = paste0(x$prefix, x$res$keep), variance = x$res$variance, id = x$id)
  } else {
    tibble::tibble(terms = recipes::sel2char(x$terms), variance = NA_real_, id = x$id)
  }
}

#' @exportS3Method generics::tunable
tunable.step_wavelet <- function(x, ...) {
  tibble::tibble(
    name = c("level", "num_coef"),
    call_info = list(list(pkg = "specProc", fun = "wavelet_level"),
                     list(pkg = "dials", fun = "num_terms", range = c(10L, 200L))),
    source = "recipe", component = "step_wavelet", component_id = x$id
  )
}

#' @exportS3Method generics::required_pkgs
required_pkgs.step_wavelet <- function(x, ...) {
  c("specProc")
}
