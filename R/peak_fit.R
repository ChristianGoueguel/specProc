#' @title Peak Fitting
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Fitting of a single spectral line by lineshape functions with variable parameters.
#'
#' @details
#' The function uses `minpack.lm::nlsLM`, which is based on the Levenberg-Marquardt
#' algorithm for searching the minimum value of the square of the sum of the residuals.
#' Each spectrum (row of `x`) is fitted separately with the model
#' \deqn{y = y_0 + A \cdot f(x; x_c, w)}
#' where \eqn{f} is a unit-area Gaussian, Lorentzian, Voigt or pseudo-Voigt profile
#' (see [gaussian_profile()], [lorentzian_profile()], [voigt_profile()] and
#' [pseudo_voigt_profile()]). The fitted parameters are `y0`, `xc`, `A` and the
#' width(s) `wG` and/or `wL` (full widths at half maximum). The Voigt profile
#' is evaluated exactly in C++; the pseudo-Voigt (Thompson-Cox-Hastings)
#' approximation is faster but accurate to about 1\%.
#'
#' Initial values that are not supplied are estimated from the data: the peak
#' center from the position of the maximum, the width from the full width at
#' half maximum, and the area from the integrated intensity above the minimum.
#' The center is constrained to the fitted wavelength range and the widths and
#' area to positive values. If the fit of a spectrum fails, a warning is issued
#' and the corresponding `fit`, `tidied` and `augmented` entries are `NULL`.
#'
#' @param x A data frame or tibble of spectra, one spectrum per row. Column
#'   names are the wavelengths (e.g. `"396.15"`); an optional identifier column
#'   can be given with `id`.
#' @param profile A character specifying the lineshape function to be used:
#' "lorentzian", "gaussian", "voigt" (exact) or "pseudo_voigt".
#' @param wL A numeric specifying the Lorentzian full width at half maximum (initial guess)
#' @param wG A numeric specifying the Gaussian full width at half maximum (initial guess)
#' @param A A numeric specifying the peak area (initial guess)
#' @param wlgth.min A numeric specifying the lower bound of the wavelength subset
#' @param wlgth.max A numeric specifying the upper bound of the wavelength subset
#' @param id A character specifying the name of the column holding the spectra id (optional)
#' @param max.iter A numeric specifying the maximum number of iteration (200 by default)
#'
#' @return A tibble with one row per spectrum and the columns:
#'  - `id` column (or `spectrum`, the row number, when `id = NULL`),
#'  - `data`: the fitted data (`x`, `y`),
#'  - `fit`: the `nls` model object,
#'  - `tidied`: the estimated parameters with their standard errors,
#'  - `augmented`: the data with fitted values (`.fitted`) and residuals (`.resid`).
#'
#' @export peak_fit
#'
#' @examples
#' wl <- seq(395, 397, by = 0.02)
#' set.seed(1)
#' spec <- 10 + gaussian_profile(wl, y0 = 0, xc = 396.15, wG = 0.2, A = 50) + rnorm(length(wl))
#' df <- as.data.frame(t(spec))
#' names(df) <- wl
#' res <- peak_fit(df, profile = "gaussian")
#' res$tidied[[1]]
#'
peak_fit <- function(
    x,
    profile = "voigt",
    wL = NULL,
    wG = NULL,
    A = NULL,
    wlgth.min = NULL,
    wlgth.max = NULL,
    id = NULL,
    max.iter = 200) {

  if (missing(x)) {
    stop("Missing 'data' argument.")
  }
  if (!is.data.frame(x)) {
    stop("Input 'data' must be a data frame or tibble.")
  }
  if (!is.character(profile) || length(profile) != 1 ||
      !tolower(profile) %in% lineshapes) {
    stop("The profile function must be: 'lorentzian', 'gaussian', 'voigt' or 'pseudo_voigt'")
  }
  if (!is.numeric(max.iter)) {
    stop("Maximum number of iteration must be numeric")
  }

  spectra <- long_spectra(x, id, wlgth.min, wlgth.max)
  fit_spectra(spectra, peaks = NA_real_, profiles = tolower(profile),
              wL = wL, wG = wG, A = A, max.iter = max.iter, single = TRUE)
}


lineshapes <- c("gaussian", "lorentzian", "voigt", "pseudo_voigt")

# Splits a wide data frame of spectra into a list of (x, y) tibbles, one per
# spectrum, restricted to [wlgth.min, wlgth.max].
long_spectra <- function(x, id, wlgth.min, wlgth.max) {
  if (!is.null(id)) {
    if (!is.character(id) || length(id) != 1 || !id %in% names(x)) {
      stop("'id' must be the name of a column of 'x'.", call. = FALSE)
    }
    ids <- x[[id]]
    x <- x[setdiff(names(x), id)]
    id_name <- id
  } else {
    ids <- seq_len(nrow(x))
    id_name <- "spectrum"
  }
  if (!all(vapply(x, is.numeric, logical(1)))) {
    stop("All spectral columns of 'x' must be numeric.", call. = FALSE)
  }
  wl <- parse_wavelength(names(x))
  if (anyNA(wl)) {
    stop("Column names of 'x' must be wavelengths (numeric values).", call. = FALSE)
  }

  lo <- if (is.null(wlgth.min)) -Inf else as.numeric(wlgth.min)
  hi <- if (is.null(wlgth.max)) Inf else as.numeric(wlgth.max)
  if (lo >= hi) {
    stop("wlgth.min must be strictly smaller than wlgth.max", call. = FALSE)
  }
  keep <- wl >= lo & wl <= hi
  if (sum(keep) < 5) {
    stop("Fewer than 5 data points in the selected wavelength range.", call. = FALSE)
  }
  ord <- order(wl[keep])
  wl <- wl[keep][ord]
  m <- as.matrix(x[keep])[, ord, drop = FALSE]

  data <- lapply(seq_len(nrow(m)), function(i) {
    ok <- !is.na(m[i, ])
    tibble::tibble(x = wl[ok], y = unname(m[i, ok]))
  })
  list(ids = ids, id_name = id_name, data = data)
}

# Initial guesses for one peak from the data around `xc`.
guess_peak <- function(x, y, xc = NA_real_) {
  y0 <- min(y)
  i <- if (is.na(xc)) which.max(y) else which.min(abs(x - xc))
  xc <- x[i]
  h <- y[i] - y0
  step <- stats::median(diff(x))
  half <- y - y0 >= h / 2
  lo <- i
  while (lo > 1 && half[lo - 1]) lo <- lo - 1
  hi <- i
  while (hi < length(x) && half[hi + 1]) hi <- hi + 1
  fwhm <- max(x[hi] - x[lo], 2 * step)
  list(xc = xc, fwhm = fwhm, A = max(h, .Machine$double.eps) * fwhm * 1.064, step = step)
}

# Builds the parameter specification of the multi-peak model.
peak_parameters <- function(data, peaks, profiles, wL, wG, A, single) {
  k <- length(profiles)
  pick <- function(v, i) if (is.null(v)) NA_real_ else if (length(v) == 1) v else v[i]
  x <- data$x
  y <- data$y
  sfx <- function(nm, i) if (single) nm else paste0(nm, "_", i)

  start <- list(y0 = min(y))
  lower <- c(y0 = -Inf)
  upper <- c(y0 = Inf)
  terms <- character(k)
  for (i in seq_len(k)) {
    g <- guess_peak(x, y, if (single) NA_real_ else peaks[i])
    nm_xc <- sfx("xc", i)
    nm_A <- sfx("A", i)
    start[[nm_xc]] <- if (single) g$xc else peaks[i]
    lower[nm_xc] <- min(x)
    upper[nm_xc] <- max(x)
    wmin <- g$step / 100
    width_term <- switch(
      profiles[i],
      gaussian = {
        nm <- sfx("wG", i)
        start[[nm]] <- ifelse(is.na(pick(wG, i)), g$fwhm, pick(wG, i))
        lower[nm] <- wmin
        upper[nm] <- Inf
        sprintf("profile_gaussian(x, %s, %s)", nm_xc, nm)
      },
      lorentzian = {
        nm <- sfx("wL", i)
        start[[nm]] <- ifelse(is.na(pick(wL, i)), g$fwhm, pick(wL, i))
        lower[nm] <- wmin
        upper[nm] <- Inf
        sprintf("profile_lorentzian(x, %s, %s)", nm_xc, nm)
      },
      voigt = ,
      pseudo_voigt = {
        nmG <- sfx("wG", i)
        nmL <- sfx("wL", i)
        start[[nmG]] <- ifelse(is.na(pick(wG, i)), g$fwhm / 2, pick(wG, i))
        start[[nmL]] <- ifelse(is.na(pick(wL, i)), g$fwhm / 2, pick(wL, i))
        lower[c(nmG, nmL)] <- wmin
        upper[c(nmG, nmL)] <- Inf
        sprintf("profile_%s(x, %s, %s, %s)", profiles[i], nm_xc, nmG, nmL)
      }
    )
    start[[nm_A]] <- ifelse(is.na(pick(A, i)), g$A, pick(A, i))
    lower[nm_A] <- 0
    upper[nm_A] <- Inf
    terms[i] <- sprintf("%s * %s", nm_A, width_term)
  }
  list(start = start, lower = lower[names(start)], upper = upper[names(start)], terms = terms)
}

fit_one <- function(data, peaks, profiles, wL, wG, A, max.iter, single) {
  spec <- peak_parameters(data, peaks, profiles, wL, wG, A, single)
  form <- stats::as.formula(
    paste("y ~ y0 +", paste(spec$terms, collapse = " + ")),
    env = environment(profile_gaussian)
  )
  fit <- minpack.lm::nlsLM(
    form,
    data = data,
    start = spec$start,
    lower = spec$lower,
    upper = spec$upper,
    control = minpack.lm::nls.lm.control(maxiter = max.iter)
  )

  coefs <- summary(fit)$coefficients
  tidied <- tibble::tibble(
    term = rownames(coefs),
    estimate = coefs[, 1],
    std.error = coefs[, 2],
    statistic = coefs[, 3],
    p.value = coefs[, 4]
  )
  fitted <- stats::fitted(fit)
  augmented <- tibble::tibble(
    x = data$x, y = data$y,
    .fitted = as.numeric(fitted),
    .resid = data$y - as.numeric(fitted)
  )
  if (!single) {
    par <- as.list(stats::coef(fit))
    env <- list2env(c(par, list(x = data$x)), parent = environment(profile_gaussian))
    for (i in seq_along(spec$terms)) {
      augmented[[paste0(".peak_", i)]] <- eval(str2lang(spec$terms[i]), env)
    }
  }
  list(fit = fit, tidied = tidied, augmented = augmented)
}

fit_spectra <- function(spectra, peaks, profiles, wL, wG, A, max.iter, single) {
  for (v in list(wL, wG, A)) {
    if (!is.null(v) && (!is.numeric(v) || !length(v) %in% c(1, length(profiles)) || any(v <= 0))) {
      stop("'wL', 'wG' and 'A' must be positive numbers, either one value or one per peak.",
           call. = FALSE)
    }
  }
  results <- lapply(seq_along(spectra$data), function(i) {
    tryCatch(
      fit_one(spectra$data[[i]], peaks, profiles, wL, wG, A, max.iter, single),
      error = function(e) {
        warning("Fitting failed for ", spectra$id_name, " '", spectra$ids[i], "': ",
                conditionMessage(e), call. = FALSE)
        list(fit = NULL, tidied = NULL, augmented = NULL)
      }
    )
  })
  out <- tibble::tibble(
    !!spectra$id_name := spectra$ids,
    data = spectra$data,
    fit = lapply(results, `[[`, "fit"),
    tidied = lapply(results, `[[`, "tidied"),
    augmented = lapply(results, `[[`, "augmented")
  )
  out
}
