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
#' The Gaussian and Lorentzian widths of a Voigt profile are strongly
#' correlated, so a free fit determines the Lorentzian (Stark) width poorly
#' when the line is narrow. In a LIBS plasma, the Gaussian width is the
#' instrumental width, combined with the Doppler width in quadrature,
#' \eqn{w_G = \sqrt{w_{inst}^2 + w_D^2}}, and can be held fixed with
#' `fixed = "wG"` and the value of `wG`. The instrumental width is the width
#' of a line of a low-pressure lamp (mercury, argon), whose Stark and
#' Doppler widths are negligible, fitted with a Gaussian profile at a nearby
#' wavelength; the Doppler width is given by [doppler_width()]. A fixed width
#' is listed in `tidied` without standard error.
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
#' @param fixed The widths held at their given values (`wG`, `wL`) instead
#'   of being fitted: `"wG"`, `"wL"` or both. Default is `NULL` (all
#'   widths fitted).
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
#' data(forageLIBS)
#' mean_spectrum <- colMeans(forageLIBS[-(1:14)])
#' wl <- as.numeric(names(mean_spectrum))
#' # the H-alpha line of the hydrogen of the samples, Stark-broadened
#' halpha <- tibble::as_tibble(as.list(mean_spectrum[wl > 653.5 & wl < 659.5]))
#' res <- peak_fit(halpha, profile = "lorentzian")
#' res$tidied[[1]]
#'
#' # a Voigt line whose Gaussian width (instrument and Doppler) is known
#' x <- seq(395, 397, by = 0.01)
#' set.seed(1)
#' y <- voigt_profile(x, y0 = 10, xc = 396, wG = 0.08, wL = 0.05, A = 50) +
#'   rnorm(length(x), sd = 2)
#' line <- tibble::as_tibble(as.list(stats::setNames(y, x)))
#' peak_fit(line, profile = "voigt", wG = 0.08, fixed = "wG")$tidied[[1]]
peak_fit <- function(
    x,
    profile = "voigt",
    wL = NULL,
    wG = NULL,
    A = NULL,
    wlgth.min = NULL,
    wlgth.max = NULL,
    id = NULL,
    max.iter = 200,
    fixed = NULL) {

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
              wL = wL, wG = wG, A = A, max.iter = max.iter, single = TRUE, fixed = fixed)
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

# Checks the widths held fixed (`fixed`) against the profiles and the given
# widths.
check_fixed <- function(fixed, profiles, wL, wG) {
  if (is.null(fixed) || length(fixed) == 0) return(character())
  if (!is.character(fixed) || !all(fixed %in% c("wG", "wL")) || anyDuplicated(fixed)) {
    stop("'fixed' must be \"wG\", \"wL\" or both.", call. = FALSE)
  }
  has_width <- list(wG = c("gaussian", "voigt", "pseudo_voigt"),
                    wL = c("lorentzian", "voigt", "pseudo_voigt"))
  for (w in fixed) {
    if (is.null(if (w == "wG") wG else wL)) {
      stop("A fixed width needs its value: give '", w, "'.", call. = FALSE)
    }
    if (!any(profiles %in% has_width[[w]])) {
      stop("'fixed = \"", w, "\"' needs a profile with a ",
           if (w == "wG") "Gaussian" else "Lorentzian", " width.", call. = FALSE)
    }
  }
  fixed
}

# Builds the parameter specification of the multi-peak model: the fitted
# parameters (start values and bounds), the model terms, and the definition
# of each parameter of each line (y0, xc, wG, wL, A) as an expression of the
# fitted parameters: a parameter of its own, a fixed value (`fixed`), or a
# function of a parameter shared by a group of lines (`centers`: the lines
# keep the separations of `peaks` and move by a common shift; `widths`: the
# lines share their widths; `area_ratios`: the areas are in fixed ratios).
# `centers` and `widths` give a group number per line (NA for a free line).
peak_parameters <- function(data, peaks, profiles, wL, wG, A, single, fixed = character(),
                            centers = NULL, widths = NULL, area_ratios = NULL) {
  k <- length(profiles)
  pick <- function(v, i) if (is.null(v)) NA_real_ else if (length(v) == 1) v else v[i]
  x <- data$x
  y <- data$y
  sfx <- function(nm, i) if (single) nm else paste0(nm, "_", i)
  num <- function(v) sprintf("%.17g", v)
  # the name of a parameter shared by the lines `members`
  shared <- function(base, members) {
    if (length(members) == k) base else paste(c(base, members), collapse = "_")
  }
  guesses <- lapply(seq_len(k), function(i) guess_peak(x, y, if (single) NA_real_ else peaks[i]))

  start <- list(y0 = min(y))
  lower <- c(y0 = -Inf)
  upper <- c(y0 = Inf)
  add <- function(nm, value, lo, hi) {
    if (is.null(start[[nm]])) {
      start[[nm]] <<- value
      lower[nm] <<- lo
      upper[nm] <<- hi
    }
    nm
  }
  center <- function(i) {
    g <- if (is.null(centers)) NA else centers[i]
    if (is.na(g)) {
      return(add(sfx("xc", i), if (single) guesses[[i]]$xc else peaks[i], min(x), max(x)))
    }
    members <- which(centers == g)
    if (min(peaks[members]) < min(x) || max(peaks[members]) > max(x)) {
      stop("The lines locked together must lie within the fitted wavelength range.",
           call. = FALSE)
    }
    shift <- add(shared("shift", members), 0, min(x) - min(peaks[members]),
                 max(x) - max(peaks[members]))
    paste(num(peaks[i]), "+", shift)
  }
  width <- function(type, i, guess) {
    value <- pick(if (type == "wG") wG else wL, i)
    if (type %in% fixed) return(num(value))
    g <- if (is.null(widths)) NA else widths[i]
    members <- if (is.na(g)) i else which(widths == g)
    nm <- if (length(members) > 1) shared(type, members) else sfx(type, i)
    add(nm, ifelse(is.na(value), guess, value), stats::median(diff(x)) / 100, Inf)
  }
  area <- function(i) {
    r <- if (is.null(area_ratios)) NA_real_ else area_ratios[i]
    if (is.na(r)) {
      return(add(sfx("A", i), ifelse(is.na(pick(A, i)), guesses[[i]]$A, pick(A, i)), 0, Inf))
    }
    members <- which(!is.na(area_ratios))
    first <- stats::median(vapply(members, function(j) guesses[[j]]$A / area_ratios[j], numeric(1)))
    paste(num(r), "*", add(shared("A", members), first, 0, Inf))
  }
  wrap <- function(e) if (grepl(" ", e, fixed = TRUE)) paste0("(", e, ")") else e

  definitions <- c(y0 = "y0")
  terms <- character(k)
  for (i in seq_len(k)) {
    xc <- center(i)
    w <- switch(
      profiles[i],
      gaussian = c(wG = width("wG", i, guesses[[i]]$fwhm)),
      lorentzian = c(wL = width("wL", i, guesses[[i]]$fwhm)),
      c(wG = width("wG", i, guesses[[i]]$fwhm / 2), wL = width("wL", i, guesses[[i]]$fwhm / 2))
    )
    a <- area(i)
    definitions[sfx(c("xc", names(w), "A"), i)] <- c(xc, w, a)
    terms[i] <- sprintf("%s * profile_%s(x, %s)", wrap(a), profiles[i],
                        paste(c(xc, w), collapse = ", "))
  }
  list(start = start, lower = lower[names(start)], upper = upper[names(start)], terms = terms,
       definitions = definitions)
}

fit_one <- function(data, peaks, profiles, wL, wG, A, max.iter, single, fixed = character(),
                    centers = NULL, widths = NULL, area_ratios = NULL) {
  spec <- peak_parameters(data, peaks, profiles, wL, wG, A, single, fixed, centers, widths,
                          area_ratios)
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
  attr(fit, "peak_terms") <- spec$terms
  attr(fit, "definitions") <- spec$definitions

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
  list(fit = fit, tidied = fit_parameters(fit), augmented = augmented)
}

# Values of the parameters of the lines (the definitions of the fit) and
# their covariance matrix, propagated from the fitted parameters (the
# definitions are linear in them). `constant` marks the fixed values.
line_estimates <- function(fit) {
  coefs <- stats::coef(fit)
  cov <- tryCatch(stats::vcov(fit), error = function(e) NULL)
  defs <- attr(fit, "definitions")
  if (is.null(defs)) {   # fits made before the definitions were kept
    return(list(estimate = coefs, vcov = cov, constant = rep(FALSE, length(coefs))))
  }
  exprs <- lapply(defs, str2lang)
  value <- function(b) {
    env <- as.list(b)
    vapply(exprs, function(e) eval(e, env, baseenv()), numeric(1))
  }
  estimate <- value(coefs)
  jacobian <- matrix(vapply(seq_along(coefs), function(j) {
    h <- 1e-6 * max(1, abs(coefs[[j]]))
    up <- dn <- coefs
    up[j] <- up[j] + h
    dn[j] <- dn[j] - h
    (value(up) - value(dn)) / (2 * h)
  }, numeric(length(defs))), nrow = length(defs), dimnames = list(names(defs), names(coefs)))
  list(estimate = estimate,
       vcov = if (is.null(cov)) NULL else jacobian %*% cov %*% t(jacobian),
       constant = rowSums(jacobian != 0) == 0)
}

# The tidied parameters of a fit: those of each line, with their standard
# errors (none for a fixed value), and the common shifts of locked centers.
fit_parameters <- function(fit) {
  le <- line_estimates(fit)
  estimate <- le$estimate
  se <- if (is.null(le$vcov)) rep(NA_real_, length(estimate)) else sqrt(pmax(diag(le$vcov), 0))
  se[le$constant] <- NA_real_
  shifts <- grep("^shift", names(stats::coef(fit)), value = TRUE)
  if (length(shifts) > 0) {
    cov <- tryCatch(stats::vcov(fit), error = function(e) NULL)
    estimate <- c(estimate, stats::coef(fit)[shifts])
    se <- c(se, if (is.null(cov)) rep(NA_real_, length(shifts)) else
      sqrt(pmax(diag(cov)[shifts], 0)))
  }
  statistic <- estimate / se
  tibble::tibble(term = names(estimate), estimate = estimate, std.error = unname(se),
                 statistic = unname(statistic),
                 p.value = unname(2 * stats::pt(-abs(statistic), stats::df.residual(fit))))
}

fit_spectra <- function(spectra, peaks, profiles, wL, wG, A, max.iter, single,
                        fixed = character(), centers = NULL, widths = NULL,
                        area_ratios = NULL) {
  for (v in list(wL, wG, A)) {
    if (!is.null(v) && (!is.numeric(v) || !length(v) %in% c(1, length(profiles)) || any(v <= 0))) {
      stop("'wL', 'wG' and 'A' must be positive numbers, either one value or one per peak.",
           call. = FALSE)
    }
  }
  fixed <- check_fixed(fixed, profiles, wL, wG)
  results <- lapply(seq_along(spectra$data), function(i) {
    tryCatch(
      fit_one(spectra$data[[i]], peaks, profiles, wL, wG, A, max.iter, single, fixed, centers,
              widths, area_ratios),
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

# Evaluates a fitted line model, and each line of a multi-peak model, on a
# grid of wavelengths (used to draw smooth curves in plot_fit()).
fit_curve <- function(fit, x) {
  par <- as.list(stats::coef(fit))
  env <- list2env(c(par, list(x = x)), parent = environment(profile_gaussian))
  terms <- attr(fit, "peak_terms")
  out <- tibble::tibble(x = x, .fitted = as.numeric(stats::predict(fit, newdata = data.frame(x = x))))
  if (length(terms) > 1) {
    for (i in seq_along(terms)) {
      out[[paste0(".peak_", i)]] <- par$y0 + eval(str2lang(terms[i]), env)
    }
  }
  out
}
