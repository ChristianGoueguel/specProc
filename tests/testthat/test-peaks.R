area <- function(f) stats::integrate(f, -Inf, Inf, subdivisions = 1000L)$value

test_that("lineshape functions have unit-area profiles and the right FWHM", {
  expect_equal(area(function(x) gaussian_profile(x, 0, 1, 0.7, 3)), 3, tolerance = 1e-6)
  expect_equal(area(function(x) lorentzian_profile(x, 0, 1, 0.7, 3)), 3, tolerance = 1e-4)
  expect_equal(area(function(x) pseudo_voigt_profile(x, 0, 1, 0.5, 0.4, 3)$y), 3, tolerance = 1e-4)
  expect_equal(area(function(x) pseudo_voigt_profile(x, 0, 1, 0.5, 0.4, 3, eta = 0.3)), 3, tolerance = 1e-4)
  # half maximum at xc +/- FWHM / 2
  for (f in list(function(x) gaussian_profile(x, 0, 2, 0.6, 1), function(x) lorentzian_profile(x, 0, 2, 0.6, 1))) {
    expect_equal(f(2 + 0.3) / f(2), 0.5, tolerance = 1e-10)
  }
  expect_equal(gaussian_profile(0, y0 = 5, xc = 0, wG = 1, A = 0), 5)
})

test_that("pseudo_voigt reduces to its components", {
  x <- seq(-3, 3, length.out = 50)
  expect_equal(pseudo_voigt_profile(x, 0, 0, 1, 0.5, 2, eta = 0), gaussian_profile(x, 0, 0, 1, 2))
  expect_equal(pseudo_voigt_profile(x, 0, 0, 1, 0.5, 2, eta = 1), lorentzian_profile(x, 0, 0, 0.5, 2))
  res <- pseudo_voigt_profile(x, 1, 0, 1, 0.5, 2)
  expect_named(res, c("y", "eta"))
  expect_true(res$eta > 0 && res$eta < 1)
  expect_equal(min(res$y), 1, tolerance = 0.1)
  # nearly pure Gaussian / Lorentzian limits of the TCH mixing parameter
  expect_lt(pseudo_voigt_profile(x, 0, 0, 1, 1e-6, 1)$eta, 1e-4)
  expect_gt(pseudo_voigt_profile(x, 0, 0, 1e-6, 1, 1)$eta, 0.9999)
  expect_error(pseudo_voigt_profile(x, 0, 0, 1, 1, 1, eta = 2), "between 0 and 1")
  expect_error(gaussian_profile("a", 0, 0, 1, 1), "numeric vector")
  expect_error(lorentzian_profile(x, 0, 0, -1, 1), "positive")
})

wl <- seq(395, 397, by = 0.01)

test_that("peak_fit recovers the parameters of a single line", {
  set.seed(1)
  truth <- list(y0 = 10, xc = 396.15, w = 0.2, A = 50)
  for (profile in c("gaussian", "lorentzian")) {
    f <- if (profile == "gaussian") gaussian_profile else lorentzian_profile
    y <- f(wl, truth$y0, truth$xc, truth$w, truth$A) + stats::rnorm(length(wl), sd = 0.3)
    res <- peak_fit(wide_spectrum(wl, y), profile = profile)
    expect_named(res, c("spectrum", "data", "fit", "tidied", "augmented"))
    est <- stats::setNames(res$tidied[[1]]$estimate, res$tidied[[1]]$term)
    expect_equal(unname(est["xc"]), truth$xc, tolerance = 1e-3)
    expect_equal(unname(est["A"]), truth$A, tolerance = 0.03)
    expect_equal(unname(est["y0"]), truth$y0, tolerance = 0.05)
    expect_named(res$augmented[[1]], c("x", "y", ".fitted", ".resid"))
  }
  y <- pseudo_voigt_profile(wl, 5, 396, 0.1, 0.1, 20)$y + stats::rnorm(length(wl), sd = 0.1)
  res <- peak_fit(wide_spectrum(wl, y), profile = "voigt")
  est <- stats::setNames(res$tidied[[1]]$estimate, res$tidied[[1]]$term)
  expect_equal(unname(est["xc"]), 396, tolerance = 1e-3)
  expect_equal(unname(est["A"]), 20, tolerance = 0.05)
})

test_that("peak_fit fits several spectra, with ids and wavelength windows", {
  set.seed(2)
  spectra <- rbind(
    gaussian_profile(wl, 1, 396, 0.2, 10) + stats::rnorm(length(wl), sd = 0.05),
    gaussian_profile(wl, 2, 396.3, 0.2, 20) + stats::rnorm(length(wl), sd = 0.05)
  )
  df <- as.data.frame(spectra)
  names(df) <- wl
  df <- cbind(sample = c("s1", "s2"), df)
  res <- peak_fit(df, profile = "gaussian", id = "sample", wlgth.min = 395.5, wlgth.max = 396.8)
  expect_equal(res$sample, c("s1", "s2"))
  xc <- sapply(res$tidied, function(t) t$estimate[t$term == "xc"])
  expect_equal(unname(xc), c(396, 396.3), tolerance = 1e-3)
  expect_true(all(sapply(res$data, function(d) all(d$x >= 395.5 & d$x <= 396.8))))
  expect_error(peak_fit(df, id = "nope"), "'id'")
  expect_error(peak_fit(df, id = "sample", wlgth.min = 397, wlgth.max = 396), "strictly smaller")
})

test_that("peak_fit validates its inputs", {
  df <- wide_spectrum(wl, gaussian_profile(wl, 0, 396, 0.2, 1))
  expect_error(peak_fit(), "Missing 'data' argument.")
  expect_error(peak_fit(as.matrix(df)), "data frame")
  expect_error(peak_fit(df, profile = "foo"), "must be")
  expect_error(peak_fit(df, max.iter = "a"), "numeric")
  expect_error(peak_fit(df, profile = "gaussian", A = -1), "positive")
})

test_that("multipeak_fit resolves overlapping lines", {
  set.seed(3)
  y <- 5 + gaussian_profile(wl, 0, 395.8, 0.15, 20) + lorentzian_profile(wl, 0, 396.2, 0.2, 30) +
    stats::rnorm(length(wl), sd = 0.2)
  res <- multipeak_fit(wide_spectrum(wl, y), peaks = c(395.8, 396.2),
                      profiles = c("Gaussian", "Lorentzian"))
  est <- stats::setNames(res$tidied[[1]]$estimate, res$tidied[[1]]$term)
  expect_equal(unname(est[c("xc_1", "xc_2")]), c(395.8, 396.2), tolerance = 1e-3)
  expect_equal(unname(est[c("A_1", "A_2")]), c(20, 30), tolerance = 0.05)
  aug <- res$augmented[[1]]
  expect_true(all(c(".peak_1", ".peak_2") %in% names(aug)))
  expect_equal(aug$.fitted, unname(est["y0"]) + aug$.peak_1 + aug$.peak_2, tolerance = 1e-10)
  expect_s3_class(plot_fit(res), "patchwork")
  # smooth curve on a fine grid: total = sum of the line components (each includes y0)
  grid <- seq(395, 397, length.out = 300)
  cv <- specProc:::fit_curve(res$fit[[1]], grid)
  expect_equal(nrow(cv), 300)
  expect_equal(cv$.fitted, cv$.peak_1 + cv$.peak_2 - unname(est["y0"]), tolerance = 1e-10)
  expect_error(multipeak_fit(wide_spectrum(wl, y), peaks = c(1, 2), profiles = c("gaussian", "x")), "Profiles")
  expect_error(multipeak_fit(wide_spectrum(wl, y), peaks = c(1, 2), profiles = rep("gaussian", 3)), "same length")
})

test_that("a failing fit gives a warning, not an error", {
  flat <- wide_spectrum(wl, rep(1, length(wl)))
  expect_warning(res <- peak_fit(flat, profile = "gaussian", max.iter = 5), "Fitting failed")
  expect_null(res$fit[[1]])
})

test_that("plot_fit plots single and multiple fits", {
  set.seed(4)
  y <- gaussian_profile(wl, 1, 396, 0.2, 10) + stats::rnorm(length(wl), sd = 0.05)
  res <- peak_fit(wide_spectrum(wl, y), profile = "gaussian")
  expect_s3_class(plot_fit(res, title = "Ca II"), "patchwork")
  expect_error(plot_fit(data.frame(a = 1)), "peak_fit")
})

test_that("voigt_profile matches exact references", {
  # value at the line center: erfcx(y) / (sigma sqrt(2 pi)), erfcx via pnorm
  for (wG in c(0.2, 1)) for (wL in c(0.01, 0.3, 3)) {
    s <- wG / (2 * sqrt(2 * log(2)))
    y <- (wL / 2) / (s * sqrt(2))
    ref <- 2 * exp(y^2) * stats::pnorm(-y * sqrt(2)) / (s * sqrt(2 * pi))
    expect_equal(voigt_profile(0, 0, 0, wG, wL, 1), ref, tolerance = 1e-10)
  }
  # numerical convolution of a Gaussian and a Lorentzian
  s <- 0.7 / (2 * sqrt(2 * log(2)))
  conv <- function(x) stats::integrate(function(t) stats::dnorm(t, 0, s) * stats::dcauchy(x - t, 0, 0.15),
                                       -Inf, Inf, rel.tol = 1e-12, subdivisions = 2000L)$value
  xs <- c(-20, -2, -0.3, 0.1, 1, 5)
  expect_equal(voigt_profile(xs, 0, 0, 0.7, 0.3, 1), sapply(xs, conv), tolerance = 1e-9)
  # limits, area and offset
  x <- seq(-3, 3, length.out = 41)
  expect_equal(voigt_profile(x, 0, 0, 1, 0, 1), gaussian_profile(x, 0, 0, 1, 1), tolerance = 1e-12)
  expect_equal(voigt_profile(x, 0, 0, 0, 1, 1), lorentzian_profile(x, 0, 0, 1, 1), tolerance = 1e-12)
  expect_equal(area(function(x) voigt_profile(x, 0, 1, 0.5, 0.4, 3)), 3, tolerance = 1e-6)
  expect_equal(voigt_profile(1, 5, 1, 0.5, 0.4, 0), 5)
  # the pseudo-Voigt approximation is within about 1.5% of the exact profile
  v <- voigt_profile(x, 0, 0, 1, 0.5, 1)
  expect_lt(max(abs(v - pseudo_voigt_profile(x, 0, 0, 1, 0.5, 1)$y)) / max(v), 0.015)
  expect_error(voigt_profile(x, 0, 0, 0, 0, 1), "both be zero")
  expect_error(voigt_profile(x, 0, 0, -1, 1, 1), "non-negative")
})

test_that("peak_fit recovers the widths of an exact Voigt line", {
  set.seed(5)
  y <- voigt_profile(wl, 2, 396, 0.15, 0.1, 30) + stats::rnorm(length(wl), sd = 0.05)
  res <- peak_fit(wide_spectrum(wl, y), profile = "voigt")
  est <- stats::setNames(res$tidied[[1]]$estimate, res$tidied[[1]]$term)
  expect_equal(unname(est[c("xc", "wG", "wL", "A")]), c(396, 0.15, 0.1, 30), tolerance = 0.05)
  pv <- peak_fit(wide_spectrum(wl, y), profile = "pseudo_voigt")
  expect_gt(stats::deviance(pv$fit[[1]]), stats::deviance(res$fit[[1]]))
})

test_that("plot_fit gives the parameters of the peaks and the quality of the fit", {
  set.seed(6)
  y <- gaussian_profile(wl, 1, 396, 0.2, 10) + stats::rnorm(length(wl), sd = 0.05)
  res <- peak_fit(wide_spectrum(wl, y), profile = "gaussian")
  fit <- res$fit[[1]]
  aug <- res$augmented[[1]]
  s <- fit_summary(fit, aug, fit_curve(fit, seq(min(wl), max(wl), length.out = 500)))
  est <- stats::coef(fit)
  se <- sqrt(diag(stats::vcov(fit)))
  expect_equal(s$peaks$center, unname(est["xc"]))
  expect_equal(s$peaks$fwhm, unname(est["wG"]))
  expect_equal(s$peaks$area_se, unname(se["A"]))
  expect_equal(s$rmse, summary(fit)$sigma)
  expect_equal(s$r2, 1 - sum(aug$.resid^2) / sum((aug$y - mean(aug$y))^2))
  # the FWHM of a Voigt profile, with its standard error by the delta method
  v <- peak_fit(wide_spectrum(wl, voigt_profile(wl, 1, 396, 0.1, 0.15, 10) +
                                stats::rnorm(length(wl), sd = 0.05)), profile = "voigt")
  vf <- v$fit[[1]]
  sv <- fit_summary(vf, v$augmented[[1]], fit_curve(vf, wl))
  ev <- stats::coef(vf)
  expect_equal(sv$peaks$fwhm, voigt_fwhm(ev[["wG"]], ev[["wL"]]))
  h <- 1e-6
  grad <- c((voigt_fwhm(ev[["wG"]] + h, ev[["wL"]]) - voigt_fwhm(ev[["wG"]] - h, ev[["wL"]])) / (2 * h),
            (voigt_fwhm(ev[["wG"]], ev[["wL"]] + h) - voigt_fwhm(ev[["wG"]], ev[["wL"]] - h)) / (2 * h))
  vc <- stats::vcov(vf)[c("wG", "wL"), c("wG", "wL")]
  expect_equal(sv$peaks$fwhm_se, sqrt(drop(t(grad) %*% vc %*% grad)), tolerance = 1e-5)
  # rounded to two significant digits of the standard error
  expect_equal(estimate_text(656.33124, 0.00942), "656.3312 ± 0.0094")
  expect_equal(estimate_text(26000.4, 773), "26,000 ± 770")
  expect_equal(estimate_text(1.23456, NA), "1.235")
})

test_that("plot_fit annotates, marks the FWHM and keeps the former arguments", {
  set.seed(7)
  y <- gaussian_profile(wl, 1, 396, 0.2, 10) + stats::rnorm(length(wl), sd = 0.05)
  res <- peak_fit(wide_spectrum(wl, y), profile = "gaussian")
  p <- plot_fit(res, title = "Ca II", show_fwhm = TRUE)
  expect_s3_class(p, "patchwork")
  top <- p[[1]][[1]]
  expect_match(top$labels$subtitle, "^Center .*nm\nFWHM .*nm, area .*\nR² ")
  expect_true(any(vapply(top$layers, function(l) inherits(l$geom, "GeomSegment"), logical(1))))
  expect_match(p$patches$annotation$caption, "Gaussian profile")
  expect_match(p$patches$annotation$caption, "Bars: the FWHM")
  expect_null(plot_fit(res, annotate = FALSE)[[1]][[1]]$labels$subtitle)
  expect_null(plot_fit(res, caption = FALSE)$patches$annotation$caption)
  # the unit of the center and FWHM comes from xlab
  expect_match(plot_fit(res, xlab = "Wavelength (Å)")[[1]][[1]]$labels$subtitle, "Å\nFWHM")
  # several spectra: one pair of panels each
  two <- rbind(wide_spectrum(wl, y), wide_spectrum(wl, 1.2 * y))
  expect_length(plot_fit(peak_fit(two, profile = "gaussian"))$patches$plots, 2)
  lifecycle::expect_deprecated(plot_fit(res, pt.size = 2))
  lifecycle::expect_deprecated(plot_fit(res, resid.fill = "blue"))
  expect_error(plot_fit(res, fit_color = "nocolor"), "colors")
})
