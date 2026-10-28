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
  res <- peak_fit(df, profile = "gaussian", id = "sample", wavelength = c(395.5, 396.8))
  expect_equal(res$sample, c("s1", "s2"))
  xc <- sapply(res$tidied, function(t) t$estimate[t$term == "xc"])
  expect_equal(unname(xc), c(396, 396.3), tolerance = 1e-3)
  expect_true(all(sapply(res$data, function(d) all(d$x >= 395.5 & d$x <= 396.8))))
  expect_error(peak_fit(df, id = "nope"), "'id'")
  expect_error(peak_fit(df, id = "sample", wavelength = c(397, 396)), "min < max")
  rlang::local_options(lifecycle_verbosity = "warning")
  # the former bounds, each with a deprecation warning
  expect_warning(lo <- peak_fit(df, profile = "gaussian", id = "sample", wlgth.min = 395.5),
                 "wlgth.min")
  expect_equal(lo$data, peak_fit(df, profile = "gaussian", id = "sample",
                                 wavelength = c(395.5, Inf))$data)
  expect_warning(hi <- peak_fit(df, profile = "gaussian", id = "sample", wlgth.max = 396.8),
                 "wlgth.max")
  expect_equal(hi$data, peak_fit(df, profile = "gaussian", id = "sample",
                                 wavelength = c(-Inf, 396.8))$data)
})

test_that("peak_fit validates its inputs", {
  df <- wide_spectrum(wl, gaussian_profile(wl, 0, 396, 0.2, 1))
  expect_error(peak_fit(), "Missing 'data' argument.")
  expect_error(peak_fit(as.matrix(df)), "data frame")
  expect_error(peak_fit(df, profile = "foo"), "must be")
  expect_error(peak_fit(df, max_iter = "a"), "numeric")
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
  expect_warning(res <- peak_fit(flat, profile = "gaussian", max_iter = 5), "Fitting failed")
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

test_that("peak_fit and multipeak_fit hold widths fixed", {
  set.seed(8)
  y <- voigt_profile(wl, 2, 396, 0.15, 0.05, 30) + stats::rnorm(length(wl), sd = 0.2)
  free <- peak_fit(wide_spectrum(wl, y), profile = "voigt")
  held <- peak_fit(wide_spectrum(wl, y), profile = "voigt", wG = 0.15, fixed = "wG")
  t <- held$tidied[[1]]
  expect_equal(t$term, c("y0", "xc", "wG", "wL", "A"))
  expect_equal(unname(t$estimate[t$term == "wG"]), 0.15)
  expect_true(is.na(t$std.error[t$term == "wG"]))
  expect_false("wG" %in% names(stats::coef(held$fit[[1]])))
  expect_true(line_estimates(held$fit[[1]])$constant[["wG"]])
  # the Lorentzian width is better determined than with both widths free
  tf <- free$tidied[[1]]
  expect_lt(t$std.error[t$term == "wL"], tf$std.error[tf$term == "wL"])
  expect_equal(unname(t$estimate[t$term == "wL"]), 0.05, tolerance = 0.2)
  # plot_fit uses the fixed width
  s <- fit_summary(held$fit[[1]], held$augmented[[1]], fit_curve(held$fit[[1]], wl))
  expect_equal(s$peaks$fwhm, voigt_fwhm(0.15, unname(t$estimate[t$term == "wL"])))
  expect_gt(s$peaks$fwhm_se, 0)
  expect_equal(s$rmse, summary(held$fit[[1]])$sigma)
  expect_s3_class(plot_fit(held), "patchwork")
  # both widths fixed: only the baseline, center and area are fitted
  both <- peak_fit(wide_spectrum(wl, y), profile = "voigt", wG = 0.15, wL = 0.05,
                   fixed = c("wG", "wL"))
  expect_named(stats::coef(both$fit[[1]]), c("y0", "xc", "A"))
  expect_equal(unname(both$tidied[[1]]$estimate[both$tidied[[1]]$term == "A"]), 30,
               tolerance = 0.05)

  # one fixed width per peak, for the peaks whose profile has it
  y2 <- voigt_profile(wl, 1, 395.7, 0.10, 0.05, 10) + voigt_profile(wl, 0, 396.2, 0.12, 0.08, 20) +
    stats::rnorm(length(wl), sd = 0.05)
  mp <- multipeak_fit(wide_spectrum(wl, y2), peaks = c(395.7, 396.2), profiles = "voigt",
                      wG = c(0.10, 0.12), fixed = "wG")
  tm <- mp$tidied[[1]]
  expect_equal(unname(tm$estimate[match(c("wG_1", "wG_2"), tm$term)]), c(0.10, 0.12))
  expect_equal(unname(tm$estimate[match(c("wL_1", "wL_2"), tm$term)]), c(0.05, 0.08),
               tolerance = 0.15)
  mixed <- multipeak_fit(wide_spectrum(wl, y2), peaks = c(395.7, 396.2),
                         profiles = c("lorentzian", "voigt"), wG = 0.12, fixed = "wG")
  expect_equal(line_estimates(mixed$fit[[1]])$estimate[["wG_2"]], 0.12)
  expect_true(line_estimates(mixed$fit[[1]])$constant[["wG_2"]])

  expect_error(peak_fit(wide_spectrum(wl, y), profile = "voigt", fixed = "wG"), "give 'wG'")
  expect_error(peak_fit(wide_spectrum(wl, y), profile = "lorentzian", wG = 0.1, fixed = "wG"),
               "Gaussian width")
  expect_error(peak_fit(wide_spectrum(wl, y), profile = "voigt", wG = 0.1, fixed = "xc"), "'fixed'")
})

test_that("doppler_width follows the thermal broadening", {
  expect_equal(doppler_width(400, 1e4, 40), 0.0045297, tolerance = 1e-4)
  expect_equal(doppler_width(656.28, 1e4, "H"), 0.0469, tolerance = 0.01)
  # proportional to the wavelength and to sqrt(T / m)
  expect_equal(doppler_width(800, 4e4, 10) / doppler_width(400, 1e4, 40), 2 * 2 * 2)
  expect_equal(doppler_width(c(393.37, 656.28), 1e4, c("Ca", "H")),
               c(doppler_width(393.37, 1e4, "Ca"), doppler_width(656.28, 1e4, "H")))
  expect_error(doppler_width(400, 1e4, "Xx"), "Unknown element")
  expect_error(doppler_width(-1, 1e4, "Ca"), "wavelength")
  expect_error(doppler_width(400, 0, "Ca"), "temperature")
  expect_error(doppler_width(400, 1e4, -2), "mass")
})

test_that("multipeak_fit locks centers, shares widths and fixes area ratios", {
  peaks <- c(395.80, 396.05, 396.40)
  set.seed(9)
  truth_shift <- 0.012
  y <- 2 + gaussian_profile(wl, 0, peaks[1] + truth_shift, 0.06, 6) +
    gaussian_profile(wl, 0, peaks[2] + truth_shift, 0.06, 3) +
    lorentzian_profile(wl, 0, 396.40, 0.10, 4) + stats::rnorm(length(wl), sd = 0.02)
  spec <- wide_spectrum(wl, y)
  # the first two lines form a multiplet; the third interferes
  res <- multipeak_fit(spec, peaks, profiles = c("gaussian", "gaussian", "lorentzian"),
                       lock_centers = c(TRUE, TRUE, FALSE), share_widths = c(1, 1, NA),
                       area_ratios = c(2, 1, NA))
  fit <- res$fit[[1]]
  expect_named(stats::coef(fit), c("y0", "shift_1_2", "wG_1_2", "A_1_2", "xc_3", "wL_3", "A_3"))
  t <- res$tidied[[1]]
  est <- stats::setNames(t$estimate, t$term)
  expect_equal(t$term, c("y0", "xc_1", "wG_1", "A_1", "xc_2", "wG_2", "A_2", "xc_3", "wL_3", "A_3",
                         "shift_1_2"))
  expect_equal(unname(est["shift_1_2"]), truth_shift, tolerance = 0.05)
  expect_equal(unname(est["xc_2"] - est["xc_1"]), diff(peaks[1:2]))
  expect_equal(unname(est["wG_1"]), unname(est["wG_2"]))
  expect_equal(unname(est["A_1"] / est["A_2"]), 2)
  expect_equal(unname(est[c("wG_1", "A_1", "A_2", "xc_3", "A_3")]),
               c(0.06, 6, 3, 396.40, 4), tolerance = 0.03)
  # propagated standard errors: equal for shared parameters, scaled for ratios
  se <- stats::setNames(t$std.error, t$term)
  cov <- stats::vcov(fit)
  expect_equal(unname(se["xc_1"]), sqrt(cov["shift_1_2", "shift_1_2"]), tolerance = 1e-6)
  expect_equal(unname(se["xc_2"]), unname(se["xc_1"]), tolerance = 1e-6)
  expect_equal(unname(se["A_1"]), 2 * unname(se["A_2"]), tolerance = 1e-6)
  expect_equal(unname(se["shift_1_2"]), unname(se["xc_1"]), tolerance = 1e-6)
  # the augmented columns and plot_fit use the constrained lines
  expect_equal(res$augmented[[1]]$.fitted,
               est[["y0"]] + res$augmented[[1]]$.peak_1 + res$augmented[[1]]$.peak_2 +
                 res$augmented[[1]]$.peak_3, tolerance = 1e-8)
  s <- fit_summary(fit, res$augmented[[1]], fit_curve(fit, wl))
  expect_equal(s$peaks$center, unname(est[c("xc_1", "xc_2", "xc_3")]))
  expect_equal(s$peaks$area_se, unname(se[c("A_1", "A_2", "A_3")]), tolerance = 1e-6)
  expect_s3_class(plot_fit(res), "patchwork")

  # TRUE constrains every line: one shift, one width
  all_locked <- multipeak_fit(spec, peaks[1:2], profiles = "gaussian", lock_centers = TRUE,
                              share_widths = TRUE, wavelength = c(395, 396.25))
  expect_true(all(c("shift", "wG") %in% names(stats::coef(all_locked$fit[[1]]))))
  # unconstrained fits keep their parameters
  free <- multipeak_fit(spec, peaks[1:2], profiles = "gaussian", wavelength = c(395, 396.25))
  expect_named(stats::coef(free$fit[[1]]), c("y0", "xc_1", "wG_1", "A_1", "xc_2", "wG_2", "A_2"))
  expect_equal(free$tidied[[1]]$std.error, unname(sqrt(diag(stats::vcov(free$fit[[1]])))))

  expect_error(multipeak_fit(spec, peaks, "gaussian", lock_centers = c(TRUE, FALSE)), "lock_centers")
  expect_error(multipeak_fit(spec, peaks, c("gaussian", "lorentzian", "gaussian"),
                             share_widths = TRUE), "same profile")
  expect_error(multipeak_fit(spec, peaks, "gaussian", area_ratios = c(1, -1, NA)), "area_ratios")
  expect_warning(multipeak_fit(spec, c(395.8, 398), "gaussian", lock_centers = TRUE),
                 "within the fitted wavelength range")
})

test_that("lte_criteria computes the relaxation time and the diffusion length", {
  kt <- 8.617333262e-5 * 9000
  out <- lte_criteria(9000, 1e17, delta_e = 3.15, oscillator_strength = 0.68, mass = "Ca",
                      length_scale = 1)
  expect_s3_class(out, "tbl_df")
  expect_equal(out$minimum_density, 1.6e12 * sqrt(9000) * 3.15^3)
  expect_true(out$mcwhirter)
  expect_equal(out$relaxation_time, 6.3e4 / (1e17 * 0.68 * 0.2) * 3.15 * sqrt(kt) * exp(3.15 / kt))
  ca <- 40.078
  expect_equal(out$diffusion_length,
               10 * 1.4e12 / 1e17 * kt^0.75 * sqrt(3.15 / (ca * 0.68 * 0.2)) * exp(3.15 / (2 * kt)),
               tolerance = 1e-4)
  expect_equal(out$length_ratio, out$diffusion_length / 1)
  expect_true(out$spatial)
  # lower densities relax more slowly
  slow <- lte_criteria(9000, c(1e15, 1e17), 3.15, 0.68, "Ca")
  expect_gt(slow$relaxation_time[1], slow$relaxation_time[2])
  expect_false(slow$mcwhirter[1])
})

test_that("lte_criteria checks the temporal criterion on a time series", {
  time <- c(1, 2, 3, 5) * 1e-6
  t <- 12000 * (time / 1e-6)^-0.3
  ne <- 3e17 * (time / 1e-6)^-1
  out <- lte_criteria(t, ne, 3.15, 0.68, "Ca", time = time, tolerance = 0.01)
  expect_equal(out$time, time)
  # relative change over the relaxation time: |d log T / dt| tau
  rate <- c((log(t[2]) - log(t[1])) / 1e-6, (log(t[3]) - log(t[1])) / 2e-6,
            (log(t[4]) - log(t[2])) / 3e-6, (log(t[4]) - log(t[3])) / 2e-6)
  expect_equal(out$temperature_change, abs(expm1(rate * out$relaxation_time)))
  expect_true(all(out$temporal))
  # a plasma that changes within nanoseconds
  fast <- lte_criteria(t, ne / 100, 3.15, 0.68, "Ca", time = time * 1e-3)
  expect_false(any(fast$temporal))
  # order of the times does not matter
  shuffled <- lte_criteria(t[4:1], ne[4:1], 3.15, 0.68, "Ca", time = time[4:1])
  expect_equal(shuffled$temperature_change, rev(out$temperature_change))
  expect_error(lte_criteria(t, ne, 3.15, 0.68, "Ca", time = 1e-6), "'time'")
  expect_error(lte_criteria(9000, 1e17, 3.15, 0.68, "Xx"), "Unknown element")
  expect_error(lte_criteria(9000, -1, 3.15, 0.68, "Ca"), "electron_density")
  expect_error(lte_criteria(9000, 1e17, 3.15, 0.68, "Ca", length_scale = 0), "length_scale")
})
