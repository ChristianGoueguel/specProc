# Unit-area line profiles used by the exported lineshape functions and by the
# peak fitting routines. They perform no argument checking so that the
# optimizer can evaluate them freely.

profile_gaussian <- function(x, xc, wG) {
  sqrt(4 * log(2) / pi) / wG * exp(-4 * log(2) * (x - xc)^2 / wG^2)
}

profile_lorentzian <- function(x, xc, wL) {
  (2 / pi) * wL / (4 * (x - xc)^2 + wL^2)
}

# Thompson-Cox-Hastings pseudo-Voigt: a common FWHM for both components and a
# mixing parameter that depends on the Gaussian and Lorentzian widths.
tch_width <- function(wG, wL) {
  (wG^5 + 2.69269 * wG^4 * wL + 2.42843 * wG^3 * wL^2 +
     4.47163 * wG^2 * wL^3 + 0.07842 * wG * wL^4 + wL^5)^(1 / 5)
}

tch_eta <- function(wG, wL) {
  r <- wL / tch_width(wG, wL)
  1.36603 * r - 0.47719 * r^2 + 0.11116 * r^3
}

profile_voigt <- function(x, xc, wG, wL) {
  f <- tch_width(wG, wL)
  eta <- tch_eta(wG, wL)
  eta * profile_lorentzian(x, xc, f) + (1 - eta) * profile_gaussian(x, xc, f)
}
