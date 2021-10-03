initParam <- function(profile = "Voigt", y0 = NULL, xc = NULL, wG = NULL, wL = NULL, A = NULL) {

  if (profile != "Lorentzian" || profile != "Gaussian" || profile != "Voigt") {
    stop("The profile function must be Lorentzian, Gaussian or Voigt")
  }

  if (wL < 0 || wG < 0 || A < 0) {
    stop("Initial parameter must be non-negative")
  }

  if (is.null(y0) == TRUE && is.null(xc) == TRUE && is.null(wL) == TRUE && is.null(wG) == TRUE && is.null(A) == TRUE) {
    guess <- list(
      y0 = .$y[which.min(.$y)],
      xc = .$x[which.max(.$y)],
      wL = 0.1,
      wG = 0.1,
      A = 1
    )
    return(guess)
  }


}
