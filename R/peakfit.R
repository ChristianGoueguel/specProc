#' @title Spectral Fitting
#' @author Christian L. Goueguel
#' @description Fitting of spectroscopic peak by line shape functions with variable parameters.
#' @details The function uses `minpack.lm::nlsLM`, which is based on the Levenberg-Marquardt algorithm for searching the minimum value of the square of the sum of the residuals.
#' @param data Data frame of emission spectra
#' @param profile Line shape functions (Lorentzian, Gaussian and Voigt)
#' @param wlgth.min The lower bound of the wavelength subset
#' @param wlgth.max The upper bound of the wavelength subset
#' @return Fitted value and estimated parameters
#' @export peakfit
peakfit <- function(data, profile = "Voigt", wlgth.min = NULL, wlgth.max = NULL) {

  if (length(data) == 0 | is.null(data) == TRUE) {
    stop("Apparently you forgot to provide the spectra.")
  }

  if (is.data.frame(data) == FALSE & tibble::is.tibble(data) == FALSE) {
    stop("Data must be of class tbl_df, tbl or data.frame")
  }

  if (profile != "Lorentzian" || profile != "Gaussian" || profile != "Voigt") {
    stop("The profile function must be Lorentzian, Gaussian or Voigt")
  }

  if (is.numeric(wlgth.min) == FALSE | is.numeric(wlgth.min) == FALSE) {
    stop("When defining a subset of wavelengths, wlgth.min and wlgth.max must be numeric")
  }

  if (wlgth.min >= wlgth.max) {
    stop("wlgth.min must be strictly smaller than wlgth.max")
  }

  if (is.null(wlgth.min) == FALSE & is.null(wlgth.max) == TRUE) {
    wlgth.min <- as.numeric(wlgth.min)

    X <- data %>%
      tidyr::pivot_longer(
        cols = tidyr::everything(),
        names_to = "x",
        values_to = "y"
      ) %>%
      purrr::modify_at("x", as.numeric) %>%
      dplyr::filter(x >= wlgth.min)
  }

  if (is.null(wlgth.min) == TRUE & is.null(wlgth.max) == FALSE) {
    wlgth.max <- as.numeric(wlgth.max)

    X <- data %>%
      tidyr::pivot_longer(
        cols = tidyr::everything(),
        names_to = "x",
        values_to = "y"
      ) %>%
      purrr::modify_at("x", as.numeric) %>%
      dplyr::filter(x <= wlgth.max)
  }

  if (is.null(wlgth.min) == TRUE & is.null(wlgth.max) == TRUE) {
    X <- data %>%
      tidyr::pivot_longer(
        cols = tidyr::everything(),
        names_to = "x",
        values_to = "y"
      ) %>%
      purrr::modify_at("x", as.numeric)
  }

  if (is.null(wlgth.min) == FALSE & is.null(wlgth.max) == FALSE) {
    wlgth.min <- as.numeric(wlgth.min)
    wlgth.max <- as.numeric(wlgth.max)

    X <- data %>%
      tidyr::pivot_longer(
        cols = tidyr::everything(),
        names_to = "x",
        values_to = "y"
      ) %>%
      purrr::modify_at("x", as.numeric) %>%
      dplyr::filter(x >= wlgth.min & x <= wlgth.max)
  }

  if(profile == "Lorentzian") {
    fitpeak <- X %>%
      tidyr::nest() %>%
      dplyr::mutate(
        fit = purrr::map(
          data, ~ minpack.lm::nlsLM(
            data = .,
            y ~ lorentzian_func(x, y0, xc, wL, A),
            start = list(
              y0 = .$y[which.min(.$y)],
              xc = .$x[which.max(.$y)],
              wL = .abs(.$x[which.max(.$y)] - 0.5*(xmin + xmax)),
              A = abs(0.5*(.$y[which.max(.$y)] - .$y[which.min(.$y)])*(xmax - xmin))
            ),
            control = minpack.lm::nls.lm.control(maxiter = 200),
            lower = c(0, 0, 0, 0)
          )
        ),
        tidied = purrr::map(fit, broom::tidy),
        augmented = purrr::map(fit, broom::augment)
      )
    return(fitpeak)
  }

  if(profile == "Gaussian") {
    fitpeak <- X %>%
      tidyr::nest() %>%
      dplyr::mutate(
        fit = purrr::map(
          data, ~ minpack.lm::nlsLM(
            data = .,
            y ~ gaussian_func(x, y0, xc, wG, A),
            start = list(
              y0 = .$y[which.min(.$y)],
              xc = .$x[which.max(.$y)],
              wG = abs(.$x[which.max(.$y)] - 0.5*(xmin + xmax)),
              A = abs(0.5*(.$y[which.max(.$y)] - .$y[which.min(.$y)])*(xmax - xmin)),
              lower = c(0, 0, 0, 0)
            ),
            control = minpack.lm::nls.lm.control(maxiter = 200)
          )
        ),
        tidied = purrr::map(fit, broom::tidy),
        augmented = purrr::map(fit, broom::augment)
      )
    return(fitpeak)
  }

  if(profile == "Voigt") {
    fitpeak <- X %>%
      tidyr::nest() %>%
      dplyr::mutate(
        fit = purrr::map(
          data, ~ minpack.lm::nlsLM(
            data = .,
            y ~ voigt_func(x, y0, xc, wG, wL, A),
            start = list(
              y0 = .$y[which.min(.$y)],
              xc = .$x[which.max(.$y)],
              wG = abs(.$x[which.max(.$y)] - 0.5*(xmin + xmax)),
              wL = abs(.$x[which.max(.$y)] - 0.5*(xmin + xmax)),
              A = abs(0.5*(.$y[which.max(.$y)] - .$y[which.min(.$y)])*(xmax - xmin))
            ),
            control = minpack.lm::nls.lm.control(maxiter = 200),
            lower = c(0, 0, 0, 0, 0)
          )
        ),
        tidied = purrr::map(fit, broom::tidy),
        augmented = purrr::map(fit, broom::augment)
      )
    return(fitpeak)
  }
}
