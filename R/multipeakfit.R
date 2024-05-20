#' @title Multiple Peaks Fitting
#' @author Christian L. Goueguel
#' @description Fitting of multiple spectral lines by the same or different lineshape functions with variable parameters.
#' @details The function uses `minpack.lm::nlsLM`, which is based on the Levenberg-Marquardt algorithm for searching the minimum value of the square of the sum of the residuals.
#' @param .data Data frame of emission spectra
#' @param peaks (vector) Center wavelengths of selected peaks
#' @param profiles (vector) Lineshape functions
#' @param wL (numeric) Lorentzian full width at half maximum (overall initial guess)
#' @param wG (numeric) Gaussian full width at half maximum (overall initial guess)
#' @param A (numeric) Peak area (overall initial guess)
#' @param wlgth.min (numeric) Lower bound of the wavelength subset
#' @param wlgth.max (numeric) Upper bound of the wavelength subset
#' @param id Spectra name (optional)
#' @param max.iter (numeric) Maximum number of iteration (200 by default)
#' @return Fitted value for each peak and the estimated parameters along with the corresponding errors
#' @export multipeakfit
multipeakfit <- function(.data, peaks, profiles, wL = NULL, wG = NULL, A = NULL, wlgth.min = NULL, wlgth.max = NULL, id = NULL, max.iter = 200) {
  if (length(.data) == 0 & is.null(.data) == TRUE) {
    stop("Apparently you forgot to provide the spectra.")
  }
  if (is.data.frame(.data) == FALSE & tibble::is.tibble(.data) == FALSE) {
    stop("Data must be of class tbl_df, tbl or data.frame")
  }
  if (is.numeric(peaks) == FALSE) {
    stop("Please enter a valid vector of wavelengths")
  }
  if (is.character(profiles) == FALSE) {
    stop("Profiles must be a valid vector of lineshape functions: Lorentzian, Gaussian and Voigt")
  }
  if (length(peaks) < 2 | length(profiles) < 2) {
    stop("The number of peaks to be fitted must be at least 2 otherwise use the Peakfit function")
  }
  if (length(peaks) != length(profiles)) {
    stop("Peaks and profiles must have the same length")
  }

  x <- NULL
  fit <- NULL

  rlang::check_installed("broom")
  if (is.null(id) == TRUE) {
    if (is.null(wlgth.min) == FALSE & is.null(wlgth.max) == TRUE) {
      wlgth.min <- as.numeric(wlgth.min)
      df <- .data %>%
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
      df <- .data %>%
        tidyr::pivot_longer(
          cols = tidyr::everything(),
          names_to = "x",
          values_to = "y"
        ) %>%
        purrr::modify_at("x", as.numeric) %>%
        dplyr::filter(x <= wlgth.max)
    }
    if (is.null(wlgth.min) == TRUE & is.null(wlgth.max) == TRUE) {
      df <- .data %>%
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
      if (wlgth.min >= wlgth.max) {
        stop("wlgth.min must be strictly smaller than wlgth.max")
      } else {
        df <- .data %>%
          tidyr::pivot_longer(
            cols = tidyr::everything(),
            names_to = "x",
            values_to = "y"
          ) %>%
          purrr::modify_at("x", as.numeric) %>%
          dplyr::filter(x >= wlgth.min & x <= wlgth.max)
      }
    }

    lgth <- length(peaks)
    p <- max.iter
    .fit <- tibble::tibble()

    for (i in 1:lgth) {
      if (profiles[i] == "Lorentzian") {
        if (is.null(wL) == FALSE & is.null(A) == FALSE) {
          param1 <- wL
          param2 <- A
          } else {
            stop("Please provide an initial guess value for the Lorentzian fitting paramters: wL and A")
            }
        .fit[i, 1:6] <- df %>%
          tidyr::nest(data = tidyr::everything()) %>%
          dplyr::mutate(
            peak = peaks[i],
            lineshape = profiles[i],
            fit = purrr::map(
              .data, ~ minpack.lm::nlsLM(
                data = .,
                y ~ lorentzianfun(x, y0, xc, wL, A),
                start =  list(
                  y0 = .$y[which.min(.$y)],
                  xc = peaks[i],
                  wL = param1[i],
                  A = param2[i]
                  ),
                control = minpack.lm::nls.lm.control(maxiter = p),
                lower = c(0, 0, 0, 0)
                )
              ),
            tidied = purrr::map(fit, broom::tidy),
            augmented = purrr::map(fit, broom::augment)
          )
        }
      if(profiles[i] == "Gaussian") {
        if (is.null(wG) == FALSE & is.null(A) == FALSE) {
          param1 <- wG
          param2 <- A
          } else {
            stop("Please provide an initial guess value for the Gaussian fitting paramters: wG and A")
            }
        .fit[i, 1:6] <- df %>%
          tidyr::nest(data = tidyr::everything()) %>%
          dplyr::mutate(
            peak = peaks[i],
            lineshape = profiles[i],
            fit = purrr::map(
              .data, ~ minpack.lm::nlsLM(
                data = .,
                y ~ gaussianfun(x, y0, xc, wG, A),
                start =  list(
                  y0 = .$y[which.min(.$y)],
                  xc = peaks[i],
                  wG = param1[i],
                  A = param2[i]
                  ),
                control = minpack.lm::nls.lm.control(maxiter = p),
                lower = c(0, 0, 0, 0)
                )
              ),
            tidied = purrr::map(fit, broom::tidy),
            augmented = purrr::map(fit, broom::augment)
          )
        }
      if(profiles[i] == "Voigt") {
        if (is.null(wL) == FALSE & is.null(wG) == FALSE & is.null(A) == FALSE) {
          param1 <- wL
          param2 <- wG
          param3 <- A
          } else {
            stop("Please provide an initial guess value for the Voigt fitting paramters: wL, wG and A")
            }
        .fit[i, 1:6] <- df %>%
          tidyr::nest(data = tidyr::everything()) %>%
          dplyr::mutate(
            peak = peaks[i],
            lineshape = profiles[i],
            fit = purrr::map(
              .data, ~ minpack.lm::nlsLM(
                data = .,
                y ~ voigtfun(x, y0, xc, wG, wL, A),
                start =  list(
                  y0 = .$y[which.min(.$y)],
                  xc = peaks[i],
                  wL = param1[i],
                  wG = param2[i],
                  A = param3[i]
                  ),
                control = minpack.lm::nls.lm.control(maxiter = p),
                lower = c(0, 0, 0, 0, 0)
                )
              ),
            tidied = purrr::map(fit, broom::tidy),
            augmented = purrr::map(fit, broom::augment)
          )
        }
      }
  } else {
    if (is.null(wlgth.min) == FALSE & is.null(wlgth.max) == TRUE) {
      wlgth.min <- as.numeric(wlgth.min)
      df <- .data %>%
        tidyr::pivot_longer(
          cols = !id,
          names_to = "x",
          values_to = "y"
        ) %>%
        purrr::modify_at("x", as.numeric) %>%
        dplyr::filter(x >= wlgth.min)
    }

    if (is.null(wlgth.min) == TRUE & is.null(wlgth.max) == FALSE) {
      wlgth.max <- as.numeric(wlgth.max)
      df <- .data %>%
        tidyr::pivot_longer(
          cols = !id,
          names_to = "x",
          values_to = "y"
        ) %>%
        purrr::modify_at("x", as.numeric) %>%
        dplyr::filter(x <= wlgth.max)
    }

    if (is.null(wlgth.min) == TRUE & is.null(wlgth.max) == TRUE) {
      df <- .data %>%
        tidyr::pivot_longer(
          cols = !id,
          names_to = "x",
          values_to = "y"
        ) %>%
        purrr::modify_at("x", as.numeric)
    }

    if (is.null(wlgth.min) == FALSE & is.null(wlgth.max) == FALSE) {
      wlgth.min <- as.numeric(wlgth.min)
      wlgth.max <- as.numeric(wlgth.max)
      if (wlgth.min >= wlgth.max) {
        stop("wlgth.min must be strictly smaller than wlgth.max")
      }
      df <- .data %>%
        tidyr::pivot_longer(
          cols = !id,
          names_to = "x",
          values_to = "y"
        ) %>%
        purrr::modify_at("x", as.numeric) %>%
        dplyr::filter(x >= wlgth.min & x <= wlgth.max)
    }

    lgth <- length(peaks)
    p <- max.iter

    for (i in 1:lgth) {
      if (profiles[i] == "Lorentzian") {
        if (is.null(wL) == FALSE & is.null(A) == FALSE) {
          param1 <- wL
          param2 <- A
        } else {
          stop("Please provide an initial guess value for the Lorentzian fitting paramters: wL and A")
        }
        .fit[i, 1:6] <- df %>%
          tidyr::nest(data = tidyr::everything()) %>%
          dplyr::mutate(
            peak = peaks[i],
            lineshape = profiles[i],
            fit = purrr::map(
              .data, ~ minpack.lm::nlsLM(
                data = .,
                y ~ lorentzianfun(x, y0, xc, wL, A),
                start =  list(
                  y0 = .$y[which.min(.$y)],
                  xc = peaks[i],
                  wL = param1[i],
                  A = param2[i]
                ),
                control = minpack.lm::nls.lm.control(maxiter = p),
                lower = c(0, 0, 0, 0)
              )
            ),
            tidied = purrr::map(fit, broom::tidy),
            augmented = purrr::map(fit, broom::augment)
          )
      }
      if(profiles[i] == "Gaussian") {
        if (is.null(wG) == FALSE & is.null(A) == FALSE) {
          param1 <- wG
          param2 <- A
        } else {
          stop("Please provide an initial guess value for the Gaussian fitting paramters: wG and A")
        }
        .fit[i, 1:6] <- df %>%
          tidyr::nest(data = tidyr::everything()) %>%
          dplyr::mutate(
            peak = peaks[i],
            lineshape = profiles[i],
            fit = purrr::map(
              .data, ~ minpack.lm::nlsLM(
                data = .,
                y ~ gaussianfun(x, y0, xc, wG, A),
                start =  list(
                  y0 = .$y[which.min(.$y)],
                  xc = peaks[i],
                  wG = param1[i],
                  A = param2[i]
                ),
                control = minpack.lm::nls.lm.control(maxiter = p),
                lower = c(0, 0, 0, 0)
              )
            ),
            tidied = purrr::map(fit, broom::tidy),
            augmented = purrr::map(fit, broom::augment)
          )
      }
      if(profiles[i] == "Voigt") {
        if (is.null(wL) == FALSE & is.null(wG) == FALSE & is.null(A) == FALSE) {
          param1 <- wL
          param2 <- wG
          param3 <- A
        } else {
          stop("Please provide an initial guess value for the Voigt fitting paramters: wL, wG and A")
        }
        .fit[i, 1:6] <- df %>%
          tidyr::nest(data = tidyr::everything()) %>%
          dplyr::mutate(
            peak = peaks[i],
            lineshape = profiles[i],
            fit = purrr::map(
              .data, ~ minpack.lm::nlsLM(
                data = .,
                y ~ voigtfun(x, y0, xc, wG, wL, A),
                start =  list(
                  y0 = .$y[which.min(.$y)],
                  xc = peaks[i],
                  wL = param1[i],
                  wG = param2[i],
                  A = param3[i]
                ),
                control = minpack.lm::nls.lm.control(maxiter = p),
                lower = c(0, 0, 0, 0, 0)
              )
            ),
            tidied = purrr::map(fit, broom::tidy),
            augmented = purrr::map(fit, broom::augment)
          )
      }
    }
  }
  return(.fit)
  }
