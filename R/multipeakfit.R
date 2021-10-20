#' @title Fitting Multiple Peaks
#' @author Christian L. Goueguel
#' @description Fitting of spectroscopic peak by line shape functions with variable parameters.
#' @details The function uses `minpack.lm::nlsLM`, which is based on the Levenberg-Marquardt algorithm for searching the minimum value of the square of the sum of the residuals.
#' @param peaks (vector) center wavelengths of selected peaks
#' @param profiles (vector) Lineshape functions
#' @param wlgth.min (numeric) Lower bound of the wavelength subset
#' @param wlgth.max (numeric) Upper bound of the wavelength subset
#' @param id Spectra name (optional)
#' @param max.iter (numeric) Maximum number of iteration (200 by default)
#' @return Fitted value for each peak and the estimated parameters along with the corresponding errors
#' @export multipeakfit
multipeakfit <- function(peaks, profiles, wlgth.min = NULL, wlgth.max = NULL, id = NULL, max.iter = 200) {

  if (is.numeric(peaks) == FALSE) {
    stop("Please enter a valid vector of wavelengths")
  }

  if (is.character(profiles) == FALSE) {
    stop("Profiles must be a valid vector of lineshape functions: Lorentzian, Gaussian and Voigt")
  }

  if (profile != "Lorentzian" & profile != "Gaussian" & profile != "Voigt") {
    stop("The profile function must be Lorentzian, Gaussian or Voigt")
  }

  if (length(peaks) < 2 | length(profiles) < 2) {
    stop("The number of peaks to be fitted must be at least 2 otherwise use the Peakfit function")
  }

  if (is.integer(max.iter) == FALSE | max.iter <= 0) {
    stop("Maximum number of iterations must be a positive integer")
  }

  lgth.xc <- length(peaks)
  lgth.pfl <- length(profile)
  p <- max.iter

  if (is.null(id) == TRUE) {

    if (is.null(wlgth.min) == FALSE & is.null(wlgth.max) == TRUE) {
      wlgth.min <- as.numeric(wlgth.min)
      df <- data %>%
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
      df <- data %>%
        tidyr::pivot_longer(
          cols = tidyr::everything(),
          names_to = "x",
          values_to = "y"
        ) %>%
        purrr::modify_at("x", as.numeric) %>%
        dplyr::filter(x <= wlgth.max)
    }

    if (is.null(wlgth.min) == TRUE & is.null(wlgth.max) == TRUE) {
      df <- data %>%
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
        df <- data %>%
          tidyr::pivot_longer(
            cols = tidyr::everything(),
            names_to = "x",
            values_to = "y"
          ) %>%
          purrr::modify_at("x", as.numeric) %>%
          dplyr::filter(x >= wlgth.min & x <= wlgth.max)
      }
    }

    for (i in 1:lgth.xc) {
      wlgth.center <- as.numeric(peaks[i])

      for (j in 1:lgth.pfl) {
        line.pfl <- as.character(profile[j])

        if (line.pfl == "Lorentzian") {
          if (is.null(wL) == FALSE & is.null(A) == FALSE) {
            param1 <- as.numeric(wL)
            param2 <- as.numeric(A)
          } else {
            stop("Please provide an initial guess value for the Lorentzian fitting paramters: wL and A")
          }
          .fit <- df %>%
            tidyr::nest(data = tidyr::everything()) %>%
            dplyr::mutate(
              fit = purrr::map(
                data, ~ minpack.lm::nlsLM(
                  data = .,
                  y ~ lorentzian_func(x, y0, xc, wL, A),
                  start =  list(
                    y0 = .$y[which.min(.$y)],
                    xc = wlgth.center,
                    wL = param1,
                    A = param2
                  ),
                  control = minpack.lm::nls.lm.control(maxiter = p),
                  lower = c(0, 0, 0, 0)
                )
              ),
              tidied = purrr::map(fit, broom::tidy),
              augmented = purrr::map(fit, broom::augment)
            )
          .name <- paste("fitpeak", round(wlgth.center, digits = 2), sep = "_")
          assign(x = .name, value = .fit)
        }
        if(line.pfl == "Gaussian") {
          if (is.null(wG) == FALSE & is.null(A) == FALSE) {
            param1 <- as.numeric(wG)
            param2 <- as.numeric(A)
          } else {
            stop("Please provide an initial guess value for the Gaussian fitting paramters: wG and A")
          }
          .fit <- df %>%
            tidyr::nest(data = tidyr::everything()) %>%
            dplyr::mutate(
              fit = purrr::map(
                data, ~ minpack.lm::nlsLM(
                  data = .,
                  y ~ gaussian_func(x, y0, xc, wG, A),
                  start =  list(
                    y0 = .$y[which.min(.$y)],
                    xc = wlgth.center,
                    wG = param1,
                    A = param2
                  ),
                  control = minpack.lm::nls.lm.control(maxiter = p),
                  lower = c(0, 0, 0, 0)
                )
              ),
              tidied = purrr::map(fit, broom::tidy),
              augmented = purrr::map(fit, broom::augment)
            )
          .name <- paste("fitpeak", round(wlgth.center, digits = 2), sep = "_")
          assign(x = .name, value = .fit)
        }
        if(line.pfl == "Voigt") {
          if (is.null(wL) == FALSE & is.null(wG) == FALSE & is.null(A) == FALSE) {
            param1 <- as.numeric(wL)
            param2 <- as.numeric(wG)
            param3 <- as.numeric(A)
          } else {
            stop("Please provide an initial guess value for the Voigt fitting paramters: wL, wG and A")
          }
          .fit <- df %>%
            tidyr::nest(data = tidyr::everything()) %>%
            dplyr::mutate(
              fit = purrr::map(
                data, ~ minpack.lm::nlsLM(
                  data = .,
                  y ~ voigt_func(x, y0, xc, wG, wL, A),
                  start =  list(
                    y0 = .$y[which.min(.$y)],
                    xc = wlgth.center,
                    wL = param1,
                    wG = param2,
                    A = param3
                  ),
                  control = minpack.lm::nls.lm.control(maxiter = p),
                  lower = c(0, 0, 0, 0, 0)
                )
              ),
              tidied = purrr::map(fit, broom::tidy),
              augmented = purrr::map(fit, broom::augment)
            )
          .name <- paste("fitpeak", round(wlgth.center, digits = 2), sep = "_")
          assign(x = .name, value = .fit)
        }
      }
    }
  } else {
    if (is.null(wlgth.min) == FALSE & is.null(wlgth.max) == TRUE) {
      wlgth.min <- as.numeric(wlgth.min)
      df <- data %>%
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
      df <- data %>%
        tidyr::pivot_longer(
          cols = !id,
          names_to = "x",
          values_to = "y"
        ) %>%
        purrr::modify_at("x", as.numeric) %>%
        dplyr::filter(x <= wlgth.max)
    }

    if (is.null(wlgth.min) == TRUE & is.null(wlgth.max) == TRUE) {
      df <- data %>%
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
      df <- data %>%
        tidyr::pivot_longer(
          cols = !id,
          names_to = "x",
          values_to = "y"
        ) %>%
        purrr::modify_at("x", as.numeric) %>%
        dplyr::filter(x >= wlgth.min & x <= wlgth.max)
    }

    for (i in 1:lgth.xc) {
      wlgth.center <- as.numeric(peaks[i])

      for (j in 1:lgth.pfl) {
        line.pfl <- as.character(profile[j])

        if (line.pfl == "Lorentzian") {
          if (is.null(wL) == FALSE & is.null(A) == FALSE) {
            param1 <- as.numeric(wL)
            param2 <- as.numeric(A)
          } else {
            stop("Please provide an initial guess value for the Lorentzian fitting paramters: wL and A")
          }
          .fit <- df %>%
            tidyr::nest(data = tidyr::everything()) %>%
            dplyr::mutate(
              fit = purrr::map(
                data, ~ minpack.lm::nlsLM(
                  data = .,
                  y ~ lorentzian_func(x, y0, xc, wL, A),
                  start =  list(
                    y0 = .$y[which.min(.$y)],
                    xc = wlgth.center,
                    wL = param1,
                    A = param2
                  ),
                  control = minpack.lm::nls.lm.control(maxiter = p),
                  lower = c(0, 0, 0, 0)
                )
              ),
              tidied = purrr::map(fit, broom::tidy),
              augmented = purrr::map(fit, broom::augment)
            )
          .name <- paste("fitpeak", round(wlgth.center, digits = 2), sep = "_")
          assign(x = .name, value = .fit)
        }
        if(line.pfl == "Gaussian") {
          if (is.null(wG) == FALSE & is.null(A) == FALSE) {
            param1 <- as.numeric(wG)
            param2 <- as.numeric(A)
          } else {
            stop("Please provide an initial guess value for the Gaussian fitting paramters: wG and A")
          }
          .fit <- df %>%
            tidyr::nest(data = tidyr::everything()) %>%
            dplyr::mutate(
              fit = purrr::map(
                data, ~ minpack.lm::nlsLM(
                  data = .,
                  y ~ gaussian_func(x, y0, xc, wG, A),
                  start =  list(
                    y0 = .$y[which.min(.$y)],
                    xc = wlgth.center,
                    wG = param1,
                    A = param2
                  ),
                  control = minpack.lm::nls.lm.control(maxiter = p),
                  lower = c(0, 0, 0, 0)
                )
              ),
              tidied = purrr::map(fit, broom::tidy),
              augmented = purrr::map(fit, broom::augment)
            )
          .name <- paste("fitpeak", round(wlgth.center, digits = 2), sep = "_")
          assign(x = .name, value = .fit)
        }
        if(line.pfl == "Voigt") {
          if (is.null(wL) == FALSE & is.null(wG) == FALSE & is.null(A) == FALSE) {
            param1 <- as.numeric(wL)
            param2 <- as.numeric(wG)
            param3 <- as.numeric(A)
          } else {
            stop("Please provide an initial guess value for the Voigt fitting paramters: wL, wG and A")
          }
          .fit <- df %>%
            tidyr::nest(data = tidyr::everything()) %>%
            dplyr::mutate(
              fit = purrr::map(
                data, ~ minpack.lm::nlsLM(
                  data = .,
                  y ~ voigt_func(x, y0, xc, wG, wL, A),
                  start =  list(
                    y0 = .$y[which.min(.$y)],
                    xc = wlgth.center,
                    wL = param1,
                    wG = param2,
                    A = param3
                  ),
                  control = minpack.lm::nls.lm.control(maxiter = p),
                  lower = c(0, 0, 0, 0, 0)
                )
              ),
              tidied = purrr::map(fit, broom::tidy),
              augmented = purrr::map(fit, broom::augment)
            )
          .name <- paste("fitpeak", round(wlgth.center, digits = 2), sep = "_")
          assign(x = .name, value = .fit)
        }
      }
    }
  }
}
