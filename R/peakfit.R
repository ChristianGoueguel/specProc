#' @title Spectral Fitting
#' @author Christian L. Goueguel
#' @description Fitting of spectroscopic peak by line shape functions with variable parameters.
#' @details The function uses `minpack.lm::nlsLM`, which is based on the Levenberg-Marquardt algorithm for searching the minimum value of the square of the sum of the residuals.
#' @param data Data frame of emission spectra
#' @param profile Line shape function: "Lorentzian", "Gaussian" or "Voigt"
#' @param wL Lorentzian full width at half maximum (initial guess)
#' @param wG Gaussian full width at half maximum (initial guess)
#' @param A Peak area (initial guess)
#' @param wlgth.min Lower bound of the wavelength subset
#' @param wlgth.max Upper bound of the wavelength subset
#' @param id spectra identification (optional)
#' @return Fitted value and the estimated parameters along with the corresponding errors
#' @export peakfit
peakfit <- function(data, profile = "Voigt", wL = NULL, wG = NULL, A = NULL, wlgth.min = NULL, wlgth.max = NULL, id = NULL) {

  if (length(data) == 0 & is.null(data) == TRUE) {
    stop("Apparently you forgot to provide the spectra.")
  }

  if (is.data.frame(data) == FALSE & tibble::is.tibble(data) == FALSE) {
    stop("Data must be of class tbl_df, tbl or data.frame")
  }

  if (profile != "Lorentzian" & profile != "Gaussian" & profile != "Voigt") {
    stop("The profile function must be Lorentzian, Gaussian or Voigt")
  }

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
      }
      df <- data %>%
        tidyr::pivot_longer(
          cols = tidyr::everything(),
          names_to = "x",
          values_to = "y"
        ) %>%
        purrr::modify_at("x", as.numeric) %>%
        dplyr::filter(x >= wlgth.min & x <= wlgth.max)
    }

    if (profile == "Lorentzian") {
      if (is.null(wL) == FALSE & is.null(A) == FALSE) {
        param1 <- as.numeric(wL)
        param2 <- as.numeric(A)
      } else {
        stop("Please provide an initial guess value for the Lorentzian fitting paramters: wL and A")
      }
      fitpeak <- df %>%
        tidyr::nest(data = tidyr::everything()) %>%
        dplyr::mutate(
          fit = purrr::map(
            data, ~ minpack.lm::nlsLM(
              data = .,
              y ~ lorentzian_func(x, y0, xc, wL, A),
              start =  list(
                y0 = .$y[which.min(.$y)],
                xc = .$x[which.max(.$y)],
                wL = param1,
                A = param2
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
      if (is.null(wG) == FALSE & is.null(A) == FALSE) {
        param1 <- as.numeric(wG)
        param2 <- as.numeric(A)
      } else {
        stop("Please provide an initial guess value for the Gaussian fitting paramters: wG and A")
      }
      fitpeak <- df %>%
        tidyr::nest(data = tidyr::everything()) %>%
        dplyr::mutate(
          fit = purrr::map(
            data, ~ minpack.lm::nlsLM(
              data = .,
              y ~ gaussian_func(x, y0, xc, wG, A),
              start =  list(
                y0 = .$y[which.min(.$y)],
                xc = .$x[which.max(.$y)],
                wG = param1,
                A = param2
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

    if(profile == "Voigt") {
      if (is.null(wL) == FALSE & is.null(wG) == FALSE & is.null(A) == FALSE) {
        param1 <- as.numeric(wL)
        param2 <- as.numeric(wG)
        param3 <- as.numeric(A)
      } else {
        stop("Please provide an initial guess value for the Voigt fitting paramters: wL, wG and A")
      }
      fitpeak <- df %>%
        tidyr::nest(data = tidyr::everything()) %>%
        dplyr::mutate(
          fit = purrr::map(
            data, ~ minpack.lm::nlsLM(
              data = .,
              y ~ voigt_func(x, y0, xc, wG, wL, A),
              start =  list(
                y0 = .$y[which.min(.$y)],
                xc = .$x[which.max(.$y)],
                wL = param1,
                wG = param2,
                A = param3
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

    if (profile == "Lorentzian") {
      if (is.null(wL) == FALSE & is.null(A) == FALSE) {
        param1 <- as.numeric(wL)
        param2 <- as.numeric(A)
      } else {
        stop("Please provide an initial guess value for the Lorentzian fitting paramters: wL and A")
      }
      fitpeak <- df %>%
        tidyr::nest(data = -id) %>%
        dplyr::mutate(
          fit = purrr::map(
            data, ~ minpack.lm::nlsLM(
              data = .,
              y ~ lorentzian_func(x, y0, xc, wL, A),
              start =  list(
                y0 = .$y[which.min(.$y)],
                xc = .$x[which.max(.$y)],
                wL = param1,
                A = param2
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
      if (is.null(wG) == FALSE & is.null(A) == FALSE) {
        param1 <- as.numeric(wG)
        param2 <- as.numeric(A)
      } else {
        stop("Please provide an initial guess value for the Gaussian fitting paramters: wG and A")
      }
      fitpeak <- df %>%
        tidyr::nest(data = -id) %>%
        dplyr::mutate(
          fit = purrr::map(
            data, ~ minpack.lm::nlsLM(
              data = .,
              y ~ gaussian_func(x, y0, xc, wG, A),
              start =  list(
                y0 = .$y[which.min(.$y)],
                xc = .$x[which.max(.$y)],
                wG = param1,
                A = param2
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

    if(profile == "Voigt") {
      if (is.null(wL) == FALSE & is.null(wG) == FALSE & is.null(A) == FALSE) {
        param1 <- as.numeric(wL)
        param2 <- as.numeric(wG)
        param3 <- as.numeric(A)
      } else {
        stop("Please provide an initial guess value for the Voigt fitting paramters: wL, wG and A")
      }
      fitpeak <- df %>%
        tidyr::nest(data = -id) %>%
        dplyr::mutate(
          fit = purrr::map(
            data, ~ minpack.lm::nlsLM(
              data = .,
              y ~ voigt_func(x, y0, xc, wG, wL, A),
              start =  list(
                y0 = .$y[which.min(.$y)],
                xc = .$x[which.max(.$y)],
                wL = param1,
                wG = param2,
                A = param3
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
}
