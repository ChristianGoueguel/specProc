#' @title Peak Fitting
#' @author Christian L. Goueguel
#' @description Fitting of a single spectral line by lineshape functions with variable parameters.
#' @details The function uses `minpack.lm::nlsLM`, which is based on the Levenberg-Marquardt algorithm for searching the minimum value of the square of the sum of the residuals.
#' @param .data Data frame of emission spectra
#' @param profile (character) Lineshape function: "lorentzian", "gaussian" or "voigt"
#' @param wL (numeric) Lorentzian full width at half maximum (initial guess)
#' @param wG (numeric) Gaussian full width at half maximum (initial guess)
#' @param A (numeric) Peak area (initial guess)
#' @param wlgth.min (numeric) Lower bound of the wavelength subset
#' @param wlgth.max (numeric) Upper bound of the wavelength subset
#' @param id Spectra name (optional)
#' @param max.iter (numeric) Maximum number of iteration (200 by default)
#' @return Fitted value and the estimated parameters along with the corresponding errors
#' @export peakfit
peakfit <- function(.data, profile = "voigt", wL = NULL, wG = NULL, A = NULL, wlgth.min = NULL, wlgth.max = NULL, id = NULL, max.iter = 200) {
  if (missing(.data)) {
    stop("Missing 'data' argument.")
  }
  if (!is.data.frame(.data) && !tibble::is_tibble(.data)) {
    stop("Input 'data' must be a data frame or tibble.")
  }
  if (profile != "lorentzian" & profile != "gaussian" & profile != "voigt") {
    stop("The profile function must be: 'lorentzian', 'gaussian' or 'voigt'")
  }
  if (is.numeric(max.iter) == FALSE) {
    stop("Maximum number of iteration must be numeric")
  } else {
    p <- as.numeric(max.iter)
  }

  #rlang::check_installed("broom")
  x <- NULL
  fit <- NULL

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
    if (profile == "lorentzian") {
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
            .data, ~ minpack.lm::nlsLM(
              data = .,
              y ~ lorentzian(x, y0, xc, wL, A),
              start =  list(
                y0 = .$y[which.min(.$y)],
                xc = .$x[which.max(.$y)],
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
    }
    if(profile == "gaussian") {
      if (is.null(wG) == FALSE & is.null(A) == FALSE) {
        param1 <- 1
        param2 <- 15000
      } else {
        stop("Please provide an initial guess value for the Gaussian fitting paramters: wG and A")
      }
      .fit <- df %>%
        tidyr::nest(data = tidyr::everything()) %>%
        dplyr::mutate(
          fit = purrr::map(
            .data, ~ minpack.lm::nlsLM(
              data = .,
              y ~ gaussian(x, y0, xc, wG, A),
              start =  list(
                y0 = .$y[which.min(.$y)],
                xc = .$x[which.max(.$y)],
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
    }
    if(profile == "voigt") {
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
            .data, ~ minpack.lm::nlsLM(
              data = .,
              y ~ voigt(x, y0, xc, wG, wL, A),
              start =  list(
                y0 = .$y[which.min(.$y)],
                xc = .$x[which.max(.$y)],
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
    if (profile == "lorentzian") {
      if (is.null(wL) == FALSE & is.null(A) == FALSE) {
        param1 <- as.numeric(wL)
        param2 <- as.numeric(A)
      } else {
        stop("Please provide an initial guess value for the Lorentzian fitting paramters: wL and A")
      }
      .fit <- df %>%
        tidyr::nest(data = -id) %>%
        dplyr::mutate(
          fit = purrr::map(
            .data, ~ minpack.lm::nlsLM(
              data = .,
              y ~ lorentzian(x, y0, xc, wL, A),
              start =  list(
                y0 = .$y[which.min(.$y)],
                xc = .$x[which.max(.$y)],
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
    }
    if(profile == "gaussian") {
      if (is.null(wG) == FALSE & is.null(A) == FALSE) {
        param1 <- as.numeric(wG)
        param2 <- as.numeric(A)
      } else {
        stop("Please provide an initial guess value for the Gaussian fitting paramters: wG and A")
      }
      .fit <- df %>%
        tidyr::nest(data = -id) %>%
        dplyr::mutate(
          fit = purrr::map(
            .data, ~ minpack.lm::nlsLM(
              data = .,
              y ~ gaussian(x, y0, xc, wG, A),
              start =  list(
                y0 = .$y[which.min(.$y)],
                xc = .$x[which.max(.$y)],
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
    }
    if(profile == "voigt") {
      if (is.null(wL) == FALSE & is.null(wG) == FALSE & is.null(A) == FALSE) {
        param1 <- as.numeric(wL)
        param2 <- as.numeric(wG)
        param3 <- as.numeric(A)
      } else {
        stop("Please provide an initial guess value for the Voigt fitting paramters: wL, wG and A")
      }
      .fit <- df %>%
        tidyr::nest(data = -id) %>%
        dplyr::mutate(
          fit = purrr::map(
            .data, ~ minpack.lm::nlsLM(
              data = .,
              y ~ voigt(x, y0, xc, wG, wL, A),
              start =  list(
                y0 = .$y[which.min(.$y)],
                xc = .$x[which.max(.$y)],
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
    }
  }
  return(.fit)
}
