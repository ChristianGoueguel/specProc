peakfit <- function(data, profile = "Voigt", wlgth.min = NULL, wlgth.max = NULL) {

  if (length(data) == 0 | is.null(data) == TRUE) {
    stop("Apparently you forgot to provide the spectra.")
  }

  if (is.data.frame(data) == FALSE) {
    stop("Data must be of class tbl_df, tbl or data.frame")
  }

  if (is.null(wlgth.min) | is.null(wlgth.max)) {
    X <- data %>%
      tidyr::pivot_longer(
        cols = tidyr::everything(),
        names_to = "x",
        values_to = "y"
      ) %>%
      purrr::modify_at("x", as.numeric)
  } else {
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
