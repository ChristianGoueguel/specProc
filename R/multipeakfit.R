


multipeakfit <- function(peaks.center, profile, wlgth.min = NULL, wlgth.max = NULL) {

  if (length(data) == 0 & is.null(data) == TRUE) {
    stop("Apparently you forgot to provide the spectra.")
  }

  if (is.data.frame(data) == FALSE & tibble::is.tibble(data) == FALSE) {
    stop("Data must be of class tbl_df, tbl or data.frame")
  }

  if (profile != "Lorentzian" & profile != "Gaussian" & profile != "Voigt") {
    stop("The profile function must be Lorentzian, Gaussian or Voigt")
  }

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
    peakfit(data = df, profile = "Lorentzian", wL = NULL, A = NULL, wlgth.min = NULL, wlgth.max = NULL)
  }

  if(profile == "Gaussian") {
    peakfit(data = df, profile = "Gaussian", wG = NULL, A = NULL, wlgth.min = NULL, wlgth.max = NULL)
  }

  if(profile == "Voigt") {
    peakfit(data = df, profile = "Voigt", wL = NULL, wG = NULL, A = NULL, wlgth.min = NULL, wlgth.max = NULL)
  }






}
