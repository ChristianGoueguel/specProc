#' @title Robust and Non-Robust Descriptive Statistics
#' @description Function used to produce summary statistics.
#' @param data frame or tibble. Columns are the numeric variables.
#' @param var Numeric. Selected variable(s).
#' @return Tibble. Summary statistic that quantitatively describes the variable(s).
#' @export summaryStat
summaryStat <- function(data, var = NULL, remove.na = TRUE, sign.fig = 2) {
  requireNamespace("ggplot2", quietly = TRUE)
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("tidyr", quietly = TRUE)
  requireNamespace("tibble", quietly = TRUE)
  requireNamespace("moments", quietly = TRUE)
  requireNamespace("robustbase", quietly = TRUE)

  if (is.null(data) == TRUE) {
    stop("Data must be provided")
  }
  if (is.data.frame(data) == FALSE & tibble::is_tibble(data) == FALSE) {
    stop("Data must be of class data.frame, tbl_df, or tbl")
  }

  fct_summary <- function(x) {
    x %>%
      dplyr::group_by(variable) %>%
      dplyr::summarise(
        mean = round(mean(value), sign.fig),
        median = round(median(value), sign.fig),
        mad = round(mad(value), sign.fig),
        sd = round(sd(value), sign.fig),
        cv = round((sd/mean)*100, sign.fig),
        rcv = round((1.4826*(mad/median))*100, sign.fig),
        IQR = round(IQR(value, na.rm = TRUE), sign.fig),
        min = min(value),
        max = max(value),
        skewness = round(moments::skewness(value), sign.fig),
        medcouple = round(robustbase::mc(value), sign.fig),
        kurtosis = round(moments::kurtosis(value), sign.fig),
        n = n()
      )
  }

  if (is.null(var) == FALSE) {
    if (remove.na == TRUE) {
      data %>%
        dplyr::select(dplyr::where(is.numeric)) %>%
        tidyr::pivot_longer(
          cols = dplyr::all_of(var),
          names_to = "variable",
          values_to = "value"
          ) %>%
        tidyr::drop_na(value) %>%
        fct_summary()
    } else {
      data %>%
        select(where(is.numeric)) %>%
        pivot_longer(
          cols = all_of(var),
          names_to = "variable",
          values_to = "value"
        ) %>%
        fct_summary()
    }
  } else {
    if (remove.na == TRUE) {
      data %>%
        dplyr::select(dplyr::where(is.numeric)) %>%
        tidyr::pivot_longer(
          cols = dplyr::everything(),
          names_to = "variable",
          values_to = "value"
        ) %>%
        tidyr::drop_na(value) %>%
        fct_summary()
    } else {
      data %>%
        dplyr::select(dplyr::where(is.numeric)) %>%
        tidyr::pivot_longer(
          cols = dplyr::everything(),
          names_to = "variable",
          values_to = "value"
        ) %>%
        fct_summary()
    }
  }
}
