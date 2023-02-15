#' @title Robust and Non-Robust Descriptive Statistics
#' @description Function used to produce summary statistics.
#' @param data Data frame or tibble. Columns are the numeric variables.
#' @param var Numeric. Selected variable(s).
#' @return Tibble. Summary statistic that quantitatively describes the variable(s).
#' @export summaryStat
summaryStat <- function(data, var = NULL) {
  data %>%
    select(all_of(var)) %>%
    pivot_longer(
      cols = everything(),
      names_to = "variable",
      values_to = "value"
    ) %>%
    drop_na(value) %>%
    group_by(variable) %>%
    summarise(
      mean = round(mean(value), 2),
      median = round(median(value), 2),
      mad = round(mad(value), 2),
      sd = round(sd(value), 3),
      cv = round((sd/mean)*100, 2),
      rcv = round((1.4826*(mad/median))*100, 2),
      IQR = round(IQR(value), 2),
      min = min(value),
      max = max(value),
      skewness = round(moments::skewness(concentration), 2),
      medcouple = round(robustbase::mc(concentration), 2),
      kurtosis = round(moments::kurtosis(concentration), 2),
      n = n()
    )
}



