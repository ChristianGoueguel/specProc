# Load required packages
install.packages("tidyverse")
install.packages("corrr")
library(tidyverse)
library(corrr)

# Function to compute correlation with respect to a response variable
corr <- function(.data, response_var, method = "pearson") {
  if (!is.data.frame(.data) || !all(.data %>% map_lgl(is.numeric))) {
    stop("Input must be a numeric data frame.")
  }

  if (!response_var %in% colnames(.data)) {
    stop("Response variable not found in the data frame.")
  }

  valid_methods <- c("pearson", "kendall", "spearman")
  if (!method %in% valid_methods) {
    stop("Invalid method specified. Available methods are: pearson, kendall, and spearman.")
  }

  correlation <- df %>%
    correlate(method = ifelse(method == "kendall", "kendall", "pearson")) %>%
    rownames_to_column("variable") %>%
    select(variable, !!response_var) %>%
    rename(correlation = !!response_var) %>%
    mutate(method = method)

  if (method == "spearman") {
    correlation <- correlation %>%
      mutate(correlation = as.numeric(correlation)) %>%
      mutate(correlation = 2 * asin(correlation / 2)) %>%
      mutate(correlation = round(correlation, 2))
  }

  correlation <- .data %>%
    correlate(method = method) %>%
    rownames_to_column("variable") %>%
    select(variable, !!response_var) %>%
    rename(correlation = !!response_var) %>%
    mutate(method = method)

  return(correlation)
}

# Example usage
data(iris)
numeric_iris <- iris %>% dplyr::select(-Species)
correlation_results <- compute_correlation(numeric_iris, response_var = "Sepal.Length", method = "pearson")
print(correlation_results)
