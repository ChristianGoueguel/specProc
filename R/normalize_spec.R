#' @title Normalize Spectra with Different Normalization Methods
#' @author Christian L. Goueguel
#' @description This function takes a data frame (spectra) as input and normalizes it using one or more specified normalization methods.
#' @param .data A numeric data frame containing the spectra.
#' @param method A character vector specifying the normalization method(s) to apply. Available methods are "min_max", "mean", "vector", "area", "max_intensity", "msc", and "snv". Default is "min_max".
#' @return A data frame containing the original columns and the normalized columns for each specified method with the method name as a prefix.
#' @export normalize_spec
normalize_spec <- function(.data, method = "min_max") {
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("purrr", quietly = TRUE)

  if (missing(.data)) {
    stop("Missing 'data' argument.")
  }
  if (!all(.data %>% purrr::map_lgl(is.numeric))) {
    stop("Input must be a numeric data frame.")
  }

  res <- .data %>%
    mutate(across(everything(), as.double)) %>%
    mutate(
      across(
        everything(),
        list(
          min_max = ~ ( . - min(.)) / (max(.) - min(.)),
          mean = ~ (. - mean(.)) / (max(.) - min(.)),
          vector = ~ . / sqrt(sum(.^2)),
          area = ~ . / sum(.),
          max_intensity = ~ . / max(.),
          msc = {
            H <- qr.solve(t(.data)) %*% .data;
            ~ . / H[1, ] - H[2, ]
          },
          snv = ~ (. - mean(.)) / sd(.)
        ),
        .names = paste0(method, "_", .col)
      )
    )

  return(res)
}
