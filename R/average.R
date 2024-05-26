#' @title Fast Column-wise Averaging for Large Datasets
#'
#' @description
#' This function calculates the column-wise average values of numeric
#' columns in a data frame or tibble. It provides flexibility by allowing users
#' to compute either the overall column means or group-wise column means based
#' on the values of a specified grouping column.
#'
#' @details
#' The function uses the package `Rcpp` to perform the mean calculations efficiently in C++.
#' The underlying C++ function has a time complexity of *O(n × m)*, where *n* is the number
#' of rows and *m* is the number of columns in the data. This efficient
#' implementation ensures that the function can handle reasonably large datasets
#' without significant performance overhead. While the R wrapper function adds
#' some minimal overhead for argument checking and data manipulation,
#' the overall time complexity remains *O(n × m)*, ensuring scalability and
#' efficient computation even for large datasets.
#'
#' @author Christian L. Goueguel
#' @param .data A data frame or tibble.
#' @param .group_by The column name to group the data by (optional).
#' If not provided, the average of the overall data will be computed.
#' @return A tibble of the computed averages, with one row for each group
#' (if grouped) or a single row with the overall column averages (if not grouped).
#' @export average
average <- function(.data, .group_by = NULL) {
  if (missing(.data)) {
    stop("Missing '.data' argument.")
  }
  if (!is.data.frame(.data) && !tibble::is_tibble(.data)) {
    stop("'.data' must be a data frame or tibble.")
  }
  if (!rlang::quo_is_null(rlang::enquo(.group_by)) &&
      !rlang::quo_name(rlang::enquo(.group_by)) %in% colnames(.data)) {
    stop("Grouping variable '.group_by' not found in the data")
  }

  if (rlang::quo_is_null(rlang::enquo(.group_by))) {
    if (!all(.data %>% purrr::map_lgl(is.numeric))) {
      stop("The input '.data' must be numeric")
    } else {
      Xmat <- .data %>% as.matrix()
      Rcpp::sourceCpp("computeMeans.cpp")
      avg <- computeMeans(Xmat)
    }
  } else {
    Xmat <- .data %>%
      dplyr::select(-{{ .group_by }}) %>%
      as.matrix()
    grp_vec <- .data[[rlang::ensym(.group_by)]]
    if (!is.factor(grp_vec)) {
      grp_vec <- forcats::as_factor(grp_vec)
    }
    Rcpp::sourceCpp("computeGroupedMeans.cpp")
    avg <- computeGroupedMeans(grp_vec, Xmat)
  }
  return(tibble::as_tibble(avg))
}

