#' @title Fast Average for Large Dataset
#'
#' @author Christian L. Goueguel
#'
#' @description
#' This function efficiently averages the spectra (or observations) in a large
#' dataset. It provides flexibility by computing either the overall
#' mean across all spectra or group-wise means based on the values of a
#' specified grouping column.
#'
#' @details
#' The function leverages the power of `Rcpp` to perform the mean calculations
#' in C++. The underlying C++ implementation has a time complexity of
#' \emph{O(n × m)}, where \emph{n} is the number of rows and \emph{m} is
#' the number of columns in the data. This efficient implementation ensures that
#' the function can handle large datasets without significant performance overhead.
#'
#' The R wrapper function adds minimal overhead for argument checking and data manipulation,
#' while leveraging the efficient C++ implementation for the core computations. The overall
#' time complexity remains \emph{O(n × m)}.
#'
#' @param data A data frame or tibble.
#' @param .group_by The column name to group the data by (optional).
#' If not provided, the average of the overall data will be computed.
#' @return
#'   - If `.group_by = NULL`, the function returns a numeric vector containing the
#' mean of each column across all spectra.
#'   - If `.group_by` is provided, the function returns a data frame with columns corresponding
#' to the unique values in the grouping column. Each cell contains the mean of the corresponding
#' column for that group.
#'
#' @export average
#'
average <- function(data, .group_by = NULL) {
  if (missing(data)) {
    stop("Missing 'data' argument.")
  }
  if (!is.data.frame(data) && !tibble::is_tibble(data)) {
    stop("'data' must be a data frame or tibble.")
  }
  if (!rlang::quo_is_null(rlang::enquo(.group_by)) &&
      !rlang::quo_name(rlang::enquo(.group_by)) %in% colnames(data)) {
    stop("Grouping variable '.group_by' not found in the data")
  }

  if (rlang::quo_is_null(rlang::enquo(.group_by))) {
    if (!all(data %>% purrr::map_lgl(is.numeric))) {
      stop("The input 'data' must be numeric")
    } else {
      Xmat <- data %>% as.matrix()
      avg <- computeMeans(Xmat)
      colnames(avg) <- names(data)
    }
  } else {
    Xmat <- data %>% dplyr::select(-{{ .group_by }}) %>% as.matrix()
    grp_vec <- data[[rlang::ensym(.group_by)]]

    if (!is.factor(grp_vec)) {
      grp_vec <- forcats::as_factor(grp_vec)
    }

    data_name <- data %>% dplyr::select(-{{ .group_by }}) %>% names()

    avg <- computeGroupedMeans(Xmat, grp_vec)
    colnames(avg) <- data_name
  }

  return(tibble::as_tibble(avg))
}

