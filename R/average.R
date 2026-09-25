#' @title Fast Average for Large Spectral Dataset
#'
#' @author Christian L. Goueguel
#'
#' @description
#' This function efficiently averages the samples spectra in a large
#' dataset. It provides flexibility by computing either the overall
#' mean across all spectra or group-wise means based on the values of a
#' specified grouping column.
#'
#' @details
#' The function leverages the power of `Rcpp` to perform the mean calculations
#' in C++. The underlying C++ implementation has a time complexity of
#' \emph{O(n × m)}, where \emph{n} is the number of rows and \emph{m} is
#' the number of columns in the data. Missing values are ignored in the
#' computation of each mean.
#'
#' @param x A data frame or tibble.
#' @param .group_by The column to group the data by (optional), given either
#'   unquoted or as a string. If not provided, the average of the overall data
#'   will be computed.
#' @return
#'   - If `.group_by = NULL`, a one-row tibble containing the mean of each column.
#'   - If `.group_by` is provided, a tibble with one row per group: the first
#'   column holds the group labels and the remaining columns hold the group means.
#'
#' @export average
#'
#' @examples
#' spectra <- data.frame(
#'   sample = rep(c("a", "b"), each = 3),
#'   `200.1` = c(1, 2, 3, 10, 11, 12),
#'   `200.2` = c(2, 3, 4, 20, 21, 22),
#'   check.names = FALSE
#' )
#' average(spectra[, -1])
#' average(spectra, sample)
#'
average <- function(x, .group_by = NULL) {
  if (missing(x)) {
    stop("Missing 'x' argument.")
  }
  if (!is.data.frame(x)) {
    stop("'x' must be a data frame or tibble.")
  }

  group_quo <- rlang::enquo(.group_by)

  if (rlang::quo_is_null(group_quo)) {
    Xmat <- as_numeric_matrix(x, "x")
    avg <- computeMeans(Xmat)
    return(as_tbl(avg, names(x)))
  }

  group_name <- rlang::as_name(group_quo)
  if (!group_name %in% colnames(x)) {
    stop("Grouping variable '.group_by' not found in the data")
  }

  data_cols <- setdiff(names(x), group_name)
  Xmat <- as_numeric_matrix(x[data_cols], "x")
  grp <- x[[group_name]]
  if (!is.factor(grp)) {
    grp <- factor(grp, levels = unique(grp[!is.na(grp)]))
  }

  avg <- computeGroupedMeans(Xmat, as.integer(grp), nlevels(grp))
  out <- as_tbl(avg, data_cols)
  out <- tibble::add_column(out, !!group_name := levels(grp), .before = 1)
  return(out)
}
