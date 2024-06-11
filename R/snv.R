#' @title Standard Normal Variate
#'
#' @author Christian L. Goueguel
#'
#' @description
#' This function performs Standard Normal Variate (SNV) scaling on the input
#' spectral data. SNV scaling scales each row of the input data to have zero
#' mean and unit standard deviation. This is equivalent to autoscaling the
#' transpose of the input data.
#'
#' @param x A numeric matrix or data frame.
#' @param drop.na A logical value indicating whether to remove missing values
#' (NA) from the calculations. If `TRUE` (the default), missing values will be
#' removed. If `FALSE`, missing values will be included.
#'
#' @return A list with the following components:
#'   \item{`correction`}{The SNV-scaled data.}
#'   \item{`means`}{A vector of row means.}
#'   \item{`stds`}{A vector of row standard deviations.}
#'
#' @export snv
#'
snv <- function(x, drop.na = TRUE) {

  if (missing(x)) {
    stop("Missing 'x' argument.")
  }
  if (!all(x %>% purrr::map_lgl(is.numeric))) {
    stop("Input 'x' must be a numeric matrix or data frame.")
  }
  if (!is.logical(drop.na)) {
    stop("'drop.na' must be a logical value (TRUE or FALSE).")
  }

  if (is.data.frame(x) || tibble::is_tibble(x)) {
    x <- as.matrix(x)
  }
  if (drop.na) {
    x <- stats::na.omit(x)
  }

  mns <- rowMeans(x, na.rm = drop.na)
  sds <- apply(x, 1, stats::sd, na.rm = drop.na)
  x_snv <- sweep(x, 1, mns, "-") / sds

  res <- list(
    "correction" = tibble::as_tibble(x_snv),
    "means" = mns,
    "stds" = sds
    )

  return(res)
}
