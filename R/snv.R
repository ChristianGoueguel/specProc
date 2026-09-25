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
#' @param drop.na A logical value indicating whether to remove spectra (rows)
#' containing missing values. If `TRUE` (the default), such rows are removed.
#'
#' @return A list with the following components:
#'   \item{`correction`}{The SNV-scaled data.}
#'   \item{`means`}{A vector of row means.}
#'   \item{`stds`}{A vector of row standard deviations.}
#'
#' @export snv
#'
#' @examples
#' x <- rbind(c(1, 2, 3, 4), c(10, 20, 30, 40))
#' snv(x)$correction
#'
snv <- function(x, drop.na = TRUE) {

  if (missing(x)) {
    stop("Missing 'x' argument.")
  }
  if (!is.logical(drop.na)) {
    stop("'drop.na' must be a logical value (TRUE or FALSE).")
  }
  x <- tryCatch(as_numeric_matrix(x, "x"), error = function(e) {
    stop("Input 'x' must be a numeric matrix or data frame.", call. = FALSE)
  })
  if (drop.na) {
    x <- x[stats::complete.cases(x), , drop = FALSE]
  }

  mns <- rowMeans(x, na.rm = drop.na)
  sds <- apply(x, 1, stats::sd, na.rm = drop.na)
  if (any(sds == 0, na.rm = TRUE)) {
    warning("Some spectra are constant (zero standard deviation); they are only centered.")
    sds[sds == 0] <- 1
  }
  x_snv <- sweep(x, 1, mns, "-") / sds

  res <- list(
    "correction" = as_tbl(x_snv, colnames(x)),
    "means" = mns,
    "stds" = sds
    )

  return(res)
}
