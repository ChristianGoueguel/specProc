#' @title Poisson Scaling
#'
#' @author Christian L. Goueguel
#'
#' @description
#' This function performs Poisson scaling on a numeric matrix or data frame.
#' Poisson scaling scales each variable by its square root mean value,
#' with an optional scaling offset to avoid over-scaling when a variable has a
#' near-zero mean.
#'
#' @param x A numeric matrix or data frame.
#' @param sc A vector of previously calculated scales. If provided, these scales
#' will be applied to the data.
#' @param drop.na A logical value indicating whether to remove missing values
#' (NA) from the calculations. If `TRUE` (the default), missing values will be
#' removed. If `FALSE`, missing values will be included.
#' @param options A list of options for Poisson scaling. See the 'Options' section below.
#'
#' @return If `sc` is not provided, the function returns a list with the following components:
#'   \item{`xs`}{The Poisson-scaled data.}
#'   \item{`sc`}{A vector of scales calculated for the given data.}
#'
#'   If `sc` is provided, the function returns the Poisson-scaled data `xs`.
#'
#' @section Options:
#' The `options` argument is a list with the following fields:
#'   \itemize{
#'     \item `offset`: A numeric value representing the percentage of the maximum mean value to be used as an offset on all scales. Avoids division by near-zero means. Default is 3.
#'     \item `mode`: An integer value (1 or 2) specifying the dimension of the data on which to calculate the mean value for scaling. 1 = mean over rows (to scale variables); 2 = mean over columns (to scale samples). Default is 1.
#'   }
#'
#' @export poisson_scale
#'
poisson_scale <- function(x, sc = NULL, drop.na = TRUE, options = list()) {

  if (missing(x)) {
    stop("Missing 'x' argument.")
  }
  if (!is.numeric(x) && !is.data.frame(x) && !tibble::is_tibble(x)) {
    stop("'x' must be a numeric matrix or data frame.")
  }

  if (!is.null(sc) && (!is.numeric(sc) || length(sc) != ncol(x))) {
    stop("'sc' must be a numeric vector of length equal to the number of columns in 'x'.")
  }
  if (!is.logical(drop.na)) {
    stop("'drop.na' must be a logical value (TRUE or FALSE).")
  }

  if (!is.null(sc)) {
    # Apply previously calculated scales
    return(sweep(x, 2, sc, "/"))
  }

  # Set default options
  default_options <- list(offset = 3, mode = 1)
  options <- utils::modifyList(default_options, options)

  if (is.data.frame(x) || tibble::is_tibble(x)) {
    x <- as.matrix(x)
  }
  if (drop.na) {
    x <- stats::na.omit(x)
  }

  # Calculate mean values
  if (options$mode == 1) {
    means <- colMeans(x, na.rm = TRUE)
  } else if (options$mode == 2) {
    means <- rowMeans(x, na.rm = TRUE)
  } else {
    stop("Invalid 'mode' value. It must be either 1 or 2.")
  }

  # Calculate scaling offset
  max_mean <- max(means, na.rm = TRUE)
  offset <- max_mean * (options$offset / 100)

  # Calculate scales
  sc <- sqrt(means + offset)

  # Check for zero scales
  if (any(sc == 0)) {
    warning("Some variables have zero scale and will not be scaled.")
    sc[sc == 0] <- 1  # Set zero scales to 1 to avoid division by zero
  }

  # Perform Poisson scaling
  xs <- sweep(x, if (options$mode == 1) 2 else 1, sc, "/")

  return(list(xs = xs, sc = sc))
}
