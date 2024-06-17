#' @title Orthogonal Projections to Latent Structures
#'
#' @author Christian L. Goueguel
#'
#' @description
#'  This function fits an Orthogonal Projections to Latent Structures (OPLS)
#'  model to the provided x (predictor) and y (response) data.
#'
#' @details
#'  OPLS is a supervised modeling technique used to find the
#'  multidimensional direction in the x-space that explains the maximum
#'  multidimensional variance in the y-space. It separates the systematic
#'  variation in x into two parts: one that is linearly related to y
#'  (predictive components) and one that is statistically uncorrelated to the
#'  response variable y (orthogonal components).
#'
#' @references
#'  - Trygg, J., and Wold, S., (2002).
#'    Orthogonal projections to latent structures (O-PLS).
#'    Journal of Chemometrics, 16(3):119-128.
#'
#' @param x A data.frame or tibble containing the x-data (predictors).
#' @param y A data.frame or tibble containing the y-data (responses).
#' @param scale A character string indicating the scaling method for the data ("center" or "pareto").
#' @param crossval An integer representing the number of cross-validation groups.
#' @param permutation An integer representing the number of permutations for the permutation test.
#'
#' @return A list containing the following components:
#' \describe{
#'   \item{x_scores}{A matrix of x-scores (the projections of the x-data onto the predictive components).}
#'   \item{x_loadings}{A matrix of x-loadings (the weights of the original x-variables on the predictive components).}
#'   \item{x_weights}{A matrix of x-weights (the weights used to calculate the x-scores).}
#'   \item{orthoScores}{A matrix of orthogonal scores (the projections of the x-data onto the orthogonal components).}
#'   \item{orthoLoadings}{A matrix of orthogonal loadings (the weights of the original x-variables on the orthogonal components).}
#'   \item{orthoWeights}{A matrix of orthogonal weights (the weights used to calculate the orthogonal scores).}
#'   \item{y_weights}{A matrix of y-weights (the weights used to calculate the y-scores).}
#'   \item{y_orthoWeights}{A matrix of orthogonal y-weights (the weights used to calculate the orthogonal y-scores).}
#'   \item{y_scores}{A matrix of y-scores (the projections of the y-data onto the predictive components).}
#' }
#'
#' @export opls
#'
opls <- function(x, y, scale = "center", crossval = 7, permutation = 20) {
  if (is.null(x) == TRUE) {
    stop("x-data must be provided")
  }
  if (is.null(y) == TRUE) {
    stop("y-data must be provided")
  }
  if (is.data.frame(x) == FALSE & tibble::is_tibble(x) == FALSE) {
    stop("x-data must be of class data.frame, tbl_df, or tbl")
  }
  if (is.data.frame(y) == FALSE & tibble::is_tibble(y) == FALSE) {
    stop("y-data must be of class data.frame, tbl_df, or tbl")
  }

  x <- as.matrix(x)
  y <- as.matrix(y)

  modout <- ropls::opls(
    x,
    y,
    predI = 1,
    orthoI = NA,
    algoC = "nipals",
    crossvalI = crossval,
    log10L = FALSE,
    permI = permutation,
    scaleC = scale,
    subset = NULL
  )

  res <- list(
    x_scores = modout@scoreMN |> tibble::as_tibble(),
    x_loadings = modout@loadingMN |> tibble::as_tibble(),
    x_weights = modout@weightMN |> tibble::as_tibble(),
    orthoScores = modout@orthoScoreMN |> tibble::as_tibble(),
    orthoLoadings = modout@orthoLoadingMN |> tibble::as_tibble(),
    orthoWeights = modout@orthoWeightMN |> tibble::as_tibble(),
    y_weights = modout@cMN |> tibble::as_tibble(),
    y_orthoWeights = modout@uMN |> tibble::as_tibble(),
    y_scores = modout@weightStarMN |> tibble::as_tibble()
  )
  return(res)
}
