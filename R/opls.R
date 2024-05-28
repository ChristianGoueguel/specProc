#' @title Orthogonal Projections to Latent Structures
#'
#' @description
#'  This function fits an Orthogonal Projections to Latent Structures (OPLS)
#'  model to the provided X (predictor) and Y (response) data.
#'
#' @details
#'  OPLS is a supervised modeling technique used to find the
#'  multidimensional direction in the X-space that explains the maximum
#'  multidimensional variance in the Y-space. It separates the systematic
#'  variation in X into two parts: one that is linearly related to Y
#'  (predictive components) and one that is statistically uncorrelated to the
#'  response variable Y (orthogonal components).
#'
#' @references
#'  - Trygg, J., and Wold, S., (2002). Orthogonal projections to latent structures (O-PLS).
#'    Journal of Chemometrics, 16(3):119-128.
#'
#' @author Christian L. Goueguel
#' @param X A data.frame or tibble containing the X-data (predictors).
#' @param Y A data.frame or tibble containing the Y-data (responses).
#' @param scale A character string indicating the scaling method for the data
#' ("center" or "pareto").
#' @param crossval An integer representing the number of cross-validation groups.
#' @param permutation An integer representing the number of permutations for
#' the permutation test.
#' @return A list containing the following components:
#' \describe{
#'   \item{x_scores}{A matrix of X-scores (the projections of the X-data onto
#'   the predictive components).}
#'   \item{x_loadings}{A matrix of X-loadings (the weights of the original
#'   X-variables on the predictive components).}
#'   \item{x_weights}{A matrix of X-weights (the weights used to calculate
#'   the X-scores).}
#'   \item{orthoScores}{A matrix of orthogonal scores (the projections of
#'   the X-data onto the orthogonal components).}
#'   \item{orthoLoadings}{A matrix of orthogonal loadings (the weights of
#'   the original X-variables on the orthogonal components).}
#'   \item{orthoWeights}{A matrix of orthogonal weights (the weights used
#'   to calculate the orthogonal scores).}
#'   \item{y_weights}{A matrix of Y-weights (the weights used to calculate
#'   the Y-scores).}
#'   \item{y_orthoWeights}{A matrix of orthogonal Y-weights (the weights
#'   used to calculate the orthogonal Y-scores).}
#'   \item{y_scores}{A matrix of Y-scores (the projections of the Y-data
#'   onto the predictive components).}
#' }
#' @export opls
#' @examples
#' \dontrun{
#' opls_result <- opls(
#' x_data, y_data,
#' scale = "center",
#' crossval = 7,
#' permutation = 20
#' )
#' }
opls <- function(X, Y, scale = "center", crossval = 7, permutation = 20) {
  if (is.null(X) == TRUE) {
    stop("X-data must be provided")
  }
  if (is.null(Y) == TRUE) {
    stop("Y-data must be provided")
  }
  if (is.data.frame(X) == FALSE & tibble::is_tibble(X) == FALSE) {
    stop("X-data must be of class data.frame, tbl_df, or tbl")
  }
  if (is.data.frame(Y) == FALSE & tibble::is_tibble(Y) == FALSE) {
    stop("Y-data must be of class data.frame, tbl_df, or tbl")
  }

  X <- as.matrix(X)
  Y <- as.matrix(Y)

  modout <- ropls::opls(
    X,
    Y,
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
