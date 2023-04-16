#' @title Orthogonal Projections to Latent Structures (OPLS) Function
#' @description This function fits an OPLS model to provided x and y data.
#' @author Christian L. Goueguel
#' @param x A data.frame or tibble containing the X-data (predictors).
#' @param y A data.frame or tibble containing the Y-data (responses).
#' @param scale A character string indicating the scaling method for the data ("center" or "pareto").
#' @param crossval An integer representing the number of cross-validation groups.
#' @param permutation An integer representing the number of permutations for the permutation test.
#' @return A list containing matrices of x-scores, x-loadings, x-weights, orthoScores, orthoLoadings, orthoWeights, y-weights, y-orthoWeights, and y-scores.
#' @export opls
#' @examples
#' \dontrun{
#' opls_result <- opls(x_data, y_data, scale = "center", crossval = 7, permutation = 20)
#' }
opls <- function(x, y, scale = "center", crossval = 7, permutation = 20) {
  requireNamespace("ropls", quietly = TRUE)
  requireNamespace("tibble", quietly = TRUE)

  if (is.null(x) == TRUE) {
    stop("X-data must be provided")
  }
  if (is.null(y) == TRUE) {
    stop("Y-data must be provided")
  }
  if (is.data.frame(x) == FALSE & tibble::is_tibble(x) == FALSE) {
    stop("X-data must be of class data.frame, tbl_df, or tbl")
  }
  if (is.data.frame(y) == FALSE & tibble::is_tibble(y) == FALSE) {
    stop("Y-data must be of class data.frame, tbl_df, or tbl")
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
    x.scores = modout@scoreMN |> tibble::as_tibble(),
    x.loadings = modout@loadingMN |> tibble::as_tibble(),
    x.weights = modout@weightMN |> tibble::as_tibble(),
    orthoScores = modout@orthoScoreMN |> tibble::as_tibble(),
    orthoLoadings = modout@orthoLoadingMN |> tibble::as_tibble(),
    orthoWeights = modout@orthoWeightMN |> tibble::as_tibble(),
    y.weights = modout@cMN |> tibble::as_tibble(),
    y.orthoWeights = modout@uMN |> tibble::as_tibble(),
    y.scores = modout@weightStarMN |> tibble::as_tibble()
  )
  return(res)
}
