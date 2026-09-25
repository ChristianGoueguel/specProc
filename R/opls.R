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
#'  The function is a wrapper around `ropls::opls()` from the Bioconductor
#'  package \pkg{ropls}, which must be installed
#'  (`BiocManager::install("ropls")`). The model is fitted silently (no
#'  printed summary or plots). For a dependency-free alternative that returns
#'  the OPLS-filtered data, see [projected_osc()] or [o2pls()].
#'
#' @references
#'  - Trygg, J., and Wold, S., (2002).
#'    Orthogonal projections to latent structures (O-PLS).
#'    Journal of Chemometrics, 16(3):119-128.
#'
#' @param x A data.frame or tibble containing the x-data (predictors).
#' @param y A data.frame or tibble containing the y-data (responses).
#' @param scale A character string indicating the scaling method for the data: "none", "center", "pareto" or "standard".
#' @param crossval An integer representing the number of cross-validation groups.
#' @param permutation An integer representing the number of permutations for the permutation test.
#' @param ncomp.ortho The number of orthogonal components. If `NA` (default), it is determined automatically by cross-validation.
#'
#' @return A list containing the following components:
#' \describe{
#'   \item{x_scores}{A matrix of x-scores (the projections of the x-data onto the predictive components).}
#'   \item{x_loadings}{A matrix of x-loadings (the weights of the original x-variables on the predictive components).}
#'   \item{x_weights}{A matrix of x-weights (the weights used to calculate the x-scores).}
#'   \item{orthoScores}{A matrix of orthogonal scores (the projections of the x-data onto the orthogonal components).}
#'   \item{orthoLoadings}{A matrix of orthogonal loadings (the weights of the original x-variables on the orthogonal components).}
#'   \item{orthoWeights}{A matrix of orthogonal weights (the weights used to calculate the orthogonal scores).}
#'   \item{y_weights}{A matrix of y-weights.}
#'   \item{y_scores}{A matrix of y-scores (the projections of the y-data onto the predictive components).}
#'   \item{summary}{The model summary (R2X, R2Y, Q2, ...).}
#'   \item{model}{The fitted `ropls` model object.}
#' }
#'
#' @export opls
#'
opls <- function(x, y, scale = "center", crossval = 7, permutation = 20, ncomp.ortho = NA) {
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
  scale <- match.arg(scale, c("none", "center", "pareto", "standard"))
  rlang::check_installed("ropls", reason = "to fit OPLS models (install it with BiocManager::install(\"ropls\")).")

  x <- as_numeric_matrix(x, "x")
  y <- as_numeric_matrix(y, "y")

  modout <- suppressMessages(suppressWarnings(ropls::opls(
    x,
    y,
    predI = 1,
    orthoI = ncomp.ortho,
    algoC = "nipals",
    crossvalI = crossval,
    log10L = FALSE,
    permI = permutation,
    scaleC = scale,
    subset = NULL,
    fig.pdfC = "none",
    info.txtC = "none"
  )))

  if (length(modout@scoreMN) == 0) {
    stop("No OPLS model could be built: the first predictive component is not significant. ",
         "Set 'ncomp.ortho' to force the number of orthogonal components.")
  }

  slot_tbl <- function(m) if (length(m) == 0) tibble::tibble() else as_tbl(m)

  res <- list(
    x_scores = slot_tbl(modout@scoreMN),
    x_loadings = slot_tbl(modout@loadingMN),
    x_weights = slot_tbl(modout@weightMN),
    orthoScores = slot_tbl(modout@orthoScoreMN),
    orthoLoadings = slot_tbl(modout@orthoLoadingMN),
    orthoWeights = slot_tbl(modout@orthoWeightMN),
    y_weights = slot_tbl(modout@cMN),
    y_scores = slot_tbl(modout@uMN),
    summary = modout@summaryDF,
    model = modout
  )
  return(res)
}
