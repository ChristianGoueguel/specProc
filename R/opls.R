
opls <- function(x, y, scale = "center", crossval = 7, permutation = 20){
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

  model <- ropls::opls(
    x,
    y,
    predI = 1,
    orthoI = NA,
    algoC = "nipals",
    crossvalI = crossval,
    log10L = FALSE,
    permI = permutation,
    scaleC = scale,
    subset = NULL,
    printL = FALSE,
    plotL = FALSE,
    .sinkC = NULL
    )

  res <- list(
    model$scoreMN <- x.scores |> tibble::as_tibble(),
    model$loadingMN <- x.loadings |> tibble::as_tibble(),
    model$weightMN <- x.weights |> tibble::as_tibble(),
    model$orthoScoreMN <- orthoScores |> tibble::as_tibble(),
    model$orthoLoadingMN <- orthoLoadings |> tibble::as_tibble(),
    model$orthoWeightMN <- orthoWeights |> tibble::as_tibble(),
    model$cMN <- y.weights |> tibble::as_tibble(),
    model$uMN <- y.orthoWeights |> tibble::as_tibble(),
    model$weightStarMN <- y.scores |> tibble::as_tibble()
  )

  res
}
