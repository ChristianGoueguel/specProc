
opls <- function(x, y, crossval = 7, permutation = 20, scale = "center"){
  if (is.null(x) == TRUE) {
    stop("Data must be provided")
  }
  if (is.null(y) == TRUE) {
    stop("Data must be provided")
  }
  if (is.data.frame(x) == FALSE & tibble::is_tibble(x) == FALSE) {
    stop("Data must be of class data.frame, tbl_df, or tbl")
  }
  if (is.data.frame(y) == FALSE & tibble::is_tibble(y) == FALSE) {
    stop("Data must be of class data.frame, tbl_df, or tbl")
  }

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









}
