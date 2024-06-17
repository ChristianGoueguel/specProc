#' @title Projected Orthogonal Signal Correction
#'
#' @author Christian L. Goueguel
#'
#' @description
#' The projected orthogonal signal correction (POSC) method is a preprocessing
#' technique used to remove systematic variation from predictor variables that
#' is orthogonal to the response variable. This function implements the POSC
#' algorithm for model fitting and prediction.
#'
#' @param x A matrix or data frame of the predictor variables.
#' @param y A vector of the response variable.
#' @param ncomp An integer specifying the number of components to include in the POSC model. Default is 5.
#' @param center A logical value indicating whether to mean-center `x` and `y`. Default is `TRUE`.
#' @param scale A logical value indicating whether to scale `x` and `y`. Default is `FALSE`.
#' @param tol A numeric value representing the tolerance for convergence. The default value is 1e-10.
#' @param newdata A matrix or data frame of new predictor variables to be corrected using the POSC model.
#'
#' @return If `newdata` is provided, a list containing the following components:
#'  - `correction`: The corrected matrix for the new data after applying POSC.
#'  - `scores`: The orthogonal scores matrix for the new data.
#'  If `newdata` is not provided, a list containing the following components:
#'  - `model`: A list containing the POSC model components:
#'    - `loadings`: The orthogonal loadings matrix.
#'    - `weights`: The orthogonal weights matrix.
#'
#' @export projected_osc
#'
projected_osc <- function(x, y, ncomp = 5, center = TRUE, scale = FALSE, tol = 1e-10, newdata = NULL) {

  if (center && scale) {
    x <- scale(x, center = TRUE, scale = TRUE)
    y <- scale(y, center = TRUE, scale = TRUE)
  } else if (center) {
    x <- scale(x, center = TRUE, scale = FALSE)
    y <- scale(y, center = TRUE, scale = FALSE)
  } else if (scale) {
    x <- scale(x, center = FALSE, scale = TRUE)
    y <- scale(y, center = FALSE, scale = TRUE)
  }

  plsFit <- pls::simpls.fit(x, y, ncomp, center = FALSE)
  w <- plsFit$coefficients[, , 1:ncomp]
  w <- w / sqrt(sum(w^2))
  t <- x %*% w

  # Step 2: Estimate principal components of T using PCA
  t_pca <- prcomp(t, center = FALSE)
  r_pca <- t_pca$sdev^2 / sum(t_pca$sdev^2)
  num_components <- sum(cumsum(r_pca) <= tol)

  # Step 3: Estimate P for each column
  p <- t(x) %*% (t_pca$x[, 1:num_components])

  # Step 4-6: Calculate PLS model and remove irrelevant variation
  x_ortho <- x - tcrossprod(p, p) %*% x
  x_pls <- x_ortho + tcrossprod(p, t)
  x_posc <- x - x_pls

  # Step 7: Set weight matrix W and orthogonal loadings P_ortho
  w <- t(p)
  P_ortho <- p

  # If newdata is provided, predict using the POSC model
  if (!is.null(newdata)) {
    # Center and scale new data if specified
    if (center && scale) {
      newdata <- scale(newdata, center = TRUE, scale = TRUE)
    } else if (center) {
      newdata <- scale(newdata, center = TRUE, scale = FALSE)
    } else if (scale) {
      newdata <- scale(newdata, center = FALSE, scale = TRUE)
    }

    # Step 8-10: Calculate orthogonal variation in new data
    t_new <- newdata %*% w
    t_ortho <- t_new %*% p_ortho
    t_ortho_new <- t_ortho - tcrossprod(t_ortho, P_ortho) %*% p_ortho

    # Step 11-14: Repeat for each orthogonal principal component
    for (i in 1:num_components) {
      t_last_ortho <- t_ortho_new[, i]
      t_last_ortho <- t_last_ortho / sqrt(sum(t_last_ortho^2))
      t_last_ortho_scores <- t_last_ortho

      t_last_ortho_p_ortho <- t(newdata) %*% t_last_ortho
      t_last_ortho_loadings <- cbind(t_last_ortho_P_ortho, t_last_ortho_scores)

      t_last_ortho <- tcrossprod(t_last_ortho_p_ortho, t_last_ortho_scores)
      newdata <- newdata - tcrossprod(t_last_ortho, t_last_ortho_p_ortho)
    }

    # Step 15: Return filtered new data
    newdata_posc <- newdata

    # Return the corrected data and scores for new data
    res <- list(
      "correction" = newdata_posc,
      "scores" = t_pca$x[, 1:num_components]
    )

  } else {
    # Return the POSC model components if no new data is provided
    res <- list(
      "model" = list(
        "correction" = x_psoc,
        "loadings" = p_ortho,
        "weights" = w
      )
    )
  }

  return(res)
}
