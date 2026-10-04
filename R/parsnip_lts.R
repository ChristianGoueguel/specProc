#' @title Robust Linear Regression in tidymodels (LTS parsnip Engine)
#'
#' @author Christian L. Goueguel
#'
#' @description
#' specProc adds the `"lts"` engine to [parsnip::linear_reg()]: the
#' reweighted least trimmed squares (LTS) regression of
#' [robustbase::ltsReg()], which resists vertical outliers and bad leverage
#' points. `lts_fit()` fits it outside of tidymodels.
#'
#' @details
#' The engine is available once parsnip and specProc are both loaded, in
#' either order. It is for regression with one outcome.
#'
#' Combined with [step_robpca()] in a workflow, it gives the robust
#' principal component regression of Hubert and Verboven (2003), whose
#' robust PCA has `num_comp` components (see also [rpcr()]): the robust
#' scores of the spectra, then the LTS regression on them. It also gives
#' robust calibration curves from a few line intensities.
#'
#' # Engine arguments
#'
#' - `alpha`: the fraction of observations whose squared residuals are
#'   minimized, between 0.5 and 1 (default 0.75, as in [rpcr()]).
#' - `nsamp`: the number of random subsets (default 500).
#'
#' The main arguments of [parsnip::linear_reg()], `penalty` and `mixture`,
#' are not used. LTS uses random subsets, so use [set.seed()] before fitting
#' for reproducible results.
#'
#' @param x A numeric matrix or data frame of the predictors.
#' @param y A numeric vector of the response.
#' @param alpha The fraction of observations whose squared residuals are
#'   minimized, between 0.5 and 1. Default is 0.75.
#' @param nsamp The number of random subsets. Default is 500.
#'
#' @return `lts_fit()` returns an object of class `specproc_lts`, a list with
#'   the `coefficients` (intercept first), the robust `scale` of the
#'   residuals, the `fitted` values, `residuals`, standardized residuals
#'   `std_resid` and the `weights` of the reweighted fit (1 for the
#'   observations whose standardized residual is within the cut-off). Its
#'   [predict()] method takes `newdata`.
#'
#' @references
#'  - Rousseeuw, P.J., Van Driessen, K. (2006). Computing LTS regression for
#'    large data sets. Data Mining and Knowledge Discovery, 12(1):29-45.
#'  - Hubert, M., Verboven, S. (2003). A robust PCR method for
#'    high-dimensional regressors. Journal of Chemometrics, 17(8-9):438-452.
#'
#' @name linear_reg_lts
#' @seealso [rpcr()], [step_robpca()], [parsnip::linear_reg()]
#'
#' @examplesIf rlang::is_installed("parsnip")
#' library(parsnip)
#' data(forageLIBS)
#' lines <- forageLIBS[c("Ca", "393.3599236", "396.8602175")]  # Ca II K and H
#' names(lines) <- c("Ca", "CaII_393", "CaII_397")
#' set.seed(1)
#' fit <- linear_reg() |>
#'   set_engine("lts") |>
#'   fit(Ca ~ ., data = lines[1:300, ])
#' predict(fit, lines[301:305, ])
#' extract_fit_engine(fit)$coefficients
NULL

#' @rdname linear_reg_lts
#' @export
lts_fit <- function(x, y, alpha = 0.75, nsamp = 500) {
  x <- as_numeric_matrix(x, "x")
  if (is.null(colnames(x))) colnames(x) <- paste0("x", seq_len(ncol(x)))
  y <- as_response_matrix(y, nrow(x))
  if (ncol(y) != 1) {
    stop("LTS regression needs a single response.", call. = FALSE)
  }
  if (anyNA(x) || anyNA(y)) {
    stop("'x' and 'y' cannot contain missing values.", call. = FALSE)
  }
  check_number(alpha, "alpha", lower = 0.5, upper = 1)
  check_count(nsamp, "nsamp")
  fit <- robustbase::ltsReg(x, drop(y), alpha = alpha, nsamp = nsamp)
  coef <- stats::setNames(unname(fit$coefficients), c("(Intercept)", colnames(x)))
  fitted <- drop(x %*% coef[-1]) + coef[1]
  res <- list(coefficients = coef, scale = fit$scale, fitted = fitted,
              residuals = drop(y) - fitted, std_resid = (drop(y) - fitted) / fit$scale,
              weights = fit$lts.wt, alpha = alpha)
  structure(res, variables = colnames(x), nvar = ncol(x), class = "specproc_lts")
}

#' @export
predict.specproc_lts <- function(object, newdata, ...) {
  x <- filter_newdata(object, newdata)
  drop(x %*% object$coefficients[-1]) + object$coefficients[[1]]
}

#' @export
print.specproc_lts <- function(x, ...) {
  cat("Reweighted LTS regression\n\n")
  print(x$coefficients)
  cat("\nResidual scale: ", format(x$scale, digits = 4), "\n", sep = "")
  cat("Outliers:       ", sum(x$weights == 0), " of ", length(x$weights), "\n", sep = "")
  invisible(x)
}

# Registers the "lts" engine of parsnip::linear_reg(), once (see
# register_pls_rsimpls()).
register_linear_reg_lts <- function() {
  env <- parsnip::get_model_env()
  if (!"linear_reg" %in% env$models || "lts" %in% env$linear_reg$engine) {
    return(invisible(FALSE))
  }
  parsnip::set_model_engine("linear_reg", mode = "regression", eng = "lts")
  parsnip::set_dependency("linear_reg", eng = "lts", pkg = "robustbase", mode = "regression")
  parsnip::set_dependency("linear_reg", eng = "lts", pkg = "specProc", mode = "regression")
  parsnip::set_fit(
    model = "linear_reg", eng = "lts", mode = "regression",
    value = list(interface = "matrix", protect = c("x", "y"),
                 func = c(pkg = "specProc", fun = "lts_fit"), defaults = list())
  )
  parsnip::set_encoding(
    model = "linear_reg", eng = "lts", mode = "regression",
    options = list(predictor_indicators = "traditional", compute_intercept = TRUE,
                   remove_intercept = TRUE, allow_sparse_x = FALSE)
  )
  parsnip::set_pred(
    model = "linear_reg", eng = "lts", mode = "regression", type = "numeric",
    value = list(pre = NULL, post = NULL, func = c(fun = "predict"),
                 args = list(object = quote(object$fit), newdata = quote(new_data)))
  )
  invisible(TRUE)
}
