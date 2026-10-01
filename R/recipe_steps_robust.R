# Recipe steps for robust transformations and robust PCA (tidymodels):
# robust Box-Cox / Yeo-Johnson, ROBPCA, ROSPCA and MacroPCA. See
# recipe_steps.R for the shared helpers.

#' @title Robust Box-Cox and Yeo-Johnson Transformation Recipe Step
#'
#' @author Christian L. Goueguel
#'
#' @description
#' `step_robust_bcyj()` creates a *specification* of a recipe step that
#' transforms the selected variables toward central normality with the
#' robust Box-Cox or Yeo-Johnson transformation of [robust_bcyj()], like
#' [recipes::step_BoxCox()] or [recipes::step_YeoJohnson()] but robust to
#' outliers.
#'
#' @details
#' The transformation parameter \eqn{\lambda} of each variable is estimated
#' on the training data by re-weighted maximum likelihood (Raymaekers and
#' Rousseeuw, 2021) with [cellWise::transfo()], and applied unchanged to new
#' data with [cellWise::transfo_newdata()]. With `standardize = TRUE`, the
#' transformed variables are also robustly centered and scaled with the
#' training estimates. Variables that cannot be transformed (for example,
#' constant ones) are left unchanged.
#'
#' [tidy()][recipes::tidy.recipe] returns a tibble with columns `terms`,
#' `lambda`, `method` (`"BC"` or `"YJ"`) and `id`.
#'
#' @inherit step_baseline return
#' @inheritParams step_baseline
#' @param type The transformation: `"bestObj"` (default; Box-Cox or
#'   Yeo-Johnson, whichever fits best, for positive variables), `"BC"` or
#'   `"YJ"`. See [robust_bcyj()].
#' @param quantile The quantile used for the weights of the re-weighting
#'   steps. Default is 0.99.
#' @param nbsteps The number of re-weighting steps. Default is 2.
#' @param standardize A logical: robustly standardize the transformed
#'   variables (`TRUE`, default).
#' @param res The fitted transformation, stored once the step has been
#'   trained.
#'
#' @seealso [robust_bcyj()]
#' @export
#'
#' @examplesIf rlang::is_installed("recipes")
#' library(recipes)
#' data(forageLIBS)
#' contents <- forageLIBS[c("Ca", "Mg", "P", "K", "Mn")]
#' rec <- recipe(~ ., data = contents[1:300, ]) |>
#'   step_robust_bcyj(all_numeric_predictors())
#' prepped <- prep(rec)
#' tidy(prepped, number = 1)
#' bake(prepped, new_data = contents[301:368, ])
step_robust_bcyj <- function(recipe, ..., role = NA, trained = FALSE, type = "bestObj",
                             quantile = 0.99, nbsteps = 2, standardize = TRUE,
                             res = NULL, columns = NULL, skip = FALSE,
                             id = recipes::rand_id("robust_bcyj")) {
  rlang::check_installed("recipes")
  type <- match.arg(type, c("bestObj", "BC", "YJ"))
  recipes::add_step(recipe, specproc_step_new(
    "robust_bcyj", terms = rlang::enquos(...), role = role, trained = trained,
    type = type, quantile = quantile, nbsteps = nbsteps, standardize = standardize,
    res = res, columns = columns, skip = skip, id = id
  ))
}

#' @title Robust PCA (ROBPCA) Recipe Step
#'
#' @description
#' `step_robpca()` creates a *specification* of a recipe step that converts
#' the selected variables into robust principal component scores, with
#' [robpca()]. It is the robust counterpart of [recipes::step_pca()].
#'
#' @details
#' The robust PCA model is estimated on the training data when the recipe is
#' prepped, and new data are projected onto it when they are baked. The new
#' columns are named with `prefix` followed by the component number. With
#' `distances = TRUE`, two more columns hold the score distance
#' (`<prefix>_SD`) and orthogonal distance (`<prefix>_OD`) of each
#' observation, which can be used to screen outliers. The selected columns
#' are removed unless `keep_original_cols = TRUE`.
#'
#' # Tuning
#'
#' `num_comp` can be tuned with [tune::tune()], using [dials::num_comp()]
#' with values 1 to 4.
#'
#' # Tidying
#'
#' [tidy()][recipes::tidy.recipe] returns the loadings as a tibble with
#' columns `terms`, `value`, `component` and `id`.
#'
#' @inherit step_baseline return
#' @inheritParams step_baseline
#' @param role The role of the new score columns. Default is `"predictor"`.
#' @param num_comp The number of principal components. Default is 2.
#' @param options A list of further arguments passed to [robpca()], such as
#'   `alpha` or `ndir`.
#' @param prefix The prefix of the new column names. Default is `"RPC"`.
#' @param distances A logical: add the score and orthogonal distances as
#'   columns. Default is `FALSE`.
#' @param keep_original_cols A logical: keep the selected columns. Default is
#'   `FALSE`.
#' @param res The fitted robust PCA model, stored once the step has been
#'   trained.
#'
#' @seealso [robpca()], [step_rospca()], [step_macropca()]
#' @export
#'
#' @examplesIf rlang::is_installed("recipes")
#' library(recipes)
#' data(forageLIBS)
#' spectra <- forageLIBS[-(1:14)]
#' set.seed(1)
#' rec <- recipe(~ ., data = spectra) |>
#'   step_robpca(all_predictors(), num_comp = 3, distances = TRUE)
#' bake(prep(rec), new_data = NULL)
step_robpca <- function(recipe, ..., role = "predictor", trained = FALSE, num_comp = 2,
                        options = list(), prefix = "RPC", distances = FALSE,
                        keep_original_cols = FALSE, res = NULL, columns = NULL,
                        skip = FALSE, id = recipes::rand_id("robpca")) {
  rlang::check_installed("recipes")
  recipes::add_step(recipe, specproc_step_new(
    "robpca", terms = rlang::enquos(...), role = role, trained = trained,
    num_comp = num_comp, options = options, prefix = prefix, distances = distances,
    keep_original_cols = keep_original_cols, res = res, columns = columns,
    skip = skip, id = id
  ))
}

#' @title Robust Sparse PCA (ROSPCA) Recipe Step
#'
#' @description
#' `step_rospca()` creates a *specification* of a recipe step that converts
#' the selected variables into robust sparse principal component scores,
#' with [rospca()].
#'
#' @details
#' As [step_robpca()], with sparse loadings. `num_comp` and `lambda` can be
#' tuned with [tune::tune()]; `lambda` uses [dials::penalty()] with a range
#' of 0.01 to 100.
#'
#' @inherit step_robpca return
#' @inheritParams step_robpca
#' @param lambda The sparsity parameter of [rospca()]. Default is 1.
#' @param options A list of further arguments passed to [rospca()], such as
#'   `alpha`, `stand` or `ndir`.
#' @param prefix The prefix of the new column names. Default is `"RSPC"`.
#'
#' @seealso [rospca()], [step_robpca()]
#' @export
step_rospca <- function(recipe, ..., role = "predictor", trained = FALSE, num_comp = 2,
                        lambda = 1, options = list(), prefix = "RSPC", distances = FALSE,
                        keep_original_cols = FALSE, res = NULL, columns = NULL,
                        skip = FALSE, id = recipes::rand_id("rospca")) {
  rlang::check_installed("recipes")
  recipes::add_step(recipe, specproc_step_new(
    "rospca", terms = rlang::enquos(...), role = role, trained = trained,
    num_comp = num_comp, lambda = lambda, options = options, prefix = prefix,
    distances = distances, keep_original_cols = keep_original_cols, res = res,
    columns = columns, skip = skip, id = id
  ))
}

#' @title MacroPCA Recipe Step
#'
#' @description
#' `step_macropca()` creates a *specification* of a recipe step that converts
#' the selected variables into robust principal component scores with
#' [macropca()], which handles cellwise outliers and missing values as well
#' as outlying observations.
#'
#' @details
#' As [step_robpca()]. Missing values are allowed in the selected columns,
#' both when the recipe is prepped and when it is baked.
#'
#' @inherit step_robpca return
#' @inheritParams step_robpca
#' @param options A list of further arguments passed to [macropca()], such
#'   as `alpha` or MacroPCA parameters.
#' @param prefix The prefix of the new column names. Default is `"MPC"`.
#'
#' @seealso [macropca()], [step_robpca()]
#' @export
step_macropca <- function(recipe, ..., role = "predictor", trained = FALSE, num_comp = 2,
                          options = list(), prefix = "MPC", distances = FALSE,
                          keep_original_cols = FALSE, res = NULL, columns = NULL,
                          skip = FALSE, id = recipes::rand_id("macropca")) {
  rlang::check_installed("recipes")
  recipes::add_step(recipe, specproc_step_new(
    "macropca", terms = rlang::enquos(...), role = role, trained = trained,
    num_comp = num_comp, options = options, prefix = prefix, distances = distances,
    keep_original_cols = keep_original_cols, res = res, columns = columns,
    skip = skip, id = id
  ))
}

# ---- internals ---------------------------------------------------------------

robust_titles <- c(
  robust_bcyj = "Robust Box-Cox/Yeo-Johnson transformation on ",
  robpca = "Robust PCA (ROBPCA) on ",
  rospca = "Robust sparse PCA (ROSPCA) on ",
  macropca = "MacroPCA on "
)

# Keeps only what transfo_newdata() needs. Recent cellWise versions read the
# variable names from the transformed training data `Y`, so a zero-row copy
# of it is kept.
strip_transfo <- function(fit) {
  if (!is.null(fit$Y)) fit$Y <- fit$Y[0, , drop = FALSE]
  fit[c("Xt", "weights", "remX", "rowInAnalysis", "namesCaseNumber")] <- NULL
  fit
}

# Keeps only what predict() needs from a robust PCA model.
strip_robust_pca <- function(fit) {
  drop <- c("scores", "sd", "od", "outlier_type", "H0", "H1", "H2", "H3", "std_resid",
            "flagged_cells", "imputed")
  fit[drop] <- NULL
  fit
}

prep_robust_step <- function(x, training, info = NULL, ...) {
  cols <- step_predictors(x, training, info)
  step <- class(x)[1]
  xmat <- step_matrix(training, cols)
  if (step != "step_macropca" && length(cols) > 0 && anyNA(xmat)) {
    stop("`", step, "()` does not handle missing values; impute them first",
         if (step %in% c("step_robpca", "step_rospca")) " or use `step_macropca()`" else "",
         ".", call. = FALSE)
  }
  if (length(cols) > 0) {
    if (step != "step_robust_bcyj") check_count(x$num_comp, "num_comp")
    opts <- x$options %||% list()
    x$res <- switch(
      step,
      step_robust_bcyj = strip_transfo(cellWise::transfo(
        xmat, type = x$type, robust = TRUE, standardize = x$standardize,
        quant = x$quantile, nbsteps = x$nbsteps, checkPars = list(silent = TRUE)
      )),
      step_robpca = strip_robust_pca(do.call(robpca, c(list(xmat, k = x$num_comp), opts))),
      step_rospca = strip_robust_pca(do.call(rospca, c(list(xmat, k = x$num_comp, lambda = x$lambda), opts))),
      step_macropca = strip_robust_pca(do.call(macropca, c(list(xmat, k = x$num_comp), opts)))
    )
  }
  if (step != "step_robust_bcyj") {
    x$keep_original_cols <- recipes::get_keep_original_cols(x)
  }
  x$columns <- cols
  x$trained <- TRUE
  x
}

bake_robust_step <- function(object, new_data, ...) {
  cols <- object$columns
  recipes::check_new_data(cols, object, new_data)
  if (length(cols) == 0) {
    return(new_data)
  }
  xmat <- step_matrix(new_data, cols)
  if (class(object)[1] == "step_robust_bcyj") {
    out <- cellWise::transfo_newdata(xmat, object$res)
    transformed <- intersect(colnames(out), cols)
    new_data[transformed] <- as.data.frame(out[, transformed, drop = FALSE])
    return(new_data)
  }
  pred <- stats::predict(object$res, xmat)
  k <- object$res$k
  comps <- as.matrix(pred[seq_len(k)])
  colnames(comps) <- paste0(object$prefix, seq_len(k))
  if (isTRUE(object$distances)) {
    comps <- cbind(comps, pred$sd, pred$od)
    colnames(comps)[k + 1:2] <- paste0(object$prefix, c("_SD", "_OD"))
  }
  comps <- recipes::check_name(tibble::as_tibble(comps), new_data, object, newname = colnames(comps))
  new_data <- dplyr::bind_cols(new_data, comps)
  recipes::remove_original_cols(new_data, object, cols)
}

print_robust_step <- function(x, width = max(20, options()$width - 30), ...) {
  title <- robust_titles[[sub("^step_", "", class(x)[1])]]
  recipes::print_step(x$columns, x$terms, x$trained, title, width)
  invisible(x)
}

tidy_robust_step <- function(x, ...) {
  trained <- recipes::is_trained(x) && !is.null(x$res)
  if (class(x)[1] == "step_robust_bcyj") {
    if (trained) {
      res <- tibble::tibble(terms = x$res$colnamX %||% x$columns,
                            lambda = unname(x$res$lambdahats),
                            method = unname(x$res$ttypes))
    } else {
      terms <- if (recipes::is_trained(x)) x$columns else recipes::sel2char(x$terms)
      res <- tibble::tibble(terms = terms, lambda = NA_real_, method = NA_character_)
    }
  } else if (trained) {
    loadings <- x$res$loadings
    res <- tibble::tibble(
      terms = rep(x$columns, ncol(loadings)),
      value = as.vector(loadings),
      component = rep(paste0(x$prefix, seq_len(ncol(loadings))), each = nrow(loadings))
    )
  } else {
    terms <- if (recipes::is_trained(x)) x$columns else recipes::sel2char(x$terms)
    res <- tibble::tibble(terms = terms, value = NA_real_, component = NA_character_)
  }
  res$id <- x$id
  res
}

tunable_robust_step <- function(x, ...) {
  row <- function(name, call_info) {
    tibble::tibble(name = name, call_info = list(call_info), source = "recipe",
                   component = class(x)[1], component_id = x$id)
  }
  num_comp <- row("num_comp", list(pkg = "dials", fun = "num_comp", range = c(1L, 4L)))
  switch(
    class(x)[1],
    step_robust_bcyj = num_comp[0, ],
    step_rospca = rbind(num_comp, row("lambda", list(pkg = "dials", fun = "penalty", range = c(-2, 2)))),
    num_comp
  )
}

required_pkgs_robust_step <- function(x, ...) {
  c("specProc", "cellWise")
}

# ---- S3 methods (registered when recipes / generics are loaded) ---------------

#' @exportS3Method recipes::prep
prep.step_robust_bcyj <- prep_robust_step
#' @exportS3Method recipes::prep
prep.step_robpca <- prep_robust_step
#' @exportS3Method recipes::prep
prep.step_rospca <- prep_robust_step
#' @exportS3Method recipes::prep
prep.step_macropca <- prep_robust_step

#' @exportS3Method recipes::bake
bake.step_robust_bcyj <- bake_robust_step
#' @exportS3Method recipes::bake
bake.step_robpca <- bake_robust_step
#' @exportS3Method recipes::bake
bake.step_rospca <- bake_robust_step
#' @exportS3Method recipes::bake
bake.step_macropca <- bake_robust_step

#' @export
print.step_robust_bcyj <- print_robust_step
#' @export
print.step_robpca <- print_robust_step
#' @export
print.step_rospca <- print_robust_step
#' @export
print.step_macropca <- print_robust_step

#' @exportS3Method generics::tidy
tidy.step_robust_bcyj <- tidy_robust_step
#' @exportS3Method generics::tidy
tidy.step_robpca <- tidy_robust_step
#' @exportS3Method generics::tidy
tidy.step_rospca <- tidy_robust_step
#' @exportS3Method generics::tidy
tidy.step_macropca <- tidy_robust_step

#' @exportS3Method generics::tunable
tunable.step_robust_bcyj <- tunable_robust_step
#' @exportS3Method generics::tunable
tunable.step_robpca <- tunable_robust_step
#' @exportS3Method generics::tunable
tunable.step_rospca <- tunable_robust_step
#' @exportS3Method generics::tunable
tunable.step_macropca <- tunable_robust_step

#' @exportS3Method generics::required_pkgs
required_pkgs.step_robust_bcyj <- required_pkgs_robust_step
#' @exportS3Method generics::required_pkgs
required_pkgs.step_robpca <- required_pkgs_robust_step
#' @exportS3Method generics::required_pkgs
required_pkgs.step_rospca <- required_pkgs_robust_step
#' @exportS3Method generics::required_pkgs
required_pkgs.step_macropca <- required_pkgs_robust_step
