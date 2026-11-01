#' @title Robust Tukey g-and-h Transformation Recipe Step
#'
#' @description
#' Creates a recipe step applying [robust_gandh()] to the selected variables.
#' The rQLS fit and all standardization constants are estimated during
#' `prep()` on the training data and applied unchanged during `bake()`.
#'
#' @details
#' Selected columns are replaced in place. Missing values stay missing and
#' variables without enough observations or spread are left unchanged, as
#' in [robust_gandh()]. `tidy()` returns `terms`, `location`, `scale`, `g`,
#' `h` and `id`; location and scale are on the original input scale.
#'
#' @inherit step_baseline return
#' @inheritParams step_robust_boxcox
#' @inheritParams robust_gandh
#' @seealso [robust_gandh()], [step_robust_boxcox()], [step_robust_yeojohnson()]
#' @author Christian L. Goueguel
#' @export
#'
#' @examplesIf rlang::is_installed("recipes")
#' data(forageLIBS)
#' contents <- forageLIBS[c("Ca", "K", "P", "Mn")]
#' rec <- recipes::recipe(~ ., data = contents[1:300, ]) |>
#'   step_robust_gandh(recipes::all_numeric_predictors())
#' trained <- recipes::prep(rec)
#' recipes::bake(trained, new_data = contents[301:368, ])
step_robust_gandh <- function(recipe, ..., role = NA, trained = FALSE,
                              n_quantiles = 10, cutoff = NULL, max_iter = 50,
                              tol = 1e-6, standardize = TRUE, res = NULL,
                              columns = NULL, skip = FALSE,
                              id = recipes::rand_id("robust_gandh")) {
  rlang::check_installed("recipes")
  recipes::add_step(recipe, specproc_step_new(
    "robust_gandh", terms = rlang::enquos(...), role = role, trained = trained,
    n_quantiles = n_quantiles, cutoff = cutoff, max_iter = max_iter, tol = tol,
    standardize = standardize, res = res, columns = columns, skip = skip, id = id
  ))
}

#' @exportS3Method recipes::prep
prep.step_robust_gandh <- function(x, training, info = NULL, ...) {
  check_gandh_args(x$n_quantiles, x$cutoff, x$max_iter, x$tol, x$standardize)
  x$columns <- step_predictors(x, training, info)
  if (length(x$columns) > 0) {
    fit <- robust_gandh(step_matrix(training, x$columns), n_quantiles = x$n_quantiles,
                         cutoff = x$cutoff, max_iter = x$max_iter, tol = x$tol,
                         standardize = x$standardize)
    x$res <- fit[c("fits", "standardize")]
  }
  x$trained <- TRUE
  x
}

#' @exportS3Method recipes::bake
bake.step_robust_gandh <- bake_robust_step
#' @export
print.step_robust_gandh <- print_robust_step
#' @exportS3Method generics::tunable
tunable.step_robust_gandh <- tunable_robust_step
#' @exportS3Method generics::required_pkgs
required_pkgs.step_robust_gandh <- required_pkgs_robust_step

#' @exportS3Method generics::tidy
tidy.step_robust_gandh <- function(x, ...) {
  if (recipes::is_trained(x) && !is.null(x$res)) {
    res <- gandh_summary(x$res$fits)[c("variable", "location", "scale", "g", "h")]
    names(res)[1] <- "terms"
  } else {
    terms <- if (recipes::is_trained(x)) x$columns else recipes::sel2char(x$terms)
    res <- tibble::tibble(terms = terms, location = rep(NA_real_, length(terms)),
                          scale = rep(NA_real_, length(terms)), g = rep(NA_real_, length(terms)),
                          h = rep(NA_real_, length(terms)))
  }
  res$id <- x$id
  res
}
