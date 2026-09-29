#' @title Spectral Normalization Recipe Step
#'
#' @author Christian L. Goueguel
#'
#' @description
#' `step_spectral_norm()` creates a *specification* of a recipe step that
#' divides each spectrum by one of its norms, with [normalize()]: its total
#' area, L1 norm, L2 (Euclidean) norm, or maximum absolute intensity.
#'
#' @details
#' The selected columns form one spectrum per row, and are replaced by the
#' normalized values. Unlike [recipes::step_normalize()], which scales each
#' column (variable), this step scales each row (spectrum), to remove
#' multiplicative variation between spectra such as shot-to-shot changes of
#' the ablated mass. Nothing is estimated from the training data. Spectra
#' whose norm is zero are set to `NA`, with a warning.
#'
#' The methods are those of [normalize()]: `"l1"` (sum of absolute values,
#' default), `"area"` (sum of values, equal to `"l1"` for non-negative
#' spectra), `"l2"` (Euclidean norm, also called vector normalization) and
#' `"max"` (largest absolute value). The `method` can be tuned with
#' `tune::tune()` (dials parameter `spectral_norm_method()`).
#' [tidy()][recipes::tidy.recipe] returns the `terms`, the `method` and `id`.
#'
#' @inherit step_baseline return
#' @inheritParams step_baseline
#' @param method The norm: `"l1"` (default), `"area"`, `"l2"` or `"max"`.
#'
#' @seealso [normalize()], [step_snv()], [step_line_ratio()]
#' @export
#'
#' @examples
#' if (rlang::is_installed("recipes")) {
#'   data(soilLIBS)
#'   rec <- recipes::recipe(Clay ~ ., data = soilLIBS[-c(1:2, 4:8)]) |>
#'     step_baseline(recipes::all_predictors()) |>
#'     step_spectral_norm(recipes::all_predictors(), method = "l2") |>
#'     recipes::prep()
#'   baked <- recipes::bake(rec, new_data = NULL)
#'   spectra <- baked[setdiff(names(baked), "Clay")]
#'   rowSums(spectra[1:3, ]^2)   # 1
#' }
step_spectral_norm <- function(recipe, ..., method = "l1", role = NA, trained = FALSE,
                               columns = NULL, skip = FALSE,
                               id = recipes::rand_id("spectral_norm")) {
  rlang::check_installed("recipes")
  if (!inherits(method, "tune_call") && !rlang::is_call(method)) {
    method <- match.arg(method, spectral_norm_methods)
  }
  recipes::add_step(recipe, specproc_step_new(
    "spectral_norm", terms = rlang::enquos(...), role = role, trained = trained,
    method = method, columns = columns, skip = skip, id = id
  ))
}

#' @title Norm of a Spectral Normalization
#'
#' @description
#' A dials parameter for the `method` of [step_spectral_norm()].
#'
#' @param values The norms to try. Default is all of `"l1"`, `"area"`,
#'   `"l2"` and `"max"`.
#'
#' @return A dials `qual_param` object.
#' @seealso [step_spectral_norm()]
#' @export
spectral_norm_method <- function(values = c("l1", "area", "l2", "max")) {
  rlang::check_installed("dials")
  dials::new_qual_param(type = "character", values = values,
                        label = c(spectral_norm_method = "Spectral norm"), finalize = NULL)
}

# ---- internals ---------------------------------------------------------------

spectral_norm_methods <- c("l1", "area", "l2", "max")

#' @exportS3Method recipes::prep
prep.step_spectral_norm <- function(x, training, info = NULL, ...) {
  x$method <- match.arg(x$method, spectral_norm_methods)
  x$columns <- step_predictors(x, training, info)
  x$trained <- TRUE
  x
}

#' @exportS3Method recipes::bake
bake.step_spectral_norm <- function(object, new_data, ...) {
  cols <- object$columns
  recipes::check_new_data(cols, object, new_data)
  if (length(cols) == 0) return(new_data)
  xmat <- step_matrix(new_data, cols)
  new_data[cols] <- as.data.frame(xmat / spectral_norm_factor(xmat, object$method))
  new_data
}

#' @export
print.step_spectral_norm <- function(x, width = max(20, options()$width - 30), ...) {
  label <- if (is.character(x$method)) x$method else "tuned"
  recipes::print_step(x$columns, x$terms, x$trained,
                      paste0("Spectral normalization (", label, ") on "), width)
  invisible(x)
}

#' @exportS3Method generics::tidy
tidy.step_spectral_norm <- function(x, ...) {
  terms <- if (recipes::is_trained(x)) x$columns else recipes::sel2char(x$terms)
  tibble::tibble(terms = terms, method = if (is.character(x$method)) x$method else NA_character_,
                 id = x$id)
}

#' @exportS3Method generics::tunable
tunable.step_spectral_norm <- function(x, ...) {
  tibble::tibble(name = "method",
                 call_info = list(list(pkg = "specProc", fun = "spectral_norm_method")),
                 source = "recipe", component = "step_spectral_norm", component_id = x$id)
}

#' @exportS3Method generics::required_pkgs
required_pkgs.step_spectral_norm <- function(x, ...) {
  c("specProc")
}
