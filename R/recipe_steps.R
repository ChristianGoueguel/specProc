# Recipe steps for the orthogonalization filters (tidymodels).
#
# Each step estimates its filter in prep() and applies it in bake(), so that
# inside workflows and tune the filter is re-estimated on the analysis set of
# every resample. The steps replace the selected columns in place. recipes is
# only suggested: the S3 methods are registered when recipes is loaded.

#' @title Orthogonal Signal Correction Recipe Step
#'
#' @author Christian L. Goueguel
#'
#' @description
#' `step_osc()` creates a *specification* of a recipe step that removes the
#' variation of the selected predictors that is orthogonal to the outcome,
#' with [osc()]. The filter is estimated on the training data when the recipe
#' is prepped and applied to new data when it is baked.
#'
#' @details
#' The selected columns are replaced by the corrected values, which are
#' centered (and scaled, if `options = list(scale = TRUE)`). Because the
#' filter uses the outcome, it must be estimated on training data only.
#' Within a [workflows::workflow()] and [tune::tune_grid()], this happens
#' automatically for every resample. The outcome is not needed when new data
#' are baked.
#'
#' The related steps [step_direct_orthogonal()], [step_direct_osc()] and
#' [step_projected_osc()] remove response-orthogonal variation with other
#' algorithms. [step_epo()] and [step_glsw()] remove variation described by
#' external clutter, and [step_y_gradient_glsw()] down-weights variation
#' between samples with similar outcomes.
#'
#' # Tuning
#'
#' `num_comp` can be tuned with [tune::tune()]; its default range is
#' [dials::num_comp()] with values 1 to 4. When the model also has a
#' `num_comp` argument (for example, [parsnip::pls()]), give the step's
#' parameter its own id, such as `num_comp = tune("filter_comp")`, so that
#' the two can be tuned together:
#'
#' ```r
#' rec <- recipe(K ~ ., data = spectra) |>
#'   step_osc(all_predictors(), method = "fearn", num_comp = tune("filter_comp"))
#' model <- parsnip::pls(num_comp = tune()) |>
#'   set_mode("regression") |>
#'   set_engine("mixOmics", scale = FALSE)  # center only, like pls::plsr()
#' wf <- workflow(rec, model)
#' tune_grid(wf, resamples = group_vfold_cv(spectra, group = Sample), grid = 9)
#' ```
#'
#' # Tidying
#'
#' [tidy()][recipes::tidy.recipe] returns a tibble with columns `terms` (the
#' selected predictors), `num_comp` and `id`.
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param ... One or more selector functions to choose the predictors (for
#'   example, the spectral channels). See [recipes::selections()].
#' @param role Not used by this step, since no new variables are created.
#' @param trained A logical indicating whether the step has been trained.
#' @param outcome The outcome variable, as a bare name or a selector. If
#'   `NULL` (default), the single outcome of the recipe is used.
#' @param method The OSC algorithm: `"wold"`, `"sjoblom"` (default) or
#'   `"fearn"`. See [osc()].
#' @param num_comp The number of orthogonal components to remove.
#' @param options A list of further arguments passed to the underlying
#'   function, such as `scale`, `tol` or `max.iter` for [osc()].
#' @param res The fitted filter, stored once the step has been trained.
#' @param columns The names of the selected predictors, stored once the step
#'   has been trained.
#' @param skip A logical. Should the step be skipped when the recipe is baked
#'   by [recipes::bake()]? Keep the default `FALSE`, so that new data are
#'   corrected with the same filter as the training data.
#' @param id A character string that is unique to this step.
#'
#' @return An updated version of `recipe` with the new step added to the
#'   sequence of existing steps.
#'
#' @seealso [osc()], [predict.specproc_filter()]
#'
#' @export
#'
#' @examplesIf rlang::is_installed("recipes")
#' library(recipes)
#' set.seed(1)
#' x <- matrix(rnorm(60 * 20), 60, 20, dimnames = list(NULL, paste0("wl", 1:20)))
#' dat <- data.frame(y = x[, 1] + rnorm(60, sd = 0.1), x)
#'
#' rec <- recipe(y ~ ., data = dat[1:40, ]) |>
#'   step_osc(all_predictors(), method = "fearn", num_comp = 2)
#' prepped <- prep(rec)
#' tidy(prepped, number = 1)
#'
#' # new spectra are corrected with the filter estimated on the training data
#' bake(prepped, new_data = dat[41:60, -1])
#'
step_osc <- function(recipe, ..., role = NA, trained = FALSE, outcome = NULL,
                     method = "sjoblom", num_comp = 2, options = list(),
                     res = NULL, columns = NULL, skip = FALSE,
                     id = recipes::rand_id("osc")) {
  rlang::check_installed("recipes")
  method <- match.arg(method, c("wold", "sjoblom", "fearn"))
  recipes::add_step(recipe, specproc_step_new(
    "osc", terms = rlang::enquos(...), role = role, trained = trained,
    outcome = rlang::enquos(outcome), method = method, num_comp = num_comp,
    options = options, res = res, columns = columns, skip = skip, id = id
  ))
}

#' @title Direct Orthogonalization Recipe Step
#'
#' @description
#' `step_direct_orthogonal()` creates a *specification* of a recipe step that
#' removes response-orthogonal variation from the selected predictors with
#' [direct_orthogonal()] (equivalent to [nas()]).
#'
#' @inherit step_osc details return
#' @inheritParams step_osc
#' @param options A list of further arguments passed to
#'   [direct_orthogonal()], such as `scale`.
#'
#' @seealso [direct_orthogonal()], [predict.specproc_filter()]
#' @export
step_direct_orthogonal <- function(recipe, ..., role = NA, trained = FALSE,
                                   outcome = NULL, num_comp = 2, options = list(),
                                   res = NULL, columns = NULL, skip = FALSE,
                                   id = recipes::rand_id("direct_orthogonal")) {
  rlang::check_installed("recipes")
  recipes::add_step(recipe, specproc_step_new(
    "direct_orthogonal", terms = rlang::enquos(...), role = role,
    trained = trained, outcome = rlang::enquos(outcome), num_comp = num_comp,
    options = options, res = res, columns = columns, skip = skip, id = id
  ))
}

#' @title Direct Orthogonal Signal Correction Recipe Step
#'
#' @description
#' `step_direct_osc()` creates a *specification* of a recipe step that
#' removes response-orthogonal variation from the selected predictors with
#' [direct_osc()].
#'
#' @inherit step_osc details return
#' @inheritParams step_osc
#' @param options A list of further arguments passed to [direct_osc()], such
#'   as `scale` or `tol`.
#'
#' @seealso [direct_osc()], [predict.specproc_filter()]
#' @export
step_direct_osc <- function(recipe, ..., role = NA, trained = FALSE,
                            outcome = NULL, num_comp = 2, options = list(),
                            res = NULL, columns = NULL, skip = FALSE,
                            id = recipes::rand_id("direct_osc")) {
  rlang::check_installed("recipes")
  recipes::add_step(recipe, specproc_step_new(
    "direct_osc", terms = rlang::enquos(...), role = role, trained = trained,
    outcome = rlang::enquos(outcome), num_comp = num_comp, options = options,
    res = res, columns = columns, skip = skip, id = id
  ))
}

#' @title Projected Orthogonal Signal Correction (OPLS Filter) Recipe Step
#'
#' @description
#' `step_projected_osc()` creates a *specification* of a recipe step that
#' removes `num_comp` response-orthogonal components from the selected
#' predictors with [projected_osc()]. The result is the OPLS-filtered data
#' (equivalently, [o2pls()] with a single outcome).
#'
#' @inherit step_osc details return
#' @inheritParams step_osc
#' @param num_comp The number of orthogonal components to remove. The
#'   underlying PLS model has `num_comp + 1` components.
#' @param options A list of further arguments passed to [projected_osc()],
#'   such as `scale` or `tol`.
#'
#' @seealso [projected_osc()], [predict.specproc_filter()]
#' @export
step_projected_osc <- function(recipe, ..., role = NA, trained = FALSE,
                               outcome = NULL, num_comp = 2, options = list(),
                               res = NULL, columns = NULL, skip = FALSE,
                               id = recipes::rand_id("projected_osc")) {
  rlang::check_installed("recipes")
  recipes::add_step(recipe, specproc_step_new(
    "projected_osc", terms = rlang::enquos(...), role = role,
    trained = trained, outcome = rlang::enquos(outcome), num_comp = num_comp,
    options = options, res = res, columns = columns, skip = skip, id = id
  ))
}

#' @title External Parameter Orthogonalization Recipe Step
#'
#' @description
#' `step_epo()` creates a *specification* of a recipe step that projects the
#' selected predictors onto the space orthogonal to the dominant directions
#' of a clutter matrix, with [epo()].
#'
#' @details
#' The clutter describes variation that should not affect the outcome, for
#' example the differences between repeated measurements of the same samples.
#' It is external information, supplied when the step is specified, and is
#' not estimated from the training data. If `clutter = NULL`, the dominant
#' directions of the training data themselves are removed, as in [epo()].
#' The outcome is not used.
#'
#' The selected columns are replaced by the corrected values (not centered).
#' `num_comp` can be tuned with [tune::tune()]. [tidy()][recipes::tidy.recipe]
#' returns the selected `terms`, `num_comp` and `id`.
#'
#' @inherit step_osc return
#' @inheritParams step_osc
#' @param clutter A numeric matrix or data frame of clutter spectra. If it
#'   has column names, the columns matching the selected predictors are used;
#'   otherwise it must have one column per selected predictor, in the same
#'   order.
#'
#' @seealso [epo()], [predict.specproc_filter()]
#' @export
step_epo <- function(recipe, ..., role = NA, trained = FALSE, clutter = NULL,
                     num_comp = 2, res = NULL, columns = NULL, skip = FALSE,
                     id = recipes::rand_id("epo")) {
  rlang::check_installed("recipes")
  recipes::add_step(recipe, specproc_step_new(
    "epo", terms = rlang::enquos(...), role = role, trained = trained,
    clutter = clutter, num_comp = num_comp, res = res, columns = columns,
    skip = skip, id = id
  ))
}

#' @title Generalized Least Squares Weighting Recipe Step
#'
#' @description
#' `step_glsw()` creates a *specification* of a recipe step that down-weights
#' the directions of the selected predictors that vary in a clutter matrix,
#' with the generalized least squares weighting filter of [glsw()].
#'
#' @details
#' `clutter` holds differences between spectra that should be identical, such
#' as `x2 - x1` for the paired spectra of [glsw()]. It is centered, so the
#' filter equals `glsw(x1, x2, alpha = a)`, where the absolute `a` is `alpha`
#' times the largest eigenvalue of the centered clutter cross-product. Setting
#' `alpha` relative to that eigenvalue makes it independent of the scale of
#' the spectra, which simplifies tuning: small values remove the clutter
#' directions almost completely (like [step_epo()]), large values leave the
#' data nearly unchanged.
#'
#' The filter is stored in factored form, so its memory use grows with the
#' number of predictors rather than its square. The selected columns are
#' replaced by the filtered values (not centered). `alpha` can be tuned with
#' [tune::tune()], using [glsw_alpha()]. [tidy()][recipes::tidy.recipe]
#' returns the selected `terms`, `alpha` (relative) and `id`.
#'
#' @inherit step_osc return
#' @inheritParams step_epo
#' @param clutter A numeric matrix or data frame of difference spectra. It is
#'   matched to the selected predictors as in [step_epo()].
#' @param alpha A positive number: the weighting parameter, relative to the
#'   largest eigenvalue of the clutter. Default is 0.01.
#'
#' @seealso [glsw()], [step_y_gradient_glsw()]
#' @export
step_glsw <- function(recipe, ..., role = NA, trained = FALSE, clutter,
                      alpha = 0.01, res = NULL, columns = NULL, skip = FALSE,
                      id = recipes::rand_id("glsw")) {
  rlang::check_installed("recipes")
  if (missing(clutter)) {
    stop("'clutter' must be provided.", call. = FALSE)
  }
  recipes::add_step(recipe, specproc_step_new(
    "glsw", terms = rlang::enquos(...), role = role, trained = trained,
    clutter = clutter, alpha = alpha, res = res, columns = columns,
    skip = skip, id = id
  ))
}

#' @title y-Gradient Generalized Least Squares Weighting Recipe Step
#'
#' @description
#' `step_y_gradient_glsw()` creates a *specification* of a recipe step that
#' down-weights variation between samples with similar outcomes, with the
#' filter of [y_gradient_glsw()].
#'
#' @details
#' As in [step_glsw()], `alpha` is relative to the largest eigenvalue of the
#' (weighted) gradient matrix, and the filter is stored in factored form. The
#' filter uses the outcome, so it is estimated on training data only; the
#' outcome is not needed when new data are baked. The selected columns are
#' replaced by the filtered values (not centered). `alpha` can be tuned with
#' [tune::tune()], using [glsw_alpha()]. [tidy()][recipes::tidy.recipe]
#' returns the selected `terms`, `alpha` (relative) and `id`.
#'
#' @inherit step_osc return
#' @inheritParams step_osc
#' @inheritParams step_glsw
#' @param window An odd integer giving the width of the Savitzky-Golay window
#'   used to compute the gradients. Default is 5.
#'
#' @seealso [y_gradient_glsw()], [step_glsw()]
#' @export
step_y_gradient_glsw <- function(recipe, ..., role = NA, trained = FALSE,
                                 outcome = NULL, alpha = 0.01, window = 5,
                                 res = NULL, columns = NULL, skip = FALSE,
                                 id = recipes::rand_id("y_gradient_glsw")) {
  rlang::check_installed("recipes")
  recipes::add_step(recipe, specproc_step_new(
    "y_gradient_glsw", terms = rlang::enquos(...), role = role,
    trained = trained, outcome = rlang::enquos(outcome), alpha = alpha,
    window = window, res = res, columns = columns, skip = skip, id = id
  ))
}

#' @title Relative GLSW Weighting Parameter
#'
#' @description
#' A [dials][dials::dials-package] parameter for the `alpha` argument of
#' [step_glsw()] and [step_y_gradient_glsw()], on a log10 scale.
#'
#' @param range A two-element vector with the range of `alpha`, in log10
#'   units. Default is `c(-4, 0)`, i.e. 0.0001 to 1.
#' @param trans A transformation object from the scales package. Default is
#'   log10.
#'
#' @return A `quant_param` object.
#' @export
#'
#' @examplesIf rlang::is_installed("dials")
#' glsw_alpha()
#' dials::value_seq(glsw_alpha(), 5)
glsw_alpha <- function(range = c(-4, 0), trans = scales::transform_log10()) {
  rlang::check_installed("dials")
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(glsw_alpha = "GLSW weighting (relative)"),
    finalize = NULL
  )
}

# ---- internals ---------------------------------------------------------------

specproc_step_new <- function(subclass, terms, role, trained, skip, id, ...) {
  recipes::step(
    subclass = subclass, terms = terms, role = role, trained = trained,
    skip = skip, id = id, ...
  )
}

step_titles <- c(
  osc = "Orthogonal signal correction on ",
  direct_orthogonal = "Direct orthogonalization on ",
  direct_osc = "Direct orthogonal signal correction on ",
  projected_osc = "Projected OSC (OPLS filter) on ",
  epo = "External parameter orthogonalization on ",
  glsw = "GLSW filter on ",
  y_gradient_glsw = "y-gradient GLSW filter on "
)

# Selected predictors, checked to be numeric.
step_predictors <- function(x, training, info) {
  cols <- recipes::recipes_eval_select(x$terms, training, info)
  recipes::check_type(training[, cols], types = c("double", "integer"))
  unname(cols)
}

# Outcome name: the `outcome` argument, or the recipe's single outcome.
step_outcome <- function(x, training, info) {
  y_name <- if (is.character(x$outcome)) {
    x$outcome  # already resolved when the step was trained
  } else if (length(x$outcome) == 0 || rlang::quo_is_null(x$outcome[[1]])) {
    info$variable[info$role %in% "outcome"]
  } else {
    recipes::recipes_argument_select(x$outcome, training, info, single = FALSE)
  }
  if (length(y_name) != 1) {
    stop("`", class(x)[1], "()` needs a single outcome: specify `outcome`.", call. = FALSE)
  }
  if (!is.numeric(training[[y_name]])) {
    stop("The outcome `", y_name, "` must be numeric.", call. = FALSE)
  }
  y_name
}

# Clutter columns matching the selected predictors.
step_clutter <- function(clutter, cols) {
  clutter <- as_numeric_matrix(clutter, "clutter")
  if (!is.null(colnames(clutter))) {
    missing_cols <- setdiff(cols, colnames(clutter))
    if (length(missing_cols) > 0) {
      stop("'clutter' has no column for ", length(missing_cols), " selected predictor(s), e.g. `",
           missing_cols[1], "`.", call. = FALSE)
    }
    clutter <- clutter[, cols, drop = FALSE]
  } else if (ncol(clutter) != length(cols)) {
    stop("'clutter' must have one column per selected predictor (", length(cols), ").", call. = FALSE)
  }
  clutter
}

# Keeps only what predict() needs from a fitted filter.
strip_filter <- function(fit) {
  fit[c("correction", "clutter", "scores", "score", "angle", "R2", "newdata", "singular_values")] <- NULL
  fit
}

# Fits the filter of a step to the training predictors (matrix x) and outcome.
fit_step_filter <- function(x, xmat, y) {
  k <- x$num_comp
  if (!is.null(k)) check_count(k, "num_comp")
  opts <- x$options %||% list()
  switch(
    class(x)[1],
    step_osc = strip_filter(do.call(osc, c(list(xmat, y, method = x$method, ncomp = k), opts))),
    step_direct_orthogonal = strip_filter(do.call(direct_orthogonal, c(list(xmat, y, ncomp = k), opts))),
    step_direct_osc = strip_filter(do.call(direct_osc, c(list(xmat, y, ncomp = k), opts))),
    step_projected_osc = strip_filter(do.call(projected_osc, c(list(xmat, y, ncomp = k + 1), opts))),
    step_epo = {
      clutter <- if (is.null(x$clutter)) NULL else step_clutter(x$clutter, colnames(xmat))
      strip_filter(epo(xmat, ncomp = k, clutter = clutter))
    },
    step_glsw = {
      check_number(x$alpha, "alpha", lower = 0, lower_open = TRUE)
      d <- step_clutter(x$clutter, colnames(xmat))
      glsw_factor(scale(d, scale = FALSE), x$alpha)
    },
    step_y_gradient_glsw = {
      check_number(x$alpha, "alpha", lower = 0, lower_open = TRUE)
      check_count(x$window, "window", lower = 3)
      if (x$window %% 2 == 0) stop("'window' must be an odd integer.", call. = FALSE)
      g <- y_gradient(xmat, y, x$window)
      glsw_factor(g$w_i * g$x_diff, x$alpha)
    }
  )
}

prep_specproc_step <- function(x, training, info = NULL, ...) {
  cols <- step_predictors(x, training, info)
  supervised <- !class(x)[1] %in% c("step_epo", "step_glsw")
  y_name <- if (supervised) step_outcome(x, training, info) else NULL
  xmat <- as.matrix(training[, cols])
  storage.mode(xmat) <- "double"
  y <- if (supervised) training[[y_name]] else NULL
  if (anyNA(xmat) || anyNA(y)) {
    stop("`", class(x)[1], "()` does not handle missing values; impute or remove them first.",
         call. = FALSE)
  }
  x$res <- fit_step_filter(x, xmat, y)
  x$columns <- cols
  if (supervised) x$outcome <- y_name
  x$trained <- TRUE
  x
}

bake_specproc_step <- function(object, new_data, ...) {
  cols <- object$columns
  recipes::check_new_data(cols, object, new_data)
  if (length(cols) == 0) {
    return(new_data)
  }
  xmat <- as.matrix(new_data[, cols])
  storage.mode(xmat) <- "double"
  corrected <- if (inherits(object$res, "specproc_filter") || inherits(object$res, "o2pls")) {
    as.matrix(stats::predict(object$res, xmat))
  } else {
    apply_glsw_factor(xmat, object$res)
  }
  new_data[cols] <- as.data.frame(corrected)
  new_data
}

print_specproc_step <- function(x, width = max(20, options()$width - 30), ...) {
  title <- step_titles[[sub("^step_", "", class(x)[1])]]
  recipes::print_step(x$columns, x$terms, x$trained, title, width)
  invisible(x)
}

tidy_specproc_step <- function(x, ...) {
  terms <- if (recipes::is_trained(x)) x$columns else recipes::sel2char(x$terms)
  param <- if (class(x)[1] %in% c("step_glsw", "step_y_gradient_glsw")) "alpha" else "num_comp"
  value <- x[[param]]
  if (!is.numeric(value)) value <- NA_real_
  res <- tibble::tibble(terms = terms)
  res[[param]] <- rep(value, length(terms))
  res$id <- x$id
  res
}

tunable_specproc_step <- function(x, ...) {
  if (class(x)[1] %in% c("step_glsw", "step_y_gradient_glsw")) {
    tibble::tibble(
      name = "alpha",
      call_info = list(list(pkg = "specProc", fun = "glsw_alpha")),
      source = "recipe", component = class(x)[1], component_id = x$id
    )
  } else {
    tibble::tibble(
      name = "num_comp",
      call_info = list(list(pkg = "dials", fun = "num_comp", range = c(1L, 4L))),
      source = "recipe", component = class(x)[1], component_id = x$id
    )
  }
}

required_pkgs_specproc_step <- function(x, ...) {
  c("specProc")
}

# ---- S3 methods (registered when recipes / generics are loaded) ---------------

#' @exportS3Method recipes::prep
prep.step_osc <- prep_specproc_step
#' @exportS3Method recipes::prep
prep.step_direct_orthogonal <- prep_specproc_step
#' @exportS3Method recipes::prep
prep.step_direct_osc <- prep_specproc_step
#' @exportS3Method recipes::prep
prep.step_projected_osc <- prep_specproc_step
#' @exportS3Method recipes::prep
prep.step_epo <- prep_specproc_step
#' @exportS3Method recipes::prep
prep.step_glsw <- prep_specproc_step
#' @exportS3Method recipes::prep
prep.step_y_gradient_glsw <- prep_specproc_step

#' @exportS3Method recipes::bake
bake.step_osc <- bake_specproc_step
#' @exportS3Method recipes::bake
bake.step_direct_orthogonal <- bake_specproc_step
#' @exportS3Method recipes::bake
bake.step_direct_osc <- bake_specproc_step
#' @exportS3Method recipes::bake
bake.step_projected_osc <- bake_specproc_step
#' @exportS3Method recipes::bake
bake.step_epo <- bake_specproc_step
#' @exportS3Method recipes::bake
bake.step_glsw <- bake_specproc_step
#' @exportS3Method recipes::bake
bake.step_y_gradient_glsw <- bake_specproc_step

#' @export
print.step_osc <- print_specproc_step
#' @export
print.step_direct_orthogonal <- print_specproc_step
#' @export
print.step_direct_osc <- print_specproc_step
#' @export
print.step_projected_osc <- print_specproc_step
#' @export
print.step_epo <- print_specproc_step
#' @export
print.step_glsw <- print_specproc_step
#' @export
print.step_y_gradient_glsw <- print_specproc_step

#' @exportS3Method generics::tidy
tidy.step_osc <- tidy_specproc_step
#' @exportS3Method generics::tidy
tidy.step_direct_orthogonal <- tidy_specproc_step
#' @exportS3Method generics::tidy
tidy.step_direct_osc <- tidy_specproc_step
#' @exportS3Method generics::tidy
tidy.step_projected_osc <- tidy_specproc_step
#' @exportS3Method generics::tidy
tidy.step_epo <- tidy_specproc_step
#' @exportS3Method generics::tidy
tidy.step_glsw <- tidy_specproc_step
#' @exportS3Method generics::tidy
tidy.step_y_gradient_glsw <- tidy_specproc_step

#' @exportS3Method generics::tunable
tunable.step_osc <- tunable_specproc_step
#' @exportS3Method generics::tunable
tunable.step_direct_orthogonal <- tunable_specproc_step
#' @exportS3Method generics::tunable
tunable.step_direct_osc <- tunable_specproc_step
#' @exportS3Method generics::tunable
tunable.step_projected_osc <- tunable_specproc_step
#' @exportS3Method generics::tunable
tunable.step_epo <- tunable_specproc_step
#' @exportS3Method generics::tunable
tunable.step_glsw <- tunable_specproc_step
#' @exportS3Method generics::tunable
tunable.step_y_gradient_glsw <- tunable_specproc_step

#' @exportS3Method generics::required_pkgs
required_pkgs.step_osc <- required_pkgs_specproc_step
#' @exportS3Method generics::required_pkgs
required_pkgs.step_direct_orthogonal <- required_pkgs_specproc_step
#' @exportS3Method generics::required_pkgs
required_pkgs.step_direct_osc <- required_pkgs_specproc_step
#' @exportS3Method generics::required_pkgs
required_pkgs.step_projected_osc <- required_pkgs_specproc_step
#' @exportS3Method generics::required_pkgs
required_pkgs.step_epo <- required_pkgs_specproc_step
#' @exportS3Method generics::required_pkgs
required_pkgs.step_glsw <- required_pkgs_specproc_step
#' @exportS3Method generics::required_pkgs
required_pkgs.step_y_gradient_glsw <- required_pkgs_specproc_step
