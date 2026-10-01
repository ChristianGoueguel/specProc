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
#' The related steps [step_direct_orthogonal()], [step_direct_osc()],
#' [step_projected_osc()], [step_opls()] and [step_o2pls()] (which also
#' handles several outcomes) remove response-orthogonal variation with other
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
#' data(forageLIBS)
#' # potassium and the K I resonance lines
#' wl <- suppressWarnings(as.numeric(names(forageLIBS)))
#' dat <- forageLIBS[c(which(names(forageLIBS) == "K"), which(wl > 760 & wl < 780))]
#' rec <- recipe(K ~ ., data = dat[1:300, ]) |>
#'   step_osc(all_predictors(), method = "fearn", num_comp = 2)
#' prepped <- prep(rec)
#' tidy(prepped, number = 1)
#' # new spectra are corrected with the filter estimated on the training data
#' bake(prepped, new_data = dat[301:368, -1])
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
#' [direct_orthogonal()].
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
#' @param num_comp The number of orthogonal components to remove (`ncomp` of
#'   [projected_osc()]).
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

#' @title Orthogonal Projections to Latent Structures (OPLS) Recipe Step
#'
#' @description
#' `step_opls()` creates a *specification* of a recipe step that removes
#' `num_comp` orthogonal components from the selected predictors with the
#' OPLS model of [opls()]: the selected columns are replaced by the
#' OPLS-filtered data.
#'
#' @details
#' The filtered data are the same as those of [step_projected_osc()] with the
#' same `num_comp`. For Pareto scaling, add [step_pareto_scale()] before this
#' step. The model is fitted without cross-validation or permutation test,
#' which do not change the filter.
#' The number of orthogonal components is not selected automatically: tune
#' `num_comp` instead. As for [step_osc()], the filter uses the outcome and
#' is estimated on the training data only; the outcome is not needed when new
#' data are baked.
#'
#' # Tuning
#'
#' `num_comp` can be tuned with [tune::tune()]; its default range is
#' [dials::num_comp()] with values 1 to 4.
#'
#' # Tidying
#'
#' [tidy()][recipes::tidy.recipe] returns a tibble with columns `terms` (the
#' selected predictors), `num_comp` and `id`.
#'
#' @inherit step_osc return
#' @inheritParams step_osc
#' @param num_comp The number of orthogonal components to remove.
#' @param options A list of further arguments passed to [opls()]: `center`
#'   and `scale`.
#'
#' @seealso [opls()], [predict.specproc_opls()]
#' @export
#'
#' @examplesIf rlang::is_installed("recipes")
#' data(forageLIBS)
#' # potassium and the K I resonance lines
#' wl <- suppressWarnings(as.numeric(names(forageLIBS)))
#' dat <- forageLIBS[c(which(names(forageLIBS) == "K"), which(wl > 760 & wl < 780))]
#' # Pareto scaling, then the OPLS filter
#' rec <- recipes::recipe(K ~ ., data = dat[1:300, ]) |>
#'   step_pareto_scale(recipes::all_predictors()) |>
#'   step_opls(recipes::all_predictors(), num_comp = 2)
#' prepped <- recipes::prep(rec)
#' recipes::bake(prepped, new_data = dat[301:368, ])
step_opls <- function(recipe, ..., role = NA, trained = FALSE,
                      outcome = NULL, num_comp = 2, options = list(),
                      res = NULL, columns = NULL, skip = FALSE,
                      id = recipes::rand_id("opls")) {
  rlang::check_installed("recipes")
  recipes::add_step(recipe, specproc_step_new(
    "opls", terms = rlang::enquos(...), role = role,
    trained = trained, outcome = rlang::enquos(outcome), num_comp = num_comp,
    options = options, res = res, columns = columns, skip = skip, id = id
  ))
}

#' @title O2PLS Filter Recipe Step for One or Several Outcomes
#'
#' @description
#' `step_o2pls()` creates a *specification* of a recipe step that removes
#' `num_comp` outcome-orthogonal components from the selected predictors
#' with [o2pls()]. Unlike the other orthogonalization steps, the filter can
#' be estimated against several outcomes at once.
#'
#' @details
#' The joint (predictive) directions are the `joint_comp` dominant singular
#' vectors of \eqn{\textbf{Y}^T\textbf{X}}, and the removed components are the
#' systematic variation of the predictors orthogonal to them (Trygg and Wold,
#' 2003). Only the predictors are filtered: the outcomes are left unchanged,
#' so that models are fitted to, and assessed on, the measured values. (The
#' outcome-side filtering of [o2pls()], its `ny` argument, does not change the
#' filtered predictors.)
#'
#' With a single outcome and `joint_comp = 1`, the filtered data are those of
#' [step_opls()] and [step_projected_osc()] with the same `num_comp`.
#'
#' The selected columns are replaced by the filtered values, which are
#' centered (and scaled, if `options = list(scale = TRUE)`). The filter uses
#' the outcomes, so it is estimated on the training data only (on the
#' analysis set of each resample within [tune::tune_grid()]); the outcomes are
#' not needed when new data are baked.
#'
#' # Several outcomes
#'
#' Declare the outcomes in the recipe formula; a model that handles several
#' outcomes, such as [parsnip::pls()] with the mixOmics engine, then predicts
#' them all, with one column per outcome (`.pred_K`, `.pred_Ca`, ...):
#'
#' ```r
#' rec <- recipe(K + Ca ~ ., data = spectra) |>
#'   step_o2pls(all_predictors(), num_comp = 2, joint_comp = 2)
#' model <- parsnip::pls(num_comp = 2) |>
#'   set_mode("regression") |>
#'   set_engine("mixOmics", scale = FALSE)
#' fitted <- fit(workflow(rec, model), data = spectra)
#' predict(fitted, new_data = new_spectra)
#' ```
#'
#' Use `outcome` to estimate the filter against some of the outcomes only.
#'
#' # Tuning
#'
#' `num_comp` and `joint_comp` can be tuned with [tune::tune()] when the
#' recipe has a single outcome. Their default ranges are [dials::num_comp()]
#' with values 1 to 4 and 1 to 3; `joint_comp` cannot exceed the number of
#' outcomes.
#'
#' [tune::tune_grid()] does not support several outcomes. Instead, create
#' one workflow per outcome with the workflowsets package, and tune them all
#' with `workflowsets::workflow_map("tune_grid", ...)`. Each recipe has a
#' single outcome, for the model, but the filter can still be estimated
#' against all the outcomes: give the others a role of their own
#' (`"reference"` here), not needed to bake new data, and select them with
#' `outcome`. Each outcome gets its own number of filter and model components:
#'
#' ```r
#' outcomes <- c("K", "Ca", "Mg")
#' recipes <- purrr::map(outcomes, \(y) {
#'   recipe(reformulate(".", response = y), data = spectra) |>
#'     update_role(all_of(setdiff(outcomes, y)), new_role = "reference") |>
#'     update_role_requirements("reference", bake = FALSE) |>
#'     step_o2pls(all_predictors(), outcome = c(all_outcomes(), has_role("reference")),
#'                num_comp = tune("filter"), joint_comp = length(outcomes))
#' }) |>
#'   purrr::set_names(outcomes)
#' model <- parsnip::pls(num_comp = tune()) |>
#'   set_mode("regression") |>
#'   set_engine("mixOmics", scale = FALSE)
#'
#' wf_set <- workflow_set(preproc = recipes, models = list(pls = model))
#' res <- workflow_map(wf_set, "tune_grid", resamples = vfold_cv(spectra, v = 5),
#'                     grid = 10, metrics = metric_set(rmse), seed = 1)
#'
#' # best settings and final model of each outcome
#' fits <- purrr::map(purrr::set_names(res$wflow_id), \(id) {
#'   best <- select_best(extract_workflow_set_result(res, id), metric = "rmse")
#'   extract_workflow(res, id) |>
#'     finalize_workflow(best) |>
#'     fit(data = spectra)
#' })
#' purrr::map(fits, \(f) predict(f, new_data = new_spectra))
#' ```
#'
#' Select the outcomes by role (`all_outcomes()`, `has_role()`) rather than
#' with `all_of(outcomes)`, which refers to a variable outside the recipe.
#' To filter each outcome against itself only, drop the other outcomes from
#' its recipe instead; the step is then equivalent to [step_opls()]. Read
#' the results outcome by outcome: `workflowsets::rank_results()` would rank
#' workflows of different outcomes, whose RMSE are in different units.
#'
#' # Tidying
#'
#' [tidy()][recipes::tidy.recipe] returns a tibble with columns `terms` (the
#' selected predictors), `num_comp`, `joint_comp` and `id`.
#'
#' @inherit step_osc return
#' @inheritParams step_osc
#' @param outcome The outcome variables, as bare names or selectors. If
#'   `NULL` (default), all the outcomes of the recipe are used.
#' @param num_comp The number of outcome-orthogonal components to remove
#'   (`nx` in [o2pls()]).
#' @param joint_comp The number of joint (predictive) components (`ncomp` in
#'   [o2pls()]), at most the number of outcomes. Default is 1.
#' @param options A list of further arguments passed to [o2pls()]: `center`
#'   and `scale`.
#'
#' @references
#'    - Trygg, J., Wold, S., (2003).
#'      O2-PLS, a two-block (X–Y) latent variable regression (LVR) method with an integral OSC filter.
#'      J. Chemom. 17(1):53–64.
#'
#' @seealso [o2pls()], [predict.o2pls()][predict.specproc_filter]
#' @export
#'
#' @examplesIf rlang::is_installed("recipes")
#' data(forageLIBS)
#' # potassium, calcium and the spectral channels
#' dat <- forageLIBS[-c(1:2, 4:10, 12:14)]
#' rec <- recipes::recipe(K + Ca ~ ., data = dat[1:300, ]) |>
#'   step_o2pls(recipes::all_predictors(), num_comp = 2, joint_comp = 2)
#' prepped <- recipes::prep(rec)
#' recipes::tidy(prepped, number = 1)
#' dim(recipes::bake(prepped, new_data = dat[301:368, ]))
step_o2pls <- function(recipe, ..., role = NA, trained = FALSE,
                       outcome = NULL, num_comp = 2, joint_comp = 1, options = list(),
                       res = NULL, columns = NULL, skip = FALSE,
                       id = recipes::rand_id("o2pls")) {
  rlang::check_installed("recipes")
  recipes::add_step(recipe, specproc_step_new(
    "o2pls", terms = rlang::enquos(...), role = role,
    trained = trained, outcome = rlang::enquos(outcome), num_comp = num_comp,
    joint_comp = joint_comp, options = options, res = res, columns = columns,
    skip = skip, id = id
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
  opls = "OPLS filter on ",
  o2pls = "O2PLS filter on ",
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

# Outcome names: the `outcome` argument, or the recipe's outcomes. Steps
# other than step_o2pls() need a single outcome.
step_outcome <- function(x, training, info) {
  y_name <- if (is.character(x$outcome)) {
    x$outcome  # already resolved when the step was trained
  } else if (length(x$outcome) == 0 || rlang::quo_is_null(x$outcome[[1]])) {
    info$variable[info$role %in% "outcome"]
  } else {
    recipes::recipes_argument_select(x$outcome, training, info, single = FALSE)
  }
  if (length(y_name) == 0 || (length(y_name) > 1 && !inherits(x, "step_o2pls"))) {
    stop("`", class(x)[1], "()` needs a single outcome: specify `outcome`.", call. = FALSE)
  }
  for (nm in y_name) {
    if (!is.numeric(training[[nm]])) {
      stop("The outcome `", nm, "` must be numeric.", call. = FALSE)
    }
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
  fit[c("correction", "clutter", "scores", "score", "angle", "R2", "newdata", "singular_values",
        "x_scores", "orthoScores", "y_scores", "fitted", "correction_y")] <- NULL
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
    step_projected_osc = strip_filter(do.call(projected_osc, c(list(xmat, y, ncomp = k), opts))),
    step_o2pls = {
      check_count(x$joint_comp, "joint_comp")
      strip_filter(do.call(o2pls, c(list(xmat, y, ncomp = x$joint_comp, nx = k, ny = 0), opts)))
    },
    step_opls = strip_filter(do.call(opls, c(list(xmat, y, ncomp = k, crossval = 0, permutation = 0), opts))),
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
  y <- if (!supervised) NULL else if (length(y_name) > 1) as.matrix(training[y_name]) else training[[y_name]]
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
  if (inherits(x, "step_o2pls")) {
    joint <- if (is.numeric(x$joint_comp)) x$joint_comp else NA_real_
    res$joint_comp <- rep(joint, length(terms))
  }
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
  } else if (inherits(x, "step_o2pls")) {
    tibble::tibble(
      name = c("num_comp", "joint_comp"),
      call_info = list(
        list(pkg = "dials", fun = "num_comp", range = c(1L, 4L)),
        list(pkg = "dials", fun = "num_comp", range = c(1L, 3L))
      ),
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
prep.step_opls <- prep_specproc_step
#' @exportS3Method recipes::prep
prep.step_o2pls <- prep_specproc_step
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
bake.step_opls <- bake_specproc_step
#' @exportS3Method recipes::bake
bake.step_o2pls <- bake_specproc_step
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
print.step_opls <- print_specproc_step
#' @export
print.step_o2pls <- print_specproc_step
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
tidy.step_opls <- tidy_specproc_step
#' @exportS3Method generics::tidy
tidy.step_o2pls <- tidy_specproc_step
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
tunable.step_opls <- tunable_specproc_step
#' @exportS3Method generics::tunable
tunable.step_o2pls <- tunable_specproc_step
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
required_pkgs.step_opls <- required_pkgs_specproc_step
#' @exportS3Method generics::required_pkgs
required_pkgs.step_o2pls <- required_pkgs_specproc_step
#' @exportS3Method generics::required_pkgs
required_pkgs.step_epo <- required_pkgs_specproc_step
#' @exportS3Method generics::required_pkgs
required_pkgs.step_glsw <- required_pkgs_specproc_step
#' @exportS3Method generics::required_pkgs
required_pkgs.step_y_gradient_glsw <- required_pkgs_specproc_step
