#' @title SIMCA Classification Model (parsnip)
#'
#' @author Christian L. Goueguel
#'
#' @description
#' `simca()` defines a soft independent modeling of class analogy (SIMCA)
#' classification model for tidymodels: a principal component model of each
#' class, and the assignment of each observation to the closest class. Its
#' engine `"rsimca"` (the default) is the robust SIMCA of [rsimca()], whose
#' class models resist outlying spectra of the training data.
#'
#' @details
#' `simca()` is a parsnip model specification: it is fitted with
#' [parsnip::fit()] or in a workflow, and can be tuned and resampled like any
#' parsnip model. It needs the parsnip package, and the `"rsimca"` engine is
#' available once parsnip and specProc are both loaded.
#'
#' # Main arguments
#'
#' - `num_comp` is the number of components of the robust PCA of each class
#'   (`ncomp` of [rsimca()]), the same for all the classes. With `NULL` (the
#'   default), each class gets the number chosen by [robpca()] from the
#'   proportion of variance explained.
#' - `gamma` is the weight of the orthogonal distances, against the score
#'   distances, in the classification rule (`gamma` of [rsimca()]). With
#'   `NULL` (the default), it is 0.5.
#'
#' Both can be tuned with [tune::tune()], using [dials::num_comp()] and
#' [simca_gamma()].
#'
#' # Engine arguments
#'
#' The other arguments of [rsimca()] are set with [parsnip::set_engine()]:
#' `squared`, `kmax`, `alpha`, `var_explained`, `prior`, `ndir` and
#' `nsamp`.
#'
#' # Predictions
#'
#' [predict()][parsnip::predict.model_fit] gives the classes
#' (`type = "class"`), or with `type = "raw"` the combined distances to the
#' classes and whether each observation is outlying for all of them
#' ([predict.specproc_rsimca()]). SIMCA does not give class probabilities:
#' when tuning, use metrics of the predicted classes, such as
#' `yardstick::metric_set(yardstick::accuracy, yardstick::kap)`.
#'
#' The fitted [rsimca()] model is returned by
#' [parsnip::extract_fit_engine()]. [rsimca()] uses random directions and
#' subsets, so use [set.seed()] before fitting for reproducible results.
#'
#' @param mode The type of model: `"classification"`, the only one.
#' @param num_comp The number of components of the PCA model of each class,
#'   or `NULL` (default) to let the engine choose them.
#' @param gamma The weight of the orthogonal distances in the classification
#'   rule, between 0 and 1, or `NULL` (default) for 0.5.
#' @param engine The computational engine: `"rsimca"` (default).
#' @param object A `simca` model specification.
#' @param parameters A one-row tibble or named list of main parameters to
#'   update, such as those returned by [tune::select_best()].
#' @param fresh If `TRUE`, the arguments replace those of `object`; if
#'   `FALSE` (default), they update them.
#' @param ... Not used.
#'
#' @return A model specification of classes `simca` and `model_spec`.
#'
#' @seealso [rsimca()], [predict.specproc_rsimca()], [simca_gamma()]
#' @export
#'
#' @examplesIf rlang::is_installed("parsnip")
#' library(parsnip)
#' data(forageLIBS)
#' wl <- suppressWarnings(as.numeric(names(forageLIBS)))
#' dat <- forageLIBS[which(wl > 380 & wl < 430)]
#' # forage samples with low and high calcium
#' dat$level <- cut(forageLIBS$Ca, c(-Inf, 0.6, Inf), labels = c("low", "high"))
#' spec <- simca(num_comp = 3, gamma = 0.5) |>
#'   set_engine("rsimca", alpha = 0.75)
#' spec
#' set.seed(1)
#' fit <- fit(spec, level ~ ., data = dat[1:300, ])
#' table(predict(fit, dat[301:368, ])$.pred_class, dat$level[301:368])
#' head(predict(fit, dat[301:368, ], type = "raw"))
simca <- function(mode = "classification", num_comp = NULL, gamma = NULL, engine = "rsimca") {
  rlang::check_installed("parsnip")
  if (!identical(mode, "classification")) {
    stop("`mode` must be \"classification\" for SIMCA models.", call. = FALSE)
  }
  args <- list(num_comp = rlang::enquo(num_comp), gamma = rlang::enquo(gamma))
  parsnip::new_model_spec("simca", args = args, eng_args = NULL, mode = mode,
                          user_specified_mode = !missing(mode), method = NULL,
                          engine = engine, user_specified_engine = !missing(engine))
}

#' @rdname simca
#' @exportS3Method stats::update
update.simca <- function(object, parameters = NULL, num_comp = NULL, gamma = NULL,
                         fresh = FALSE, ...) {
  args <- list(num_comp = rlang::enquo(num_comp), gamma = rlang::enquo(gamma))
  parsnip::update_spec(object = object, parameters = parameters, args_enquo_list = args,
                       fresh = fresh, cls = "simca", ...)
}

#' @title SIMCA Distance Weighting Parameter
#'
#' @description
#' A [dials][dials::dials-package] parameter for the `gamma` argument of the
#' [simca()] parsnip model (and of [rsimca()]): the weight of the orthogonal
#' distances, against the score distances, in the classification rule.
#'
#' @param range A two-element vector with the range of `gamma`. Default is
#'   `c(0, 1)`.
#' @param trans A transformation object from the scales package, or `NULL`
#'   (default) for none.
#'
#' @return A `quant_param` object.
#' @seealso [simca()], [rsimca()]
#' @export
#'
#' @examplesIf rlang::is_installed("dials")
#' simca_gamma()
#' dials::value_seq(simca_gamma(), 5)
simca_gamma <- function(range = c(0, 1), trans = NULL) {
  rlang::check_installed("dials")
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(simca_gamma = "Weight of the orthogonal distances"),
    finalize = NULL
  )
}

#' @export
print.simca <- function(x, ...) {
  parsnip::print_model_spec(x, desc = "SIMCA", ...)
}

# Registers the simca model of parsnip and its "rsimca" engine, once (see
# register_pls_rsimpls()).
register_simca_rsimca <- function() {
  env <- parsnip::get_model_env()
  if (!"simca" %in% env$models) {
    parsnip::set_new_model("simca")
    parsnip::set_model_mode("simca", "classification")
  }
  if ("rsimca" %in% env$simca$engine) {
    return(invisible(FALSE))
  }
  parsnip::set_model_engine("simca", mode = "classification", eng = "rsimca")
  parsnip::set_dependency("simca", eng = "rsimca", pkg = "specProc", mode = "classification")
  parsnip::set_model_arg(
    model = "simca", eng = "rsimca", parsnip = "num_comp", original = "ncomp",
    func = list(pkg = "dials", fun = "num_comp"), has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "simca", eng = "rsimca", parsnip = "gamma", original = "gamma",
    func = list(pkg = "specProc", fun = "simca_gamma"), has_submodel = FALSE
  )
  parsnip::set_fit(
    model = "simca", eng = "rsimca", mode = "classification",
    value = list(interface = "matrix", data = c(x = "x", y = "group"),
                 protect = c("x", "group"), func = c(pkg = "specProc", fun = "rsimca"),
                 defaults = list())
  )
  parsnip::set_encoding(
    model = "simca", eng = "rsimca", mode = "classification",
    options = list(predictor_indicators = "traditional", compute_intercept = TRUE,
                   remove_intercept = TRUE, allow_sparse_x = FALSE)
  )
  for (type in c("class", "raw")) {
    parsnip::set_pred(
      model = "simca", eng = "rsimca", mode = "classification", type = type,
      value = list(pre = NULL, post = NULL, func = c(fun = "predict"),
                   args = list(object = quote(object$fit), newdata = quote(new_data),
                               type = if (type == "class") "class" else "distances"))
    )
  }
  invisible(TRUE)
}
