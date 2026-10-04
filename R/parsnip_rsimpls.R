#' @title Robust PLS Regression in tidymodels (parsnip Engine)
#'
#' @author Christian L. Goueguel
#'
#' @description
#' specProc adds the `"rsimpls"` engine to [parsnip::pls()], which fits the
#' robust PLS regression of [rsimpls()]: outlying spectra and wrong reference
#' values have little influence on the model. It can be tuned, resampled and
#' combined with recipes like any parsnip model.
#'
#' @details
#' The engine is available once parsnip and specProc are both loaded, in
#' either order. It is for regression only, with one or several outcomes
#' (for example `cbind(y1, y2) ~ .`).
#'
#' # Tuning parameters
#'
#' - `num_comp` (`ncomp` of [rsimpls()]): the number of PLS components. It
#'   has no default and must be given (or tuned). It can be tuned with
#'   [tune::tune()], using [dials::num_comp()].
#' - `predictor_prop`, the proportion of predictors of sparse PLS, is not
#'   used by this engine.
#'
#' The models with 1 to `kmax` components all come from the same ROBPCA fit
#' (see [rsimpls()]), so [parsnip::multi_predict()] gives the predictions of
#' all of them from one fit, and [tune::tune_grid()] fits a single model per
#' resample for a grid of `num_comp`. The predictions of a model with fewer
#' components are those of a separate fit with the same `kmax`, as long as
#' `kmax` is at least the largest `num_comp`.
#'
#' # Engine arguments
#'
#' The other arguments of [rsimpls()] are set with [parsnip::set_engine()]:
#' `kmax`, the largest number of components (default 10), `alpha`, the
#' fraction of observations assumed to be regular (default 0.75), and
#' `ndir` and `nsamp` of the ROBPCA step.
#'
#' # Preprocessing and randomness
#'
#' The predictors are centered robustly by the model, and are not scaled:
#' scale them beforehand if their units differ. Factor predictors are
#' converted to indicator variables. [rsimpls()] uses random directions and
#' subsets, so use [set.seed()] before fitting for reproducible results.
#'
#' # Outlier diagnostics
#'
#' The fitted [rsimpls()] model is returned by
#' [parsnip::extract_fit_engine()], for its outlier maps
#' ([plot_outlier_map()]) and distances.
#'
#' @name pls_rsimpls
#' @seealso [rsimpls()], [step_rsimpls()], [parsnip::pls()]
#'
#' @examplesIf rlang::is_installed("parsnip")
#' library(parsnip)
#' data(forageLIBS)
#' # calcium and the Ca II and Ca I lines
#' wl <- suppressWarnings(as.numeric(names(forageLIBS)))
#' dat <- forageLIBS[c(which(names(forageLIBS) == "Ca"), which(wl > 380 & wl < 430))]
#' spec <- pls(num_comp = 4) |>
#'   set_engine("rsimpls") |>
#'   set_mode("regression")
#' set.seed(1)
#' fit <- fit(spec, Ca ~ ., data = dat[1:300, ])
#' predict(fit, dat[301:368, ])
#' # predictions with 1 to 4 components
#' multi_predict(fit, dat[301:303, ], num_comp = 1:4)$.pred[[1]]
#' plot_outlier_map(extract_fit_engine(fit))
NULL

# Registers the "rsimpls" engine of parsnip::pls(), once. parsnip is
# suggested: .onLoad() calls it when parsnip is loaded, and otherwise sets a
# hook to call it when parsnip is loaded.
register_pls_rsimpls <- function() {
  env <- parsnip::get_model_env()
  if (!"pls" %in% env$models || "rsimpls" %in% env$pls$engine) {
    return(invisible(FALSE))
  }
  parsnip::set_model_engine("pls", mode = "regression", eng = "rsimpls")
  parsnip::set_dependency("pls", eng = "rsimpls", pkg = "specProc", mode = "regression")
  parsnip::set_model_arg(
    model = "pls", eng = "rsimpls", parsnip = "num_comp", original = "ncomp",
    func = list(pkg = "dials", fun = "num_comp"), has_submodel = TRUE
  )
  parsnip::set_fit(
    model = "pls", eng = "rsimpls", mode = "regression",
    value = list(interface = "matrix", protect = c("x", "y"),
                 func = c(pkg = "specProc", fun = "rsimpls"), defaults = list())
  )
  parsnip::set_encoding(
    model = "pls", eng = "rsimpls", mode = "regression",
    options = list(predictor_indicators = "traditional", compute_intercept = TRUE,
                   remove_intercept = TRUE, allow_sparse_x = FALSE)
  )
  parsnip::set_pred(
    model = "pls", eng = "rsimpls", mode = "regression", type = "numeric",
    value = list(pre = NULL, post = NULL, func = c(fun = "predict"),
                 args = list(object = quote(object$fit), newdata = quote(new_data)))
  )
  invisible(TRUE)
}

# Predictions of a parsnip rsimpls fit for several numbers of components: a
# tibble with a list column `.pred`, holding for each new observation a
# tibble with `num_comp` and the predictions (`.pred`, or `.pred_<outcome>`
# with several outcomes), as parsnip::multi_predict() returns. The class
# starts with "_", so it is quoted in the NAMESPACE directive.
#' @rawNamespace S3method(parsnip::multi_predict, "_specproc_rsimpls")
multi_predict._specproc_rsimpls <- function(object, new_data, type = NULL, num_comp = NULL, ...) {
  if (any(names(rlang::enquos(...)) == "newdata")) {
    stop("Did you mean to use `new_data` instead of `newdata`?", call. = FALSE)
  }
  if (!is.null(type) && !identical(type, "numeric")) {
    stop("`type` must be \"numeric\" for robust PLS regression.", call. = FALSE)
  }
  fit <- object$fit
  num_comp <- sort(unique(num_comp %||% fit$ncomp))
  new_data <- parsnip::prepare_data(object, new_data)
  preds <- lapply(num_comp, function(k) as.matrix(stats::predict(fit, new_data, ncomp = k)))
  nms <- if (ncol(preds[[1]]) == 1) ".pred" else paste0(".pred_", colnames(fit$coefficients))
  # one array: observations x outcomes x numbers of components
  k <- length(num_comp)
  values <- array(unlist(preds), c(nrow(preds[[1]]), ncol(preds[[1]]), k))
  out <- lapply(seq_len(dim(values)[1]), function(i) {
    res <- as.data.frame(t(matrix(values[i, , ], ncol = k)))
    names(res) <- nms
    tibble::add_column(tibble::as_tibble(res), num_comp = as.integer(num_comp), .before = 1)
  })
  tibble::tibble(.pred = out)
}
