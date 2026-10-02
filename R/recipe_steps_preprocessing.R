# Recipe steps for spectral preprocessing (tidymodels): baseline correction,
# SNV, MSC, EMSC, Pareto and Poisson scaling. See recipe_steps.R for the
# shared helpers and the orthogonalization steps.
#
# baseline correction and SNV work on each spectrum alone, so prep() only
# records the selected columns. MSC and EMSC estimate a reference spectrum,
# and Pareto and Poisson scaling estimate column scales, on the training data;
# bake() applies them unchanged to new data.

#' @title Baseline Correction Recipe Step
#'
#' @author Christian L. Goueguel
#'
#' @description
#' `step_baseline()` creates a *specification* of a recipe step that
#' subtracts a fitted baseline from each spectrum, with [baseline_arpls()],
#' [baseline_als()] or [baseline_lsp()].
#'
#' @details
#' The selected columns, in the order in which they appear in the data, form
#' one spectrum per row, so select all the channels of the spectrum and only
#' them. The baseline is fitted to each spectrum separately, so nothing is
#' estimated from the training data and the step gives the same result
#' whether it is applied before or after the data are split. The selected
#' columns are replaced by the corrected values.
#'
#' # Tuning
#'
#' `lambda` (methods `"arpls"` and `"als"`) can be tuned with [tune::tune()],
#' using [baseline_lambda()], and `degree` (method `"lsp"`) using
#' [dials::degree_int()] with values 1 to 8.
#'
#' # Tidying
#'
#' [tidy()][recipes::tidy.recipe] returns a tibble with columns `terms`,
#' `method` and `id`.
#'
#' @inheritParams step_osc
#' @param method The baseline algorithm: `"arpls"` (default), `"als"` or
#'   `"lsp"`.
#' @param lambda The smoothing parameter of the `"arpls"` and `"als"`
#'   methods. Default is 1000.
#' @param degree The polynomial degree of the `"lsp"` method. Default is 4.
#' @param options A list of further arguments passed to the baseline
#'   function: `ratio` and `max.iter` for [baseline_arpls()], `p` and
#'   `max.iter` for [baseline_als()], `tol` and `max.iter` for
#'   [baseline_lsp()].
#' @param columns The names of the selected columns, stored once the step has
#'   been trained.
#'
#' @return An updated version of `recipe` with the new step added to the
#'   sequence of existing steps.
#'
#' @seealso [baseline_arpls()], [baseline_als()], [baseline_lsp()]
#' @export
#'
#' @examplesIf rlang::is_installed("recipes")
#' library(recipes)
#' data(forageLIBS)
#' spectra <- forageLIBS[c(2, 15:406)]  # sample id and spectral channels
#'
#' rec <- recipe(~ ., data = spectra[1:16, ]) |>
#'   update_role(Sample, new_role = "id") |>
#'   step_baseline(all_predictors(), method = "arpls", lambda = 1e5) |>
#'   step_snv(all_predictors())
#' prepped <- prep(rec)
#' bake(prepped, new_data = spectra[17:24, ])[, 1:6]
#'
#' # five spectra between 240 and 300 nm, before and after the baseline correction
#' wl <- suppressWarnings(as.numeric(names(forageLIBS)))
#' region <- forageLIBS[1:5, c(2, which(wl > 240 & wl < 300))]
#' corrected <- recipe(~ ., data = region) |>
#'   update_role(Sample, new_role = "id") |>
#'   step_baseline(all_predictors(), method = "arpls", lambda = 1e5) |>
#'   prep() |>
#'   bake(new_data = NULL)
#' plot_spectra(region, id = Sample) +
#'   ggplot2::coord_cartesian(ylim = c(500, 3000))
#' plot_spectra(corrected, id = Sample) +
#'   ggplot2::coord_cartesian(ylim = c(-200, 2500))
#'
step_baseline <- function(recipe, ..., role = NA, trained = FALSE,
                          method = "arpls", lambda = 1e3, degree = 4,
                          options = list(), columns = NULL, skip = FALSE,
                          id = recipes::rand_id("baseline")) {
  rlang::check_installed("recipes")
  method <- match.arg(method, c("arpls", "als", "lsp"))
  recipes::add_step(recipe, specproc_step_new(
    "baseline", terms = rlang::enquos(...), role = role, trained = trained,
    method = method, lambda = lambda, degree = degree, options = options,
    columns = columns, skip = skip, id = id
  ))
}

#' @title Standard Normal Variate Recipe Step
#'
#' @description
#' `step_snv()` creates a *specification* of a recipe step that centers and
#' scales each spectrum by its own mean and standard deviation, with [snv()].
#'
#' @details
#' The selected columns form one spectrum per row; select all the channels
#' of the spectrum and only them. Nothing is estimated from the training
#' data. The selected columns are replaced by the SNV-transformed values.
#' [tidy()][recipes::tidy.recipe] returns the selected `terms` and `id`.
#'
#' @inherit step_baseline return
#' @inheritParams step_baseline
#'
#' @seealso [snv()]
#' @export
#'
#' @examplesIf rlang::is_installed("recipes")
#' data(forageLIBS)
#' # potassium and the K I resonance lines
#' wl <- suppressWarnings(as.numeric(names(forageLIBS)))
#' dat <- forageLIBS[c(which(names(forageLIBS) == "K"), which(wl > 760 & wl < 780))]
#' rec <- recipes::recipe(K ~ ., data = dat[1:300, ]) |>
#'   step_snv(recipes::all_predictors())
#' prepped <- recipes::prep(rec)
#' recipes::bake(prepped, new_data = dat[301:368, ])
step_snv <- function(recipe, ..., role = NA, trained = FALSE, columns = NULL,
                     skip = FALSE, id = recipes::rand_id("snv")) {
  rlang::check_installed("recipes")
  recipes::add_step(recipe, specproc_step_new(
    "snv", terms = rlang::enquos(...), role = role, trained = trained,
    columns = columns, skip = skip, id = id
  ))
}

#' @title Multiplicative Scatter Correction Recipe Step
#'
#' @description
#' `step_msc()` creates a *specification* of a recipe step that corrects
#' each spectrum for multiplicative and additive effects relative to a
#' reference spectrum, with [msc()].
#'
#' @details
#' The reference spectrum (the median or mean of the training spectra) is
#' estimated when the recipe is prepped, and new spectra are corrected
#' against it when they are baked. The selected columns form one spectrum per
#' row. They are replaced by the corrected values. [tidy()][recipes::tidy.recipe]
#' returns the selected `terms`, the `reference` value of each (`NA` before
#' the step is trained) and `id`.
#'
#' @inherit step_baseline return
#' @inheritParams step_baseline
#' @param robust A logical: use the median (`TRUE`, default) or the mean of
#'   the training spectra as the reference.
#' @param drop.offset A logical: remove the additive offset (`TRUE`,
#'   default) or only the multiplicative effect.
#' @param window An optional list of column index vectors (within the
#'   selected columns) for piecewise MSC. See [msc()].
#' @param reference The reference spectrum, stored once the step has been
#'   trained.
#'
#' @seealso [msc()], [step_emsc()]
#' @export
#'
#' @examplesIf rlang::is_installed("recipes")
#' data(forageLIBS)
#' # potassium and the K I resonance lines
#' wl <- suppressWarnings(as.numeric(names(forageLIBS)))
#' dat <- forageLIBS[c(which(names(forageLIBS) == "K"), which(wl > 760 & wl < 780))]
#' rec <- recipes::recipe(K ~ ., data = dat[1:300, ]) |>
#'   step_msc(recipes::all_predictors())
#' prepped <- recipes::prep(rec)
#' recipes::bake(prepped, new_data = dat[301:368, ])
step_msc <- function(recipe, ..., role = NA, trained = FALSE, robust = TRUE,
                     drop.offset = TRUE, window = NULL, reference = NULL,
                     columns = NULL, skip = FALSE, id = recipes::rand_id("msc")) {
  rlang::check_installed("recipes")
  recipes::add_step(recipe, specproc_step_new(
    "msc", terms = rlang::enquos(...), role = role, trained = trained,
    robust = robust, drop.offset = drop.offset, window = window,
    reference = reference, columns = columns, skip = skip, id = id
  ))
}

#' @title Extended Multiplicative Signal Correction Recipe Step
#'
#' @description
#' `step_emsc()` creates a *specification* of a recipe step that corrects
#' each spectrum for multiplicative effects, a polynomial baseline and,
#' optionally, known interferents, with [emsc()].
#'
#' @details
#' The reference spectrum is estimated from the training spectra when the
#' recipe is prepped, and new spectra are corrected with the same reference,
#' polynomials and interferents when they are baked. The selected columns
#' form one spectrum per row and are replaced by the corrected values.
#'
#' `degree` can be tuned with [tune::tune()], using [dials::degree_int()]
#' with values 0 to 4. [tidy()][recipes::tidy.recipe] returns the selected
#' `terms`, the `reference` value of each (`NA` before the step is trained)
#' and `id`.
#'
#' @inherit step_baseline return
#' @inheritParams step_msc
#' @param degree The degree of the polynomial baseline. Default is 2.
#' @param interferents An optional numeric matrix or data frame of interferent
#'   spectra, one per row. If it has column names, the columns matching the
#'   selected predictors are used; otherwise it must have one column per
#'   selected predictor, in the same order.
#' @param wavelength An optional numeric vector of wavelengths, one per
#'   selected column, used to build the polynomials. If `NULL`, the column
#'   names are used when they are numeric, and the column positions
#'   otherwise.
#' @param res The fitted EMSC model, stored once the step has been trained.
#'
#' @seealso [emsc()], [step_msc()]
#' @export
#'
#' @examplesIf rlang::is_installed("recipes")
#' data(forageLIBS)
#' # potassium and the K I resonance lines
#' wl <- suppressWarnings(as.numeric(names(forageLIBS)))
#' dat <- forageLIBS[c(which(names(forageLIBS) == "K"), which(wl > 760 & wl < 780))]
#' rec <- recipes::recipe(K ~ ., data = dat[1:300, ]) |>
#'   step_emsc(recipes::all_predictors(), degree = 2)
#' prepped <- recipes::prep(rec)
#' recipes::bake(prepped, new_data = dat[301:368, ])
step_emsc <- function(recipe, ..., role = NA, trained = FALSE, degree = 2,
                      interferents = NULL, wavelength = NULL, robust = TRUE,
                      res = NULL, columns = NULL, skip = FALSE,
                      id = recipes::rand_id("emsc")) {
  rlang::check_installed("recipes")
  recipes::add_step(recipe, specproc_step_new(
    "emsc", terms = rlang::enquos(...), role = role, trained = trained,
    degree = degree, interferents = interferents, wavelength = wavelength,
    robust = robust, res = res, columns = columns, skip = skip, id = id
  ))
}

#' @title Pareto Scaling Recipe Step
#'
#' @description
#' `step_pareto_scale()` creates a *specification* of a recipe step that
#' divides each selected column by the square root of its standard
#' deviation, as [pareto_scale()] does.
#'
#' @details
#' The standard deviations are estimated from the training data and applied
#' unchanged to new data. Columns with zero standard deviation are not
#' scaled. The data are not centered; add [recipes::step_center()] if needed.
#' [tidy()][recipes::tidy.recipe] returns the selected `terms`, the `scale`
#' each column is divided by (`NA` before the step is trained) and `id`.
#'
#' @inherit step_baseline return
#' @inheritParams step_baseline
#' @param scales The scale of each column, stored once the step has been
#'   trained.
#'
#' @seealso [pareto_scale()], [step_poisson_scale()]
#' @export
#'
#' @examplesIf rlang::is_installed("recipes")
#' data(forageLIBS)
#' # potassium and the K I resonance lines
#' wl <- suppressWarnings(as.numeric(names(forageLIBS)))
#' dat <- forageLIBS[c(which(names(forageLIBS) == "K"), which(wl > 760 & wl < 780))]
#' rec <- recipes::recipe(K ~ ., data = dat[1:300, ]) |>
#'   step_pareto_scale(recipes::all_predictors())
#' prepped <- recipes::prep(rec)
#' recipes::bake(prepped, new_data = dat[301:368, ])
step_pareto_scale <- function(recipe, ..., role = NA, trained = FALSE,
                              scales = NULL, columns = NULL, skip = FALSE,
                              id = recipes::rand_id("pareto_scale")) {
  rlang::check_installed("recipes")
  recipes::add_step(recipe, specproc_step_new(
    "pareto_scale", terms = rlang::enquos(...), role = role,
    trained = trained, scales = scales, columns = columns, skip = skip, id = id
  ))
}

#' @title Poisson Scaling Recipe Step
#'
#' @description
#' `step_poisson_scale()` creates a *specification* of a recipe step that
#' divides each selected column by the square root of its mean plus an
#' offset, as [poisson_scale()] does (column mode).
#'
#' @details
#' Poisson scaling suits count data, whose variance grows with the mean. The
#' scales \eqn{\sqrt{\bar{x}_j + c}} are estimated from the training data,
#' where the offset \eqn{c} is `offset` percent of the largest column mean,
#' and are applied unchanged to new data. [tidy()][recipes::tidy.recipe]
#' returns the selected `terms`, the `scale` each column is divided by (`NA`
#' before the step is trained) and `id`.
#'
#' @inherit step_baseline return
#' @inheritParams step_pareto_scale
#' @param offset The offset, in percent of the largest column mean. Default
#'   is 3.
#'
#' @seealso [poisson_scale()], [step_pareto_scale()]
#' @export
#'
#' @examplesIf rlang::is_installed("recipes")
#' data(forageLIBS)
#' # potassium and the K I resonance lines
#' wl <- suppressWarnings(as.numeric(names(forageLIBS)))
#' dat <- forageLIBS[c(which(names(forageLIBS) == "K"), which(wl > 760 & wl < 780))]
#' rec <- recipes::recipe(K ~ ., data = dat[1:300, ]) |>
#'   step_poisson_scale(recipes::all_predictors())
#' prepped <- recipes::prep(rec)
#' recipes::bake(prepped, new_data = dat[301:368, ])
step_poisson_scale <- function(recipe, ..., role = NA, trained = FALSE,
                               offset = 3, scales = NULL, columns = NULL,
                               skip = FALSE, id = recipes::rand_id("poisson_scale")) {
  rlang::check_installed("recipes")
  recipes::add_step(recipe, specproc_step_new(
    "poisson_scale", terms = rlang::enquos(...), role = role,
    trained = trained, offset = offset, scales = scales, columns = columns,
    skip = skip, id = id
  ))
}

#' @title Baseline Smoothness Parameter
#'
#' @description
#' A [dials][dials::dials-package] parameter for the `lambda` argument of
#' [step_baseline()], on a log10 scale.
#'
#' @param range A two-element vector with the range of `lambda`, in log10
#'   units. Default is `c(2, 8)`, i.e. 100 to 10^8.
#' @param trans A transformation object from the scales package. Default is
#'   log10.
#'
#' @return A `quant_param` object.
#' @export
#'
#' @examplesIf rlang::is_installed("dials")
#' baseline_lambda()
baseline_lambda <- function(range = c(2, 8), trans = scales::transform_log10()) {
  rlang::check_installed("dials")
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(baseline_lambda = "Baseline smoothness"),
    finalize = NULL
  )
}

# ---- internals ---------------------------------------------------------------

preprocessing_titles <- c(
  baseline = "Baseline correction on ",
  snv = "Standard normal variate on ",
  msc = "Multiplicative scatter correction on ",
  emsc = "Extended multiplicative signal correction on ",
  pareto_scale = "Pareto scaling on ",
  poisson_scale = "Poisson scaling on "
)

step_matrix <- function(data, cols) {
  rlang::check_installed("recipes")
  x <- as.matrix(data[, cols])
  storage.mode(x) <- "double"
  x
}

prep_preprocessing_step <- function(x, training, info = NULL, ...) {
  cols <- step_predictors(x, training, info)
  xmat <- step_matrix(training, cols)
  if (length(cols) > 0 && anyNA(xmat)) {
    stop("`", class(x)[1], "()` does not handle missing values; impute or remove them first.",
         call. = FALSE)
  }
  switch(
    class(x)[1],
    step_msc = {
      x$reference <- msc(xmat, robust = x$robust, window = x$window, drop.na = FALSE)$reference
    },
    step_emsc = {
      check_count(x$degree, "degree", lower = 0)
      interferents <- if (is.null(x$interferents)) NULL else step_clutter(x$interferents, cols)
      fit <- emsc(xmat, degree = x$degree, interferents = interferents,
                  wavelength = x$wavelength, robust = x$robust)
      fit[c("correction", "coefficients")] <- NULL
      x$res <- fit
    },
    step_pareto_scale = {
      s <- apply(xmat, 2, stats::sd)
      s[!is.finite(s) | s == 0] <- 1
      x$scales <- stats::setNames(sqrt(s), cols)
    },
    step_poisson_scale = {
      check_number(x$offset, "offset", lower = 0)
      sc <- poisson_scale(xmat, drop.na = FALSE, options = list(offset = x$offset, mode = 1))$sc
      x$scales <- stats::setNames(sc, cols)
    },
    step_baseline = {
      if (x$method == "lsp") check_count(x$degree, "degree") else check_number(x$lambda, "lambda", lower = 0, lower_open = TRUE)
    }
  )
  x$columns <- cols
  x$trained <- TRUE
  x
}

bake_preprocessing_step <- function(object, new_data, ...) {
  cols <- object$columns
  recipes::check_new_data(cols, object, new_data)
  if (length(cols) == 0) {
    return(new_data)
  }
  xmat <- step_matrix(new_data, cols)
  opts <- object$options %||% list()
  corrected <- switch(
    class(object)[1],
    step_baseline = switch(
      object$method,
      arpls = do.call(baseline_arpls, c(list(xmat, lambda = object$lambda), opts))$correction,
      als = do.call(baseline_als, c(list(xmat, lambda = object$lambda), opts))$correction,
      lsp = do.call(baseline_lsp, c(list(xmat, degree = object$degree), opts))$correction
    ),
    step_snv = snv(xmat, drop.na = FALSE)$correction,
    step_msc = msc(xmat, xref = object$reference, drop.offset = object$drop.offset,
                   window = object$window, drop.na = FALSE)$correction,
    step_emsc = stats::predict(object$res, xmat),
    step_pareto_scale = ,
    step_poisson_scale = sweep(xmat, 2, object$scales, "/")
  )
  new_data[cols] <- as.data.frame(as.matrix(corrected))
  new_data
}

print_preprocessing_step <- function(x, width = max(20, options()$width - 30), ...) {
  title <- preprocessing_titles[[sub("^step_", "", class(x)[1])]]
  recipes::print_step(x$columns, x$terms, x$trained, title, width)
  invisible(x)
}

tidy_preprocessing_step <- function(x, ...) {
  trained <- recipes::is_trained(x)
  terms <- if (trained) x$columns else recipes::sel2char(x$terms)
  res <- tibble::tibble(terms = terms)
  value <- function(v) if (trained && !is.null(v)) unname(as.numeric(v)) else rep(NA_real_, length(terms))
  switch(
    class(x)[1],
    step_baseline = res$method <- rep(x$method, length(terms)),
    step_msc = res$reference <- value(x$reference),
    step_emsc = res$reference <- value(x$res$reference),
    step_pareto_scale = ,
    step_poisson_scale = res$scale <- value(x$scales)
  )
  res$id <- x$id
  res
}

tunable_preprocessing_step <- function(x, ...) {
  empty <- tibble::tibble(
    name = character(0), call_info = list(), source = character(0),
    component = character(0), component_id = character(0)
  )
  row <- function(name, call_info) {
    tibble::tibble(name = name, call_info = list(call_info), source = "recipe",
                   component = class(x)[1], component_id = x$id)
  }
  switch(
    class(x)[1],
    step_baseline = if (x$method == "lsp") {
      row("degree", list(pkg = "dials", fun = "degree_int", range = c(1L, 8L)))
    } else {
      row("lambda", list(pkg = "specProc", fun = "baseline_lambda"))
    },
    step_emsc = row("degree", list(pkg = "dials", fun = "degree_int", range = c(0L, 4L))),
    empty
  )
}

required_pkgs_preprocessing_step <- function(x, ...) {
  c("specProc")
}

# ---- S3 methods (registered when recipes / generics are loaded) ---------------

#' @exportS3Method recipes::prep
prep.step_baseline <- prep_preprocessing_step
#' @exportS3Method recipes::prep
prep.step_snv <- prep_preprocessing_step
#' @exportS3Method recipes::prep
prep.step_msc <- prep_preprocessing_step
#' @exportS3Method recipes::prep
prep.step_emsc <- prep_preprocessing_step
#' @exportS3Method recipes::prep
prep.step_pareto_scale <- prep_preprocessing_step
#' @exportS3Method recipes::prep
prep.step_poisson_scale <- prep_preprocessing_step

#' @exportS3Method recipes::bake
bake.step_baseline <- bake_preprocessing_step
#' @exportS3Method recipes::bake
bake.step_snv <- bake_preprocessing_step
#' @exportS3Method recipes::bake
bake.step_msc <- bake_preprocessing_step
#' @exportS3Method recipes::bake
bake.step_emsc <- bake_preprocessing_step
#' @exportS3Method recipes::bake
bake.step_pareto_scale <- bake_preprocessing_step
#' @exportS3Method recipes::bake
bake.step_poisson_scale <- bake_preprocessing_step

#' @export
print.step_baseline <- print_preprocessing_step
#' @export
print.step_snv <- print_preprocessing_step
#' @export
print.step_msc <- print_preprocessing_step
#' @export
print.step_emsc <- print_preprocessing_step
#' @export
print.step_pareto_scale <- print_preprocessing_step
#' @export
print.step_poisson_scale <- print_preprocessing_step

#' @exportS3Method generics::tidy
tidy.step_baseline <- tidy_preprocessing_step
#' @exportS3Method generics::tidy
tidy.step_snv <- tidy_preprocessing_step
#' @exportS3Method generics::tidy
tidy.step_msc <- tidy_preprocessing_step
#' @exportS3Method generics::tidy
tidy.step_emsc <- tidy_preprocessing_step
#' @exportS3Method generics::tidy
tidy.step_pareto_scale <- tidy_preprocessing_step
#' @exportS3Method generics::tidy
tidy.step_poisson_scale <- tidy_preprocessing_step

#' @exportS3Method generics::tunable
tunable.step_baseline <- tunable_preprocessing_step
#' @exportS3Method generics::tunable
tunable.step_snv <- tunable_preprocessing_step
#' @exportS3Method generics::tunable
tunable.step_msc <- tunable_preprocessing_step
#' @exportS3Method generics::tunable
tunable.step_emsc <- tunable_preprocessing_step
#' @exportS3Method generics::tunable
tunable.step_pareto_scale <- tunable_preprocessing_step
#' @exportS3Method generics::tunable
tunable.step_poisson_scale <- tunable_preprocessing_step

#' @exportS3Method generics::required_pkgs
required_pkgs.step_baseline <- required_pkgs_preprocessing_step
#' @exportS3Method generics::required_pkgs
required_pkgs.step_snv <- required_pkgs_preprocessing_step
#' @exportS3Method generics::required_pkgs
required_pkgs.step_msc <- required_pkgs_preprocessing_step
#' @exportS3Method generics::required_pkgs
required_pkgs.step_emsc <- required_pkgs_preprocessing_step
#' @exportS3Method generics::required_pkgs
required_pkgs.step_pareto_scale <- required_pkgs_preprocessing_step
#' @exportS3Method generics::required_pkgs
required_pkgs.step_poisson_scale <- required_pkgs_preprocessing_step
