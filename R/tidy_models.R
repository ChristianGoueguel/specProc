#' @title Tidy and Glance Methods for Chemometric Models
#'
#' @author Christian L. Goueguel
#'
#' @description
#' `tidy()` lists the estimates of a chemometric model in a long tibble, and
#' `glance()` summarizes the model in a one-row tibble, following the
#' conventions of the broom package. Through parsnip, they also apply to the
#' fitted models and workflows of the `"rsimpls"`, `"lts"`, `"rsimca"` and
#' `"mcd"` engines.
#'
#' @details
#' `tidy()` returns, choosing the table with `matrix` when a model has
#' several:
#'  - robust PCA ([robpca()], [rospca()], [macropca()], [cellpca()]):
#'    `"eigenvalues"` (default), one row per component `PC` with its
#'    `eigenvalue` and `std.dev`; `"loadings"`, the `value` of each variable
#'    (`column`) on each `PC`; `"scores"`, the `value` of each observation
#'    (`row`) on each `PC`, as `tidy()` of a `prcomp` object in broom (whose
#'    names of the tables are also accepted, such as `"rotation"` and
#'    `"x"`);
#'  - [rsimpls()] and [rpcr()]: `"coefficients"` (default), the `estimate`
#'    of each `term` (the intercept and the variables) for each `response`,
#'    in the units of the responses; `"components"`, the robust `r.squared`
#'    and the `rmse` of the models with 1 to `kmax` components (`ncomp`);
#'    `"loadings"` and `"scores"`, as for robust PCA;
#'  - [lts_fit()]: the `term`, `estimate`, `std.error`, `statistic` and
#'    `p.value` of the reweighted least-squares fit, and, with
#'    `conf.int = TRUE`, its confidence interval (`conf.low`, `conf.high`);
#'  - [rsimca()] and [robust_da()]: one row per `class`, with its `prior`,
#'    the number `n` of its regular observations and their misclassification
#'    rate `error`, and, for RSIMCA, its number of components `ncomp` and the
#'    cutoffs of the score and orthogonal distances (`cutoff.sd`,
#'    `cutoff.od`);
#'  - [opls()]: `"coefficients"` (default), the `estimate` of each `term`
#'    (variable) with its `vip` and `ortho.vip`; `"components"`, the
#'    explained variances `r2x`, `r2y` and `q2` of each component, their
#'    cumulative values (`r2x.cum`, ...) and their `significance`;
#'  - [som()]: one row per `unit` of the map, with its `row` and `col` in
#'    the grid, its position (`x`, `y`), the number `n` of observations it
#'    represents and their mean quantization error `qe`;
#'  - [select_wavelengths()]: one row per variable (`term`), with its
#'    `importance`, whether it is `selected`, and, for iPLS, its `interval`;
#'  - [nas()]: the regression vector, the `estimate` of each `term`;
#'  - [applicability_domain()]: one row per component `PC` of the PCA, with
#'    its `std.dev`, `percent` of the variance and `cumulative` percent (as
#'    fractions).
#'
#' `glance()` returns the number of observations `nobs` and of variables
#' `nvar`, and:
#'  - robust PCA: the number of components `k`, `h`, `alpha`, the cutoffs
#'    `cutoff.sd` and `cutoff.od`, the numbers of `n.good.leverage`,
#'    `n.orthogonal.outlier` and `n.bad.leverage` observations, the number
#'    of `flagged.cells` (MacroPCA and cellPCA) and the `method`;
#'  - [rsimpls()] and [rpcr()]: `ncomp`, `kmax`, `h`, `alpha`, the robust
#'    `r.squared`, the robust scale of the residuals `sigma` (one response),
#'    the numbers of `n.good.leverage`, `n.vertical.outlier` and
#'    `n.bad.leverage` observations, the `method` and, for RPCR, the
#'    `regression`;
#'  - [lts_fit()]: the robust `r.squared`, the robust scale of the residuals
#'    `sigma`, `df.residual`, `alpha` and the number of `n.outliers` (weight
#'    0 in the reweighted fit);
#'  - [rsimca()] and [robust_da()]: the number of `n.classes`, the overall
#'    misclassification rate `error`, `alpha`, the number of observations
#'    left out (`n.outliers`; for RSIMCA, those outlying for all classes) and
#'    the `method`;
#'  - [opls()]: the numbers of predictive and orthogonal components
#'    (`ncomp`, `ncomp.ortho`), `r2x.cum`, `r2y.cum`, `q2.cum`, `rmsee` and,
#'    after a permutation test, its p-values `p.r2y` and `p.q2`;
#'  - [som()]: the number of `n.units`, the `topology`, the
#'    `quantization.error` and `topographic.error` of the map, the number of
#'    `n.novel` observations (beyond the cutoff of the quantization error),
#'    the `epochs` and whether the map is `robust`;
#'  - [select_wavelengths()]: the `method`, `ncomp`, the number of selected
#'    variables `n.selected`, `robust`, the `threshold` and, for iPLS, the
#'    `rmsecv` of the final model;
#'  - [nas()]: the `method`, `ncomp` and the figures of merit
#'    (`sensitivity`, `selectivity` and, with a noise level,
#'    `analytical.sensitivity`, `lod` and `loq`);
#'  - [applicability_domain()]: the number of components `k`, the variance
#'    `explained`, the `conf.level` and the limits `t2.limit` and `q.limit`.
#'
#' @inheritParams specproc-tidy
#' @param x An object returned by [robpca()], [rospca()], [macropca()],
#'   [cellpca()], [rsimpls()], [rpcr()], [lts_fit()], [rsimca()],
#'   [robust_da()], [opls()], [som()], [select_wavelengths()], [nas()] or
#'   [applicability_domain()].
#' @param matrix The table to return (see Details).
#'
#' @return A tibble.
#'
#' @seealso [specproc-tidy] for the fits, calibrations and plasma diagnostics
#' @name specproc-tidy-models
#'
#' @examples
#' data(forageLIBS)
#' wl <- as.numeric(names(forageLIBS[-(1:14)]))
#' spectra <- forageLIBS[-(1:14)][, wl > 380 & wl < 430]
#' set.seed(1)
#' fit <- rsimpls(spectra[1:300, ], forageLIBS$Ca[1:300], ncomp = 4)
#' tidy(fit, matrix = "components")
#' glance(fit)
#' # the regression vector, one row per wavelength
#' head(tidy(fit))
#'
#' pca <- robpca(spectra, k = 3)
#' tidy(pca)
#' glance(pca)
NULL

# ---- robust PCA --------------------------------------------------------------

#' @rdname specproc-tidy-models
#' @export
tidy.specproc_robpca <- function(x, matrix = "eigenvalues", ...) {
  matrix <- tidy_matrix(matrix, c("eigenvalues", "loadings", "scores"))
  switch(
    matrix,
    eigenvalues = tibble::tibble(PC = seq_along(x$eigenvalues), eigenvalue = x$eigenvalues,
                                 std.dev = sqrt(x$eigenvalues)),
    loadings = component_table(x$loadings, model_variables(x, nrow(x$loadings)), "column"),
    scores = component_table(x$scores, seq_len(nrow(x$scores)), "row")
  )
}

#' @rdname specproc-tidy-models
#' @export
glance.specproc_robpca <- function(x, ...) {
  out <- tibble::tibble(
    nobs = nrow(x$scores), nvar = nrow(x$loadings), k = x$k, h = x$h, alpha = x$alpha,
    cutoff.sd = x$cutoff_sd, cutoff.od = x$cutoff_od
  )
  out <- cbind(out, outlier_counts(x$outlier_type))
  out$flagged.cells <- if (is.null(x$flagged_cells)) NA_integer_ else sum(x$flagged_cells)
  out$method <- robust_pca_method(x)
  tibble::as_tibble(out)
}

# ---- robust regression -------------------------------------------------------

#' @rdname specproc-tidy-models
#' @export
tidy.specproc_rsimpls <- function(x, matrix = "coefficients", ...) {
  tidy_robust_regression(x, matrix)
}

#' @rdname specproc-tidy-models
#' @export
glance.specproc_rsimpls <- function(x, ...) {
  glance_robust_regression(x, "RSIMPLS")
}

#' @rdname specproc-tidy-models
#' @export
tidy.specproc_rpcr <- function(x, matrix = "coefficients", ...) {
  tidy_robust_regression(x, matrix)
}

#' @rdname specproc-tidy-models
#' @export
glance.specproc_rpcr <- function(x, ...) {
  out <- glance_robust_regression(x, "RPCR")
  out$regression <- x$regression
  out
}

#' @rdname specproc-tidy-models
#' @export
tidy.specproc_lts <- function(x, conf.int = FALSE, conf.level = 0.95, ...) {
  se <- x$std_error %||% rep(NA_real_, length(x$coefficients))
  df <- x$df_residual %||% NA_real_
  statistic <- unname(x$coefficients / se)
  out <- tibble::tibble(term = names(x$coefficients), estimate = unname(x$coefficients),
                        std.error = unname(se), statistic = statistic,
                        p.value = 2 * stats::pt(-abs(statistic), df))
  if (check_conf_int(conf.int, conf.level)) out <- add_conf_int(out, conf.level, df)
  out
}

#' @rdname specproc-tidy-models
#' @export
glance.specproc_lts <- function(x, ...) {
  tibble::tibble(
    nobs = length(x$residuals), nvar = length(x$coefficients) - 1L,
    r.squared = x$r_squared %||% NA_real_, sigma = x$scale,
    df.residual = x$df_residual %||% NA_real_, alpha = x$alpha,
    n.outliers = sum(x$weights == 0)
  )
}

# ---- classification ----------------------------------------------------------

#' @rdname specproc-tidy-models
#' @export
tidy.specproc_rsimca <- function(x, ...) {
  out <- class_table(x)
  out$ncomp <- unname(x$ncomp[x$levels])
  out$cutoff.sd <- vapply(x$models[x$levels], `[[`, numeric(1), "cutoff_sd", USE.NAMES = FALSE)
  out$cutoff.od <- vapply(x$models[x$levels], `[[`, numeric(1), "cutoff_od", USE.NAMES = FALSE)
  out
}

#' @rdname specproc-tidy-models
#' @export
glance.specproc_rsimca <- function(x, ...) {
  glance_classifier(x, nobs = length(x$group), n_outliers = sum(x$outlying), method = "RSIMCA")
}

#' @rdname specproc-tidy-models
#' @export
tidy.specproc_robust_da <- function(x, ...) {
  class_table(x)
}

#' @rdname specproc-tidy-models
#' @export
glance.specproc_robust_da <- function(x, ...) {
  glance_classifier(x, nobs = length(x$rd), n_outliers = sum(x$weights == 0), method = x$method)
}

# ---- OPLS, SOM, wavelength selection, NAS and applicability domain ----------

#' @rdname specproc-tidy-models
#' @export
tidy.specproc_opls <- function(x, matrix = "coefficients", ...) {
  matrix <- tidy_matrix(matrix, c("coefficients", "components"))
  if (matrix == "coefficients") {
    return(tibble::tibble(term = model_variables(x, length(x$coefficients)),
                          estimate = unname(x$coefficients), vip = unname(x$vip),
                          ortho.vip = unname(x$ortho_vip)))
  }
  co <- x$components
  tibble::tibble(component = co$component, r2x = co$R2X, r2x.cum = co$`R2X(cum)`,
                 r2y = co$R2Y, r2y.cum = co$`R2Y(cum)`, q2 = co$Q2, q2.cum = co$`Q2(cum)`,
                 significance = co$significance)
}

#' @rdname specproc-tidy-models
#' @export
glance.specproc_opls <- function(x, ...) {
  s <- x$summary
  tibble::tibble(
    nobs = length(x$fitted), nvar = length(x$coefficients), ncomp = as.integer(s$pre),
    ncomp.ortho = as.integer(s$ort), r2x.cum = s$`R2X(cum)`, r2y.cum = s$`R2Y(cum)`,
    q2.cum = s$`Q2(cum)`, rmsee = s$RMSEE, p.r2y = s$pR2Y %||% NA_real_,
    p.q2 = s$pQ2 %||% NA_real_
  )
}

#' @rdname specproc-tidy-models
#' @export
tidy.specproc_som <- function(x, ...) {
  out <- x$grid
  units <- nrow(out)
  out$n <- tabulate(x$unit, nbins = units)
  qe <- rep(NA_real_, units)
  mean_qe <- tapply(x$qe, x$unit, mean)
  qe[as.integer(names(mean_qe))] <- mean_qe
  out$qe <- qe
  out
}

#' @rdname specproc-tidy-models
#' @export
glance.specproc_som <- function(x, ...) {
  tibble::tibble(
    nobs = length(x$unit), nvar = ncol(x$codebook), n.units = nrow(x$grid),
    topology = x$topology, quantization.error = x$quantization_error,
    topographic.error = x$topographic_error, n.novel = sum(x$qe > x$cutoff),
    epochs = x$epochs, robust = x$robust
  )
}

#' @rdname specproc-tidy-models
#' @export
tidy.specproc_wavelength_selection <- function(x, ...) {
  terms <- names(x$importance) %||% names(x$mean_spectrum)
  out <- tibble::tibble(term = terms, importance = unname(x$importance),
                        selected = terms %in% x$selected)
  iv <- x$intervals
  if (!is.null(iv) && sum(iv$size) == length(terms)) {
    out$interval <- rep(iv$interval, as.vector(iv$size))
  }
  out
}

#' @rdname specproc-tidy-models
#' @export
glance.specproc_wavelength_selection <- function(x, ...) {
  tibble::tibble(
    nobs = x$observations, nvar = length(x$importance), method = x$method, ncomp = x$ncomp,
    n.selected = length(x$selected), robust = x$robust, threshold = x$threshold %||% NA_real_,
    rmsecv = if ("RMSECV" %in% names(x$path)) utils::tail(x$path$RMSECV, 1) else NA_real_
  )
}

#' @rdname specproc-tidy-models
#' @export
tidy.specproc_nas <- function(x, ...) {
  tibble::tibble(term = model_variables(x, length(x$coefficients)),
                 estimate = unname(x$coefficients))
}

#' @rdname specproc-tidy-models
#' @export
glance.specproc_nas <- function(x, ...) {
  fom <- x$figures_of_merit
  merit <- function(name) if (name %in% names(fom)) unname(fom[[name]]) else NA_real_
  tibble::tibble(
    nobs = nrow(x$nas), nvar = length(x$coefficients), method = x$method, ncomp = x$ncomp,
    sensitivity = merit("sensitivity"), selectivity = merit("selectivity"),
    analytical.sensitivity = merit("analytical_sensitivity"), lod = merit("lod"),
    loq = merit("loq")
  )
}

#' @rdname specproc-tidy-models
#' @export
tidy.specproc_domain <- function(x, ...) {
  variance <- x$pca$sdev^2
  k <- seq_len(x$k)
  tibble::tibble(PC = k, std.dev = x$pca$sdev[k], percent = variance[k] / sum(variance),
                 cumulative = cumsum(variance)[k] / sum(variance))
}

#' @rdname specproc-tidy-models
#' @export
glance.specproc_domain <- function(x, ...) {
  tibble::tibble(nobs = x$n, nvar = nrow(x$pca$rotation), k = x$k, explained = x$explained,
                 conf.level = x$conf_level, t2.limit = unname(x$limits[["t2"]]),
                 q.limit = unname(x$limits[["q"]]))
}

# ---- internals ---------------------------------------------------------------

# The table of `matrix`, with the names of the tables of tidy.prcomp() in broom.
tidy_matrix <- function(matrix, choices) {
  aliases <- c(u = "scores", x = "scores", samples = "scores", v = "loadings",
               rotation = "loadings", variables = "loadings", d = "eigenvalues",
               pcs = "eigenvalues")
  if (!is.character(matrix) || length(matrix) != 1) {
    stop("'matrix' must be one of ", paste0('"', choices, '"', collapse = ", "), ".", call. = FALSE)
  }
  if (!matrix %in% choices && matrix %in% names(aliases)) matrix <- aliases[[matrix]]
  if (!matrix %in% choices) {
    stop("'matrix' must be one of ", paste0('"', choices, '"', collapse = ", "), ".", call. = FALSE)
  }
  matrix
}

# A matrix of loadings or scores in long format: one row per element.
component_table <- function(m, labels, name) {
  m <- as.matrix(m)
  out <- tibble::tibble(id = rep(labels, times = ncol(m)), PC = rep(seq_len(ncol(m)), each = nrow(m)),
                        value = as.vector(m))
  names(out)[1] <- name
  out
}

model_variables <- function(x, n) {
  attr(x, "variables") %||% paste0("x", seq_len(n))
}

# Numbers of observations of each outlier type but the regular ones.
outlier_counts <- function(type) {
  levels <- setdiff(levels(type), "regular")
  counts <- as.list(as.integer(table(type)[levels]))
  names(counts) <- paste0("n.", gsub(" ", ".", levels))
  tibble::as_tibble(counts)
}

robust_pca_method <- function(x) {
  if (inherits(x, "specproc_cellpca")) return("cellPCA")
  if (inherits(x, "specproc_macropca")) return("MacroPCA")
  if (inherits(x, "specproc_rospca")) return("ROSPCA")
  "ROBPCA"
}

tidy_robust_regression <- function(x, matrix) {
  matrix <- tidy_matrix(matrix, c("coefficients", "components", "loadings", "scores"))
  switch(
    matrix,
    coefficients = {
      b <- rbind(x$intercept, x$coefficients)
      responses <- attr(x, "responses") %||% paste0("y", seq_len(ncol(b)))
      terms <- c("(Intercept)", model_variables(x, nrow(x$coefficients)))
      tibble::tibble(response = rep(responses, each = nrow(b)), term = rep(terms, ncol(b)),
                     estimate = as.vector(b))
    },
    components = tibble::tibble(ncomp = x$components$ncomp, r.squared = x$components$R2,
                                rmse = x$components$RMSE),
    loadings = component_table(x$x_loadings, model_variables(x, nrow(x$x_loadings)), "column"),
    scores = component_table(x$x_scores, seq_len(nrow(x$x_scores)), "row")
  )
}

glance_robust_regression <- function(x, method) {
  sigma <- as.matrix(x$sigma)
  out <- tibble::tibble(
    nobs = nrow(x$x_scores), nvar = nrow(x$coefficients), ncomp = x$ncomp, kmax = x$kmax,
    h = x$h, alpha = x$alpha, r.squared = x$R2,
    sigma = if (length(sigma) == 1) sqrt(sigma[[1]]) else NA_real_
  )
  out <- cbind(out, outlier_counts(x$outlier_type))
  out$method <- method
  tibble::as_tibble(out)
}

class_table <- function(x) {
  mis <- x$misclassification
  i <- match(x$levels, mis$class)
  tibble::tibble(class = x$levels, prior = unname(x$prior[x$levels]), n = mis$n[i],
                 error = mis$error[i])
}

glance_classifier <- function(x, nobs, n_outliers, method) {
  mis <- x$misclassification
  tibble::tibble(nobs = nobs, nvar = attr(x, "nvar") %||% NA_integer_,
                 n.classes = length(x$levels), error = mis$error[mis$class == "overall"],
                 alpha = x$alpha, n.outliers = n_outliers, method = method)
}
