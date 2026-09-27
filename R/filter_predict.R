#' @title Apply an Orthogonalization Filter to New Spectra
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Corrects new spectra with a filter estimated by [epo()], [osc()],
#' [direct_orthogonal()], [direct_osc()], [projected_osc()] or [o2pls()].
#' The new spectra are preprocessed with the centers and scales of the
#' calibration data, and the components estimated on the calibration data are
#' removed. Estimating the filter on calibration data and applying it to
#' validation data with `predict()` avoids the optimistic bias that arises
#' when a supervised filter is fitted to all the data before validation.
#'
#' @details
#' For [epo()] the spectra are not centered, and the correction is
#' \eqn{\textbf{X}_{new} - \textbf{X}_{new}\textbf{VV}^T}. For the other
#' methods, the corrected spectra are centered (and scaled, if `scale = TRUE`
#' was used), like the `correction` component of the fitted object:
#'  - [osc()] with `method = "wold"` or `"sjoblom"`, and [o2pls()], remove the
#'    orthogonal components one at a time, because their weights refer to the
#'    deflated matrix.
#'  - [osc()] with `method = "fearn"`, [direct_orthogonal()], [direct_osc()]
#'    and [projected_osc()] remove all components at once:
#'    \eqn{\textbf{X}_{new} - \textbf{X}_{new}\textbf{WP}^T}.
#'
#' Applied to the calibration spectra, `predict()` returns the `correction`
#' component of the fitted object.
#'
#' @param object A filter returned by [epo()], [osc()], [direct_orthogonal()],
#'   [direct_osc()], [projected_osc()] or [o2pls()].
#' @param newdata A numeric matrix or data frame of new spectra, with the same
#'   variables as the calibration data. If both have column names, the
#'   columns of `newdata` are matched by name.
#' @param ... Not used.
#'
#' @return A tibble of corrected spectra, with the same columns as `newdata`.
#'
#' @seealso The recipe steps [step_osc()] and related functions, which apply
#'   these filters within a tidymodels workflow.
#'
#' @name predict.specproc_filter
#'
#' @examples
#' set.seed(1)
#' x <- matrix(rnorm(40 * 30), 40, 30)
#' y <- x[, 1] + rnorm(40, sd = 0.1)
#' cal <- 1:30
#'
#' fit <- osc(x[cal, ], y[cal], method = "fearn", ncomp = 2)
#' corrected <- predict(fit, x[-cal, ])
#' dim(corrected)
#'
#' # applied to the calibration spectra, predict() gives the correction
#' all.equal(predict(fit, x[cal, ]), fit$correction)
#'
NULL

# Adds the filter class, and records the calibration variables (`x` is the
# calibration matrix) so that predict() can check new data.
new_filter <- function(res, subclass, x, ...) {
  structure(
    res,
    variables = colnames(x), nvar = ncol(x), ...,
    class = c(subclass, "specproc_filter")
  )
}

# Checks new data against the calibration variables and returns a matrix
# with the columns in calibration order.
filter_newdata <- function(object, newdata) {
  newdata <- as_numeric_matrix(newdata, "newdata")
  p <- attr(object, "nvar")
  if (ncol(newdata) != p) {
    stop("'newdata' must have ", p, " columns, like the calibration data.", call. = FALSE)
  }
  vars <- attr(object, "variables")
  nms <- colnames(newdata)
  if (!is.null(vars) && !is.null(nms) && !identical(nms, vars)) {
    if (!setequal(nms, vars)) {
      stop("The column names of 'newdata' do not match those of the calibration data.", call. = FALSE)
    }
    newdata <- newdata[, vars, drop = FALSE]
  }
  newdata
}

# (X - center) / scale
filter_preprocess <- function(object, newdata) {
  apply_preprocess(newdata, list(center = object$center, scale = object$scale))
}

remove_block <- function(z, w, p) {
  z - (z %*% w) %*% t(p)
}

remove_sequential <- function(z, w, p) {
  for (i in seq_len(ncol(w))) {
    z <- z - (z %*% w[, i]) %*% t(p[, i])
  }
  z
}

filter_output <- function(z, object) {
  as_tbl(z, attr(object, "variables"))
}

#' @rdname predict.specproc_filter
#' @export
predict.specproc_epo <- function(object, newdata, ...) {
  x <- filter_newdata(object, newdata)
  v <- as.matrix(object$loadings)
  filter_output(x - (x %*% v) %*% t(v), object)
}

#' @rdname predict.specproc_filter
#' @export
predict.specproc_osc <- function(object, newdata, ...) {
  z <- filter_preprocess(object, filter_newdata(object, newdata))
  w <- as.matrix(object$weights)
  p <- as.matrix(object$loadings)
  z <- if (attr(object, "method") == "fearn") remove_block(z, w, p) else remove_sequential(z, w, p)
  filter_output(z, object)
}

#' @rdname predict.specproc_filter
#' @export
predict.specproc_direct_orthogonal <- function(object, newdata, ...) {
  z <- filter_preprocess(object, filter_newdata(object, newdata))
  p <- as.matrix(object$loading)
  filter_output(remove_block(z, p, p), object)
}

#' @rdname predict.specproc_filter
#' @export
predict.specproc_direct_osc <- function(object, newdata, ...) {
  z <- filter_preprocess(object, filter_newdata(object, newdata))
  filter_output(remove_block(z, as.matrix(object$weight), as.matrix(object$loading)), object)
}

#' @rdname predict.specproc_filter
#' @export
predict.specproc_projected_osc <- function(object, newdata, ...) {
  z <- filter_preprocess(object, filter_newdata(object, newdata))
  filter_output(remove_block(z, as.matrix(object$weights), as.matrix(object$loadings)), object)
}

#' @rdname predict.specproc_filter
#' @export
predict.o2pls <- function(object, newdata, ...) {
  z <- filter_preprocess(object, filter_newdata(object, newdata))
  w <- object$weights$x_ortho
  if (!is.null(w)) {
    z <- remove_sequential(z, w, object$loadings$x_ortho)
  }
  filter_output(z, object)
}

#' @export
print.specproc_filter <- function(x, ...) {
  label <- switch(
    class(x)[1],
    specproc_epo = "External parameter orthogonalization (EPO)",
    specproc_osc = paste0("Orthogonal signal correction (OSC, method = \"", attr(x, "method"), "\")"),
    specproc_direct_orthogonal = "Direct orthogonalization (DO)",
    specproc_direct_osc = "Direct orthogonal signal correction (DOSC)",
    specproc_projected_osc = "Projected orthogonal signal correction (POSC)"
  )
  loadings <- x$loadings %||% x$loading
  cat(label, "\n\n", sep = "")
  cat("Variables:            ", attr(x, "nvar"), "\n", sep = "")
  cat("Components removed:   ", ncol(loadings), "\n", sep = "")
  if (!is.null(x$correction)) {
    cat("Calibration spectra:  ", nrow(x$correction), "\n", sep = "")
  }
  cat("\nUse predict(<filter>, newdata) to correct new spectra.\n")
  invisible(x)
}
