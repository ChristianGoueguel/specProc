# Detection of deviating cells (DDC), after Rousseeuw and Van den Bossche
# (2018), following the algorithm of the cellWise package (DDC with its
# default options). Used by macropca(). Not exported.
#
#  1. Each column is robustly standardized: 1-step M estimators of location
#     (biweight) and scale (Huber), or, for more than 750 variables (`fast`),
#     the univariate MCD with a 1-step M location, as in cellWise.
#  2. The cells beyond the cut-off c = sqrt(qchisq(tol_prob, 1)) are set
#     aside as univariate outliers.
#  3. Each column gets as neighbors the `maxnb` columns with the largest
#     absolute robust correlations: the Gnanadesikan-Kettenring correlation
#     followed by a weighted Pearson correlation, or, with `fast`, the
#     correlation of the wrapped data (Raymaekers and Rousseeuw, 2021),
#     computed exactly in C++ by blocks of columns (cellWise searches the
#     neighbors approximately). The neighbors with an absolute correlation
#     below `corrlim` are left out; a column without neighbors is standalone.
#     The slope of the column on each neighbor is the median of the ratios
#     followed by least squares on the inlying cells.
#  4. Each cell of a connected column is predicted by the weighted mean
#     (with the absolute correlations as weights) of its own value and of
#     slope * neighbor value, and the predictions of each column are
#     deshrunk by the robust slope of the column on them.
#  5. The residuals of each column are standardized by their 1-step M scale,
#     and the cells beyond the cut-off are flagged (for standalone columns,
#     the univariate outliers). The rows whose cells deviate on average (the
#     mean of pchisq(r^2, 1) - 1/2, standardized by its median and MAD)
#     beyond the cut-off are flagged.
#  6. The flagged and missing cells are imputed by their predictions.

ddc <- function(x, tol_prob = 0.99, corrlim = 0.5, maxnb = 100, fast = ncol(x) > 750,
                block = 256L) {
  x <- as_numeric_matrix(x, "x")
  core <- ddc_core_cpp(x, tol_prob, corrlim, as.integer(maxnb), isTRUE(fast), as.integer(block))
  flagged <- core$flagged
  std_resid <- core$std_resid
  std_resid[is.na(x)] <- NA
  imputed <- x
  replace <- flagged | is.na(x)
  imputed[replace] <- core$estimate[replace]
  cutoff <- sqrt(stats::qchisq(tol_prob, 1))
  dimnames(imputed) <- dimnames(flagged) <- dimnames(std_resid) <- dimnames(x)
  structure(list(
    imputed = imputed, flagged_cells = flagged,
    flagged_rows = !is.na(core$Ti) & core$Ti > cutoff, Ti = core$Ti,
    std_resid = std_resid, center = core$loc, scale = core$scale, cutoff = cutoff,
    tol_prob = tol_prob,
    model = core[c("ngbrs", "weights", "slopes", "robcors", "deshrink", "res_scale",
                   "standalone", "med_ti", "mad_ti")]
  ), class = "specproc_ddc")
}

# DDC of new data with the model of a ddc() fit (DDCpredict of cellWise).
predict_ddc <- function(fit, x) {
  x <- as_numeric_matrix(x, "x")
  m <- fit$model
  out <- ddc_apply_cpp(x, fit$center, fit$scale, m$ngbrs, m$weights, m$slopes, m$deshrink,
                       m$res_scale, m$standalone, fit$tol_prob, m$med_ti, m$mad_ti)
  imputed <- x
  replace <- out$flagged | is.na(x)
  imputed[replace] <- out$estimate[replace]
  std_resid <- out$std_resid
  std_resid[is.na(x)] <- NA
  dimnames(imputed) <- dimnames(out$flagged) <- dimnames(std_resid) <- dimnames(x)
  list(imputed = imputed, flagged_cells = out$flagged,
       flagged_rows = !is.na(out$Ti) & out$Ti > fit$cutoff, Ti = out$Ti, std_resid = std_resid)
}
