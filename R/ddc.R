# Detection of deviating cells (DDC), after Rousseeuw and Van den Bossche
# (2018). Used by macropca(). Not exported.
#
#  1. Each column is robustly standardized (median and MAD), and the cells
#     beyond the cut-off c = sqrt(qchisq(tol_prob, 1)) are set aside as
#     univariate outliers.
#  2. Each column gets as neighbors the (at most `maxnb`) columns with the
#     largest robust correlations, of at least `corrlim` in absolute value:
#     the correlations of the wrapped standardized data (Raymaekers and
#     Rousseeuw, 2021), computed in C++ by blocks of columns, so that the
#     p x p correlation matrix of spectra with thousands of channels is
#     never stored.
#  3. Each cell is predicted from the cells of its row in the neighboring
#     columns, through robust slopes (medians of ratios), averaged with the
#     absolute correlations as weights; the predictions of each column are
#     then deshrunk by a robust slope of the data on them.
#  4. The residuals of each column are robustly standardized: the cells
#     beyond the cut-off are flagged, and so are the rows whose cells
#     deviate on average (the mean of pchisq(r^2, 1) - 1/2 over the row,
#     robustly standardized, beyond the cut-off).
#  5. The flagged and missing cells are imputed by their predictions.

ddc <- function(x, tol_prob = 0.99, corrlim = 0.5, maxnb = 100, block = 256L) {
  x <- as_numeric_matrix(x, "x")
  n <- nrow(x)
  p <- ncol(x)
  cutoff <- sqrt(stats::qchisq(tol_prob, 1))
  center <- apply(x, 2, stats::median, na.rm = TRUE)
  scale <- apply(x, 2, stats::mad, na.rm = TRUE)
  constant <- !is.finite(scale) | scale <= 0 | !is.finite(center)
  center[!is.finite(center)] <- 0
  scale[constant] <- 1
  z <- sweep(sweep(x, 2, center), 2, scale, "/")
  z[, constant] <- 0
  u <- z
  u[!is.na(u) & abs(u) > cutoff] <- NA

  u_nan <- u
  u_nan[is.na(u_nan)] <- NaN
  nb <- ddc_neighbors_cpp(u_nan, corrlim, as.integer(maxnb), 0.5, as.integer(block))
  pred <- ddc_predict_cpp(u_nan, nb$index, nb$correlation, nb$slope)

  # deshrink: robust slope of the cleaned data on their predictions
  deshrink <- vapply(seq_len(p), function(j) {
    ok <- !is.na(u[, j]) & abs(pred[, j]) >= 0.5
    if (sum(ok) < 5) return(1)
    a <- stats::median(u[ok, j] / pred[ok, j])
    if (is.finite(a) && a > 0) a else 1
  }, numeric(1))
  pred <- sweep(pred, 2, deshrink, "*")

  resid <- z - pred
  resid_center <- apply(resid, 2, stats::median, na.rm = TRUE)
  resid_scale <- apply(resid, 2, stats::mad, na.rm = TRUE)
  resid_center[!is.finite(resid_center)] <- 0
  resid_scale[!is.finite(resid_scale) | resid_scale <= 0] <- 1
  std_resid <- sweep(sweep(resid, 2, resid_center), 2, resid_scale, "/")
  std_resid[, constant] <- 0
  std_resid[is.na(x)] <- NA

  flagged_cells <- !is.na(std_resid) & abs(std_resid) > cutoff
  row_stat <- rowMeans(stats::pchisq(std_resid^2, 1) - 0.5, na.rm = TRUE)
  row_stat[!is.finite(row_stat)] <- 0
  row_scale <- stats::mad(row_stat)
  flagged_rows <- if (is.finite(row_scale) && row_scale > 0) {
    (row_stat - stats::median(row_stat)) / row_scale > cutoff
  } else {
    rep(FALSE, n)
  }

  z_imputed <- z
  replace <- flagged_cells | is.na(x)
  z_imputed[replace] <- pred[replace]
  imputed <- sweep(sweep(z_imputed, 2, scale, "*"), 2, center, "+")
  dimnames(imputed) <- dimnames(flagged_cells) <- dimnames(std_resid) <- dimnames(x)

  list(imputed = imputed, flagged_cells = flagged_cells, flagged_rows = flagged_rows,
       std_resid = std_resid, center = center, scale = scale, cutoff = cutoff,
       neighbors = nb, deshrink = deshrink)
}
