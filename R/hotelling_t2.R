#' @title Hotelling's T-squared Statistic of Samples in an Embedding
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Computes Hotelling's \eqn{T^2} statistic of each sample from its scores
#' on the first `k` components of an embedding (such as PCA or PLS scores),
#' with its limits at one or more confidence levels, for all the samples
#' together or within groups, with [HotellingEllipse::ellipseParam()].
#' Samples beyond a limit are far from the center of the data (or of their
#' group) given its covariance: candidate outliers.
#'
#' @details
#' \eqn{T^2} is the squared Mahalanobis distance of a sample to the mean of
#' the scores, with their covariance. Its limit at the confidence level
#' \eqn{1 - \alpha}, for \eqn{n} samples, is
#'  - `method = "f"` (default): \eqn{k(n - 1)/(n - k)\, F_{1-\alpha}(k, n -
#'    k)}, the conventional limit of score plots (Jackson, 1991);
#'  - `method = "beta"`: \eqn{(n - 1)^2/n\, B_{1-\alpha}(k/2, (n - k -
#'    1)/2)}, the exact distribution for the samples that estimated the mean
#'    and covariance (Tracy, Young and Mason, 1992); the F limit can even
#'    exceed the largest \eqn{T^2} a sample can reach, \eqn{(n - 1)^2 / n},
#'    for small \eqn{n};
#'  - `method = "new"`: \eqn{k(n + 1)(n - 1)/(n(n - k))\, F_{1-\alpha}(k,
#'    n - k)}, the exact limit for a new sample, independent of the estimates
#'    (larger than the F limit by \eqn{(n + 1)/n}).
#'
#' Within groups (`group`), each group gets its own mean, covariance and
#' limits, which tells whether a sample is typical of its own group rather
#' than of the whole data set; each group needs more than `k + 1` samples.
#'
#' The statistic uses the `k` components given, not only the two shown on a
#' plot: a sample can lie inside the ellipse of two components and still
#' exceed the limit in `k` dimensions. Classical estimates are themselves
#' sensitive to outliers; with many outliers, prefer the robust distances of
#' [robpca()].
#'
#' @param data The embedding: a data frame, a matrix, a [stats::prcomp()]
#'   fit, or an object of [robpca()], [rospca()] or [macropca()].
#' @param columns The components used: a character vector of column names.
#'   By default, the first `k` embedding coordinates (columns named like
#'   `PC1`, `UMAP1`, ...), or else the first `k` numeric columns.
#' @param k The number of components, when `columns` is not given. Default
#'   is 2.
#' @param group An optional grouping: a column of `data` (unquoted or as a
#'   string), or a vector with one value per sample.
#' @param conf_level The confidence level(s) of the limits: one or more
#'   values between 0 and 1. Default is 0.975.
#' @param method The distribution of the limits: `"f"` (default), `"beta"`
#'   or `"new"`.
#'
#' @return A tibble with one row per sample: its row number `sample`, the
#'   `group` (with `group`), `t2`, then for each confidence level (in %, e.g.
#'   97.5) its limit `limit_97.5` and whether the sample exceeds it,
#'   `outlier_97.5`, and the number of samples `n` of its group (or of the
#'   data).
#'
#' @references
#'  - Hotelling, H. (1931). The generalization of Student's ratio. The
#'    Annals of Mathematical Statistics, 2(3):360-378.
#'  - Tracy, N.D., Young, J.C., Mason, R.L. (1992). Multivariate control
#'    charts for individual observations. Journal of Quality Technology,
#'    24(2):88-95.
#'  - Jackson, J.E. (1991). A User's Guide to Principal Components. Wiley,
#'    New York.
#'
#' @seealso [plot_embedding()], [q_residuals()], [robpca()]
#' @export hotelling_t2
#'
#' @examples
#' if (rlang::is_installed("HotellingEllipse", version = "1.3.0")) {
#'   data(forageLIBS)
#'   # the 380-430 nm window (Ca II H and K lines)
#'   wl <- suppressWarnings(as.numeric(names(forageLIBS)))
#'   spectra <- forageLIBS[which(wl > 380 & wl < 430)]
#'   pca <- stats::prcomp(spectra)
#'   t2 <- hotelling_t2(pca, k = 3)
#'   t2[t2$outlier_97.5, ]
#'   # two limits, with the exact distribution of the calibration samples
#'   hotelling_t2(pca, k = 3, conf_level = c(0.95, 0.99), method = "beta")
#' }
hotelling_t2 <- function(data, columns = NULL, k = 2, group = NULL, conf_level = 0.975,
                         method = "f") {
  rlang::check_installed("HotellingEllipse", version = "1.3.0",
                         reason = "to compute Hotelling's T-squared.")
  conf_level <- check_conf_level(conf_level)
  method <- match.arg(method, c("f", "beta", "new"))
  df <- embedding_data(data)
  group_values <- embedding_values(rlang::enquo(group), df, "group")$values
  if (is.null(columns)) {
    check_count(k, "k", lower = 2)
    columns <- embedding_components(df, k)
  } else if (!is.character(columns) || !all(columns %in% names(df)) ||
             !all(vapply(df[columns], is.numeric, logical(1))) || length(columns) < 2) {
    stop("'columns' must name at least 2 numeric columns of 'data'.", call. = FALSE)
  }
  labels <- conf_label(conf_level)
  scores <- as.data.frame(df[columns])
  rows <- if (is.null(group_values)) list(all = seq_len(nrow(df))) else
    split(seq_len(nrow(df)), as.character(group_values))
  out <- lapply(names(rows), function(g) {
    r <- rows[[g]]
    r <- r[stats::complete.cases(scores[r, , drop = FALSE])]
    res <- tibble::tibble(sample = r, t2 = NA_real_)
    limits <- rep(NA_real_, length(conf_level))
    if (length(r) <= length(columns) + 1) {
      if (!is.null(group_values)) {
        warning("No T-squared for group ", g, ": it needs more than ", length(columns) + 1,
                " samples.", call. = FALSE)
      }
    } else {
      fit <- tryCatch(
        HotellingEllipse::ellipseParam(scores[r, , drop = FALSE], k = length(columns),
                                       rel.tol = .Machine$double.eps,
                                       method = if (method == "new") "f" else method,
                                       conf.limit = conf_level),
        error = function(e) {
          stop("T-squared failed", if (!is.null(group_values)) paste0(" in group ", g), ": ",
               conditionMessage(e), call. = FALSE)
        }
      )
      res$t2 <- fit$Tsquare$value
      limits <- vapply(labels, function(l) fit[[paste0("cutoff.", l, "pct")]], numeric(1))
      # the limit of a new sample: the F limit times (n + 1) / n
      if (method == "new") limits <- limits * (length(r) + 1) / length(r)
    }
    for (i in seq_along(labels)) res[[paste0("limit_", labels[i])]] <- unname(limits[i])
    res$n <- length(r)
    res
  })
  res <- do.call(rbind, out)
  if (is.null(res) || nrow(res) == 0) {
    stop("Too few complete samples to compute T-squared.", call. = FALSE)
  }
  res <- res[order(res$sample), , drop = FALSE]
  for (l in labels) {
    res[[paste0("outlier_", l)]] <- !is.na(res$t2) & res$t2 > res[[paste0("limit_", l)]]
  }
  if (!is.null(group_values)) {
    res <- tibble::add_column(res, group = group_values[res$sample], .after = "sample")
  }
  res[c(setdiff(names(res), "n"), "n")]
}

# ---- internals ---------------------------------------------------------------

# Limit of T-squared on k components for n samples: "f" (the conventional
# limit), "beta" (the samples of the model) or "new" (a new sample, with
# the uncertainty of the mean).
t2_limit <- function(level, k, n, method = "f") {
  switch(method,
         f = k * (n - 1) / (n - k) * stats::qf(level, k, n - k),
         beta = (n - 1)^2 / n * stats::qbeta(level, k / 2, (n - k - 1) / 2),
         new = k * (n + 1) * (n - 1) / (n * (n - k)) * stats::qf(level, k, n - k))
}

check_conf_level <- function(conf_level) {
  if (!is.numeric(conf_level) || length(conf_level) == 0 || anyNA(conf_level) ||
      any(conf_level <= 0 | conf_level >= 1)) {
    stop("'conf_level' must contain confidence levels between 0 and 1, such as c(0.95, 0.99).",
         call. = FALSE)
  }
  sort(unique(conf_level))
}

# 0.95 -> "95", 0.975 -> "97.5", as in the names of HotellingEllipse.
conf_label <- function(level) {
  sub("\\.?0+$", "", formatC(100 * level, format = "f", digits = 6))
}

# The first k embedding coordinates, or else the first k numeric columns.
embedding_components <- function(df, k) {
  numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  coords <- grep("^(UMAP|PC|Comp|Dim|tSNE|TSNE|IC|LV)_?[0-9]+$", numeric_cols, value = TRUE,
                 ignore.case = TRUE)
  pool <- if (length(coords) >= k) coords else numeric_cols
  if (length(pool) < k) {
    stop("'data' has fewer than ", k, " numeric columns for the components.", call. = FALSE)
  }
  pool[seq_len(k)]
}

# A column of df (bare name or string) or a vector with one value per row.
embedding_values <- function(quo, df, arg) {
  if (rlang::quo_is_null(quo)) return(list(values = NULL, name = NULL))
  expr <- rlang::quo_get_expr(quo)
  name <- if (rlang::is_symbol(expr)) rlang::as_string(expr) else if (is.character(expr) && length(expr) == 1) expr
  if (!is.null(name) && name %in% names(df)) {
    return(list(values = df[[name]], name = name))
  }
  values <- rlang::eval_tidy(quo)
  if (is.null(values)) return(list(values = NULL, name = NULL))
  if (length(values) != nrow(df)) {
    stop("`", arg, "` must be a column of 'data' or have one value per sample (", nrow(df), ").",
         call. = FALSE)
  }
  list(values = values, name = if (rlang::is_symbol(expr)) rlang::as_string(expr) else arg)
}
