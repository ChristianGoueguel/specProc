#' @title Hotelling's T-squared Statistic of Samples in an Embedding
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Computes Hotelling's \eqn{T^2} statistic of each sample from its scores
#' on the first `k` components of an embedding (such as PCA or PLS scores),
#' with the 95% and 99% limits, for all the samples together or within
#' groups. Samples beyond the
#' limits are far from the center of the data (or of their group) given its
#' covariance: candidate outliers.
#'
#' @details
#' \eqn{T^2} is the squared Mahalanobis distance of a sample to the mean of
#' the scores, with their covariance. Its limit at the confidence level
#' \eqn{1 - \alpha} is \eqn{k(n - 1)/(n - k)\, F_{1-\alpha}(k, n - k)} for
#' \eqn{n} samples. Within groups (`group`), each group gets its own mean,
#' covariance and limits, which tells whether a sample is typical of its
#' own group rather than of the whole data set; each group needs more than
#' `k + 1` samples.
#'
#' The statistic uses the `k` components given, not only the two shown on a
#' plot: a sample can lie inside the ellipse of two components and still
#' exceed the limit in `k` dimensions. Classical estimates are themselves sensitive to outliers; with many
#' outliers, prefer the robust distances of [robpca()].
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
#'
#' @return A tibble with one row per sample: its row number `sample`, the
#'   `group` (with `group`), `t2`, the limits `limit_95` and `limit_99`,
#'   `outlier_95` and `outlier_99`, and the number of samples `n` of its
#'   group (or of the data).
#'
#' @references
#'  - Hotelling, H. (1931). The generalization of Student's ratio. The
#'    Annals of Mathematical Statistics, 2(3):360-378.
#'  - Jackson, J.E. (1991). A User's Guide to Principal Components. Wiley,
#'    New York.
#'
#' @seealso [plot_embedding()], [robpca()], [plot_outlier_map()]
#' @export hotelling_t2
#'
#' @examples
#' data(soilLIBS)
#' spectra <- average(soilLIBS[-(2:8)], Sample)
#' pca <- stats::prcomp(spectra[-1], scale. = TRUE)
#' t2 <- hotelling_t2(pca, k = 3)
#' t2[t2$outlier_95, ]
hotelling_t2 <- function(data, columns = NULL, k = 2, group = NULL) {
  df <- embedding_data(data)
  group_values <- embedding_values(rlang::enquo(group), df, "group")$values
  if (is.null(columns)) {
    check_count(k, "k", lower = 2)
    columns <- embedding_components(df, k)
  } else if (!is.character(columns) || !all(columns %in% names(df)) ||
             !all(vapply(df[columns], is.numeric, logical(1))) || length(columns) < 2) {
    stop("'columns' must name at least 2 numeric columns of 'data'.", call. = FALSE)
  }
  scores <- as.data.frame(df[columns])
  rows <- if (is.null(group_values)) list(all = seq_len(nrow(df))) else
    split(seq_len(nrow(df)), as.character(group_values))
  out <- lapply(names(rows), function(g) {
    r <- rows[[g]]
    r <- r[stats::complete.cases(scores[r, , drop = FALSE])]
    empty <- tibble::tibble(sample = r, t2 = NA_real_, limit_95 = NA_real_, limit_99 = NA_real_,
                            n = length(r))
    if (length(r) <= length(columns) + 1) {
      if (!is.null(group_values)) {
        warning("No T-squared for group ", g, ": it needs more than ", length(columns) + 1,
                " samples.", call. = FALSE)
      }
      return(empty)
    }
    x <- as.matrix(scores[r, , drop = FALSE])
    t2 <- tryCatch(stats::mahalanobis(x, colMeans(x), stats::cov(x)), error = function(e) {
      stop("The covariance of the components", if (!is.null(group_values)) paste0(" in group ", g),
           " is singular: use fewer components.", call. = FALSE)
    })
    tibble::tibble(sample = r, t2 = unname(t2), limit_95 = t2_limit(0.95, length(columns), length(r)),
                   limit_99 = t2_limit(0.99, length(columns), length(r)), n = length(r))
  })
  res <- do.call(rbind, out)
  if (is.null(res) || nrow(res) == 0) {
    stop("Too few complete samples to compute T-squared.", call. = FALSE)
  }
  res <- res[order(res$sample), , drop = FALSE]
  res$outlier_95 <- !is.na(res$t2) & res$t2 > res$limit_95
  res$outlier_99 <- !is.na(res$t2) & res$t2 > res$limit_99
  if (!is.null(group_values)) {
    res <- tibble::add_column(res, group = group_values[res$sample], .after = "sample")
  }
  res[c(setdiff(names(res), "n"), "n")]
}

# ---- internals ---------------------------------------------------------------

# Limit of T-squared on k components for n samples.
t2_limit <- function(level, k, n) {
  k * (n - 1) / (n - k) * stats::qf(level, k, n - k)
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
