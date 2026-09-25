#' @title Classical or Robust Descriptive Statistics
#'
#' @description
#' This function calculates various descriptive statistics (robust and non-robust)
#' for a specified variable or all variables in a given data frame or tibble.
#'
#' @param x A data frame or tibble.
#' @param var A character vector of names (or a numeric vector of column positions) specifying the variable(s) for which to calculate the summary statistics. If left as \code{NULL} (the default), summary statistics will be calculated for all numeric variables in the data frame/tibble.
#' @param digits An integer specifying the number of significant digits to display after the decimal point in the output.
#' @param robust A logical value indicating whether to compute robust descriptive statistics. If \code{FALSE} (the default), computes the classical descriptive statistics for describing the distribution of a univariate variable.
#' @param drop.na A logical value indicating whether to remove missing values (\code{NA}) from the calculations. If \code{TRUE} (the default), missing values will be removed. If \code{FALSE}, the statistics of variables containing missing values are \code{NA}.
#'
#' @return A tibble with one row per variable. The classical statistics are the mean,
#' mode, median, IQR, standard deviation, variance, coefficient of variation
#' (`cv`, in \%), range, skewness, kurtosis and count. The robust statistics are
#' the median, MAD (scaled to be consistent at the normal distribution), Qn and Sn
#' estimators, medcouple, left/right medcouples, biweight location, scale and
#' midvariance, robust coefficient of variation (`rcv` = MAD / median, in \%) and count.
#' Robust statistics that cannot be computed (e.g. for a constant variable) are `NA`.
#'
#' @author Christian L. Goueguel
#'
#' @export summary_stats
#'
#' @examples
#' # Load the iris dataset
#' data(iris)
#'
#' # Example1:
#' iris |> summary_stats()
#'
#' # Example2:
#' iris |> summary_stats(
#'   var = c("Sepal.Length", "Petal.Length"),
#'   robust = TRUE
#'   )
#'
summary_stats <- function(x, var = NULL, digits = 2, robust = FALSE, drop.na = TRUE) {

  if (is.null(x) == TRUE) {
    stop("Data must be provided")
  }
  if (is.data.frame(x) == FALSE & tibble::is_tibble(x) == FALSE) {
    stop("Data must be of class data.frame, tbl_df, or tbl")
  }
  if (!is.null(var)) {
    if (!is.character(var) && !is.numeric(var)) {
      stop("'var' must be either a character vector or a numeric vector")
    }
    if (is.character(var) && !all(var %in% names(x))) {
      stop("One or more variables specified in 'var' are not present in the data")
    }
    if (is.numeric(var)) {
      if (any(var < 1 | var > ncol(x) | var %% 1 != 0)) {
        stop("One or more variables specified in 'var' are not present in the data")
      }
      var <- names(x)[var]
    }
    if (!all(vapply(x[var], is.numeric, logical(1)))) {
      stop("All variables specified in 'var' must be numeric")
    }
  }
  if (!is.logical(drop.na)) {
    stop("'drop.na' must be a logical value (TRUE or FALSE)")
  }
  if (!is.numeric(digits) || digits < 0 || digits %% 1 != 0) {
    stop("'digits' must be a non-negative integer")
  }
  if (!is.logical(robust)) {
    stop("'robust' must be a logical value (TRUE or FALSE)")
  }

  variable <- value <- NULL

  getmode <- function(vec) {
    unique_x <- unique(vec)
    unique_x[which.max(tabulate(match(vec, unique_x)))]
  }
  # Robust estimators error on degenerate input (e.g. constant variables);
  # report NA for that statistic instead of failing the whole summary.
  safe <- function(expr) tryCatch(expr, error = function(e) NA_real_)

  classical <- function(v) {
    m <- mean(v)
    s <- stats::sd(v)
    tibble::tibble(
      mean = round(m, digits),
      mode = round(getmode(v), digits),
      median = round(stats::median(v), digits),
      IQR = round(stats::IQR(v), digits),
      sd = round(s, digits),
      variance = round(stats::var(v), digits),
      cv = round((s / m) * 100, digits),
      min = min(v),
      max = max(v),
      range = max(v) - min(v),
      skewness = round(moments::skewness(v), digits),
      kurtosis = round(moments::kurtosis(v), digits),
      count = length(v)
    )
  }

  robust_stats <- function(v) {
    med <- stats::median(v)
    mad_v <- stats::mad(v)
    mw <- safe(medcouple_weight(v))
    tibble::tibble(
      median = round(med, digits),
      mad = round(mad_v, digits),
      Qn = round(safe(rousseeuw_croux(v, estimator = "Qn")), digits),
      Sn = round(safe(rousseeuw_croux(v, estimator = "Sn")), digits),
      medcouple = round(safe(medcouple(v)), digits),
      LMC = round(if (is.data.frame(mw)) mw$LMC else NA_real_, digits),
      RMC = round(if (is.data.frame(mw)) mw$RMC else NA_real_, digits),
      biloc = round(safe(biweight_location(v)), digits),
      biscale = round(safe(biweight_scale(v)), digits),
      bivar = round(safe(biweight_midvariance(v)), digits),
      rcv = round((mad_v / med) * 100, digits),
      count = length(v)
    )
  }

  num <- x[vapply(x, is.numeric, logical(1))]
  if (!is.null(var)) {
    num <- num[var]
  }
  if (ncol(num) == 0) {
    stop("No numeric variables to summarize.")
  }

  fct <- if (robust) robust_stats else classical
  res <- lapply(names(num), function(nm) {
    v <- num[[nm]]
    if (drop.na) {
      v <- v[!is.na(v)]
    }
    if (length(v) == 0) {
      stop("Variable '", nm, "' has no non-missing values.", call. = FALSE)
    }
    if (anyNA(v)) {
      out <- fct(stats::na.omit(v))
      out[] <- lapply(out, function(col) if (is.numeric(col)) NA_real_ else col)
      out$count <- length(v)
      out
    } else {
      fct(v)
    }
  })
  names(res) <- names(num)
  dplyr::bind_rows(res, .id = "variable")
}
