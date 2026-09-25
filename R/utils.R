# Internal helpers shared across the package. Not exported.

# Converts a matrix, data frame or tibble to a numeric matrix, keeping column
# names. Fails with an informative message for non-numeric input.
as_numeric_matrix <- function(x, arg = "x") {
  if (is.data.frame(x)) {
    if (!all(vapply(x, is.numeric, logical(1)))) {
      stop("'", arg, "' must contain only numeric columns.", call. = FALSE)
    }
    x <- as.matrix(x)
  } else if (is.vector(x) && is.numeric(x)) {
    x <- matrix(x, ncol = 1)
  }
  if (!is.matrix(x) || !is.numeric(x)) {
    stop("'", arg, "' must be a numeric matrix or data frame.", call. = FALSE)
  }
  storage.mode(x) <- "double"
  x
}

# Converts a response vector, matrix or data frame to a numeric matrix.
as_response_matrix <- function(y, n, arg = "y") {
  y <- as_numeric_matrix(y, arg)
  if (nrow(y) != n) {
    stop("'x' and '", arg, "' must have the same number of rows (observations).", call. = FALSE)
  }
  y
}

# Converts a matrix to a tibble, keeping column names or creating V1, V2, ...
# names silently when there are none.
as_tbl <- function(m, names = colnames(m)) {
  m <- as.matrix(m)
  if (is.null(names) || length(names) != ncol(m)) {
    names <- paste0("V", seq_len(ncol(m)))
  }
  colnames(m) <- names
  tibble::as_tibble(m, .name_repair = "minimal")
}

check_flag <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1 || is.na(x)) {
    stop("'", arg, "' must be a single logical value (TRUE or FALSE).", call. = FALSE)
  }
  invisible(x)
}

check_number <- function(x, arg, lower = -Inf, upper = Inf, lower_open = FALSE, upper_open = FALSE) {
  ok <- is.numeric(x) && length(x) == 1 && !is.na(x) &&
    (if (lower_open) x > lower else x >= lower) &&
    (if (upper_open) x < upper else x <= upper)
  if (!ok) {
    stop("'", arg, "' must be a single numeric value in ",
         if (lower_open) "(" else "[", lower, ", ", upper, if (upper_open) ")" else "]",
         ".", call. = FALSE)
  }
  invisible(x)
}

check_count <- function(x, arg, lower = 1) {
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || x %% 1 != 0 || x < lower) {
    stop("'", arg, "' must be a single integer >= ", lower, ".", call. = FALSE)
  }
  invisible(as.integer(x))
}

# Centers and/or scales the columns of a matrix, returning the parameters so
# they can be applied to new data.
preprocess <- function(x, center = TRUE, scale = FALSE) {
  mu <- if (center) colMeans(x) else rep(0, ncol(x))
  sdev <- if (scale) apply(x, 2, stats::sd) else rep(1, ncol(x))
  sdev[!is.finite(sdev) | sdev == 0] <- 1
  xs <- sweep(sweep(x, 2, mu, "-"), 2, sdev, "/")
  list(x = xs, center = mu, scale = sdev)
}

apply_preprocess <- function(x, pp) {
  sweep(sweep(x, 2, pp$center, "-"), 2, pp$scale, "/")
}

# Medcouple without the robustbase start-up message about 'doScale'.
medcouple <- function(x) {
  robustbase::mc(x[!is.na(x)], doScale = FALSE)
}

# Runs `expr` with a fixed RNG seed and restores the caller's RNG state.
with_seed <- function(seed, expr) {
  env <- globalenv()
  old <- if (exists(".Random.seed", envir = env, inherits = FALSE)) get(".Random.seed", envir = env) else NULL
  on.exit({
    if (is.null(old)) {
      if (exists(".Random.seed", envir = env, inherits = FALSE)) rm(".Random.seed", envir = env)
    } else {
      assign(".Random.seed", old, envir = env)
    }
  })
  set.seed(seed)
  expr
}

# Parses wavelength column names ("200.5", "X200.5", "wl_200.5") to numbers.
parse_wavelength <- function(nms) {
  num <- suppressWarnings(as.numeric(nms))
  bad <- is.na(num)
  if (any(bad)) {
    num[bad] <- suppressWarnings(as.numeric(sub("^[^0-9+-.]*", "", nms[bad])))
  }
  num
}
