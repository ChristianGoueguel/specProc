# Synthetic data shared by the tests.

# LIBS-like spectra: smooth continuum + emission lines + noise, one per row.
make_spectra <- function(n = 10, p = 200, seed = 1) {
  set.seed(seed)
  wl <- seq(390, 400, length.out = p)
  background <- t(sapply(seq_len(n), function(i) {
    50 + 5 * i + 0.8 * (wl - 390)^2
  }))
  lines <- t(sapply(seq_len(n), function(i) {
    (100 + 10 * i) * exp(-(wl - 393.4)^2 / 0.005) +
      (80 + 5 * i) * exp(-(wl - 396.8)^2 / 0.005)
  }))
  noise <- matrix(stats::rnorm(n * p, sd = 0.5), n, p)
  x <- background + lines + noise
  colnames(x) <- format(wl, nsmall = 3, trim = TRUE)
  list(x = x, wl = wl, background = background, lines = lines)
}

# Latent-variable regression data: X = T P + E, y driven by the first latent variable.
make_xy <- function(n = 40, p = 25, seed = 10) {
  set.seed(seed)
  t_mat <- matrix(stats::rnorm(n * 3), n)
  p_mat <- matrix(stats::rnorm(3 * p), 3)
  x <- t_mat %*% p_mat + matrix(stats::rnorm(n * p, sd = 0.1), n)
  colnames(x) <- paste0("v", seq_len(p))
  y <- t_mat[, 1] + stats::rnorm(n, sd = 0.05)
  list(x = x, y = y)
}

# Dense reference implementations of the penalized baselines (O(n^3)).
dense_als <- function(y, lambda, p, max_iter) {
  n <- length(y)
  d <- diff(diag(n), differences = 2)
  w <- rep(1, n)
  for (i in seq_len(max_iter)) {
    z <- solve(diag(w) + lambda * crossprod(d), w * y)
    w_new <- ifelse(y > z, p, 1 - p)
    if (all(w_new == w)) break
    w <- w_new
  }
  z
}

dense_arpls <- function(y, lambda, ratio, max_iter) {
  n <- length(y)
  d <- diff(diag(n), differences = 2)
  h <- lambda * crossprod(d)
  w <- rep(1, n)
  for (i in seq_len(max_iter)) {
    z <- solve(diag(w) + h, w * y)
    r <- y - z
    rn <- r[r < 0]
    m <- mean(rn)
    s <- stats::sd(rn)
    wt <- 1 / (1 + exp(2 * (r - (2 * s - m)) / s))
    if (sqrt(sum((w - wt)^2)) / sqrt(sum(w^2)) < ratio) break
    w <- wt
  }
  z
}

wide_spectrum <- function(wl, y) {
  df <- as.data.frame(t(y))
  names(df) <- wl
  df
}
