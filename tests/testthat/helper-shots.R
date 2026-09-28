# Two samples of 8 shots, with a weak shot and a shot of another shape
shots_data <- function() {
  set.seed(42)
  wl <- seq(390, 400, by = 0.05)
  line1 <- exp(-(wl - 393.4)^2 / 0.01)
  line2 <- exp(-(wl - 396.8)^2 / 0.01)
  x <- t(sapply(rep(c(1, 2), each = 8), function(s) {
    1000 * s * (line1 + 0.5 * line2) * stats::runif(1, 0.95, 1.05) + stats::rnorm(length(wl), 50, 2)
  }))
  x[2, ] <- x[2, ] / 5                                   # weak shot
  x[12, ] <- 2000 * (0.75 * line1 + 0.75 * line2) + 50     # other line ratio, same intensity
  colnames(x) <- wl
  data.frame(Sample = rep(c("a", "b"), each = 8), Location = rep(1:8, 2), x, check.names = FALSE)
}
