# Spectra of three groups, with lines at different wavelengths
make_groups <- function(n_per = 20, seed = 1) {
  set.seed(seed)
  wl <- seq(400, 405, length.out = 60)
  line <- function(center) exp(-(wl - center)^2 / 0.02)
  centers <- c(401, 402.5, 404)
  x <- do.call(rbind, lapply(centers, function(cc) {
    t(replicate(n_per, stats::runif(1, 80, 120) * line(cc) + 20 * line(403.3) +
                  stats::rnorm(length(wl), sd = 1)))
  }))
  colnames(x) <- format(wl, nsmall = 3)
  list(x = x, group = rep(1:3, each = n_per), wl = wl, line = line)
}

test_that("som separates groups of spectra and keeps them together", {
  d <- make_groups()
  fit <- som(d$x, grid = c(6, 4))
  expect_s3_class(fit, "specproc_som")
  expect_equal(dim(fit$codebook), c(24L, 60L))
  expect_equal(nrow(fit$grid), 24L)
  # each group on its own units, far from the other groups on the map
  xy <- as.matrix(fit$grid[fit$unit, c("x", "y")])
  centers <- apply(xy, 2, function(v) tapply(v, d$group, mean))
  within <- mean(sapply(1:3, function(g) mean(stats::dist(xy[d$group == g, ]))))
  expect_gt(min(stats::dist(centers)), 2 * within)
  expect_equal(length(intersect(fit$unit[d$group == 1], fit$unit[d$group == 2])), 0)
  # topographic errors: some spectra have their second unit across the empty
  # units between the groups
  expect_lt(fit$topographic_error, 0.3)
  expect_output(print(fit), "Self-organizing map")
})

test_that("one batch epoch matches its definition", {
  d <- make_groups(n_per = 5)
  x <- d$x
  layout <- som_grid(3, 2, "rectangular")
  set.seed(2)
  start <- x[sample.int(nrow(x), 6), ]
  d2 <- as.matrix(stats::dist(layout[c("x", "y")]))^2
  out <- som_batch_cpp(x, start, d2, 1.5, FALSE, 2.5)
  # reference: BMU, Gaussian kernel, weighted means
  bmu <- apply(x, 1, function(r) which.min(colSums((t(start) - r)^2)))
  h <- exp(-d2 / (2 * 1.5^2))
  ref <- t(sapply(1:6, function(j) colSums(h[j, bmu] * x) / sum(h[j, bmu])))
  expect_equal(out$codebook, unname(ref), tolerance = 1e-10)
})

test_that("som maps new spectra and flags the novel ones", {
  d <- make_groups()
  fit <- som(d$x, grid = c(6, 4))
  mapped <- predict(fit, d$x)
  expect_equal(mapped$unit, fit$unit)
  expect_equal(mapped$qe, fit$qe, tolerance = 1e-8)
  expect_false(any(mapped$novel[1:5]))
  # a spectrum with a line at another wavelength fits no unit
  novel <- 100 * d$line(400.3) + 20 * d$line(403.3)
  expect_true(predict(fit, rbind(novel))$novel)
  expect_error(predict(fit, d$x[, 1:10]), "columns")
})

test_that("the robust som down-weights outlying spectra", {
  d <- make_groups()
  x <- d$x
  x[1:3, ] <- x[1:3, ] + 300 * d$line(400.3)
  fit <- som(x, grid = c(6, 4), robust = TRUE)
  expect_true(all(fit$weights[1:3] < 1))
  expect_gt(stats::median(fit$weights[-(1:3)]), 0.99)
  expect_null(som(x, grid = c(6, 4))$weights)
})

test_that("som chooses a grid and handles its options", {
  d <- make_groups()
  fit <- som(d$x)
  # about 5 sqrt(n) units
  expect_true(abs(nrow(fit$grid) - 5 * sqrt(60)) <= 6)
  rect <- som(d$x, grid = c(5, 4), topology = "rectangular", init = "random")
  expect_equal(rect$topology, "rectangular")
  expect_true(all(rect$grid$x == rect$grid$col))
  expect_error(som(d$x, grid = c(1, 1)), "grid")
  expect_error(som(d$x, radius = c(2, -1)), "radius")
  x_na <- d$x
  x_na[1, 1] <- NA
  expect_error(som(x_na), "missing")
})

test_that("plot_som draws every type of plot", {
  d <- make_groups()
  fit <- som(d$x, grid = c(6, 4))
  for (type in c("counts", "umatrix", "quality")) {
    expect_s3_class(plot_som(fit, type = type), "ggplot")
  }
  comp <- plot_som(fit, type = "component", variables = c(401, 404))
  expect_equal(levels(comp$data$variable), c("401 nm", "404 nm"))
  expect_equal(range(comp$data$value), c(0, 1))
  expect_s3_class(plot_som(fit, type = "mapping", colour = factor(d$group), newdata = d$x[1:3, ]),
                  "ggplot")
  proto <- plot_som(fit, type = "prototypes")
  expect_equal(nlevels(proto$data$unit), 6L)
  expect_error(plot_som(fit, type = "component"), "variables")
  expect_error(plot_som(fit, type = "mapping", colour = 1:3), "one value per")
  expect_error(plot_som(list()), "som")
})

test_that("som_stability measures the agreement of resampled maps", {
  d <- make_groups()
  set.seed(3)
  st <- som_stability(d$x, runs = 3, grid = c(6, 4))
  expect_named(st, c("runs", "stability", "correlations"))
  expect_equal(nrow(st$runs), 3L)
  expect_gt(st$stability, 0.6)
  expect_equal(dim(st$correlations), c(3L, 3L))
})
