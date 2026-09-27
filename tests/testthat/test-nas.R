# Mixtures of three constituents with known pure spectra; the analyte is the
# first one.
wl <- seq(390, 400, length.out = 150)
line <- function(center) exp(-(wl - center)^2 / 0.1)
pure <- rbind(line(393.4) + 0.3 * line(396.8), line(393.8), line(396.5))
make_mixtures <- function(n, seed, sd = 0) {
  set.seed(seed)
  conc <- matrix(stats::runif(n * 3), n, 3)
  x <- conc %*% pure + matrix(stats::rnorm(n * length(wl), sd = sd), n)
  colnames(x) <- as.character(wl)
  list(x = x, y = conc[, 1])
}
# Theoretical NAS of the analyte: its spectrum orthogonal to the interferents
interf <- t(pure[2:3, ])
net <- drop(pure[1, ] - interf %*% qr.solve(interf, pure[1, ]))

test_that("the sensitivity equals the theoretical net analyte signal", {
  d <- make_mixtures(30, 1)
  for (method in c("pls", "pcr")) {
    fit <- nas(d$x, d$y, ncomp = 3, method = method)
    expect_equal(fit$figures_of_merit[["sensitivity"]], sqrt(sum(net^2)), tolerance = 1e-8)
    # the regression vector is parallel to the net analyte spectrum
    cosine <- sum(fit$coefficients * net) / (sqrt(sum(fit$coefficients^2)) * sqrt(sum(net^2)))
    expect_equal(abs(cosine), 1, tolerance = 1e-8)
  }
})

test_that("the NAS of each sample is its concentration times the sensitivity", {
  d <- make_mixtures(30, 2)
  fit <- nas(d$x, d$y, ncomp = 3)
  sen <- fit$figures_of_merit[["sensitivity"]]
  expect_equal(fit$nas$nas, (d$y - mean(d$y)) * sen, tolerance = 1e-8)
  expect_equal(fit$nas$fitted, d$y, tolerance = 1e-8)
  expect_true(all(fit$nas$selectivity >= 0 & fit$nas$selectivity <= 1))
  # NAS vectors: nas_i * b / ||b||
  b <- fit$coefficients
  expect_equal(as.matrix(fit$nas_vectors), outer(fit$nas$nas, b / sqrt(sum(b^2))), ignore_attr = TRUE)
})

test_that("noise gives analytical sensitivity, LOD, LOQ and SNR", {
  d <- make_mixtures(30, 3, sd = 0.001)
  fit <- nas(d$x, d$y, ncomp = 3, noise = 0.001)
  fom <- fit$figures_of_merit
  expect_named(fom, c("sensitivity", "selectivity", "analytical_sensitivity", "lod", "loq"))
  expect_equal(fom[["analytical_sensitivity"]], fom[["sensitivity"]] / 0.001)
  expect_equal(fom[["lod"]], 3.3 * 0.001 / fom[["sensitivity"]])
  expect_equal(fom[["loq"]], 10 * 0.001 / fom[["sensitivity"]])
  expect_equal(fit$nas$snr, fit$nas$nas / 0.001)
  expect_output(print(fit), "LOD:")
  expect_output(print(nas(d$x, d$y, ncomp = 3)), "Give 'noise'")
})

test_that("predict() computes the NAS of new samples", {
  d <- make_mixtures(40, 4, sd = 0.001)
  fit <- nas(d$x[1:30, ], d$y[1:30], ncomp = 3, noise = 0.001)
  new <- predict(fit, d$x[31:40, ])
  expect_named(new, c("nas", "selectivity", "predicted", "snr"))
  model <- pls::plsr(y ~ x, ncomp = 3, method = "simpls", data = list(y = d$y[1:30], x = d$x[1:30, ]))
  expect_equal(new$predicted, drop(predict(model, newdata = list(x = d$x[31:40, ]), ncomp = 3)),
               ignore_attr = TRUE)
  on_cal <- predict(fit, d$x[1:30, ])
  expect_equal(on_cal$nas, fit$nas$nas)
})

test_that("nas validates its inputs", {
  d <- make_mixtures(10, 5)
  expect_error(nas(d$x, cbind(d$y, d$y)), "single response")
  expect_error(nas(d$x, d$y, noise = 0), "noise")
  expect_error(nas(d$x, d$y, method = "cls"))
  expect_warning(nas(d$x[1:4, ], d$y[1:4], ncomp = 5), "reduced")
  expect_equal(nas(as.data.frame(d$x), data.frame(y = d$y), ncomp = 2)$coefficients,
               nas(d$x, d$y, ncomp = 2)$coefficients)
})
