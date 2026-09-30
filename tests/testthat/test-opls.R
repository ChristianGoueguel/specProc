d <- make_xy()

test_that("opls validates its inputs", {
  expect_error(opls(NULL, d$y), "must be provided")
  expect_error(opls(d$x, NULL), "must be provided")
  expect_error(opls(list(1, 2, 3), d$y), "numeric matrix or data frame")
  expect_error(opls(d$x, cbind(d$y, d$y)), "single response")
  expect_error(opls(d$x, d$y[-1]), "same number of rows")
  expect_error(opls(d$x, d$y, scale = "unit"), "should be one of")
  expect_error(opls(d$x, d$y, crossval = 1), "'crossval'")
  expect_error(opls(d$x, d$y, crossval = 0), "requires a fixed")
  expect_error(opls(d$x, d$y, ncomp.ortho = 25), "cannot exceed")
})

test_that("opls returns the model components", {
  res <- opls(as.data.frame(d$x), data.frame(y = d$y), ncomp.ortho = 2, permutation = 0)
  expect_s3_class(res, c("specproc_opls", "specproc_filter"))
  expect_equal(dim(res$x_scores), c(40L, 1L))
  expect_equal(dim(res$orthoScores), c(40L, 2L))
  expect_equal(dim(res$orthoLoadings), c(25L, 2L))
  expect_equal(res$components$component, c("p1", "o1", "o2"))
  expect_equal(res$summary$ort, 2L)
  expect_equal(names(res$vip), colnames(d$x))
  # the orthogonal scores are uncorrelated with y, and the filtered data are
  # those of the other OPLS implementations
  expect_equal(drop(crossprod(as.matrix(res$orthoScores), d$y - mean(d$y))), c(o1 = 0, o2 = 0),
               tolerance = 1e-8)
  expect_equal(as.matrix(res$correction), as.matrix(projected_osc(d$x, d$y, ncomp = 3)$correction),
               tolerance = 1e-8)
  expect_output(print(res), "Orthogonal components:   2")
})

test_that("opls selects the significant orthogonal components", {
  res <- opls(d$x, d$y, permutation = 0)
  expect_equal(res$summary$ort, 1L)
  expect_true(all(res$components$significance == "R1"))
  set.seed(1)
  expect_error(opls(d$x, stats::rnorm(40), permutation = 0), "not significant")
})

test_that("opls predicts new data with the calibration model", {
  cal <- 1:30
  fit <- opls(d$x[cal, ], d$y[cal], ncomp.ortho = 2, permutation = 0)
  expect_equal(predict(fit, d$x[cal, ]), fit$correction, tolerance = 1e-10)
  expect_equal(predict(fit, d$x[cal, ], type = "response"), fit$fitted, tolerance = 1e-10)
  scores <- predict(fit, d$x[cal, ], type = "scores")
  expect_equal(unname(as.matrix(scores)), unname(cbind(as.matrix(fit$x_scores), as.matrix(fit$orthoScores))),
               tolerance = 1e-10)
  expect_equal(length(predict(fit, d$x[-cal, ], type = "response")), 10L)
  expect_error(predict(fit, d$x[, 1:3]), "25 columns")
})

test_that("opls without cross-validation gives the same model", {
  a <- opls(d$x, d$y, ncomp.ortho = 2, permutation = 0)
  b <- opls(d$x, d$y, ncomp.ortho = 2, crossval = 0, permutation = 0)
  expect_equal(b$correction, a$correction)
  expect_true(is.na(b$summary$`Q2(cum)`))
})

test_that("opls reproduces ropls", {
  skip_if_not_installed("ropls")
  ropls_fit <- function(...) {
    suppressWarnings(suppressMessages(ropls::opls(
      d$x, d$y, predI = 1, algoC = "nipals", fig.pdfC = "none", info.txtC = "none", ...
    )))
  }
  cases <- list(
    list(scale = "center", ortho = 2, crossval = 7, perm = 0),
    list(scale = "pareto", ortho = 1, crossval = 5, perm = 10),
    list(scale = "standard", ortho = NA, crossval = 7, perm = 10)
  )
  for (cs in cases) {
    set.seed(5)
    m <- ropls_fit(orthoI = cs$ortho, scaleC = cs$scale, crossvalI = cs$crossval, permI = cs$perm)
    set.seed(5)
    f <- opls(d$x, d$y, scale = cs$scale, crossval = cs$crossval, permutation = cs$perm,
              ncomp.ortho = cs$ortho)
    expect_equal(signif(unlist(f$summary), 3), unlist(m@summaryDF))
    expect_equal(as.matrix(f$x_scores), m@scoreMN, tolerance = 1e-8, ignore_attr = TRUE)
    expect_equal(as.matrix(f$orthoScores), m@orthoScoreMN, tolerance = 1e-8, ignore_attr = TRUE)
    expect_equal(as.matrix(f$orthoLoadings), m@orthoLoadingMN, tolerance = 1e-8, ignore_attr = TRUE)
    expect_equal(f$vip, m@vipVn, tolerance = 1e-8, ignore_attr = TRUE)
    expect_equal(f$ortho_vip, m@orthoVipVn, tolerance = 1e-8, ignore_attr = TRUE)
    expect_equal(f$fitted, drop(m@suppLs$yPreMN), tolerance = 1e-8, ignore_attr = TRUE)
    expect_equal(signif(f$components$R2Y, 3), m@modelDF$R2Y[seq_len(nrow(f$components))])
    expect_equal(signif(f$components$Q2, 3), m@modelDF$Q2[seq_len(nrow(f$components))])
  }
})
