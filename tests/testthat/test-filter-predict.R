d <- make_xy(n = 40, p = 25)
cal <- 1:30
x_cal <- d$x[cal, ]
y_cal <- d$y[cal]
x_new <- d$x[-cal, ]

fits <- list(
  epo = epo(x_cal, ncomp = 2),
  epo_clutter = epo(x_cal, ncomp = 2, clutter = x_cal[1:5, ] - x_cal[6:10, ]),
  osc_wold = osc(x_cal, y_cal, method = "wold", ncomp = 2),
  osc_sjoblom = osc(x_cal, y_cal, method = "sjoblom", ncomp = 2),
  osc_fearn = osc(x_cal, y_cal, method = "fearn", ncomp = 2),
  osc_scaled = osc(x_cal, y_cal, method = "sjoblom", ncomp = 2, scale = TRUE),
  direct_orthogonal = direct_orthogonal(x_cal, y_cal, ncomp = 2),
  direct_osc = direct_osc(x_cal, y_cal, ncomp = 2),
  projected_osc = projected_osc(x_cal, y_cal, ncomp = 3),
  o2pls = o2pls(x_cal, y_cal, ncomp = 1, nx = 2)
)

test_that("predict() on the calibration data reproduces the correction", {
  for (name in names(fits)) {
    expect_equal(predict(fits[[name]], x_cal), fits[[name]]$correction, info = name)
  }
})

test_that("filters keep their fields and gain a class", {
  expect_s3_class(fits$osc_fearn, c("specproc_osc", "specproc_filter"))
  expect_s3_class(fits$epo, "specproc_epo")
  expect_s3_class(fits$o2pls, "o2pls")
  expect_named(fits$direct_orthogonal, c("correction", "loading", "score", "center", "scale"))
})

test_that("predict() corrects new data with the calibration model", {
  # POSC has its own newdata argument: both must agree
  posc <- projected_osc(x_cal, y_cal, ncomp = 3, newdata = x_new)
  expect_equal(predict(posc, x_new), posc$newdata$correction)

  # DOSC: X_new - X_new W P' after centering
  fit <- fits$direct_osc
  z <- sweep(x_new, 2, fit$center)
  expected <- z - z %*% as.matrix(fit$weight) %*% t(as.matrix(fit$loading))
  expect_equal(as.matrix(predict(fit, x_new)), expected, ignore_attr = TRUE)

  # EPO: no centering
  v <- as.matrix(fits$epo$loadings)
  expect_equal(as.matrix(predict(fits$epo, x_new)), x_new - x_new %*% v %*% t(v), ignore_attr = TRUE)
})

test_that("predict() matches columns by name and checks them", {
  shuffled <- x_new[, rev(colnames(x_new))]
  expect_equal(predict(fits$direct_osc, shuffled), predict(fits$direct_osc, x_new))
  expect_equal(predict(fits$direct_osc, as.data.frame(x_new)), predict(fits$direct_osc, x_new))

  renamed <- x_new
  colnames(renamed)[1] <- "other"
  expect_error(predict(fits$direct_osc, renamed), "do not match")
  expect_error(predict(fits$direct_osc, x_new[, -1]), "25 columns")
})

test_that("filters print a short summary", {
  expect_output(print(fits$osc_fearn), "method = \"fearn\"")
  expect_output(print(fits$epo), "Components removed:   2")
})
