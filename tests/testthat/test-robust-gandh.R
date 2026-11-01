# Deterministic normal quantiles avoid simulation-dependent assertions.
gh_reference <- function(n = 1000, A = 2, B = 3, g = 0.4, h = 0.15) {
  z <- stats::qnorm((seq_len(n) - 0.5) / n)
  list(x = A + B * if (g == 0) z * exp(h * z^2 / 2) else
         expm1(g * z) / g * exp(h * z^2 / 2), z = z)
}

test_that("rQLS recovers a known g-and-h distribution and latent scores", {
  ref <- gh_reference(2000, A = -5, B = 2.4, g = -0.45, h = 0.17)
  fit <- robust_gandh(ref$x, cutoff = 100, standardize = FALSE)
  expect_s3_class(fit, c("specproc_gandh", "specproc_transformation"))
  expect_named(fit, c("transformation", "summary", "fits", "standardize"))
  expect_equal(fit$summary$variable, "V1")
  expect_equal(fit$summary$location, -5, tolerance = 0.01)
  expect_equal(fit$summary$scale, 2.4, tolerance = 0.01)
  expect_equal(fit$summary$g, -0.45, tolerance = 0.01)
  expect_lt(abs(fit$summary$h - 0.17), 0.005)
  expect_gt(stats::cor(fit$transformation$V1, ref$z), 0.9999)
  expect_lt(max(abs(fit$transformation$V1 - ref$z)), 0.02)
  expect_true(fit$summary$converged)
  expect_true(is.na(fit$summary$tuning_met))
  expect_output(print(fit), "Tukey g-and-h.*rQLS")
})

test_that("adaptive rQLS excludes contamination from fitting but transforms it", {
  ref <- gh_reference()
  x <- c(ref$x, rep(1e4, 30))
  expect_no_warning(fit <- robust_gandh(x, standardize = FALSE))
  expect_equal(fit$summary$n_trimmed, 30)
  expect_true(fit$summary$tuning_met)
  expect_equal(fit$summary$location, 2, tolerance = 0.01)
  expect_equal(fit$summary$scale, 3, tolerance = 0.01)
  expect_equal(fit$summary$g, 0.4, tolerance = 0.01)
  expect_equal(fit$summary$h, 0.15, tolerance = 0.01)
  expect_length(fit$transformation$V1, length(x))
  expect_true(all(is.finite(fit$transformation$V1)))
  expect_true(all(fit$fits$V1$weights[1001:1030] == 0))
  expect_gt(min(fit$transformation$V1[1001:1030]), 5)
  expect_equal(predict(fit, fit$transformation, inverse = TRUE)$V1, x, tolerance = 1e-8)
  # Reordering rows cannot change the fitted transformation, and diagnostic
  # weights retain the order of the nonmissing training observations.
  idx <- rev(seq_along(x))
  shuffled <- robust_gandh(x[idx], standardize = FALSE)
  expect_equal(shuffled$summary, fit$summary)
  expect_equal(shuffled$fits$V1$weights, rev(fit$fits$V1$weights))
})

test_that("g-and-h transforms are stable in normal, lognormal and extreme-tail limits", {
  z <- c(-25, -10, -2, 0, 2, 10, 25, NA)
  for (g in c(-0.4, 0, 1e-12, 0.4)) {
    for (h in c(0, 0.2)) {
      # At h=0 an extremely compressed lognormal tail cannot be recovered
      # beyond floating point resolution; keep that round trip moderate.
      zz <- if (h == 0) c(-3, -1, 0, 1, 3, NA) else z
      x <- gandh_values(zz, -2, 3, g, h)
      expect_equal(gandh_scores(x, -2, 3, g, h), zz, tolerance = 1e-8)
    }
  }
  expect_equal(gandh_values(c(-1, 0, 1), 2, 3, 0, 0), c(-1, 2, 5))
  expect_equal(gandh_scores(c(-1, 2, 5), 2, 3, 0, 0), c(-1, 0, 1))
  expect_equal(gandh_scores(c(-2, -1, 0), 0, 1, 1, 0), c(NA, NA, 0))
  expect_equal(gandh_scores(c(0, 1, 2), 0, 1, -1, 0), c(0, NA, NA))
  expect_equal(gandh_scores(1e308, 0, 1, 4, 0), (log(4) + log(1e308)) / 4)
})

test_that("central initialization prevents masking at outer fitting quantiles", {
  ref <- gh_reference()
  for (side in c(-1, 1)) {
    expect_no_warning(fit <- robust_gandh(c(ref$x, rep(side * 1e4, 100))))
    expect_equal(fit$summary$n_trimmed, 100)
    expect_equal(fit$summary$g, 0.4, tolerance = 0.01)
    expect_equal(fit$summary$h, 0.15, tolerance = 0.02)
  }
})

test_that("the documented spectroscopy example converges without adaptive fallback", {
  x <- forageLIBS[1:300, c("Ca", "K", "P", "Mn")]
  expect_no_warning(fit <- robust_gandh(x))
  expect_true(all(fit$summary$converged))
  expect_true(all(fit$summary$tuning_met))
  expect_equal(predict(fit, fit$transformation, inverse = TRUE),
               tibble::as_tibble(x), tolerance = 1e-8)
})

test_that("column matching, missing values, inverse and smearing follow the shared API", {
  ref <- gh_reference(200)
  x <- data.frame(right = ref$x, left = -ref$x, constant = 1)
  x$right[2] <- NA
  for (std in c(TRUE, FALSE)) {
    fit <- robust_gandh(x, cutoff = 100, standardize = std)
    expect_equal(predict(fit, x), fit$transformation)
    expect_equal(predict(fit, fit$transformation, inverse = TRUE),
                 tibble::as_tibble(x), tolerance = 1e-8)
    expect_equal(predict(fit, x[c("left", "right")]), fit$transformation[c("right", "left")])
    expect_equal(predict(fit, x["left"]), fit$transformation["left"])
    expect_equal(fit$transformation$constant, rep(1, nrow(x)))
    expect_true(all(is.na(fit$summary[3, -1])))
    f <- fit$fits$left
    if (std) {
      keep <- f$weights > 0
      expect_equal(mean(fit$transformation$left[keep]), 0, tolerance = 1e-8)
      expect_equal(stats::sd(fit$transformation$left[keep]), 1, tolerance = 1e-8)
    }
    # Duan smearing integrates the inverse over the supplied residuals.
    e <- c(-0.2, 0, 0.2, NA)
    avg <- predict(fit, data.frame(left = c(-1, 1)), inverse = TRUE,
                   estimate = "mean", residuals = e)
    manual <- vapply(c(-1, 1), function(zi) {
      mean(invert_values(zi + e[1:3], f, std))
    }, numeric(1))
    expect_equal(avg$left, manual)
  }
  fit <- robust_gandh(x, cutoff = 100)
  expect_error(predict(fit, data.frame(unknown = 1)), "not found")
  expect_error(predict(fit, matrix(1, 2, 1)), "3 column")
  # Prediction must not refit, including a new extreme value.
  before <- fit$fits
  expect_equal(predict(fit, rbind(x[1, ], x[200, ]))[1, ], predict(fit, x[1, ]))
  expect_identical(fit$fits, before)
})

test_that("the transformation is equivariant to positive changes of units", {
  x <- gh_reference(250)$x
  a <- robust_gandh(x, cutoff = 100)
  b <- robust_gandh(17 + 1000 * x, cutoff = 100)
  expect_equal(a$transformation, b$transformation, tolerance = 1e-7)
  expect_equal(b$summary$location, 17 + 1000 * a$summary$location, tolerance = 1e-7)
  expect_equal(b$summary$scale, 1000 * a$summary$scale, tolerance = 1e-7)
})

test_that("unsupported values and numerical overflow are reported", {
  fit <- robust_gandh(gh_reference(100)$x, cutoff = 100, standardize = FALSE)
  fit$fits$V1[c("A", "B", "g", "h", "center", "scale")] <- list(0, 1, 1, 0, 0, 1)
  expect_warning(z <- predict(fit, c(-2, 0, NA))$V1, "outside the fitted g-and-h support")
  expect_equal(z, c(NA, 0, NA))
  expect_warning(back <- predict(fit, c(10000, 0, NA), inverse = TRUE)$V1,
                 "no back-transformation")
  expect_equal(back, c(NA, 0, NA))
})

test_that("invalid and degenerate inputs are handled explicitly", {
  expect_error(robust_gandh(), "Missing 'x'")
  expect_error(robust_gandh(letters), "numeric")
  expect_error(robust_gandh(data.frame(x = 1:20, label = letters[1:20])), "numeric")
  expect_error(robust_gandh(c(1:20, Inf)), "infinite")
  for (m in list(3, 21, 4.5, NA, Inf)) expect_error(robust_gandh(1:30, n_quantiles = m), "n_quantiles")
  for (c in list(0, -1, Inf, NA, c(1, 2))) expect_error(robust_gandh(1:30, cutoff = c), "cutoff")
  expect_error(robust_gandh(1:30, max_iter = 0), "max_iter")
  expect_error(robust_gandh(1:30, tol = Inf), "tol")
  expect_error(robust_gandh(1:30, standardize = 1), "standardize")
  expect_equal(robust_gandh(rep(NA_real_, 20))$transformation$V1, rep(NA_real_, 20))
  expect_equal(robust_gandh(1:5)$transformation$V1, as.numeric(1:5))
  expect_equal(robust_gandh(rep(2, 20))$transformation$V1, rep(2, 20))
  # Too little retained data or nonconvergence must not look like a fit.
  expect_warning(failed <- robust_gandh(gh_reference(100)$x, cutoff = 1e-15), "fit failed")
  expect_true(is.na(failed$summary$converged))
  expect_equal(failed$transformation$V1, gh_reference(100)$x)
})

test_that("g-and-h recipes use training fits and handle empty selections", {
  skip_if_not_installed("recipes")
  ref <- gh_reference(200)
  train <- data.frame(a = ref$x, b = -ref$x, label = rep("sample", 200))
  train$a[5] <- NA
  rec <- recipes::recipe(~ ., data = train) |>
    step_robust_gandh(recipes::all_numeric_predictors(), cutoff = 100)
  expect_named(generics::tidy(rec$steps[[1]]), c("terms", "location", "scale", "g", "h", "id"))
  trained <- recipes::prep(rec)
  direct <- robust_gandh(train[c("a", "b")], cutoff = 100)
  out <- recipes::bake(trained, new_data = train)
  expect_equal(out[c("a", "b")], direct$transformation)
  expect_equal(as.character(out$label), train$label)
  expect_equal(recipes::tidy(trained, number = 1)$g, direct$summary$g)
  new <- train[1:3, ]
  new$a[1] <- 1e5
  expect_equal(recipes::bake(trained, new_data = new)[c("a", "b")],
               predict(direct, new[c("a", "b")]))
  expect_identical(print(trained$steps[[1]]), trained$steps[[1]])
  expect_equal(nrow(generics::tunable(trained$steps[[1]])), 0L)
  expect_equal(generics::required_pkgs(trained$steps[[1]]), "specProc")
  empty <- recipes::recipe(~ ., data = train) |>
    step_robust_gandh(dplyr::starts_with("absent")) |>
    recipes::prep()
  expect_equal(recipes::bake(empty, new_data = train)[c("a", "b")],
               tibble::as_tibble(train[c("a", "b")]))
  expect_equal(nrow(recipes::tidy(empty, number = 1)), 0L)
})
