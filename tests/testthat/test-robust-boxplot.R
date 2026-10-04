# Generalized boxplot fences as in the Stata command robbox of Jann, Verardi and
# Vermandele (Bruffaerts et al., 2014), with the interquartile range as the
# scale of the first step, as in the paper.
robbox_fences <- function(x, alpha, bp = 0.1, delta = 0.1) {
  q <- function(v, p) unname(stats::quantile(v, p))
  p25 <- q(x, 0.25)
  p50 <- q(x, 0.5)
  p75 <- q(x, 0.75)
  scale <- p75 - p25
  v <- (x - p50) / scale
  rmin <- min(v)
  rscale <- 2 * delta + max(v) - rmin
  v <- stats::qnorm((v - rmin + delta) / rscale)
  qmed <- q(v, 0.5)
  qscale <- (q(v, 0.75) - q(v, 0.25)) / (stats::qnorm(0.75) - stats::qnorm(0.25))
  v <- (v - qmed) / qscale
  lo <- q(v, bp)
  up <- q(v, 1 - bp)
  z <- stats::qnorm(1 - bp)
  g <- log(-up / lo) / z
  h <- 2 / z^2 * log(-g * (lo * up) / (lo + up))
  tau <- function(u) (exp(g * u) - 1) / g * exp(h * u^2 / 2)
  b <- p50 + scale * (stats::pnorm(tau(stats::qnorm(c(alpha / 2, 1 - alpha / 2))) * qscale + qmed) *
                        rscale + rmin - delta)
  c(min(p25, b[1]), max(p75, b[2]), g, h)
}

test_that("generalized_boxplot follows Bruffaerts et al. (2014), as the authors' robbox", {
  # robbox's default rate: Tukey's boxplot for normal data
  default <- eval(formals(generalized_boxplot)$alpha)
  expect_equal(default, 2 * (1 - stats::pnorm(stats::qnorm(0.75) + 1.5 * (stats::qnorm(0.75) - stats::qnorm(0.25)))))
  expect_equal(default, 0.006976603, tolerance = 1e-6)
  set.seed(11)
  samples <- list(normal = stats::rnorm(300), skewed = stats::rexp(300), heavy = stats::rt(300, 3),
                  light = stats::runif(300))
  for (name in names(samples)) {
    x <- samples[[name]]
    ref <- robbox_fences(x, default)
    st <- generalized_stats(x, default, 0.9)$stats
    expect_equal(c(st$g, st$h), ref[3:4], info = name)
    # the same fences, but where the g-and-h transform turns back (h < 0)
    if (ref[4] >= 0 || 1 + ref[4] * stats::qnorm(1 - default / 2)^2 > 0.2) {
      expect_equal(c(st$lower_fence, st$upper_fence), ref[1:2], info = name)
    }
  }
  # negative h is kept (tails lighter than normal), not set to 0
  expect_lt(generalized_stats(samples$light, default, 0.9)$stats$h, 0)
})

test_that("generalized_boxplot flags about alpha of clean data", {
  set.seed(12)
  x <- data.frame(a = stats::rnorm(20000))
  st <- generalized_boxplot(x, plot = FALSE)$stats
  expect_equal(st$n_outliers / st$n, 0.007, tolerance = 0.3)
  st5 <- generalized_boxplot(x, alpha = 0.05, plot = FALSE)$stats
  expect_equal(st5$n_outliers / st5$n, 0.05, tolerance = 0.15)
  # fences are never inside the box
  tiny <- generalized_boxplot(x, alpha = 0.9, plot = FALSE)$stats
  expect_lte(tiny$lower_fence, tiny$q1)
  expect_gte(tiny$upper_fence, tiny$q3)
})

test_that("adjusted_boxplot follows Hubert and Vandervieren (2008)", {
  set.seed(13)
  for (x in list(stats::rexp(200), -stats::rexp(200), stats::rnorm(200))) {
    st <- adjusted_boxplot(data.frame(x = x), plot = FALSE)$stats
    hinges <- stats::fivenum(x)
    iqr <- hinges[4] - hinges[2]
    mc <- robustbase::mc(x, doScale = FALSE)
    fences <- if (mc >= 0) {
      c(hinges[2] - 1.5 * exp(-4 * mc) * iqr, hinges[4] + 1.5 * exp(3 * mc) * iqr)
    } else {
      c(hinges[2] - 1.5 * exp(-3 * mc) * iqr, hinges[4] + 1.5 * exp(4 * mc) * iqr)
    }
    expect_equal(c(st$lower_fence, st$upper_fence), fences)
    expect_equal(st$medcouple, mc)
    expect_equal(c(st$lower, st$upper), range(x[x >= fences[1] & x <= fences[2]]))
    expect_equal(st$n_outliers, sum(x < fences[1] | x > fences[2]))
    # McGill et al. (1978) notches
    expect_equal(c(st$notch_lower, st$notch_upper), hinges[3] + c(-1, 1) * 1.58 * iqr / sqrt(200))
  }
})

test_that("the boxplots trace the outliers to their rows and ids, by group", {
  set.seed(14)
  df <- data.frame(sample = paste0("s", 1:60), site = rep(c("A", "B", NA), each = 20),
                   a = c(stats::rnorm(19), 15, stats::rnorm(40)), b = stats::rexp(60))
  for (fn in list(adjusted_boxplot, generalized_boxplot)) {
    res <- fn(df, id = sample, group = site, plot = FALSE)
    expect_equal(nrow(res$stats), 4) # 2 variables x 2 groups; rows without a site left out
    expect_equal(as.character(res$stats$group), c("A", "B", "A", "B"))
    expect_equal(res$stats$n, c(20, 20, 20, 20))
    expect_equal(res$stats$mean[1], mean(df$a[1:20]))
    out <- res$outliers
    expect_true(all(c("variable", "group", "row", "id", "value", "out") %in% names(out)))
    expect_equal(out$value, unname(mapply(function(v, r) df[[v]][r], as.character(out$variable), out$row)))
    expect_equal(out$id, df$sample[out$row])
    expect_true(20 %in% out$row[out$variable == "a"])
    expect_equal(sum(res$stats$n_outliers), nrow(out))
  }
  expect_error(adjusted_boxplot(df, group = zz), "'group' column does not exist")
  expect_error(adjusted_boxplot(df[c("site", "a")]), "numeric data frame")
})

test_that("the boxplots draw one panel per variable, or a common axis", {
  df <- data.frame(a = stats::rnorm(50), b = 100 * stats::rexp(50))
  p <- adjusted_boxplot(df)
  expect_s3_class(p$facet, "FacetWrap")
  expect_true(p$facet$params$free$y)
  expect_s3_class(adjusted_boxplot(df, scales = "fixed")$facet, "FacetNull")
  expect_s3_class(adjusted_boxplot(df["a"])$facet, "FacetNull")
  # the number of values below each box
  built <- ggplot2::ggplot_build(p)
  expect_equal(built$layout$panel_params[[1]]$x$get_labels(), "n = 50")
  expect_equal(ggplot2::ggplot_build(adjusted_boxplot(df, scales = "fixed", show_n = FALSE))$layout$panel_params[[1]]$x$get_labels(),
               c("a", "b"))
  # horizontal boxes keep free axes
  h <- adjusted_boxplot(df, horizontal = TRUE)
  expect_equal(h$layers[[1]]$geom_params$orientation, "y")
  expect_s3_class(ggplot2::ggplotGrob(h), "gtable")
})

test_that("the boxplots show what is asked, with notches", {
  set.seed(15)
  df <- data.frame(a = c(stats::rnorm(48), 8, 9), b = stats::rexp(50))
  geoms <- function(p) unname(vapply(p$layers, function(l) class(l$geom)[1], character(1)))
  expect_equal(geoms(adjusted_boxplot(df)), c("GeomBoxplot", "GeomPoint"))
  expect_equal(geoms(adjusted_boxplot(df, points = "none")), "GeomBoxplot")
  expect_equal(geoms(adjusted_boxplot(df, points = "all", show_mean = TRUE, label_outliers = TRUE)),
               c("GeomPoint", "GeomBoxplot", "GeomPoint", "GeomText", "GeomPoint"))
  p <- adjusted_boxplot(df, notch = TRUE)
  expect_no_warning(built <- ggplot2::ggplot_build(p))
  expect_true(all(c("notchlower", "notchupper") %in% names(built$data[[1]])))
  expect_s3_class(ggplot2::ggplotGrob(p), "gtable")
  # log axis, fill colors, titles and caption
  lg <- adjusted_boxplot(df + 20, log = TRUE)
  expect_equal(lg$scales$get_scales("y")$get_transformation()$name, "log-10")
  colors <- ggplot2::ggplot_build(adjusted_boxplot(df, scales = "fixed", fill = c("red", "blue")))$data[[1]]$fill
  expect_equal(colors, c("red", "blue"))
  q <- generalized_boxplot(df, ylab = "Content (%)", title = "Contents")
  expect_equal(q$labels$y, "Content (%)")
  expect_equal(q$labels$title, "Contents")
  expect_match(q$labels$caption, "Generalized boxplot \\(Bruffaerts et al., 2014\\)")
  expect_match(q$labels$caption, "alpha = 0.7%")
  expect_match(adjusted_boxplot(df)$labels$caption, "Hubert and Vandervieren")
  expect_null(adjusted_boxplot(df, caption = FALSE)$labels$caption)
  expect_equal(adjusted_boxplot(df, caption = "Mine")$labels$caption, "Mine")
  # the colors do not depend on the packages installed
  expect_equal(unique(ggplot2::ggplot_build(adjusted_boxplot(df))$data[[1]]$fill), "grey85")
})
