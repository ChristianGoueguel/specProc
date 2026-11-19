# Unbiased Median Absolute Deviation

This function calculates the Median Absolute Deviation (MAD) scale
estimator for a numeric vector, using the Park-Kim-Wang approach. The
method adds a small sample correction factor to make MAD unbiased at the
normal distribution.

## Usage

``` r
umad(x, method = "hayes", drop_na = TRUE, drop.na = deprecated())
```

## Arguments

- x:

  A numeric vector.

- method:

  A character string specifying the method to use for calculating the
  correction factor when the number of sample is more than 100. The
  available options are "hayes" (default) and "williams".

- drop_na:

  A logical value indicating whether to remove missing values (NA) from
  the calculations. If `TRUE` (the default), missing values will be
  removed. If `FALSE`, missing values will be included.

- drop.na:

  **\[deprecated\]** Use `drop_na`.

## Value

The MAD scale estimate for the input vector `x`.

## Details

The correction factor, \\C\\, is calculated differently based on the
sample size \\n\\:

- For \\n \> 100\\, \\C\\ is calculated using an analytical
  approximation proposed by either Hayes (2014) or Williams (2011).

- For \\n \leq 100\\, \\C\\ is obtained from a pre-computed table of
  values proposed by Park *et al.* (2020).

## References

- Park, C., Kim, H., Wang, M., (2020). Investigation of finite-sample
  properties of robust location and scale estimators. Communications in
  Statistics - Simulation and Computation, 51(5):2619–2645.

- Hayes, K., (2014). Finite-Sample Bias-Correction Factors for the
  Median Absolute Deviation.Communications in Statistics - Simulation
  and Computation, 43(10):2205–2212.

- Williams, D.C., (2011). Finite sample correction factors for several
  simple robust estimators of normal standard deviation. Journal of
  Statistical Computation and Simulation, 81(11):1697–1702.

## Author

Christian L. Goueguel

## Examples

``` r
# mad() is unbiased for the standard deviation of a normal distribution
# only in large samples. In 2000 samples of 5 replicate values from a
# standard normal distribution (standard deviation 1), mad() is low by
# about 16% on average, while umad() is unbiased
set.seed(1)
samples <- replicate(2000, rnorm(5), simplify = FALSE)
est <- data.frame(estimator = rep(c("mad()", "umad()"), each = 2000),
                  value = c(sapply(samples, mad), sapply(samples, umad)))
means <- tapply(est$value, est$estimator, mean)
round(means, 3)
#>  mad() umad() 
#>  0.836  1.017 
# the estimates from 5 values are skewed: the peak of umad() lies below 1,
# but its mean (solid line) is at 1 (dashed line)
ggplot2::ggplot(est, ggplot2::aes(value, fill = estimator)) +
  ggplot2::geom_density(alpha = 0.4) +
  ggplot2::geom_vline(xintercept = means, colour = c("#b2182b", "#2166ac")) +
  ggplot2::geom_vline(xintercept = 1, linetype = "dashed") +
  ggplot2::scale_fill_manual(values = c("#b2182b", "#2166ac"))


# the bias of mad() fades as the samples grow: the means of each
# estimator over 1000 samples of n values
set.seed(2)
bias <- sapply(c(3, 5, 10, 30, 100), function(n) {
  s <- replicate(1000, rnorm(n), simplify = FALSE)
  c(n = n, mad = mean(sapply(s, mad)), umad = mean(sapply(s, umad)))
})
round(bias, 3)
#>       [,1]  [,2]   [,3]   [,4]    [,5]
#> n    3.000 5.000 10.000 30.000 100.000
#> mad  0.691 0.816  0.921  0.973   0.986
#> umad 1.027 0.993  1.010  1.000   0.994

# both are robust: two outliers inflate the standard deviation, but
# neither MAD
set.seed(1)
x <- c(rnorm(50), 10, 12)
sd(x)
#> [1] 2.276614
mad(x)
#> [1] 0.7691122
umad(x)
#> [1] 0.7808216
```
