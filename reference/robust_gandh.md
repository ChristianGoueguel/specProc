# Robust Tukey g-and-h Transformation

Transforms each variable toward central normality by inverting a Tukey
g-and-h distribution fitted by robust quantile least squares (rQLS; Xu,
Iglewicz and Chervoneva, 2014). Accepts positive, zero and negative
values, and fits skewness and tail heaviness separately.

## Usage

``` r
robust_gandh(
  x,
  n_quantiles = 10,
  cutoff = NULL,
  max_iter = 50,
  tol = 1e-06,
  standardize = TRUE
)
```

## Arguments

- x:

  A numeric vector, matrix, data frame or tibble, one variable per
  column.

- n_quantiles:

  Number of fitting quantiles, an integer from 4 to 20. Default is 10,
  as recommended for rQLS in the paper.

- cutoff:

  Positive, finite Tukey biweight cutoff on the median/MAD standardized
  input scale. `NULL` (default) selects it adaptively for each variable.
  It is an order-statistic fitting tolerance, not a normal score cutoff
  or a contamination percentage.

- max_iter:

  Maximum number of trimming/refitting iterations per cutoff. Default is
  50.

- tol:

  Positive, finite relative tolerance for convergence. Default is 1e-6.

- standardize:

  Logical: additionally standardize latent scores using the fitting
  inliers (`TRUE`, default).

## Value

An object of class `specproc_gandh`, inheriting from
`specproc_transformation`, with the same components as
[`robust_boxcox()`](https://christiangoueguel.com/specProc/reference/robust_boxcox.md):

- `transformation`: A tibble of transformed values.

- `summary`: A tibble with `variable`, `location`, `scale` (both on the
  original input scale), `g`, `h`, `objective` (quantile mean squared
  error on the median/MAD input scale), `n_trimmed`, `cutoff` (on the
  standardized input scale), `converged` and `tuning_met`. Parameters
  and diagnostics are `NA` for variables left unchanged. `tuning_met` is
  `NA` when a cutoff was supplied.

- `fits`: Parameters and fitting diagnostics used by
  [`predict()`](https://rdrr.io/r/stats/predict.html).

- `standardize`: The `standardize` argument.

## Details

The fitted distribution maps a standard normal value \\z\\ to \$\$x =
A + B \frac{\exp(gz)-1}{g}\exp(hz^2/2),\$\$ with \\B \> 0\\ and \\h \ge
0\\. At \\g = 0\\, the fraction is \\z\\. The transformation solves this
equation for \\z\\ directly, without rounding extreme cumulative
probabilities to zero or one. The inverse transformation evaluates the
equation.

Each variable is first centered by its median and divided by its MAD.
QLS fits model quantiles to sample order statistics at probabilities
\\p_j = (j-1/3)/(m+1/3)\\. Initialization compares this QLS fit with a
second QLS fit using equally spaced probabilities from 0.15 to 0.85,
selecting the smaller median absolute order-statistic discrepancy. This
additional start reduces masking when contamination reaches an outer
fitting quantile. The rQLS iterations compare all order statistics with
their fitted quantiles, calculate Tukey biweight weights, and refit QLS
to quantiles of the observations with nonzero weights. The positive
weights are used for selection, not as quantile weights. All original
observations, including the trimmed ones, are transformed.

With `cutoff = NULL`, a bounded numerical version of the paper's
adaptive search is used: starting at least at 10 on the standardized
input scale, a geometric pilot search brackets the cutoffs retaining
more than half and all observations, followed by a descending grid with
the paper's grading interval and tail-weight ordering checks. At most 25
pilot and 51 grading fits are made. If no candidate meets the ordering
checks, the largest converged candidate retaining more than half the
observations is used, with a warning and `tuning_met = FALSE`. A
supplied cutoff bypasses this search. This numerical search need not
give exactly the same cutoff as the authors' implementation.

With `standardize = TRUE`, latent normal scores are additionally
centered and scaled by the mean and standard deviation of the fitting
inliers. With `FALSE`, the fitted latent scores are returned directly;
they already account for the distribution's location and scale.

Variables with fewer than `max(10, n_quantiles + 1)` observed values or
zero MAD are left unchanged. Failed fits are left unchanged with a
warning. Missing values are ignored by fitting and stay missing.
Infinite input values are rejected. This is a continuous-distribution
model; substantial ties, mixtures and censoring need separate
consideration. For \\h = 0, g \ne 0\\, new values outside the fitted
distribution's support become `NA` with a warning. Inverse values
exceeding floating point range also become `NA` with a warning.

## References

Xu, Y., Iglewicz, B., Chervoneva, I. (2014). Robust estimation of the
parameters of g-and-h distributions, with applications to outlier
detection. Computational Statistics & Data Analysis, 75:66-80.
[doi:10.1016/j.csda.2014.01.003](https://doi.org/10.1016/j.csda.2014.01.003)
.

## See also

[`robust_boxcox()`](https://christiangoueguel.com/specProc/reference/robust_boxcox.md),
[`robust_yeojohnson()`](https://christiangoueguel.com/specProc/reference/robust_yeojohnson.md),
[`tukey_gh()`](https://christiangoueguel.com/specProc/reference/tukey_gh.md),
[`predict.specproc_transformation()`](https://christiangoueguel.com/specProc/reference/predict.specproc_transformation.md),
[`step_robust_gandh()`](https://christiangoueguel.com/specProc/reference/step_robust_gandh.md)

## Author

Christian L. Goueguel

## Examples

``` r
data(forageLIBS)
contents <- forageLIBS[c("Ca", "K", "P", "Mn")]
fit <- robust_gandh(contents[1:300, ])
fit$summary
#> # A tibble: 4 × 10
#>   variable location   scale     g      h objective n_trimmed cutoff converged
#>   <chr>       <dbl>   <dbl> <dbl>  <dbl>     <dbl>     <dbl>  <dbl> <lgl>    
#> 1 Ca          0.614  0.171  0.319 0       0.000694         0   5    TRUE     
#> 2 K           1.98   0.479  0.148 0.0726  0.00174          0   9.85 TRUE     
#> 3 P           0.250  0.0541 0.115 0.0401  0.00105          0   5    TRUE     
#> 4 Mn         47.3   17.8    0.509 0       0.00179          0   5    TRUE     
#> # ℹ 1 more variable: tuning_met <lgl>
new <- predict(fit, contents[301:368, ])
head(predict(fit, new, inverse = TRUE))
#> # A tibble: 6 × 4
#>      Ca     K     P    Mn
#>   <dbl> <dbl> <dbl> <dbl>
#> 1 0.431  2.25 0.229    56
#> 2 0.805  1.91 0.210   112
#> 3 0.786  2.61 0.312    35
#> 4 0.807  2.11 0.232    41
#> 5 0.919  2.17 0.235   136
#> 6 0.323  1.61 0.206    49
```
