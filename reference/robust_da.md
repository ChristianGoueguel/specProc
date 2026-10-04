# Robust Linear and Quadratic Discriminant Analysis

Robust linear (LDA) and quadratic (QDA) discriminant analysis of Hubert
and Van Driessen (2004): the class centers and covariance matrices are
estimated by the minimum covariance determinant (MCD) estimator, so that
outlying observations of the training data have little influence on the
discriminant rules.

## Usage

``` r
robust_da(
  x,
  group,
  method = c("linear", "quadratic"),
  alpha = 0.75,
  prior = NULL,
  nsamp = 500
)
```

## Arguments

- x:

  A numeric matrix or data frame of the predictors, one observation per
  row.

- group:

  The classes of the observations: a factor, or a vector converted to
  one.

- method:

  The discriminant rule: `"linear"` (default) or `"quadratic"`.

- alpha:

  The fraction of observations of each class assumed to be regular (the
  MCD subset size), between 0.5 and 1. Default is 0.75.

- prior:

  The membership (prior) probabilities of the classes, in the order of
  the levels of `group` (or named by them). Default is the proportions
  of the regular training observations in each class.

- nsamp:

  The number of random subsets of the MCD. Default is 500.

## Value

An object of class `specproc_robust_da`, a list with:

- `center`: the robust centers of the classes (one row per class).

- `cov`: the common covariance matrix (linear rule), or a list of the
  covariance matrices of the classes (quadratic rule).

- `prior`: the membership probabilities.

- `rd`: the robust distance of each training observation to the center
  of its class, and `cutoff_rd` its cut-off.

- `weights`: 1 for the regular training observations, 0 for the
  outliers.

- `fitted`: the classes assigned to the training observations.

- `misclassification`: a tibble with the misclassification rate of each
  class on its regular training observations, and the overall rate
  weighted by the membership probabilities.

- `method`, `levels`, `alpha`.

Use
[predict()](https://christiangoueguel.com/specProc/reference/predict.specproc_robust_da.md)
for the classes and posterior probabilities of new observations.

## Details

**Quadratic rule.** The center \\\hat\mu_j\\ and covariance
\\\hat\Sigma_j\\ of each class are its reweighted MCD estimates. An
observation \\x\\ is assigned to the class with the largest discriminant
score \$\$d_j(x) = -\frac{1}{2} \log\|\hat\Sigma_j\| - \frac{1}{2} (x -
\hat\mu_j)^T \hat\Sigma_j^{-1} (x - \hat\mu_j) + \log p_j.\$\$

**Linear rule.** The classes share one covariance matrix: the reweighted
MCD of the observations centered by the MCD center of their class, whose
own center shifts the class centers. The scores are those of the
quadratic rule with this common covariance.

The membership (prior) probabilities \\p_j\\ are, by default, the
proportions of the regular observations of the training data in each
class: those within the cut-off \\\sqrt{\chi^2\_{p, 0.975}}\\ of the
robust distance to the center of their class. The posterior
probabilities are proportional to \\p_j\\ times the normal density of
each class.

The MCD needs more observations than variables in each class, so robust
discriminant analysis applies to low-dimensional data: line intensities
or ratios, or the robust principal component scores of spectra
([`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md),
[`step_robpca()`](https://christiangoueguel.com/specProc/reference/step_robpca.md)).
For whole spectra, see
[`rsimca()`](https://christiangoueguel.com/specProc/reference/rsimca.md).

The misclassification rates are estimated on the regular observations of
the training data (`misclassification`), which tends to underestimate
them: estimate them on test data or by cross-validation (for example
with the `"mcd"` engine of
[`parsnip::discrim_linear()`](https://parsnip.tidymodels.org/reference/discrim_linear.html)
and
[`parsnip::discrim_quad()`](https://parsnip.tidymodels.org/reference/discrim_quad.html)
in tidymodels).

This is an independent implementation of the published description. The
MCD uses random subsets, so use
[`set.seed()`](https://rdrr.io/r/base/Random.html) for reproducible
results.

## tidymodels

specProc adds the `"mcd"` engine to
[`parsnip::discrim_linear()`](https://parsnip.tidymodels.org/reference/discrim_linear.html)
(linear rule) and
[`parsnip::discrim_quad()`](https://parsnip.tidymodels.org/reference/discrim_quad.html)
(quadratic rule); `alpha`, `prior` and `nsamp` are engine arguments. The
engines are available once parsnip and specProc are both loaded, in
either order.

## References

- Hubert, M., Van Driessen, K. (2004). Fast and robust discriminant
  analysis. Computational Statistics and Data Analysis, 45(2):301-320.

- Rousseeuw, P.J., Van Driessen, K. (1999). A fast algorithm for the
  minimum covariance determinant estimator. Technometrics,
  41(3):212-223.

## See also

[`predict.specproc_robust_da()`](https://christiangoueguel.com/specProc/reference/predict.specproc_robust_da.md),
[`rsimca()`](https://christiangoueguel.com/specProc/reference/rsimca.md)

## Author

Christian L. Goueguel

## Examples

``` r
data(forageLIBS)
# forage samples with low and high calcium, from two Ca II lines
lines <- forageLIBS[c("393.3599236", "396.8602175")]
level <- cut(forageLIBS$Ca, c(-Inf, 0.6, Inf), labels = c("low", "high"))
set.seed(1)
fit <- robust_da(lines[1:300, ], level[1:300])
fit
#> Robust linear discriminant analysis (MCD)
#> 
#> Observations:   300 (9 outliers)
#> Variables:      2
#> Classes:        low, high
#> Prior:          0.46 0.54
#> 
#> Misclassification of the regular training observations:
#> # A tibble: 3 × 3
#>   class       n error
#>   <chr>   <int> <dbl>
#> 1 low       134 0.515
#> 2 high      157 0.312
#> 3 overall   291 0.405
table(predict(fit, lines[301:368, ]), level[301:368])
#>       
#>        low high
#>   low   13   22
#>   high   6   27
head(predict(fit, lines[301:368, ], type = "prob"))
#> # A tibble: 6 × 2
#>     low  high
#>   <dbl> <dbl>
#> 1 0.835 0.165
#> 2 0.759 0.241
#> 3 0.591 0.409
#> 4 0.724 0.276
#> 5 0.630 0.370
#> 6 0.740 0.260
```
