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
# iris: train on 100 flowers, predict the 50 others
set.seed(1)
train <- sample(nrow(iris), 100)
fit <- robust_da(iris[train, 1:4], iris$Species[train])
fit
#> Robust linear discriminant analysis (MCD)
#> 
#> Observations:   100 (12 outliers)
#> Variables:      4
#> Classes:        setosa, versicolor, virginica
#> Prior:          0.341 0.352 0.307
#> 
#> Misclassification of the regular training observations:
#> # A tibble: 4 × 3
#>   class          n  error
#>   <chr>      <int>  <dbl>
#> 1 setosa        30 0     
#> 2 versicolor    31 0.0323
#> 3 virginica     27 0.0370
#> 4 overall       88 0.0227
table(predicted = predict(fit, iris[-train, 1:4]), true = iris$Species[-train])
#>             true
#> predicted    setosa versicolor virginica
#>   setosa         16          0         0
#>   versicolor      0         18         0
#>   virginica       0          1        15
head(predict(fit, iris[-train, 1:4], type = "prob"))
#> # A tibble: 6 × 3
#>   setosa versicolor virginica
#>    <dbl>      <dbl>     <dbl>
#> 1      1   1.01e-23  3.70e-46
#> 2      1   2.46e-20  9.10e-42
#> 3      1   2.35e-28  3.66e-52
#> 4      1   6.15e-25  8.94e-48
#> 5      1   2.20e-18  3.16e-39
#> 6      1   1.53e-29  8.77e-54
```
