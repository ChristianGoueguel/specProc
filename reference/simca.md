# SIMCA Classification Model (parsnip)

`simca()` defines a soft independent modeling of class analogy (SIMCA)
classification model for tidymodels: a principal component model of each
class, and the assignment of each observation to the closest class. Its
engine `"rsimca"` (the default) is the robust SIMCA of
[`rsimca()`](https://christiangoueguel.com/specProc/reference/rsimca.md),
whose class models resist outlying spectra of the training data.

## Usage

``` r
simca(
  mode = "classification",
  num_comp = NULL,
  gamma = NULL,
  squared = NULL,
  engine = "rsimca"
)

# S3 method for class 'simca'
update(
  object,
  parameters = NULL,
  num_comp = NULL,
  gamma = NULL,
  squared = NULL,
  fresh = FALSE,
  ...
)
```

## Arguments

- mode:

  The type of model: `"classification"`, the only one.

- num_comp:

  The number of components of the PCA model of each class, or `NULL`
  (default) to let the engine choose them.

- gamma:

  The weight of the orthogonal distances in the classification rule,
  between 0 and 1, or `NULL` (default) for 0.5.

- squared:

  A logical: combine the squared scaled distances (`TRUE`) or the scaled
  distances (`FALSE`) in the classification rule, or `NULL` (default)
  for `TRUE`.

- engine:

  The computational engine: `"rsimca"` (default).

- object:

  A `simca` model specification.

- parameters:

  A one-row tibble or named list of main parameters to update, such as
  those returned by
  [`tune::select_best()`](https://tune.tidymodels.org/reference/show_best.html).

- fresh:

  If `TRUE`, the arguments replace those of `object`; if `FALSE`
  (default), they update them.

- ...:

  Not used.

## Value

A model specification of classes `simca` and `model_spec`.

## Details

`simca()` is a parsnip model specification: it is fitted with
[`parsnip::fit()`](https://generics.r-lib.org/reference/fit.html) or in
a workflow, and can be tuned and resampled like any parsnip model. It
needs the parsnip package, and the `"rsimca"` engine is available once
parsnip and specProc are both loaded.

## Main arguments

- `num_comp` is the number of components of the robust PCA of each class
  (`ncomp` of
  [`rsimca()`](https://christiangoueguel.com/specProc/reference/rsimca.md)),
  the same for all the classes. With `NULL` (the default), each class
  gets the number chosen by
  [`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md)
  from the proportion of variance explained.

- `gamma` is the weight of the orthogonal distances, against the score
  distances, in the classification rule (`gamma` of
  [`rsimca()`](https://christiangoueguel.com/specProc/reference/rsimca.md)).
  With `NULL` (the default), it is 0.5.

- `squared` chooses the classification rule (`squared` of
  [`rsimca()`](https://christiangoueguel.com/specProc/reference/rsimca.md)):
  the squared scaled distances (`TRUE`, rule R2 of the paper) or the
  scaled distances (`FALSE`, rule R1). With `NULL` (the default), it is
  `TRUE`.

All three can be tuned with
[`tune::tune()`](https://hardhat.tidymodels.org/reference/tune.html),
using
[`dials::num_comp()`](https://dials.tidymodels.org/reference/num_comp.html),
[`simca_gamma()`](https://christiangoueguel.com/specProc/reference/simca_gamma.md)
and
[`simca_squared()`](https://christiangoueguel.com/specProc/reference/simca_gamma.md).

## Engine arguments

The other arguments of
[`rsimca()`](https://christiangoueguel.com/specProc/reference/rsimca.md)
are set with
[`parsnip::set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.html):
`kmax`, `alpha`, `var_explained`, `prior`, `ndir` and `nsamp`.

## Predictions

[predict()](https://parsnip.tidymodels.org/reference/predict.model_fit.html)
gives the classes (`type = "class"`), or with `type = "raw"` the
combined distances to the classes and whether each observation is
outlying for all of them
([`predict.specproc_rsimca()`](https://christiangoueguel.com/specProc/reference/predict.specproc_rsimca.md)).
SIMCA does not give class probabilities: when tuning, use metrics of the
predicted classes, such as
`yardstick::metric_set(yardstick::accuracy, yardstick::kap)`.

The fitted
[`rsimca()`](https://christiangoueguel.com/specProc/reference/rsimca.md)
model is returned by
[`parsnip::extract_fit_engine()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html).
[`rsimca()`](https://christiangoueguel.com/specProc/reference/rsimca.md)
uses random directions and subsets, so use
[`set.seed()`](https://rdrr.io/r/base/Random.html) before fitting for
reproducible results.

## See also

[`rsimca()`](https://christiangoueguel.com/specProc/reference/rsimca.md),
[`predict.specproc_rsimca()`](https://christiangoueguel.com/specProc/reference/predict.specproc_rsimca.md),
[`simca_gamma()`](https://christiangoueguel.com/specProc/reference/simca_gamma.md),
[`simca_squared()`](https://christiangoueguel.com/specProc/reference/simca_gamma.md)

## Author

Christian L. Goueguel

## Examples

``` r
library(parsnip)
spec <- simca(num_comp = 2, gamma = 0.5) |>
  set_engine("rsimca", alpha = 0.75)
spec
#> SIMCA Model Specification (classification)
#> 
#> Main Arguments:
#>   num_comp = 2
#>   gamma = 0.5
#> 
#> Engine-Specific Arguments:
#>   alpha = 0.75
#> 
#> Computational engine: rsimca 
#> 
# iris: train on 100 flowers, predict the 50 others
set.seed(1)
train <- sample(nrow(iris), 100)
fit <- fit(spec, Species ~ ., data = iris[train, ])
table(predict(fit, iris[-train, ])$.pred_class, iris$Species[-train])
#>             
#>              setosa versicolor virginica
#>   setosa         16          0         0
#>   versicolor      0         17         0
#>   virginica       0          2        15
head(predict(fit, iris[-train, ], type = "raw"))
#> # A tibble: 6 × 4
#>   setosa versicolor virginica outlying
#>    <dbl>      <dbl>     <dbl> <lgl>   
#> 1 0.0893       7.77      16.8 FALSE   
#> 2 0.214        6.41      14.7 FALSE   
#> 3 0.0472      10.1       19.0 FALSE   
#> 4 0.0234       8.37      17.2 FALSE   
#> 5 0.275        5.68      14.0 FALSE   
#> 6 0.0947      10.7       20.0 FALSE   
```
