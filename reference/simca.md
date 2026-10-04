# SIMCA Classification Model (parsnip)

`simca()` defines a soft independent modeling of class analogy (SIMCA)
classification model for tidymodels: a principal component model of each
class, and the assignment of each observation to the closest class. Its
engine `"rsimca"` (the default) is the robust SIMCA of
[`rsimca()`](https://christiangoueguel.com/specProc/reference/rsimca.md),
whose class models resist outlying spectra of the training data.

## Usage

``` r
simca(mode = "classification", num_comp = NULL, engine = "rsimca")

# S3 method for class 'simca'
update(object, parameters = NULL, num_comp = NULL, fresh = FALSE, ...)
```

## Arguments

- mode:

  The type of model: `"classification"`, the only one.

- num_comp:

  The number of components of the PCA model of each class, or `NULL`
  (default) to let the engine choose them.

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

## Main argument

`num_comp` is the number of components of the robust PCA of each class
(`ncomp` of
[`rsimca()`](https://christiangoueguel.com/specProc/reference/rsimca.md)),
the same for all the classes. With `NULL` (the default), each class gets
the number chosen by
[`robpca()`](https://christiangoueguel.com/specProc/reference/robpca.md)
from the proportion of variance explained. It can be tuned with
[`tune::tune()`](https://hardhat.tidymodels.org/reference/tune.html),
using
[`dials::num_comp()`](https://dials.tidymodels.org/reference/num_comp.html).

## Engine arguments

The other arguments of
[`rsimca()`](https://christiangoueguel.com/specProc/reference/rsimca.md)
are set with
[`parsnip::set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.html):
`gamma` (the weight of the orthogonal distances in the classification
rule, default 0.5), `squared`, `kmax`, `alpha`, `var_explained`,
`prior`, `ndir` and `nsamp`.

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
[`predict.specproc_rsimca()`](https://christiangoueguel.com/specProc/reference/predict.specproc_rsimca.md)

## Author

Christian L. Goueguel

## Examples

``` r
library(parsnip)
data(forageLIBS)
wl <- suppressWarnings(as.numeric(names(forageLIBS)))
dat <- forageLIBS[which(wl > 380 & wl < 430)]
# forage samples with low and high calcium
dat$level <- cut(forageLIBS$Ca, c(-Inf, 0.6, Inf), labels = c("low", "high"))
spec <- simca(num_comp = 3) |>
  set_engine("rsimca", gamma = 0.5)
spec
#> SIMCA Model Specification (classification)
#> 
#> Main Arguments:
#>   num_comp = 3
#> 
#> Engine-Specific Arguments:
#>   gamma = 0.5
#> 
#> Computational engine: rsimca 
#> 
set.seed(1)
fit <- fit(spec, level ~ ., data = dat[1:300, ])
table(predict(fit, dat[301:368, ])$.pred_class, dat$level[301:368])
#>       
#>        low high
#>   low   16   28
#>   high   3   21
head(predict(fit, dat[301:368, ], type = "raw"))
#> # A tibble: 6 × 3
#>     low  high outlying
#>   <dbl> <dbl> <lgl>   
#> 1 0.857 3.98  FALSE   
#> 2 1.41  3.55  TRUE    
#> 3 0.405 0.533 FALSE   
#> 4 0.469 1.73  FALSE   
#> 5 0.494 0.759 FALSE   
#> 6 0.761 2.51  TRUE    
```
