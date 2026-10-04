# Robust Linear Regression in tidymodels (LTS parsnip Engine)

specProc adds the `"lts"` engine to
[`parsnip::linear_reg()`](https://parsnip.tidymodels.org/reference/linear_reg.html):
the reweighted least trimmed squares (LTS) regression of
[`robustbase::ltsReg()`](https://rdrr.io/pkg/robustbase/man/ltsReg.html),
which resists vertical outliers and bad leverage points. `lts_fit()`
fits it outside of tidymodels.

## Usage

``` r
lts_fit(x, y, alpha = 0.75, nsamp = 500)
```

## Arguments

- x:

  A numeric matrix or data frame of the predictors.

- y:

  A numeric vector of the response.

- alpha:

  The fraction of observations whose squared residuals are minimized,
  between 0.5 and 1. Default is 0.75.

- nsamp:

  The number of random subsets. Default is 500.

## Value

`lts_fit()` returns an object of class `specproc_lts`, a list with the
`coefficients` (intercept first), the robust `scale` of the residuals,
the `fitted` values, `residuals`, standardized residuals `std_resid` and
the `weights` of the reweighted fit (1 for the observations whose
standardized residual is within the cut-off). Its
[`predict()`](https://rdrr.io/r/stats/predict.html) method takes
`newdata`.

## Details

The engine is available once parsnip and specProc are both loaded, in
either order. It is for regression with one outcome.

Combined with
[`step_robpca()`](https://christiangoueguel.com/specProc/reference/step_robpca.md)
in a workflow, it gives the robust principal component regression of
Hubert and Verboven (2003), whose robust PCA has `num_comp` components
(see also
[`rpcr()`](https://christiangoueguel.com/specProc/reference/rpcr.md)):
the robust scores of the spectra, then the LTS regression on them. It
also gives robust calibration curves from a few line intensities.

## Engine arguments

- `alpha`: the fraction of observations whose squared residuals are
  minimized, between 0.5 and 1 (default 0.75, as in
  [`rpcr()`](https://christiangoueguel.com/specProc/reference/rpcr.md)).

- `nsamp`: the number of random subsets (default 500).

The main arguments of
[`parsnip::linear_reg()`](https://parsnip.tidymodels.org/reference/linear_reg.html),
`penalty` and `mixture`, are not used. LTS uses random subsets, so use
[`set.seed()`](https://rdrr.io/r/base/Random.html) before fitting for
reproducible results.

## References

- Rousseeuw, P.J., Van Driessen, K. (2006). Computing LTS regression for
  large data sets. Data Mining and Knowledge Discovery, 12(1):29-45.

- Hubert, M., Verboven, S. (2003). A robust PCR method for
  high-dimensional regressors. Journal of Chemometrics, 17(8-9):438-452.

## See also

[`rpcr()`](https://christiangoueguel.com/specProc/reference/rpcr.md),
[`step_robpca()`](https://christiangoueguel.com/specProc/reference/step_robpca.md),
[`parsnip::linear_reg()`](https://parsnip.tidymodels.org/reference/linear_reg.html)

## Author

Christian L. Goueguel

## Examples

``` r
library(parsnip)
data(forageLIBS)
lines <- forageLIBS[c("Ca", "393.3599236", "396.8602175")]  # Ca II K and H
names(lines) <- c("Ca", "CaII_393", "CaII_397")
set.seed(1)
fit <- linear_reg() |>
  set_engine("lts") |>
  fit(Ca ~ ., data = lines[1:300, ])
predict(fit, lines[301:305, ])
#> # A tibble: 5 × 1
#>   .pred
#>   <dbl>
#> 1 0.478
#> 2 0.517
#> 3 0.580
#> 4 0.533
#> 5 0.567
extract_fit_engine(fit)$coefficients
#>   (Intercept)      CaII_393      CaII_397 
#> -2.574715e+00  4.387295e-05  6.141614e-06 
```
