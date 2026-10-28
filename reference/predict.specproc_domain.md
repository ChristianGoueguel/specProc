# Flag Spectra Outside the Applicability Domain

Computes Hotelling's \\T^2\\ and the Q residual of new spectra in the
PCA model of
[`applicability_domain()`](https://christiangoueguel.com/specProc/reference/applicability_domain.md),
and flags those outside the domain.

## Usage

``` r
# S3 method for class 'specproc_domain'
predict(object, newdata, ...)
```

## Arguments

- object:

  The result of
  [`applicability_domain()`](https://christiangoueguel.com/specProc/reference/applicability_domain.md).

- newdata:

  The new spectra: a numeric matrix or data frame with the variables of
  the calibration spectra.

- ...:

  Not used.

## Value

A tibble of class `specproc_influence`, as returned by
[`q_residuals()`](https://christiangoueguel.com/specProc/reference/q_residuals.md),
with one row per spectrum: `sample`, `t2` and its limit, `q` and its
limit, `outlier` (`"regular"`, `"extreme"` for a high \\T^2\\,
`"residual"` for a high Q, or `"both"`), and `inside`, whether the
spectrum lies inside the domain. Draw it with
[`plot_influence()`](https://christiangoueguel.com/specProc/reference/plot_influence.md).

## See also

[`applicability_domain()`](https://christiangoueguel.com/specProc/reference/applicability_domain.md)

## Examples

``` r
data(forageLIBS)
wl <- suppressWarnings(as.numeric(names(forageLIBS)))
spectra <- as.matrix(forageLIBS[which(wl > 380 & wl < 430)])
domain <- applicability_domain(spectra[1:300, ])
flags <- predict(domain, spectra[301:368, ])
flags[!flags$inside, ]
#> # A tibble: 5 × 7
#>   sample    t2 t2_limit_99         q q_limit_99 outlier  inside
#>    <int> <dbl>       <dbl>     <dbl>      <dbl> <fct>    <lgl> 
#> 1     19 11.8         26.4 49478203.  34152170. residual FALSE 
#> 2     25 30.8         26.4 19058606.  34152170. extreme  FALSE 
#> 3     58  8.56        26.4 57894595.  34152170. residual FALSE 
#> 4     64 42.5         26.4 54753969.  34152170. both     FALSE 
#> 5     65 41.3         26.4 73988807.  34152170. both     FALSE 
```
