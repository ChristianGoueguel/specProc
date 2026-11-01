# Apply a Robust Transformation to New Data

Transforms new data with the \\\lambda\\ (and, with
`standardize = TRUE`, the mean and standard deviation of the inliers)
estimated by
[`robust_boxcox()`](https://christiangoueguel.com/specProc/reference/robust_boxcox.md)
or
[`robust_yeojohnson()`](https://christiangoueguel.com/specProc/reference/robust_yeojohnson.md).

## Usage

``` r
# S3 method for class 'specproc_transformation'
predict(object, newdata, ...)
```

## Arguments

- object:

  An object returned by
  [`robust_boxcox()`](https://christiangoueguel.com/specProc/reference/robust_boxcox.md)
  or
  [`robust_yeojohnson()`](https://christiangoueguel.com/specProc/reference/robust_yeojohnson.md).

- newdata:

  A numeric vector, matrix or data frame of new data, with the same
  variables as the data of the fit.

- ...:

  Not used.

## Value

A tibble of transformed variables. Missing values stay missing, and with
the Box-Cox transformation, values that are zero or negative become
`NA`.

## See also

[`robust_boxcox()`](https://christiangoueguel.com/specProc/reference/robust_boxcox.md),
[`robust_yeojohnson()`](https://christiangoueguel.com/specProc/reference/robust_yeojohnson.md)

## Examples

``` r
data(forageLIBS)
contents <- forageLIBS[c("Ca", "K", "P", "Mn")]
fit <- robust_boxcox(contents[1:300, ])
new <- predict(fit, contents[301:368, ])
dim(new)
#> [1] 68  4
# Box-Cox is not defined for zero or negative values
x <- contents[301:302, ]
x$Ca[2] <- 0
predict(fit, x)
#> # A tibble: 2 × 4
#>      Ca      K      P    Mn
#>   <dbl>  <dbl>  <dbl> <dbl>
#> 1 -1.35  0.546 -0.400 0.465
#> 2 NA    -0.139 -0.779 2.27 
```
