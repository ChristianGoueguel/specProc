# Robust Box-Cox and Yeo-Johnson Transformation

Transforms each variable in a dataset toward central normality using
re-weighted maximum likelihood to robustly fit the Box-Cox or
Yeo-Johnson transformation.

## Usage

``` r
robust_bcyj(x, var = NULL, type = "bestObj", quantile = 0.99, nbsteps = 2)
```

## Arguments

- x:

  A data frame or tibble containing the variables to be transformed.

- var:

  A vector of character or numeric variable names to be transformed. If
  `NULL` (default), all columns are selected.

- type:

  A character string specifying the transformation method(s) to use.
  Allowed values are "BC", "YJ", or "bestObj" (default).

- quantile:

  A numeric value between 0 and 1 specifying the quantile to use for
  determining the weights in the re-weighting step. Default is 0.99.

- nbsteps:

  An integer specifying the number of re-weighting steps to perform.
  Default is 2.

## Value

A list containing two data frames:

- `summary`:

  - `variable`: the variable(s) name

  - `lambda`: the estimated lambda parameter

  - `method`: the method used ('BC' for Box-Cox, 'YJ' for Yeo-Johnson,
    or 'none')

  - `objective`: the objective function value

- `transformation`:

  - the transformed variable(s)

## Details

The Box-Cox and Yeo-Johnson transformations are power transformations
aimed at making the data distribution more normal-like. The Box-Cox
transformation is suitable for strictly positive values, while the
Yeo-Johnson transformation can handle both positive and negative values.
The transformations are fitted robustly (Raymaekers and Rousseeuw,
2021), so that outlying observations do not drive the estimate of
\\\lambda\\:

1.  Each variable is pre-standardized: divided by its median for
    Box-Cox, which needs strictly positive values, and centered by its
    median and divided by its MAD for Yeo-Johnson.

2.  An initial \\\lambda\\ (between -4 and 6) minimizes a robust
    distance (Tukey's biweight) between the sorted, robustly
    standardized transformed values and the quantiles of the normal
    distribution. For this step, the transformation is continued
    linearly on the side that it compresses (the upper side for
    \\\lambda \< 1\\, the lower side for \\\lambda \> 1\\), beyond the
    point where the transformed value is 1.5 times that of the quartile,
    so that outliers on that side cannot dictate \\\lambda\\.

3.  The values whose standardized transformed value exceeds the
    `quantile` of the normal distribution are given weight zero (first
    with the rectified transformation), and \\\lambda\\ is re-estimated
    by maximum likelihood on the others, `nbsteps` times.

4.  The transformed variable is standardized by the mean and standard
    deviation of these inliers.

Variables with fewer than 5 values or no spread (zero MAD) are left
unchanged (method `"none"`), as are variables with non-positive values
when `type = "BC"`.

The `type` parameter controls which transformation method(s) to use:

- "BC": Only applies the Box-Cox transformation to strictly positive
  variables.

- "YJ": Only applies the Yeo-Johnson transformation to all variables.

- "bestObj" (default): For strictly positive variables, both BC and YJ
  are applied, and the solution with the lowest objective function value
  is kept. For variables with negative values, only YJ is applied.

## References

- Raymaekers, J., Rousseeuw, P.J., (2021). Transforming variables to
  central normality. Machine Learning,
  https://doi.org/10.1007/s10994-021-05960-5.

- Box, G. E. P., Cox, D. R. (1964). An analysis of transformations.
  Journal of the Royal Statistical Society, Series B, 26:211–252.

## Author

Christian L. Goueguel

## Examples

``` r
data(forageLIBS)
wl <- suppressWarnings(as.numeric(names(forageLIBS)))
spectra <- forageLIBS[which(wl > 760 & wl < 780)]  # the K I resonance lines
# two lines, transformed to central normality
res <- robust_bcyj(spectra[, c(10, 50)])
res$summary
#> # A tibble: 2 × 4
#>   variable    lambda method objective
#>   <chr>        <dbl> <chr>      <dbl>
#> 1 760.7723867   1.14 YJ          1.82
#> 2 764.1334761   2.51 BC          2.26
```
