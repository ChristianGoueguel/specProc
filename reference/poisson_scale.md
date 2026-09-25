# Poisson Scaling

This function performs Poisson scaling on a numeric matrix or data
frame. Poisson scaling scales each variable by its square root mean
value, with an optional scaling offset to avoid over-scaling when a
variable has a near-zero mean.

## Usage

``` r
poisson_scale(x, sc = NULL, drop.na = TRUE, options = list())
```

## Arguments

- x:

  A numeric matrix or data frame.

- sc:

  A vector of previously calculated scales. If provided, these scales
  will be applied to the data.

- drop.na:

  A logical value indicating whether to remove rows containing missing
  values. If `TRUE` (the default), such rows are removed. If `FALSE`,
  missing values are kept and ignored when computing the means.

- options:

  A list of options for Poisson scaling. See the 'Options' section
  below.

## Value

If `sc` is not provided, the function returns a list with the following
components:

- `xs`:

  The Poisson-scaled data.

- `sc`:

  A vector of scales calculated for the given data.

If `sc` is provided, the function returns the Poisson-scaled data `xs`.

## Options

The `options` argument is a list with the following fields:

- `offset`: A numeric value representing the percentage of the maximum
  mean value to be used as an offset on all scales. Avoids division by
  near-zero means. Default is 3.

- `mode`: An integer value (1 or 2) specifying the dimension of the data
  on which to calculate the mean value for scaling. 1 = mean over rows
  (to scale variables); 2 = mean over columns (to scale samples).
  Default is 1.

## Author

Christian L. Goueguel

## Examples

``` r
set.seed(1)
x <- matrix(rpois(40, lambda = rep(c(5, 50, 500, 5000), each = 10)), ncol = 4)
res <- poisson_scale(x)
res$sc
#> [1] 12.46455 14.07356 25.55514 71.73120
# apply the same scales to new data
poisson_scale(x[1:2, ], sc = res$sc)
#>           [,1]     [,2]     [,3]     [,4]
#> [1,] 0.3209101 3.126431 20.26990 69.46768
#> [2,] 0.3209101 3.339596 20.07424 69.59315
```
