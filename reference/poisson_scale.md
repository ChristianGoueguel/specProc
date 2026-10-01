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
data(forageLIBS)
spectra <- forageLIBS[-(1:14)]  # the spectral channels
res <- poisson_scale(spectra[1:300, ])
head(res$sc)
#> 199.3771616 199.4644141 199.5516666 199.6389192 199.7261717 199.8134242 
#>    51.53800    51.59162    51.94261    51.63528    51.66161    51.70972 
# the same scales for new spectra
poisson_scale(spectra[301:368, ], sc = res$sc)[1:3, 1:4]
#>      199.3771616 199.4644141 199.5516666 199.6389192
#> [1,]    13.27176    13.47118    14.74704    13.45979
#> [2,]    13.25236    13.23858    14.51602    13.24676
#> [3,]    13.46579    13.49056    14.49677    13.69219
```
