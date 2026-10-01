# Directional Outlyingness for Skewed Distribution

This function computes the directional outlyingness of a numeric vector,
as proposed by Rousseeuw *et al.* (2018), which is a measure of
outlyingness of data point that takes the skewness of the underlying
distribution into account.

## Usage

``` r
directional_outlyingness(
  x,
  cutoff.quantile = 0.995,
  rmZeroes = FALSE,
  maxRatio = NULL,
  precScale = 1e-10
)
```

## Arguments

- x:

  A numeric vector

- cutoff.quantile:

  A numeric value between 0 and 1 specifying the quantile for outlier
  detection (default: 0.995).

- rmZeroes:

  A logical value. If `TRUE`, removes values close to zero (default:
  `FALSE`).

- maxRatio:

  A numeric value greater than 2. If provided, constrains the ratio
  between positive and negative scales (default: `NULL`).

- precScale:

  A numeric value specifying the precision scale for near-zero
  comparisons (default: 1e-10).

## Value

A tibble with columns:

- `data`: The original numeric values.

- `score`: The calculated outlyingness score.

- `flag`: A logical vector indicating whether each value is a potential
  outlier or not.

## Details

Directional outlyingness takes the potential skewness of the underlying
distribution into account, by splitting the univariate dataset in two
half samples around the median. And then apply one-step M-estimator with
Huber \\\rho\\-function for scaling each part.

## References

- Rousseeuw, P.J., Raymaekers, J., Hubert, M., (2018). A Measure of
  Directional Outlyingness With Applications to Image Data and Video.
  Journal of Computational and Graphical Statistics, 27(2):345–359.

## Author

Christian L. Goueguel

## Examples

``` r
data(forageLIBS)
# iron contents (mg/kg): a few samples are far above the others
fe <- forageLIBS$Fe[!is.na(forageLIBS$Fe)]
head(directional_outlyingness(fe))
#> # A tibble: 6 × 3
#>    data score flag 
#>   <int> <dbl> <lgl>
#> 1  2060 13.2  TRUE 
#> 2  1290  7.92 TRUE 
#> 3  1210  7.37 FALSE
#> 4  1190  7.23 FALSE
#> 5  1080  6.47 FALSE
#> 6  1060  6.34 FALSE
```
