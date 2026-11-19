# Quantile Tail Weight Measure

This function calculates the left quantile weight (LQW) and the right
quantile weight (RQW) for a given numeric vector. These weights serve as
robust measures of tail heaviness, providing insights into the
distribution's behavior in the left and right tails, respectively.

## Usage

``` r
quantile_weight(
  x,
  p = 0.125,
  q = 0.875,
  drop_na = FALSE,
  drop.na = deprecated()
)
```

## Arguments

- x:

  A numeric vector.

- p:

  A probability between 0 and 0.5 that chooses the quantiles of the left
  half compared by LQW: \\Q(p/2)\\ and \\Q((1-p)/2)\\ of the data. A
  smaller value looks further into the left tail. Default is 0.125.

- q:

  A probability between 0.5 and 1 that chooses the quantiles of the
  right half compared by RQW: \\Q(1-q/2)\\ and \\Q((1+q)/2)\\ of the
  data. A larger value looks further into the right tail. Default is
  0.875.

- drop_na:

  Logical value indicating whether to remove missing values (NA). If
  `FALSE` (default) and `x` contains missing values, `NA` is returned.

- drop.na:

  **\[deprecated\]** Use `drop_na`.

## Value

A tibble with two numeric columns:

- `LQW`: Left quantile weight.

- `RQW`: Right quantile weight.

## Details

The left quantile weight (LQW) and the right quantile weight (RQW) are
robust measures of tail weight (Brys *et al.* 2006). They apply the
quantile skewness of Hinkley (1975), \$\$SK(p) = \frac{Q(1-p) + Q(p) -
2Q(0.5)}{Q(1-p) - Q(p)},\$\$ to each half of the distribution, divided
at the median: the left half, whose median is the first quartile
\\Q(0.25)\\, and the right half, whose median is the third quartile
\\Q(0.75)\\: \$\$LQW(p) = -\frac{Q((1-p)/2) + Q(p/2) -
2Q(0.25)}{Q((1-p)/2) - Q(p/2)}\$\$ \$\$RQW(q) = \frac{Q((1+q)/2) +
Q(1-q/2) - 2Q(0.75)}{Q((1+q)/2) - Q(1-q/2)}\$\$ where \\Q\\ is the
quantile function of the data. `p` and `q` are probabilities that choose
the quantiles compared: LQW compares \\Q(p/2)\\ and \\Q((1-p)/2)\\, the
\\p\\ and \\1 - p\\ quantiles of the left half, around \\Q(0.25)\\; RQW
compares \\Q(1-q/2)\\ and \\Q((1+q)/2)\\, the \\1 - q\\ and \\q\\
quantiles of the right half, around \\Q(0.75)\\. A heavier tail
stretches the outer quantile further than the inner one, which gives a
larger weight. With \\q = 1 - p\\, both weights look equally far into
their tails. The defaults, `p = 0.125` and `q = 0.875`, compare
\\Q(0.0625)\\ and \\Q(0.4375)\\ for LQW, and \\Q(0.5625)\\ and
\\Q(0.9375)\\ for RQW.

The breakdown value of LQW is \\p/2\\ and that of RQW is \\(1 - q)/2\\:
6.25% for the defaults, which resist up to 6.25% of outliers in a tail.
A smaller `p` (or a `q` closer to 1) looks further into the tails, which
makes the weights more sensitive to the tail weight but less robust;
`p = 0.25` and `q = 0.75` give a breakdown value of 12.5%, as
[`medcouple_weight()`](https://christiangoueguel.com/specProc/reference/medcouple_weight.md).

Interpretation of the quantile weights:

- At the normal distribution, LQW = RQW = 0.249 with the defaults (0.144
  with `p = 0.25` and `q = 0.75`).

- Larger values indicate heavier tails than the normal distribution, and
  smaller values lighter tails.

## References

- Brys, G., Hubert, M., and Struyf, A. (2006). Robust measures of tail
  weight. Computational Statistics & Data Analysis, 50(3):733-759

- Hinkley, D.V., (1975). On power transformations to symmetry.
  Biometrika, 62(1):101–111.

## See also

[`medcouple_weight()`](https://christiangoueguel.com/specProc/reference/medcouple_weight.md),
[`medcouple()`](https://christiangoueguel.com/specProc/reference/medcouple.md)

## Author

Christian L. Goueguel

## Examples

``` r
# 10000 values from three symmetric distributions: uniform (light tails),
# normal, and Laplace (double exponential: heavy tails)
set.seed(1)
n <- 10000
tails <- data.frame(
  distribution = factor(rep(c("uniform", "normal", "Laplace"), each = n),
                        levels = c("uniform", "normal", "Laplace")),
  value = c(runif(n, -2, 2), rnorm(n),
            rexp(n) * sample(c(-1, 1), n, replace = TRUE))
)
ggplot2::ggplot(tails, ggplot2::aes(value, fill = distribution)) +
  ggplot2::geom_density(alpha = 0.4) +
  ggplot2::coord_cartesian(xlim = c(-5, 5)) +
  ggplot2::scale_fill_manual(values = c("#fdb863", "#2166ac", "#b2182b"))


# the weights grow with the tail weight, in both tails: about 0 for the
# uniform distribution, 0.25 for the normal one and 0.43 for the Laplace one
weights <- function(v) c(kurtosis = moments::kurtosis(v), unlist(quantile_weight(v)))
round(t(sapply(split(tails$value, tails$distribution), weights)), 3)
#>         kurtosis   LQW    RQW
#> uniform    1.789 0.032 -0.040
#> normal     2.978 0.255  0.248
#> Laplace    6.103 0.441  0.439

# 1% of outliers: 9900 normal values and 100 near 10. The kurtosis jumps
# from 3 to 25, while the right quantile weight hardly moves and the left
# one does not see the outliers
set.seed(2)
x <- c(rnorm(9900), rnorm(100, mean = 10, sd = 0.5))
group <- rep(c("regular", "outlier"), c(9900, 100))
ggplot2::ggplot(data.frame(x, group), ggplot2::aes(x)) +
  ggplot2::geom_density(fill = "#2166ac", alpha = 0.3) +
  ggplot2::geom_rug(ggplot2::aes(colour = group)) +
  ggplot2::scale_colour_manual(values = c(outlier = "#b2182b", regular = "#2166ac"))

round(weights(x), 3)
#> kurtosis      LQW      RQW 
#>   25.304    0.235    0.271 

data(forageLIBS)
# iron contents (mg/kg): a few samples are far above the others
fe <- forageLIBS$Fe[!is.na(forageLIBS$Fe)]
moments::kurtosis(fe)   # classical
#> [1] 25.01662
quantile_weight(fe)     # robust, left and right tails
#> # A tibble: 1 × 2
#>      LQW   RQW
#>    <dbl> <dbl>
#> 1 0.0174 0.687
```
