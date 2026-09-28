# Net Analyte Signal and Figures of Merit

This function computes the net analyte signal (NAS) of each sample for a
multivariate inverse calibration model (PLS or PCR), following Lorber
(1997) and Faber (1998), and the figures of merit derived from it:
sensitivity, selectivity and, when the instrumental noise is known,
analytical sensitivity, limits of detection and quantification, and
signal-to-noise ratios.

## Usage

``` r
nas(
  x,
  y,
  ncomp = 5,
  method = "pls",
  center = TRUE,
  scale = FALSE,
  noise = NULL
)
```

## Arguments

- x:

  A numeric matrix or data frame of calibration spectra (one per row).

- y:

  A numeric vector (or one-column matrix or data frame) of analyte
  concentrations.

- ncomp:

  A positive integer giving the number of latent variables of the
  calibration model. Default is 5.

- method:

  The inverse calibration model: `"pls"` (default, SIMPLS) or `"pcr"`.

- center:

  A logical value indicating whether to mean-center `x` and `y`. Default
  is `TRUE`.

- scale:

  A logical value indicating whether to scale the columns of `x` to unit
  variance. Default is `FALSE`. The figures of merit are then expressed
  in scaled units.

- noise:

  An optional positive number: the standard deviation of the
  instrumental noise, in the units of `x`. Needed for the analytical
  sensitivity, LOD, LOQ and signal-to-noise ratios.

## Value

An object of class `specproc_nas`, a list with:

- `nas`: A tibble with one row per calibration sample: the scalar `nas`,
  the `selectivity`, the fitted concentration `fitted` and, if `noise`
  is given, the signal-to-noise ratio `snr`.

- `figures_of_merit`: A named vector with the `sensitivity`, the mean
  selectivity (`selectivity`) and, if `noise` is given,
  `analytical_sensitivity`, `lod` and `loq`.

- `nas_vectors`: A tibble of the NAS vectors \\\textbf{r}\_i^\*\\ of the
  calibration samples.

- `coefficients`: The regression vector \\\textbf{b}\\.

- `ncomp`, `method`, `noise`, `center`, `scale` and `y_center`: the
  model settings and preprocessing parameters.

Use
[predict()](https://christiangoueguel.com/specProc/reference/predict.specproc_nas.md)
to compute the NAS, selectivity and predicted concentration of new
samples.

## Details

The net analyte signal is the part of a spectrum that is unique to the
analyte, i.e. orthogonal to the spectral contributions of all other
constituents (the interferents). In an inverse calibration model
\\\hat{y} = \bar{y} + (\textbf{x} - \bar{\textbf{x}})^T\textbf{b}\\ with
\\A\\ latent variables, the interferent space is spanned by the
calibration spectra reconstructed from the model, from which the part
explained by \\\hat{\textbf{y}}\\ has been removed. The regression
vector \\\textbf{b}\\ is orthogonal to this space, so the NAS space
within the model is one-dimensional and spanned by \\\textbf{b}\\
(Faber, 1998; Bro and Andersen, 2003). The NAS vector of sample \\i\\ is
therefore \$\$\textbf{r}\_i^\* = \frac{(\textbf{x}\_i -
\bar{\textbf{x}})^T\textbf{b}}{\\\textbf{b}\\^2}\\\textbf{b}\$\$ and its
signed length, the scalar NAS, is \$\$\mathrm{NAS}\_i =
\frac{(\textbf{x}\_i - \bar{\textbf{x}})^T\textbf{b}}{\\\textbf{b}\\} =
\frac{\hat{y}\_i - \bar{y}}{\\\textbf{b}\\}\$\$

The figures of merit follow Olivieri *et al.* (2006):

- **Sensitivity**, \\\mathrm{SEN} = 1 / \\\textbf{b}\\\\: the NAS
  produced by a unit change in concentration, in signal units per
  concentration unit.

- **Selectivity** of sample \\i\\, \\\mathrm{SEL}\_i =
  \|\mathrm{NAS}\_i\| / \\\textbf{x}\_i - \bar{\textbf{x}}\\\\: the
  fraction of the (centered) signal that is used for prediction, between
  0 and 1.

- With the standard deviation of the instrumental noise \\\sigma_x\\
  (`noise`): the **analytical sensitivity** \\\gamma = \mathrm{SEN} /
  \sigma_x\\, whose inverse is the smallest concentration difference
  that can be distinguished; the **limit of detection** \\\mathrm{LOD} =
  3.3\\\sigma_x / \mathrm{SEN}\\; the **limit of quantification**
  \\\mathrm{LOQ} = 10\\\sigma_x / \mathrm{SEN}\\; and the
  **signal-to-noise ratio** of each sample, \\\mathrm{NAS}\_i /
  \sigma_x\\.

The LOD and LOQ above account for the instrumental noise only, not for
the uncertainty of the calibration model or of the reference values, so
they are lower bounds. The noise level must be estimated independently,
for example as the standard deviation of the differences between
repeated spectra of the same sample divided by \\\sqrt{2}\\, in the same
units (and after the same preprocessing) as `x`.

All quantities depend on the number of latent variables `ncomp`, which
should be chosen by validation beforehand.

Before specProc 0.4.0, `nas()` returned spectra filtered by direct
orthogonalization; that correction is available from
[`direct_orthogonal()`](https://christiangoueguel.com/specProc/reference/direct_orthogonal.md).

## References

- Lorber, A. (1986). Error propagation and figures of merit for
  quantification by solving matrix equations. Analytical Chemistry,
  58(6):1167-1172.

- Lorber, A., Faber, K., Kowalski, B.R. (1997). Net analyte signal
  calculation in multivariate calibration. Analytical Chemistry,
  69(8):1620-1626.

- Faber, N.M. (1998). Efficient computation of net analyte signal vector
  in inverse multivariate calibration models. Analytical Chemistry,
  70(23):5108-5110.

- Bro, R., Andersen, C.M. (2003). Theory of net analyte signal vectors
  in inverse regression. Journal of Chemometrics, 17(12):646-652.

- Olivieri, A.C., Faber, N.M., Ferré, J., Boqué, R., Kalivas, J.H.,
  Mark, H. (2006). Uncertainty estimation and figures of merit for
  multivariate calibration (IUPAC Technical Report). Pure and Applied
  Chemistry, 78(3):633-661.

## See also

[`direct_orthogonal()`](https://christiangoueguel.com/specProc/reference/direct_orthogonal.md)
for the filter formerly returned by `nas()`.

## Author

Christian L. Goueguel

## Examples

``` r
# Three constituents with overlapping lines; the analyte is the first one
set.seed(1)
wl <- seq(390, 400, length.out = 200)
line <- function(center) exp(-(wl - center)^2 / 0.1)
pure <- rbind(line(393.4) + 0.3 * line(396.8), line(393.8), line(396.5))
conc <- matrix(runif(30 * 3, 0, 1), 30, 3)
x <- conc %*% pure + matrix(rnorm(30 * 200, sd = 0.002), 30)

fit <- nas(x, conc[, 1], ncomp = 3, noise = 0.002)
fit
#> Net analyte signal (PLS model, 3 components)
#> 
#> Calibration samples:     30
#> Sensitivity:             2.59
#> Mean selectivity:        0.565
#> Noise (sd):              0.002
#> Analytical sensitivity:  1295
#> LOD:                     0.002548
#> LOQ:                     0.007722
head(fit$nas)
#> # A tibble: 6 × 4
#>      nas selectivity fitted    snr
#>    <dbl>       <dbl>  <dbl>  <dbl>
#> 1 -0.633       0.510  0.265 -317. 
#> 2 -0.358       0.460  0.371 -179. 
#> 3  0.164       0.803  0.573   82.1
#> 4  1.03        0.895  0.908  516. 
#> 5 -0.795       0.829  0.202 -398. 
#> 6  1.01        0.692  0.898  503. 

# the sensitivity equals the length of the analyte spectrum orthogonal
# to the interferent spectra
interf <- t(pure[2:3, ])
net <- pure[1, ] - interf %*% qr.solve(interf, pure[1, ])
c(model = fit$figures_of_merit[["sensitivity"]], theory = sqrt(sum(net^2)))
#>    model   theory 
#> 2.590138 2.591504 
```
