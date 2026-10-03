# Wavelength Selection for PLS Regression

Selects the spectral variables (wavelengths) that are informative for a
response, from a PLS regression model:

- `"vip"`: the variable importance in projection (VIP) of each variable;

- `"sr"`: the selectivity ratio (SR) of each variable;

- `"ipls"`: forward interval PLS (iPLS), which selects contiguous
  spectral intervals.

With `robust = TRUE`, the PLS model is the robust model of
[`rsimpls()`](https://christiangoueguel.com/specProc/reference/rsimpls.md),
so that outlying spectra and wrong reference values do not drive the
selection.
[`step_select_wavelengths()`](https://christiangoueguel.com/specProc/reference/step_select_wavelengths.md)
performs the selection in a tidymodels recipe, where it is repeated on
every resample.

## Usage

``` r
select_wavelengths(
  x,
  y,
  method = c("vip", "sr", "ipls"),
  ncomp = 5,
  num_terms = NULL,
  threshold = NULL,
  recursive = FALSE,
  prop_drop = 0.25,
  intervals = 40,
  num_intervals = NULL,
  folds = 5,
  robust = FALSE,
  ...
)
```

## Arguments

- x:

  A numeric matrix or data frame of the spectra, one observation per row
  and the variables in wavelength order.

- y:

  A numeric vector of the response.

- method:

  The selection method: `"vip"` (default), `"sr"` or `"ipls"`.

- ncomp:

  The number of PLS components of the model that ranks the variables
  (`"vip"`, `"sr"`), or the largest number of components of the interval
  models (`"ipls"`). Default is 5.

- num_terms:

  The number of variables to keep (`"vip"`, `"sr"`). If `NULL`
  (default), the variables above `threshold` are kept.

- threshold:

  The importance above which variables are kept when `num_terms = NULL`.
  If `NULL` (default), 1 for VIP; SR has no default threshold.

- recursive:

  If `TRUE`, remove the variables by backward elimination (`"vip"`,
  `"sr"`; requires `num_terms`). Default is `FALSE`.

- prop_drop:

  The fraction of the remaining variables removed at each round of
  backward elimination. Default is 0.25.

- intervals:

  The number of contiguous intervals (`"ipls"`). Default is 40.

- num_intervals:

  The number of intervals to select (`"ipls"`). If `NULL` (default),
  intervals are added while the RMSECV decreases.

- folds:

  The number of cross-validation groups (`"ipls"`). Default is 5.

- robust:

  If `TRUE`, use the robust PLS model of
  [`rsimpls()`](https://christiangoueguel.com/specProc/reference/rsimpls.md).
  Default is `FALSE`.

- ...:

  Further arguments of
  [`rsimpls()`](https://christiangoueguel.com/specProc/reference/rsimpls.md)
  when `robust = TRUE`: `alpha`, `ndir` and `nsamp`.

## Value

An object of class `specproc_wavelength_selection`, a list with:

- `selected`: the names of the selected variables, in column order.

- `importance`: the VIP or SR of every variable (from the model of all
  the variables), or for iPLS the RMSECV of the interval of each
  variable alone.

- `intervals`: for iPLS, a tibble with one row per interval: its first
  and last variables, size, RMSECV alone, and `step`, the step at which
  it was selected (`NA` if not selected).

- `path`: a tibble of the selection path: the number of variables left
  after each round of backward elimination, or for iPLS the interval
  added at each step, with the RMSECV and number of components.

- `method`, `ncomp`, `robust`, `threshold` (when used), `observations`
  (the number of observations used; with `robust = TRUE`, those of the
  robust regression) and `mean_spectrum` (for
  [`plot_wavelength_selection()`](https://christiangoueguel.com/specProc/reference/plot_wavelength_selection.md)).

## Details

**VIP** (Wold, Johansson and Cocchi, 1993) sums the squared PLS weights
of each variable over the components, weighted by the variance of the
response that each component explains: \$\$VIP_j = \sqrt{p \sum_a SSY_a
w\_{ja}^2 / \sum_a SSY_a}.\$\$ The weights are the orthonormal PLS
weights of the NIPALS algorithm (an orthonormal basis of the SIMPLS
weights, which span the same nested subspaces). The squared VIP values
average 1, so the usual rule \\VIP \> 1\\ keeps the variables of
above-average importance. The PLS weights favor variables of large
variance, so on unscaled spectra a strong line unrelated to the response
can get a large VIP through the later components.

**SR** (Rajalahti et al., 2009) splits each variable, by target
projection on the normalized regression vector \\w\_{TP} = b/\\b\\\\,
into a part explained by the predictive component \\t\_{TP} = X
w\_{TP}\\ and a residual, and divides their variances. Unlike VIP, it
does not depend on the intensity of the variable: a weak emission line
that follows the response as closely as a strong one gets the same
ratio, which matters for unscaled spectra. The ratios depend on how much
of the variance of each variable is unrelated to the response (in LIBS,
shot-to-shot fluctuations), so there is no general threshold: in the
`forageLIBS` spectra, the largest ratio for calcium, at the Ca II 317.9
nm line, is about 0.5.

Both keep the `num_terms` variables with the largest values or, if
`num_terms = NULL`, those above `threshold` (by default 1 for VIP; SR
needs `num_terms` or `threshold`). With `recursive = TRUE` (backward
variable elimination), the model is refitted on the remaining variables
and the fraction `prop_drop` of the least important ones is removed,
until `num_terms` variables are left, so that the importance is
recomputed without the variables already removed.

**iPLS** (Nørgaard et al., 2000) splits the variables into `intervals`
contiguous intervals of nearly equal sizes (in the order of the columns,
which should be sorted by wavelength). Starting from none, it adds at
each step the interval that gives the lowest cross-validated error
(RMSECV) of a PLS model on the intervals selected so far, with the best
number of components up to `ncomp`. It stops after `num_intervals`
intervals or, if `num_intervals = NULL`, when no interval lowers the
RMSECV. The observations are split into `folds` interleaved groups
(observation `i` in group `(i - 1) %% folds + 1`); average replicate
spectra of a sample first, or they will fall in different groups. When a
parallel plan is set with
[`future::plan()`](https://future.futureverse.org/reference/plan.html)
(and the future.apply package is installed), the candidate intervals of
each step are evaluated in parallel; within
[`tune::tune_grid()`](https://tune.tidymodels.org/reference/tune_grid.html),
which runs the resamples in parallel, they are evaluated sequentially.

**Robust selection.** With `robust = TRUE`, VIP and SR are computed from
an
[`rsimpls()`](https://christiangoueguel.com/specProc/reference/rsimpls.md)
model, on the observations of its final regression (those that are not
regression outliers; good leverage points are kept). For iPLS, an
[`rsimpls()`](https://christiangoueguel.com/specProc/reference/rsimpls.md)
model of all the variables identifies these observations once, and the
intervals are selected by classical PLS on them, so that outliers affect
neither the models nor the RMSECV. Refitting a robust model for every
candidate interval would be about 35 times slower. Use
[`set.seed()`](https://rdrr.io/r/base/Random.html) for reproducible
results.

The selection uses the response, so it must be estimated on training
data only: to assess a model built on the selected variables, select
within each resample with
[`step_select_wavelengths()`](https://christiangoueguel.com/specProc/reference/step_select_wavelengths.md).

## References

- Wold, S., Johansson, E., Cocchi, M. (1993). PLS: partial least squares
  projections to latent structures. In Kubinyi, H. (ed.), 3D QSAR in
  Drug Design: Theory, Methods and Applications, 523-550. ESCOM, Leiden.

- Rajalahti, T., Arneberg, R., Berven, F.S., Myhr, K.-M., Ulvik, R.J.,
  Kvalheim, O.M. (2009). Biomarker discovery in mass spectral profiles
  by means of selectivity ratio plot. Chemometrics and Intelligent
  Laboratory Systems, 95(1):35-48.

- Nørgaard, L., Saudland, A., Wagner, J., Nielsen, J.P., Munck, L.,
  Engelsen, S.B. (2000). Interval partial least-squares regression
  (iPLS): a comparative chemometric study with an example from
  near-infrared spectroscopy. Applied Spectroscopy, 54(3):413-419.

- Mehmood, T., Liland, K.H., Snipen, L., Sæbø, S. (2012). A review of
  variable selection methods in partial least squares regression.
  Chemometrics and Intelligent Laboratory Systems, 118:62-69.

## See also

[`step_select_wavelengths()`](https://christiangoueguel.com/specProc/reference/step_select_wavelengths.md),
[`plot_wavelength_selection()`](https://christiangoueguel.com/specProc/reference/plot_wavelength_selection.md),
[`rsimpls()`](https://christiangoueguel.com/specProc/reference/rsimpls.md)

## Author

Christian L. Goueguel

## Examples

``` r
data(forageLIBS)
wl <- suppressWarnings(as.numeric(names(forageLIBS)))
spectra <- forageLIBS[which(wl > 380 & wl < 430)]  # Ca II and Ca I lines
sel <- select_wavelengths(spectra, forageLIBS$Ca, method = "sr", num_terms = 50)
sel
#> Wavelength selection (selectivity ratio)
#> 
#> Variables:      594
#> Selected:       50 (8.42%)
#> Observations:   368
#> Components:     5
#> Rule:           the most important variables
head(sel$selected)
#> [1] "387.3560123" "387.4407596" "387.6102544" "387.6950018" "390.7493163"
#> [6] "390.8341854"
select_wavelengths(spectra, forageLIBS$Ca, method = "ipls", intervals = 10)
#> Wavelength selection (forward interval PLS)
#> 
#> Variables:      594
#> Selected:       238 (40.1%)
#> Observations:   368
#> Components:     up to 5
#> Intervals:      4 of 10 (RMSECV 0.09791, 5 components)
```
