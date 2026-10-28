# Uncertainty of Transition Probabilities from NIST Accuracy Grades

Converts the accuracy grades of the transition probabilities of the NIST
Atomic Spectra Database into relative standard uncertainties.

## Usage

``` r
aki_uncertainty(accuracy)
```

## Arguments

- accuracy:

  A character vector of accuracy grades, such as the column `accuracy`
  returned by
  [`nist_lines()`](https://christiangoueguel.com/specProc/reference/nist_lines.md).

## Value

A numeric vector of relative standard uncertainties (0.25 for 25%).

## Details

The grades bound the uncertainty of the transition probability (and of
the oscillator strength and line strength) at the level of one standard
deviation: AAA \\\le\\ 0.3%, AA \\\le\\ 1%, A+ \\\le\\ 2%, A \\\le\\ 3%,
B+ \\\le\\ 7%, B \\\le\\ 10%, C+ \\\le\\ 18%, C \\\le\\ 25%, D+ \\\le\\
40%, D \\\le\\ 50%, and E \> 50%. The bound of each grade is returned,
so the uncertainties are upper bounds. Grade E, which has no bound, and
unknown grades give `NA`: such lines are best left out of plasma
diagnostics.

## References

- Kramida, A., Ralchenko, Yu., Reader, J., and NIST ASD Team. NIST
  Atomic Spectra Database, <https://physics.nist.gov/asd>. National
  Institute of Standards and Technology, Gaithersburg, MD.

## See also

[`nist_lines()`](https://christiangoueguel.com/specProc/reference/nist_lines.md),
[`plasma_uncertainty()`](https://christiangoueguel.com/specProc/reference/plasma_uncertainty.md)

## Examples

``` r
aki_uncertainty(c("AA", "B+", "C", "D", "E"))
#> [1] 0.01 0.07 0.25 0.50   NA
```
