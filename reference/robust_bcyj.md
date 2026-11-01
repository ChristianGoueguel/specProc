# Robust Box-Cox and Yeo-Johnson Transformation

**\[deprecated\]**

`robust_bcyj()` was deprecated in specProc 0.9.0: use
[`robust_boxcox()`](https://christiangoueguel.com/specProc/reference/robust_boxcox.md)
or
[`robust_yeojohnson()`](https://christiangoueguel.com/specProc/reference/robust_yeojohnson.md)
instead, which return a fitted object that
[predict()](https://christiangoueguel.com/specProc/reference/predict.specproc_transformation.md)
applies to new data. It still works, with a warning.

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
  Allowed values are "BC", "YJ", or "bestObj" (default: Box-Cox or
  Yeo-Johnson, whichever fits best, for strictly positive variables).

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

## Author

Christian L. Goueguel
