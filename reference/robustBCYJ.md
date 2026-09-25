# Robust Box-Cox and Yeo-Johnson Transformation

Transforms each variable in a dataset toward central normality using
re-weighted maximum likelihood to robustly fit the Box-Cox or
Yeo-Johnson transformation.

## Usage

``` r
robustBCYJ(x, var = NULL, type = "bestObj", quantile = 0.99, nbsteps = 2)
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

  - `method`: the method used ('BC' for Box-Cox or 'YJ' for Yeo-Johnson)

  - `objective`: the objective function value

- `transformation`:

  - the transformed variable(s)

## Details

The Box-Cox and Yeo-Johnson transformations are power transformations
aimed at making the data distribution more normal-like. The Box-Cox
transformation is suitable for strictly positive values, while the
Yeo-Johnson transformation can handle both positive and negative values.
The function is a wrapper around the `transfo` function from the
`cellWise` package, which applies a robust version of these
transformations by using re-weighted maximum likelihood estimation. This
approach downweights outlying observations to make the transformation
more robust to their influence.

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
