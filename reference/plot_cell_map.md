# Cell Map of a MacroPCA Fit

Shows which cells of the data deviate from a
[`macropca()`](https://christiangoueguel.com/specProc/reference/macropca.md)
fit, with
[`cellWise::cellMap()`](https://rdrr.io/pkg/cellWise/man/cellMap.html).
Each cell is colored by its standardized residual: red when the observed
value is much higher than the fit, blue when it is much lower. Rows of
outlying observations are marked.

## Usage

``` r
plot_cell_map(
  object,
  rows = NULL,
  columns = NULL,
  nrowsinblock = NULL,
  ncolumnsinblock = NULL,
  title = "MacroPCA cell map",
  ...
)
```

## Arguments

- object:

  An object returned by
  [`macropca()`](https://christiangoueguel.com/specProc/reference/macropca.md).

- rows, columns:

  Optional indices or names of the rows and columns to show. Default is
  all.

- nrowsinblock, ncolumnsinblock:

  Optional numbers of rows and columns combined into one block.

- title:

  The plot title.

- ...:

  Further arguments passed to
  [`cellWise::cellMap()`](https://rdrr.io/pkg/cellWise/man/cellMap.html),
  such as `rowlabels`, `columnlabels` or `columnangle`.

## Value

A ggplot object.

## Details

Spectra have many more variables than can be shown one by one. Use
`columns` to select a spectral region, and `ncolumnsinblock` (and
`nrowsinblock`) to combine adjacent cells into blocks; the color of a
block then summarizes its cells. By default, the columns are grouped
into blocks when there are more than 60 of them.

## See also

[`macropca()`](https://christiangoueguel.com/specProc/reference/macropca.md),
[`plot_outlier_map()`](https://christiangoueguel.com/specProc/reference/plot_outlier_map.md)

## Examples

``` r
set.seed(1)
x <- matrix(rnorm(40 * 8), 40, 8) %*% diag(8:1)
x[1:2, ] <- x[1:2, ] + 20
x[10, 2] <- 40
fit <- macropca(x, k = 2)
plot_cell_map(fit)

```
