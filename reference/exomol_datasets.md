# Line Lists of a Molecule in the ExoMol Database

Lists the line lists of a molecule in the ExoMol database, for each
isotopologue, with the sizes of their files, to choose the `dataset` of
[`molecular_spectrum()`](https://christiangoueguel.com/specProc/reference/molecular_spectrum.md).

## Usage

``` r
exomol_datasets(molecule, timeout = 60)
```

## Arguments

- molecule:

  The molecule, as named by ExoMol, such as `"CN"`, `"C2"`, `"OH"` or
  `"AlO"`.

- timeout:

  The download timeout, in seconds. Default is 60.

## Value

A tibble with one row per line list: the `molecule`, the `isotopologue`
(as in the file names), the `dataset`, whether ExoMol `recommended` it,
and the sizes of its transition and states files (`trans_mb`,
`states_mb`, in MB; `NA` when not listed).

## Details

The list is read from the ExoMol API, and needs the jsonlite package and
an internet connection.

## See also

[`molecular_spectrum()`](https://christiangoueguel.com/specProc/reference/molecular_spectrum.md)

## Examples

``` r
# \donttest{
# needs an internet connection; gives up after 20 s if ExoMol does not answer
if (rlang::is_installed("jsonlite")) {
  try(exomol_datasets("CN", timeout = 20))
}
#> # A tibble: 12 × 6
#>    molecule isotopologue dataset    recommended trans_mb states_mb
#>    <chr>    <chr>        <chr>      <lgl>          <dbl>     <dbl>
#>  1 CN       12C-14N      KTPSYT     TRUE           NA      NA     
#>  2 CN       12C-14N      Trihybrid  FALSE          27.0     0.533 
#>  3 CN       12C-14N      MoLLIST-CN FALSE           1.10    0.0615
#>  4 CN       12C-15N      Trihybrid  FALSE          27.0     0.581 
#>  5 CN       12C-15N      KTPSYT     TRUE           NA      NA     
#>  6 CN       12C-15N      KTPSYT-iso FALSE          NA      NA     
#>  7 CN       13C-14N      Trihybrid  FALSE          28.7     0.599 
#>  8 CN       13C-14N      KTPSYT     TRUE           NA      NA     
#>  9 CN       13C-14N      KTPSYT-iso FALSE          NA      NA     
#> 10 CN       13C-15N      Trihybrid  FALSE          30.2     0.613 
#> 11 CN       13C-15N      KTPSYT     TRUE           NA      NA     
#> 12 CN       13C-15N      KTPSYT-iso FALSE          NA      NA     
# }
```
