# Read Saved STARK-B Data

Reads Stark broadening parameters from an XSAMS file of the STARK-B
database saved earlier: for example, the result of a query to the
STARK-B VAMDC service downloaded in a web browser or through the VAMDC
portal. This gives the same table as
[`starkb_lines()`](https://christiangoueguel.com/specProc/reference/starkb_lines.md)
without an internet connection, and makes an analysis reproducible with
archived data.

## Usage

``` r
read_starkb(file, wavelength = NULL, perturber = NULL)
```

## Arguments

- file:

  The path to an XSAMS (XML) file from STARK-B.

- wavelength:

  An optional numeric vector of length 2: the wavelength range to keep,
  in nm.

- perturber:

  An optional character vector of perturbers to keep, such as
  `"electron"`, `"H II"` (protons) or `"He II"`. By default all are
  kept.

## Value

A tibble as returned by
[`starkb_lines()`](https://christiangoueguel.com/specProc/reference/starkb_lines.md).

## Details

A query for one species can be downloaded from
`http://stark-b.obspm.fr/12.07/vamdc/tap/sync?LANG=VSS2&REQUEST=doQuery&FORMAT=XSAMS&QUERY=select * where (atomsymbol = 'Ca' and ioncharge = 1)`
(with the element and the ion charge, 0 for neutral atoms, adapted).
Cite the STARK-B database and the original publications, listed in the
`source` column, as for
[`starkb_lines()`](https://christiangoueguel.com/specProc/reference/starkb_lines.md).

## See also

[`starkb_lines()`](https://christiangoueguel.com/specProc/reference/starkb_lines.md),
[`stark_table()`](https://christiangoueguel.com/specProc/reference/stark_table.md)
for data from other sources.

## Examples

``` r
# a minimal file in the STARK-B format, with made-up values
file <- tempfile(fileext = ".xml")
writeLines(c(
  '<XSAMSData xmlns="http://vamdc.org/xml/xsams/1.0">',
  '<Sources><Source sourceID="B1"><Title>Example</Title><Year>2000</Year></Source></Sources>',
  '<Environments><Environment envID="E1"><Temperature><Value units="K">10000</Value>',
  '</Temperature><TotalNumberDensity><Value units="1/cm3">1e17</Value></TotalNumberDensity>',
  '<Composition><Species name="electron" speciesRef="XP1"/></Composition></Environment>',
  '</Environments><Species><Atoms><Atom><ChemicalElement><ElementSymbol>Ca</ElementSymbol>',
  '</ChemicalElement><Isotope><Ion speciesID="X1"><IonCharge>1</IonCharge></Ion></Isotope>',
  '</Atom></Atoms></Species><Processes><Radiative><RadiativeTransition id="P1">',
  '<SourceRef>B1</SourceRef><EnergyWavelength><Wavelength><Value units="A">3950</Value>',
  '</Wavelength></EnergyWavelength><SpeciesRef>X1</SpeciesRef>',
  '<Broadening name="pressure" envRef="E1"><Lineshape name="Lorentzian">',
  '<LineshapeParameter name="gammaL"><Value units="A">0.2</Value></LineshapeParameter>',
  '</Lineshape></Broadening></RadiativeTransition></Radiative></Processes></XSAMSData>'
), file)
read_starkb(file)
#> # A tibble: 1 × 10
#>   species wavelength upper lower perturber temperature density width shift
#>   <chr>        <dbl> <chr> <chr> <chr>           <dbl>   <dbl> <dbl> <dbl>
#> 1 Ca II          395 ""    ""    electron        10000    1e17  0.02    NA
#> # ℹ 1 more variable: source <chr>
```
