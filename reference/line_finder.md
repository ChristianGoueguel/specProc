# Interactive Identification of Emission Lines

Launches a Shiny app to identify the emission lines of LIBS spectra.
Elements are selected on a periodic table, and the strongest lines of
their selected ionization stages, from the NIST Atomic Spectra Database,
are overlaid on a spectrum in an interactive plotly graph, with one
color per ionization stage.

## Usage

``` r
line_finder(spectra, launch.browser = interactive(), show_table = TRUE)
```

## Arguments

- spectra:

  Spectra, one per row: a numeric matrix, or a data frame whose
  wavelength columns are named by their wavelengths (other columns are
  used as labels).

- launch.browser:

  Passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).
  Default is `TRUE` when R is interactive.

- show_table:

  A logical: show the periodic table when the app starts (`TRUE`,
  default). The **Hide table** button of the Elements panel hides or
  shows it at any time; hiding it gives the spectrum the whole height of
  the window.

## Value

Called for its side effect: runs the app. Use
[`libs_lines()`](https://christiangoueguel.com/specProc/reference/libs_lines.md)
and
[`plot_lines()`](https://christiangoueguel.com/specProc/reference/plot_lines.md)
for the same results in scripts.

## Details

The app offers:

- a clickable periodic table; elements without lines in the wavelength
  range of the spectra are grayed out once queried. It can be hidden
  (**Hide table**) to enlarge the spectrum, and the selected elements
  stay listed in the panel header;

- the ionization stages to show (I, II, III);

- the plasma temperature and the number of lines per species, which set
  the lines kept and the height of their markers (relative intensities
  in local thermodynamic equilibrium, see
  [`libs_lines()`](https://christiangoueguel.com/specProc/reference/libs_lines.md));

- the wavelength range, a wavelength shift to correct the calibration of
  the spectrometer, and the spectrum to show: the mean of all spectra,
  the mean of a group (from the first character or factor column of
  `spectra`), or a single spectrum;

- a download of the displayed lines as CSV, and buttons to save and
  reload the lines fetched so far, so that a session can continue
  offline.

Each species is downloaded from NIST once per R session (see
[`nist_lines()`](https://christiangoueguel.com/specProc/reference/nist_lines.md));
later changes of the settings are computed locally. The zoom of the plot
is kept when the lines change.

The app needs the shiny, plotly and bslib packages, and an internet
connection for the species not yet fetched.

## See also

[`libs_lines()`](https://christiangoueguel.com/specProc/reference/libs_lines.md),
[`plot_lines()`](https://christiangoueguel.com/specProc/reference/plot_lines.md),
[`nist_lines()`](https://christiangoueguel.com/specProc/reference/nist_lines.md)

## Author

Christian L. Goueguel

## Examples

``` r
if (interactive() && rlang::is_installed(c("shiny", "plotly", "bslib"))) {
  data(soilLIBS)
  line_finder(soilLIBS)
}
```
