# Interactive Identification of Emission Lines

Launches a Shiny app to identify the emission lines of LIBS spectra.
Elements are selected on a periodic table, and the strongest lines of
their selected ionization stages, from the NIST Atomic Spectra Database,
are overlaid on a spectrum in an interactive plotly graph, with one
color per ionization stage. The heads of molecular bands can be overlaid
too. Lines of a species picked on the spectrum build a Boltzmann plot,
whose temperature is fitted again as lines are added or left out.

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

Invisibly, when the app is closed with **Done**, the
[`boltzmann()`](https://christiangoueguel.com/specProc/reference/boltzmann.md)
fit of the Boltzmann plot, whose `lines` element holds the measured
lines; `NULL` if the plot has fewer than 3 usable lines. Use
[`libs_lines()`](https://christiangoueguel.com/specProc/reference/libs_lines.md),
[`plot_lines()`](https://christiangoueguel.com/specProc/reference/plot_lines.md),
[`line_intensities()`](https://christiangoueguel.com/specProc/reference/line_intensities.md)
and
[`boltzmann()`](https://christiangoueguel.com/specProc/reference/boltzmann.md)
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

- molecules whose band heads are marked with dashed lines, at a
  vibrational temperature (see
  [`molecular_bands()`](https://christiangoueguel.com/specProc/reference/molecular_bands.md);
  no download needed);

- the wavelength range, a wavelength shift to correct the calibration of
  the spectrometer, and the spectrum to show: the mean of all spectra,
  the mean of a group (from the first character or factor column of
  `spectra`), or a single spectrum;

- a download of the displayed lines as CSV, and buttons to save and
  reload the lines fetched so far, so that a session can continue
  offline.

Each species is downloaded from NIST once per R session (see
[`nist_lines()`](https://christiangoueguel.com/specProc/reference/nist_lines.md)),
or once for all sessions with the persistent cache (see
[`download_cache()`](https://christiangoueguel.com/specProc/reference/download_cache.md));
later changes of the settings are computed locally. The zoom of the plot
is kept when the lines change.

**Boltzmann plot.** Clicking the dot at the top of a line marker adds
the line to the Boltzmann plot of its species, and clicking it again
removes it; **Add shown lines** adds all the lines shown for the species
chosen in the Boltzmann plot panel. The intensities are measured on the
displayed spectrum with
[`line_intensities()`](https://christiangoueguel.com/specProc/reference/line_intensities.md),
around the tabulated wavelengths plus the wavelength shift, with the
settings of the panel, and the temperature is fitted with
[`boltzmann()`](https://christiangoueguel.com/specProc/reference/boltzmann.md).
Clicking a point of the plot leaves its line out of the fit or puts it
back; lines without a positive intensity are left out. The table under
the plot lists the upper-level energy, the signal-to-noise ratio and the
residual of each line, **Use T** sets the temperature of the candidate
lines to the fitted one, and **Done** closes the app and returns the fit
to R. The spectra should be corrected for the spectral response of the
instrument first (see
[`correct_response()`](https://christiangoueguel.com/specProc/reference/correct_response.md)),
and the lines should be optically thin (see
[`self_absorption()`](https://christiangoueguel.com/specProc/reference/self_absorption.md)).

The app needs the shiny, plotly and bslib packages, and an internet
connection for the species not yet fetched.

## See also

[`libs_lines()`](https://christiangoueguel.com/specProc/reference/libs_lines.md),
[`plot_lines()`](https://christiangoueguel.com/specProc/reference/plot_lines.md),
[`nist_lines()`](https://christiangoueguel.com/specProc/reference/nist_lines.md),
[`boltzmann()`](https://christiangoueguel.com/specProc/reference/boltzmann.md)

## Author

Christian L. Goueguel

## Examples

``` r
if (interactive() && rlang::is_installed(c("shiny", "plotly", "bslib"))) {
  data(forageLIBS)
  fit <- line_finder(forageLIBS[-(1:14)])
  # after a Boltzmann plot closed with Done
  if (!is.null(fit)) plot_boltzmann(fit)
}
```
