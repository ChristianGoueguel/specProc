## Submission

This is a new submission.

## Test environments

* local macOS (aarch64-apple-darwin), R 4.6.1
* win-builder, R-devel (Windows Server 2022, x86_64-w64-mingw32)

## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE

  New submission.

  Possibly misspelled words in DESCRIPTION: Bossche, Kohonen, LIBS,
  MacroPCA, OPLS, Raman, Raymaekers, Rousseeuw, Saha, Trygg, Vanden,
  cellwise. These are author names, method names (MacroPCA, OPLS,
  Saha-Boltzmann), the acronym of laser-induced breakdown spectroscopy
  (LIBS), Raman spectroscopy, and the statistical term "cellwise"
  (cellwise outliers).

## Suggested packages

* `mixOmics` and `ropls` are Bioconductor packages. They are only used in
  the tests, as references for the results, and the tests are skipped when
  they are not installed.
* All suggested packages are used conditionally (`rlang::check_installed()`,
  `rlang::is_installed()`, `skip_if_not_installed()` or `@examplesIf`).

## Examples

Examples that need an internet connection (queries of the NIST and STARK-B
databases) are wrapped in `\donttest{}` and fail gracefully with `try()`.
Examples that take more than a few seconds are wrapped in `\donttest{}`.
