## Submission

This is a new submission.

## Test environments

* local macOS (aarch64-apple-darwin), R 4.6.1
* win-builder (devel and release)
* R-hub (Linux, Windows, macOS)

## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE

  New submission.

* Installed package size (reported as INFO locally, may be a NOTE on other
  platforms):

  installed size is 8.1Mb; sub-directories of 1Mb or more: data 4.2Mb, doc 1.0Mb.

  The `data` directory holds `forageLIBS`, a dataset of 368 LIBS spectra
  (7152 channels each), compressed with xz. It is used by the examples and
  vignettes to show the methods on real spectra. The size of the compiled
  code depends on the platform.

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
