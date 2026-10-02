## Resubmission

This is a resubmission. In this version I have:

* Reduced the check time, which exceeded 10 minutes on Windows
  (r-devel-windows-x86_64, "Overall checktime 13 min > 10 min"):
  * The preprocessing vignette applies the baseline correction once,
    instead of in every recipe and every resample (the results are
    identical, since this correction estimates nothing from the data).
  * Three small functions of compiled code, each instantiating a large
    template of the Eigen library, now use `svd()` in R, which shortens the
    installation.
  * The tests that compare the results with those of other packages
    (ropls, rospca, cellWise) are skipped on CRAN.
  * The examples of several functions run on a window of the spectra
    instead of all 7152 channels.
* Kept the possibly misspelled words in DESCRIPTION, which are explained
  below.

## Submission

This is a new submission (the package is not yet on CRAN).

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

* `mixOmics` and `ropls` are Bioconductor packages. `ropls` is only used in
  the tests, as a reference for the results (skipped on CRAN and when it is
  not installed). `mixOmics` is the PLS engine of a vignette and of some
  tests, which are skipped when it is not installed.
* All suggested packages are used conditionally (`rlang::check_installed()`,
  `rlang::is_installed()`, `skip_if_not_installed()` or `@examplesIf`).

## Examples

Examples that need an internet connection (queries of the NIST and STARK-B
databases) are wrapped in `\donttest{}` and fail gracefully with `try()`.
Examples that take more than a few seconds are wrapped in `\donttest{}`.
