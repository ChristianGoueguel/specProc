## Update

This is an update of specProc 0.8.3 to 0.9.0. The main changes (see
NEWS.md) are robust PLS and principal component regression with robust
cross-validation, robust SIMCA and discriminant analysis, and their
'parsnip' engines; wavelength selection; calibration transfer; uncertainty
propagation in the plasma diagnostics; and molecular band heads. The
functions deprecated since 0.2.0 and 0.8.0 are now defunct.

## Test environments

* local macOS (aarch64-apple-darwin), R 4.6.1
* GitHub Actions: macOS (R release), Windows (R release), Ubuntu (R devel,
  release and oldrel-1)

## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE

  Possibly misspelled words in DESCRIPTION: Bossche, Driessen, Engelen,
  Kohonen, LIBS, MacroPCA, OPLS, Raman, Raymaekers, Rousseeuw, Saha, SIMCA,
  Trygg, Vanden, Verboven, cellwise. These are author names, method names
  (MacroPCA, OPLS, Saha-Boltzmann, SIMCA), the acronym of laser-induced
  breakdown spectroscopy (LIBS), Raman spectroscopy, and the statistical
  term "cellwise" (cellwise outliers).

## Reverse dependencies

There are no reverse dependencies.

## Suggested packages

* `mixOmics` and `ropls` are Bioconductor packages. `ropls` is only used in
  the tests, as a reference for the results (skipped on CRAN and when it is
  not installed). `mixOmics` is the PLS engine of a vignette and of some
  tests, which are skipped when it is not installed.
* All suggested packages are used conditionally (`rlang::check_installed()`,
  `rlang::is_installed()`, `skip_if_not_installed()` or `@examplesIf`).

## Examples

Examples that need an internet connection (queries of the NIST Atomic
Spectra Database, STARK-B and ExoMol) are wrapped in `\donttest{}`, fail
gracefully with `try()`, and give up after 20 seconds when a server does not
answer. Examples that take more than a few seconds are wrapped in
`\donttest{}`. Downloaded data are kept on disk only when the user sets the
option `specProc.cache` (in `tools::R_user_dir()` or a directory of their
choice); by default nothing is written outside the temporary directory.
