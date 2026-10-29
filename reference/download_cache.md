# Persistent Cache of Downloaded Data

Lists or clears the data downloaded from the NIST Atomic Spectra
Database, STARK-B and ExoMol and kept on disk, so that they are reused
in later sessions and without an internet connection.

## Usage

``` r
download_cache()

clear_download_cache(source = c("nist", "starkb", "exomol"), older_than = NULL)
```

## Arguments

- source:

  The sources to clear: `"nist"`, `"starkb"` and/or `"exomol"`. Default
  is all of them.

- older_than:

  Clear only the entries older than this number of days, or `NULL`
  (default) for all of them.

## Value

`download_cache()`: a tibble with one row per file of the persistent
cache, with its `source`, `file` name, `size` (kB), `modified` time and
`age` (days); its directory is the attribute `dir` (`NULL` when the
cache is not enabled). `clear_download_cache()`: invisibly, the paths of
the deleted files. It also clears the session cache of the sources.

## Details

Downloaded data are always kept for the R session. To keep them between
sessions, set the option `specProc.cache`, for example in your
`.Rprofile`:

- `options(specProc.cache = TRUE)` keeps them in the user cache
  directory of specProc, given by
  [`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html);

- `options(specProc.cache = "path/to/dir")` keeps them in that
  directory;

- `NULL` or `FALSE` (default) keeps them for the session only.

The results of
[`nist_lines()`](https://christiangoueguel.com/specProc/reference/nist_lines.md),
[`nist_ionization_energy()`](https://christiangoueguel.com/specProc/reference/nist_ionization_energy.md),
[`nist_levels()`](https://christiangoueguel.com/specProc/reference/nist_levels.md)
(and so
[`libs_lines()`](https://christiangoueguel.com/specProc/reference/libs_lines.md)
and
[`line_finder()`](https://christiangoueguel.com/specProc/reference/line_finder.md)),
[`starkb_lines()`](https://christiangoueguel.com/specProc/reference/starkb_lines.md)
and
[`molecular_spectrum()`](https://christiangoueguel.com/specProc/reference/molecular_spectrum.md)
are then read from the cache when they are there. Entries older than the
option `specProc.cache_max_age` (in days, default 180) are downloaded
again, as the databases are updated; when the download fails, the older
copy is used with a warning. The NIST and STARK-B entries are small (a
few kilobytes to a megabyte); ExoMol line lists can be large, see
[`exomol_datasets()`](https://christiangoueguel.com/specProc/reference/exomol_datasets.md).

The cache can also be cleared by deleting its directory.

## See also

[`nist_lines()`](https://christiangoueguel.com/specProc/reference/nist_lines.md),
[`starkb_lines()`](https://christiangoueguel.com/specProc/reference/starkb_lines.md),
[`molecular_spectrum()`](https://christiangoueguel.com/specProc/reference/molecular_spectrum.md)

## Author

Christian L. Goueguel

## Examples

``` r
# keep the downloaded data in a directory (here a temporary one)
old <- options(specProc.cache = file.path(tempdir(), "specProc-cache"))
download_cache()
#> # A tibble: 0 × 5
#> # ℹ 5 variables: source <chr>, file <chr>, size <dbl>, modified <dttm>,
#> #   age <dbl>
clear_download_cache()
options(old)
```
