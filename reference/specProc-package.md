# specProc: Preprocessing Tools for Laser-Induced Breakdown Spectroscopy

Tools for exploring, preprocessing and modeling spectroscopic data,
developed for laser-induced breakdown spectroscopy (LIBS) and also
applicable to other techniques such as Raman, infrared, and inductively
coupled plasma optical emission spectroscopy. Covers the workflow from
raw spectra to calibrated results: peak finding and fitting, wavelength
calibration, baseline correction, normalization and scaling, orthogonal
signal correction including orthogonal partial least squares (OPLS;
Trygg and Wold (2002)
[doi:10.1002/cem.695](https://doi.org/10.1002/cem.695) ), wavelength
selection (variable importance in projection, selectivity ratio,
interval partial least squares), calibration transfer, calibration
curves and figures of merit, and plasma diagnostics (Boltzmann and
Saha-Boltzmann plots, Stark broadening). Includes robust tools for
outlier detection and exploration: robust principal component analysis
(Hubert, Rousseeuw and Vanden Branden (2005)
[doi:10.1198/004017004000000563](https://doi.org/10.1198/004017004000000563)
), robust partial least squares regression (Hubert and Vanden Branden
(2003) [doi:10.1002/cem.822](https://doi.org/10.1002/cem.822) ),
detection of deviating cells (Rousseeuw and Van den Bossche (2018)
[doi:10.1080/00401706.2017.1340909](https://doi.org/10.1080/00401706.2017.1340909)
), PCA with cellwise outliers and missing values (MacroPCA; Hubert,
Rousseeuw and Van den Bossche (2019)
[doi:10.1080/00401706.2018.1562989](https://doi.org/10.1080/00401706.2018.1562989)
), robust transformations to central normality (Raymaekers and Rousseeuw
(2021)
[doi:10.1007/s10994-021-05960-5](https://doi.org/10.1007/s10994-021-05960-5)
), and self-organizing maps (Kohonen (1982)
[doi:10.1007/BF00337288](https://doi.org/10.1007/BF00337288) ).
Preprocessing steps are also available as 'recipes' steps for
'tidymodels' workflows. Computationally intensive steps are implemented
in 'C++' via 'Rcpp' and 'RcppEigen'.

## See also

Useful links:

- <https://github.com/ChristianGoueguel/specProc>

- <https://christiangoueguel.com/specProc/>

- Report bugs at <https://github.com/ChristianGoueguel/specProc/issues>

## Author

**Maintainer**: Christian L. Goueguel <christian.goueguel@gmail.com>
([ORCID](https://orcid.org/0000-0003-0521-3446))

Authors:

- Christian L. Goueguel <christian.goueguel@gmail.com>
  ([ORCID](https://orcid.org/0000-0003-0521-3446))
