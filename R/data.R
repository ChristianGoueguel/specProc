#' @title LIBS Spectra of Forage Samples
#'
#' @description
#' Laser-induced breakdown spectroscopy (LIBS) spectra of 365 forage samples
#' (368 measurements), together with their reference contents of 12 elements
#' measured by conventional laboratory analysis.
#'
#' @details
#' Each measurement is the mean of 8 laser shots, rounded to integer counts;
#' the shot-level spectra are not included, to keep the package small. Three
#' samples were measured twice, so there are 368 measurements of 365 samples.
#' Keep the measurements of a sample together when splitting the data into
#' calibration and validation sets.
#'
#' The spectra cover 199.4 to 822.2 nm in 7152 channels (about 0.084 nm per
#' channel) and are raw detector counts (no background subtraction or
#' normalization). They are recorded by several spectrometers whose ranges
#' are concatenated: the channels are in increasing wavelength order except
#' near 766 nm, where two spectrometers overlap by about 0.43 nm, and there
#' are gaps between 781.5 and 789.2 nm and between 800.7 and 813.9 nm. The detector saturates at 65535 counts: the
#' strongest lines (Ca II 393.37/396.85 nm, K I 766.49/769.90 nm, Mg II
#' 279.55/280.27 nm) are saturated in many shots and are nonlinear in
#' concentration.
#'
#' Reference values are missing for some elements (see the examples). Cu and
#' Mo are available for only a minority of samples.
#'
#' @format A tibble with 368 rows and 7166 columns:
#' \describe{
#'   \item{Measurement}{Measurement identifier.}
#'   \item{Sample}{Sample identifier (365 samples).}
#'   \item{Ca, Cl, Mg, P, K, Na, S}{Macro-element content, in percent.}
#'   \item{Cu, Fe, Mn, Mo, Zn}{Trace-element content, in mg/kg.}
#'   \item{199.3771616, ..., 822.1848577}{Emission intensity (counts) at each
#'     wavelength, in nm. The column names are the wavelengths.}
#' }
#'
#' @source Laboratory LIBS measurements provided by Christian L. Goueguel.
#'   The script `data-raw/forageLIBS.R` in the package source builds the data
#'   set from the shot-level export.
#'
#' @examples
#' data(forageLIBS)
#' dim(forageLIBS)
#' colSums(!is.na(forageLIBS[3:14])) # available reference values per element
#'
#' # K I doublet of the first three measurements
#' wl <- as.numeric(names(forageLIBS)[-(1:14)])
#' keep <- names(forageLIBS)[-(1:14)][wl > 403.5 & wl < 405.5]
#' plot_spectra(forageLIBS[1:3, c("Measurement", keep)], id = Measurement)
#'
"forageLIBS"

#' @title LIBS Laser Shots of Forage Samples
#'
#' @description
#' The single laser shots of 20 of the measurements of [forageLIBS]: 8
#' shots per measurement, in two wavelength windows, to study the
#' shot-to-shot variability and the rejection of outlying shots.
#'
#' @details
#' [forageLIBS] holds the mean of the 8 shots of each measurement; these are
#' the shots themselves, raw detector counts, in the order in which they
#' were fired. To keep the package small, only two windows are kept:
#' 380 to 430 nm (Ca II 393.37 and 396.85 nm, Ca I 422.67 nm, Al I 394.40
#' and 396.15 nm, the CN band at 388 nm) and 760 to 780 nm (K I 766.49 and
#' 769.90 nm, O I 777 nm). The strongest lines reach the saturation of the
#' detector (65535 counts) in most shots (132 of the 160); the `wavelength`
#' argument of [reject_shots()] can leave them out.
#'
#' The 20 measurements are those of the 368 with a shot rejected by
#' [reject_shots()] in these windows (7), and 13 others drawn at random:
#' their outlying shots are weak or strong plasmas (total intensity 0.7 or
#' 1.3 times that of the other shots) with spectra of a different shape.
#'
#' @format A tibble with 160 rows (shots) and 844 columns:
#' \describe{
#'   \item{Measurement}{Measurement identifier, as in [forageLIBS].}
#'   \item{Sample}{Sample identifier, as in [forageLIBS].}
#'   \item{shot}{Shot number in the measurement (1 to 8), in firing order.}
#'   \item{Ca, K}{Calcium and potassium contents, in percent.}
#'   \item{380.011254, ..., 779.977295}{Emission intensity (counts) at each
#'     wavelength, in nm.}
#' }
#'
#' @source Laboratory LIBS measurements provided by Christian L. Goueguel.
#'   The script `data-raw/forageShots.R` in the package source builds the
#'   data set from the shot-level export.
#'
#' @seealso [reject_shots()], [plot_shots()], [forageLIBS]
#'
#' @examples
#' data(forageShots)
#' dim(forageShots)
#' # the shot-to-shot variability of the total intensity, per measurement
#' total <- rowSums(forageShots[-(1:5)])
#' round(tapply(total, forageShots$Measurement, function(v) 100 * sd(v) / mean(v)), 1)
"forageShots"
